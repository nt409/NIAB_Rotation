# setwd(params$folder_containing_scripts)
# source('parameters.R')
# source('Image_classifier_functions.R')

# how many of these necessary?
library(keras)
library(rjson)
library(magick)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(XML)
library(xml2)
library(jsonlite)
library(tensorflow)

######################################################################
use_session_with_seed(1) # Keras and Tensorflow seed
set.seed(params$seed) # R seed

create_image_container <- function(annotation){
  
  imageinfo <- annotation$images %>% {
    tibble(
      id = map_dbl(.$id, c),
      file_name = map_chr(.$file_name, c),
      image_height = map_dbl(.$height, c),
      image_width = map_dbl(.$width, c)
    )
  }
  
  # Load bounding box
  boxinfo <- annotation$annotations %>% {
    tibble(
      image_id = map_dbl(.$image_id, c),
      category_id = map_dbl(.$category_id, c),
      bbox = map(.$bbox, c)
    )
  }
  
  
  boxinfo <- boxinfo %>% 
    mutate(bbox = unlist(map(.$bbox, function(x) paste(x, collapse = " "))))
  boxinfo <- boxinfo %>% 
    separate(bbox, into = c("x_left", "y_top", "bbox_width", "bbox_height"))
  boxinfo <- boxinfo %>% mutate_all(as.numeric)
  
  #As usual in image processing, the y axis starts from the top.
  boxinfo <- boxinfo %>% 
    mutate(y_bottom = y_top + bbox_height - 1, x_right = x_left + bbox_width - 1)
  
  #Finally, we still need to match class ids to class names.
  catinfo <- annotation$categories %>%  {
    tibble(id = map_dbl(.$id, c), name = map_chr(.$name, c))
  }
  
  #So, putting it all together:
  imageinfo <- imageinfo %>%
    inner_join(boxinfo, by = c("id" = "image_id")) %>%
    inner_join(catinfo, by = c("category_id" = "id"))
  
  return(imageinfo)
}

scale_image_boundingbox <- function(imageinfo, target_height, target_width){
  
  imageinfo <- imageinfo %>% mutate(
    x_left_scaled = (x_left / image_width * target_width) %>% round(),
    x_right_scaled = (x_right / image_width * target_width) %>% round(),
    y_top_scaled = (y_top / image_height * target_height) %>% round(),
    y_bottom_scaled = (y_bottom / image_height * target_height) %>% round(),
    bbox_width_scaled =  (bbox_width / image_width * target_width) %>% round(),
    bbox_height_scaled = (bbox_height / image_height * target_height) %>% round()
  )
  
  return(imageinfo)
}

######################################################################
annotations <- jsonlite::fromJSON(txt = params$annot_file)

# use annotations to correctly update parameters automatically.
params$label_names <- annotations$categories$name
params$cl_output <- length(params$label_names)
params$class_background <- length(params$label_names)

# create image object
imageinfo <- create_image_container(annotations)

# Scale bbox
imageinfo <- scale_image_boundingbox(imageinfo, params$target_height, params$target_width)

# define box metric
metric_iou <- function(y_true, y_pred) {
  # order is [x_left, y_top, x_right, y_bottom]
  intersection_xmin <- k_maximum(y_true[ ,1], y_pred[ ,1])
  intersection_ymin <- k_maximum(y_true[ ,2], y_pred[ ,2])
  intersection_xmax <- k_minimum(y_true[ ,3], y_pred[ ,3])
  intersection_ymax <- k_minimum(y_true[ ,4], y_pred[ ,4])
  
  area_intersection <- (intersection_xmax - intersection_xmin) * 
    (intersection_ymax - intersection_ymin)
  area_y <- (y_true[ ,3] - y_true[ ,1]) * (y_true[ ,4] - y_true[ ,2])
  area_yhat <- (y_pred[ ,3] - y_pred[ ,1]) * (y_pred[ ,4] - y_pred[ ,2])
  area_union <- area_y + area_yhat - area_intersection
  
  iou <- area_intersection/area_union
  k_mean(iou)
  
}
attr(metric_iou, "py_function_name") <- "metric_iou"


######################################################################
# Data generator
image_size <- params$target_width # same as height

# is slow so only run if necessary
# if feature_extractor not in environment, then define
if(exists("feature_extractor")==FALSE){
  feature_extractor <<- application_xception(
    include_top = FALSE,
    input_shape = c(224, 224, 3)
  )
}else{if(is.null(feature_extractor$input)){ # or if feature_extractor not in correct form
  feature_extractor <- application_xception(
    include_top = FALSE,
    input_shape = c(224, 224, 3)
  )
}
}

input <- feature_extractor$input

n_samples <- nrow(imageinfo)
train_indices <- sample(1:n_samples, params$proportion_of_samples * n_samples)

# create model framework... this bit runs using different parameters
formulate_model<-function(){
  train_data <<- imageinfo[train_indices,]
  validation_data <<- imageinfo[-train_indices,]

  common <<- feature_extractor$output %>%
    layer_flatten(name = "flatten") %>%
    layer_activation_relu() %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = params$layer_units, activation = "relu") %>%
    #layer_batch_normalization() %>% #turn off?? - probably yes otherwise output is centred around 0,0 i.e box is always small in top left corner
    layer_dropout(rate = 0.5)
  
  # Bounding box
  regression_output <<- layer_dense(common, units = 4, name = "regression_output")
  
  # Class prediction
  class_output <<- layer_dense(
    common,
    units = params$cl_output,
    activation = "softmax",
    name = "class_output"
  )
  
  model <<- keras_model(
    inputs = input,
    outputs = list(regression_output, class_output)
  )

  model %>% freeze_weights(to = "flatten")
  
  model %>% compile(
    optimizer = "adam",
    loss = list("mae", "sparse_categorical_crossentropy"),
    metrics = list(
      regression_output = custom_metric("iou", metric_iou),
      class_output = "accuracy"
    )
  )
  
  train_gen <<- loc_class_generator(
    train_data,
    target_height = params$target_height,
    target_width = params$target_width,
    shuffle = TRUE, # was true, affects choice of model??
    batch_size = params$batch_size
  )
  
  valid_gen <<- loc_class_generator(
    validation_data,
    target_height = params$target_height,
    target_width = params$target_width,
    shuffle = FALSE,
    batch_size = params$batch_size_val
  )
return(model)
}

######################################################################
###
# added from image_classifier_functions
load_and_preprocess_image <- function(image_name, target_height, target_width) {
  
  img_array <- image_load(
    file.path(params$img_dir, image_name),
    target_size = c(target_height, target_width)
  ) %>%
    image_to_array() %>%
    xception_preprocess_input() 
  dim(img_array) <- c(1, dim(img_array))
  img_array
}

###
######################################################################

loc_class_generator <-  function(data,target_height,target_width,shuffle,batch_size) {
    i <- 1
    function() {
      if (shuffle) {
        indices <- sample(1:nrow(data), size = batch_size)
      } else {
        if (i + batch_size >= nrow(data))  i <<- 1
        indices <- c(i:min(i + batch_size - 1, nrow(data)))
        i <<- i + length(indices)
      }
      x <-
        array(0, dim = c(length(indices), target_height, target_width, 3))
      y1 <- array(0, dim = c(length(indices), 4))
      y2 <- array(0, dim = c(length(indices), 1))
      
      for (j in 1:length(indices)) {
        x[j, , , ] <-
          load_and_preprocess_image(data[[indices[j], "file_name"]], 
                                    target_height, target_width)
        y1[j, ] <-
          data[indices[j], c("x_left_scaled", "y_top_scaled", "x_right_scaled", "y_bottom_scaled")] %>% as.matrix() # _scaled? - probably yes? We want to work with scaled images so that output is in (0,224)
        y2[j, ] <-
          data[[indices[j], "category_id"]] - 1
      }
      x <- x / 255
      list(x, list(y1, y2))
    }
  }

formulate_model()
######################################################################
# used later for testing
tr_data <- train_data[, c("file_name",
                          "name",
                          "x_left_scaled",
                          "y_top_scaled",
                          "x_right_scaled",
                          "y_bottom_scaled")]

val_data <- validation_data[, c("file_name",
                                "name",
                                "x_left_scaled",
                                "y_top_scaled",
                                "x_right_scaled",
                                "y_bottom_scaled")]



######################################################################
### model trainer
model_trainer<-function(){
  model<-formulate_model()
  #params$batch_size_val<<-nrow(val_data)
  hist<- model %>% fit_generator(
    train_gen,
    epochs = params$epochs,
    steps_per_epoch = nrow(train_data) / params$batch_size,
    validation_data = valid_gen,
    validation_steps = nrow(validation_data) / params$batch_size_val,
    callbacks = list(
      callback_model_checkpoint(
        file.path(params$folder_to_save_weights_in, "weights.{epoch:02d}-{val_loss:.2f}.hdf5"),
        save_best_only = FALSE, # was TRUE
        save_weights_only = TRUE, # otherwise we are generating a lot of big files
        verbose = 1
      ),
      callback_early_stopping(patience = params$patience)
    )
  )
  model_to_output<-model #? problem here?#hist$model??
  return(list('hist'=hist,'model_trained'=model_to_output))
}


######################################################################
## train model with variety of parameters, keep model with best validation class accuracy
grid<-function(proportion_samples_vec,epochs_vec,batch_size_vec,layers_vec){
  ### initialise
  counter<-0
  vals<-list()
  parameters_used<-list()
  model_hist_val_class_acc <- list()
  model_hist_val_iou <- list()
  model_hist_train_iou <- list()
  model_hist_train_class_acc <- list()
  train_class_acc <- list()
  train_iou <- list()
  train_class_output_loss <- list()
  val_class_acc <- list()
  val_iou <- list()
  val_class_output_loss <- list()
  #####
  for(i in 1:length(proportion_samples_vec)){
    for(j in 1:length(epochs_vec)){
      for(k in 1:length(batch_size_vec)){
        for(l in 1:length(layers_vec)){
          params$proportion_of_samples <<- proportion_samples_vec[i]
          params$epochs <<- epochs_vec[j]
          params$batch_size <<- batch_size_vec[k]
          params$layer_units <<- layers_vec[l]
          counter<-counter+1
          vals[[counter]]<-list(i,j,k,l)
          parameters_used[[counter]]<-list('Proportion_Samples' = proportion_samples_vec[i],'Epochs' = epochs_vec[j],'Batch_Size'= batch_size_vec[k],'Layers'= layers_vec[l])
          hist1<-model_trainer()
          history<-hist1$hist
          model_produced<-hist1$model_trained
          
          # model_hist_train_class_acc[[counter]]<-history$metrics$class_output_acc   # every epoch value
          # model_hist_train_iou[[counter]]<-history$metrics$regression_output_iou    # every epoch value
          # model_hist_val_class_acc[[counter]]<-history$metrics$val_class_output_acc # every epoch value
          # model_hist_val_iou[[counter]]<-history$metrics$val_regression_output_iou  # every epoch value
          
          no_model_saved <- which.min(unlist(history$metrics$val_loss)) # which model is saved? Thus which metrics are evaluated on the model we'll use
          train_class_acc[[counter]]<-history$metrics$class_output_acc[no_model_saved]              # epoch value no_model_saved
          train_iou[[counter]]<-history$metrics$regression_output_iou[no_model_saved]               # epoch value no_model_saved
          train_class_output_loss[[counter]]<-history$metrics$class_output_loss[no_model_saved]     # epoch value no_model_saved
          val_class_acc[[counter]]<-history$metrics$val_class_output_acc[no_model_saved]            # epoch value no_model_saved
          val_iou[[counter]]<-history$metrics$val_regression_output_iou[no_model_saved]             # epoch value no_model_saved
          val_class_output_loss[[counter]]<-history$metrics$val_class_output_loss[no_model_saved]   # epoch value no_model_saved
          if(counter == which.max(unlist(val_class_acc))){ # We want to keep the model with best class acc? But is this the way saving works for us, or is it saving models based on val_loss?
            best_model<-model_produced
            best_count<-counter
            best_params<-parameters_used[[counter]]
          }
        }
      }
    }
  }
  train_class_acc_vec<-as.data.frame(do.call(rbind,train_class_acc))
  train_iou_vec<-as.data.frame(do.call(rbind,train_iou))
  train_class_output_loss_vec<-as.data.frame(do.call(rbind,train_class_output_loss))
  val_iou_vec<-as.data.frame(do.call(rbind,val_iou))
  val_class_acc_vec<-as.data.frame(do.call(rbind,val_class_acc))
  val_class_output_loss_vec<-as.data.frame(do.call(rbind,val_class_output_loss))
  parameters_used_vec<-as.data.frame(do.call(rbind,parameters_used))
  metric_table<-cbind('train_class_output_loss' = train_class_output_loss_vec,'train_class_acc'=train_class_acc_vec,'train_iou' = train_iou_vec,'val_class_output_loss' = val_class_output_loss_vec,'val_class_acc' = val_class_acc_vec,'val_iou' = val_iou_vec)
  colnames(metric_table)<-c('train_class_output_loss','train_class_acc','train_iou','val_class_output_loss','val_class_acc','val_iou')
  grid_results<-arrange(cbind(metric_table,parameters_used_vec),desc(val_class_output_loss))
  return(list('best_model' = best_model,'best_count'=best_count,'best_params'=best_params,'parameters_used'=parameters_used,'train_class_acc'=train_class_acc,'train_iou'=train_iou,'train_class_output_loss'=train_class_output_loss,'val_class_acc'=val_class_acc,'val_iou'=val_iou,'val_class_output_loss'=val_class_output_loss,'grid_results'=grid_results))
}


######################################################################
# load model or use one in environment
load_model <- function(load){
  if(load == 1){ # if not running CNN_model_trainer, load model previously saved
    setwd(params$folder_to_save_model_in)
    conv_nn_model <- load_model_hdf5(params$model_name_to_load,custom_objects=c("iou" = metric_iou))
    setwd(params$folder_containing_scripts)
  }else{
    conv_nn_model <- model # as model is already in the environment
  }
  return(conv_nn_model)
}


######################################################################
generate_empty_data_frame <- function(name_disease_to_use){
  dat_1 <- as.data.frame(t(c('disease'="about_to_be_empty")))
  dat_2 <- data.frame(as.list(name_disease_to_use))
  for(kk in 1:length(params$label_names)){
    colnames(dat_2)[kk] <- paste(name_disease_to_use[[kk]],"_score",sep="")
  }
  dat_3<-as.data.frame(t(c('location'="about_to_be_empty",'rainfall'="about_to_be_empty",'mean_temp'="about_to_be_empty",'crop_variety'="about_to_be_empty",'soil_type'="about_to_be_empty")))
  dat_4 <- cbind(dat_1,dat_2,dat_3)
  dat_fm<-list()
  for(kk in 1:(length(colnames(dat_4)))){
    dat_fm[[kk]]<-numeric(0)
  }
  data_frame_output<-as.data.frame(dat_fm)
  colnames(data_frame_output)<-colnames(dat_4)
  return(data_frame_output)
}

######################################################################
# svm fake data creator
fake_data_creator<- function(dataframe_to_use,name_to_use,image_data_to_use,L_bias,WB_bias,S_bias,Rain_av,Temp_av,index){
  datalist <- list()
  d_score <- list()
  for(i in 1:length(image_data_to_use$label)){
    #initialise random variables
    rr<-rnorm(1,mean=0,sd=1)
    tt<-rnorm(1,mean=0,sd=1)
    ll<-rnorm(1,mean=0,sd=1)
    ss<-rnorm(1,mean=0,sd=1)
    ww<-rnorm(1,mean=0,sd=1)
    # data
    location<-'East_Anglia'
    soil<-"clay"
    WB <- 'WB1'
    rainfall <- Rain_av+rainfall_sd*rr
    mean_temp<- Temp_av+temp_sd*tt
    for(k in 1:length(params$label_names)){
    d_score[[k]]<-image_data_to_use[i,k]
    }
    # adjustments
    if(ll > L_bias){location='Midlands'}
    if(ss > S_bias){soil='sandy'}
    if(ww > WB_bias){WB='WB2'}
    
    # add into data frame
    dat_1 <- as.data.frame(t(c('disease'=name_to_use[[index]])))
    dat_2 <- data.frame(as.list(d_score))
    for(kk in 1:length(params$label_names)){
    colnames(dat_2)[kk] <- paste(name_to_use[[kk]],"_score",sep="")
    }
    dat_3<-as.data.frame(t(c('location'=location,'rainfall'=rainfall,'mean_temp'=mean_temp,'crop_variety'=WB,'soil_type'=soil)))
    dat <- cbind(dat_1,dat_2,dat_3)
    datalist[[i]] <- dat # add to list of data frames
  }
  
  big_data <- do.call(rbind, datalist)
  
  if(length(dataframe_to_use)>0){
  dataframe_to_use<-rbind(dataframe_to_use,big_data)
  return(dataframe_to_use)
  }else(return(big_data))
}

######################################################################
# SVM fake data formatter
format_data <- function(dataframe,name_disease_to_use){
  # make sure values are correct format
  if("disease" %in% colnames(dataframe)){
    dataframe$disease   <- as.factor(dataframe$disease)
  }
  position<-list()
  for(j in length(params$label_names)){
  position[[j]]<-which(colnames(dataframe) == paste(name_disease_to_use[[j]],"_score",sep=""))
  }
  for(jj in position){
  dataframe[,jj]  <- as.numeric(as.character(dataframe[,jj]))
  }
  dataframe$rainfall  <- as.numeric(as.character(dataframe$rainfall))
  dataframe$mean_temp <- as.numeric(as.character(dataframe$mean_temp))
  
  # make categorical data into numeric data taking values 0 or 1
  dataframe <- mutate(dataframe,
                      Loc_EA_indic = ifelse(location=="East_Anglia",1,0),
                      Loc_Midlands_indic = ifelse(location=="Midlands",1,0),
                      WB_1_indic = ifelse(crop_variety=="WB1",1,0),
                      WB_2_indic = ifelse(crop_variety=="WB2",1,0),
                      ST_clay_indic = ifelse(soil_type=="clay",1,0),
                      ST_sandy_indic = ifelse(soil_type=="sandy",1,0)
  )
  return(dataframe)
}



######################################################################
# image plotter
plot_image_with_boxes_single <- function(file_name,
                                         object_class,
                                         box,
                                         scaled = FALSE,
                                         class_pred = NULL,
                                         box_pred = NULL) {
  img <- image_read(file.path(params$img_dir, file_name))
  if(scaled) img <- image_resize(img, geometry = "224x224!")
  img <- image_draw(img)
  x_left <- box[1]
  y_bottom <- box[2]
  x_right <- box[3]
  y_top <- box[4]
  rect(
    x_left,
    y_bottom,
    x_right,
    y_top,
    border = "cyan",
    lwd = 2.5
  )
  text(
    x_left,
    y_top,
    object_class,
    offset = 1,
    pos = 2,
    cex = 1.5,
    col = "cyan"
  )
  if (!is.null(box_pred))
    rect(box_pred[1],
         box_pred[2],
         box_pred[3],
         box_pred[4],
         border = "yellow",
         lwd = 2.5)
  if (!is.null(class_pred))
  label_no <- which(class_pred == max(class_pred))
  text(
    box_pred[1],
    box_pred[2],
    params$label_names[label_no],
    offset = 1,
    pos = 4,
    cex = 1.5,
    col = "yellow")
  dev.off()
  setwd(params$folder_to_save_images_in)
  img %>% image_write(paste0("preds_", file_name))
  setwd(params$folder_containing_scripts)
  plot(img)
}


