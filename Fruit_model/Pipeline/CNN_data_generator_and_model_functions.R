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
annotations <- jsonlite::fromJSON(txt = params$annot_file)

# create image object
imageinfo <- create_image_container(annotations)

# Scale bbox
imageinfo <- scale_image_boundingbox(imageinfo, params$target_height, params$target_width)

n_samples <- nrow(imageinfo)
set.seed(12) # seed
train_indices <- sample(1:n_samples, params$proportion_of_samples * n_samples)
train_data <- imageinfo[train_indices,]
validation_data <- imageinfo[-train_indices,]


# Data generator
image_size <- params$target_width # same as height


feature_extractor <- application_xception(
  include_top = FALSE,
  input_shape = c(224, 224, 3)
)

input <- feature_extractor$input

common <- feature_extractor$output %>%
  layer_flatten(name = "flatten") %>%
  layer_activation_relu() %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = params$layer_units, activation = "relu") %>%
  #layer_batch_normalization() %>% #turn off?? - probably yes otherwise output is centred around 0,0 i.e box is always small in top left corner
  layer_dropout(rate = 0.5)

# Bounding box
regression_output <-
  layer_dense(common, units = 4, name = "regression_output")

# Class prediction
class_output <- layer_dense(
  common,
  units = params$cl_output,
  activation = "softmax",
  name = "class_output"
)

model <- keras_model(
  inputs = input,
  outputs = list(regression_output, class_output)
)

# box metric
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

model %>% freeze_weights(to = "flatten")

model %>% compile(
  optimizer = "adam",
  loss = list("mae", "sparse_categorical_crossentropy"),
  metrics = list(
    regression_output = custom_metric("iou", metric_iou),
    class_output = "accuracy"
  )
)




loc_class_generator <-
  function(data,
           target_height,
           target_width,
           shuffle,
           batch_size) {
    i <- 1
    function() {
      if (shuffle) {
        indices <- sample(1:nrow(data), size = batch_size)
      } else {
        if (i + batch_size >= nrow(data))
          i <<- 1
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

train_gen <- loc_class_generator(
  train_data,
  target_height = params$target_height,
  target_width = params$target_width,
  shuffle = TRUE,
  batch_size = params$batch_size
)

valid_gen <- loc_class_generator(
  validation_data,
  target_height = params$target_height,
  target_width = params$target_width,
  shuffle = FALSE,
  batch_size = params$batch_size
)


# used later for testing
tr_data <- train_data[, c("file_name", # or train_data if preferred
                          "name",
                          "x_left_scaled",
                          "y_top_scaled",
                          "x_right_scaled",
                          "y_bottom_scaled")]

val_data <- validation_data[, c("file_name", # or train_data if preferred
                                "name",
                                "x_left_scaled",
                                "y_top_scaled",
                                "x_right_scaled",
                                "y_bottom_scaled")]


# load model or use one in environment
load_model <- function(load){
  if(load == 1){ # if not running CNN_model_trainer, load model previously saved
    setwd(params$folder_to_save_model_in)
    conv_nn_model <- load_model_hdf5(params$model_name,custom_objects=c("iou" = metric_iou))
    setwd(params$folder_containing_scripts)
  }else{
    conv_nn_model <- model # as model is already in the environment
  }
  return(conv_nn_model)
}


# SVM fake data formatter
format_data <- function(dataframe){
  # make sure values are correct format
  if("disease" %in% colnames(dataframe)){
    dataframe$disease   <- as.factor(dataframe$disease)
  }
  dataframe$d1_score  <- as.numeric(as.character(dataframe$d1_score))
  dataframe$d2_score  <- as.numeric(as.character(dataframe$d2_score))
  dataframe$d3_score  <- as.numeric(as.character(dataframe$d3_score))
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



fake_data_creator<- function(dataframe_to_use,name_to_use,image_data_to_use,start_no,L_bias,WB_bias,S_bias,Rain_av,Temp_av){
  for(i in start_no:length(image_data_to_use$label)){
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
    d1_score<-image_data_to_use[i,1]
    d2_score<-image_data_to_use[i,2]
    d3_score<-image_data_to_use[i,3]
    # adjustments
    if(ll > L_bias){location='Midlands'}
    if(ss > S_bias){soil='sandy'}
    if(ww > WB_bias){WB='WB2'}
    # add into data frame
    dataframe_to_use<-rbind(dataframe_to_use,t(c('disease'=name_to_use,'d1_score'=d1_score,'d2_score'=d2_score,'d3_score'=d3_score,'location'=location,'rainfall'=rainfall,'mean_temp'=mean_temp,'crop_variety'=WB,'soil_type'=soil)))
  }
  return(dataframe_to_use)
}