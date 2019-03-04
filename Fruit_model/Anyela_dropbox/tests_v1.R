setwd("C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox")

source('image_library_v1.R')

# source('~/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/tests_v1.R')

class_list <- c("MSD","BS","YR")

params <- list('img_dir' = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/all2",
               'annot_file' = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/jsonfold/online.json",
               'target_height' = 224,
               'target_width' = 224,
               'batch_size' = 2, #10 #1
               'proportion_of_samples' = 0.25,
               'threshold' = 0.4,
               'class_background' = 3, #21
               'cl_output' = 3, # 20
               'epochs' = 20,
               'weight_file_path' = "C:/Users/Administrator/Documents/GitHub/Weights",
               'label_names' = class_list,
               'layer_units' = 64, # 30
               'patience' = 2 # was 8, but that's quite slow
)


annotations <- jsonlite::fromJSON(txt = params$annot_file)

# create image object
imageinfo <- create_image_container(annotations)

# Scale bbox
imageinfo <- scale_image_boundingbox(imageinfo, params$target_height, params$target_width)


n_samples <- nrow(imageinfo)
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





model %>% fit_generator(
  train_gen,
  epochs = params$epochs,
  steps_per_epoch = nrow(train_data) / params$batch_size,
  validation_data = valid_gen,
  validation_steps = nrow(validation_data) / params$batch_size,
  callbacks = list(
    callback_model_checkpoint(
      file.path(params$weight_file_path, "weights.{epoch:02d}-{val_loss:.2f}.hdf5")
    ),
    callback_early_stopping(patience = params$patience)
  )
)





# analyse output

train_1_8 <- train_data[1:8, c("file_name",
                                "name",
                                "x_left_scaled",
                                "y_top_scaled",
                                "x_right_scaled",
                                "y_bottom_scaled")]
 

preds<-  model %>% predict(
  load_and_preprocess_image(train_1_8[1, "file_name"], 
                            params$target_height, params$target_width),
  batch_size = 1
)
plot_image_with_boxes_single(train_1_8$file_name[1],
                             train_1_8$name[1],
                             train_1_8[1, 3:6] %>% as.matrix(),
                             scaled = TRUE, # FALSE?
                             box_pred = preds[[1]], # should be just preds[[1]]
                             class_pred = preds[[2]]
)
preds[[1]]
preds[[2]]
box_predictions<-as.data.frame(preds[[1]])
class_preds <- as.data.frame(preds[[2]])
#class_preds$actual_category<-train_1_8$name[1]
for (i in 2:8) {
  preds <-
    model %>% predict(
      load_and_preprocess_image(train_1_8[i, "file_name"], 
                                params$target_height, params$target_width),
      batch_size = 1
    )
preds2<-as.data.frame(preds[[1]])
cl_preds2<-as.data.frame(preds[[2]])
#cl_preds2$actual_category<-train_1_8$name[i]
box_predictions<- rbind(box_predictions,preds2)
class_preds <- rbind(class_preds,cl_preds2)
}

train_1_8[1:8, 3:6]
box_predictions

colnames(box_predictions)<-c("xl_pred","yt_pred","xr_pred","yb_pred")

colnames(class_preds) <- params$label_names
class_preds1<-class_preds #[-actual_category]
class_preds$predicted_disease <- names(class_preds)[apply(class_preds, 1, which.max)]
data_labels<-as.data.frame(train_1_8$name[1:8])
colnames(data_labels)<-"label"
class_preds<-cbind(class_preds,data_labels)
class_preds

corners<-cbind(train_1_8[1:8, 3:6],box_predictions)
corners$xl_error <- corners[,1]-corners[,5]
corners$yt_error <- corners[,2]-corners[,6]
corners$xr_error <- corners[,3]-corners[,7]
corners$yb_error <- corners[,4]-corners[,8]
corners

dev.off() # allows new plot to open
par(mfrow=c(2,2))
boxplot(corners$xl_error)
boxplot(corners$yt_error)
boxplot(corners$xr_error)
boxplot(corners$yb_error)

dev.off() # allows new plot to open
par(mfrow=c(2,2))

for(i in 1:4){
plot_image_with_boxes_single(train_1_8$file_name[i],
                             train_1_8$name[i],
                             train_1_8[i, 3:6] %>% as.matrix(),
                             scaled = TRUE, # FALSE? - probably not
                             box_pred = box_predictions[i,], # should be just preds[[1]]
                             class_pred = class_preds1[i,]
)
}