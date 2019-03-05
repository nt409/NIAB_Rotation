setwd(params$folder_containing_scripts)
source('parameters.R')
source('Image_classifier_functions.R')

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

# source('~/GitHub/NIAB_Rotation/Fruit_model/Pipeline/Image_classifier.R')

######################################################################
annotations <- jsonlite::fromJSON(txt = params$annot_file)

# create image object
imageinfo <- create_image_container(annotations)

# Scale bbox
imageinfo <- scale_image_boundingbox(imageinfo, params$target_height, params$target_width)

n_samples <- nrow(imageinfo)
set.seed(142) # seed
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
