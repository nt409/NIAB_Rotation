setwd("C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox")

source('image_library_v1.R')

# source('~/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/tests_v1.R')

class_list <- c("MSD","BS")

params <- list('img_dir' = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/all2",
               'annot_file' = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/jsonfold/online.json",
               'target_height' = 224,
               'target_width' = 224,
               'batch_size' = 2, #10 #1
               'proportion_of_samples' = 0.15,
               'threshold' = 0.4,
               'class_background' = 2, #21
               'cl_output' = 2, # 20
               'epochs' = 20,
               'weight_file_path' = "C:/Users/Administrator/Documents/GitHub/Weights",
               'label_names' = class_list,
               'layer_units' = 64, # 30
               'patience' = 4
)


annotations <- jsonlite::fromJSON(txt = params$annot_file)

# create image object
imageinfo <- create_image_container(annotations)

# Scale bbox
imageinfo <- scale_image_boundingbox(imageinfo, params$target_height, params$target_width)

#iimagesc <- select_scale_image(imageinfo, "scaled")

n_samples <- nrow(imageinfo)
train_indices <- sample(1:n_samples, params$proportion_of_samples * n_samples)
train_data <- imageinfo[train_indices,]
validation_data <- imageinfo[-train_indices,]
#anchor_list  <- create_anchors(cells_per_row)

# Data generator
image_size <- params$target_width # same as height
# threshold <- 0.4
#n_classes <- 2
# class_background <- 21


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
  #layer_batch_normalization() %>% #turn off??
  layer_dropout(rate = 0.5)

regression_output <-
  layer_dense(common, units = 4, name = "regression_output") # Bounding box


class_output <- layer_dense(
  common,
  units = params$cl_output, # was 20
  activation = "softmax",
  name = "class_output" # class probs
)

model <- keras_model(
  inputs = input,
  outputs = list(regression_output, class_output)
)

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
  #loss_weights = list(
  #  regression_output = 0.05,
  #  class_output = 0.95),
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
          data[indices[j], c("x_left_scaled", "y_top_scaled", "x_right_scaled", "y_bottom_scaled")] %>% as.matrix() # _scaled?
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
  validation_steps = nrow(validation_data) / params$batch_size, #100,
  callbacks = list(
    callback_model_checkpoint(
      file.path(params$weight_file_path, "weights.{epoch:02d}-{val_loss:.2f}.hdf5")
    ),
    callback_early_stopping(patience = params$patience)
  )
)



 train_1_8 <- train_data[1:8, c("file_name",
                                "name",
                                "x_left_scaled",
                                "y_top_scaled",
                                "x_right_scaled",
                                "y_bottom_scaled")]
 

k <- 8 #4
for (i in k:k) {
  preds <-
    model %>% predict(
      load_and_preprocess_image(train_1_8[i, "file_name"], 
                                params$target_height, params$target_width),
      batch_size = 1
    )
  plot_image_with_boxes_single(train_1_8$file_name[i],
                        train_1_8$name[i],
                        train_1_8[i, 3:6] %>% as.matrix(),
                        scaled = TRUE, # FALSE?
                        box_pred = preds[[1]], # should be just preds[[1]]
                        class_pred = preds[[2]]
                        )
}
preds[[1]]
train_1_8[i, 3:6]
preds[[2]]