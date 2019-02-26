setwd("C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox")

# source('image_library.R')
# 
# 
# img_dir <- 'C:/Users/x992029/Downloads/VOCtrainval_06-Nov-2007/VOCdevkit/VOC2007/JPEGImages'
# "C:/Users/x992029/share/ENT Ear Photos/" # images
# #annot_dir <- "C:/Users/x992029/share/ENT Ear Photos/labels/" # labels
# # annotation file
# annot_file <- 'C:/Users/x992029/Downloads/PASCAL_VOC/pascal_train2007.json'
#"C:/Users/x992029/share/ENT Ear Photos/annotation/instances.json"


source('C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/image_library.R')

img_dir <- "C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/Test_pics_online"
#annot_dir <- "C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/labels_online" # not used?
annot_file <- "C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/Annotation_online/online.json"



#I was successful when I went to Tensorflow 1.5, cudNN 7.0, cuda 8.0.

annotations <- fromJSON(txt = annot_file)


# create image object
imageinfo <- create_image_container(annotations)

# Scale bbox
target_height <- 224
target_width <- 224
imageinfo <- scale_image_boundingbox(imageinfo, target_height, target_width)

#iimagesc <- select_scale_image(imageinfo, "scaled")

n_samples <- nrow(imageinfo)
train_indices <- sample(1:n_samples, 0.5 * n_samples)
train_data <- imageinfo[train_indices,]
validation_data <- imageinfo[-train_indices,]
#anchor_list  <- create_anchors(cells_per_row)

# Data generator
batch_size <- 10#1
image_size <- target_width # same as height
threshold <- 0.4
#n_classes <- 2
class_background <- 21


feature_extractor <- application_xception(
  include_top = FALSE,
  input_shape = c(224, 224, 3)
)

feature_extractor %>% freeze_weights()


model <- keras_model_sequential() %>%
  feature_extractor %>%
  layer_flatten() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 80, activation = "relu") %>% #80 # 512
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 4)

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



model %>% compile(
  optimizer = "adam",
  loss = "mae",
  metrics = list(custom_metric("iou", metric_iou))
)


localization_generator <-
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
      y <- array(0, dim = c(length(indices), 4))
      
      for (j in 1:length(indices)) {
        x[j, , , ] <-
          load_and_preprocess_image(data[[indices[j], "file_name"]], 
                                    target_height, target_width)
        y[j, ] <-
          data[indices[j], c("x_left_scaled",
                             "y_top_scaled",
                             "x_right_scaled",
                             "y_bottom_scaled")] %>% as.matrix()
      }
      x <- x / 255
      list(x, y)
    }
  }

train_gen <- localization_generator(train_data,
  target_height = target_height,
  target_width = target_width,
  shuffle = TRUE,
  batch_size = batch_size
)

valid_gen <- localization_generator(validation_data,
  target_height = target_height,
  target_width = target_width,
  shuffle = FALSE,
  batch_size = batch_size
)

plot_image_with_boxes <- function(file_name,
                                  object_class,
                                  box,
                                  scaled = FALSE,
                                  class_pred = NULL,
                                  box_pred = NULL) {
  img <- image_read(file.path(img_dir, file_name))
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
    text(
      box_pred[1],
      box_pred[2],
      class_pred,
      offset = 0,
      pos = 4,
      cex = 1.5,
      col = "yellow")
  dev.off()
  img %>% image_write(paste0("preds_", file_name))
  plot(img)
}

# 1:8 not 1:3
train_1_8 <- train_data[1:3, c("file_name",
                               "name",
                               "x_left_scaled",
                               "y_top_scaled",
                               "x_right_scaled",
                               "y_bottom_scaled")]


model %>% fit_generator(
    train_gen,
    epochs = 20,
    steps_per_epoch = nrow(train_data) / batch_size,
    validation_data = valid_gen,
    validation_steps = nrow(validation_data) / batch_size,
    callbacks = list(
      callback_model_checkpoint(
        file.path("C:/Users/Administrator/Documents/GitHub/Weights", "weights.{epoch:02d}-{val_loss:.2f}.hdf5")
      ),
      callback_early_stopping(patience = 2) # if validation loss hasn't improved after 2 epochs, we stop
    )
  )

k<-3
for (i in k:k) {
  preds <-
    model %>% predict(
      load_and_preprocess_image(train_1_8[i, "file_name"], 
                                target_height, target_width),
      batch_size = 1
    )
  print(preds)
  plot_image_with_boxes(train_1_8$file_name[i],
                        train_1_8$name[i],
                        train_1_8[i, 3:6] %>% as.matrix(),
                        scaled = TRUE,
                        box_pred = preds)
}