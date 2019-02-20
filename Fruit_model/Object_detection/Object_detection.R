library(keras)
library(rjson)
library(magick)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

img_dir <- "C:/Users/Administrator/Documents/Nick/VOCtrainval_06-Nov-2007/VOCdevkit/VOC2007/JPEGImages"
annot_file <- "C:/Users/Administrator/Documents/PASCAL_VOC/pascal_train2007.json"

annotations <- fromJSON(file = annot_file)
str(annotations, max.level = 1)

imageinfo <- annotations$images %>% {
  tibble(
    id = map_dbl(., "id"),
    file_name = map_chr(., "file_name"),
    image_height = map_dbl(., "height"),
    image_width = map_dbl(., "width")
  )
}

classes <- c(
  "aeroplane",
  "bicycle",
  "bird",
  "boat",
  "bottle",
  "bus",
  "car",
  "cat",
  "chair",
  "cow",
  "diningtable",
  "dog",
  "horse",
  "motorbike",
  "person",
  "pottedplant",
  "sheep",
  "sofa",
  "train",
  "tvmonitor"
)

boxinfo <- annotations$annotations %>% {
  tibble(
    image_id = map_dbl(., "image_id"),
    category_id = map_dbl(., "category_id"),
    bbox = map(., "bbox")
  )
}

boxinfo <- boxinfo %>% 
  mutate(bbox = unlist(map(.$bbox, function(x) paste(x, collapse = " "))))
boxinfo <- boxinfo %>% 
  separate(bbox, into = c("x_left", "y_top", "bbox_width", "bbox_height"))
boxinfo <- boxinfo %>% mutate_all(as.numeric)

boxinfo <- boxinfo %>% 
  mutate(y_bottom = y_top + bbox_height - 1, x_right = x_left + bbox_width - 1)

catinfo <- annotations$categories %>%  {
  tibble(id = map_dbl(., "id"), name = map_chr(., "name"))
}

imageinfo <- imageinfo %>%
  inner_join(boxinfo, by = c("id" = "image_id")) %>%
  inner_join(catinfo, by = c("category_id" = "id"))



target_height <- 224
target_width <- 224

imageinfo <- imageinfo %>% mutate(
  x_left_scaled = (x_left / image_width * target_width) %>% round(),
  x_right_scaled = (x_right / image_width * target_width) %>% round(),
  y_top_scaled = (y_top / image_height * target_height) %>% round(),
  y_bottom_scaled = (y_bottom / image_height * target_height) %>% round(),
  bbox_width_scaled =  (bbox_width / image_width * target_width) %>% round(),
  bbox_height_scaled = (bbox_height / image_height * target_height) %>% round()
)

img_data <- imageinfo[4,]
img <- image_read(file.path(img_dir, img_data$file_name))
img <- image_draw(img)
rect(
  img_data$x_left,
  img_data$y_bottom,
  img_data$x_right,
  img_data$y_top,
  border = "white",
  lwd = 2
)
text(
  img_data$x_left,
  img_data$y_top,
  img_data$name,
  offset = 1,
  pos = 2,
  cex = 1.5,
  col = "white"
)
dev.off()


imageinfo <- imageinfo %>% mutate(area = bbox_width_scaled * bbox_height_scaled)

imageinfo_maxbb <- imageinfo %>%
  group_by(id) %>%
  filter(which.max(area) == row_number())


n_samples<-2501 # me
train_indices <- sample(1:n_samples, 0.8 * n_samples)
train_data <- imageinfo_maxbb[train_indices,]
validation_data <- imageinfo_maxbb[-train_indices,]





#### single object classification




feature_extractor <-
  application_xception(
    include_top = FALSE,
    input_shape = c(224, 224, 3),
    pooling = "avg"
  )

feature_extractor %>% freeze_weights()

model <- keras_model_sequential() %>%
  feature_extractor %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 20, activation = "softmax")

model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = list("accuracy")
)


batch_size <- 10

load_and_preprocess_image <- function(image_name, target_height, target_width) {
  img_array <- image_load(
    file.path(img_dir, image_name),
    target_size = c(target_height, target_width)
  ) %>%
    image_to_array() %>%
    xception_preprocess_input() 
  dim(img_array) <- c(1, dim(img_array))
  img_array
}

classification_generator <-
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
      y <- array(0, dim = c(length(indices), 1))
      
      for (j in 1:length(indices)) {
        x[j, , , ] <-
          load_and_preprocess_image(data[[indices[j], "file_name"]],
                                    target_height, target_width)
        y[j, ] <-
          data[[indices[j], "category_id"]] - 1
      }
      x <- x / 255
      list(x, y)
    }
  }

train_gen <- classification_generator(
  train_data,
  target_height = target_height,
  target_width = target_width,
  shuffle = TRUE,
  batch_size = batch_size
)

valid_gen <- classification_generator(
  validation_data,
  target_height = target_height,
  target_width = target_width,
  shuffle = FALSE,
  batch_size = batch_size
)

model %>% fit_generator(
  train_gen,
  epochs = 20,
  steps_per_epoch = nrow(train_data) / batch_size,
  validation_data = valid_gen,
  validation_steps = nrow(validation_data) / batch_size,
  callbacks = list(
    callback_model_checkpoint(
      file.path("class_only", "weights.{epoch:02d}-{val_loss:.2f}.hdf5")
    ),
    callback_early_stopping(patience = 2)
  )
)


### Multiple object classification

image_cats <- imageinfo %>% 
  select(category_id) %>%
  mutate(category_id = category_id - 1) %>%
  pull() %>%
  to_categorical(num_classes = 20)

image_cats <- data.frame(image_cats) %>%
  add_column(file_name = imageinfo$file_name, .before = TRUE)

image_cats <- image_cats %>% 
  group_by(file_name) %>% 
  summarise_all(.funs = funs(max))

n_samples <- nrow(image_cats)
train_indices <- sample(1:n_samples, 0.8 * n_samples)
train_data <- image_cats[train_indices,]
validation_data <- image_cats[-train_indices,]

classification_generator <- 
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
      y <- array(0, dim = c(length(indices), 20))
      
      for (j in 1:length(indices)) {
        x[j, , , ] <-
          load_and_preprocess_image(data[[indices[j], "file_name"]], 
                                    target_height, target_width)
        y[j, ] <-
          data[indices[j], 2:21] %>% as.matrix()
      }
      x <- x / 255
      list(x, y)
    }
  }

train_gen <- classification_generator(
  train_data,
  target_height = target_height,
  target_width = target_width,
  shuffle = TRUE,
  batch_size = batch_size
)

valid_gen <- classification_generator(
  validation_data,
  target_height = target_height,
  target_width = target_width,
  shuffle = FALSE,
  batch_size = batch_size
)

feature_extractor <-
  application_xception(
    include_top = FALSE,
    input_shape = c(224, 224, 3),
    pooling = "avg"
  )

feature_extractor %>% freeze_weights()

model <- keras_model_sequential() %>%
  feature_extractor %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 20, activation = "sigmoid")

model %>% compile(optimizer = "adam",
                  loss = "binary_crossentropy",
                  metrics = list("accuracy"))


model %>% fit_generator(
  train_gen,
  epochs = 20,
  steps_per_epoch = nrow(train_data) / batch_size,
  validation_data = valid_gen,
  validation_steps = nrow(validation_data) / batch_size,
  callbacks = list(
    callback_model_checkpoint(
      file.path("multiclass", "weights.{epoch:02d}-{val_loss:.2f}.hdf5")
    ),
    callback_early_stopping(patience = 2)
  )
)


