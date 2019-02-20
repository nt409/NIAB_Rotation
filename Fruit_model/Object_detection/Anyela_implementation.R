source('C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/image_library.R')


img_dir <- "C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/Test_pics_online"
annot_dir <- "C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/labels_online" # not used?
annot_file <- "C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Anyela_dropbox/Annotation_online/online.json"


#I was successful when I went to Tensorflow 1.5, cudNN 7.0, cuda 8.0.

annotations <- fromJSON(txt = annot_file)

# create image object
imageinfo <- create_image_container(annotations)

# Scale bbox
target_height <- 224
target_width <- 224
imageinfo <- scale_image_boundingbox(imageinfo, target_height, target_width)



imagesc <- select_scale_image(imageinfo, "scaled")
#   

plot_image(imagesc[1, ])


# Create anchors
cells_per_row <- 4
gridsize <- 1/cells_per_row
anchor_offset <- 1 / (cells_per_row * 2) 

anchor_list  <- create_anchors(cells_per_row)

# Data generator
batch_size <- 1
image_size <- target_width # same as height
threshold <- 0.4
n_classes <- 2
class_background <- 21


# Train set
train_gen <- ssd_generator(
  imagesc,
  target_height = target_height,
  target_width = target_width,
  shuffle = TRUE,
  batch_size = batch_size, 
  anchor_corners = anchor_list$anchor_corners
)

# break

r_res <- create_resnet_model()

class_output <-
  layer_conv_2d(
    r_res$common,
    filters = 21,
    kernel_size = 3,
    padding = "same",
    name = "class_conv"
  ) %>%
  layer_reshape(target_shape = c(16, 21), name = "class_output")



bbox_output <-
  layer_conv_2d(
    r_res$common,
    filters = 4,
    kernel_size = 3,
    padding = "same",
    name = "bbox_conv"
  ) %>%
  layer_reshape(target_shape = c(16, 4), name = "bbox_flatten") %>%
  layer_activation("tanh") %>%
  layer_lambda(
    f = function(x) {
      activation_centers <-
        (x[, , 1:2] / 2 * gridsize) + k_constant(anchor_list$anchors[, 1:2])
      activation_height_width <-
        (x[, , 3:4] / 2 + 1) * k_constant(anchor_list$anchors[, 3:4])
      activation_corners <-
        k_concatenate(
          list(
            activation_centers - activation_height_width / 2,
            activation_centers + activation_height_width / 2
          )
        )
      activation_corners
    },
    name = "bbox_output"
  )

# break

model <- keras_model(
  inputs = r_res$input,
  outputs = list(class_output, bbox_output)
)

model %>% freeze_weights()
model %>% unfreeze_weights(from = "head_conv1_1")
model
model %>% compile(
  loss = list(class_loss, bbox_loss),
  optimizer = "adam",
  metrics = list(
    class_output = custom_metric("class_loss", metric_fn = class_loss),
    bbox_output = custom_metric("bbox_loss", metric_fn = bbox_loss)
  )
)

# break

steps_per_epoch <- nrow(imagesc) / batch_size

model %>% fit_generator(
  train_gen,
  steps_per_epoch = steps_per_epoch,
  epochs = 5,
  callbacks = callback_model_checkpoint(
    "weights.{epoch:02d}-{loss:.2f}.hdf5", 
    save_weights_only = TRUE
  )
)





##########################################


#for (i in 1:2) {
  i<-3
  preds <-
    model %>% predict(
      load_and_preprocess_image(imageinfo[i, "file_name"], 
                                target_height, target_width),
      batch_size = 1
    )
  
  box_p <- c(1,10,15,30)
  class_p <- 'hello'
  
  plot_image_with_boxes(img_dir,
                        imagesc[i,],
                        scaled = TRUE,
                        box_pred = box_p,
                        class_pred = class_p)
#}
