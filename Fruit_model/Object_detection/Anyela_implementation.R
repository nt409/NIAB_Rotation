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




###########################################
# me, using https://blogs.rstudio.com/tensorflow/posts/2018-11-05-naming-locating-objects/


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


################################

# 
# plot_image_with_boxes <- function(file_name,
#                                   object_class,
#                                   box,
#                                   scaled = FALSE,
#                                   class_pred = NULL,
#                                   box_pred = NULL) {
#   img <- image_read(file.path(img_dir, file_name))
#   #print(img)
#   if(scaled) img <- image_resize(img, geometry = "224x224!")
#   #print(img)
#   img <- image_draw(img)
#   #print(img)
#   x_left <- box[1]
#   y_bottom <- box[2]
#   x_right <- box[3]
#   y_top <- box[4]
#   rect(
#     x_left,
#     y_bottom,
#     x_right,
#     y_top,
#     border = "cyan",
#     lwd = 2.5
#   )
#   text(
#     x_left,
#     y_top,
#     object_class,
#     offset = 1,
#     pos = 2,
#     cex = 1.5,
#     col = "cyan"
#   )
#   #print(img)
#   if (!is.null(box_pred))
#     rect(box_pred[1],
#          box_pred[2],
#          box_pred[3],
#          box_pred[4],
#          border = "yellow",
#          lwd = 2.5)
#   if (!is.null(class_pred))
#     text(
#       box_pred[1],
#       box_pred[2],
#       class_pred,
#       offset = 0,
#       pos = 4,
#       cex = 1.5,
#       col = "yellow")
#   dev.off()
#   #print(img)
#   img %>% image_write(paste0("preds_", file_name))
#   image_draw(img)
#   #plot(img)
#   return(img) # me
# }




for (i in 1:2) {
  preds <-
    model %>% predict(
      load_and_preprocess_image(imageinfo[i, "file_name"], 
                                target_height, target_width),
      batch_size = 1
    )
  plot_image_with_boxes(img_dir, imagesc[i,],
                        box_pred = NULL)
}


trainer <- imagesc[1:2, c("file_name",
                               "name",
                               "xl",
                               "yt",
                               "xr",
                               "yb")]

#for( i in 1:2){
i<-1
preds <- model %>% predict(
  load_and_preprocess_image(trainer[i,"file_name"], 
                            target_height, target_width),
  batch_size = 1
)
library(magrittr)

nick <- plot_image_with_boxes(trainer$file_name[i],
                     trainer$name[i],
                     trainer[i,3:6]  %>% as.matrix(), # trainer[i,3:6]
                     scaled = TRUE,
                     box_pred = NULL)
#}
iris2<-trainer[1,3][1]
iris2 %>% pull(0)
