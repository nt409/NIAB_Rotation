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


#' Plot image and bounding boxes
#' @param image_obj image

plot_image <- function(img_dir, image_obj){
  
  img <- image_read(file.path(img_dir, image_obj$file_name))
  name <- (image_obj$name %>% str_split(pattern = ", "))[[1]]
  x_left <- (image_obj$xl_orig %>% str_split(pattern = ", "))[[1]]
  x_right <- (image_obj$xr_orig %>% str_split(pattern = ", "))[[1]]
  y_top <- (image_obj$yt_orig %>% str_split(pattern = ", "))[[1]]
  y_bottom <- (image_obj$yb_orig %>% str_split(pattern = ", "))[[1]]
  
  img <- image_draw(img)
  for (i in 1:image_obj$cnt) {
    rect(x_left[i],
         y_bottom[i],
         x_right[i],
         y_top[i],
         border = "white",
         lwd = 2)
    text(
      x = as.integer(x_right[i]),
      y = as.integer(y_top[i]),
      labels = name[i],
      offset = 1,
      pos = 2,
      cex = 1,
      col = "white"
    )
  }
  dev.off()
  print(img)
  
}

#' create image info container
#' @param annotation image annotation list

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


#' scale image bounding box
#' @param imageinfo image info object
#' @param target_height
#' @param target_width
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

#' scale image bounding box
#' @param imageinfo image info object
select_scale_image <- function(imageinfo, kw){
  
  imagesc <- imageinfo %>%
    select(category_id,
           file_name,
           name,
           x_left,
           y_top,
           x_right,
           y_bottom,
           ends_with(kw))
  
  imagesc <- imagesc %>%
    group_by(file_name) %>%
    summarise(
      categories = toString(category_id),
      name = toString(name),
      xl = toString(x_left_scaled),
      yt = toString(y_top_scaled),
      xr = toString(x_right_scaled),
      yb = toString(y_bottom_scaled),
      xl_orig = toString(x_left),
      yt_orig = toString(y_top),
      xr_orig = toString(x_right),
      yb_orig = toString(y_bottom),
      cnt = n()
    )
  
  return(imagesc)
}

# overlaps shape is: number of ground truth objects * number of grid cells
map_to_ground_truth <- function(overlaps) {
  
  # for each ground truth object, find maximally overlapping cell (crit. 1)
  # measure of overlap, shape: number of ground truth objects
  prior_overlap <- apply(overlaps, 1, max)
  # which cell is this, for each object
  prior_idx <- apply(overlaps, 1, which.max)
  
  # for each grid cell, what object does it overlap with most (crit. 2)
  # measure of overlap, shape: number of grid cells
  gt_overlap <-  apply(overlaps, 2, max)
  # which object is this, for each cell
  gt_idx <- apply(overlaps, 2, which.max)
  
  # set all definitely overlapping cells to respective object (crit. 1)
  gt_overlap[prior_idx] <- 1.99
  
  # now still set all others to best match by crit. 2
  # actually it's other way round, we start from (2) and overwrite with (1)
  for (i in 1:length(prior_idx)) {
    # iterate over all cells "absolutely assigned"
    p <- prior_idx[i] # get respective grid cell
    gt_idx[p] <- i # assign this cell the object number
  }
  
  # return: for each grid cell, object it overlaps with most + measure of overlap
  list(gt_overlap, gt_idx)
  
}

# compute IOU
jaccard <- function(bbox, anchor_corners) {
  bbox <- k_constant(bbox)
  anchor_corners <- k_constant(anchor_corners)
  intersection <- intersect(bbox, anchor_corners)
  union <-
    k_expand_dims(box_area(bbox), axis = 2)  + k_expand_dims(box_area(anchor_corners), axis = 1) - intersection
  res <- intersection / union
  res %>% k_eval()
}

# compute intersection for IOU
intersect <- function(box1, box2) {
  box1_a <- box1[, 3:4] %>% k_expand_dims(axis = 2)
  box2_a <- box2[, 3:4] %>% k_expand_dims(axis = 1)
  max_xy <- k_minimum(box1_a, box2_a)
  
  box1_b <- box1[, 1:2] %>% k_expand_dims(axis = 2)
  box2_b <- box2[, 1:2] %>% k_expand_dims(axis = 1)
  min_xy <- k_maximum(box1_b, box2_b)
  
  intersection <- k_clip(max_xy - min_xy, min = 0, max = Inf)
  intersection[, , 1] * intersection[, , 2]
  
}

box_area <- function(box) {
  (box[, 3] - box[, 1]) * (box[, 4] - box[, 2]) 
}



ssd_generator <- function(data, target_height, target_width, shuffle,
           batch_size, anchor_corners) {
  
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
      y1 <- array(0, dim = c(length(indices), 16))
      y2 <- array(0, dim = c(length(indices), 16, 4))
      
      for (j in 1:length(indices)) {
        x[j, , , ] <-
          load_and_preprocess_image(data[[indices[j], "file_name"]], target_height, target_width)
        
        class_string <- data[indices[j], ]$categories
        xl_string <- data[indices[j], ]$xl
        yt_string <- data[indices[j], ]$yt
        xr_string <- data[indices[j], ]$xr
        yb_string <- data[indices[j], ]$yb
        
        classes <-  str_split(class_string, pattern = ", ")[[1]]
        xl <-
          str_split(xl_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        yt <-
          str_split(yt_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        xr <-
          str_split(xr_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        yb <-
          str_split(yb_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        
        # rows are objects, columns are coordinates (xl, yt, xr, yb)
        # anchor_corners are 16 rows with corresponding coordinates
        bbox <- cbind(xl, yt, xr, yb)
        overlaps <- jaccard(bbox, anchor_corners)
        
        c(gt_overlap, gt_idx) %<-% map_to_ground_truth(overlaps)
        gt_class <- classes[gt_idx]
        
        pos <- gt_overlap > params$threshold
        gt_class[gt_overlap < params$threshold] <- params$class_background
        
        # columns correspond to objects
        boxes <- rbind(xl, yt, xr, yb)
        # columns correspond to object boxes according to gt_idx
        gt_bbox <- boxes[, gt_idx]
        # set those with non-sufficient overlap to 0
        gt_bbox[, !pos] <- 0
        gt_bbox <- gt_bbox %>% t()
        
        y1[j, ] <- as.integer(gt_class) - 1
        y2[j, , ] <- gt_bbox
        
      }
      
      x <- x %>% imagenet_preprocess_input()
      y1 <- y1 %>% to_categorical(num_classes = params$class_background)
      list(x, list(y1, y2))
    }
}


feature_extractor <- application_resnet50(
 include_top = FALSE,
 input_shape = c(224, 224, 3)
)


create_resnet_model <- function(){

  input <- feature_extractor$input

  common <- feature_extractor$output %>%
    layer_conv_2d(
      filters = 256,
      kernel_size = 3,
      padding = "same",
      activation = "relu",
      name = "head_conv1_1"
    ) %>%
    layer_batch_normalization() %>%
    layer_conv_2d(
      filters = 256,
      kernel_size = 3,
      padding = "same",
      activation = "relu",
      name = "head_conv1_2"
    ) %>%
    layer_batch_normalization() %>%
    layer_conv_2d(
      filters = 256,
      kernel_size = 3,
      padding = "same",
      activation = "relu",
      name = "head_conv1_3"
    ) %>%
    layer_batch_normalization() %>%
    layer_conv_2d(
      filters = 256,
      kernel_size = 3,
      strides = 2,
      padding = "same",
      activation = "relu",
      name = "head_conv2"
    ) %>%
    layer_batch_normalization()
  
  return(list(common = common, input = input))
}


# shapes are batch_size * 16 * 21
class_loss <- function(y_true, y_pred) {
  
  n_classes <- 2
  class_loss  <-
    tf$nn$sigmoid_cross_entropy_with_logits(labels = y_true, logits = y_pred)
  
  class_loss <-
    tf$reduce_sum(class_loss) / tf$cast(n_classes + 1, "float32")
  
  class_loss
}

# shapes are batch_size * 16 * 4
bbox_loss <- function(y_true, y_pred) {
  
  # calculate localization loss for all boxes where ground truth was assigned some overlap
  # calculate mask
  pos <- y_true[, , 1] + y_true[, , 3] > 0
  pos <-
    pos %>% k_cast(tf$float32) %>% k_reshape(shape = c(params$batch_size, 16, 1))
  pos <-
    tf$tile(pos, multiples = k_constant(c(1L, 1L, 4L), dtype = tf$int32))
  
  diff <- y_pred - y_true
  # mask out irrelevant activations
  diff <- diff %>% tf$multiply(pos)
  
  loc_loss <- diff %>% tf$abs() %>% tf$reduce_mean()
  loc_loss * 100
}


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


hw2corners <- function(centers, height_width) {
  cbind(centers - height_width / 2, centers + height_width / 2) %>% unname()
}

#' Create grid
#' @param cells_per_row number of cells per row
create_anchors <- function(cells_per_row){
  
  gridsize <- 1/cells_per_row
  anchor_offset <- 1 / (cells_per_row * 2) 
  
  anchor_xs <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
    rep(each = cells_per_row)
  anchor_ys <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
    rep(cells_per_row)
  
  anchor_centers <- cbind(anchor_xs, anchor_ys)
  anchor_height_width <- matrix(1 / cells_per_row, nrow = 16, ncol = 2)
  anchors <- cbind(anchor_centers, anchor_height_width)
  anchor_corners <- hw2corners(anchor_centers, anchor_height_width)
  
  return(list(anchor_corners = anchor_corners, anchors= anchors))
  
  
}

#' Plot image from test set with predicted boxes and classes
#' @img_dir folder where image is located
#' @scaled = FALSE,
#' @class_pred predicted class 
#' @box_pred predicted boxes
plot_image_with_boxes1 <- function(img_dir, image_obj,
                                  scaled = FALSE,
                                  class_pred = NULL,
                                  box_pred = NULL) {
  
  
  img <- image_read(file.path(img_dir, image_obj$file_name))
  name <- (image_obj$name %>% str_split(pattern = ", "))[[1]]
  
 
  x_left <- (image_obj$xl_orig %>% str_split(pattern = ", "))[[1]]
  x_right <- (image_obj$xr_orig %>% str_split(pattern = ", "))[[1]]
  y_top <- (image_obj$yt_orig %>% str_split(pattern = ", "))[[1]]
  y_bottom <- (image_obj$yb_orig %>% str_split(pattern = ", "))[[1]]
  
  img <- image_draw(img)
  for (i in 1:image_obj$cnt) {
    rect(x_left[i],
         y_bottom[i],
         x_right[i],
         y_top[i],
         border = "white",
         lwd = 2)
    text(
      x = as.integer(x_right[i]),
      y = as.integer(y_top[i]),
      labels = name[i],
      offset = 1,
      pos = 2,
      cex = 1,
      col = "white"
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
     #dev.off()
     #img %>% image_write(paste0("preds_", file_name))
     #plot(img)
  }
}


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
    text(
      box_pred[1],
      box_pred[2],
      which(class_pred == max(class_pred)),
      offset = 0,
      pos = 4,
      cex = 1.5,
      col = "yellow")
  dev.off()
  img %>% image_write(paste0("preds_", file_name))
  plot(img)
}




