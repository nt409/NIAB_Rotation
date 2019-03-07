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
#' 
#' #' Plot image and bounding boxes
#' #' @param image_obj image
#' 
#' plot_image <- function(img_dir, image_obj){
#'   
#'   img <- image_read(file.path(img_dir, image_obj$file_name))
#'   name <- (image_obj$name %>% str_split(pattern = ", "))[[1]]
#'   x_left <- (image_obj$xl_orig %>% str_split(pattern = ", "))[[1]]
#'   x_right <- (image_obj$xr_orig %>% str_split(pattern = ", "))[[1]]
#'   y_top <- (image_obj$yt_orig %>% str_split(pattern = ", "))[[1]]
#'   y_bottom <- (image_obj$yb_orig %>% str_split(pattern = ", "))[[1]]
#'   
#'   img <- image_draw(img)
#'   for (i in 1:image_obj$cnt) {
#'     rect(x_left[i],
#'          y_bottom[i],
#'          x_right[i],
#'          y_top[i],
#'          border = "white",
#'          lwd = 2)
#'     text(
#'       x = as.integer(x_right[i]),
#'       y = as.integer(y_top[i]),
#'       labels = name[i],
#'       offset = 1,
#'       pos = 2,
#'       cex = 1,
#'       col = "white"
#'     )
#'   }
#'   dev.off()
#'   print(img)
#'   
#' }

#' create image info container
#' @param annotation image annotation list

#' 
#' #'
#' scale image bounding box
#' @param imageinfo image info object
#' @param target_height
#' @param target_width




# hw2corners <- function(centers, height_width) {
#   cbind(centers - height_width / 2, centers + height_width / 2) %>% unname()
# }
#' 
#' #' Create grid
#' #' @param cells_per_row number of cells per row
#' create_anchors <- function(cells_per_row){
#'   
#'   gridsize <- 1/cells_per_row
#'   anchor_offset <- 1 / (cells_per_row * 2) 
#'   
#'   anchor_xs <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
#'     rep(each = cells_per_row)
#'   anchor_ys <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
#'     rep(cells_per_row)
#'   
#'   anchor_centers <- cbind(anchor_xs, anchor_ys)
#'   anchor_height_width <- matrix(1 / cells_per_row, nrow = 16, ncol = 2)
#'   anchors <- cbind(anchor_centers, anchor_height_width)
#'   anchor_corners <- hw2corners(anchor_centers, anchor_height_width)
#'   
#'   return(list(anchor_corners = anchor_corners, anchors= anchors))
#'   
#'   
#' }

