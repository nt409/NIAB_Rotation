fruit_image_data <- function(image_path){

  img <- image_load(img_path, target_size = c(224,224))
  a <- image_to_array(img)
  
  # ensure we have a 4d tensor with single element in the batch dimension,
  # the preprocess the input for prediction using resnet50
  a <- array_reshape(a, c(1, dim(a)))
  a <- imagenet_preprocess_input(a)
  
  return(a)
}