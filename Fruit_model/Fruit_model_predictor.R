library(keras)
library(tidyr)
library(ggplot2)
model <- application_resnet50(weights = 'imagenet')

features <- function(a){

# make predictions then decode and print them
preds <- model %>% predict(a)
prediction<-imagenet_decode_predictions(preds, top = 3)[[1]]

return(prediction)
}