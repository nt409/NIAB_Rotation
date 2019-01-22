library(keras)
library(tidyr)
library(ggplot2)

model <- application_resnet50(weights = 'imagenet')

#model_my_own_saved <- load_model_hdf5("fruit_model.h5")
#model_my_own_saved %>% summary()

features <- function(a){

# make predictions then decode and print them
preds <- model %>% predict(a)
# preds_my_own <- model_my_own_saved %>% predict(a)
prediction <- imagenet_decode_predictions(preds, top = 3)[[1]]
# prediction_my_own <- imagenet_decode_predictions(preds_my_own, top = 3)[[1]]

return(prediction)
}