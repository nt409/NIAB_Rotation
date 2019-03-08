# setwd(params$folder_containing_scripts)
# source('CNN_data_generator_and_model_functions.R')
# source('Image_classifier_functions.R')
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

CNN_model <- load_model(params$load)

# analyse CNN output. It is far too confident of it's predictions. Is there a way to scale or use these more sensibly?

CNN_analysis <- function(testing_data){
###
###
box_predictions_individual<-list()
class_preds_individual<-list()
for (i in 1:length(testing_data$name)) {
  preds <-  CNN_model %>% predict(
      load_and_preprocess_image(testing_data[i, "file_name"], 
                                params$target_height, params$target_width),
      batch_size = 1
    )
  box_predictions_individual[[i]]<-as.data.frame(preds[[1]])
  class_preds_individual[[i]] <- as.data.frame(preds[[2]])
}
box_predictions<- do.call(rbind, box_predictions_individual)
class_preds    <- do.call(rbind, class_preds_individual)
# now have model predictions for all of our data
###
###
colnames(class_preds) <- params$label_names
class_preds$predicted_disease <- names(class_preds)[apply(class_preds, 1, which.max)]
data_labels<-as.data.frame(testing_data$name)
colnames(data_labels)<-"label"
class_preds<-cbind(class_preds,data_labels)
class_preds # predicted disease classes
#####################################################################################
# analyse regression outputs and plot boxplot
colnames(box_predictions)<-c("xl_pred","yt_pred","xr_pred","yb_pred")
corners<-cbind(testing_data[, 3:6],box_predictions)
corners$xl_error <- corners[,1]-corners[,5]
corners$yt_error <- corners[,2]-corners[,6]
corners$xr_error <- corners[,3]-corners[,7]
corners$yb_error <- corners[,4]-corners[,8]
corners # predicted bbox coordinates
#### boxplot of corner errors
par(mfrow=c(1,1))
boxplot(corners$xl_error,
        corners$yt_error,
        corners$xr_error,
        corners$yb_error,
        names=c("Left Error","Top Error","Right Error","Bottom Error"), main= 'Corner Errors')
#####################################################################################
#### plot image output for first 4 images
par(mfrow=c(2,2))
for(i in 1:4){
  plot_image_with_boxes_single(testing_data$file_name[i],
                               testing_data$name[i],
                               testing_data[i, 3:6] %>% as.matrix(),
                               scaled = TRUE,
                               box_pred = box_predictions[i,],
                               class_pred = class_preds[i,1:length(params$label_names)]
  )
}
return(list('class_predictions' = class_preds,'corners' = corners))
}

tr_analysis <- CNN_analysis(tr_data)
table_train<-table(tr_analysis$class_predictions$predicted_disease,tr_analysis$class_predictions$label)

tr_analysis$class_predictions <- arrange(tr_analysis$class_predictions,label)
tr_analysis$class_predictions

val_analysis <- CNN_analysis(val_data)
table_val<-table(val_analysis$class_predictions$predicted_disease,val_analysis$class_predictions$label)

val_analysis$class_predictions <- arrange(val_analysis$class_predictions,label)
val_analysis$class_predictions
