# setwd("C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Pipeline")
# source('Image_classifier_functions.R')

if(params$save ==1){
CNN_model <- load_model_hdf5(params$model_name)
CNN_model %>% summary()
}else{
CNN_model <- model
}

# analyse CNN output
testing_data <- validation_data[, c("file_name", # or train_data if preferred
                                "name",
                                "x_left_scaled",
                                "y_top_scaled",
                                "x_right_scaled",
                                "y_bottom_scaled")]
###
preds<-  CNN_model %>% predict(
  load_and_preprocess_image(testing_data[1, "file_name"], 
                            params$target_height, params$target_width),
  batch_size = 1
)
plot_image_with_boxes_single(testing_data$file_name[1],
                             testing_data$name[1],
                             testing_data[1, 3:6] %>% as.matrix(),
                             scaled = TRUE, # FALSE?
                             box_pred = preds[[1]], # should be just preds[[1]]
                             class_pred = preds[[2]]
)
preds[[1]]
preds[[2]]
box_predictions<-as.data.frame(preds[[1]])
class_preds <- as.data.frame(preds[[2]])
for (i in 2:length(testing_data$name)) {
  preds <-
    CNN_model %>% predict(
      load_and_preprocess_image(testing_data[i, "file_name"], 
                                params$target_height, params$target_width),
      batch_size = 1
    )
  preds2<-as.data.frame(preds[[1]])
  cl_preds2<-as.data.frame(preds[[2]])
  box_predictions<- rbind(box_predictions,preds2)
  class_preds <- rbind(class_preds,cl_preds2)
}

# testing_data[, 3:6]
# box_predictions
colnames(class_preds) <- params$label_names
class_preds1<-class_preds #[-actual_category]
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
#### boxplot
# dev.off?
boxplot(corners$xl_error,
        corners$yt_error,
        corners$xr_error,
        corners$yb_error,
        names=c("Left Error","Top Error","Right Error","Bottom Error"), main= 'Corner Errors')
#####################################################################################
#### plot image output
# dev.off?
par(mfrow=c(2,2))
for(i in 1:4){
  plot_image_with_boxes_single(testing_data$file_name[i],
                               testing_data$name[i],
                               testing_data[i, 3:6] %>% as.matrix(),
                               scaled = TRUE, # FALSE? - probably not
                               box_pred = box_predictions[i,], # should be just preds[[1]]
                               class_pred = class_preds1[i,]
  )
}