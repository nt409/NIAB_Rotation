setwd(params$folder_containing_scripts)
source('CNN_data_generator_and_model_functions.R')
source('Image_classifier_functions.R')
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

if(params$load == 1){ # if not running CNN_model_trainer, load model previously saved
  setwd(params$folder_to_save_model_in)
  CNN_model <- load_model_hdf5(params$model_name,custom_objects=c("iou" = metric_iou))
  setwd(params$folder_containing_scripts)
}else{
  CNN_model <- model # as model is already in the environment
}

source('SVM.R')


#########################################################
# mock prediction with new input data
# include stage of disease??
i<-3
preds<-  CNN_model %>% predict(
  load_and_preprocess_image(val_data[i, "file_name"], 
                            params$target_height, params$target_width),
  batch_size = 1
)
par(mfrow = c(1,1))
plot_image_with_boxes_single(val_data$file_name[i],
                             val_data$name[i],
                             val_data[i, 3:6] %>% as.matrix(),
                             scaled = TRUE, # FALSE?
                             box_pred = preds[[1]], # should be just preds[[1]]
                             class_pred = preds[[2]]
)
####
####
# the output predictions are far too confident - scale or modify using validation set accuracy?
d1_sc<-preds[[2]][1]
d2_sc<-preds[[2]][2]
d3_sc<-preds[[2]][3]
test_sample<- as.data.frame(t(c('d1_score'=d1_sc,
                                'd2_score'=d2_sc,
                                'd3_score'=d3_sc,
                                'location'="East_Anglia",
                                'rainfall'=50,
                                'mean_temp'=16,
                                'crop_variety'="WB2",
                                'soil_type'="sandy")))
test_sample$d1_score <- as.numeric(as.character(test_sample$d1_score))
test_sample$d2_score <- as.numeric(as.character(test_sample$d2_score))
test_sample$d3_score <- as.numeric(as.character(test_sample$d3_score))
test_sample$rainfall <- as.numeric(as.character(test_sample$rainfall))
test_sample$mean_temp <- as.numeric(as.character(test_sample$mean_temp))
#### make data input orthogonal
test_sample <- mutate(test_sample,
                      Loc_EA_indic = ifelse(location=="East_Anglia",1,0),
                      Loc_Midlands_indic = ifelse(location=="Midlands",1,0),
                      WB_1_indic = ifelse(crop_variety=="WB1",1,0),
                      WB_2_indic = ifelse(crop_variety=="WB2",1,0),
                      ST_clay_indic = ifelse(soil_type=="clay",1,0),
                      ST_sandy_indic = ifelse(soil_type=="sandy",1,0)
)
head(test_sample) # preview test_sample
#########################################################
predz<-predict(svm_all$svm_tuned,test_sample,probability=TRUE)
head(attr(predz,"probabilities")) # gives predicted classes