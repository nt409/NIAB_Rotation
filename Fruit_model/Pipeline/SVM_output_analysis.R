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

# setwd(params$folder_containing_scripts)
# source('CNN_data_generator_and_model_functions.R')
# source('Image_classifier_functions.R')

# either loads saved model or uses one in current environment
CNN_model <- load_model(params$load)

# source('SVM.R')

#########################################################
# mock prediction with new input data
# include stage of disease??
SVM_predictor <- function(svm_input,CNN_model_input,v_data,loc,rain,m_t,w_b,s_t){
preds<-  CNN_model_input %>% predict(
  load_and_preprocess_image(v_data[1, "file_name"], 
                            params$target_height, params$target_width),
  batch_size = 1
)
par(mfrow = c(1,1))
plot_image_with_boxes_single(v_data$file_name[1],
                             v_data$name[1],
                             v_data[1, 3:6] %>% as.matrix(),
                             scaled = TRUE, # FALSE?
                             box_pred = preds[[1]], # should be just preds[[1]]
                             class_pred = preds[[2]]
)
####
d1_sc<-preds[[2]][1]
d2_sc<-preds[[2]][2]
d3_sc<-preds[[2]][3]

test_sample<- as.data.frame(t(c('d1_score'=d1_sc,
                                'd2_score'=d2_sc,
                                'd3_score'=d3_sc,
                                'location'=loc,
                                'rainfall'=rain,
                                'mean_temp'=m_t,
                                'crop_variety'=w_b,
                                'soil_type'=s_t)))
test_sample<-format_data(test_sample)
head(test_sample) # preview test_sample
#########################################################
predz<-predict(svm_input,test_sample,probability=TRUE)
head_of_preds<-head(attr(predz,"probabilities")) # gives predicted classes
return(head_of_preds)
}

categorical_test_sample<-list('location'="East_Anglia",
                 'rainfall'=50,
                 'mean_temp'=16,
                 'crop_variety'="WB2",
                 'soil_type'="sandy")

SVM_predictor(svm_all$svm_tuned,CNN_model,val_data,categorical_test_sample$location,categorical_test_sample$rainfall,categorical_test_sample$mean_temp,categorical_test_sample$crop_variety,categorical_test_sample$soil_type)