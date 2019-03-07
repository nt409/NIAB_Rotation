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
#### image scores
d_sc<-list()
for(i in 1:length(params$label_names)){
d_sc[[i]]<-preds[[2]][i]
}

disease_im_scores<- as.data.frame(t(c('d1_score'=d_sc[[1]],
                                      'd2_score'=d_sc[[2]],
                                      'd3_score'=d_sc[[3]])))

colnames(disease_im_scores)<-c(name_disease[[1]],name_disease[[2]],name_disease[[3]]) # name_1,name_2,name_3 from disease fake data

test_sample<- as.data.frame(t(c('d1_score'=d_sc[[1]],
                                'd2_score'=d_sc[[2]],
                                'd3_score'=d_sc[[3]],
                                'location'=loc,
                                'rainfall'=rain,
                                'mean_temp'=m_t,
                                'crop_variety'=w_b,
                                'soil_type'=s_t)))
test_sample<-format_data(test_sample) # format data so can input
head(test_sample) # preview test_sample, which is now in correct format
#########################################################
predz<-predict(svm_input,test_sample,probability=TRUE) # use svm to predict
head_of_preds<-head(attr(predz,"probabilities")) # gives predicted classes
return(list('SVM_pred' = head_of_preds,'Disease_image_scores'=disease_im_scores))
}