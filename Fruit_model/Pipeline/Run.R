source('C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Pipeline/parameters.R')
setwd(params$folder_containing_scripts)
### load libraries
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

# with set.seed(12) we should get in CNN_data_generator_and_model_functions
# train_indicies = [6 70 80 23 14  3 15 51  2  1 30 62 28 74 20 32 33 38 46  8 84 52  7 45 82]

run_model_trainer<-0 # do we want to train model, or just load an existing one?
params$load <- 1     # if CNN_model is already in the environment, can change to params$load <- 0 to save computational time
params$save <- 0

run_xml_to_json<-0
run_analyse_CNN_output<-1
run_analyse_SVM_output<-1

if(run_xml_to_json==1){
  system("python xmltojson.py")
}

##################################################################################
# CNN
# source('Image_classifier_functions_used.R',echo= TRUE)
source('CNN_data_generator_and_model_functions.R',echo= TRUE)

if(run_model_trainer==1){
source('CNN_model_trainer.R',echo= TRUE) # sources 'Image_classifier_functions.R', 'parameters.R', feeds into
}

source('CNN_output_analysis.R',echo= TRUE)
##################################################################################
# SVM stuff from here
source('Disease_fake_data.R',echo= TRUE) # feeds into
source('SVM.R',echo= TRUE) # feeds into

if(run_analyse_SVM_output==1){
  source('SVM_output_analysis.R',echo= TRUE)
  
# now can predict for 'new data'
  categorical_test_sample<-list('location'="East_Anglia",
                                'rainfall'=50,
                                'mean_temp'=16,
                                'crop_variety'="WB2",
                                'soil_type'="sandy")

  SVM_predictor(svm_all$svm_tuned,CNN_model,val_data,categorical_test_sample$location,categorical_test_sample$rainfall,categorical_test_sample$mean_temp,categorical_test_sample$crop_variety,categorical_test_sample$soil_type)
}
