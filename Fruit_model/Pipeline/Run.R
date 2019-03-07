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


run_model_trainer<-1 # do we want to train model, or just load an existing one?
params$load <- 0     # if CNN_model is already in the environment, can change to params$load <- 0 to save computational time
params$save <- 1

run_xml_to_json<-0
run_analyse_CNN_output<-1
run_analyse_SVM_output<-1

# adds our annotations to the relevant images
if(run_xml_to_json==1){
  system("python xmltojson.py")
}
##################################################################################
# CNN
source('CNN_data_generator_and_model_functions.R',echo= TRUE) # generates data and contains functions called upon in CNN model, SVM model and the output analysis file.
# check that approx even number of each category present
sum(train_data$category_id==1)
sum(train_data$category_id==2)
sum(train_data$category_id==3)


if(run_model_trainer==1){
source('CNN_model_trainer.R',echo= TRUE) # trains CNN model
}

source('CNN_output_analysis.R',echo= TRUE) # analyse resulting CNN (or a loaded CNN)
table_train  # confusion matrix
table_val # confusion matrix


##################################################################################
# SVM stuff from here
source('Disease_fake_data.R',echo= TRUE) # creates dataframes to train SVM model.
source('SVM.R',echo= TRUE) # trains SVM

if(run_analyse_SVM_output==1){
  source('SVM_output_analysis.R',echo= TRUE) # analyse output and create function to test new data
  
# now can predict for 'new data'
  
  val_data$name[1]
  dis_number<-which(name_disease == val_data$name[1])
  
  # assume categorical variables are for the average case of disease appearance
  categorical_test_sample<-list('location'=if(crop_bias[[dis_number]]<0){"Midlands"}else{"East_Anglia"},
                                'rainfall'=rain_av[dis_number],
                                'mean_temp'=temp_av[dis_number],
                                'crop_variety'=if(crop_bias[[dis_number]]<0){"WB2"}else{"WB1"},
                                'soil_type'=if(soil_bias[[dis_number]]<0){"sandy"}else{"clay"})

  SVM_predictor(svm_no_images$svm_tuned,CNN_model,val_data,categorical_test_sample$location,categorical_test_sample$rainfall,categorical_test_sample$mean_temp,categorical_test_sample$crop_variety,categorical_test_sample$soil_type)
  SVM_predictor(svm_im_only$svm_tuned,CNN_model,val_data,categorical_test_sample$location,categorical_test_sample$rainfall,categorical_test_sample$mean_temp,categorical_test_sample$crop_variety,categorical_test_sample$soil_type)
  SVM_predictor(svm_all$svm_tuned,CNN_model,val_data,categorical_test_sample$location,categorical_test_sample$rainfall,categorical_test_sample$mean_temp,categorical_test_sample$crop_variety,categorical_test_sample$soil_type)
}
