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

# with set.seed(142) we should get in CNN_data_generator_and_model_functions
# train_indicies = [77 60 81 47 67 71 23 54 34 78 66 82 57 41 38 40 11 37 84 12 62 61  4 29 56]

run_model_trainer<-1 # do we want to train model, or just load an existing one?
params$load <- 0     # if CNN_model is already in the environment, can change to params$load <- 0 to save computational time

run_xml_to_json<-0
run_analyse_CNN_output<-1
run_analyse_SVM_output<-1

if(run_xml_to_json==1){
  system("python xmltojson.py")
}

source('Image_classifier_functions.R',echo= TRUE)
source('CNN_data_generator_and_model_functions.R',echo= TRUE)

if(run_model_trainer==1){
source('CNN_model_trainer.R',echo= TRUE) # sources 'Image_classifier_functions.R', 'parameters.R', feeds into
}

if(run_analyse_CNN_output==1){
 source('CNN_output_analysis.R',echo= TRUE)
}

source('Disease_fake_data.R',echo= TRUE) # feeds into
source('SVM.R',echo= TRUE) # feeds into

if(run_analyse_SVM_output==1){
  source('SVM_output_analysis.R',echo= TRUE)
}

