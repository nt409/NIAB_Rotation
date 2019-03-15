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

##################################################################################
run_xml_to_json<-0

run_model_trainer<-0 # train model, or just load an existing one?
params$save <- 0     # save model? most of the time this should agree with run_model_trainer

params$load <- 0     # if CNN_model is already in the environment, can change to params$load <- 0 to save computational time
## pick which model to load if loading
x<-as.data.frame(list.files(path = params$folder_to_save_model_in, pattern=".h5", all.files=T, full.names=F, no.. = T))
colnames(x)<-'Model_Names'
x
params$model_name_to_load <- list.files(path = params$folder_to_save_model_in, pattern=".h5", all.files=T, full.names=F, no.. = T)[1]

####
proportion_samples_vec_input<- c(0.8) #seq(0.1,0.7,0.3)
epochs_vec_input            <- c(20)  #seq(10,40,15)
batch_size_vec_input        <- c(4) #seq(1,7,2)
layers_vec_input            <- c(256)


##################################################################################
# adds our annotations to the relevant images
if(run_xml_to_json==1){ 
# source('save_to_xml.R',echo= TRUE) # with semi-automated json, not using
system("python xmltojson.py")
}
##################################################################################
# CNN
source('CNN_data_generator_and_model_functions.R',echo= TRUE) # generates data and contains functions called upon in CNN model, SVM model and the output analysis file.

# check that approx even number of each category present
for(k in 1:length(params$label_names)){
print(paste("Number of training images in category",params$label_names[k],"is",sum(train_data$category_id==k)))
}

if(run_model_trainer==1){
  grid_output<-grid(proportion_samples_vec_input,epochs_vec_input,batch_size_vec_input,layers_vec_input)
  grid_output$grid_results
  model<-grid_output$best_model # use model with best val_class_acc
  params$proportion_of_samples <- grid_output$best_params$Proportion_Samples
  params$epochs <- grid_output$best_params$Epochs
  params$batch_size <- grid_output$best_params$Batch_Size
  params$layer_units <- grid_output$best_params$Layers
}


##########
# save? problem with this??
if(params$save == 1){
  x<- paste0("Disease_CNN_proportion-samples-",params$proportion_of_samples,"-epochs-",params$epochs,"-batch_size-",params$batch_size,"-layers-",params$layer_units)
  x1<-gsub("\\.","_",x)
  params$model_name_to_save<-paste0(x1,".h5") # name now more descriptive
  setwd(params$folder_to_save_model_in)
  model %>% save_model_hdf5(params$model_name_to_save)
  setwd(params$folder_containing_scripts)
}

####
source('CNN_output_analysis.R',echo= TRUE) # analyse resulting CNN (or a loaded CNN)
table_train  # confusion matrix
table_val # confusion matrix

##################################################################################
# SVM stuff from here
source('Disease_fake_data.R',echo= TRUE) # creates dataframes to train SVM model.
source('SVM.R',echo= TRUE) # trains SVM

#####
source('SVM_output_analysis.R',echo= TRUE) # analyse output and create function to test new data
##################################################################################
# now can predict for 'new data'

new_predictions<-predictions_obtained(val_data)
summary(new_predictions$S_N)
new_predictions$S_I
new_predictions$S_A
new_predictions$Disease_image_scores
summary(new_predictions$S_A)
