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

# list existing models, informs choice of one to load if we are loading
Model_names<-as.data.frame(list.files(path = params$folder_to_save_model_in, pattern=".h5", all.files=T, full.names=F, no.. = T))
colnames(Model_names)<-'Model_Names'
Model_names

##################################################################################
# changable things in this section
run_xml_to_json <- 0    # use new data?
run_model_trainer <- 1 # train model, or just load an existing one?
# if train, then save. If not, then load.

## pick which model to load if loading # 1, 2, or 3?
params$model_name_to_load <- as.character(Model_names[1,]) # 1, 2, or 3?

# vector with one or more components to train over.
proportion_samples_vec_input<- c(0.6) #seq(0.1,0.7,0.3)
epochs_vec_input            <- c(25) #22  #seq(10,40,15)
batch_size_vec_input        <- c(5) #seq(1,7,2) # c(5) 
layers_vec_input            <- seq(160,200,20) #c(40,80,128) #c(160,256,320,448,512) #28,192,256)

##################################################################################
params$save <- run_model_trainer     # save model? most of the time this should agree with run_model_trainer, but sometimes we might want to not save a model that we just trained
# if CNN_model is already in the environment, can change to params$load <- 0 to save computational time. If we are training a new model, don't load. Otherwise load another.
params$load <- 1 - run_model_trainer
# don't change anything from here down. All settings should be accessible from above.
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
  model<-grid_output$best_model # use model with best val_class_acc
  print(grid_output$grid_results)
  params$proportion_of_samples <- grid_output$best_params$Proportion_Samples
  params$epochs <- grid_output$best_params$Epochs
  params$batch_size <- grid_output$best_params$Batch_Size
  params$layer_units <- grid_output$best_params$Layers
  setwd(params$folder_to_save_data_in)
  saveRDS(as.data.frame(grid_output$grid_results),file=paste("grid_output",paste(layers_vec_input,collapse="_"),sep="_"))
  setwd(params$folder_containing_scripts)
}
Data_file_names<-as.data.frame(list.files(path = params$folder_to_save_data_in, pattern="grid_output", all.files=T, full.names=F, no.. = T))
colnames(Data_file_names)<-'Data_file_names'

##################################################################################
if(params$save == 1){
  x<- paste0("Disease_CNN_proportion-samples-",params$proportion_of_samples,"-epochs-",params$epochs,"-batch_size-",params$batch_size,"-layers-",params$layer_units)
  x1<-gsub("\\.","_",x)
  params$model_name_to_save<-paste0(x1,".h5") # name now more descriptive
  setwd(params$folder_to_save_model_in)
  model %>% save_model_hdf5(params$model_name_to_save)
  setwd(params$folder_containing_scripts)
}

##################################################################################
source('CNN_output_analysis.R',echo= TRUE) # analyse resulting CNN (or a loaded CNN)
table_train  # confusion matrix
table_val # confusion matrix

##################################################################################
# SVM stuff from here
source('Disease_fake_data.R',echo= TRUE) # creates dataframes to train SVM model.
source('SVM.R',echo= TRUE) # trains SVM

source('SVM_output_analysis.R',echo= TRUE) # analyse output and create function to test new data
##################################################################################
# now can predict for 'new data'
new_predictions<-predictions_obtained(val_data)
summary(new_predictions$S_N)

new_predictions$S_I
new_predictions$S_A
new_predictions$Disease_image_scores

Data_read<-list()
setwd(params$folder_to_save_data_in)
for(i in 1:nrow(Data_file_names)){
  Data_read[[i]]<-readRDS(as.character(Data_file_names[i,]))
  #Grid_table<-cbind(Data_saved,Grid_table)
}
setwd(params$folder_containing_scripts)

arrange(as.data.frame(do.call(rbind, Data_read)),desc(val_class_acc))
