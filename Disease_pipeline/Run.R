source('C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Disease_pipeline/parameters.R')
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
run_xml_to_json   <- 0    # use new data?
run_model_trainer <- 1 # train model, or just load an existing one?
# if train, then save. If not, then load.

## pick which model to load if loading # 1, 2, or 3?
params$model_name_to_load <- as.character(Model_names[1,]) # 1, 2, or 3?

# vector with one or more components to train over.
proportion_samples_vec_input<- c(0.6) #seq(0.1,0.7,0.3)
epochs_vec_input            <- c(2) #25  #seq(10,40,15)
batch_size_vec_input        <- c(5) #seq(1,7,2) # c(5) 
layers_vec_input            <- c(20)#seq(260,280,20) #c(40,80,128) #c(160,256,320,448,512) #28,192,256)

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


#FIXME if I run just the model trainer bit below, for the same parameters, the model output is different every time. Further, the keras output quoting the model accuracy seems to disagree with the output when I test the model on the data set producing table_train and table_val.
# this relates to the functions grid(), model_trainer(), formulate_model().
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
  x<- paste0("Disease_CNN_proportion-samples-",params$proportion_of_samples,"-epochs-",params$epochs,"-batch_size-",params$batch_size,"-layers-",params$layer_units,"-TF_seed-",params$TF_seed,"-R_seed-",params$seed,"-count-",grid_output$best_count) # best_count means that we know which iteration the model was trained on, since we set a seed at the start of the session and we want repeatability
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


# see how different parameter values have performed
Data_read<-list()
setwd(params$folder_to_save_data_in)
for(i in 1:nrow(Data_file_names)){
  Data_read[[i]]<-readRDS(as.character(Data_file_names[i,]))
  #Grid_table<-cbind(Data_saved,Grid_table)
}
setwd(params$folder_containing_scripts)

grid_resulting_table<-arrange(as.data.frame(do.call(rbind, Data_read)),desc(val_class_acc))
grid_resulting_table
##################################################################################
# SVM stuff from here
source('Disease_fake_data.R',echo= TRUE) # creates dataframes to train SVM model.
source('SVM.R',echo= TRUE) # trains SVM
head(data_use)
source('SVM_output_analysis.R',echo= TRUE) # analyse output and create function to test new data
##################################################################################
# now can predict for 'new data'
new_predictions<-predictions_obtained(val_data)

for(i in 1:(ncol(new_predictions$S_N)-1)){new_predictions$S_N[,i]<-as.numeric(as.character(new_predictions$S_N[,i]))}
for(i in 1:(ncol(new_predictions$S_I)-2)){new_predictions$S_I[,i]<-as.numeric(as.character(new_predictions$S_I[,i]))}
for(i in 1:(ncol(new_predictions$S_A)-2)){new_predictions$S_A[,i]<-as.numeric(as.character(new_predictions$S_A[,i]))}
for(i in 1:(ncol(new_predictions$Disease_image_scores)-2)){new_predictions$Disease_image_scores[,i]<-as.numeric(as.character(new_predictions$Disease_image_scores[,i]))}
par(mfrow=c(2,2))
boxplot(filter(new_predictions$Disease_image_scores,Disease=="BS")[,1:3],main="CNN, BS")
boxplot(filter(new_predictions$S_I,Disease=="BS")[,1:3],main="Just images")
boxplot(filter(new_predictions$S_N,Disease=="BS")[,1:3],main="Just descriptors")
boxplot(filter(new_predictions$S_A,Disease=="BS")[,1:3],main="All")
# par(mfrow=c(2,2))
boxplot(filter(new_predictions$Disease_image_scores,Disease=="MSD")[,1:3],main="CNN, MSD")
boxplot(filter(new_predictions$S_I,Disease=="MSD")[,1:3],main="Just images")
boxplot(filter(new_predictions$S_N,Disease=="MSD")[,1:3],main="Just descriptors")
boxplot(filter(new_predictions$S_A,Disease=="MSD")[,1:3],main="All")
# par(mfrow=c(2,2))
boxplot(filter(new_predictions$Disease_image_scores,Disease=="YR")[,1:3],main="CNN, YR")
boxplot(filter(new_predictions$S_I,Disease=="YR")[,1:3],main="Just images")
boxplot(filter(new_predictions$S_N,Disease=="YR")[,1:3],main="Just descriptors")
boxplot(filter(new_predictions$S_A,Disease=="YR")[,1:3],main="All")
