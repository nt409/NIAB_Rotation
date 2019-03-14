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
run_model_trainer<-1 # train model, or just load an existing one?
params$save <- 0     # save model?
params$load <- 1     # if CNN_model is already in the environment, can change to params$load <- 0 to save computational time
run_xml_to_json<-0

####
params$epochs<-7
proportion_samples_vec_input<- c(1)#seq(0.1,0.7,0.3)
epochs_vec_input<-c(2)#seq(10,40,15)
batch_size_vec_input<- c(1)# seq(1,7,3)
layers_vec_input<-c(32)


##################################################################################
# adds our annotations to the relevant images
if(run_xml_to_json==1){
# source('save_to_xml.R',echo= TRUE) # with semi-automated json, not using
system("python xmltojson.py")
}
##################################################################################
# CNN
source('CNN_data_generator_and_model_functions.R',echo= TRUE) # generates data and contains functions called upon in CNN model, SVM model and the output analysis file.
####
# check that approx even number of each category present
for(k in 1:length(params$label_names)){
print(paste("Number of training images in category",params$label_names[k],"is",sum(train_data$category_id==k)))
}

if(run_model_trainer==1){
  grid_output<-grid(proportion_samples_vec_input,epochs_vec_input,batch_size_vec_input,layers_vec_input)
  grid_output$grid_results
  model<-grid_output$best_model # use model with best val_class_acc
}


##########
# save?
if(params$save == 1){
  setwd(params$folder_to_save_images_in)
  model%>% save_model_hdf5(params$model_name)
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

# test properly tomorrow
##########################################
SVM_on_new_data <- function(data_to_use,im_no){
dis_name<- data_to_use$name[im_no]
dis_number<-which(name_disease == dis_name)

# assume categorical variables are for the average case of disease appearance
categ_data_to_use<-list('location'=if(crop_bias[[dis_number]]<0){"Midlands"}else{"East_Anglia"},
                              'rainfall'=rain_av[dis_number],
                              'mean_temp'=temp_av[dis_number],
                              'crop_variety'=if(crop_bias[[dis_number]]<0){"WB2"}else{"WB1"},
                              'soil_type'=if(soil_bias[[dis_number]]<0){"sandy"}else{"clay"})


res_no_im<-SVM_predictor(svm_no_images$svm_tuned,CNN_model,data_to_use,categ_data_to_use$location,categ_data_to_use$rainfall,categ_data_to_use$mean_temp,categ_data_to_use$crop_variety,categ_data_to_use$soil_type)
res_im_only<-SVM_predictor(svm_im_only$svm_tuned,CNN_model,data_to_use,categ_data_to_use$location,categ_data_to_use$rainfall,categ_data_to_use$mean_temp,categ_data_to_use$crop_variety,categ_data_to_use$soil_type)
res_all<-SVM_predictor(svm_all$svm_tuned,CNN_model,data_to_use,categ_data_to_use$location,categ_data_to_use$rainfall,categ_data_to_use$mean_temp,categ_data_to_use$crop_variety,categ_data_to_use$soil_type)
return(list('No_images' = res_no_im, 'Images_only' = res_im_only, 'All' = res_all, 'Disease' = dis_name))
}

SVM_No_im <- list()
Dis_No_im <- list()
SVM_Images_only <- list()
Dis_Images_only <- list()
SVM_All_data <- list()
Dis_All_data <- list()
Name <- list()

for(i in 1:nrow(val_data)){
output<-SVM_on_new_data(val_data,i)
Name[[i]]<-output$Disease
SVM_No_im[[i]] <- cbind(output$No_images$SVM_pred,'Disease'=as.character(Name[[i]]))
SVM_Images_only[[i]] <- cbind(output$Images_only$SVM_pred,'Disease'=Name[[i]])
SVM_All_data[[i]] <- cbind(output$All$SVM_pred,'Disease'=Name[[i]])
Dis_No_im[[i]] <- cbind(output$No_images$Disease_image_scores,'Disease'=Name[[i]])
Dis_Images_only[[i]] <- cbind(output$Images_only$Disease_image_scores,'Disease'=Name[[i]])
Dis_All_data[[i]] <- cbind(output$All$Disease_image_scores,'Disease'=as.character(Name[[i]]))
}
S_N_bind<-arrange(as.data.frame(do.call(rbind, SVM_No_im)),Disease)
S_I_bind<-arrange(as.data.frame(do.call(rbind, SVM_Images_only)),Disease)
S_A_bind<-arrange(as.data.frame(do.call(rbind, SVM_All_data)),Disease)
D_N_bind<-arrange(as.data.frame(do.call(rbind, Dis_No_im)),Disease)
D_I_bind<-arrange(as.data.frame(do.call(rbind, Dis_Images_only)),Disease)
D_A_bind<-arrange(as.data.frame(do.call(rbind, Dis_All_data)),Disease)
