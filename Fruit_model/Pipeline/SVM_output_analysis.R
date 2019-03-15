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

disease_im_scores<- as.data.frame(as.list(d_sc)) # changed

for(j in 1:length(params$label_names)){
colnames(disease_im_scores)[j]<-paste(name_disease[[j]],"_score",sep="")
}

test_sample<- as.data.frame(t(c('location'=loc,
                                'rainfall'=rain,
                                'mean_temp'=m_t,
                                'crop_variety'=w_b,
                                'soil_type'=s_t)))
test_sample<-cbind(disease_im_scores,test_sample)
test_sample<-format_data(test_sample,name_disease) # format data so can input
head(test_sample) # preview test_sample, which is now in correct format
#########################################################
predz<-predict(svm_input,test_sample,probability=TRUE) # use svm to predict
head_of_preds<-head(attr(predz,"probabilities")) # gives predicted classes
return(list('SVM_pred' = head_of_preds,'Disease_image_scores'=disease_im_scores))
}

####################################################################################
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


####################################################################################
predictions_obtained<-function(val_data_fn){
  SVM_No_im <- list()
  #Dis_No_im <- list()
  SVM_Images_only <- list()
  #Dis_Images_only <- list()
  SVM_All_data <- list()
  Dis_All_data <- list()
  Name <- list()
  
  for(i in 1:nrow(val_data_fn)){
    output<-SVM_on_new_data(val_data_fn,i)
    Name[[i]]<-output$Disease
    SVM_No_im[[i]] <- cbind(output$No_images$SVM_pred,'Disease'=as.character(Name[[i]]))
    SVM_Images_only[[i]] <- cbind(output$Images_only$SVM_pred,'Disease'=Name[[i]])
    SVM_All_data[[i]] <- cbind(output$All$SVM_pred,'Disease'=Name[[i]])
    #Dis_No_im[[i]] <- cbind(output$No_images$Disease_image_scores,'Disease'=Name[[i]])
    #Dis_Images_only[[i]] <- cbind(output$Images_only$Disease_image_scores,'Disease'=Name[[i]])
    Dis_All_data[[i]] <- cbind(output$All$Disease_image_scores,'Disease'=as.character(Name[[i]]))
  }
  S_N_bind<-arrange(as.data.frame(do.call(rbind, SVM_No_im)),Disease)
  S_I_bind<-arrange(as.data.frame(do.call(rbind, SVM_Images_only)),Disease)
  S_A_bind<-arrange(as.data.frame(do.call(rbind, SVM_All_data)),Disease)
  #D_N_bind<-arrange(as.data.frame(do.call(rbind, Dis_No_im)),Disease)
  #D_I_bind<-arrange(as.data.frame(do.call(rbind, Dis_Images_only)),Disease)
  D_A_bind<-arrange(as.data.frame(do.call(rbind, Dis_All_data)),Disease)
  return(list('S_N' = S_N_bind,'S_I' = S_I_bind,'S_A' = S_A_bind,'Disease_image_scores' = D_A_bind))
}
