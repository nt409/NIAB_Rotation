setwd("C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Pipeline")
source('SVM.R')
# source('Image_classifier_functions.R')
# source('CNN_image_classifier.R')

if(params$save ==1){
  CNN_model <- load_model_hdf5(params$model_name)
  CNN_model %>% summary()
}else{
  CNN_model <- model
}


#########################################################
# mock prediction with new input data
# include stage of disease??

i<-3
preds<-  CNN_model %>% predict(
  load_and_preprocess_image(testing_data[i, "file_name"], 
                            params$target_height, params$target_width),
  batch_size = 1
)
plot_image_with_boxes_single(testing_data$file_name[i],
                             testing_data$name[i],
                             testing_data[i, 3:6] %>% as.matrix(),
                             scaled = TRUE, # FALSE?
                             box_pred = preds[[1]], # should be just preds[[1]]
                             class_pred = preds[[2]]
)
####
####
# the output predictions are far too confident - scale or modify using validation set accuracy?
d1_sc<-preds[[2]][1]
d2_sc<-preds[[2]][2]
d3_sc<-preds[[2]][3]
test_sample<- as.data.frame(t(c('d1_score'=d1_sc,
                                'd2_score'=d2_sc,
                                'd3_score'=d3_sc,
                                'location'="East_Anglia",
                                'rainfall'=50,
                                'mean_temp'=16,
                                'crop_variety'="WB2",
                                'soil_type'="sandy")))
test_sample$d1_score <- as.numeric(as.character(test_sample$d1_score))
test_sample$d2_score <- as.numeric(as.character(test_sample$d2_score))
test_sample$d3_score <- as.numeric(as.character(test_sample$d3_score))
test_sample$rainfall <- as.numeric(as.character(test_sample$rainfall))
test_sample$mean_temp <- as.numeric(as.character(test_sample$mean_temp))
#### make data input orthogonal
test_sample <- mutate(test_sample,
                      Loc_EA_indic = ifelse(location=="East_Anglia",1,0),
                      Loc_Midlands_indic = ifelse(location=="Midlands",1,0),
                      WB_1_indic = ifelse(crop_variety=="WB1",1,0),
                      WB_2_indic = ifelse(crop_variety=="WB2",1,0),
                      ST_clay_indic = ifelse(soil_type=="clay",1,0),
                      ST_sandy_indic = ifelse(soil_type=="sandy",1,0)
)
head(test_sample) # preview test_sample
#########################################################
predz<-predict(svm_all$svm_tuned,test_sample,probability=TRUE)
head(attr(predz,"probabilities")) # gives predicted classes