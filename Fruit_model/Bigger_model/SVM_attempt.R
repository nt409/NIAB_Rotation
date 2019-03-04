library(e1071)
# attach(iris)

source('~/GitHub/NIAB_Rotation/Fruit_model/Bigger_model/Disease_fake_data.R')
# do we need prevalence data??

################################################
#fn to create svm model, and a tuned svm model. Could improve tuning.
svm_creator<-function(data_to_use,predictor_data_to_use){
  svm_model_within_function <- svm(category_id~., data=data_to_use,probability=TRUE)
  summary(svm_model_within_function)
  
  pred_within_function <- predict(svm_model_within_function,predictor_data_to_use)
  
  svm_tune_within_function <- tune(svm, train.x=predictor_data_to_use, train.y=class_labels,
                                   kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
  
  print(svm_tune_within_function)
  
  ##
  resulting_cost_within_function<-as.numeric(svm_tune_within_function$best.parameters[1]) # get from svm_tune
  resulting_gamma_within_function<-as.numeric(svm_tune_within_function$best.parameters[2]) # get from svm_tune
  ##
  
  svm_model_after_tune_within_function <- svm(category_id ~ ., data=data_to_use, kernel="radial", cost=resulting_cost_within_function, gamma=resulting_gamma_within_function,probability=TRUE)
  summary(svm_model_after_tune_within_function)
  
  pred_within_function_tuned <- predict(svm_model_after_tune_within_function,predictor_data_to_use)
  return(list('svm'=svm_model_within_function,'svm_tuned'=svm_model_after_tune_within_function,'pred'=pred_within_function,'pred_tuned'=pred_within_function_tuned,'tune'=svm_tune_within_function))
}
################################################

data <- dis_data2 # comes in from Disease_fake_data.R
category_id<-dis_data2$disease
names(data)[names(data)=="disease"] <- "category_id"
#########################################################
raw_data <- subset(data, select = c(category_id,d1_score,d2_score,d3_score,location,rainfall,mean_temp,crop_variety,soil_type))
data_use<- subset(data, select = c(-location,-crop_variety,-soil_type))
predictor_data <- subset(data_use, select = c(-category_id))
class_labels <- category_id

head(data_use)
################################################
svm_all<-svm_creator(data_use,predictor_data)

# table(svm_all$pred,class_labels) # confusion matrices
# table(svm_all$pred_tuned,class_labels) # confusion matrices

################################################
# without disease images
data_use_without_images<- subset(data, select = c(-location,-crop_variety,-soil_type,-d1_score,-d2_score,-d3_score))
predictor_data_without_images <- subset(data_use_without_images, select = c(-category_id))

svm_no_images<-svm_creator(data_use_without_images,predictor_data_without_images)

# table(svm_no_images$pred,class_labels) # confusion matrices
# table(svm_no_images$pred_tuned,class_labels) # confusion matrices

################################################
# only with images
data_use_images_only<- subset(data, select = c(category_id,d1_score,d2_score,d3_score))
predictor_data_images_only <- subset(data_use_images_only, select = c(-category_id))

svm_im_only<-svm_creator(data_use_images_only,predictor_data_images_only)

# table(svm_im_only$pred,class_labels) # confusion matrices
# table(svm_im_only$pred_tuned,class_labels) # confusion matrices

################################################
# comparison:
table(svm_all$pred,class_labels)
table(svm_no_images$pred,class_labels)
table(svm_im_only$pred,class_labels)

table(svm_all$pred_tuned,class_labels)
table(svm_no_images$pred_tuned,class_labels)
table(svm_im_only$pred_tuned,class_labels)

svm_all$tune$best.performance
svm_no_images$tune$best.performance
svm_im_only$tune$best.performance

################################################
### plots don't look right?
# dev.off() # allows new plot to open
# par(mfrow=c(2,2))
plot(svm_all$svm,data,mean_temp~rainfall,fill=TRUE,color.palette = terrain.colors)
#,slice = list(category_id = 1,Loc_Midlands_indic = 0,Loc_EA_indic = 1,WB_1_indic=0,WB_2_indic=1,ST_clay_indic=0,ST_sandy_indic=1,d1_score>0.5),color.palette = terrain.colors)
plot(svm_no_images$svm,data,mean_temp~rainfall,fill=TRUE,color.palette = terrain.colors)
plot(svm_all$svm_tuned,data,d1_score~rainfall,fill=TRUE)
plot(svm_all$svm_tuned,data,d1_score~d2_score,fill=FALSE)
################################################
# mock prediction with new input data
preds<-  model %>% predict(
  load_and_preprocess_image(train_1_8[1, "file_name"], 
                            params$target_height, params$target_width),
  batch_size = 1
)
plot_image_with_boxes_single(train_1_8$file_name[1],
                             train_1_8$name[1],
                             train_1_8[1, 3:6] %>% as.matrix(),
                             scaled = TRUE, # FALSE?
                             box_pred = preds[[1]], # should be just preds[[1]]
                             class_pred = preds[[2]]
)


# the output predictions are far too confident - scale or modify using validation set accuracy?
d1_sc<-preds[[2]][1]
d2_sc<-preds[[2]][2]
d3_sc<-0
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

test_sample <- mutate(test_sample,
                    Loc_EA_indic = ifelse(location=="East_Anglia",1,0),
                    Loc_Midlands_indic = ifelse(location=="Midlands",1,0),
                    WB_1_indic = ifelse(crop_variety=="WB1",1,0),
                    WB_2_indic = ifelse(crop_variety=="WB2",1,0),
                    ST_clay_indic = ifelse(soil_type=="clay",1,0),
                    ST_sandy_indic = ifelse(soil_type=="sandy",1,0)
)

predz<-predict(svm_all$svm_tuned,test_sample,probability=TRUE)
head(attr(predz,"probabilities"))