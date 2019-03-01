library(e1071)
# attach(iris)

source('~/GitHub/NIAB_Rotation/Fruit_model/Bigger_model/Disease_fake_data.R')

# do we need prevalence data??

data <- dis_data2
category_id<-dis_data2$disease
names(data)[names(data)=="disease"] <- "category_id"
#########################################################
raw_data <- subset(data, select = c(category_id,location,rainfall,mean_temp,crop_variety,soil_type,d1_score,d2_score,d3_score))
predictor_data <- subset(data, select = c(-location,-crop_variety,-soil_type,-category_id))
data_use<- subset(data, select = c(-location,-crop_variety,-soil_type))
class_labels <- category_id

head(data_use)
###

svm_model <- svm(category_id~., data=data_use)
summary(svm_model)

pred <- predict(svm_model,predictor_data)
table(pred,class_labels)

svm_tune <- tune(svm, train.x=predictor_data, train.y=class_labels,
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

##
resulting_cost<-as.numeric(svm_tune$best.parameters[1]) # get from svm_tune
resulting_gamma<-as.numeric(svm_tune$best.parameters[2]) # get from svm_tune
##

svm_model_after_tune <- svm(category_id ~ ., data=data_use, kernel="radial", cost=resulting_cost, gamma=resulting_gamma)
summary(svm_model_after_tune)

pred_tuned <- predict(svm_model_after_tune,predictor_data)
table(pred_tuned,class_labels)

################################################
# without disease images

data_use_without_images<- subset(data, select = c(-location,-crop_variety,-soil_type,-d1_score,-d2_score,-d3_score))
predictor_data_without_images <- subset(data, select = c(-location,-crop_variety,-soil_type,-category_id,-d1_score,-d2_score,-d3_score))

svm_model_without_images <- svm(category_id~., data=data_use_without_images)
summary(svm_model_without_images)

pred_without_images <- predict(svm_model_without_images,predictor_data_without_images)
table(pred_without_images,class_labels)

svm_tune_without_images <- tune(svm, train.x=predictor_data_without_images, train.y=class_labels,
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune_without_images)

##
resulting_cost_without_images<-as.numeric(svm_tune_without_images$best.parameters[1]) # get from svm_tune
resulting_gamma_without_images<-as.numeric(svm_tune_without_images$best.parameters[2]) # get from svm_tune
##

svm_model_after_tune_without_images <- svm(category_id ~ ., data=data_use_without_images, kernel="radial", cost=resulting_cost_without_images, gamma=resulting_gamma_without_images)
summary(svm_model_after_tune_without_images)

pred_without_images_tuned <- predict(svm_model_after_tune_without_images,predictor_data_without_images)
table(pred_without_images_tuned,class_labels)

################################################
# only with images

data_use_images_only<- subset(data, select = c(category_id,d1_score,d2_score,d3_score))
predictor_data_images_only <- subset(data, select = c(d1_score,d2_score,d3_score))


#function(data_to_use,)
svm_model_images_only <- svm(category_id~., data=data_use_images_only)
summary(svm_model_images_only)

pred_images_only <- predict(svm_model_images_only,predictor_data_images_only)
table(pred_images_only,class_labels)

svm_tune_images_only <- tune(svm, train.x=predictor_data_images_only, train.y=class_labels,
                                kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune_images_only)

##
resulting_cost_images_only<-as.numeric(svm_tune_images_only$best.parameters[1]) # get from svm_tune
resulting_gamma_images_only<-as.numeric(svm_tune_images_only$best.parameters[2]) # get from svm_tune
##

svm_model_after_tune_images_only <- svm(category_id ~ ., data=data_use_images_only, kernel="radial", cost=resulting_cost_images_only, gamma=resulting_gamma_images_only)
summary(svm_model_after_tune_images_only)

pred_images_only_tuned <- predict(svm_model_after_tune_images_only,predictor_data_images_only)
table(pred_images_only_tuned,class_labels)

# compare to 
table(pred_tuned,class_labels)