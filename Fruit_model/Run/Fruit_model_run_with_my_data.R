source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Labeller.R') #contains functions 'folder_names', 'labeller', 'Data_in_final_form' # slow to run
source('~/GitHub/NIAB_Rotation/Fruit_model/Run/Parameters.R')     # needs Data_Labeller
source('~/GitHub/NIAB_Rotation/Fruit_model/Model/Fruit_model.R') # needs parameters   # contains function 'create_fruit_model', 'image_predictor'

library(keras)
library(dplyr)
library(ggplot2)


# data comes in from Data_Labeller

############################################
# fit model
model_name <- "fruit_model.h5"
model_my_own <-create_fruit_model(Train_data_reshaped[,,,channel_no,drop = F],Train_labels,Test_data_reshaped[,,,channel_no,drop = F],Test_labels,channels)
model_my_own %>% summary()
model_my_own %>% save_model_hdf5(model_name)

results <- model_my_own %>% predict(Test_data_reshaped[,,,channel_no,drop = F]) # drop = F stops R collapsing array to 3 dimensions not 4

############################################
# returns the prediction and corresponding probability for each image.
preds <- function(result){
prediction_and_prob <- image_predictor(1,result)
prediction_and_prob <- as.data.frame(prediction_and_prob)
if(length(result[,1])>1){
for(k in 2:length(result[,1])){
  new_prediction <- image_predictor(k,result)
  new_prediction <- as.data.frame(new_prediction)
  prediction_and_prob<-rbind(prediction_and_prob,new_prediction)
}
}
prediction_and_prob$Prediction <- class_names[prediction_and_prob$Label]
prediction_and_prob <- prediction_and_prob[c("Prediction","Probability","Label")]
return(prediction_and_prob)
}
preds(results)

############################################
im <- readImage("C:/Users/Administrator/Documents/Rotation/Photos_to_test_model/wal_test.jpg")
min_dimension <- which.min(c(dim(im)[1],dim(im)[2]))
im_size<-dim(im)[min_dimension] -1 # shouldn't need -1, but didn't like it otherwise
eq_sp <- cropImage(im, new_width = im_size, new_height = im_size, type = 'equal_spaced')
im <- resizeImage(eq_sp, width = xshape, height = yshape, method = 'bilinear')
new_data <- as.data.frame(im)
data <- abind(new_data,new_data,along=3)
data <-  aperm(data,c(3,1,2)) # reorders elements
data <- array_reshape(data,c(2,xshape,yshape,3),order=c("F"))
res2 <- model_my_own %>% predict(data[1,,,channel_no,drop=F])
preds(res2)




############################################
# plot_fruit(500) # from plot_model_graphs

