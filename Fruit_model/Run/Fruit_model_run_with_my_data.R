source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Labeller.R') #contains functions 'folder_names', 'labeller', 'Data_in_final_form' # slow to run
source('~/GitHub/NIAB_Rotation/Fruit_model/Model/Fruit_model.R')   # contains function 'create_fruit_model', 'image_predictor'
source('~/GitHub/NIAB_Rotation/Fruit_model/Run/Parameters.R')

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
return(prediction_and_prob)
}
preds(results)

############################################
res <- model_my_own %>% predict(Test_data_reshaped[1,,,channel_no,drop = F])
preds(res)

############################################
# plot_fruit(500) # from plot_model_graphs

