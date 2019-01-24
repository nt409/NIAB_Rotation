source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Labeller.R') #contains functions 'folder_names', 'labeller', 'Data_in_final_form', 'image_tester' # slow to run
source('~/GitHub/NIAB_Rotation/Fruit_model/Run/Parameters.R')     # needs Data_Labeller
source('~/GitHub/NIAB_Rotation/Fruit_model/Model/Fruit_model.R') # needs parameters   # contains function 'create_fruit_model', 'image_predictor'

library(keras)
library(dplyr)
library(ggplot2)
library(tidyr)

# data comes in from Data_Labeller

############################################
# fit model
model_name <- "fruit_model.h5"
model_my_own <-create_fruit_model(Train_data_reshaped[,,,channel_no,drop = F],Train_labels,Test_data_reshaped[,,,channel_no,drop = F],Test_labels,channels)
model_my_own %>% summary()
model_my_own %>% save_model_hdf5(model_name)

results <- model_my_own %>% predict(Test_data_reshaped[,,,channel_no,drop = F]) # drop = F stops R collapsing array to 3 dimensions not 4

############################################

data <- image_tester(internet_path,"Avocado")
res2 <- model_my_own %>% predict(data)
preds(res2)#,number_of_preds)

data <- image_tester(pathname,"Test/Mango")
res2 <- model_my_own %>% predict(data)
preds(res2)#,number_of_preds)
multipreds(res2,number_probs)

data <- image_tester(pathname,"Test/Avocado")
res2 <- model_my_own %>% predict(data)
preds(res2)


data <- image_tester(internet_path,"Mango")
res2 <- model_my_own %>% predict(data)
preds(res2)#,number_of_preds)

############################################
# multipreds returns number_of_probs many predictions for each image, ranked by most to least likely.
multipreds <- function(result,number_of_probs){
frame<-as.data.frame(result[1,])
colnames(frame) <- "Probability"
frame2<-mutate(frame,Label = 0:(length(frame[,1])-1))  # because runs from 1 to 82 not 0 to 81?
frame3<-mutate(frame2,Rank = rank(desc(Probability))) %>%
  arrange(Rank)
frame4<-filter(frame3,Rank <= number_of_probs)
frame4$Prediction <- class_names[frame4$Label]
frame4$Image_number <- paste('Image',1)
frame4 <- frame4[c("Image_number","Prediction","Probability","Label","Rank")]
final_frame<-frame4
if(nrow(result)>1){
for(k in 2:nrow(result)){
frame<-as.data.frame(result[k,]) # t(result)?
colnames(frame) <- "Probability"
frame2<-mutate(frame,Label = 0:(length(frame[,1])-1))  # because runs from 1 to 82 not 0 to 81?
frame3<-mutate(frame2,Rank = rank(desc(Probability))) %>%
  arrange(Rank)
frame4<-filter(frame3,Rank <= number_of_probs)
frame4$Prediction <- class_names[frame4$Label]
frame4$Image_number <- paste('Image',k)
frame4 <- frame4[c("Image_number","Prediction","Probability","Label","Rank")]
final_frame <- rbind(final_frame,frame4)
}
}
return(final_frame)
}


############################################
# returns the prediction and corresponding probability for each image.
preds <- function(result){#,number_of_preds
  prediction_and_prob <- image_predictor(1,result)#,number_of_preds)
  prediction_and_prob <- as.data.frame(prediction_and_prob)
  if(length(result[,1])>1){
    for(k in 2:length(result[,1])){
      new_prediction <- image_predictor(k,result)#,number_of_preds)
      new_prediction <- as.data.frame(new_prediction)
      prediction_and_prob<-rbind(prediction_and_prob,new_prediction)
    }
  }
  prediction_and_prob$Prediction <- class_names[prediction_and_prob$Label]
  prediction_and_prob <- prediction_and_prob[c("Prediction","Probability","Label")]
  return(prediction_and_prob)
}
############################################
# plot_fruit(500) # from plot_model_graphs

