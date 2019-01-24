source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Functions.R')# contains functions 'folder_names', 'labeller', 'Data_in_final_form'
source('~/GitHub/NIAB_Rotation/Fruit_model/Run/Parameters.R')     # needs Data_Functions
source('~/GitHub/NIAB_Rotation/Fruit_model/Analysis/Functions.R') # contains functions 'image_tester', 'preds', 'multipreds', 'image_predictor'
source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Producer.R') # slow to run
source('~/GitHub/NIAB_Rotation/Fruit_model/Model/Fruit_model.R')  # needs parameters   # contains function 'create_fruit_model', 'image_predictor'

library(keras)
library(dplyr)
library(ggplot2)
library(tidyr)

# data comes in from Data_Labeller

############################################
# fit model
model_name <- "fruit_model_new.h5" # "fruit_model.h5"
model_my_own <-create_fruit_model(Train_data_reshaped[,,,channel_no,drop = F],Train_labels,Test_data_reshaped[,,,channel_no,drop = F],Test_labels,channels) # drop = F stops R collapsing array to 3 dimensions not 4
model_my_own %>% summary()
model_my_own %>% save_model_hdf5(model_name)


# below model has all fruits
#new_model <- load_model_hdf5("fruit_model.h5")
#new_model %>% summary()

results <- model_my_own %>% evaluate(Test_data_reshaped[165:3622,,,channel_no,drop=F], Test_labels[165:3622])
results

results <- model_my_own %>% predict_classes(Test_data_reshaped[1:164,,,channel_no,drop = F]) # drop = F stops R collapsing array to 3 dimensions not 4
############################################
data <- image_tester(internet_path,"Walnut")
res2 <- model_my_own %>% predict(data)
preds(res2)
multipreds(res2,number_probs)


# manually test accuracy
right <- 0
wrong <- 0
for(k in 20:40){
data <- image_tester(pathname,paste("Test",class_names[k],sep="/"))
res2 <- model_my_own %>% predict(data)
print(k)
print(preds(res2)[,3])
vec<-preds(res2)[,3]-rep(k,length(preds(res2)[,3]))
right<-right+sum(vec==0)
wrong<-wrong+(length(preds(res2)[,3]) - sum(vec==0))
print(paste("Number right =",right,", Number wrong =",wrong,", Accuracy =",right/(wrong+right),sep=''))
}