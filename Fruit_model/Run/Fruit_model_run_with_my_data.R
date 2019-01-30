source('~/GitHub/NIAB_Rotation/Fruit_model/Run/params.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Functions.R')# contains functions, 'labeller', 'Data_in_final_form'
source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Cluster_data_functions.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Analysis/Functions.R') # contains functions 'image_tester', 'preds', 'multipreds', 'image_predictor'
source('~/GitHub/NIAB_Rotation/Fruit_model/Model/Fruit_model.R')  # needs params   # contains function 'create_fruit_model' # needs to be before data_producer, so library(keras) is before library(reticulate)?
source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Producer.R') # slow to run, saves train & test data & labels

library(keras)
library(dplyr)
library(ggplot2)
library(tidyr)

# data comes in from Data_Producer

Train_data_saved   <- readRDS(params$train_data_name)
Train_labels_saved <- readRDS(params$train_label_name)
Test_data_saved    <- readRDS(params$test_data_name)
Test_labels_saved  <- readRDS(params$test_label_name)


############################################
# fit model
model_name <- "fruit_model_new.h5" # "fruit_model.h5"
model_my_own <-create_fruit_model(Train_data_saved[,,,params$channel_no,drop = F],Train_labels_saved,Test_data_saved[,,,params$channel_no,drop = F],Test_labels_saved) # drop = F stops R collapsing array to 3 dimensions not 4
model_my_own %>% summary()
model_my_own %>% save_model_hdf5(model_name)



# below model has all fruits
#new_model <- load_model_hdf5("fruit_model.h5")
#new_model %>% summary()

results <- model_my_own %>% evaluate(Test_data_saved[,,,params$channel_no,drop=F], Test_labels_saved)
results

results <- model_my_own %>% predict_classes(Test_data_saved[,,,params$channel_no,drop = F]) # drop = F stops R collapsing array to 3 dimensions not 4
############################################
data <- image_tester(params$internet_path,"Apricot")
res2 <- model_my_own %>% predict(data)
preds(res2)
multipreds(res2,params$number_probs)
