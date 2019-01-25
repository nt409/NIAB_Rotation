source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Functions.R')# contains functions 'folder_names', 'labeller', 'Data_in_final_form'
source('~/GitHub/NIAB_Rotation/Fruit_model/Run/Parameters.R')     # needs Data_Functions
# source('~/GitHub/NIAB_Rotation/Fruit_model/Analysis/Functions.R') # contains functions 'image_tester', 'preds', 'multipreds', 'image_predictor'
# source('~/GitHub/NIAB_Rotation/Fruit_model/Model/Fruit_model.R')  # needs parameters   # contains function 'create_fruit_model', 'image_predictor'

library(reticulate)

Train_data_and_labels <- Data_in_final_form(params$pathname,params$training_folder,params$class_names,params$labels_to_be_tested,params$filetype)

Train_data <-Train_data_and_labels$Image_array
Train_data <- aperm(Train_data,c(3,1,2)) # reorders elements
Train_labels <- Train_data_and_labels$label_vector[,1]
Train_total  <- Train_data_and_labels$total_no
####

Test_data_and_labels <- Data_in_final_form(params$pathname,params$test_folder,params$class_names,params$labels_to_be_tested,params$filetype)

Test_data <-Test_data_and_labels$Image_array
Test_data <- aperm(Test_data,c(3,1,2)) # reorders elements
Test_labels <- Test_data_and_labels$label_vector[,1]
Test_total  <- Test_data_and_labels$total_no

############################################

Train_data_reshaped <- array_reshape(Train_data,c(Train_total,params$xshape,params$yshape,3),order=c("F"))
Test_data_reshaped <- array_reshape(Test_data,c(Test_total,params$xshape,params$yshape,3),order=c("F"))
