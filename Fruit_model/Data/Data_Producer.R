source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Functions.R')# contains functions 'folder_names', 'labeller', 'Data_in_final_form'
source('~/GitHub/NIAB_Rotation/Fruit_model/Run/params.R')

library(reticulate)

# Train_data_and_labels <- Cluster_data_in_final_form(params$pathname,params$training_folder,params$labels_to_be_tested,params$filetype)

Train_data_and_labels <- Data_in_final_form(params$pathname,params$training_folder,params$labels_to_be_tested,params$filetype)

Train_data <-Train_data_and_labels$Image_array
Train_data <- aperm(Train_data,c(3,1,2)) # reorders elements
Train_labels <- Train_data_and_labels$label_vector[,1]
Train_total  <- length(Train_labels)
####

Test_data_and_labels <- Data_in_final_form(params$pathname,params$test_folder,params$labels_to_be_tested,params$filetype)

Test_data <-Test_data_and_labels$Image_array
Test_data <- aperm(Test_data,c(3,1,2)) # reorders elements
Test_labels <- Test_data_and_labels$label_vector[,1]
Test_total  <- length(Test_labels)

############################################

Train_data_reshaped <- array_reshape(Train_data,c(Train_total,params$xshape,params$yshape,3),order=c("F"))
Test_data_reshaped  <- array_reshape(Test_data,c(Test_total,params$xshape,params$yshape,3),order=c("F"))

saveRDS(Train_data_reshaped,file=params$train_data_name)
saveRDS(Train_labels,file=params$train_label_name)
saveRDS(Test_data_reshaped,file=params$test_data_name)
saveRDS(Test_labels,file=params$test_label_name)
