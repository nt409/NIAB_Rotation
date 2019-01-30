source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Functions.R')# contains functions 'folder_names', 'labeller', 'Data_in_final_form'
source('~/GitHub/NIAB_Rotation/Fruit_model/Run/params.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Cluster_data_functions.R')

library(reticulate)

Train_data_and_labels_parallel <- Cluster_data_in_final_form(params$pathname,params$training_folder,params$labels_to_be_tested,params$filetype)
Train_data_parallel <- Train_data_and_labels_parallel$Data
Train_data_parallel <- aperm(Train_data_parallel,c(3,1,2)) # reorders elements
Train_labels_parallel <- Train_data_and_labels_parallel$Labels[,1]
Train_total_parallel  <- length(Train_labels_parallel)

Test_data_and_labels_parallel <- Cluster_data_in_final_form(params$pathname,params$test_folder,params$labels_to_be_tested,params$filetype)
Test_data_parallel <- Test_data_and_labels_parallel$Data
Test_data_parallel <- aperm(Test_data_parallel,c(3,1,2)) # reorders elements
Test_labels_parallel <- Test_data_and_labels_parallel$Labels[,1]
Test_total_parallel  <- length(Test_labels_parallel)


# Train_data_and_labels <- Data_in_final_form(params$pathname,params$training_folder,params$labels_to_be_tested,params$filetype)
# 
# Train_data <-Train_data_and_labels$Image_array
# Train_data <- aperm(Train_data,c(3,1,2)) # reorders elements
# Train_labels <- Train_data_and_labels$label_vector[,1]
# Train_total  <- length(Train_labels)
# ####

# Test_data_and_labels <- Data_in_final_form(params$pathname,params$test_folder,params$labels_to_be_tested,params$filetype)
# 
# Test_data <-Test_data_and_labels$Image_array
# Test_data <- aperm(Test_data,c(3,1,2)) # reorders elements
# Test_labels <- Test_data_and_labels$label_vector[,1]
# Test_total  <- length(Test_labels)

############################################

# Train_data_reshaped <- array_reshape(Train_data,c(Train_total,params$xshape,params$yshape,3),order=c("F"))
# Test_data_reshaped  <- array_reshape(Test_data,c(Test_total,params$xshape,params$yshape,3),order=c("F"))

Train_data_reshaped_parallel <- array_reshape(Train_data_parallel,c(Train_total_parallel,params$xshape,params$yshape,3),order=c("F"))
Test_data_reshaped_parallel  <- array_reshape(Test_data_parallel,c(Test_total_parallel,params$xshape,params$yshape,3),order=c("F"))

saveRDS(Train_data_reshaped_parallel,file=params$train_data_name)
saveRDS(Train_labels_parallel,file=params$train_label_name)
saveRDS(Test_data_reshaped_parallel,file=params$test_data_name)
saveRDS(Test_labels_parallel,file=params$test_label_name)
