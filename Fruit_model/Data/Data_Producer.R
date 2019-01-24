Train_data_and_labels <- Data_in_final_form(pathname,training_folder,class_names,labels_to_be_tested,filetype)

Train_data <-Train_data_and_labels$Image_array
Train_data <- aperm(Train_data,c(3,1,2)) # reorders elements
Train_labels <- Train_data_and_labels$label_vector[,1]
Train_total  <- Train_data_and_labels$total_no
####

Test_data_and_labels <- Data_in_final_form(pathname,test_folder,class_names,labels_to_be_tested,filetype)

Test_data <-Test_data_and_labels$Image_array
Test_data <- aperm(Test_data,c(3,1,2)) # reorders elements
Test_labels <- Test_data_and_labels$label_vector[,1]
Test_total  <- Test_data_and_labels$total_no

############################################

Train_data_reshaped <- array_reshape(Train_data,c(Train_total,xshape,yshape,3),order=c("F"))
Test_data_reshaped <- array_reshape(Test_data,c(Test_total,xshape,yshape,3),order=c("F"))




