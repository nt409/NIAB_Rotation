source('~/GitHub/NIAB_Rotation/Fruit_model/Data_Labeller.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Fruit_model.R')

library(keras)
library(dplyr)

max_train<-10000 # goes up to label 21
max_test<-3400 # goes up to label 21

Fruit_data_training <- Data_labelled('Training')
Train_data_and_labels <- Data_in_final_form(Fruit_data_training,'Training',max_train)

Train_data <-Train_data_and_labels$Image_array
Train_data <- aperm(Train_data,c(3,1,2)) # reorders elements
Train_labels <- Train_data_and_labels$label_vector[,1]

# im_number <- 3
# Train_data[,,im_number]
# Train_labels[im_number]

Fruit_data_test <- Data_labelled('Test')
Test_data_and_labels <- Data_in_final_form(Fruit_data_test,'Test',max_test)

Test_data <-Test_data_and_labels$Image_array
Test_data <- aperm(Test_data,c(3,1,2)) # reorders elements
Test_labels <- Test_data_and_labels$label_vector[,1]

model_my_own <-create_fruit_model(Train_data,Train_labels,Test_data,Test_labels)
model_my_own %>% summary()
model_my_own %>% save_model_hdf5("fruit_model.h5")