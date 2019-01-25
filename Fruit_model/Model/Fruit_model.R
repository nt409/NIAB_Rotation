# source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Functions.R')# contains functions 'folder_names', 'labeller', 'Data_in_final_form'
# source('~/GitHub/NIAB_Rotation/Fruit_model/Run/Parameters.R')     # needs Data_Functions
# source('~/GitHub/NIAB_Rotation/Fruit_model/Analysis/Functions.R') # contains functions 'image_tester', 'preds', 'multipreds', 'image_predictor'
# source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Producer.R') # slow to run

library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

# contains function 'create_fruit_model', 'image_predictor'

create_fruit_model <- function(train_data,train_labels,test_data,test_labels,channels) {

model <- keras_model_sequential()
# output_n <- 1 + length(unique(train_labels)) # number of classes, plus one because we don't cast to 0?
# needs to include label values? eg 14, 81 -> is max
model %>%
  layer_conv_2d(filter=params$My_filter_number, kernel_size= params$My_kernel_size,padding="same",input_shape = c(params$xshape, params$yshape, params$channels)) %>%
  layer_activation("relu") %>%
  layer_conv_2d(filter=params$My_filter_number, kernel_size= params$My_kernel_size,padding="same") %>%
  layer_activation_leaky_relu(0.5) %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>% # dropout- regularises
  
  # Flatten max filtered output into feature vector 
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  
  
  # Outputs from dense layer are projected onto output layer
  layer_dense(params$output_n) %>% 
  layer_activation("softmax")

  #layer_flatten(input_shape = c(100, 300)) %>% # was 28,28
  #layer_dense(units = 1024, activation = 'relu') %>%
  #layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  optimizer = 'adam', # lr =? eps =?
  loss = 'sparse_categorical_crossentropy',
  metrics = 'accuracy'
)


history <- model %>% fit(
  train_data, #batch_size = 128? #validation_split = 0.1,
  train_labels,
  batch_size = params$My_batch_size,
  epochs = params$My_epoch_number
)



results <- model %>% evaluate(test_data, test_labels)
print(results)

plot(history)

return(model)
}