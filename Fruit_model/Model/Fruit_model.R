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
  layer_conv_2d(filter=32, kernel_size= c(3,3),padding="same",input_shape = c(xshape, yshape, channels)) %>%
  layer_activation("relu") %>%
  layer_conv_2d(filter=32, kernel_size= c(3,3),padding="same") %>%
  layer_activation_leaky_relu(0.5) %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  
  # Flatten max filtered output into feature vector 
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  
  
  # Outputs from dense layer are projected onto output layer
  layer_dense(output_n) %>% 
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
  batch_size = 32,
  epochs = 5
)



results <- model %>% evaluate(test_data, test_labels)
print(results)

plot(history)

return(model)
}

image_predictor <- function(n,results_of_model){
  pred <- which.max(results_of_model[n, ])-1
  prob <- results_of_model[n,pred+1]
  return(list('Label' = pred,'Probability' = prob))
}
