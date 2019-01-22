library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

create_fruit_model_conv <- function(train_data,train_labels,test_data,test_labels) {


output_n <- 2 #length fruit list

# initialise model
model <- keras_model_sequential()

# add layers
model %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(100,100,3)) %>%
  layer_activation("relu") %>%
  
  # Second hidden layer
  layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  
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

# compile
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
  metrics = "accuracy"
)

history <- model %>% fit(
  train_data, #batch_size = 128? #validation_split = 0.1,
  train_labels,
  batch_size = 32,
  epochs = 10
)



# batch_size<-32
# # fit
# history <- model %>% fit_generator(
#   # training data
#   train_image_array_gen,
# 
#   # epochs
#   steps_per_epoch = as.integer(length(train_data[,1,1]) / batch_size),
#   epochs = epochs,
# 
#   # validation data
#   validation_data =  test_data,
#   validation_steps = as.integer(length(test_data[,1,1]) / batch_size),
# 
#   # print progress
#   verbose = 2,
#   callbacks = list(
#     # save best model after every epoch
#     callback_model_checkpoint("/Users/shiringlander/Documents/Github/DL_AI/Tutti_Frutti/fruits-360/keras/fruits_checkpoints.h5", save_best_only = TRUE),
#     # only needed for visualising with TensorBoard
#     callback_tensorboard(log_dir = "/Users/shiringlander/Documents/Github/DL_AI/Tutti_Frutti/fruits-360/keras/logs")
#   )
# )

results <- model %>% evaluate(test_data, test_labels)
print(results)

plot(history)

return(model)
}
