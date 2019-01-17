library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

create_movie_model <- function(x_val,partial_x_train,y_val,partial_y_train,test_data,test_labels) {

# input shape is the vocabulary count used for the movie reviews (10,000 words)
vocab_size <- 10000

model <- keras_model_sequential()
model %>% 
  layer_embedding(input_dim = vocab_size, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% summary()

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 40,
  batch_size = 512,
  validation_data = list(x_val, y_val),
  verbose=1
)
  
results <- model %>% evaluate(test_data, test_labels)
results

plot(history)

return(model)
}
