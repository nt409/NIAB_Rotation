library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

create_fruit_model <- function(train_data,train_labels,test_data,test_labels) {

model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')
  
model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

history <- model %>% fit(
  train_data,
  train_labels, 
  epochs = 5
)
  
results <- model %>% evaluate(test_data, test_labels)
results

plot(history)

return(model)
}
