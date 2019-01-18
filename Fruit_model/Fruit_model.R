library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

create_fruit_model <- function(train_data,train_labels,test_data,test_labels) {

model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(100, 300)) %>% # was 28,28
  layer_dense(units = 1024, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  optimizer = 'adam',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
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
