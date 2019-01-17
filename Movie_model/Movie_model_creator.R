# try and get it to work with the data as a saved model
Mov_data <- Movie_data()
x_val <- Mov_data$x_val
partial_x_train <- Mov_data$partial_x_train
y_val <- Mov_data$y_val
partial_y_train <- Mov_data$partial_y_train
test_data <- Mov_data$test_data
test_labels <- Mov_data$test_labels

model <- create_movie_model(x_val,partial_x_train,y_val,partial_y_train,test_data,test_labels)
model %>% summary()
model %>% save_model_hdf5("movie_model.h5")
new_model <- load_model_hdf5("movie_model.h5")
new_model %>% summary()


#####
results <- new_model %>% evaluate(test_data, test_labels)
results
####
N_test<-100
evaluate <- new_model %>% evaluate(test_data[1:N_test,],test_labels[1:N_test])
evaluate$acc # percentage got right
predictions <- new_model %>% predict(test_data)
predictions[1:10]
test_labels[1:10]
decode_review(test_data[1,])