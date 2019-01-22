library(keras)

fashion_mnist <- dataset_fashion_mnist()
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

train_images <- train_images / 255
test_images <- test_images / 255

create_fruit_model(train_images,train_labels,test_images,test_labels)

#model_my_own <- create_fruit_model(train_images,train_labels,test_images,test_labels)
#model_my_own %>% summary()
#model_my_own %>% save_model_hdf5("fruit_model.h5")


