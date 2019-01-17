source('~/GitHub/NIAB_Rotation/Fruit_model/Fruit_model_graphs.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Fruit_model_data.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Fruit_model_predictor.R')

rgb_plots('C:/Users/Administrator/Documents/Rotation/10_100.jpg')
fruit_data_1 <- fruit_image_data('C:/Users/Administrator/Documents/Rotation/10_100.jpg')
features(fruit_data_1)



#model <- create_model(train_data,train_labels,test_data,test_labels)
#model %>% summary()
#model %>% save_model_hdf5("model.h5")
#new_model <- load_model_hdf5("model.h5")
#new_model %>% summary()

#####
#results <- new_model %>% evaluate(test_data, test_labels)
#results
####
#N_test<-100
#evaluate <- new_model %>% evaluate(test_data[1:N_test,],test_labels[1:N_test])
#evaluate$acc # percentage got right
#predictions <- new_model %>% predict(test_data)
#predictions[1:10]
#test_labels[1:10]