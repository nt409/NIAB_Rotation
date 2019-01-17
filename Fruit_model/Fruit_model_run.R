source('~/GitHub/NIAB_Rotation/Fruit_model/Fruit_model_graphs.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Fruit_model_data.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Fruit_model_predictor.R')

Run_fruit_model <- function(image_path_name,x,y){
  
rgb_plots(image_path_name,x,y)
fruit_data_1 <- fruit_image_data(image_path_name)
predict <- features(fruit_data_1)

return(predict)
}

# e.g Run_fruit_model('C:/Users/Administrator/Documents/Rotation/10_100.jpg')