#source('C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Pipeline/parameters.R') # don't need?
setwd(params$folder_containing_scripts)
# source('CNN_data_generator_and_model_functions.R')
# source('Image_classifier_functions.R')

# how many of these necessary?
library(keras)
library(rjson)
library(magick)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(XML)
library(xml2)
library(jsonlite)
library(tensorflow)

model %>% fit_generator(
  train_gen,
  epochs = params$epochs,
  steps_per_epoch = nrow(train_data) / params$batch_size,
  validation_data = valid_gen,
  validation_steps = nrow(validation_data) / params$batch_size,
  callbacks = list(
    callback_model_checkpoint(
      file.path(params$weight_file_path, "weights.{epoch:02d}-{val_loss:.2f}.hdf5")
    ),
    callback_early_stopping(patience = params$patience)
  )
)

##########
# save?
if(params$save == 1){
setwd(params$folder_to_save_images_in)
model %>% save_model_hdf5(params$model_name)
setwd(params$folder_containing_scripts)
}