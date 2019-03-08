class_list <- c("names_of_diseases_will_go_in_here") # gets extracted from JSON in CNN_data_generator_and_model_functions.R
# Need to check the 2 pathnames on the xmltojson.py file are correct for the device running the model and for the data we want to input.

# default parameters
params <- list('img_dir'    = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/all2",
               'annot_file' = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/jsonfold/online.json",
               'folder_containing_scripts' = "C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Pipeline",
               'folder_to_save_model_in'   = "C:/Users/Administrator/Documents/GitHub",
               'folder_to_save_images_in'  = "C:/Users/Administrator/Documents/GitHub/Pipeline_resulting_images",
               'weight_file_path' = "C:/Users/Administrator/Documents/GitHub/Weights",
               'target_height' = 224,
               'target_width'  = 224,
               'class_background' = length(class_list), # gets updated when we run CNN_data_generator_and_model_functions.R
               'cl_output' = length(class_list),        # gets updated when we run CNN_data_generator_and_model_functions.R
               'batch_size' = 5,                        #10 #1 # low is faster but less accurate?   ### 4
               'proportion_of_samples' = 0.5,           # 0.2, but was classifying everything the same. ###0.3
               'seed' = 4,
               'threshold' = 0.4,
               'epochs' = 20,                           # 20
               'label_names' = class_list,
               'layer_units' = 256,                     #256, # 30
               'patience' = 8,                          # was 8, but that's quite slow
               'save' = 1,                              #save model?
               'load' = 1,                              #load a saved model?
               'model_name' = "disease_image_classifier.h5" # 'model_name' = "disease_image_classifier_prop_is_half.h5" did ok, 78% on test set but never predicted YR
)