class_list <- c("names_of_diseases_will_go_in_here") # gets extracted from JSON in CNN_data_generator_and_model_functions.R

# Also need to check the 2 pathnames on the xmltojson.py file are correct for the device running the model and for the data we want to input.

# for semi-automated JSON
# fold_cont_annot <- "C:/Users/Administrator/Documents/GitHub/test_images_to_use/jsonfold"
# then inside params had these
# 'folder_containing_annotations' = fold_cont_annot,
# 'input_labels' = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/all_label/YR_MSD_BS",
# 'annot_file' = paste(fold_cont_annot,"online.json",sep="/"),


# default parameters
params <- list('img_dir'                   = "C:/Users/Administrator/Documents/GitHub/Images_and_labels/Image_directory",         # file path unique to each device
               'annot_file'                = "C:/Users/Administrator/Documents/GitHub/Images_and_labels/json_folder/online.json", # file path unique to each device
               'folder_containing_scripts' = "C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Disease_pipeline",            # file path unique to each device
               'folder_to_save_data_in'    = "C:/Users/Administrator/Documents/GitHub/Saved/Grid_output",                         # file path unique to each device
               'folder_to_save_model_in'   = "C:/Users/Administrator/Documents/GitHub/Saved/Models",                              # file path unique to each device
               'folder_to_save_images_in'  = "C:/Users/Administrator/Documents/GitHub/Saved/Pipeline_resulting_images",           # file path unique to each device
               'folder_to_save_weights_in' = "C:/Users/Administrator/Documents/GitHub/Saved/Weights",                             # file path unique to each device
               'target_height'         = 224,
               'target_width'          = 224,
               'class_background'      = length(class_list), # gets updated when we run CNN_data_generator_and_model_functions.R
               'cl_output'             = length(class_list), # gets updated when we run CNN_data_generator_and_model_functions.R
               'batch_size'            = 5,                  # updated in Run.R # 10 #1 # low is faster but less accurate?   ### 4
               'batch_size_val'        = 1,
               'proportion_of_samples' = 0.6,                # updated in Run.R # 0.2, but was classifying everything the same. ###0.3
               'seed'                  = 6,
               'TF_seed'               = 1,
               'threshold'             = 0.4,
               'epochs'                = 20,                 # updated in Run.R
               'label_names'           = class_list,
               'layer_units'           = 256,                # updated in Run.R
               'patience'              = 8,                  # was 8, but that's quite slow
               'save'                  = 1,                  # save model?  updated in Run.R
               'load'                  = 0,                  # load a saved model?  updated in Run.R
               'model_name_to_load' = "disease_image_classifier.h5", # updated in Run.R
               'model_name_to_save' = "disease_image_classifier.h5"  # updated in Run.R
)