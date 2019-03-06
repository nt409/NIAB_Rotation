class_list <- c("YR","MSD","BS") # from json

# also need to change the 2 pathnames on the xmltojson.py file.

params <- list('img_dir'    = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/all2",
               'annot_file' = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/jsonfold/online.json",
               'folder_containing_scripts' = "C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Pipeline",
               'folder_to_save_model_in'   = "C:/Users/Administrator/Documents/GitHub",
               'folder_to_save_images_in'  = "C:/Users/Administrator/Documents/GitHub/Pipeline_resulting_images",
               'target_height' = 224,
               'target_width'  = 224,
               'batch_size' = 1, #10 #1 # low is faster but less accurate?   ### 4
               'proportion_of_samples' = 0.1, # 0.2, but was classifying everything the same. ###0.3
               'threshold' = 0.4,
               'class_background' = length(class_list), # should it be length(class_list), or length(class_list) + 1?
               'cl_output' = length(class_list), # 20
               'epochs' = 3, # 20
               'weight_file_path' = "C:/Users/Administrator/Documents/GitHub/Weights",
               'label_names' = class_list,
               'layer_units' = 16, #256, # 30
               'patience' = 2, # was 8, but that's quite slow
               'save' = 1, #save model?
               'load' = 1, #load a saved model?
               'model_name' = "disease_image_classifier.h5"
)