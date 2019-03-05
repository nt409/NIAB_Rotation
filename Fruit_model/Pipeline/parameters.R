class_list <- c("YR","MSD","BS") # json

params <- list('img_dir' = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/all2",
               'annot_file' = "C:/Users/Administrator/Documents/GitHub/test_images_to_use/jsonfold/online.json",
               'target_height' = 224,
               'target_width' = 224,
               'batch_size' = 3, #10 #1 # low is faster but less accurate?
               'proportion_of_samples' = 0.3, # 0.2, but was classifying everything the same
               'threshold' = 0.4,
               'class_background' = length(class_list), # should it be length(class_list), or length(class_list) + 1?
               'cl_output' = length(class_list), # 20
               'epochs' = 20,
               'weight_file_path' = "C:/Users/Administrator/Documents/GitHub/Weights",
               'label_names' = class_list,
               'layer_units' = 256, # 30
               'patience' = 6, # was 8, but that's quite slow
               'save' = 0, #save model?
               'model_name' = "disease_image_classifier.h5"
)
