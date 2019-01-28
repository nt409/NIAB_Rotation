##################################################
folder_names <- function(path_name_of_images,folder_name){
  path_name_with_folder <- paste(path_name_of_images,folder_name,sep = "/")
  folders <- list.dirs(path = path_name_with_folder, full.names = TRUE, recursive = TRUE)
  class_names_from_folder <- gsub(path_name_with_folder,"",folders)
  class_names_from_folder <- class_names_from_folder[2:(length(class_names_from_folder))] # ignores containing folder
  class_names_from_folder <- gsub("/","",class_names_from_folder)
  return(class_names_from_folder)
}
##################################################
pathname <- "C:/Users/Administrator/Documents/Rotation/fruits-360"
training_folder <- "Training"
class_names <- folder_names(pathname,training_folder)
##################################################
# get label numbers after specify name of fruit
fruit_list <- c("Apple_Braeburn","Banana","Orange","Grape_White","Kiwi")
class_names_frame <- cbind(class_names,seq(length(class_names)))
class_names_frame <- as.data.frame(class_names_frame,stringsAsFactors=F)
colnames(class_names_frame) <- c('Fruit','label')
##################################################
params <- list('pathname' = pathname,
  'training_folder'= training_folder,
  'test_folder' = "Test",
  'filetype' = ".jpg", # working with ".jpg" or ".png"?
  'internet_path'= "C:/Users/Administrator/Documents/Rotation/Photos_to_test_model",
  'internet_file_type' = ".jpg",
  'class_names'= class_names,
  'labels_to_be_tested' = as.numeric(filter(class_names_frame,Fruit %in% fruit_list)[,2]), # or e.g. 20:21, or 1:(length(class_names))
  'channels'= 1,           # how many channels? - 1, 2 or 3
  'channel_no' = 3,        # which of these channels? 1,2,3 or: e.g. c(1,2) or 1:3 depending on how many channels
  'number_probs' = 4,      # this many probabilities quoted by multipred
  'My_filter_number' = 32, # model parameters from here downwards
  'My_kernel_size'  = c(3,3),
  'My_batch_size'= 32,
  'My_epoch_number' = 5,
  'output_n'= 1 + length(class_names), # number of outputs
  'xshape'= 40,                        # image size
  'yshape' = 40,                       # image size
  'resize_method' = 'bilinear'         # or 'nearest' 
)