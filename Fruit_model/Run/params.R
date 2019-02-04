##################################################
# function to extract folder (ie category) names
folder_names <- function(path_name_of_images,folder_name){
  path_name_with_folder <- paste(path_name_of_images,folder_name,sep = "/")
  folders <- list.dirs(path = path_name_with_folder, full.names = TRUE, recursive = TRUE)
  class_names_from_folder <- gsub(path_name_with_folder,"",folders)
  class_names_from_folder <- class_names_from_folder[2:(length(class_names_from_folder))] # ignores containing folder
  class_names_from_folder <- gsub("/","",class_names_from_folder)
  return(class_names_from_folder)
}

library(dplyr)

##################################################
# file/path parameters
pathname <- "C:/Users/Administrator/Documents/Rotation/fruits-360" ## In params
training_folder <- "Training" ## In params
test_folder <- "Test"         ## In params
filetype <- ".jpg"            ## In params  # working with ".jpg" or ".png"?
internet_path <- "C:/Users/Administrator/Documents/Rotation/Photos_to_test_model" ## In params
internet_file_type <- ".jpg"  ## In params
class_names <- folder_names(pathname,training_folder) ## In params

##################################################
#######
# get label numbers of fruits to be tested - can uncomment lines below to specify fruits
#######
# fruit_list <- c("Apple_Braeburn","Banana") #,"Orange","Grape_White","Kiwi")
# class_names_frame <- cbind(class_names,seq(length(class_names)))
# class_names_frame <- as.data.frame(class_names_frame,stringsAsFactors=F)
# colnames(class_names_frame) <- c('Fruit','label')
# labels_to_be_tested <- as.numeric(filter(class_names_frame,Fruit %in% fruit_list)[,2]) ## In params
#######
### or we can just specify fruit numbers e.g. c(13,48), or 20:30, or 1:(length(class_names))
#######
labels_to_be_tested <- 29:30 # comment line if using the name method above to denote which fruits to test
##################################################
# model parameters
channel_no <- 1:3        ## In params # which channels? 1,2,3 or: e.g. c(1,2) or 1:3 depending on how many channels
My_filter_number <- 32   ## In params
My_kernel_size <- c(3,3) ## In params
My_batch_size  <- 32     ## In params
My_epoch_number <- 5 # 5 ## In params
output_n <- 1 + length(class_names)   ## In params # number of outputs, # number of classes plus one since don't cast to 0?
# needs to include label values? eg 14, 81 -> is max
xshape   <- 40                        ## In params # image size
yshape   <- 40                        ## In params # image size
resize_method <- 'bilinear'           ## In params # or 'nearest'

number_probs <- 4      ## In params # this many probabilities quoted by multipred

###

train_data_name<- "fruit_train_data.Rda"  ## In params
train_label_name<-"fruit_train_labels.Rda"## In params 
test_data_name<-  "fruit_test_data.Rda"   ## In params
test_label_name<- "fruit_test_labels.Rda" ## In params
##################################################
# all of these should be specified above-only modify above here
params <- list('pathname' = pathname,
  'training_folder' = training_folder,
  'test_folder' = test_folder,
  'filetype' = filetype,
  'internet_path' = internet_path,
  'internet_file_type' = internet_file_type,
  'class_names'= class_names,
  'labels_to_be_tested' = labels_to_be_tested,
  'channel_no' = channel_no,
  'My_filter_number' = My_filter_number,
  'My_kernel_size' = My_kernel_size,
  'My_batch_size'  = My_batch_size,
  'My_epoch_number' = My_epoch_number,
  'output_n'= output_n,
  'xshape' = xshape,
  'yshape' = yshape,
  'resize_method' = resize_method,
  'number_probs' = number_probs,
  'train_data_name' = train_data_name,
  'train_label_name' = train_label_name,
  'test_data_name' = test_data_name,
  'test_label_name' = test_label_name
)