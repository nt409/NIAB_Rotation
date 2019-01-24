############################################
pathname <- "C:/Users/Administrator/Documents/Rotation/fruits-360"
training_folder <- "Training"
test_folder <- "Test"
#####
internet_path <- "C:/Users/Administrator/Documents/Rotation/Photos_to_test_model"
internet_file_type <- ".jpg"
#####
class_names <- folder_names(pathname,training_folder)
############################################
# get label numbers after specify name of fruit
fruit_list <- c("Apple_Braeburn","Banana","Orange","Grape_White","Kiwi")
class_names_frame <- cbind(class_names,seq(length(class_names)))
class_names_frame <- as.data.frame(class_names_frame,stringsAsFactors=F)
colnames(class_names_frame) <- c('Fruit','label')
labels_to_be_tested <- as.numeric(filter(class_names_frame,Fruit %in% fruit_list)[,2]) # gives the ones specified in fruit list
# labels_to_be_tested <- c(35,78)
#labels_to_be_tested <- 1:(length(class_names))
############################################
# model parameters
channels <- 1 # how many channels
channel_no <- 2 # which of these channels?
My_filter_number<- 32
My_kernel_size  <- c(3,3)
My_batch_size <- 32
My_epoch_number <- 5
###
output_n <- 1 + length(class_names) # number of outputs

filetype <- ".jpg" # workins with ".jpg" or ".png"?

xshape <- 40 # image size
yshape <- 40 # image size
############################################
number_probs<-4 # this many probabilities quoted by multipred
############################################
resize_method <- 'bilinear' # or 'nearest' 
