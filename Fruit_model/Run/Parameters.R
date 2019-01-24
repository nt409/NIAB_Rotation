############################################
pathname <- "C:/Users/Administrator/Documents/Rotation/fruits-360"
training_folder <- "Training"
test_folder <- "Test"
#####
internet_path <- "C:/Users/Administrator/Documents/Rotation/Photos_to_test_model"
internet_file_type <- ".jpg"
#####
class_names <- folder_names(pathname,training_folder)
fruit_list <- c("Walnut","Banana")
############################################
# get label numbers after specify name of fruit
class_names_frame <- cbind(class_names,seq(length(class_names)))
class_names_frame <- as.data.frame(class_names_frame,stringsAsFactors=F)
colnames(class_names_frame) <- c('Fruit','label')
#labels_to_be_tested <- as.numeric(filter(class_names_frame,Fruit %in% fruit_list)[,2])
#labels_to_be_tested <- 1:(length(class_names))
labels_to_be_tested <- c(15,37)

############################################
channels <- 3
channel_no <- 1:3
############################################
output_n <- 1 + length(class_names)

filetype <- ".jpg"

xshape <- 100
yshape <- 100
############################################
number_probs<-4