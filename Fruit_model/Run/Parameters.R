############################################
pathname <- "C:/Users/Administrator/Documents/Rotation/fruits-360"
training_folder <- "Training"
test_folder <- "Test"

class_names <- folder_names(pathname,training_folder)
fruit_list <- c("Walnut","Banana")
############################################
# get label numbers after specify name of fruit
class_names_frame <- cbind(class_names,seq(length(class_names)))
class_names_frame <- as.data.frame(class_names_frame,stringsAsFactors=F)
colnames(class_names_frame) <- c('Fruit','label')
labels_to_be_tested <- as.numeric(filter(class_names_frame,Fruit %in% fruit_list)[,2])
#labels_to_be_tested <- 51:52
############################################
channels <- 1
channel_no <- 2
############################################
output_n <- 1 + length(class_names)