library(abind)
library(OpenImageR)

folder_names <- function(path_name,folder_name){
path_name_with_folder <- paste(path_name,folder_name,sep = "/")
folders <- list.dirs(path = path_name_with_folder, full.names = TRUE, recursive = TRUE)
class_names_from_folder <- gsub(path_name_with_folder,"",folders)
class_names_from_folder <- class_names_from_folder[2:(length(class_names_from_folder))] # ignores containing folder
class_names_from_folder <- gsub("/","",class_names_from_folder)
return(class_names_from_folder)
}

pathname <- "C:/Users/Administrator/Documents/Rotation/fruits-360"
train_fold <- "Training"
test_fold <- "Test"
folder_names(pathname,test_fold)
class_names <- folder_names(pathname,train_fold)

Data_labelled <- function(folder){

labeller <- function(n){
  pathname <- paste("C:/Users/Administrator/Documents/Rotation/fruits-360",folder,class_names[n],sep = '/')
  files <- list.files(path = pathname, pattern=".jpg",all.files=T, full.names=F, no.. = T) 
  array <- cbind(files,b=n)
  return(array)
}

Fruit_train_data <- labeller(1)
for( i in 2:(length(class_names))){
  new_data <- labeller(i)
  Fruit_train_data <- rbind(Fruit_train_data,new_data)
}

return(Fruit_train_data)
}


Data_in_final_form <- function(Fruit_train_data,folder,maxi){
  image_number <- 1
  N <- as.numeric(Fruit_train_data[image_number,2]) # label
  path <- paste("C:/Users/Administrator/Documents/Rotation/fruits-360",folder,class_names[N],as.character(Fruit_train_data[image_number,1]),sep = '/')
  
  im <- readImage(path)
  data <- as.data.frame(im)
  
  label <- as.data.frame(N)
  colnames(label)<-'label'
  for(i in 2:maxi){
    image_number <- i
    N <- as.numeric(Fruit_train_data[image_number,2])
    path <- paste("C:/Users/Administrator/Documents/Rotation/fruits-360",folder,class_names[N],as.character(Fruit_train_data[image_number,1]),sep = '/')
    
    im <- readImage(path)
    new_data <- as.data.frame(im)
    
    data <- abind(data,new_data,along=3)
    label <- rbind(label,N)
  }
  return(list('Image_array'=data,'label_vector'=label))
}

# im <- image_load(path, target_size = c(x_size,y_size)) # attempt to get size control - have as extra argument
# a <- image_to_array(im) # attempt to get size control - have as extra argument
# data <- as.data.frame(a)


# im <- image_load(path, target_size = c(x_size,y_size)) # attempt to get size control
# a <- image_to_array(im) # attempt to get size control
# data <- as.data.frame(a)




# to check a random image from the data set
# image_number <- 40000
# N <- as.numeric(Fruit_train_data[image_number,2])
# N
# class_names[N]

# path <- paste("C:/Users/Administrator/Documents/Rotation/fruits-360/Training",class_names[N],as.character(Fruit_train_data[image_number,1]),sep = '/')
# im <- readImage(path)
# dim(im) # im is the array
# imageShow(im)