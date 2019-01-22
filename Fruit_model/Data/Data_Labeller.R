library(abind)
library(OpenImageR)

#contains functions 'folder_names', 'labeller', 'Data_labelled', 'Data_in_final_form'

##################################################

folder_names <- function(path_name,folder_name){
path_name_with_folder <- paste(path_name,folder_name,sep = "/")
folders <- list.dirs(path = path_name_with_folder, full.names = TRUE, recursive = TRUE)
class_names_from_folder <- gsub(path_name_with_folder,"",folders)
class_names_from_folder <- class_names_from_folder[2:(length(class_names_from_folder))] # ignores containing folder
class_names_from_folder <- gsub("/","",class_names_from_folder)
return(class_names_from_folder)
}

##################################################

labeller <- function(folder,class_names_used,n){
  path_name_new <- paste(pathname,folder,class_names_used[n],sep = '/')
  files <- list.files(path = path_name_new, pattern=".jpg",all.files=T, full.names=F, no.. = T) 
  array <- cbind(files,b=n)
  return(array)
}

##################################################

Data_in_final_form <- function(folder,class_names_used,vector){
  
  Fruit_train_data <- labeller(folder,class_names_used,1)
  for( i in 2:(length(class_names))){
    new_data <- labeller(folder,class_names_used,i)
    Fruit_train_data <- rbind(Fruit_train_data,new_data)
  }
  Fruit_frame <- as.data.frame(Fruit_train_data)
  colnames(Fruit_frame) <- c('File','Label')
  
  
##########
  j <- vector[1]
  Fruit_filtered_data<-filter(Fruit_frame,Label==j)
  number_of_files_filtered <- length(Fruit_filtered_data[,2])
  total_files <- number_of_files_filtered
  
  image_number <- 1
  path <- paste(pathname,folder,class_names_used[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/') # was Fruit_train_data
  
  im <- readImage(path)
  data <- as.data.frame(im)
  
  label <- as.data.frame(j)
  colnames(label)<-'label'
  
  for(i in 2:number_of_files_filtered){
    image_number <- i
    path <- paste(pathname,folder,class_names_used[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    
    im <- readImage(path)
    new_data <- as.data.frame(im)
    
    data <- abind(data,new_data,along=3)
    label <- rbind(label,j)
  }

############
  
  for(j in vector[-1]){
  Fruit_filtered_data<-filter(Fruit_frame,Label==j)
  number_of_files_filtered <- length(Fruit_filtered_data[,2])
  total_files <- number_of_files_filtered + total_files
  
  for(i in 1:number_of_files_filtered){
    image_number <- i
    path <- paste(pathname,folder,class_names_used[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    
    im <- readImage(path)
    new_data <- as.data.frame(im)
    
    data <- abind(data,new_data,along=3)
    label <- rbind(label,j)
  }
  }
  
  return(list('Image_array'=data,'label_vector'=label,'total_no'=total_files))
}



##################################################

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