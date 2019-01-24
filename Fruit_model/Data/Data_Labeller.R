source('~/GitHub/NIAB_Rotation/Fruit_model/Run/Parameters.R')
library(abind)
library(OpenImageR)

#contains functions 'folder_names', 'labeller', 'Data_labelled', 'Data_in_final_form'

##################################################

folder_names <- function(pathname_of_images,folder_name){
path_name_with_folder <- paste(pathname_of_images,folder_name,sep = "/")
folders <- list.dirs(path = path_name_with_folder, full.names = TRUE, recursive = TRUE)
class_names_from_folder <- gsub(path_name_with_folder,"",folders)
class_names_from_folder <- class_names_from_folder[2:(length(class_names_from_folder))] # ignores containing folder
class_names_from_folder <- gsub("/","",class_names_from_folder)
return(class_names_from_folder)
}

##################################################

labeller <- function(pathname_of_images,folder,class_names_used,n,file_type){
  path_name_new <- paste(pathname_of_images,folder,class_names_used[n],sep = '/')
  files <- list.files(path = path_name_new, pattern=file_type,all.files=T, full.names=F, no.. = T) 
  array <- cbind(files,b=n)
  return(array)
}

##################################################

Data_in_final_form <- function(pathname_of_images,folder,class_names_used,list_of_labels_to_be_tested,file_type){
  
  Fruit_train_data <- labeller(pathname_of_images,folder,class_names_used,1,file_type)
  for( i in 2:(length(class_names))){
    new_data <- labeller(pathname_of_images,folder,class_names_used,i,file_type)
    Fruit_train_data <- rbind(Fruit_train_data,new_data)
  }
  Fruit_frame <- as.data.frame(Fruit_train_data)
  colnames(Fruit_frame) <- c('File','Label')
  
  
##########
  j <- list_of_labels_to_be_tested[1]
  print(paste('Set number',j)) # gives idea of progress.
  Fruit_filtered_data<-filter(Fruit_frame,Label==j)
  number_of_files_filtered <- length(Fruit_filtered_data[,2])
  total_files <- number_of_files_filtered
  
  image_number <- 1
  path <- paste(pathname_of_images,folder,class_names_used[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/') # was Fruit_train_data
  
  im <- readImage(path)
  im <- resizeImage(im, width = xshape, height = yshape, method = 'bilinear')
  data <- as.data.frame(im)
  #imageShow(im)
  
  label <- as.data.frame(j)
  colnames(label)<-'label'
  
  for(i in 2:number_of_files_filtered){
    image_number <- i
    path <- paste(pathname_of_images,folder,class_names_used[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    
    im <- readImage(path)
    im <- resizeImage(im, width = xshape, height = yshape, method = 'bilinear')
    new_data <- as.data.frame(im)
    
    data <- abind(data,new_data,along=3)
    label <- rbind(label,j)
  }

############
  
  for(j in list_of_labels_to_be_tested[-1]){
  print(paste('Set number',j)) # gives idea of progress.
  Fruit_filtered_data<-filter(Fruit_frame,Label==j)
  number_of_files_filtered <- length(Fruit_filtered_data[,2])
  total_files <- number_of_files_filtered + total_files
  
  for(i in 1:number_of_files_filtered){
    image_number <- i
    path <- paste(pathname_of_images,folder,class_names_used[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    
    im <- readImage(path)
    im <- resizeImage(im, width = xshape, height = yshape, method = 'bilinear')
    new_data <- as.data.frame(im)
    
    data <- abind(data,new_data,along=3)
    label <- rbind(label,j)
  }
  }
  
  return(list('Image_array'=data,'label_vector'=label,'total_no'=total_files))
}


############################################

image_tester <- function(pathname_of_images,folder){
  
  internet_fold_path <- paste(pathname_of_images,folder,sep = '/')
  internet_image_names <- list.files(path = internet_fold_path, pattern=internet_file_type, all.files=T, full.names=F, no.. = T)
  
  path_name_new <- paste(pathname_of_images,folder,internet_image_names[1],sep = '/')
  im <- readImage(path_name_new)
  
  min_dimension <- which.min(c(dim(im)[1],dim(im)[2]))
  im_size<-dim(im)[min_dimension] -1 # shouldn't need -1, but didn't like it otherwise
  eq_sp <- cropImage(im, new_width = im_size, new_height = im_size, type = 'equal_spaced')
  im <- resizeImage(eq_sp, width = xshape, height = yshape, method = 'bilinear')
  imageShow(im)
  data <- as.data.frame(im)
  
  if(length(internet_image_names)==1){
    data <- abind(data,data,along=3) # gets into a 4 dimensional array
    data <- aperm(data,c(3,1,2)) # reorders elements
    data <- array_reshape(data,c(2,xshape,yshape,3),order=c("F"))
    data <- data[1,,,,drop=F]
  }
  else{
    for(k in 2:(length(internet_image_names))){
      path_name_new <- paste(pathname_of_images,folder,internet_image_names[k],sep = '/')
      im <- readImage(path_name_new)
      
      min_dimension <- which.min(c(dim(im)[1],dim(im)[2]))
      im_size<-dim(im)[min_dimension] -1 # shouldn't need -1, but didn't like it otherwise
      eq_sp <- cropImage(im, new_width = im_size, new_height = im_size, type = 'equal_spaced')
      im <- resizeImage(eq_sp, width = xshape, height = yshape, method = 'bilinear')
      
      new_data <- as.data.frame(im)
      
      data <- abind(data,new_data,along=3) # gets into a 4 dimensional array
    }
    data <- aperm(data,c(3,1,2)) # reorders elements
    data <- array_reshape(data,c(length(internet_image_names),xshape,yshape,3),order=c("F"))
  }
  
  return(data)
}

############################################

Train_data_and_labels <- Data_in_final_form(pathname,training_folder,class_names,labels_to_be_tested,filetype)

Train_data <-Train_data_and_labels$Image_array
Train_data <- aperm(Train_data,c(3,1,2)) # reorders elements
Train_labels <- Train_data_and_labels$label_vector[,1]
Train_total  <- Train_data_and_labels$total_no
####

Test_data_and_labels <- Data_in_final_form(pathname,test_folder,class_names,labels_to_be_tested,filetype)

Test_data <-Test_data_and_labels$Image_array
Test_data <- aperm(Test_data,c(3,1,2)) # reorders elements
Test_labels <- Test_data_and_labels$label_vector[,1]
Test_total  <- Test_data_and_labels$total_no

############################################

Train_data_reshaped <- array_reshape(Train_data,c(Train_total,xshape,yshape,3),order=c("F"))
Test_data_reshaped <- array_reshape(Test_data,c(Test_total,xshape,yshape,3),order=c("F"))




