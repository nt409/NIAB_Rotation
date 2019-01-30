source('~/GitHub/NIAB_Rotation/Fruit_model/Run/params.R')

library(abind)
library(OpenImageR)
library(dplyr)
library(parallel)


#contains functions 'labeller', 'Data_in_final_form'

##################################################

labeller <- function(pathname_of_images,folder,image_label,file_type){
  path_name_new <- paste(pathname_of_images,folder,params$class_names[image_label],sep = '/')
  files <- list.files(path = path_name_new, pattern=file_type, all.files=T, full.names=F, no.. = T) 
  array <- cbind(files,b=image_label)
  return(array)
}


##################################################

Data_in_final_form <- function(pathname_of_images,folder,list_of_labels_to_be_tested,file_type){
  
  Fruit_train_data <- labeller(pathname_of_images,folder,list_of_labels_to_be_tested[1],file_type)
  for( i in list_of_labels_to_be_tested[-1]){ # changed from 2:length(params$class_names)
    new_data <- labeller(pathname_of_images,folder,i,file_type)
    Fruit_train_data <- rbind(Fruit_train_data,new_data)
  }
  Fruit_frame <- as.data.frame(Fruit_train_data)
  colnames(Fruit_frame) <- c('File','Label')
  
  
##########
  j <- list_of_labels_to_be_tested[1]
  print(paste('Set number',j)) # gives idea of progress.
  Fruit_filtered_data<-filter(Fruit_frame,Label==j)
  number_of_files_filtered <- length(Fruit_filtered_data[,2])
  
  image_number <- 1
  path <- paste(pathname_of_images,folder,params$class_names[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/') # was Fruit_train_data
  
  im <- readImage(path)
  im <- resizeImage(im, width = params$xshape, height = params$yshape, method = params$resize_method)
  data <- as.data.frame(im)
  #imageShow(im)
  
  label <- as.data.frame(j)
  colnames(label)<-'label'
  
  for(i in 2:number_of_files_filtered){
    image_number <- i
    path <- paste(pathname_of_images,folder,params$class_names[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    
    im <- readImage(path)
    im <- resizeImage(im, width = params$xshape, height = params$yshape, method = params$resize_method)
    new_data <- as.data.frame(im)
    
    data <- abind(data,new_data,along=3)
    label <- rbind(label,j)
  }

############

  for(j in list_of_labels_to_be_tested[-1]){
  print(paste('Set number',j)) # gives idea of progress.
  Fruit_filtered_data<-filter(Fruit_frame,Label==j)
  number_of_files_filtered <- length(Fruit_filtered_data[,2])
  
  for(i in 1:number_of_files_filtered){
    image_number <- i
    path <- paste(pathname_of_images,folder,params$class_names[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    
    im <- readImage(path)
    im <- resizeImage(im, width = params$xshape, height = params$yshape, method = params$resize_method)
    new_data <- as.data.frame(im)
    
    data  <- abind(data,new_data,along=3)
    label <- rbind(label,j)
  }
  }
  
  return(list('Image_array'=data,'label_vector'=label))
}





##################################################

Cluster_data_in_final_form <- function(pathname_of_images,folder,list_of_labels_to_be_tested,file_type){
  
  Fruit_train_data <- labeller(pathname_of_images,folder,list_of_labels_to_be_tested[1],file_type)
  for( i in list_of_labels_to_be_tested[-1]){ # changed from 2:length(params$class_names)
    new_data <- labeller(pathname_of_images,folder,i,file_type)
    Fruit_train_data <- rbind(Fruit_train_data,new_data)
  }
  Fruit_frame <- as.data.frame(Fruit_train_data)
  colnames(Fruit_frame) <- c('File','Label')
  
  
  ##########
  j <- list_of_labels_to_be_tested[1]
  print(paste('Set number',j)) # gives idea of progress.
  Fruit_filtered_data<-filter(Fruit_frame,Label==j)
  number_of_files_filtered <- length(Fruit_filtered_data[,2])
  
  image_number <- 1
  path <- paste(pathname_of_images,folder,params$class_names[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/') # was Fruit_train_data
  
  im <- readImage(path)
  im <- resizeImage(im, width = params$xshape, height = params$yshape, method = params$resize_method)
  data <- as.data.frame(im)
  #imageShow(im)
  
  label <- as.data.frame(j)
  colnames(label)<-'label'
  
  for(i in 2:number_of_files_filtered){
    image_number <- i
    path <- paste(pathname_of_images,folder,params$class_names[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    
    im <- readImage(path)
    im <- resizeImage(im, width = params$xshape, height = params$yshape, method = params$resize_method)
    new_data <- as.data.frame(im)
    
    data <- abind(data,new_data,along=3)
    label <- rbind(label,j)
  }
  
############
  ### parallelise
  
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  
  ##################################################
  
  acomb <- function(...) abind(..., along=3)
  
  cluster_function <- function(j){
    print(paste('Set number',j)) # gives idea of progress.
    Fruit_filtered_data<-filter(Fruit_frame[,2],j) #  frame,Label==j
    number_of_files_filtered <- length(Fruit_filtered_data[,2])
    
    image_number <- 1
    path <- paste(pathname_of_images,folder,params$class_names[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    
    im <- readImage(path)
    im <- resizeImage(im, width = params$xshape, height = params$yshape, method = params$resize_method)
    new_data <- as.data.frame(im)
    data <- abind(data,new_data,along=3)
    label <- rbind(label,j)
    
    for(i in 2:number_of_files_filtered){
      image_number <- i
      path <- paste(pathname_of_images,folder,params$class_names[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
      
      im <- readImage(path)
      im <- resizeImage(im, width = params$xshape, height = params$yshape, method = params$resize_method)
      new_data <- as.data.frame(im)
      
      data <- abind(data,new_data,along=3)
      label <- rbind(label,j)
    }
  data
  }

#data <-  clusterApply(cl, data,label,Fruit_frame,pathname_of_images,folder, list_of_labels_to_be_tested[-1], cluster_function)
  
output <- foreach(input = list_of_labels_to_be_tested[-1], 
                .combine = acomb, .multicombine=TRUE)  %dopar%
  cluster_function(input)  

data <- output
print(output)
#label <- output[[2]]

stopCluster(cl)

  
  return(output) # list('Image_array'=data,'label_vector'=label))
}


