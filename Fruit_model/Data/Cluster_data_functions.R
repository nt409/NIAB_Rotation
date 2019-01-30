source('~/GitHub/NIAB_Rotation/Fruit_model/Run/params.R')

library(abind)
library(OpenImageR)
library(dplyr)
library(parallel)

##################################################
framer <- function(pathname_of_images,folder,list_of_labels_to_be_tested,file_type){
  Fruit_train_data <- labeller(pathname_of_images,folder,list_of_labels_to_be_tested[1],file_type)
  for( i in list_of_labels_to_be_tested[-1]){ # changed from 2:length(params$class_names)
    new_data <- labeller(pathname_of_images,folder,i,file_type)
    Fruit_train_data <- rbind(Fruit_train_data,new_data)
  }
  Fruitframe <- as.data.frame(Fruit_train_data)
  colnames(Fruitframe) <- c('File','Label')
  return(Fruitframe)
}

##################################################

Cluster_data_in_final_form <- function(pathname_of_images,folder,list_of_labels_to_be_tested,file_type){
  
  
  ###
  # not sure why this step is necessary?
  pathname_of_images_env <- pathname_of_images
  folder_env <- folder
  list_of_labels_to_be_tested_env <- list_of_labels_to_be_tested
  file_type_env <- file_type
  ##
  classnames_env <- params$class_names
  xshape_env <- params$xshape
  yshape_env <- params$yshape
  resize_method_env <- params$resize_method
  # not sure why this step is necessary?
  ###
  
  Fruit_frame <- framer(pathname_of_images,folder,list_of_labels_to_be_tested,file_type)
  
  ##################################################
  acomb <- function(...) abind(..., along=3)
  ##################################################
  
  clustering <- function(j){
    Fruit_filtered_data<- filter(Fruit_frame,Label==j) #  frame,Label==j
    print(Fruit_filtered_data)
    number_of_files_filtered <- length(Fruit_filtered_data[,2])
    image_number<-1
    path <- paste(pathname_of_images_env,folder_env,classnames_env[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    im <- readImage(path)
    im <- resizeImage(im, width = xshape_env, height = yshape_env, method = resize_method_env)
    data <- as.data.frame(im)
    
    for(i in 2:number_of_files_filtered){
      image_number <- i
      path <- paste(pathname_of_images_env,folder_env,classnames_env[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
      
      im <- readImage(path)
      im <- resizeImage(im, width = xshape_env, height = yshape_env, method = resize_method_env)
      new_data <- as.data.frame(im)
      
      data <- abind(data,new_data,along=3)
    }
    return(data)
  }
  ##################################################
  no_cores <- detectCores() - 1
  cl<-makeCluster(no_cores) # Initiate cluster
  registerDoParallel(cl)
  
  array <- foreach(input = list_of_labels_to_be_tested_env,
                 .combine = acomb,
                 .packages=c('OpenImageR','dplyr','abind'))  %dopar%
    clustering(input)
  
  ## need to get labels coming out in the same order.
  
  stopCluster(cl)
  
  return(array)
}

Train_data_and_labels <- Cluster_data_in_final_form(params$pathname,params$training_folder,params$labels_to_be_tested,params$filetype)