source('~/GitHub/NIAB_Rotation/Fruit_model/Run/params.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Data/Data_Functions.R')# contains functions, 'labeller', 'Data_in_final_form'

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
  
  # pathname_of_images<-params$pathname
  # folder<-params$training_folder
  # list_of_labels_to_be_tested<-params$labels_to_be_tested
  # file_type<-params$filetype

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
  
  Fruit_frame <- framer(pathname_of_images_env,folder_env,list_of_labels_to_be_tested_env,file_type_env)
  
  ##################################################
  acomb <- function(...){
    Arguments <- list(...)
    datas<-Arguments[[1]]$Data_j
    labels<-Arguments[[1]]$Label_j
    if(length(Arguments)>1){
    for(i in 2:length(Arguments)){
    datas<-abind(datas,Arguments[[i]]$Data_j, along=3)
    labels<-rbind(labels,Arguments[[i]]$Label_j)
    }
    }
    return(list('Data'=datas,'Labels'=labels))
  }
  ##################################################
  
  clustering <- function(j){
    Fruit_filtered_data<- filter(Fruit_frame,Label==j)
    number_of_files_filtered <- length(Fruit_filtered_data[,2])
    
    image_number<-1
    path <- paste(pathname_of_images_env,folder_env,classnames_env[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
    im <- readImage(path)
    im <- resizeImage(im, width = xshape_env, height = yshape_env, method = resize_method_env)
    data <- as.data.frame(im)
    label <- as.data.frame(j)
    
    for(i in 2:number_of_files_filtered){
      image_number <- i
      path <- paste(pathname_of_images_env,folder_env,classnames_env[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
      
      im <- readImage(path)
      im <- resizeImage(im, width = xshape_env, height = yshape_env, method = resize_method_env)
      new_data <- as.data.frame(im)
      data <- abind(data,new_data,along=3)
      label <- rbind(label,j)
    }
    return(list('Data_j'=data,'Label_j'=label))
  }
  ##################################################
  no_cores <- detectCores() - 1
  cl<-makeCluster(no_cores) # Initiate cluster
  registerDoParallel(cl)
  
  array <- foreach(input = list_of_labels_to_be_tested_env,
                 .combine = acomb,
                 .multicombine = TRUE,
                 .packages=c('OpenImageR','dplyr','abind'))  %dopar%
    clustering(input)
  
  ## need to get labels coming out in the same order.
  
  stopCluster(cl)
  
  return(array)
}

# system.time(Train_data_and_labels <- Cluster_data_in_final_form(params$pathname,params$training_folder,params$labels_to_be_tested,params$filetype))
# Train_data_and_labels <- Cluster_data_in_final_form(params$pathname,params$training_folder,params$labels_to_be_tested,params$filetype)