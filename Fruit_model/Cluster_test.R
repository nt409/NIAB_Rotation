library(foreach)
library(doParallel)

pathname_of_images <- params$pathname
folder <- params$training_folder
list_of_labels_to_be_tested<- params$labels_to_be_tested
file_type <- params$filetype

#Cluster_data_in_final_form(pathname_of_images,folder,list_of_labels_to_be_tested,file_type)
Fruit_train_data <- labeller(pathname_of_images,folder,list_of_labels_to_be_tested[1],file_type)
for( i in list_of_labels_to_be_tested[-1]){ # changed from 2:length(params$class_names)
  new_data <- labeller(pathname_of_images,folder,i,file_type)
  Fruit_train_data <- rbind(Fruit_train_data,new_data)
}
Fruit_frame <- as.data.frame(Fruit_train_data)
colnames(Fruit_frame) <- c('File','Label')

###################

clustering <- function(j){
  Fruit_filtered_data<- filter(Fruit_frame,Label==j) #  frame,Label==j
  print(Fruit_filtered_data)
  number_of_files_filtered <- length(Fruit_filtered_data[,2])
  image_number<-1
  path <- paste(pathname_of_images,folder,params$class_names[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')
  im <- readImage(path)
  im <- resizeImage(im, width = params$xshape, height = params$yshape, method = params$resize_method)
  data <- as.data.frame(im)
  
  for(i in 2:number_of_files_filtered){
    image_number <- i
    path <- paste(pathname_of_images,folder,params$class_names[j],as.character(Fruit_filtered_data[image_number,1]),sep = '/')

    im <- readImage(path)
    im <- resizeImage(im, width = params$xshape, height = params$yshape, method = params$resize_method)
    new_data <- as.data.frame(im)

    data <- abind(data,new_data,along=3)
    #label <- rbind(label,j)
  }
  
return(data)
}

acomb <- function(...) abind(..., along=3)

############
### parallelise

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores) # Initiate cluster
registerDoParallel(cl)

shape<-foreach(exponent = 2:4, 
        .combine = acomb,
        .packages=c('OpenImageR','dplyr','abind'))  %dopar%  
  clustering(exponent)

shape

stopCluster(cl)

######################################


cluster_function <- function(j){

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
  return(data)
}


###################

Cluster_data_in_final_form <- function(pathname_of_images,folder,list_of_labels_to_be_tested,file_type){
  

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
  
 

  
  #data <-  clusterApply(cl, data,label,Fruit_frame,pathname_of_images,folder, list_of_labels_to_be_tested[-1], cluster_function)
  
  output <- foreach(input = list_of_labels_to_be_tested[-1], 
                    .combine = acomb, .multicombine=TRUE)  %dopar%
    cluster_function(input)
  
  data <- output
  print(output)
  
  
  return(output) # list('Image_array'=data,'label_vector'=label))
}


