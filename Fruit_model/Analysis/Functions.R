image_tester <- function(pathname_of_images,folder){
  
  internet_fold_path <- paste(pathname_of_images,folder,sep = '/')
  internet_image_names <- list.files(path = internet_fold_path, pattern=params$internet_file_type, all.files=T, full.names=F, no.. = T)
  
  path_name_new <- paste(pathname_of_images,folder,internet_image_names[1],sep = '/')
  im <- readImage(path_name_new)
  
  min_dimension <- which.min(c(dim(im)[1],dim(im)[2]))
  im_size<-dim(im)[min_dimension] -1 # shouldn't need -1, but didn't like it otherwise
  eq_sp <- cropImage(im, new_width = im_size, new_height = im_size, type = 'equal_spaced')
  im <- resizeImage(eq_sp, width = params$xshape, height = params$yshape, method = params$resize_method)
  imageShow(im)
  data <- as.data.frame(im)
  
  if(length(internet_image_names)==1){
    data <- abind(data,data,along=3) # gets into a 4 dimensional array
    data <- aperm(data,c(3,1,2)) # reorders elements
    data <- array_reshape(data,c(2,params$xshape,params$yshape,3),order=c("F"))
    data <- data[1,,,,drop=F]
  }
  else{
    for(k in 2:(length(internet_image_names))){
      path_name_new <- paste(pathname_of_images,folder,internet_image_names[k],sep = '/')
      im <- readImage(path_name_new)
      
      min_dimension <- which.min(c(dim(im)[1],dim(im)[2]))
      im_size<-dim(im)[min_dimension] -1 # shouldn't need -1, but didn't like it otherwise
      eq_sp <- cropImage(im, new_width = im_size, new_height = im_size, type = 'equal_spaced')
      im <- resizeImage(eq_sp, width = params$xshape, height = params$yshape, method = params$resize_method)
      
      new_data <- as.data.frame(im)
      
      data <- abind(data,new_data,along=3) # gets into a 4 dimensional array
    }
    data <- aperm(data,c(3,1,2)) # reorders elements
    data <- array_reshape(data,c(length(internet_image_names),params$xshape,params$yshape,3),order=c("F"))
  }
    data <- data[,,,params$channel_no,drop=F]
  return(data)
}


############################################
# multipreds returns number_of_probs many predictions for each image, ranked by most to least likely.
multipreds <- function(result,number_of_probs){
  frame<-as.data.frame(result[1,])
  colnames(frame) <- "Probability"
  frame2<-mutate(frame,Label = 0:(length(frame[,1])-1))  # because runs from 1 to 82 not 0 to 81?
  frame3<-mutate(frame2,Rank = rank(desc(Probability))) %>%
    arrange(Rank)
  frame4<-filter(frame3,Rank <= number_of_probs)
  frame4$Prediction <- params$class_names[frame4$Label]
  frame4$Image_number <- paste('Image',1)
  frame4 <- frame4[c("Image_number","Prediction","Probability","Label","Rank")]
  final_frame<-frame4
  if(nrow(result)>1){
    for(k in 2:nrow(result)){
      frame<-as.data.frame(result[k,]) # t(result)?
      colnames(frame) <- "Probability"
      frame2<-mutate(frame,Label = 0:(length(frame[,1])-1))  # because runs from 1 to 82 not 0 to 81?
      frame3<-mutate(frame2,Rank = rank(desc(Probability))) %>%
        arrange(Rank)
      frame4<-filter(frame3,Rank <= number_of_probs)
      frame4$Prediction <- params$class_names[frame4$Label]
      frame4$Image_number <- paste('Image',k)
      frame4 <- frame4[c("Image_number","Prediction","Probability","Label","Rank")]
      final_frame <- rbind(final_frame,frame4)
    }
  }
  return(final_frame)
}


############################################
# returns the prediction and corresponding probability for each image.
preds <- function(result){#,number_of_preds
  prediction_and_prob <- image_predictor(1,result)
  prediction_and_prob <- as.data.frame(prediction_and_prob)
  if(length(result[,1])>1){
    for(k in 2:length(result[,1])){
      new_prediction <- image_predictor(k,result)
      new_prediction <- as.data.frame(new_prediction)
      prediction_and_prob<-rbind(prediction_and_prob,new_prediction)
    }
  }
  prediction_and_prob$Prediction <- params$class_names[prediction_and_prob$Label]
  prediction_and_prob <- prediction_and_prob[c("Prediction","Probability","Label")]
  return(prediction_and_prob)
}
############################################
image_predictor <- function(n,results_of_model){
  pred <- which.max(results_of_model[n, ])-1
  prob <- results_of_model[n,pred+1]
  return(list('Label' = pred,'Probability' = prob))
}

# image_predictor <- function(n,results_of_model,number_of_preds){
#   len <- length(results_of_model[n, ])
#   pred <- rep(1,number_of_preds)
#   prob <- rep(1,number_of_preds)
#   for(k in 1:number_of_preds){
#   prob[k] <- sort(results_of_model[n, ], partial=len-k)[len-k]
#   pred[k] <- filter(results_of_model[n, ],prob[k]) - 1
#   }
#   else{prob <- results_of_model[n,pred+1]}
#   return(list('Label' = pred,'Probability' = prob))
# }
