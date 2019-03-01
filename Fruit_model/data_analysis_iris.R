library(data.table)
library(psych)
library(outliers)
library(glmnet)
library(rminer)
library(dplyr)
library(e1071)
library(xgboost)

# addded in family = multinomial to both glmnet and cv.glmnet

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 70, activation = "relu",
                input_shape = dim(sdata)[2]) %>%
    layer_dense(units = 70, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}



tune_svm <- function(sdata, f){
  
  tc <- tune.control(cross = 5)
  tuned_par <- tune.svm(f, data = sdata, 
                        gamma = 10^(-5:-1), cost = seq(1,5, by = 0.5),
                        epsilon = seq(0, 0.2, by = 0.01), tunecontrol = tc)
  return(tuned_par)
}

#' run SVM/regression
#' @param train_data train dataset
#' @param test_data test dataset
#' @param cn class name
#' @param dl descriptor list
#' @param p percentage split
#' @param f formula
run_svm <- function(train_data, test_data, cn, dl, f, tuned_par){
  
  svm_model <- svm(f, train_data, gamma = tuned_par$gamma, 
                   epsilon = tuned_par$epsilon, 
                   cost=tuned_par$cost, kernel = 'radial')
  
  
  predictYsvm <- predict(svm_model, test_data[, -ncol(test_data), 
                                              with = FALSE])
  
  plot(test_data[[cn]], predictYsvm, col = "blue", pch=4,  main = 'SVM',
       ylab= 'predicted', xlab = 'observed')
  res <- caret::postResample(as.numeric(test_data[[cn]]), predictYsvm)
  print(res)
  
  
  plot(test_data[[cn]], col='red', pch=19); abline(h=0,v=0,lty=3)
  points(predictYsvm,col="blue",cex=1) # show the support vectors
  
  
  return(predictYsvm)
}

#' search_outliers
#' @param sdata dataset
#' @param dl descriptor list
plot_data <- function(sdata, descriptor_list){
  
  for(descriptor in descriptor_list){
    
    plot(sdata[[descriptor]])
    if(readline(descriptor) == 'q') { break}
    
  }
}

#' search_outliers
#' @param sdata dataset
#' @param dl descriptor list

search_outliers <- function(sdata, dl){
  
  o_list <- list()
  for(descriptor in dl){
    o = match(outlier(sdata[[descriptor]]), sdata[[descriptor]])
    sdata[[descriptor]][o] = NA
    #print(o)
  }
  return(sdata)
  
}

#' run run_LASSO
#' @param train_data train dataset
#' @param test_data test dataset
#' @param cn class name
#' @param dl descriptor list

run_LASSO <- function(train_data, test_data, cn, dl){
  

  lambda <- 10^seq(10, -2, length = 100)
  cv.out <- cv.glmnet(as.matrix(train_data[,-ncol(train_data), with=FALSE]), 
                      as.matrix(train_data[,ncol(train_data), with=FALSE]), 
                      family="multinomial",
                      alpha = 0)
  
  
  lasso.mod <- glmnet(as.matrix(train_data[,-ncol(train_data), with=FALSE]), 
                      as.matrix(train_data[,ncol(train_data), with=FALSE]),
                      family="multinomial",
                      alpha = 1, lambda = lambda)
  
 
  bestlam <- cv.out$lambda.min
  n <- ncol(test_data)
  lasso.pred <- predict(lasso.mod, s = bestlam, 
                        newx = as.matrix(test_data[,n-1, with=FALSE]), 
                        type="response")
  plot(test_data[[class_name]], lasso.pred , col = "blue", pch=4,
       main = 'LASSO')
  res <- caret::postResample(as.numeric(test_data[[class_name]]), lasso.pred)
  print(res)
  return(lasso.pred)
}


feature_selection <- function(train_data, model_name = "xgboost" ){
  
  
  M <- fit(f, data = train_data, model=model_name)
  I <- Importance(M, data=train_data)
  i <- which(I$imp > 0)
  pdf('features_pdf.pdf')
  par(mfrow = c(3,5))
  for(x in i){
    vecplot(I,graph="VEC", xval=x,
            main=paste(colnames(train_data)[x], 'vs resp'),Grid=10)
    if(readline(x) == 'q') { break}
  }
  
  dev.off()
  L=list(runs=1,sen=t(I$imp),sresponses=I$sresponses)
  mgraph(L,graph="IMP",leg=names(train_data),col="gray",Grid=10)
}



run_gboosting <- function(train_data, test_data, class_name){
  
  n <- ncol(train_data)
  train.y <- as.matrix(train_data[,n, with = FALSE])
  #i <- ifelse(train_data[[class_name]] > 1, 0.5, 0.8)
  
  xgmodel <- xgboost(data = data.matrix(train_data[,-n, with = FALSE]), 
                     label = train.y, 
                     objective = "reg:linear", # "multi:softmax", # was ,
                     #num_class = 2, # N-1
                     max.depth = 2,
                     eta = 1,
                     nthread = 2,
                     min_child_weight = 1,
                     subsample = 0.5,
                     colsample_bytree = 1,
                     num_parallel_tree = 1,
                     nrounds = 2, 
                     booster =  'gblinear', #'gbtree', # was gblinear
                     eval_metric = 'rmse'  #'mlogloss' # 'rmse'
                     )

  
  
  xgb.pred <- predict(xgmodel, data.matrix(test_data[,-n, with = FALSE]))
  plot(test_data[[class_name]], xgb.pred , col = "blue", pch=4, main = 'GB')
  res <- caret::postResample(as.numeric(test_data[[class_name]]), xgb.pred)
  print(res)
  
  
}

raw_data <- iris
class_name <- 'Species'
descriptor_list <- colnames(raw_data)[1:4]



#post_data <- raw_data[,descriptor_list, with = FALSE]
#post_data <- setDT(data.frame(scale(post_data)))
#post_data <- search_outliers(post_data, descriptor_list[-(length(descriptor_list))])

post_data <- setDT(raw_data[complete.cases(raw_data),]) 

correl <- cor(post_data[, 1:4])
co <- 0.3

# Formula
f <- as.formula(paste(class_name, '~ .'))

set.seed(42)
p <- 70
index <- sample(1:nrow(post_data), p*nrow(post_data)/100)
train_data <- post_data[index, c(descriptor_list, class_name), 
                        with = FALSE]
test_data  <- post_data[-index, c(descriptor_list, class_name), 
                        with = FALSE]

# Search the best parameters
o <- tune_svm(train_data, f)

tuned_par <- list()
tuned_par$gamma <- 0.01#o$best.parameters$gamma 
tuned_par$epsilon <- 0.04#o$best.parameters$epsilon
tuned_par$cost <- 2.5#o$best.parameters$cost
par(mfrow = c(3,1))
resSVM <- run_svm(train_data, test_data, class_name, 
                  descriptor_list, f, 
                  tuned_par)
resSVM # should accuracy, kappa not be NA?
resLASSO <-  run_LASSO(train_data, test_data, class_name, 
                       descriptor_list)
resGB <- run_gboosting(train_data, test_data, class_name)


