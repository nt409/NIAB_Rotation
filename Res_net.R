# instantiate the model
source('~/GitHub/NIAB_Rotation/multiplot.R')
library(keras)
library(tidyr)
library(ggplot2)
model <- application_resnet50(weights = 'imagenet')



rgb_and_features <- function(image_name){
# load the image
img_path <- image_name
img <- image_load(img_path, target_size = c(224,224))
a <- image_to_array(img)

z <- as.data.frame(a)
colnames(z) <- seq_len(ncol(z))
width <- (ncol(z))/3


n <- as.data.frame(a) # new attempt
colnames(n) <- seq_len(ncol(n)) # new attempt
width <- (ncol(n))/3
n$y <- seq_len(nrow(n)) # new attempt
n <- gather(n, "x", "value", -y) # new attempt
n$x <- as.integer(n$x) # new attempt
n



#r<- z[,1:(width)]
#g<- z[,(width+1):(2*width)]
#b<- z[,(2*width+1):(3*width)]
#r$y <- seq_len(nrow(z))
#g$y <- seq_len(nrow(z))
#b$y <- seq_len(nrow(z))
#r <- gather(r, "x", "value", -y)
#g <- gather(g, "x", "value", -y)
#b <- gather(b, "x", "value", -y)
#r$x <- as.integer(r$x)
#g$x <- as.integer(g$x)
#b$x <- as.integer(b$x)

r<-n[1:(width^2),]
g<-n[(width^2+1):(2*width^2),]
b<-n[(2*width^2 +1):(3*width^2),]


p0 <- ggplot(n, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("") +
  ggtitle("R,G,B") +
  theme(plot.title = element_text(hjust = 0.5))

#p0_new <- ggplot(n[1:(width^2),], aes(x = x, y = y, fill = value)) +
#  geom_tile() +
#  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
#  scale_y_reverse() +
#  theme_minimal() +
#  theme(panel.grid = element_blank())   +
#  theme(aspect.ratio = 1) +
#  xlab("") +
#  ylab("") +
#  ggtitle("R") +
#  theme(plot.title = element_text(hjust = 0.5))


p1<-ggplot(r, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("") +
  ggtitle("R") +
  theme(plot.title = element_text(hjust = 0.5))

p2<-ggplot(g, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("") +
  ggtitle("G") +
  theme(plot.title = element_text(hjust = 0.5))

p3<-ggplot(b, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("") +
  ggtitle("B") +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p1,p2,p3,cols=3)



# ensure we have a 4d tensor with single element in the batch dimension,
# the preprocess the input for prediction using resnet50
a <- array_reshape(a, c(1, dim(a)))
a <- imagenet_preprocess_input(a)

# make predictions then decode and print them
preds <- model %>% predict(a)
prediction<-imagenet_decode_predictions(preds, top = 3)[[1]]

return(prediction)
}

rgb_and_features('C:/Users/Administrator/Documents/Rotation/0_100.jpg')
