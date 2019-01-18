source('~/GitHub/NIAB_Rotation/Fruit_model/Data_Labeller.R')
source('~/GitHub/NIAB_Rotation/Fruit_model/Fruit_model.R')

library(keras)
library(dplyr)
library(ggplot2)

max_train<-975 #10000 goes up to label 21
max_test<-300 #3400 goes up to label 21

Fruit_data_training <- Data_labelled('Training')
Train_data_and_labels <- Data_in_final_form(Fruit_data_training,'Training',max_train)

Train_data <-Train_data_and_labels$Image_array
Train_data <- aperm(Train_data,c(3,1,2)) # reorders elements
Train_labels <- Train_data_and_labels$label_vector[,1]

# im_number <- 3
# Train_data[,,im_number]
# Train_labels[im_number]

Fruit_data_test <- Data_labelled('Test')
Test_data_and_labels <- Data_in_final_form(Fruit_data_test,'Test',max_test)

Test_data <-Test_data_and_labels$Image_array
Test_data <- aperm(Test_data,c(3,1,2)) # reorders elements
Test_labels <- Test_data_and_labels$label_vector[,1]

####

#Train_labels2 <- Train_labels - 1
model_my_own <-create_fruit_model(Train_data,Train_labels,Test_data,Test_labels)
model_my_own %>% summary()
model_my_own %>% save_model_hdf5("fruit_model.h5")

results <- model_my_own %>% predict(Test_data)


####

Train_data_reshaped <- array_reshape(Train_data,c(max_train,100,100,3),order=c("F"))
Test_data_reshaped <- array_reshape(Test_data,c(max_test,100,100,3),order=c("F"))

model_my_own_conv <-create_fruit_model_conv(Train_data_reshaped,Train_labels,Test_data_reshaped,Test_labels)
model_my_own_conv %>% summary()
model_my_own_conv %>% save_model_hdf5("fruit_model_conv.h5")

results <- model_my_own_conv %>% predict(Test_data_reshaped[1,,,], Test_labels[1])




####


plot_fruit(500)

plot_fruit <- function(k){
n <- as.data.frame(Train_data[k,,]) # new attempt
colnames(n) <- seq_len(ncol(n)) # new attempt
width <- (ncol(n))/3
n$y <- seq_len(nrow(n)) # new attempt
n <- gather(n, "x", "value", -y) # new attempt
n$x <- as.integer(n$x) # new attempt
n

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
}