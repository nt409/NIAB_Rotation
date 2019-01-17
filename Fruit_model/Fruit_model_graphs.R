source('~/GitHub/NIAB_Rotation/multiplot.R')

rgb_plots <- function(image_path){

  img <- image_load(img_path, target_size = c(224,224))
  a <- image_to_array(img)
  
  n <- as.data.frame(a) # new attempt
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