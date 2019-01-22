n <- as.data.frame(Train_data_reshaped[1,,,3])
colnames(n) <- seq_len(100) 
n$y <- seq_len(100) 
n <- gather(n, "x", "value", -y) 
n$x <- as.integer(n$x)
n

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

p0