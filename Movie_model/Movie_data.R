Movie_data <- function(){
  imdb <- dataset_imdb(num_words = 10000)
  
  c(train_data, train_labels) %<-% imdb$train
  c(test_data, test_labels) %<-% imdb$test
  
  #num words discards rare words and keeps 10000 most used words
  # maps words to integers
  word_index <- dataset_imdb_word_index()
  
  
  paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))
  
  # integer form for review 1
  #train_data[[1]]
  
  # reviews should be same length - resolved later
  #length(train_data[[1]])
  #length(train_data[[2]])
  
  word_index_df <- data.frame(
    word = names(word_index),
    idx = unlist(word_index, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  
  # The first indices are reserved  
  word_index_df <- word_index_df %>% mutate(idx = idx + 3)
  word_index_df <- word_index_df %>%
    add_row(word = "<PAD>", idx = 0)%>%
    add_row(word = "<START>", idx = 1)%>%
    add_row(word = "<UNK>", idx = 2)%>%
    add_row(word = "<UNUSED>", idx = 3)
  
  word_index_df <- word_index_df %>% arrange(idx)
  
  decode_review <- function(text){
    paste(map(text, function(number) word_index_df %>%
                filter(idx == number) %>%
                select(word) %>% 
                pull()),
          collapse = " ")
  }
  
  decode_review(train_data[[1]])
  
  # padding at the end
  
  train_data <- pad_sequences(
    train_data,
    value = word_index_df %>% filter(word == "<PAD>") %>% select(idx) %>% pull(),
    padding = "post",
    maxlen = 256
  )
  
  test_data <- pad_sequences(
    test_data,
    value = word_index_df %>% filter(word == "<PAD>") %>% select(idx) %>% pull(),
    padding = "post",
    maxlen = 256
  )
 
  # pick 10000 values from original training data
  
  x_val <- train_data[1:10000, ]
  partial_x_train <- train_data[10001:nrow(train_data), ]
  
  y_val <- train_labels[1:10000]
  partial_y_train <- train_labels[10001:length(train_labels)]
  newList <- list("x_val" = x_val,"partial_x_train" = partial_x_train,"y_val" = y_val,"partial_y_train" = partial_y_train,"test_data"=test_data,"test_labels"=test_labels)
  
return(newList)
}