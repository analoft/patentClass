library(keras)
load("output/processed.RData")
dictionary_dir <- 'GLOVE'
max_seq_len <- 300
max_words <- 20000
embedding_dim <- 100
valid_split <- 0.2


cat('Indexing word vectors.\n')

embeddings_index <- new.env(parent = emptyenv())
lines <- readLines(file.path(dictionary_dir, 'glove.6B.100d.txt'))
for (line in lines) {
  values <- strsplit(line, ' ', fixed = TRUE)[[1]]
  word <- values[[1]]
  coefs <- as.numeric(values[-1])
  embeddings_index[[word]] <- coefs
  #print(word)
}

cat(sprintf('Found %s word vectors.\n', length(embeddings_index)))

# second, prepare text samples and their labels
cat('Processing text dataset\n')

texts <- character()  # text samples
labels <- integer() # label ids
labels_index <- list()  # dictionary: label name to numeric id

# Score data
texts<-as.character(dat1$Text)
labels_index[unique(as.character($Category))]<-unique(as.numeric(as.factor(bbc_train$Category)))-1
labels<-as.numeric(as.factor(bbc_train$Category))-1

# finally, vectorize the text samples into a 2D integer tensor
tokenizer <- text_tokenizer(num_words=max_words)
tokenizer %>% fit_text_tokenizer(bbc_train$Text)

#save tokenizer
save_text_tokenizer(tokenizer, "output/tokenizer")

sequences <- texts_to_sequences(tokenizer, bbc_train$Text)
word_index <- tokenizer$word_index
cat(sprintf('Found %s unique tokens.\n', length(word_index)))

data <- pad_sequences(sequences, maxlen=max_seq_len)
labels <- to_categorical(labels)

cat('Shape of data tensor: ', dim(data), '\n')
cat('Shape of label tensor: ', dim(labels), '\n')

# split the data into a training set and a validation set
indices <- 1:nrow(data)
indices <- sample(indices)
data <- data[indices,]
labels <- labels[indices,]
num_validation_samples <- as.integer(valid_split * nrow(data))

x_train <- data[-(1:num_validation_samples),]
y_train <- labels[-(1:num_validation_samples),]
x_val <- data[1:num_validation_samples,]
y_val <- labels[1:num_validation_samples,]

cat('Preparing embedding matrix.\n')

# prepare embedding matrix
num_words <- max(max_words, length(word_index) + 1)
prepare_embedding_matrix <- function() {
  embedding_matrix <- matrix(0L, nrow = num_words, ncol = embedding_dim)
  for (word in names(word_index)) {
    index <- word_index[[word]]
    if (index >= max_words)
      next
    embedding_vector <- embeddings_index[[word]]
    if (!is.null(embedding_vector)) {
      # words not found in embedding index will be all-zeros.
      embedding_matrix[index,] <- embedding_vector
    }
  }
  embedding_matrix
}

embedding_matrix <- prepare_embedding_matrix()

# load pre-trained word embeddings into an Embedding layer
# note that we set trainable = False so as to keep the embeddings fixed
embedding_layer <- layer_embedding(
  input_dim = num_words,
  output_dim = embedding_dim,
  weights = list(embedding_matrix),
  input_length = max_seq_len,
  trainable = FALSE
)

cat('Training model\n')

# train a 1D convnet with global maxpooling
sequence_input <- layer_input(shape = list(max_seq_len), dtype='int32')

preds <- sequence_input %>%
  embedding_layer %>% 
  layer_conv_1d(filters = 128, kernel_size = 5, activation = 'relu') %>% 
  layer_max_pooling_1d(pool_size = 5) %>% 
  layer_conv_1d(filters = 128, kernel_size = 5, activation = 'relu') %>% 
  layer_max_pooling_1d(pool_size = 5) %>% 
  layer_conv_1d(filters = 128, kernel_size = 5, activation = 'relu') %>% 
  layer_max_pooling_1d(pool_size = 35) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dense(units = length(labels_index), activation = 'softmax')


model <- keras_model(sequence_input, preds)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'rmsprop',
  metrics = c('acc')  
)

model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 10,
  validation_data = list(x_val, y_val)
)