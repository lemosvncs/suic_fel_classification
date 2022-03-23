library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

fel <- read.csv("fel_t.csv")

# df <- suic[suic$labels == "fel" | suic$labels == "n", ]
df <- fel
df <- na.omit(df)
table(df$class)
train_id <- sample.int(nrow(df), size = nrow(df) * 0.8)

train <- df[train_id, ]
test <- df[-train_id, ]

# 

num_words <- 10000
max_length <- 280
text_vectorization <- layer_text_vectorization(
  max_tokens = num_words,
  output_sequence_length = max_length
)

text_vectorization %>%
  adapt(df$text)

# get_vocabulary(text_vectorization

# embeddings <- layer_hub(
#   handle = "https://tfhub.dev/tensorflow/bert_multi_cased_L-12_H-768_A-12/4",
#   trainable = FALSE
# )

# Model ----

input <- layer_input(shape = c(1), dtype = "string")
output <- input %>%
  # embeddings %>%
  text_vectorization() %>%
  layer_embedding(input_dim = num_words + 1, output_dim =  16) %>%
  # layer_hub(handle = "https://tfhub.dev/google/LaBSE/2", trainable = FALSE) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(input, output)

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list('accuracy')
)

history_fel <- model %>% fit(
  train$text,
  as.numeric(train$class == "1"),
  epochs = 1000,
  batch_size = 512,
  validation_split = .2,
  verbose = 2
)

results <- model %>% evaluate(test$text, as.numeric(test$class == "1"), verbose = 0)
results

save_model_tf(model, "fel_model.tf")
saveRDS(history, "fel_history.RDS")
saveRDS(results, "fel_results.RDS")
