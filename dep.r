library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

suic <- read.csv("suic_v2.csv")

df <- suic[suic$labels == "dep" | suic$labels == "n", ]
table(df$labels)
s <- sample(x = nrow(df), size = table(df$labels)[1])
n <- df[s, ]
dep <- df[df$labels == "dep", ]
df <- rbind(n, dep)
table(df$labels)
train_id <- sample.int(nrow(df), size = nrow(df) * 0.8)

train <- df[train_id, ]
test <- df[-train_id, ]

# 

num_words <- 10000
max_length = 280
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
summary(model)

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list('accuracy')
)

history_dep <- model %>% fit(
  train$text,
  as.numeric(train$labels == "dep"),
  epochs = 1000,
  batch_size = 512,
  validation_split = .2,
  verbose = 2
)
#savehistory(history_suic)
saveRDS(plot(history_dep), "plot_history_dep.RDS")
write.csv(history_dep, "history_dep.csv")

results <- model %>% evaluate(test$text, as.numeric(test$labels == "dep"), verbose = 0)
results

save_model_tf(model, "dep_model3.tf")
#saveRDS(history, "suic_history.RDS")
saveRDS(results, "dep_results24.RDS")

