library(keras)
library(tensorflow)
library(RMariaDB)

con <- dbConnect(RMariaDB::MariaDB(),
                 dbname = "mestrado",
                 username = "vinilemos",
                 password = "DZqVWhvn5hpsKm",
                 host = '144.22.229.245',
                 # host = "vinilemos.com.br",
                 port = 3306)

dep_model <- load_model_tf('dep_model3.tf')
suic_model <- load_model_tf('suic_model3.tf')
fel_model <- load_model_tf('fel_model.tf')
#suic <- readRDS('suic_model.RDS')
#fel <- readRDS('fel_model.RDS')
td <- dbGetQuery(conn = con, "SELECT text, created_at, status_id,
                 fel_y_hat, dep_y_hat, suic_y_hat FROM td10")
# 
# bs <- readRDS('todas_as_buscas.rds')
# bs <- bs[, c("text", "status_id", "created_at", "is_quote", "is_retweet",
#              "favorite_count", "retweet_count", "quote_count", "reply_count")]
# bs <- as.data.frame(bs)
# 
# fel <- read.csv("felicidade.csv")
# fel <- fel[, c("text", "status_id", "created_at", "is_quote", "is_retweet",
#              "favorite_count", "retweet_count", "quote_count", "reply_count")]

# dbWriteTable(conn = con, name = "suic", value = bs, overwrite = TRUE)
# dbWriteTable(conn = con, name = "felicidade", value = fel, overwrite = TRUE)

num_words <- 10000
max_length = 280
text_vectorization <- layer_text_vectorization(
  max_tokens = num_words,
  output_sequence_length = max_length
)

text_vectorization %>%
  adapt(td$text)

td$dep_y <- predict(dep_model, x = td$text)
td$fel_y <- predict(fel_model, x = td$text)
td$suic_y <- predict(suic_model, x = td$text)

dbWriteTable(conn = con, name = "td10", value = td)
