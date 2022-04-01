#write.csv(dep_results24, "dep_results24.csv")
#write.csv(fel_results, "fel_results24.csv")
#write.csv(suic_results24, "suic_results24.csv")
library(textmineR)
library(stopwords)
library(tidytext)

td <- read.csv("td10.csv")
td$X = NULL
td$id <- 1:10000

library(stringi)
td$text <- stri_replace_all(td$text, "", regex = "<.*?>")
td$text <- stri_replace_all(td$text, "", regex = "\\d")
td$text <- stri_trim(td$text)
td$text <- stri_trans_tolower(td$text)

td[td$fel_y_hat >= 0.5, "label"] <- "fel"
td[td$dep_y_hat >= 0.5, "label"] <- "dep"
td[td$suic_y_hat >= 0.5, "label"] <- "suic"

td <- na.omit(td)

st <- as.data.frame(stopwords("pt"))
colnames(st) <- 'word'
st <- rbind(st, "é", "�", "�", "�", "j�", "206", "�", "j�", "3", "gt", "gtgt", "t�", "t�o", "tã£o", "pra", "vc",
            "vo", 'ja')
# Tdiytable

library(tidyverse)
tt <- td %>%
  tibble() %>%
  group_by(id, label) %>%
  unnest_tokens(output = word, input = text,
                token = "tweets") %>%
  anti_join(st) %>%
  anti_join(stop_words, copy = TRUE)
  
# tt[tt$word == "tã£o", "word"] <- "tão"
tt[tt$word == "ã¢nimo", "word"] <- "ânimo"
tt[tt$word == "ãšnica", "word"] <- "única"
tt[tt$word == "ningu�m", "word"] <- "ninguém"
tt[tt$word == "n�o", "word"] <- "não"
tt[tt$word == "n�o", "word"] <- "não"
tt[tt$word == "emo�oes", "word"] <- 'emoções'
tt[tt$word == "m�sica", "word"] <- 'música'
tt[tt$word == "m�sica", "word"] <- 'música'
tt[tt$word == "�ltima", "word"] <- 'última'
#tt[tt$word == "t�", "word"] <- "tô"
#tt[tt$word == "tã", "word"] <- "t�o"

tt$word <- as.character(tt$word)

count <- tt %>%
  add_count(id, name = "n") %>%
  # count(word, sort = TRUE) %>%
  # mutate(word = reorder(word, n)) %>%
  group_by(id, n) %>%
  count(word, sort = TRUE)

  bind_tf_idf(term = word, document = label, n = n)

# count <- tt %>%
#   count(word, sort = TRUE) %>%
#   mutate(word = reorder(word, n)) %>%
#   
#   bind_tf_idf(term = word, document = label, n = n)

count[count$label == "dep", ] %>%
  head(15) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  ylab("Palavras") +
  xlab("Contagem") +
  ggtitle("Contagem de palavras relacionadas à depressão") +
  theme_bw()

count[count$label == "suic", ] %>%
  head(15) %>%
  # arrange(desc(by = n)) %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(word, n))) +
  ylab("Palavras") +
  xlab("Contagem") +
  ggtitle("Contagem de palavras relacionadas à suicídio") +
  theme_bw()

count[count$label == "fel", ] %>%
  head(15) %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(word, n))) +
  ylab("Palavras") +
  xlab("Contagem") +
  ggtitle("Contagem de palavras relacionadas à Bem-estar") +
  theme_bw()

# TF-IDF
count <- count[count$label == "fel", ] 

count[count$label == "fel", ] %>%
  slice_max(tf_idf, n = 10) %>%
  ggplot() +
  geom_col(aes(x = tf_idf, y = reorder(word, tf_idf))) +
  ylab("Palavras") +
  xlab("tf-idf") +
  ggtitle("tf-idf de palavras relacionadas ao Bem-estar") +
  theme_bw()

count[count$label == "dep", ] %>%
  #head(10) %>%
  slice_max(tf_idf, n = 10) %>%
  #head(10) %>%
  ggplot() +
  geom_col(aes(x = tf_idf, y = fct_reorder(word, tf_idf))) +
  ylab("Palavras") +
  xlab("tf-idf") +
  ggtitle("tf-idf de palavras relacionadas à depressão") +
  theme_bw()

count[count$label == "suic", ] %>%
  #head(10) %>%
  slice_max(tf_idf, n = 10) %>%
  #head(10) %>%
  ggplot() +
  geom_col(aes(x = tf_idf, y = fct_reorder(word, tf_idf))) +
  ylab("Palavras") +
  xlab("tf-idf") +
  ggtitle("tf-idf de palavras relacionadas à suicídio") +
  theme_bw()
