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
td$text <- stri_trim(td$text)
td$text <- stri_trans_tolower(td$text)

td[td$fel_y_hat >= 0.5, "label"] <- "fel"
td[td$dep_y_hat >= 0.5, "label"] <- "dep"
td[td$suic_y_hat >= 0.5, "label"] <- "suic"

td <- na.omit(td)

st <- as.data.frame(stopwords("pt"))
colnames(st) <- 'word'
st <- rbind(st, "é")
# Tdiytable
library(tidyverse)
tt <- td %>%
  tibble() %>%
  group_by(label) %>%
  unnest_tokens(output = word, input = text,
                token = "tweets") %>%
  anti_join(st) %>%
  anti_join(stop_words, copy = TRUE)
  

tt
count <- tt %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))

count[count$label == "dep", ] %>%
  ggplot(aes(n, word)) +
  geom_col() +
  theme_bw()

count[count$label == "suic", ] %>%
  ggplot(aes(n, word)) +
  geom_col() +
  theme_bw()

count[count$label == "dep", ] %>%
  ggplot(aes(n, word)) +
  geom_col() +
  theme_bw()


