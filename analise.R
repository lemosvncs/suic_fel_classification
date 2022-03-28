

library(quanteda)

dtm <- CreateDtm(
  doc_vec = td$text,
  doc_names = td$label,
  ngram_window = c(1, 2),
  stopword_vec = c(
    stopwords(language = "pt"),
    stopwords(source = "smart"),
    "http", "https", "é", "pra"
  )
)

dfm <- as.dfm(dtm)
doc_freq <- docfreq(dfm)
dfm <- dfm[, doc_freq > 2]
dfm <- dfm_tfidf(dfm, scheme_tf = "count",
                    scheme_df = "inverse",
                    base = 10)

library(quanteda.textstats)
dfm_group(dfm, groups = docs)
keyness = textstat_keyness(dfm[dfm@Dimnames$docs == "suic" |
                                 dfm@Dimnames$docs == "fel", ],
                          target =)


library(quanteda.textplots)
textplot_keyness(keyness)

ts <- textstat_frequency(dfm, groups = 'Dimnames')
head(ts)













head(de_dfm)

tf_mat <- TermDocFreq(dtm)
head(tf_mat[ order(tf_mat$term_freq, decreasing = TRUE) , ], 10)


plot(ts)

fe_dtm <- CreateDtm(
  doc_vec = fe$text,
  doc_names = fe$id,
  ngram_window = c(1, 2),
  stopword_vec = c(
    stopwords(language = "pt"),
    stopwords(source = "smart"),
    "http", "https", "é", "pra"
  )
  
)
fe_dfm <- as.dfm(fe_dtm)

su_dtm <- CreateDtm(
  doc_vec = su$text,
  doc_names = su$id,
  ngram_window = c(1, 2),
  stopword_vec = c(
    stopwords(language = "pt"),
    stopwords(source = "smart"),
    "http", "https", "é", "pra"
  )
)
su_dfm <- as.dfm(su_dtm)

de_dtm <- CreateDtm(
  doc_vec = de$text,
  doc_names = de$id,
  ngram_window = c(1, 2),
  stopword_vec = c(
    stopwords(language = "pt"),
    stopwords(source = "smart"),
    "http", "https", "é", "pra"
  )
)
de_dfm <- as.dfm(de_dtm)

de_doc_freq <- docfreq(de_dfm)
de_dfm <- de_dfm[, de_doc_freq > 2]
de_dfm <- dfm_tfidf(de_dfm, scheme_tf = "count",
                    scheme_df = "inverse",
                    base = 10)
head(de_dfm)


fe_dfm <- quanteda::as.dfm(fe_dtm)
fe_doc_freq <- docfreq(fe_dfm)
fe_dtm <- fe_dtm[, fe_dfm >= 2]
fe_df <- dfm_weight(fe_dfm, "tfidf")

# ---

fe_tf_mat <- TermDocFreq(fe_dtm)
head(fe_tf_mat[ order(fe_tf_mat$term_freq, decreasing = TRUE) , ], 10)

su_tf_mat <- TermDocFreq(su_dtm)
head(su_tf_mat[ order(su_tf_mat$term_freq, decreasing = TRUE), ], 10)
                
de_tf_mat <- TermDocFreq(de_dtm)
head(de_tf_mat[ order(de_tf_mat$term_freq, decreasing = TRUE), ], 10) 


