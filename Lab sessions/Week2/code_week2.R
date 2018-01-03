library(tm)
library(slam)
getSources()


# Loading and cleaning data
job_adverts <- VCorpus(DirSource("vladi"))
job_adverts[[1]]$content

job_adverts <- tm_map(job_adverts, content_transformer(tolower)) 
job_adverts <- tm_map(job_adverts, stripWhitespace)
job_adverts[[1]]$content

dtm <- DocumentTermMatrix(job_adverts, 
                          control = list(
                            removePunctuation=TRUE,
                            removeNumbers=TRUE
                          )
)

# some exploring
dim(dtm)
summary(col_sums(dtm))
sort(col_sums(dtm), decreasing = TRUE)[1:15] # pick how many most frequent
findFreqTerms(dtm, lowfreq = 50) # different frequencies
findAssocs(dtm, terms = "verbal", corlimit = 0.3) # different words and different correlation limit

# removing redundant words
dtm_new <- dtm[, !(Terms(dtm) %in% findFreqTerms(dtm, lowfreq = 100))] # content-specific "stopwords"
dtm_new <- dtm_new[row_sums(dtm_new) > 0,]
findFreqTerms(dtm_new, lowfreq = 60)

# tfidf
term_tfidf <- tapply(dtm_new$v / row_sums(dtm_new)[dtm_new$i], dtm_new$j, mean) * 
  # first term is normalized term frequency, then this frequency is averaged across columns
  log2(nDocs(dtm_new) / col_sums(dtm_new > 0)) 
  # nDocs gives us the number of documents where the term appears and col_sums gives us the times the term appears

summary(term_tfidf)

dtm_new <- dtm_new[, term_tfidf >= 0.042] # getting rid of terms with low tfidf
dtm_new <- dtm_new[row_sums(dtm_new) > 0,] # we get rid off empty rows
summary(col_sums(dtm_new))

#some exploring
dim(dtm_new)
sort(col_sums(dtm_new), decreasing = TRUE)[1:15] # pick how many most frequent
findFreqTerms(dtm_new, lowfreq = 50) # different frequencies
findAssocs(dtm, terms = "store", corlimit = 0.6) # different words and different correlation limit

# visualization
library(wordcloud)
freq <- data.frame(freqterms = sort(colSums(as.matrix(dtm_new)), decreasing=TRUE))
wordcloud(rownames(freq), freq[, 1], max.words = 50, colors = brewer.pal(3, "Dark2"))







