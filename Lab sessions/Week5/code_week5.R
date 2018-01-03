library(kernlab)

# Load other libraries
library(data.table)
library(ggplot2)
library(text2vec)
library(stringr)
library(tokenizers)

set.seed(150)

# loading data
amazon_sentences <- fread("sentiment labelled sentences/amazon_cells_labelled.txt", 
                          header = FALSE, sep = "\t")

colnames(amazon_sentences) <- c("text", "sentiment")
amazon_sentences$id <- rownames(amazon_sentences)

# dtm
prep_fun <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, '[:digit:]', ' ') # replacing digits with space
  x <- str_replace_all(x, '\\b\\w{1,2}\\b',' ') # replacing one and two letter words with space
  x <- str_replace_all(x, '[:punct:]', ' ') # removing punctuation
  x <- str_replace_all(x, '\\s+', ' ') # replacing whitespace with space
}
tok_fun <- function(x){tokenize_words(x, stopwords = stopwords())} # removing stopwords

# 
it_train <- itoken(amazon_sentences$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun, 
                   ids = amazon_sentences$id, 
                   progressbar = TRUE)
vocab <- create_vocabulary(it_train)
pruned_vocab <- prune_vocabulary(vocab, 
                                 term_count_min = 3, 
                                 doc_proportion_max = 0.5,
                                 doc_proportion_min = 0.001)
vectorizer <- vocab_vectorizer(pruned_vocab)

t1 <- Sys.time()
dtm_train <- create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

tfidf <- TfIdf$new()
dtm_tfidf <- fit_transform(dtm_train, tfidf)

# training/testing data
dtm_tfidf <- as.matrix(dtm_tfidf)

data <- as.data.frame(cbind(as.factor(amazon_sentences$sentiment), dtm_tfidf))
names(data)[1] <- "sentiment"
rnd <- runif(1000)
data$sentiment <- as.factor(data$sentiment)

d_train <- data[rnd < 0.8, ]
d_test <- data[rnd >= 0.8, ]


# SVM
model <- ksvm(sentiment ~ ., data = d_train, kernel = "vanilladot", kpar = "automatic",
              C = 10, cross = 10)

# Q1
model@nSV

# predict
pred <- predict(model, d_test[, - 1])

# accuracy
accu <- sum(pred == d_test$sentiment) / dim(d_test)[1] * 100
print(paste("The accuracy is ", accu, "%"))

# costs
mis_cost <- 10 ^ seq(- 2, 2, by = 0.1)

pred_accu <- function(miscost){
  mod <- ksvm(sentiment ~ ., data = d_train, 
              kernel = "vanilladot", C = miscost, cross = 10)
  pred1 <- predict(mod, d_test[, - 1])
  return(sum(pred1 == d_test$sentiment) / dim(d_test)[1] * 100)
}

VDP <- sapply(mis_cost, pred_accu)
 







