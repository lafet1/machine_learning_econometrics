library(kernlab)

# Load other libraries
library(data.table)
library(ggplot2)
library(text2vec)
library(stringr)
library(tokenizers)
library(mlr)
library(tidyverse)

set.seed(150)

# loading data
amazon_sentences <- fread("sentiment labelled sentences/amazon_cells_labelled.txt", 
                          header = FALSE, sep = "\t")
yelp_sentences <- fread("sentiment labelled sentences/yelp_labelled.txt", 
                          header = FALSE, sep = "\t")
imdb_sentences <- fread("sentiment labelled sentences/imdb_labelled.txt", 
                          header = FALSE, sep = "\t")

colnames(amazon_sentences) <- c("text", "sentiment")
colnames(yelp_sentences) <- c("text", "sentiment")
colnames(imdb_sentences) <- c("text", "sentiment")

d <- rbind(amazon_sentences, imdb_sentences, yelp_sentences)
d$id <- rownames(d)



# dtm
prep_fun <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, '[:digit:]', ' ') # replacing digits with space
  x <- str_replace_all(x, '\\b\\w{1,2}\\b',' ') # replacing one and two letter words with space
  x <- str_replace_all(x, '[:punct:]', ' ') # removing punctuation
  x <- str_replace_all(x, '\\s+', ' ') # replacing whitespace with space
}
tok_fun <- function(x){tokenize_words(x, stopwords = stopwords())} # removing stopwords


it_train <- itoken(d$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun, 
                   ids = d$id, 
                   progressbar = TRUE)
vocab <- create_vocabulary(it_train)
pruned_vocab <- prune_vocabulary(vocab, 
                                 term_count_min = 3, 
                                 doc_proportion_max = 0.5,
                                 doc_proportion_min = 0.0001)
vectorizer <- vocab_vectorizer(pruned_vocab)

t1 <- Sys.time()
dtm_train <- create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

tfidf <- TfIdf$new()
dtm_tfidf <- fit_transform(dtm_train, tfidf)


# training/testing data
dtm_tfidf <- as.matrix(dtm_tfidf)

data <- as.data.frame(cbind(d$sentiment, dtm_tfidf))
names(data)[1] <- "sentiment"
names(data) <- make.names(names(data))
rnd <- runif(3000)
data$sentiment <- as.factor(data$sentiment)

d_train <- data[rnd < 0.8, ]
d_test <- data[rnd >= 0.8, ]

keep <- names(which(colSums(d_train[, -1]) != 0))
d_train <- d_train[, c("sentiment", keep)]
d_test <- d_test[, c("sentiment", keep)]


# tuning by MLR
classif.train <- makeClassifTask(data = d_train, target = "sentiment")
classif.test <- makeClassifTask(data = d_test, target = "sentiment")

ps <- makeParamSet(
  makeNumericParam("C", lower = -10, upper = 6, trafo = function(x) 2^x),
  makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
  makeNumericParam("sigma", lower = -10, upper = 6, trafo = function(x) 2^x,
                   requires = quote(kernel == "rbfdot")),
  makeIntegerParam("degree", lower = 2L, upper = 5L,
                   requires = quote(kernel == "polydot"))
)
ps1 <- makeParamSet(
  makeNumericParam("C", lower = -5, upper = 2, trafo = function(x) 2^x),
  makeDiscreteParam("kernel", values = c("vanilladot"))
)

ctrl <- makeTuneControlGrid()
rdesc <- cv10

t2 <- Sys.time()
res <- tuneParams("classif.ksvm", task = classif.train, resampling = rdesc, 
                 par.set = ps1, control = ctrl, measures = list(acc))
print(difftime(Sys.time(), t2, units = 'sec'))

res$x
res$y

lrn <- setHyperPars(makeLearner("classif.ksvm"), par.vals = res$x)
model <- train(lrn, classif.train)
pred <- predict(model, task = classif.test)
mean(pred$data$truth == pred$data$response)

# optimal misclass. cost
kernels <- list(rbfdot(sigma = 0.05),
                    polydot(scale = 1, offset = 1, degree = 2),
                    vanilladot())

pred_accu <- function(miscost, type) {
  mod <- ksvm(sentiment ~ .,
              data = d_train,
              kernel = type,
              C = miscost,
              cross = 10)
  
  acc <- sum(d_test$sentiment == 
               predict(mod, d_test %>% select(-sentiment))) / dim(d_test)[1] * 100
  print(miscost)
  return(acc)
  
}

mis_cost <- 10 ^ seq(-2, 2, by = 0.1)
costs <- mis_cost

result <- cbind(costs, sapply(mis_cost, pred_accu, type = kernels[[3]])) # linear
result1 <- cbind(costs, sapply(mis_cost, pred_accu, type = kernels[[1]])) # rbf
result2 <- cbind(costs, sapply(mis_cost, pred_accu, type = kernels[[2]])) #poly


results <- as.data.frame(cbind(result, result1[, 2], result2[, 2]))
colnames(results) <- c('Cost', 'Linear', 'Radial basis', 'Polynomial')

result_ggplot <- melt(results, id.vars = 'Cost')

ggplot(result_ggplot) +
  geom_line(aes(x = Cost, y = value, col = variable), size = 1) + scale_x_log10()


write.csv(results, "results.csv", row.names = F)

read.csv("results.csv")

