library(keras)
library(glmnet)

# downloading data and getting the train/test division
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y


# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255
# transform y to categorical
y_train_logit <- as.factor(y_train)
y_test_logit <- as.factor(y_test)
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)


# Exercise 1 - logit
# we test only on a subset, otherwise it is quite slow
logit <- cv.glmnet(x_train[1:1000, ], y_train_logit[1:1000], family = "multinomial")
pred_logit <- predict(logit, x_test[1:1000, ], type = "class")
mean(pred_logit == y_test_logit[1:1000])

# we do it only on a small sample and we do not really try to improve it, since it is
# not the goal of the assignment

# Neural networks
model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')  

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

summary(model)

# Exercise 2 
# Dropout layers are used a regularization method, analogous to lambda in lasso 
# or ridge regression. The idea of dropping certain percentage of neurons leads 
# to lower risk of overfitting. The dropped neurons are chosen at random and 
# this leads to neurons being less prone to "specialize" and thus being less prone
# to overfitting.
# Source: https://machinelearningmastery.com/dropout-regularization-deep-learning-models-keras/

# Exercise 3
# The softmax function should be used nearly exclusively in the output layer in 
# multiclass classification problems. ReLU on the other hand is used in the hidden
# layers due to several of its properties - it is fastest to compute and thus the 
# training of the net is the fastest, it and its derivation is monotonic and it 
# oes not suppress strong signals, i.e. its range is from zero to infinity.
# It can be improved as there is a Leaky ReLU that has range from minus to plus infinity.
# This also means that not as many neurons die, i.e. if the activation is zero in neuron
# then it can be ignored during the training and "die". Leaky ReLU solves this.


history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)
plot(history)

model %>% predict_classes(x_test) %>% head()
model %>% evaluate(x_test, y_test)

# Exercise 4
# In case we do not have a classification task. There are alternative measures for
# classification such as ROC (if binary), precision, recall or F-measure. There are
# several reasons to use alternatives. If we put more weight e.g. on false positives
# than false negatives or if we want more than yes/no label, i.e. ROC curve or
# precision/recall curve. In the end one must choose the measure that is good
# for the individual task at hand, there is no universal best solution.

# Exercise 5
# We do not cover the mathematical formulation of cross-entropy here as it is
# on wikipedia and in the documenation of every ML-related package. We will 
# talk about the intuition behind. We base our description on Michael Nielsen's book.
# Cross-entropy can be described as a measure of "surprise". 
# Our neuron is trying to compute the function x›y = y(x). 
# But instead it computes the function x›a = a(x). Now if we were to assume
# a is the probability our neuron estimated that y = 1 and 1 - a that y is 0.
# The cross-entropy then means the strength of our "surprise" when we learn the
# true value of y.

# Exercise 6
# Epoch is one iteration of the SGD algorothm, i.e. your algorithm sees every data
# point exactly once.

# Exercise 7
# Batch size determines at how many data points our algorithm looks before it updates
# our weights. Meaning every epoch consists of several batches. The smallest/largest
# theoretical number of batches is 1, i.e. all data points at once, or N, i.e. every data point
# alone, which is the so-called online SGD.

# Exercise 8
# The validation split is the part where the algorithm is fine-tuned, i.e. the
# hyperparamaters are tinkered with and their optimal values are found. This set is 
# used to choose the optimal model and possibly detect over-fitting on the training
# data.

# Exercise 9

# logit in keras
model1 <- keras_model_sequential() 

model1 %>% 
  layer_dense(units = 10, activation = 'sigmoid', input_shape = c(784)) %>% 
  
  summary(model1)

model1 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history1 <- model1 %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)
plot(history1)

model1 %>% predict_classes(x_test) %>% head()
model1 %>% evaluate(x_test, y_test)










