library(tidyverse)


#################

# histogram
ggplot(data = iris, aes(x = Sepal.Width, fill = Species)) + 
  geom_histogram(binwidth = 0.2, col = "black", size = 1) + 
  labs(x = "Sepal width", y = "Frequency", title = "Histogram of Sepal width")

# density plots
ggplot(data = iris, aes(x = Sepal.Length, fill = Species, col = Species)) + 
  geom_density(alpha = 0.5, size = 0.5) + 
  labs(x = "Sepal length", y = "Density", title = "Iris dataset: 
       Smoothed density of Sepal Length per Species")

# scatter plot
ggplot(data = iris, aes(x = Sepal.Length, y = iris$Sepal.Width)) + 
  geom_point(aes(x = iris$Sepal.Length, y = iris$Sepal.Width, fill = iris$Species, 
                 col = iris$Species, shape = iris$Species), 
             size = 2) + 
  geom_smooth(method = lm, aes(x = iris$Sepal.Length, 
                               y = iris$Sepal.Width, col = iris$Species)) +
  labs(x = "Sepal length", y = "Sepal Width", title = "Scatterplot with smoothers")

# faceting
ggplot(data = iris, aes(x = Sepal.Length, y = iris$Sepal.Width)) + 
  geom_point(aes(x = iris$Sepal.Length, y = iris$Sepal.Width, fill = iris$Species, 
                 col = iris$Species, shape = iris$Species), 
             size = 2) + 
  geom_smooth(method = lm, aes(x = iris$Sepal.Length, 
                               y = iris$Sepal.Width, col = iris$Species)) +
  facet_grid(. ~ iris$Species)


# data manipulation

iris %>% filter(Species == 'virginica') %>% summary()

iris %>% select(starts_with("Petal")) %>% summary()

iris_mutated <- iris %>% mutate(Petal.Width.Rank = percent_rank(Petal.Width))


ggplot(data = iris_mutated, aes(x = Petal.Width.Rank, y = Petal.Width)) + 
  geom_jitter(aes(x = iris_mutated$Petal.Width.Rank, y = iris_mutated$Petal.Width, 
                  col = iris_mutated$Species)) + 
  labs(x = "Petal Width Rank", y = "Petal Width", col = "Species")

