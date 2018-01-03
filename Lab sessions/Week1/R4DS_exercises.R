library(tidyverse)
library(nycflights13)

####### Hadley R4DS #######

### Chapter 7 ###

# 7.3.4

# 1
ggplot(data = diamonds) + 
  geom_density(mapping = aes(x = x), alpha = 1, fill = "black") + 
  geom_density(mapping = aes(x = y), alpha = 0.5, fill = "yellow") + xlim(0, 20) +
  geom_density(mapping = aes(x = z), alpha = 0.5, fill = "red")

diff_check <- diamonds %>% 
                mutate(diff = x - y) %>%
                select(diff)
  
ggplot(data = diff_check, aes(x = diff)) + 
  geom_density(alpha = 0.5, fill = "black")

# 2 
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 1000, fill = "blue") # try also binwidth 100/150/200/250/500

# 3
diamonds %>%
  filter(carat == 1) %>%
  count()

diamonds %>%
  filter(carat == 0.99) %>%
  count()

# 4
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 250, fill = "blue") +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 5000)) # does not remove observations outside the boundary

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 250, fill = "blue") +
  xlim(0, 10000) + ylim(0, 5000) # removes observations outside the boundary


# 7.4.1

# 1
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.1, fill = "blue")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = y), fill = "red") + coord_cartesian(xlim = c(0.01, 10))
ggplot(data = diamonds2) + 
  geom_bar(mapping = aes(x = y), fill = "red")

# both tell me that there are missing values plus in bar chart the missing values are visible as
# gaps in data, in histogram it is not so as the data are clustered together

# 2
mean(diamonds2$y, na.rm = T) 
sum(diamonds2$y, na.rm = F) # if T then result, else NA

# 7.5.1.1

# 1
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(x = sched_dep_time, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

#2
ggplot(data = diamonds, mapping = aes(x = reorder(color, price, FUN = median), y = price)) +
  geom_boxplot()
ggplot(data = diamonds, mapping = aes(x = reorder(clarity, price, FUN = median), y = price)) +
  geom_boxplot()

cor(diamonds$price, diamonds$depth)
cor(diamonds$price, diamonds$table)
cor(diamonds$price, diamonds$x)
cor(diamonds$price, diamonds$y)
cor(diamonds$price, diamonds$z)

ggplot(data = diamonds, aes(x = reorder(cut, x, FUN = median), y = x)) +
  geom_boxplot()

# this means that the largest diamonds are with the worst cut thus are the most expensive on average

# 3
library(ggstance)

ggplot(data = diamonds, aes(x = reorder(cut, x, FUN = median), y = x)) +
  geom_boxplot() + coord_flip()
ggplot(data = diamonds) +
  geom_boxploth(aes(y = cut, x = x))

# differs in where the variables are supposed to be written

# 4
library(lvplot)

ggplot(data = diamonds, mapping = aes(x = reorder(cut, price, FUN = median), y = price)) +
  geom_lv()

# 5

ggplot(data = diamonds, mapping = aes(x = reorder(cut, price, FUN = median), y = price)) +
  geom_violin()
ggplot(data = diamonds) +
  geom_histogram(aes(x = price, y = ..density..), binwidth = 500) + facet_grid(. ~ cut)
ggplot(data = diamonds, aes(x = price, y = ..density..)) +
  geom_freqpoly(aes(color = cut), binwidth = 500)

# 6
library(ggbeeswarm)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_jitter()

# geom_beeswarm and geom_quasirandom give some sort of structure to the data for easier orientation

# 7.5.2.

# 1
diamonds %>% 
  count(color, cut) %>%
  ungroup %>%
  mutate(freq = cume_dist(n)) %>%
  ggplot(aes(x = color, y = cut)) + 
    geom_tile(aes(fill = freq))

# 2
flights %>% 
  group_by(dest, month) %>%
  ggplot(aes(x = dest, y = month)) + 
    geom_tile(aes(fill = arr_delay))

# 3
diamonds %>% 
  count(color, cut) %>%
  ggplot(aes(x = color, y = cut)) + 
    geom_tile(aes(fill = n))
diamonds %>% 
  count(color, cut) %>%
  ggplot(aes(x = cut, y = color)) + 
    geom_tile(aes(fill = n))

# 7.5.3.

# 1
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(aes(color = cut_width(carat, 0.2)), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(aes(color = cut_number(carat, 20)), binwidth = 500)

# 2
ggplot(data = diamonds, mapping = aes(x = carat)) + 
  geom_freqpoly(aes(color = cut_width(price, 500)), binwidth = 0.2)

# 4
ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = carat, y = price)) + facet_grid(. ~ cut)








