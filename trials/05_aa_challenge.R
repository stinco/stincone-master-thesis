#####
# 5. AA Challenge
# Leonardo Stincone
# 13/12/2020
# R 4.0.2
#####

# Libraries ####

library(tidyverse)
library(knitr)
library(kableExtra)
library(lubridate)
library(scales)
library(ggeasy)
library(gridExtra)

theme_set(theme_bw())


# Variables effect trials ####

train <- read_csv("C:/Users/leona/Documents/Genertel - pc/documenti/AA Challenge/train_set/train_set.csv")


train %>% 
  count(target) %>% 
  mutate(freq = n/sum(n))

train %>% 
  summarize_all(class) %>% 
  pivot_longer(cols = everything()) %>% 
  filter(value != "numeric")

train %>% 
  count(feature_6) %>% 
  # arrange(feature_6)
  arrange(-n)

train %>% 
  count(feature_13) %>% 
  arrange(-n) %>% 
  ggplot(aes(x = feature_13, y = n)) +
  geom_col()

train %>% 
  count(feature_14) %>% 
  arrange(-n)

train %>% 
  count(feature_36) %>% 
  arrange(-n)

train %>% 
  count(feature_37) %>% 
  arrange(-n)

train %>% 
  count(feature_38) %>% 
  arrange(-n)

train %>% 
  count(feature_39) %>% 
  arrange(-n)


train %>% 
  group_by(feature_13) %>% 
  count(target) %>% 
  mutate(n_tot = sum(n),
         freq = n/sum(n)) %>% 
  filter(target == 1) %>% 
  ggplot(aes(x = feature_13, y = freq)) +
  geom_point()




