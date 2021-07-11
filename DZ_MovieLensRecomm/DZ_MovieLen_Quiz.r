#############################################################
# Create edx set, validation set, and submission file
# Note: run previously provided code by edx at course before run this Quiz
#############################################################

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

# set working dir
setwd("C:\\Users\\user\\Desktop\\Harvard_R\\CapStone\\MovieLensREcommendation")

#######################################################
# Quiz
#######################################################

# Q1
# How many rows and columns are there in the edx dataset?

dim(edx)


# Q2
# How many zeros were given as ratings in the edx dataset?

edx %>% filter(rating == 0) %>% nrow()

# How many threes were given as ratings in the edx dataset?

edx %>% filter(rating == 3) %>% count()


# Q3
# How many different movies are in the edx dataset?

n_distinct(edx$movieId)


# Q4
# How many different users are in the edx dataset?

n_distinct(edx$userId)


# Q5
# How many movie ratings are in each of the following genres in the edx dataset?
# method 1 using str_detect

edx_dat<-readRDS("edx.rda")
dim(edx_dat)

l<-edx_dat$genre
sum(str_detect(l, "Romance"))
sum(str_detect(l, "Thriller"))
sum(str_detect(l, "Comedy"))
sum(str_detect(l, "Drama"))

# or str_count()
sum(str_count(l, "\\bRomance\\b"))
sum(str_count(l, "\\bThriller\\b"))
sum(str_count(l, "\\bComedy\\b"))
sum(str_count(l, "\\bDrama\\b"))

# or grep
length(grep("Romance",l))
length(grep("Thriller",l))
length(grep("Comedy",l))
length(grep("Drama",l))

# method by separate_rows runs very very slow at my laptop
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


# Q6
# Which movie has the greatest number of ratings?

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


# Q7
# What are the five most given ratings in order from most to least?

edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))  


# Q8
# True or False: In general, half star ratings are less common than 
# whole star ratings (e.g., there are fewer ratings of 3.5 than there 
# are ratings of 3 or 4, etc.).

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

