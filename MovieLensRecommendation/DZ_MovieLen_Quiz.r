#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

#if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
#if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

# set working dir
setwd("C:\\Users\\user\\Desktop\\Harvard_R\\CapStone\\MovieLensREcommendation")

# Save ratings object at my laptop for easier re-run
# rda file size around 46MB
saveRDS(ratings, "ratings.rda")

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# save movies to rda file at my laptop, file size 188K
saveRDS(movies, "movies.rda")

# I use R 4.0 later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# save movieles to rda file at my laptop, size around 150M
#saveRDS(movies, "movies.rda")
#saveRDS(movielens, "movielens.rda")

# Release memory:
gc()
memory.size (max=FALSE)

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
saveRDS(validation, "validation.rda")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
saveRDS(edx, "edx.rda")

#rm(dl, ratings, movies, test_index, temp, movielens, removed)


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


# Adding the results to the results dataset

results <- results %>% add_row(model="Regularized Movie+User+Genre Based Model", RMSE=rmse_regularized_movie_user_genre_model)