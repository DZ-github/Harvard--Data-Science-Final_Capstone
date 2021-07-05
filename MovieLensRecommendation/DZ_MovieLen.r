#############################################################
# Create edx set, validation set
#############################################################
# Note: this process could take a couple of minutes
# use readRDS to open saved files previously processed

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
setwd("C:\\Users\\user\\Desktop\\Harvard_R\\CapStone\\MovieLensRecommendation")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#dl <- tempfile()
#download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                      col.names = c("userId", "movieId", "rating", "timestamp"))


# Save ratings object at my laptop file size around 46MB
#saveRDS(ratings, "ratings.rda")

#movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
#colnames(movies) <- c("movieId", "title", "genres")
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))

#movielens <- left_join(ratings, movies, by = "movieId")

#saveRDS(movies, "movies.rda")
#saveRDS(movielens, "movielens.rda")

# Validation set will be 10% of MovieLens data
set.seed(1)
movielens_model<-readRDS("movielens.rda")
test_index <- createDataPartition(y = movielens_model$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens_model[-test_index,]
temp <- movielens_model[test_index,]
dim(edx)  #9000047
dim(temp) #1000007

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
dim(validation) 

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
dim(edx)  #9000061

saveRDS(edx, "edx.rda")
saveRDS(validation, "validation.rda")

#rm(dl, ratings, movies, test_index, temp, movielens, removed)
rm(temp, removed, test_index)
gc()

#####################################################################
# MovieLens Reccomender System Project

# --- Descriptive Analysis ----

# Some movies have more rating than others
edx_dat <- readRDS("edx.rda")
validation_dat <-readRDS("validation.rda")

# distinct movieIDs, 10677
n_distinct(edx_dat$movieId)

edx_dat %>% 
   dplyr::count(movieId) %>% 
   ggplot(aes(n)) + 
   geom_histogram(bins = 30, color = "black") + 
   scale_x_log10() + 
   ggtitle("Movies")

# Some users rate more often than others
edx_dat %>%
   dplyr::count(userId) %>% 
   ggplot(aes(n)) + 
   geom_histogram(bins = 30, color = "black") + 
   scale_x_log10() +
   ggtitle("Users")

# Number of users and movies in edx set
edx_dat %>%
   summarize(n_users = n_distinct(userId),
             n_movies = n_distinct(movieId))

#  Top 5 movies ratings given by top 5 frequent users
top5 <- edx_dat %>%
   dplyr::count(movieId) %>%
   top_n(5) %>%
   pull(movieId)

# Top 5 frequent users
top5_u <- edx_dat %>%
   dplyr::count(userId) %>%
   top_n(5) %>%
   pull(userId)
top5_u

tab <- edx_dat %>% 
   filter(movieId %in% top5) %>% 
   filter(userId %in% top5_u) %>%
   select(userId, title, rating) %>% 
   spread(title, rating) 
print(tab)

# Ratings distribution
edx_dat %>%
   ggplot(aes(rating)) +
   geom_histogram(binwidth = 0.1)+
   labs(title="Histogram of Rating")


# View of all unique genres
unique_genres_list <- str_extract_all(unique(edx_dat$genres), "[^|]+") %>%
   unlist() %>%unique()
unique_genres_list


# --- Analysis part ---
options(digits = 4)
# The RMSE function that will be used in this project is:
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

# --- 1. Native Mean Avg Model ---
# Calculate the average of all movies
mu_hat <- mean(edx_dat$rating)
mu_hat  #3.512

# Predict the RMSE on the validation set
rmse_mean <- RMSE(validation_dat$rating, mu_hat) #1.06065

# Creating a results dataframe
results <- data.frame(model="Naive Mean Avg Model", RMSE=rmse_mean)
print.data.frame(results)

# --- 2. Movie Avg Model ---
# Calculate the average by movie
movie_avgs <- edx_dat %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu_hat))

# Compute the predicted ratings on validation dataset
rmse_movie_model <- validation_dat %>%
   left_join(movie_avgs, by='movieId') %>%
   mutate(pred = mu_hat + b_i) %>%
   pull(pred)

rmse_movie_model_result <- RMSE(validation_dat$rating, rmse_movie_model)

# Adding row to the results
results <- results %>% add_row(model="Movie-Based Model", RMSE=rmse_movie_model_result)
print.data.frame(results)
# Free up mem
rm(movie_avgs, rmse_movie_model, rmse_movie_model_result)
gc()

# --- 3. Movie User Avg Model ---
# Calculate the average by user
user_avgs <- edx_dat %>%
   left_join(movie_avgs, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu_hat - b_i))

# Compute the predicted ratings on validation dataset
rmse_movie_user_model <- validation_dat %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   mutate(pred = mu_hat + b_i + b_u) %>%
   pull(pred)

rmse_movie_user_model_result <- RMSE(validation_dat$rating, rmse_movie_user_model)

# Adding row to the results
results <- results %>% add_row(model="Movie+User Based Model", RMSE=rmse_movie_user_model_result)
print.data.frame(results)

# Free up MEM
rm(user_avgs, rmse_movie_user_model, rmse_movie_user_model_result)
gc()

# --- 4 Movie User Genre Avg Model ---
# calculate genre bias
genre_avgs <- edx_dat %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   group_by(genres) %>%
   summarize(b_u_g = mean(rating - mu_hat - b_i - b_u))


# Compute the predicted ratings on validation dataset
rmse_movie_user_genre_model <- validation_dat %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   left_join(genre_avgs, by='genres') %>%
   mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
   pull(pred)

rmse_movie_user_genre_model_result <- RMSE(validation_dat$rating,rmse_movie_user_genre_model)

# Adding row to the results
results <- results %>% add_row(model="Movie+User+Genre Based Model", RMSE=rmse_movie_user_genre_model_result)
print.data.frame(results)

# Free up mem
rm(genre_avgs, rmse_movie_user_genre_model, rmse_movie_user_genre_model_result)
gc()

# --- 5 Regularized Movie Model  ---
set.seed(1)
lambdas <- seq(0, 10, 0.1)

# Compute the predicted ratings on validation dataset using different lambda
rmses <- sapply(lambdas, function(lambda) {
   
  # Calculate the average by user
   b_i <- edx_dat %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
   
   # Compute the predicted ratings on validation dataset
   predicted_ratings <- validation_dat %>%
      left_join(b_i, by='movieId') %>%
      mutate(pred = mu_hat + b_i) %>%
      pull(pred)
   
   # Predict the RMSE on the validation set
      return(RMSE(validation_dat$rating, predicted_ratings))
})

# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

# RMSE
rmse_regularized_movie_model <- min(rmses)

# Adding row to the results
results <- results %>% add_row(model="Regularized Movie-Based Model", RMSE=rmse_regularized_movie_model)
print.data.frame(results)
# Free up MEM
rm(rmse_regularized_movie_model)
gc()

# --- 6 Regularized Movie + User Model ---
set.seed(1)

rmses <- sapply(lambdas, function(lambda) {

   # Calculate the average by movie
   b_i <- edx_dat %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
   
   # Calculate the average by user
   b_u <- edx_dat %>%
      left_join(b_i, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
   
   # Compute the predicted ratings on validation dataset
   predicted_ratings <- validation_dat %>%
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      mutate(pred = mu_hat + b_i + b_u) %>%
      pull(pred)
   
   # Predict the RMSE on the validation set
   return(RMSE(validation_dat$rating, predicted_ratings))
})

# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

# Predict the RMSE on the validation set
rmse_regularized_movie_user_model <- min(rmses)

# Adding the results to the results dataset
results <- results %>% add_row(model="Regularized Movie+User Based Model", RMSE=rmse_regularized_movie_user_model)
print.data.frame(results)

saveRDS(results, "capstone_results.rda")


# # --- 7 Regularized Movie User Genres Model ---
# set.seed(1)
# lambdas <- seq(0, 10, 0.1)
# 
# # Compute the predicted ratings on validation dataset using different values of lambda
# rmses <- sapply(lambdas, function(lambda) {
# 
#    # Calculate the average by movie
#    b_i <- edx_dat %>%
#       group_by(movieId) %>%
#       summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
#    
#    # Calculate the average by user
#    b_u <- edx_dat %>%
#       left_join(b_i, by='movieId') %>%
#       group_by(userId) %>%
#       summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
#    # Calculate genre bias
#     b_g <- edx_dat %>%
#       left_join(b_i, by='movieId') %>%
#       left_join(b_u, by='userId') %>%
#       group_by(genres) %>%
#       summarize(b_g = sum(rating - b_i - mu_hat - b_u) / (n() + lambda))
#    
#    # Compute the predicted ratings on validation dataset
#    predicted_ratings <- validation_dat s%>%
#       left_join(b_i, by='movieId') %>%
#       left_join(b_u, by='userId') %>%
#       left_join(b_g, by='genres') %>%
#       mutate(pred = mu_hat + b_i + b_u + b_g) %>%
#       pull(pred)
#    
#    # Predict the RMSE on the validation set
#    return(RMSE(validation_dat$rating, predicted_ratings))
# })
# 
# # Get the lambda value that minimize the RMSE
# min_lambda <- lambdas[which.min(rmses)]
# 
# # Predict the RMSE on the validation set
# rmse_regularized_movie_user_genre_model <- min(rmses)
# 
# # Adding the results to the results dataset
# results <- results %>% add_row(model="Regularized Movie+User+Genre Based Model", RMSE=rmse_regularized_movie_user_genre_model)
# print.data.frame(results)
