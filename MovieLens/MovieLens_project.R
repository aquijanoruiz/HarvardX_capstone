#### Loading the edx set, validation set ####

# You need to first set the working directory that contains the edx and validation sets to 
# use the following code.

edx <- readRDS("edx_dataset.rds")
validation <- readRDS("validation_dataset.rds")

# If you do not have the two datasets downloaded as a .rds file, you will need to download
# the datasets and partition them using the following chunk.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

dl <- "ml-10m.zip"

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# If you have the edx and validation sets saved as .rds files, you can load them by 
# changing the work directory to yours.

edx <- readRDS("edx_dataset.rds")
validation <- readRDS("validation_dataset.rds")

#### Packages ####
# These are the packages that will be used in this project

library(tidyverse)
library(lubridate)

#### Checking on the data for effects ####

# Movie effects
# The following is a distribution of the movies:

edx %>% group_by(movieId) %>% 
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  labs(title = "Movies by number of ratings", 
       x = "Number of raitngs", 
       y = "Number of movies")

# These are the most rated movies:

edx %>% group_by(title) %>%
  summarize(n = n(), avg_rating = round(mean(rating),2)) %>%
  arrange(desc(n)) %>% slice(1:10) 

# User effects
# The following is a distribution of the users:

edx %>% group_by(userId) %>% 
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  labs(title = "Users by number of ratings", 
       x = "Number of ratings", 
       y = "Number of users")

# This is a distribution of the average rating of each user:

edx %>% group_by(userId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 30, color = "black") + 
  labs(title = "Users by average rating", x = "Average rating", y = "Number of users")

# Genre effects
# The following are the genres with the highest rating:
edx %>% group_by(genres) %>% 
  summarize(n = n(), avg = mean(rating)) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  slice(1:20) %>%
  ggplot(aes(x = genres, y = avg)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Most popular genres", x = "", y = "Average")

# Time effects
# We first add extra date variables to our data: year, month, and day of the week.
edx <- edx %>% mutate(timestamp = as_datetime(timestamp),
                      year = year(timestamp),
                      month = month(timestamp, label = TRUE),
                      wday = wday(timestamp, label = TRUE))

validation <- validation %>%  mutate(timestamp = as_datetime(timestamp),
                                     year = year(timestamp),
                                     month = month(timestamp, label = TRUE),
                                     wday = wday(timestamp, label = TRUE))

# We can now illustrate the date variables:

# Years
edx %>% group_by(year) %>%
  summarize(avg = mean(rating)) %>%
  ggplot(aes(x = year, y = avg)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Year effects", x = "", y = "Average")

# Months
edx %>% group_by(month) %>%
  summarize(avg = mean(rating)) %>%
  ggplot(aes(x = month, y = avg)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Month effects", x = "", y = "Average")

# Days of the week
edx %>% group_by(wday) %>%
  summarize(avg = mean(rating)) %>%
  ggplot(aes(x = wday, y = avg)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Day effects", x = "", y = "Average")

#### The recommendation algorithm ####

# RMSE
# For measuring the accuracy of our model, we will us the residual mean squared error (RMSE)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Model 1: only the average

model_1_hat <- mean(edx$rating)

model_1_rmse <- RMSE(validation$rating, model_1_hat)
rmse_results <- data.frame(method = "Just the average", result= model_1_rmse)

# Model 2: Average + movie effects

mu <- mean(edx$rating)

movie_avg <- edx %>% group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

model_2_hat <- mu + validation %>% left_join(movie_avg, by = 'movieId') %>% .$b_i

model_2_rmse <- RMSE(validation$rating, model_2_hat)

rmse_results <- rbind(rmse_results, data.frame(method = "Movie Effect Model",
                                               result = model_2_rmse))

# Model 3: Average + movie effects + user effects

user_avg <-  edx %>% left_join(movie_avg, by= "movieId") %>%
  group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))

model_3_hat <- validation %>% left_join(movie_avg, by= "movieId") %>% 
  left_join(user_avg, by= "userId") %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

model_3_rmse <- RMSE(validation$rating, model_3_hat)

rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Movie Effect + User Effect Model",
                                 result = model_3_rmse))

# Model 4: Average + movie effects + user effects + genre effect

genre_avg <- edx %>% left_join(movie_avg, by= "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  group_by(genres) %>% summarize(b_genre = mean(rating - mu - b_i - b_u))

model_4_hat <- validation %>% left_join(movie_avg, by= "movieId") %>% 
  left_join(user_avg, by= "userId") %>%
  left_join(genre_avg, by = "genres") %>% 
  mutate(pred = mu + b_i + b_u + b_genre) %>% .$pred

model_4_rmse <- RMSE(validation$rating, model_4_hat)

rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Movie Effect + User Effect + Genre Effect Model",
                                 result = min(model_4_rmse)))

# Model 5: Average + movie effects + user effects + genre effect + time effect

year_avg <- edx %>% left_join(movie_avg, by= "movieId") %>%
  left_join(user_avg, by = "userId") %>% left_join(genre_avg, by = "genres") %>% 
  group_by(year) %>%
  summarize(b_year = mean(rating - mu - b_i - b_u - b_genre))

month_avg <- edx %>% left_join(movie_avg, by= "movieId") %>%
  left_join(user_avg, by = "userId") %>% left_join(genre_avg, by = "genres") %>% 
  left_join(year_avg, by = "year") %>% group_by(month) %>% 
  summarize(b_month = mean(rating - mu - b_i - b_u - b_genre - b_year))

wday_avg <- edx %>% left_join(movie_avg, by= "movieId") %>%
  left_join(user_avg, by = "userId") %>% left_join(genre_avg, by = "genres") %>% 
  left_join(year_avg, by = "year") %>% left_join(month_avg, by = "month") %>% 
  group_by(wday) %>% 
  summarize(b_wday = mean(rating - mu - b_i - b_u - b_genre - b_year - b_month))

model_5_hat <- validation %>% left_join(movie_avg, by= "movieId") %>% 
  left_join(user_avg, by= "userId") %>% left_join(genre_avg, by = "genres") %>% 
  left_join(year_avg, by = "year") %>% left_join(month_avg, by = "month") %>% 
  left_join(wday_avg, by = "wday") %>% 
  mutate(pred = mu + b_i + b_u + b_genre + b_year + b_month + b_wday) %>% .$pred

model_5_rmse <- RMSE(validation$rating, model_5_hat)

rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Movie Effect + User Effect + 
                                 Genre Effect Model + Time Effect Model",
                                 result = min(model_5_rmse)))

rmse_results

# Model 6: Average + movie effects (penalized least square)
# This code will take some minutes as each calculation is executed for each lambda.
lambda <- seq(0,10,0.25) # This is the tunning parameter

model_6_rmses <- sapply(lambda, function(l){
  
  mu <- mean(edx$rating)
  
  movie_avg_pen <- edx %>% group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  user_avg_pen <- edx %>% left_join(movie_avg_pen, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  genre_avg_pen <- edx %>% left_join(movie_avg_pen, by="movieId") %>%
    left_join(user_avg_pen, by = "userId") %>% group_by(genres) %>% 
    summarize(b_genre = sum(rating - mu - b_i - b_u)/(n()+l))
  
  year_avg_pen <- edx %>% left_join(movie_avg_pen, by= "movieId") %>%
    left_join(user_avg_pen, by = "userId") %>% 
    left_join(genre_avg_pen, by = "genres") %>% group_by(year) %>%
    summarize(b_year = sum(rating - mu - b_i - b_u - b_genre)/(n()+l))
  
  month_avg_pen <- edx %>% left_join(movie_avg_pen, by= "movieId") %>%
    left_join(user_avg_pen, by = "userId") %>% 
    left_join(genre_avg_pen, by = "genres") %>% 
    left_join(year_avg_pen, by = "year") %>% group_by(month) %>% 
    summarize(b_month = mean(rating - mu - b_i - b_u - b_genre - b_year))
  
  wday_avg_pen <- edx %>% left_join(movie_avg_pen, by= "movieId") %>%
    left_join(user_avg_pen, by = "userId") %>% left_join(genre_avg_pen, by = "genres") %>%
    left_join(year_avg_pen, by = "year") %>% left_join(month_avg_pen, by = "month") %>%
    group_by(wday) %>% 
    summarize(b_wday = mean(rating - mu - b_i - b_u - b_genre - b_year - b_month))
  
  predicted_rating <- validation %>% left_join(movie_avg_pen, by= "movieId") %>%
    left_join(user_avg_pen, by= "userId") %>% left_join(genre_avg_pen, by = "genres") %>%
    left_join(year_avg_pen, by = "year") %>% left_join(month_avg_pen, by = "month") %>% 
    left_join(wday_avg_pen, by = "wday") %>% 
    mutate(pred = mu + b_i + b_u + b_genre - b_year + b_month + b_wday) %>% .$pred
  
  return(RMSE(predicted_rating, validation$rating))
})

qplot(lambda, model_6_rmses) # We plot each lambda and its respective RMSE

lambda[which.min(model_6_rmses)] # We choose the optimal lambda

rmse_results <- rbind(rmse_results, 
                      data.frame(method = "Penalized Movie Effect + User Effect Model + 
                                 Genre Effect + Time Effect",
                                 result = min(model_6_rmses)))

rmse_results # These are all the applied models and their respective RMSEs