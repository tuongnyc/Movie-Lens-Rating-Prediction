# Tuong Nguyen
# Harvard Professional Certifcation Capstone Project
# Movie Lens Rating Prediction
# 2/22/2019

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

## plot the ratings into histogram.
movielens %>% group_by(rating) %>% qplot(rating, geom="histogram", bins = 5, data = ., color ="black", xlab="Rating", ylab="Count")

## plot the number of movies rated
movielens %>% count(movieId) %>% ggplot(aes(n)) + geom_histogram(bins=50, color = "black") +
  scale_x_log10() + ggtitle("Movies")

## plot the number of users that rated the movie
movielens %>% count(userId) %>% ggplot(aes(n)) + geom_histogram(bins = 50, color = "black") +
  scale_x_log10() + ggtitle("Users")

set.seed(1)
## Create 2 partition, training and test set.
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

# calculate the average of all ratings.
mu <- mean(edx$rating)
# calculate the movie average.
movie_avgs <- edx %>% group_by(movieId) %>% summarize(bi = mean(rating - mu))
# plot the histogram of movie average.
movie_avgs %>% ggplot(aes(bi)) + geom_histogram(bins = 10, color="black")
# calcuate the ratings for each users.
user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>%
  summarize(bu = mean(rating - mu - bi))

# Calculate the predictions.
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>% mutate(pred = mu + bi + bu)
# Print the first 5 movies rating predictions.
head(predicted_ratings, n = 5) %>% select(title, bi, bu, pred) %>% 
  knitr::kable(caption = "Predicted Rating Results sample")

# Calculate the RMSE of prediction using both movie and fuser effect
rmsewith2var <- RMSE(predicted_ratings$pred, validation$rating)
print(rmsewith2var)

rmse_results <- data_frame(method='User and Movie Effect', RMSE=rmsewith2var)
predwouser_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>%
  group_by(movieId) %>% mutate(pred = mu + bi)
#Calculate the RMSE with only movie effect.
rmsewithmovieeffect <- RMSE(predwouser_ratings$pred, validation$rating)
rmse_results<-bind_rows(rmse_results,data_frame(method='Movie Effect', RMSE=rmsewithmovieeffect))

## get the averages of all ratings for genre
genres_avgs <- edx %>% left_join(user_avgs, by='userId') %>% 
  left_join(movie_avgs, by='movieId') %>% group_by(genres) %>%
  summarize(gu = mean(rating - mu - bi - bu))

## predict the ratings
predicted_ratings_genres <- validation %>% left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>% mutate(pred = mu + bi + bu + gu)

## obtain the RMSE against validation set
predicted_genresRMSE <- RMSE(predicted_ratings_genres$pred, validation$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method='Movie, User, and Genres Effect', RMSE=predicted_genresRMSE))
rmse_results %>% knitr::kable(caption = 'Summary of RMSEs')

