## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width=6, fig.height=4.5)


## ----basic options--------------------------------------------------------------------------
### Basic options: clear list, show 4 decimal places
rm(list=ls())
options(digits=4)


## ----installing requisite packages, message=FALSE, warning=TRUE-----------------------------
### Installing requisite packages
if(!require(tidyverse)) install.packages("tidyverse", 
    repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
    repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
    repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
    repos = "http://cran.us.r-project.org")


## ----loading requisite libraries------------------------------------------------------------
### Loading required libraries
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)


## ----dataset download-----------------------------------------------------------------------
### Downloading the 10M dataset
dl <- tempfile()
file <- "http://files.grouplens.org/datasets/movielens/ml-10m.zip"
download.file(file, dl)


## ----offline processing, eval=FALSE, include=FALSE------------------------------------------
## # MovieLens 10M dataset:
## # https://grouplens.org/datasets/movielens/10m/
## # http://files.grouplens.org/datasets/movielens/ml-10m.zip
## 
## #dl <- "ml-10m.zip" # Downloaded file for offline processing


## ----Data cleaning - unzip------------------------------------------------------------------
### Unzipping the dataset and construction of a composite file
ratings <- fread(text = gsub("::", "\t", 
    readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
    col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, 
    "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")


## ----options, eval=FALSE, include=FALSE-----------------------------------------------------
## # if using R 3.6 or earlier:
## #movies1 <- as.data.frame(movies) %>%
## #	mutate(movieId = as.numeric(levels(movieId))[movieId],
## #	title = as.character(title),
## #	genres = as.character(genres))
## # if using R 4.0 or later:


## ----composite------------------------------------------------------------------------------
# Construction of a composite dataset
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(movieId),
  title = as.character(title),
  genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


## ----edx and validation creation, message=FALSE, warning=FALSE------------------------------
### Construction of edx and validation datasets
# Validation dataset will be 10% of MovieLens data
set.seed(1) # if using R 4.0 or later, 
# use `set.seed(1, sample.kind="Rounding")`
test_index <- createDataPartition(y = movielens$rating, 
    times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in the validation dataset are 
# also in the edx dataset
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation dataset back into edx dataset
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


## ----train_set and test_set creation--------------------------------------------------------
# Test set will be 10% of edx data
set.seed(1) # if using R 4.0 or later, 
# use `set.seed(1, sample.kind="Rounding")`
test_index <- createDataPartition(y = edx$rating, 
    times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

### Removing temporary files
rm(dl, ratings, movies, test_index, temp, removed)


## ----remove movielens, eval=FALSE, include=FALSE--------------------------------------------
## #rm(movielens)


## ----Exploratory data analysis: class-------------------------------------------------------
### Exploratory data analysis
# Class of edx, train_set, test_set and validation
class(edx)
class(train_set)
class(test_set)
class(validation)


## ----eda: dim-------------------------------------------------------------------------------
# Dimensions of edx, train_set, test_set and validation datasets
# Note: the values differ on my Macbook Pro and Chromebook.
dim(edx)
dim(train_set)
dim(test_set)
dim(validation)


## ----eda: str-------------------------------------------------------------------------------
# Structure of edx, train_set, test_set and validation datasets
# Argument list.len=3 lists first 3 variables only
str(edx, list.len=3)
str(train_set, list.len=3)
str(test_set, list.len=3)
str(validation, list.len=3)


## ----eda: names-----------------------------------------------------------------------------
# Names in edx, train_set, test_set and validation datasets
names(edx)
names(train_set)
names(test_set)
names(validation)


## ----eda: head------------------------------------------------------------------------------
# First 6 rows in edx, train_set, test_set and validation datasets
edx %>% head()
train_set %>% head()
test_set %>% head()
validation %>% head()


## ----eda: summary---------------------------------------------------------------------------
# Summary of edx, train_set, test_set and validation datasets
summary(edx)
summary(train_set)
summary(test_set)
summary(validation)


## ----eda: # unique movies-------------------------------------------------------------------
# Number of unique movies in edx, train_set, test_set and validation 
# datasets
n_distinct(edx$movieId)
n_distinct(train_set$movieId)
n_distinct(test_set$movieId)
n_distinct(validation$movieId)


## ----eda: # unique users--------------------------------------------------------------------
# Number of unique users in edx, train_set, test_set and validation 
# datasets
n_distinct(edx$userId)
n_distinct(train_set$userId)
n_distinct(test_set$userId)
n_distinct(validation$userId)


## ----eda: classification by genres----------------------------------------------------------
# Classification of movies by genres in edx, train_set, test_set and 
# validation datasets
edx %>% group_by(genres) %>% summarise(n = n()) %>% 
  arrange(desc(n))
train_set %>% group_by(genres) %>% summarise(n = n()) %>% 
  arrange(desc(n))
test_set %>% group_by(genres) %>% summarise(n = n()) %>% 
  arrange(desc(n))
validation %>% group_by(genres) %>% summarise(n = n()) %>% 
  arrange(desc(n))


## ----eda: movies with greatest # of ratings-------------------------------------------------
# Movies with greatest number of ratings in edx, train_set, test_set 
# and validation datasets
edx %>% group_by(title) %>% summarise(n = n()) %>% 
  arrange(desc(n))
train_set %>% group_by(title) %>% summarise(n = n()) %>% 
  arrange(desc(n))
test_set %>% group_by(title) %>% summarise(n = n()) %>% 
  arrange(desc(n))
validation %>% group_by(title) %>% summarise(n = n()) %>% 
  arrange(desc(n))


## ----eda: # movies given ratings------------------------------------------------------------
# Number of movies with given ratings in edx, train_set, test_set 
# and validation datasets
edx %>% group_by(rating) %>% summarise(n = n()) %>% 
  arrange(desc(rating))
train_set %>% group_by(rating) %>% summarise(n = n()) %>% 
  arrange(desc(rating))
test_set %>% group_by(rating) %>% summarise(n = n()) %>% 
  arrange(desc(rating))
validation %>% group_by(rating) %>% summarise(n = n()) %>% 
  arrange(desc(rating))


## -------------------------------------------------------------------------------------------
# Plot of distribution of ratings
edx %>% group_by(rating) %>% summarize(count = n()) %>%
  ggplot(aes(x=rating, y=count)) +
  geom_col() +
  xlab("Ratings") +
  ylab("Count") +
  theme_classic()

train_set %>% group_by(rating) %>% summarize(count = n()) %>%
  ggplot(aes(x=rating, y=count)) +
  geom_col() +
  xlab("Ratings") +
  ylab("Count") +
  theme_classic()

test_set %>% group_by(rating) %>% summarize(count = n()) %>%
  ggplot(aes(x=rating, y=count)) +
  geom_col() +
  xlab("Ratings") +
  ylab("Count") +
  theme_classic()

validation %>% group_by(rating) %>% summarize(count = n()) %>%
  ggplot(aes(x=rating, y=count)) +
  geom_col() +
  xlab("Ratings") +
  ylab("Count") +
  theme_classic()


## ----message=FALSE, warning=FALSE-----------------------------------------------------------
# Skew in distribution of ratings by users
edx %>% group_by(userId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color="black", fill="white") +
  xlab("Ratings") +
  ylab("Users") +
  scale_x_log10() + theme_classic()

train_set %>% group_by(userId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color="black", fill="white") +
  xlab("Ratings") +
  ylab("Users") +
  scale_x_log10() + theme_classic()

test_set %>% group_by(userId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color="black", fill="white") +
  xlab("Ratings") +
  ylab("Users") +
  scale_x_log10() + theme_classic()

validation %>% group_by(userId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color="black", fill="white") +
  xlab("Ratings") +
  ylab("Users") +
  scale_x_log10() + theme_classic()


## ----message=FALSE, warning=FALSE-----------------------------------------------------------
# Plot of average ratings by userId
edx %>% group_by(userId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="black", fill="white", binwidth=0.5) +
  xlab("Rating") +
  ylab("Number of users") +
  theme_classic()

train_set %>% group_by(userId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="black", fill="white", binwidth=0.5) +
  xlab("Rating") +
  ylab("Number of users") +
  theme_classic()

test_set %>% group_by(userId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="black", fill="white", binwidth=0.5) +
  xlab("Rating") +
  ylab("Number of users") +
  theme_classic()

validation %>% group_by(userId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="black", fill="white", binwidth=0.5) +
  xlab("Rating") +
  ylab("Number of users") +
  theme_classic()


## ----message=FALSE, warning=FALSE-----------------------------------------------------------
# Skew in distribution of ratings by movieId
edx %>% group_by(movieId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color="black", fill="white") +
  xlab("Ratings") +
  ylab("MovieId") +
  scale_x_log10() + theme_classic()

train_set %>% group_by(movieId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color="black", fill="white") +
  xlab("Ratings") +
  ylab("MovieId") +
  scale_x_log10() + theme_classic()

test_set %>% group_by(movieId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color="black", fill="white") +
  xlab("Ratings") +
  ylab("MovieId") +
  scale_x_log10() + theme_classic()

validation %>% group_by(movieId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color="black", fill="white") +
  xlab("Ratings") +
  ylab("MovieId") +
  scale_x_log10() + theme_classic()


## ----message=FALSE, warning=FALSE-----------------------------------------------------------
# Plot of average ratings by movieId
edx %>% group_by(movieId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="black", fill="white", binwidth=0.5) +
  xlab("Rating") +
  ylab("Number of movies") +
  theme_classic()

train_set %>% group_by(movieId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="black", fill="white", binwidth=0.5) +
  xlab("Rating") +
  ylab("Number of movies") +
  theme_classic()

test_set %>% group_by(movieId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="black", fill="white", binwidth=0.5) +
  xlab("Rating") +
  ylab("Number of movies") +
  theme_classic()

validation %>% group_by(movieId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="black", fill="white", binwidth=0.5) +
  xlab("Rating") +
  ylab("Number of movies") +
  theme_classic()


## -------------------------------------------------------------------------------------------
# Evidence of genre effect on average ratings
# Using a smaller size of n as filter when there are lesser data 
# points
edx %>% group_by(genres) %>%
  summarize(n = n(), avg_rating = mean(rating), 
  se_rating = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, desc(avg_rating))) %>%
  ggplot(aes(x = genres, y = avg_rating, 
  ymin = avg_rating - 2*se_rating, 
  ymax = avg_rating + 2*se_rating)) + 
  geom_point() + geom_errorbar() + theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
train_set %>% group_by(genres) %>%
  summarize(n = n(), avg_rating = mean(rating), 
  se_rating = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, desc(avg_rating))) %>%
  ggplot(aes(x = genres, y = avg_rating, 
  ymin = avg_rating - 2*se_rating, 
  ymax = avg_rating + 2*se_rating)) + 
  geom_point() + geom_errorbar() + theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

test_set %>% group_by(genres) %>%
  summarize(n = n(), avg_rating = mean(rating), 
  se_rating = sd(rating)/sqrt(n())) %>%
  filter(n >= 10000) %>% 
  mutate(genres = reorder(genres, desc(avg_rating))) %>%
  ggplot(aes(x = genres, y = avg_rating, 
  ymin = avg_rating - 2*se_rating, 
  ymax = avg_rating + 2*se_rating)) + 
  geom_point() + geom_errorbar() + theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

validation %>% group_by(genres) %>%
  summarize(n = n(), avg_rating = mean(rating), 
  se_rating = sd(rating)/sqrt(n())) %>%
  filter(n >= 10000) %>% 
  mutate(genres = reorder(genres, desc(avg_rating))) %>%
  ggplot(aes(x = genres, y = avg_rating, 
  ymin = avg_rating - 2*se_rating, 
  ymax = avg_rating + 2*se_rating)) + 
  geom_point() + geom_errorbar() + theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


## -------------------------------------------------------------------------------------------
# A look at time effect
edx %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(date, avg_rating)) +
  geom_point() +
  geom_smooth() + theme_classic()

train_set %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(date, avg_rating)) +
  geom_point() +
  geom_smooth() + theme_classic()

test_set %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(date, avg_rating)) +
  geom_point() +
  geom_smooth() + theme_classic()

validation %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(date, avg_rating)) +
  geom_point() +
  geom_smooth() + theme_classic()


## ----RMSE function--------------------------------------------------------------------------
# Creating function to generate RMSE values
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm=TRUE))
}


## ----random value, message=FALSE, warning=FALSE---------------------------------------------
### Building the Recommendation System: Using a random value (2.5) 
# for all the movies of the test_set dataset
mu_hat <- 2.5
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

rmse_results <- data_frame(method = "Random value (2.5)", 
    RMSE = naive_rmse)


## ----avaerage ratings-----------------------------------------------------------------------
### Building the Recommendation System: Using the average rating of 
# the train_set for all the movies of the test_set dataset
mu_hat <- mean(train_set$rating)
mu_hat

mu_rmse <- RMSE(test_set$rating, mu_hat)
mu_rmse

rmse_results <- bind_rows(rmse_results,
  data_frame(method = "Using the average", 
  RMSE = mu_rmse))
rmse_results %>% knitr::kable()


## -------------------------------------------------------------------------------------------
### Improving by incorporating the fact that some movies are great,
# some are not. Movie effect model.
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% ggplot(aes(b_i)) +
                geom_histogram(bins = 30, color = "black") +
                theme_classic()

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
  data_frame(method="Movie Effect Model",
  RMSE = model_1_rmse))
rmse_results %>% knitr::kable()


## -------------------------------------------------------------------------------------------
### Improving by incorporating the fact that some users give great
# reviews, some do not. User effect model.
train_set %>% 
  group_by(userId) %>% 
  summarize(mean_rating = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(mean_rating)) + 
  geom_histogram(bins = 30, color = "black") +
  theme_classic()


## ----movie + user effect--------------------------------------------------------------------
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


## -------------------------------------------------------------------------------------------
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
  data_frame(method="Movie + User Effects Model",
  RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


## ----movie + user + genre effect------------------------------------------------------------
### Movie + User + Genre effect model
genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

model_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
  data_frame(method="Movie + User + Genre Effects",
  RMSE = model_rmse ))
rmse_results %>% knitr::kable()


## ----movie + user + genre + time effect-----------------------------------------------------
### Movie + User + Genre + Time effect model
time_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%  
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(b_d = mean(rating - mu - b_i - b_u - b_g))

predicted_ratings <- test_set %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(time_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
  pull(pred)

model_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
  data_frame(method="Movie + User + Genre + Time Effects",
  RMSE = model_rmse ))
rmse_results %>% knitr::kable()


## ----regularisation lambda 10---------------------------------------------------------------
# Regularisation with constant lambda
lambda <- 10 # Test lambda
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 


## -------------------------------------------------------------------------------------------
# Making plot of regularised estimates vs. original least square
# estimates
data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5) +
  theme_classic()


## ----reg movie effect with lambda 10--------------------------------------------------------
### Regularised movie effect model, lambda = 10
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
  data_frame(method="Reg. Movie Effect Model with lambda=10",  
  RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()


## -------------------------------------------------------------------------------------------
### Regularised movie effect model
# Choosing optimal lambda, the penalty parameter
# CV (cross-validation) set will be 10% of train_set data
set.seed(1) # if using R 4.0 or later, 
# use `set.seed(1, sample.kind="Rounding")`
test_index <- createDataPartition(y = train_set$rating, 
                                  times = 1, p = 0.1, list = FALSE)
cv_set <- train_set[test_index,]

# Optimal lambda from a sequence: get lowest rmse
lambdas <- seq(0, 10, 0.5)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- cv_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, cv_set$rating))
})

qplot(lambdas, rmses) 

l = lambdas[which.min(rmses)]


## -------------------------------------------------------------------------------------------
mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l))

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

model_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
  data_frame(method="Reg. Movie Effect",  
  RMSE = model_rmse))
rmse_results %>% knitr::kable()


## -------------------------------------------------------------------------------------------
### Regularised movie + user effect model
# Optimal lambda from a sequence: get lowest rmse
lambdas <- seq(0, 10, 0.5)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  user_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  predicted_ratings <- cv_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, cv_set$rating))
})

qplot(lambdas, rmses) 

l = lambdas[which.min(rmses)]

mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l))

user_reg_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+l))

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
  data_frame(method="Reg. Movie + User Effect",  
  RMSE = model_rmse))
rmse_results %>% knitr::kable()


## -------------------------------------------------------------------------------------------
### Regularised movie + user + genre effect model
# Optimal lambda from a sequence: get lowest rmse
lambdas <- seq(0, 10, 0.5)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  user_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  genre_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  
  predicted_ratings <- cv_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(genre_avgs, by='genres') %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, cv_set$rating))
})

qplot(lambdas, rmses) 

l = lambdas[which.min(rmses)]

mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l))

user_reg_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+l))

genre_reg_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by='userId') %>%
  left_join(genre_reg_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

model_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
  data_frame(method="Reg. Movie + User + Genre Effect",  
  RMSE = model_rmse))
rmse_results %>% knitr::kable()


## -------------------------------------------------------------------------------------------
### Regularised movie + user + genre + time effect 
### model
# Optimal lambda from a sequence: get lowest rmse
lambdas <- seq(0, 10, 0.5)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  user_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  genre_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  
  time_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(genre_avgs, by='genres') %>%  
    mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
    group_by(date) %>%
    summarize(b_d = sum(rating - mu - b_i - b_u - b_g)/(n()+l))
  
  predicted_ratings <- cv_set %>% 
    mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(genre_avgs, by='genres') %>%
    left_join(time_avgs, by='date') %>%
    mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, cv_set$rating))
})

qplot(lambdas, rmses) 

l = lambdas[which.min(rmses)]

mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l))

user_reg_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+l))

genre_reg_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))

time_reg_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%  
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(b_d = sum(rating - mu - b_i - b_u - b_g)/(n()+l))

predicted_ratings <- test_set %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by='userId') %>%
  left_join(genre_reg_avgs, by='genres') %>%
  left_join(time_reg_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
  pull(pred)

model_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
  data_frame(method="Reg. Movie + User + Genre + Time Effect",  
  RMSE = model_rmse))
rmse_results %>% knitr::kable()


## -------------------------------------------------------------------------------------------
### Validation 
# Optimal lambda from a sequence: get lowest rmse
# Here we use all of train_set dataset to train
# and all of test_set dataset to optimise lambda
lambdas <- seq(0, 10, 0.5)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  user_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  genre_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  
  time_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(genre_avgs, by='genres') %>%  
    mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
    group_by(date) %>%
    summarize(b_d = sum(rating - mu - b_i - b_u - b_g)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(genre_avgs, by='genres') %>%
    left_join(time_avgs, by='date') %>%
    mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses) 

l = lambdas[which.min(rmses)]

mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l))

user_reg_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+l))

genre_reg_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))

time_reg_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%  
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(b_d = sum(rating - mu - b_i - b_u - b_g)/(n()+l))

predicted_ratings <- validation %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by='userId') %>%
  left_join(genre_reg_avgs, by='genres') %>%
  left_join(time_reg_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
  pull(pred)

model_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
  data_frame(method="Validation",  
  RMSE = model_rmse))


## ----Display all RMSE results---------------------------------------------------------------
rmse_results %>% knitr::kable()


## ----Display validation RMSE, echo=FALSE----------------------------------------------------
knitr::kable(rmse_results[12, ])


## ----Clear memory---------------------------------------------------------------------------
rm(list=ls())

