##### Loading in Requirements
# Install all needed libraries if it is not present
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")
if(!require(googledrive)) install.packages("googledrive")
if(!require(caret)) install.packages("caret")

library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(lubridate)
library(googledrive)
library(caret)

RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Downloading and reading in datasets
if(!exists("edx")){
folder_url <- "https://drive.google.com/drive/folders/1IZcBBX0OmL9wu9AdzMBFUG8GoPbGQ38D"
folder <- drive_get(as_id(folder_url))
rdrfiles <- drive_ls(folder)
walk(rdrfiles$id, ~ drive_download(as_id(.x),overwrite = T))

edx <- readRDS("edx.rds")
validation <- readRDS("validation.rds")}

##### Exploratory Data Analysis
#Number of distinct users and movies
edx %>% summarize(Users = n_distinct(userId),
                  Movies = n_distinct(movieId))

#Number of missing values
sapply(edx, function(x) sum(is.na(x)))

#Viewing data
head(edx)

#Plotting Distribution of Ratings
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  ggtitle("Distribution of ratings")

#Number of Movies with number of ratings
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("# of ratings") +
  ylab("# of movies") +
  ggtitle("Number of Ratings per Movie")

#generating table of obscure movies
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:20)

#making a plot which shows the positive correlation of #ratings a movie receives and its avg rating
edx %>% group_by(movieId) %>% summarise(n = n(), avgrat = mean(rating)) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(avgrat,n)) + geom_point()+ scale_y_continuous(trans = "log2")+ 
  geom_smooth(method = lm) + ylab("#Ratings per movie") + 
  xlab("Average rating per movie") + ggtitle("#Ratings vs. Avg. Rating")

#generating plot and summary to show user bias 
edx %>% group_by(userId) %>% summarise(avg = mean(rating)) %>% 
  arrange(desc(avg)) %>% 
  ggplot(aes(avg)) + geom_histogram(binwidth = .25)+
  xlab("Average Rating per User") + ylab("Frequency of rating")+
  ggtitle("Frequency of Average Ratings Given by Users")
edx %>% group_by(userId) %>% summarise(Avg_Rating = mean(rating)) %>% 
  select(Avg_Rating) %>% summary(Avg_Rating)

##### Methods and Model Generation
#partitioning edx data so training can be performed without using Validation set 
test_index <- createDataPartition(edx$rating,times = 1,p = .2,list = F)
test_set <- data.frame(edx[test_index,])
train_set <- data.frame(edx[-test_index,])
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#calculating average of training set
mu <- mean(train_set$rating)

#calculating RMSE based on assumption that all users will rate the average
naive_rmse <- RMSE(mu,test_set$rating)

#storing RMSE in tibble, which other models will be added to later 
rmse_results <- data_frame(method = "Average movie rating model", RMSE = naive_rmse)

#developing model that takes movie effect into account
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
#calculating predicted rating on test_set portion of edx set
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>% 
  pull(pred)
#calculating RMSE for predicted ratings and storing it in the rmse_results tibble
predicted_ratings <- data.frame(rating = predicted_ratings)
model_1_rmse <- RMSE(test_set$rating,predicted_ratings$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model",  
                                     RMSE = model_1_rmse ))

#developing model that takes user and movie effect into account
user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#predicting ratings on test_set using model trained on train_set
predicted_ratings <- test_set%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
#calculating model 2 RMSE results and storing in the tibble 
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user effect model",  
                                     RMSE = model_2_rmse))

#calculating optimal lambda for regularized model 
#vector of lamdbdas
lambdas <- seq(0, 10, 0.25)
#looping the RMSE calculation which trains on train_set and predicts on test_set through lamda vector
rmses <- sapply(lambdas, function(l){
  #calculating average for average training set raring
  mu <- mean(train_set$rating)
  #calculating movie bias for a given lambda
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  #calculating user bias for a given lambda
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  #predicting ratings on test_set
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  #calculating RMSE on test set for a given lambda
  return(RMSE(predicted_ratings, test_set$rating))
})

#visualizing where lambda minimizes RMSE
qplot(lambdas, rmses)  

#finding which lambda minimizes RMSE
lambda <- lambdas[which.min(rmses)]
#storing regularized user and movie model RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie and user effect model",  
                                     RMSE = min(rmses)))

#training model on full edx dataset using lambda discovered previously
l <- lambda
#calculating average of edx dataset
mu <- mean(edx$rating)
#calculating movie bias term
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))
#calculating user bias term
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))
#predicting ratings for validation dataset
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
#calculating RMSE for edx training on regularized user and movie model
validation_RMSE <- RMSE(predicted_ratings, validation$rating)
#storing final validation RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Final RMSE on Validaiton Dataset",  
                                     RMSE = validation_RMSE))

#printing table of RMSE results for the different models
options(pillar.sigfig = 4)
rmse_results
