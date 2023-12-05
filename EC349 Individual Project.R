#Setting GitHub repository
library(usethis)
use_github()

#Pre-Processing Yelp Academic Data for the Assignment
library(jsonlite)

#Clear
cat("\014")  
rm(list=ls())

#Set Directory as appropriate
setwd("C:/Users/jjune/OneDrive/Desktop/EC349 Data Science for Economists/EC349 Individual Project")

#Load Different Data

business_data <- stream_in(file("yelp_academic_dataset_business.json")) 
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) 
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json")) 

# Save each dataset to an RDS file
saveRDS(business_data, "business_data.rds")
saveRDS(checkin_data, "checkin_data.rds")
saveRDS(tip_data, "tip_data.rds")
saveRDS(user_data, "user_data.rds")

# Load the datasets from the saved RDS files
business_data <- readRDS("business_data.rds")
load("Yelp_review_small.Rda") # "review_data_small"
checkin_data <- readRDS("checkin_data.rds")
load("Yelp_user_small.Rda") # "user_data_small"
tip_data <- readRDS("tip_data.rds")
user_data <- readRDS("user_data.rds")

# Exploring Data
installed.packages()
install.packages("tidyverse")
library(tidyverse)
install.packages("hexbin")
library(hexbin)
install.packages("dplyr")
library(dplyr)

ls()

# It turns out i didn't merge the dataset wrong, but rather because the dataset was subsetted from a larger dataset
# There were inconsistent pairings, in that there were user id's in the review_data_small dataset that were not 
# Present in the user_data_small dataset. 

# Remerging the dataset with full user_data

user_data <- readRDS("user_data.rds")

review_user_merge2 <- merge(review_data_small, user_data, by = "user_id", all.x = TRUE)
final_data3 <- merge(review_user_merge2, business_data, by = "business_id", all.x = TRUE)

final_data3 <- select(final_data3, -name.x,-name.y, -address, -city)

final_data3 <- select(final_data3, -postal_code, -attributes, -hours,-date)

final_data3 <- final_data3 %>% rename(stars = stars.x)

final_data3 <- final_data3 %>% rename(stars_business = stars.y)

final_data3 <- final_data3 %>% rename(useful_reviews = useful.x)

final_data3 <- final_data3 %>% rename(funny_reviews = funny.x)

final_data3 <- final_data3 %>% rename(cool_reviews = cool.x)

final_data3 <- final_data3 %>% rename(review_count_user = review_count.x)

final_data3 <- final_data3 %>% rename(useful_sentbyuser = useful.y)

final_data3 <- final_data3 %>% rename(funny_sentbyuser = funny.y)

final_data3 <- final_data3 %>% rename(cool_sentbyuser = cool.y)

final_data3 <- final_data3 %>% rename(review_count_business = review_count.y)

summary(final_data3$review_count_business)

summary(final_data3$review_count_user)


# Filter categories again
split_categories <- strsplit(final_data3$categories, ",")
all_categories <- trimws(unlist(split_categories))
category_counts <- table(all_categories)
threshold <- nrow(final_data3) / 50
significant_categories <- names(category_counts[category_counts >= threshold])

filter_categories <- function(category_list) {
  filtered <- category_list[category_list %in% significant_categories]
  return(paste(filtered, collapse = ", "))
}

final_data3$filtered_categories <- sapply(strsplit(final_data3$categories, ", "), filter_categories)

head(final_data3)

# Split the categories and unlist them into a single vector
split_categories <- strsplit(final_data3$filtered_categories, ",")
all_categories <- unlist(split_categories)

# Trim any leading or trailing spaces
all_categories <- trimws(all_categories)

# Create a frequency table of individual categories
category_counts_individual <- table(all_categories)

# View the table
print(category_counts_individual)

final_data3 <- select(final_data3, -categories)

# Check for NAs

na_count_business_id <- sum(is.na(final_data3$business_id))
print(na_count_business_id)

na_count_user_id <- sum(is.na(final_data3$user_id))
print(na_count_user_id)

na_count_review_id <- sum(is.na(final_data3$review_id))
print(na_count_review_id)

na_count_stars <- sum(is.na(final_data3$stars))
print(na_count_stars)

na_count_useful_reviews <- sum(is.na(final_data3$useful_reviews))
print(na_count_useful_reviews)

na_count_funny_reviews <- sum(is.na(final_data3$funny_reviews))
print(na_count_funny_reviews)

na_count_cool_reviews <- sum(is.na(final_data3$cool_reviews))
print(na_count_cool_reviews)

na_count_text <- sum(is.na(final_data3$text))
print(na_count_text)

na_count_review_count_user <- sum(is.na(final_data3$review_count_user))
print(na_count_review_count_user)

# Checking number of rows
nrow(final_data3)

# Excluding rows with NA for review_count_user
final_data3 <- final_data3[!is.na(final_data3$review_count_user), ]

nrow(final_data3)

# Checking for NAs again
na_count_yelping_since <- sum(is.na(final_data3$yelping_since))
print(na_count_yelping_since)

na_count_useful_sentbyuser<- sum(is.na(final_data3$useful_sentbyuser))
print(na_count_useful_sentbyuser)

na_count_funny_sentbyuser<- sum(is.na(final_data3$funny_sentbyuser))
print(na_count_funny_sentbyuser)

na_count_cool_sentbyuser<- sum(is.na(final_data3$cool_sentbyuser))
print(na_count_cool_sentbyuser)

na_count_elite<- sum(is.na(final_data3$elite))
print(na_count_elite)

# Values are not given as NA and instead " "
head(final_data3$elite)

# Changing elite so that it counts number of years elite.
count_elite_years <- function(elite_string) {
  if (elite_string == "") {
    # Return 0 if the string is empty
    return(0)
  } else {
    # Split the string by commas and count the elements
    years <- unlist(strsplit(elite_string, ","))
    return(length(years))
  }
}

# Apply the function to the elite column
final_data3$elite <- sapply(final_data3$elite, count_elite_years)

# Check the results
head(final_data3$elite)

summary(final_data3$elite)

# Checking for NA again
na_count_friends<- sum(is.na(final_data3$friends))
print(na_count_friends)

na_count_fans<- sum(is.na(final_data3$fans))
print(na_count_fans)

final_data3 <- select(final_data3, -yelping_since)

na_count_average_stars<- sum(is.na(final_data3$average_stars))
print(na_count_average_stars)

na_count_compliment_hot<- sum(is.na(final_data3$compliment_hot))
print(na_count_compliment_hot)

na_count_compliment_more<- sum(is.na(final_data3$compliment_more))
print(na_count_compliment_more)

na_count_compliment_profile<- sum(is.na(final_data3$compliment_profile))
print(na_count_compliment_profile)

na_count_compliment_cute<- sum(is.na(final_data3$compliment_cute))
print(na_count_compliment_cute)

na_count_compliment_list<- sum(is.na(final_data3$compliment_list))
print(na_count_compliment_list)

na_count_compliment_note<- sum(is.na(final_data3$compliment_note))
print(na_count_compliment_note)

na_count_compliment_plain<- sum(is.na(final_data3$compliment_plain))
print(na_count_compliment_plain)

na_count_compliment_cool<- sum(is.na(final_data3$compliment_cool))
print(na_count_compliment_cool)

final_data3 <- select(final_data3, -friends)

na_count_compliment_funny<- sum(is.na(final_data3$compliment_funny))
print(na_count_compliment_funny)

na_count_compliment_writer<- sum(is.na(final_data3$compliment_writer))
print(na_count_compliment_writer)

na_count_compliment_photos<- sum(is.na(final_data3$compliment_photos))
print(na_count_compliment_photos)

na_count_state<- sum(is.na(final_data3$state))
print(na_count_state)

na_count_latitude<- sum(is.na(final_data3$latitude))
print(na_count_latitude)

na_count_longitude<- sum(is.na(final_data3$longitude))
print(na_count_longitude)

na_count_stars_business<- sum(is.na(final_data3$stars_business))
print(na_count_stars_business)

na_count_review_count_business<- sum(is.na(final_data3$review_count_business))
print(na_count_review_count_business)

na_count_is_open<- sum(is.na(final_data3$is_open))
print(na_count_is_open)

final_data3$state <- as.factor(final_data3$state)


final_data3 <- select(final_data3, -filtered_categories)

# Splitting Data
library(caret)

set.seed(1)  # Set seed for reproducibility
indexes <- createDataPartition(final_data3$stars, p = 0.8, list = FALSE)
training_data <- final_data3[indexes, ]
test_data <- final_data3[-indexes, ]

# Ensuring the test dataset has exactly 10,000 observations
test_data <- test_data[sample(nrow(test_data), 10000), ]

# Trying Sentiment Analysis
install.packages("tidytext")
library(tidytext)

# Add an index column to training_data
training_data$index <- row_number(training_data)

# Sentiment Analysis (Revised)
get_sentiments("bing") -> bing_sentiments
train_sentiment <- training_data %>%
  unnest_tokens(word, text) %>%
  inner_join(bing_sentiments) %>%
  group_by(index) %>%
  summarize(sentiment_score = sum(ifelse(sentiment == "positive", 1, -1)))

training_data <- left_join(training_data, train_sentiment, by = "index")

str(training_data)

na_count_sentiment_score_training<- sum(is.na(training_data$sentiment_score))
print(na_count_sentiment_score_training)

# Check for NA values in the training dataset
sum(is.na(training_data))

# Create a copy of the dataset excluding rows with any NAs
training_data_no_na <- training_data[complete.cases(training_data), ]

# Check dimensions of the new dataset
dim(training_data_no_na)

# Ensure no NA values are present
sum(is.na(training_data_no_na))

training_data_no_na$text <- NULL

# Exclude ID variables
model_data <- training_data_no_na[, !(names(training_data_no_na) %in% c("business_id", "user_id", "review_id"))]
model_data$index <- NULL

# Now trying to model

library(glmnet)

# Prepare the data for the model
x <- model.matrix(stars ~ . - 1, data = model_data)  # -1 to exclude the intercept
y <- model_data$stars

# Fit the Lasso model with cross-validation
set.seed(1)  # For reproducibility
cv_model <- cv.glmnet(x, y, alpha = 1, family = "gaussian", nfolds = 10)  # nfolds for k-fold cross-validation

# The best lambda value
best_lambda <- cv_model$lambda.min

# Check the model summary
print(cv_model)

# Model with sentiment analysis
saveRDS(cv_model, file = "lasso_cv_model.rds")




# Exclude the 'sentiment_score' column
model_data_without_sentiment <- model_data[, !(names(model_data) %in% c("sentiment_score"))]
model_data_without_sentiment$index <- NULL

# Prepare the model matrix
x_without_sentiment <- model.matrix(stars ~ . - 1, data = model_data_without_sentiment)  # -1 to exclude the intercept
y_without_sentiment <- model_data_without_sentiment$stars

# Fit the Lasso model with cross-validation
set.seed(1)  # For reproducibility
cv_model_without_sentiment <- cv.glmnet(x_without_sentiment, y_without_sentiment, alpha = 1, family = "gaussian", nfolds = 10)

# Check the model
print(cv_model_without_sentiment)

saveRDS(cv_model_without_sentiment, file = "lasso_cv_model_without_sentiment.rds")

# Now test on test data

# Add an index column to test_data
test_data$index <- row_number(test_data)

# Sentiment Analysis 
get_sentiments("bing") -> bing_sentiments
test_sentiment <- test_data %>%
  unnest_tokens(word, text) %>%
  inner_join(bing_sentiments) %>%
  group_by(index) %>%
  summarize(sentiment_score = sum(ifelse(sentiment == "positive", 1, -1)))

test_data <- left_join(test_data, test_sentiment, by = "index")


sum(is.na(test_data))

test_data_no_na <- test_data[complete.cases(test_data), ]

test_data_no_na$index <- NULL

# Create a copy without the 'sentiment_score' column
no_sentiment_test_data <- test_data_no_na[, !(names(test_data_no_na) %in% c("sentiment_score"))]


# Testing the model with sentiment analysis
cv_model <- readRDS("lasso_cv_model.rds")

# Prepare the model matrix for the test data
no_ids_test_data <- test_data_no_na[, !(names(test_data_no_na) %in% c("business_id", "user_id", "review_id"))]

no_ids_test_data$text <- NULL

coef(cv_model)

# Prepare the model matrix for the test data
x_test <- model.matrix(~ . - 1 - stars, data = no_ids_test_data)  

predictions_min <- predict(cv_model, newx = x_test, s = "lambda.min")


y_test <- no_ids_test_data$stars

# Predict Model using min. lambda
mse_cv_model_min <- mean((y_test - predictions_min)^2)

# Evaluation 
print(mse_cv_model_min)
# 1.024913

r_squared_cv_model_min <- 1 - sum((y_test - predictions_min)^2) / sum((y_test - mean(y_test))^2)
print(r_squared_cv_model_min)
# 0.5248089

# Predict Model using 1se lambda
predictions_1se <- predict(cv_model, newx = x_test, s = "lambda.1se")

mse_cv_model_1se <- mean((y_test - predictions_1se)^2)
print(mse_cv_model_1se)
# 1.035778

r_squared_cv_model_1se <- 1 - sum((y_test - predictions_1se)^2) / sum((y_test - mean(y_test))^2)
print(r_squared_cv_model_1se)
# 0.5197715

# load cv_model_no_sentiment
cv_model_no_sentiment <- readRDS("lasso_cv_model_without_sentiment.rds")

# Test cv_model_no_sentiment with min. Lambda

# Create a copy of the dataset without the 'sentiment_score' column
test_without_sentiment <- no_ids_test_data[, !(names(no_ids_test_data) %in% c("sentiment_score"))]

x_test2 <- model.matrix(~ . - 1 - stars, data = test_without_sentiment)  
predictions_no_sentiment_min <- predict(cv_model_no_sentiment, newx = x_test2, s = "lambda.min")

mse_cv_model_no_sentiment_min <- mean((y_test - predictions_no_sentiment_min)^2)
print(mse_cv_model_no_sentiment_min)
# 1.164779

r_squared_cv_model_no_sentiment_min <- 1 - sum((y_test - predictions_no_sentiment_min)^2) / sum((y_test - mean(y_test))^2)
print(r_squared_cv_model_no_sentiment_min)
# 0.4599614

# Test cv_model_no_sentiment with 1se Lambda
predictions_no_sentiment_1se <- predict(cv_model_no_sentiment, newx = x_test2, s = "lambda.1se")

mse_cv_model_no_sentiment_1se <- mean((y_test - predictions_no_sentiment_1se)^2)
print(mse_cv_model_no_sentiment_1se)
#  1.178501

r_squared_cv_model_no_sentiment_1se <- 1 - sum((y_test - predictions_no_sentiment_1se)^2) / sum((y_test - mean(y_test))^2)
print(r_squared_cv_model_no_sentiment_1se)
# 0.4535993