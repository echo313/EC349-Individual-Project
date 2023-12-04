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

# Load the datasets from the saved RDS files
business_data <- readRDS("business_data.rds")
load("Yelp_review_small.Rda") # "review_data_small"
checkin_data <- readRDS("checkin_data.rds")
load("Yelp_user_small.Rda") # "user_data_small"
tip_data <- readRDS("tip_data.rds")

# Exploring Data
installed.packages()
install.packages("tidyverse")
library(tidyverse)
install.packages("hexbin")
library(hexbin)
install.packages("dplyr")
library(dplyr)

ls()


str(business_data)
str(review_data_small)
str(user_data_small)


summary(business_data)
summary(review_data)
summary(user_data)

colnames(business_data)

# Finding all values
city_counts <- table(business_data$city, useNA = "ifany")
print(city_counts)
# too many cities

stars_counts <- table(business_data$stars, useNA = "ifany")
print(stars_counts)

postal_code_counts <- table(business_data$postal_code, useNA = "ifany")
print(postal_code_counts)
# too many postal codes

attributeByAppointmentOnly_counts <- table(business_data$attribute$ByAppointmentOnly, useNA = "ifany")
print(attributeByAppointmentOnly_counts)

attributeBusinessAcceptsCreditCards_counts <- table(business_data$attribute$BusinessAcceptsCreditCards, useNA = "ifany")
print(attributeBusinessAcceptsCreditCards_counts)

attributeBikeParking_counts <- table(business_data$attribute$BikeParking, useNA = "ifany")
print(attributeBikeParking_counts)

attributeRestaurantsPriceRange2_counts <- table(business_data$attribute$RestaurantsPriceRange2, useNA = "ifany")
print(attributeRestaurantsPriceRange2_counts)

attributeCoatCheck_counts <- table(business_data$attribute$CoatCheck, useNA = "ifany")
print(attributeCoatCheck_counts)

attributeRestaurantsTakeOut_counts <- table(business_data$attribute$RestaurantsTakeOut, useNA = "ifany")
print(attributeRestaurantsTakeOut_counts)

attributeRestaurantsDelivery_counts <- table(business_data$attribute$RestaurantsDelivery, useNA = "ifany")
print(attributeRestaurantsDelivery_counts)

attributeCaters_counts <- table(business_data$attribute$Caters, useNA = "ifany")
print(attributeCaters_counts)

business_data_clean <- business_data %>%
  filter(!apply(attributes, 1, function(x) any(is.na(x))))

str(business_data_clean)

# This didn't work, took out all observations meaning either I take out this attribute variable 
# or think of a different way to incorporate it

install.packages("tidytext")
install.packages("caret")
library(tidytext)
library(dplyr)
library(caret)

# Merging dataset

combined_data <- merge(review_data_small, business_data, by = "business_id", all.x = TRUE)
final_data <- merge(combined_data, user_data_small, by = "user_id", all.x = TRUE)

str(final_data)

head(final_data)

library(dplyr)
glimpse(final_data)

# Checking the format of the final_data

set.seed(1)  # for reproducibility

# Sample 6 unique business IDs
sampled_business_ids <- sample(unique(final_data$business_id), 6)

# Sample 6 unique review IDs
sampled_review_ids <- sample(unique(final_data$review_id), 6)

# Sample 6 unique user IDs
sampled_user_ids <- sample(unique(final_data$user_id), 6)

library(dplyr)

# Filter the data
sampled_data <- final_data %>%
  filter(business_id %in% sampled_business_ids |
           review_id %in% sampled_review_ids |
           user_id %in% sampled_user_ids)

View(sampled_data)

# The final_data has been merged correctly. Now I can set 'stars' as the dependent variable. 

# Starting Data prep

city_counts <- table(final_data$city)

# Print the table of city counts
print(city_counts)

# too many observations to set as predictors, would require too many dummies, so remove columns
# Too many NA's in attributes, so exclude
# Hours is also an over complicated variable
# Date is character variable
final_data <- select(final_data, -name.x,-name.y, -address, -city)

final_data <- select(final_data, -postal_code, -attributes, -hours,-date)

is_open_counts <- table(final_data$is_open)
print(is_open_counts)

# hours_counts <- table(final_data$hours)
# print(hours_counts)
# Above code didn't work as the table was too large

# Changing 'friends' to count number of 'friends' instead

# final_data$friend_count <- sapply(final_data$friends, length)
# head(final_data$friend_count)
# tail(final_data$friend_count)
# The above code didn't work properly, too many NA's, which would then count as 1 friend.. 

str(final_data$friends[1])
str(final_data$friends[1:10])
friends_na_proportion <- sum(is.na(final_data$friends)) / nrow(final_data)
print(friends_na_proportion)
# The above code was used to find out how many of the observations of friends were NA's

# Define a custom function
count_friends <- function(friends) {
  if (is.na(friends)) {
    # Return 0 if the entry is NA
    return(0)
  } else {
    # Assuming each entry is a list or vector of friends, return its length
    return(length(friends))
  }
}

# Apply the function to the friends column
final_data$friend_count <- sapply(final_data$friends, count_friends)

# Check the results
head(final_data$friend_count)
tail(final_data$friend_count)

str(final_data)

yelping_since_na_proportion <- sum(is.na(final_data$yelping_since))
print(yelping_since_na_proportion)

yelping_since_nonna_proportion <- sum(!is.na(final_data$yelping_since))
print(yelping_since_nonna_proportion)

# Way too many NA's
final_data <- select(final_data, -yelping_since)


str(final_data)


# Split the categories string into individual categories
split_categories <- strsplit(final_data$categories, ",")

# Unlist and trim spaces
all_categories <- trimws(unlist(split_categories))

# Count the frequency of each category
category_counts <- table(all_categories)

# Print the counts
print(category_counts)

# This provides way too many i need to cut down... use factor 50 rule

# Extract unique categories
unique_categories <- unique(all_categories)

# Count the number of unique categories
num_unique_categories <- length(unique_categories)

# Print the number of unique categories
print(num_unique_categories)
# there are 1307 unique categories. 

str(final_data)

# changed to categories branch
branch_data <- final_data

# Your code to filter categories
split_categories <- strsplit(branch_data$categories, ",")
all_categories <- trimws(unlist(split_categories))
category_counts <- table(all_categories)
threshold <- nrow(branch_data) / 50
significant_categories <- names(category_counts[category_counts >= threshold])

filter_categories <- function(category_list) {
  filtered <- category_list[category_list %in% significant_categories]
  return(paste(filtered, collapse = ", "))
}

branch_data$filtered_categories <- sapply(strsplit(branch_data$categories, ", "), filter_categories)

head(final_data)
head(branch_data)

# Split the categories string into individual categories
split_categories_branch <- strsplit(branch_data$filtered_categories, ",")
all_categories_branch <- trimws(unlist(split_categories_branch))

# Get unique categories
unique_categories_branch <- unique(all_categories_branch)

# Count the number of unique categories
num_unique_categories_branch <- length(unique_categories_branch)

# Print the number of unique categories
print(num_unique_categories_branch)

str(branch_data)

final_data <- branch_data

str(final_data)
