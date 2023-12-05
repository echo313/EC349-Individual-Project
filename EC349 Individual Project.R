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

final_data <- select(final_data, -categories)

str(final_data)

# Check NA's in state
na_count_state <- sum(is.na(final_data$state))
print(na_count_state)

# Check for NA's in each variable
na_count_user_id <- sum(is.na(final_data$user_id))
print(na_count_user_id)

na_count_business_id <- sum(is.na(final_data$business_id))
print(na_count_business_id)

na_count_review_id <- sum(is.na(final_data$review_id))
print(na_count_review_id)

final_data <- final_data %>% rename(stars_reviews = stars.x)

na_count_stars_reviews <- sum(is.na(final_data$star_reviews))
print(na_count_stars_reviews)

final_data <- final_data %>% rename(useful_reviews = useful.x)

na_count_useful_reviews <- sum(is.na(final_data$useful_reviews))
print(na_count_useful_reviews)

final_data <- final_data %>% rename(funny_reviews = funny.x)

na_count_funny_reviews <- sum(is.na(final_data$funny_reviews))
print(na_count_funny_reviews)

final_data <- final_data %>% rename(cool_reviews = cool.x)

na_count_cool_reviews <- sum(is.na(final_data$cool_reviews))
print(na_count_cool_reviews)


na_count_text <- sum(is.na(final_data$text))
print(na_count_text)

na_count_latitude <- sum(is.na(final_data$latitude))
print(na_count_latitude)

na_count_longitude <- sum(is.na(final_data$longitude))
print(na_count_longitude)


final_data <- final_data %>% rename(stars_business = stars.y)

na_count_stars_business <- sum(is.na(final_data$stars_business))
print(na_count_stars_business)

final_data <- final_data %>% rename(review_count_business = review_count.x)

na_count_review_count_business <- sum(is.na(final_data$review_count_business))
print(na_count_review_count_business)

na_count_is_open <- sum(is.na(final_data$is_open))
print(na_count_is_open)

final_data <- final_data %>% rename(review_count_user = review_count.y)

na_count_review_count_user <- sum(is.na(final_data$review_count_user))
print(na_count_review_count_user)
# too many NAs


zero_count <- sum(final_data$review_count_user == 0, na.rm = TRUE)
print(zero_count)

one_count <- sum(final_data$review_count_user == 1, na.rm = TRUE)
print(one_count)

summary(final_data$review_count_user)

# I thought NAs were 0s, but that doesn't seem to be the case, I will have to take out this variable as it is incomplete
# This is a shame as there could be a relationship between the amount of reviews a user has written and 'stars'

final_data <- select(final_data, -review_count_user)
# Removed review_count_user

final_data <- final_data %>% rename(useful_sentbyuser = useful.y)

na_count_useful_sentbyuser <- sum(is.na(final_data$useful_sentbyuser))
# Interestingly, the number of NAs is the same as in review count. 

final_data <- final_data %>% rename(funny_sentbyuser = funny.y)

na_count_funny_sentbyuser <- sum(is.na(final_data$funny_sentbyuser))
print(na_count_funny_sentbyuser)

zero_count_funny_sentbyuser <- sum(final_data$funny_sentbyuser == 0, na.rm = TRUE)
print(zero_count_funny_sentbyuser)
# Again the same....

final_data <- final_data %>% rename(cool_sentbyuser = cool.y)

na_count_cool_sentbyuser <- sum(is.na(final_data$cool_sentbyuser))
print(na_count_cool_sentbyuser)
# As NA is not the same as 0.=, these variables should be eliminated

final_data <- select(final_data, -cool_sentbyuser, -useful_sentbyuser, -funny_sentbyuser)

# Define a custom function to count the number of elite years
count_elite_years <- function(elite_years) {
  if (is.na(elite_years) || elite_years == "") {
    # Return 0 if the entry is NA or an empty string
    return(0)
  } else {
    # Split the string by commas and count the elements
    years <- strsplit(elite_years, ",")[[1]]
    return(length(years))
  }
}

# Apply the function to the elite column
final_data$elite_years_count <- sapply(final_data$elite, count_elite_years)

# Check the results
head(final_data$elite_years_count)

summary(final_data$elite_years_count)

highest_elite_years_count <- max(final_data$elite_years_count, na.rm = TRUE)
print(highest_elite_years_count)

table(final_data$elite_years_count)

table(final_data$elite)

# Now delete elite and friends, as they are no longer needed
final_data <- select(final_data, -elite, -friends)

# Back to counting NA
na_count_fans <- sum(is.na(final_data$fans))
print(na_count_fans)

zero_count_fans <- sum(final_data$fans == 0, na.rm = TRUE)
print(zero_count_fans)
# Again the NA value is the same as above, making me think these functions were relatively new and hence didn't 
# apply to older reviews/users

# Still get rid of variable..
final_data <- select(final_data, -fans)


na_count_average_stars <- sum(is.na(final_data$average_stars))
print(na_count_average_stars)

summary(final_data$average_stars)

na_count_average_stars_smalldata <- sum(is.na(user_data_small$average_stars))
print(na_count_average_stars_smalldata)



# Assuming you're merging on a column named 'user_id'
# Replace 'user_id' with the actual key column name

# I merged the dataset wrong, need to merge again...

# Merging review data with user data
review_user_merge <- merge(review_data_small, user_data_small, by = "user_id", all.x = TRUE)

# Merging the above result with business data
final_data2 <- merge(review_user_merge, business_data, by = "business_id", all.x = TRUE)

na_count_average_stars2 <- sum(is.na(final_data2$average_stars))
print(na_count_average_stars2)


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
threshold <- nrow(branch_data) / 50
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


