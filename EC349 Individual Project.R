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
review_data  <- stream_in(file("yelp_academic_dataset_review.json")) 
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) 
user_data <- stream_in(file("yelp_academic_dataset_user.json")) 
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json")) 

# Save each dataset to an RDS file
saveRDS(business_data, "business_data.rds")
saveRDS(review_data, "review_data.rds")
saveRDS(checkin_data, "checkin_data.rds")
saveRDS(user_data, "user_data.rds")
saveRDS(tip_data, "tip_data.rds")

# Load the datasets from the saved RDS files
business_data <- readRDS("business_data.rds")
review_data <- readRDS("review_data.rds")
checkin_data <- readRDS("checkin_data.rds")
user_data <- readRDS("user_data.rds")
tip_data <- readRDS("tip_data.rds")

# Exploring Data
installed.packages()
install.packages("tidyverse")
library(tidyverse)
install.packages("hexbin")
library(hexbin)
install.packages("dplyr")
library(dplyr)


str(business_data)
str(review_data)
str(user_data)

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


friends_counts <- table(user_data$friends, useNA = "ifany")
print(friends_counts)

