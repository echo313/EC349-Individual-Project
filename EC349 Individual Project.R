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

str(business_data)
str(review_data)
str(user_data)

attributeRestaurantsPriceRange2_counts <- table(business_data$attribute$RestaurantsPriceRange2)
print(attributeRestaurantsPriceRange2_counts)


colnames(business_data)
