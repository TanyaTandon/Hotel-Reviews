#clear environment
rm(list=ls())

library(tidyverse)
library(MASS)
library(dplyr)

# Loading the raw input
Hotel_Reviews <- read.csv('../00Data/Hotel_Reviews_v2.csv', stringsAsFactors = FALSE)
Hotel_Reviews$Hotel_Country <- word(Hotel_Reviews$Hotel_Address,-1)
Hotel_Reviews$Hotel_Country[Hotel_Reviews$Hotel_Country == "Kingdom"] <- "United Kingdom"

distance_file <- read.csv('../00Data/distance_file.csv', stringsAsFactors = FALSE)
distance_file$distance <- as.numeric(word(distance_file$distance, 1))

Hotel_Reviews <- cbind( Hotel_Reviews , distance_file$distance ) #merge wasn???t working
names(Hotel_Reviews) [ names(Hotel_Reviews) =='distance_file$distance'] <- 'distance'


# Loading correlation feature sets
hotel_cor_details <- read.csv('../01EDA/hotel_cor_details.csv', stringsAsFactors = FALSE)
hotel_cor_details$X <- NULL

# Loading review details feature set
review_details <- read.csv('../01EDA/review_details.csv', stringsAsFactors = FALSE)
review_details$X <- NULL

# Loading reviewer nationality feature set
reviewer_Nat <- read.csv('../01EDA/Reviewer_Nationality.csv', stringsAsFactors = FALSE)
reviewer_Nat$X <- NULL
reviewer_Nat$Reviewer_sub_region[is.na(reviewer_Nat$Reviewer_sub_region)] <- 'NA'
reviewer_Nat$Reviewer_sub_region <- as.factor(reviewer_Nat$Reviewer_sub_region)
reviewer_Nat$Reviewer_sub_region <- relevel(reviewer_Nat$Reviewer_sub_region, ref='NA')

# Loading weather data
weather1 <- read.csv('../00Data/hist_weather1.csv', stringsAsFactors = FALSE)
weather2 <- read.csv('../00Data/hist_weather2.csv', stringsAsFactors = FALSE)
weather3 <- read.csv('../00Data/hist_weather2b.csv', stringsAsFactors = FALSE)
weather4 <- read.csv('../00Data/hist_weather3.csv', stringsAsFactors = FALSE)
weather5 <- read.csv('../00Data/hist_weather4.csv', stringsAsFactors = FALSE)
weather6 <- read.csv('../00Data/hist_weather5.csv', stringsAsFactors = FALSE)
weather7 <- read.csv('../00Data/hist_weather6.csv', stringsAsFactors = FALSE)
weather <- rbind(weather1, weather2, weather3, weather4, weather5, weather6, weather7)
weather$X <- NULL

weather$rain <- str_detect(weather$Summary, regex("rain", ignore_case = TRUE))
weather$precipitation <- str_detect(weather$Summary, regex("precipitation", ignore_case = TRUE))
weather$snow <- str_detect(weather$Summary, regex("snow", ignore_case = TRUE))
weather$clear <- str_detect(weather$Summary, regex("clear", ignore_case = TRUE))
weather$breezy <- str_detect(weather$Summary, regex("breezy", ignore_case = TRUE))
weather$cloudy <- str_detect(weather$Summary, regex("cloudy", ignore_case = TRUE))
weather$overcast <- str_detect(weather$Summary, regex("overcast", ignore_case = TRUE))
weather$foggy <- str_detect(weather$Summary, regex("foggy", ignore_case = TRUE))
weather$humid <- str_detect(weather$Summary, regex("humid", ignore_case = TRUE))

weather$weather_summary <- NA
weather$weather_summary[weather$rain=="TRUE"] <- "rain"
weather$weather_summary[weather$precipitation=="TRUE" & is.na(weather$weather_summary)] <- "rain"
weather$weather_summary[weather$snow=="TRUE" & is.na(weather$weather_summary)] <- "snow"
weather$weather_summary[weather$breezy=="TRUE" & is.na(weather$weather_summary)] <- "breezy"
weather$weather_summary[weather$cloudy=="TRUE" & is.na(weather$weather_summary)] <- "cloudy"
weather$weather_summary[weather$overcast=="TRUE" & is.na(weather$weather_summary)] <- "cloudy"
weather$weather_summary[weather$foggy=="TRUE" & is.na(weather$weather_summary)] <- "foggy"
weather$weather_summary[weather$clear=="TRUE" & is.na(weather$weather_summary)] <- "clear"
weather$weather_summary[weather$humid=="TRUE" & is.na(weather$weather_summary)] <- "humid"
weather$weather_summary <- as.factor(weather$weather_summary)

weather <- weather[, c("City", "Date", "TempHigh", "TempLow", "weather_summary")]
weather_base_info <- read.csv('../01EDA/weather_base_info.csv', stringsAsFactors = FALSE)
weather_base_info$X <- NULL
names(weather_base_info) <- c('ID', 'City', 'Date')

weather_info <- merge(weather_base_info, weather, by.x = c('City', 'Date'), by.y = c('City', 'Date'), all.x = TRUE, all.y = FALSE)

weather_info$Date <- NULL
weather_info$City <- as.factor(weather_info$City)

# TD-IDF feature sets 
hotel_tags <- read.csv("../00Data/hotel_tags.csv", stringsAsFactors = FALSE)
hotel_tags$access_type[hotel_tags$access_type != "none"] <- "access"
hotel_tags$free[hotel_tags$free != "none"] <- "free"
hotel_tags$other_room_types[hotel_tags$other_room_types != "none"] <- "lux"
hotel_tags$room_type[hotel_tags$room_type != "double_room" & hotel_tags$room_type != "king_room" & hotel_tags$room_type !="twin_room"] <- "other"
hotel_tags$trip_type[hotel_tags$trip_type == "solo" | hotel_tags$trip_type == "couple"] <- "other"
hotel_tags$view_type[hotel_tags$view_type != "none"] <- "view"
hotel_tags$access_type <- as.factor(hotel_tags$access_type)
hotel_tags$accessibility <- as.factor(hotel_tags$accessibility)
hotel_tags$free <- as.factor(hotel_tags$free)
hotel_tags$other_room_types <- as.factor(hotel_tags$other_room_types)
hotel_tags$room_type <- as.factor(hotel_tags$room_type)
hotel_tags$time_of_stay <- factor(hotel_tags$time_of_stay, levels=c("stayed_less_than_a_week", "stayed_more_than_a_week"))
hotel_tags$trip_type <- factor(hotel_tags$trip_type, levels = c("business_trip", "family_trip", "group_trip","leisure_trip", "other"))
hotel_tags$view_type <- as.factor(hotel_tags$view_type)

##Customer tags
cust_tags <- read.csv("../00Data/id_tags.csv", stringsAsFactors = FALSE)
cust_tags$access_type_cust[cust_tags$access_type_cust != "none"] <- "access"
cust_tags$free_cust[cust_tags$free_cust != "none"] <- "free"
cust_tags$other_room_types_cust[cust_tags$other_room_types_cust != "none"] <- "lux"
cust_tags$room_type_cust[cust_tags$room_type_cust != "double_room" & cust_tags$room_type_cust != "king_room" & cust_tags$room_type_cust !="twin_room"] <- "other"
cust_tags$trip_type_cust[cust_tags$trip_type_cust == "solo" | cust_tags$trip_type_cust == "couple" | is.na(cust_tags$trip_type_cust)] <- "other"
cust_tags$view_type_cust[cust_tags$view_type_cust != "none"] <- "view"
cust_tags$access_type_cust <- as.factor(cust_tags$access_type_cust)
cust_tags$accessibility_cust <- as.factor(cust_tags$accessibility_cust)
cust_tags$free_cust <- as.factor(cust_tags$free_cust)
cust_tags$other_room_types_cust <- as.factor(cust_tags$other_room_types_cust)
cust_tags$room_type_cust <- as.factor(cust_tags$room_type_cust)
cust_tags$time_of_stay_cust <- factor(cust_tags$time_of_stay_cust, levels=c("stayed_less_than_a_week", "stayed_more_than_a_week"))
cust_tags$trip_type_cust <- factor(cust_tags$trip_type_cust, levels = c("business_trip", "family_trip", "group_trip", "leisure_trip", "other"))
cust_tags$view_type_cust <- as.factor(cust_tags$view_type_cust)


# Reviewer_Score will be the response variable
Hotel_pred_df <- Hotel_Reviews[,c('ID','Hotel_Address','Reviewer_Score', "Reviewer_Nationality", "Hotel_Country", "distance")] 


# Merging in review details features
Hotel_pred_df <- merge(Hotel_pred_df, review_details, by.x = 'ID', by.y = 'ID', all.x = TRUE, all.y = FALSE)
Hotel_pred_df$pct_positive[is.na(Hotel_pred_df$pct_positive)] <- -9999

# Merging in review nationality features
Hotel_pred_df <- merge(Hotel_pred_df, reviewer_Nat, by.x = 'ID', by.y = 'ID', all.x = TRUE, all.y = FALSE)

# Merging in weather features
Hotel_pred_df <- merge(Hotel_pred_df, weather_info, by.x = 'ID', by.y = 'ID', all.x = TRUE, all.y = FALSE)

# Merging with hotel_tags

Hotel_pred_df <- merge(Hotel_pred_df, hotel_tags, by.x = 'Hotel_Address', by.y = 'hotel_id', all.x = TRUE, all.y = FALSE)

#Merging with cust_tags
Hotel_pred_df <- merge(Hotel_pred_df, cust_tags, by.x = 'ID', by.y = 'ID', all.x = TRUE, all.y = FALSE)

##Checking if cust and hotel have the same tag

Hotel_pred_df$test1 <- NA
Hotel_pred_df$test1[Hotel_pred_df$access_type == Hotel_pred_df$access_type_cust & Hotel_pred_df$access_type!="none"] <- 1
Hotel_pred_df$test1[is.na(Hotel_pred_df$test1)] <- 0

Hotel_pred_df$test2 <- NA
Hotel_pred_df$test2[Hotel_pred_df$accessibility == Hotel_pred_df$accessibility_cust & Hotel_pred_df$accessibility!="none"] <- 1
Hotel_pred_df$test2[is.na(Hotel_pred_df$test2)] <- 0

Hotel_pred_df$test3 <- NA
Hotel_pred_df$test3[Hotel_pred_df$free == Hotel_pred_df$free_cust & Hotel_pred_df$free!="none"] <- 1
Hotel_pred_df$test3[is.na(Hotel_pred_df$test3)] <- 0

Hotel_pred_df$test4 <- NA
Hotel_pred_df$test4[Hotel_pred_df$other_room_types == Hotel_pred_df$other_room_types & Hotel_pred_df$other_room_types!="none"] <- 1
Hotel_pred_df$test4[is.na(Hotel_pred_df$test4)] <- 0

Hotel_pred_df$test5 <- NA
Hotel_pred_df$test5[Hotel_pred_df$room_type == Hotel_pred_df$room_type] <- 1
Hotel_pred_df$test5[is.na(Hotel_pred_df$test5)] <- 0

Hotel_pred_df$test6 <- NA
Hotel_pred_df$test6[Hotel_pred_df$view_type == Hotel_pred_df$view_type_cust & Hotel_pred_df$view_type!="none"] <- 1
Hotel_pred_df$test6[is.na(Hotel_pred_df$test6)] <- 0

Hotel_pred_df$test7 <- NA
Hotel_pred_df$test7[Hotel_pred_df$time_of_stay == Hotel_pred_df$time_of_stay_cust] <- 1
Hotel_pred_df$test7[is.na(Hotel_pred_df$test7)] <- 0

Hotel_pred_df$test8 <- NA
Hotel_pred_df$test8[Hotel_pred_df$trip_type == Hotel_pred_df$trip_type_cust] <- 1
Hotel_pred_df$test8[is.na(Hotel_pred_df$test8)] <- 0

Hotel_pred_df$CommonScore <- Hotel_pred_df$test1 + Hotel_pred_df$test2 + Hotel_pred_df$test3 + Hotel_pred_df$test4 + Hotel_pred_df$test5 + Hotel_pred_df$test6 + Hotel_pred_df$test7 + Hotel_pred_df$test8

Hotel_pred_df <- Hotel_pred_df[ , -which(names(Hotel_pred_df) %in% c("Hotel_Address", "test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8"))]

Hotel_pred_df <- Hotel_pred_df[complete.cases(Hotel_pred_df), ]

#Removing white spaces
 Hotel_pred_df$Reviewer_Nationality <- trimws(Hotel_pred_df$Reviewer_Nationality)
 Hotel_pred_df$Hotel_Country <- trimws( Hotel_pred_df$Hotel_Country)
 
# ## Creating tourist feature
 Hotel_pred_df <- Hotel_pred_df %>% 
   mutate(tourists = ifelse( Reviewer_Nationality == Hotel_Country ,0 , 1)) 

Hotel_pred_df <- Hotel_pred_df[ , -which(names(Hotel_pred_df) %in% "Reviewer_Nationality")]


# Splitting into train and test
set.seed(12345)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(Hotel_pred_df))

## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(Hotel_pred_df)), size = smp_size)

train <- Hotel_pred_df[train_ind, ]
test <- Hotel_pred_df[-train_ind, ]

##Removing other variables
rm(hotel_cor_details,Hotel_Reviews, hotel_tags, review_details, reviewer_Nat, weather, weather_base_info, weather_info, weather1, weather2, weather3, weather4, weather5, weather6, weather7, cust_tags, smp_size, train_ind)
