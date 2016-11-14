##############################################################################################################
##*******************************Random Forest Data Preparation for final Model*******************************
##1.fetaures from model 2- Random forest
##2. Features from Model -1 imp features
##############################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Please install mentioned R- packages before running the script
# install.packages("lubridate") # used for the manipuating time_data
# install.packages("dplyr")# used for data management
# install.packages("vegan") # Use to calculate the jaccard Similarity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Configure followinng variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dataPath <- "/home/smita/MP/train_all_2014.csv"
## Below file contains features/PCs created using principal Component Algorithm
destPCpath <- "/home/smita/MP/destination_pc.csv"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exp_Data = read.csv(dataPath) # small size for Prototyping
exp_Data$year <- NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create New feature from date_time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exp_time <- strptime(exp_Data$date_time, format = "%Y-%m-%d %H:%M:%S", tz="GMT")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  year, month, day, hour
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exp_Data$year = as.numeric(format(exp_time, "%Y"))
exp_Data$month = as.numeric(format(exp_time, "%m"))
exp_Data$day = as.numeric(format(exp_time, "%d"))
exp_Data$hour = as.numeric(format(exp_time, "%H"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# which part of the day? morning, noon, eve or night
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

exp_Data$part_of_day = factor(NA,levels=c('morning','noon', 'evening', 'night'))
exp_Data$part_of_day[exp_Data$hour>5 | exp_Data$hour<=12] <- 'morning'
exp_Data$part_of_day[exp_Data$hour>12 | exp_Data$hour<=17] <- 'noon'
exp_Data$part_of_day[exp_Data$hour>17 | exp_Data$hour<=21] <- 'evening'
exp_Data$part_of_day[exp_Data$hour>21 | exp_Data$hour<=4] <- 'night'

rm(exp_time)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# what type of day ? weekday or weekend
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#install.packages("lubridate")
library(lubridate)

exp_Data$type_of_day <- as.POSIXlt(exp_Data$date_time)$wday
exp_day = exp_Data$type_of_day
exp_Data$type_of_day = factor(NA,levels=c('weekday','weekend'))
exp_Data$type_of_day[exp_day > 0 & exp_day <= 5] <- 'weekday'
exp_Data$type_of_day[exp_day == 0 | exp_day == 6] <- 'weekend'
rm(exp_day)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create meteorological seasons: Spring, summer, fall or winter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exp_month  = exp_Data$month
exp_Data$season = factor(NA,levels=c('spring','summer', 'fall', 'winter'))
exp_Data$season[exp_month >=3 & exp_month < 6] <- 'spring'
exp_Data$season[exp_month >=6 & exp_month < 9] <- 'summer'
exp_Data$season[exp_month >=9 & exp_month < 12] <- 'fall'
exp_Data$season[exp_month == 12 | (exp_month >=1 & exp_month <3)] <- 'winter'
rm(exp_month)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create duration_of_stay from srch_ci and srch_co
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exp_ci <- strptime(exp_Data$srch_ci, format = "%Y-%m-%d", tz="GMT")
exp_co <- strptime(exp_Data$srch_co, format = "%Y-%m-%d", tz="GMT")
exp_Data$duration_of_stay = difftime(exp_co, exp_ci, units = 'day')
rm(exp_co, exp_ci)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create binary feature : is_alone
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

exp_Data$is_alone = factor(NA, levels = c(0, 1))
member_cnt = exp_Data$srch_adults_cnt + exp_Data$srch_children_cnt
exp_Data$is_alone[member_cnt > 1] <- 0
exp_Data$is_alone[member_cnt == 1] <- 1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Select necessary Columns from original Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
names(exp_Data)
column_var = c ("X", "hotel_cluster", "user_id", "is_mobile", "is_package", "is_booking", "is_alone", 
                "hotel_continent", "hotel_country", "hotel_market", "orig_destination_distance",
                "srch_destination_id", "srch_destination_type_id", "srch_rm_cnt",
                "user_location_country", "user_location_region", "user_location_city",
                "year", "month", "day", "hour", "part_of_day", "type_of_day", "season", "duration_of_stay")

#install.packages("dplyr")
library(dplyr)
expData_dat1 = subset(exp_Data, select = column_var)
rm(exp_Data) # free memory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load destinbation PC features
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dest_feature = read.csv(destPCpath) 
colnames(dest_feature)[5] = "srch_destination_id"  # name is expData$srch_destination_id so renamed it
expData_dat1 <- merge(expData_dat1, dest_feature, by=c("srch_destination_id"), all.x = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Write data for training Random forest 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.csv(expData_dat1, "/home/smita/RF_models/dataRF4.csv")
print("Done!")
