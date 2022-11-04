library(tidyverse)
library(dplyr)
library(data.table)
#Setting Working Directory
setwd("D:/Courses/Google Data Analytics/Google Data Analytics Capston Project")

#Making File Names List


files <- list.files(pattern = ".csv")
#print(files)

combined_files <- bind_rows(lapply(files, fread))

#write.csv(combined_files,"032021-032022-merged-divvy-tripdata.csv", row.names = FALSE)

colnames(combined_files)
#head(combined_files)

# Rename columns  to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Divvy)
clean_files <- rename(combined_files, 
                                     trip_id = ride_id
                                    ,bikeid = rideable_type 
                                    ,start_time = started_at 
                                    ,end_time = ended_at 
                                    ,from_station_name = start_station_name  
                                    ,from_station_id = start_station_id   
                                    ,to_station_name = end_station_name 
                                    ,to_station_id = end_station_id 
                                    , usertype = member_casual 
)

# Convert ride_id and rideable_type to character so that they can stack correctly

clean_files <- mutate(clean_files, trip_id = as.character(trip_id))

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020

clean_files <- clean_files %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
#How many rows are in data frame?
#nrow(clean_files)

#Dimensions of the data frame?
#dim(clean_files)

#Statistical summary of data. Mainly for numerics
#summary(clean_files)

# (3) We will want to add a calculated field for length of ride since the 2020Q1 
#data did not have the "tripduration" column. We will add "ride_length" 
#to the entire dataframe for consistency.

clean_files <- clean_files %>%
  mutate(clean_files, tripduration = end_time - start_time)

# Check to make sure the proper number of observations were reassigned

#table(clean_files$usertype)
#colnames(clean_files)

# (4) There are some rides where tripduration shows up as negative, 
#including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. 
#We will want to delete these rides.

clean_files <- subset(clean_files, tripduration > 0)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... 
#before completing these operations we could only aggregate at the ride level
all_trips <- clean_files

all_trips$date <- as.Date(all_trips$start_time)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips <- select(all_trips, -ride_length)
# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$end_time,all_trips$start_time)

str(all_trips)


# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)



head(all_trips)

all_trips_v2 <- all_trips[!(all_trips$from_station_name == "HQ QR" | all_trips$ride_length<0),]

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length,) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)


# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)


# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(start_time)) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%							#calculates the number of rides and average duration  		# calculates the average duration
  arrange(usertype, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(start_time)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n() ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(start_time)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n() ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')



                                      
