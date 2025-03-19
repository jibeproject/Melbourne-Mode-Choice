options(digits = 15)


######################## SETUP ######################## 
library(tidyverse)
library(sf)
rm(list = ls())


# Read model data
# Preliminary step: run 'JIBE_Melbourne_Mode_Choice.qmd', which will output 'trips_with_purpose.rds'.

trips <- readRDS("./trips_with_purpose.rds") %>%
  filter(purpose != "NA") %>%
  mutate(purpose = factor(purpose))

# filter to Greater Melbourne
read_zipped_GIS <- function(zipfile, subpath = "", file = NULL, layer = NULL) {
  temp <- tempfile()
  unzip(zipfile, exdir = temp)
  if (is.null(layer)) {
    st_read(paste0(temp, subpath, file))
  } else {
    st_read(paste0(temp, subpath, file), layer)
  }
}

melbSA1s <- read_zipped_GIS(zipfile = "./1270055001_sa1_2016_aust_shape.zip",
                            file = "/SA1_2016_AUST.shp") %>%
  filter(GCC_NAME16 == "Greater Melbourne")

trips <- trips %>%
  filter(origSA1 %in% melbSA1s$SA1_MAIN16 & destSA1 %in% melbSA1s$SA1_MAIN16)


######################## TIME OF DAY  ######################## 
timeOfDayTable <- data.frame(minute = 1:1440)
densityData <- list()
homeBasedPurposes <- c("HBW","HBE","HBS","HBR","HBO","HBA")
arrivalTimePurposes <- c("HBW","HBE","HBS","HBR","HBO","HBA","NHBW","NHBO")

# Note: the above lists of purposes omit 'business', 'RRT' or 'unknown'.


# Prepare activity duration data
homeBasedTrips <- readRDS("./trips_with_purpose.rds") %>% 
  filter(origSA1 %in% melbSA1s$SA1_MAIN16 & destSA1 %in% melbSA1s$SA1_MAIN16) %>%
  filter(full_purpose %in% homeBasedPurposes) %>% 
  mutate(activityDuration = as.numeric(duration)) %>%
  filter(!is.na(activityDuration),
         activityDuration <= 1440,
         activityDuration > 0)


# Initial plots
ggplot(filter(trips, purpose %in% arrivalTimePurposes), aes(x = arrtime, colour = purpose)) + 
  geom_density(adjust = 1) + scale_x_continuous(breaks = seq(0,1440,180), minor_breaks = seq(0,1440,60)) + 
  ggtitle("Activity arrival times") + labs(colour = "purpose", x = "arrival time (minutes from midnight)") 
ggplot(homeBasedTrips, aes(x = activityDuration, colour = purpose)) + 
  geom_density() + scale_x_continuous(breaks = seq(0,1440,180), minor_breaks = seq(0,1440,60)) + 
  ggtitle("Activity durations") + labs(colour = "purpose", x = "minutes") 


# Create arrival time table
for(purpose in arrivalTimePurposes) {
  data <- trips[trips$purpose == purpose,]
  col_name = paste0("arrival_",purpose)
  # you can set the "adjust" parameter to define the smoothing bandwidth, the higher the smoother curve. 
  # we want the curve not too sharp
  densityData[[col_name]] <- density(data$arrtime, kernel = "gaussian",adjust = 3,from = 1,to = 1440, n = 1440)
  timeOfDayTable[[col_name]] <- densityData[[col_name]][["y"]]
}


# Create duration table
for(purpose in homeBasedPurposes) {
  data <- homeBasedTrips[homeBasedTrips$purpose == purpose,]
  col_name = paste0("duration_",purpose)
  densityData[[col_name]] <- density(data$activityDuration, kernel = "gaussian",from = 1,to = 1440, n = 1440)
  timeOfDayTable[[col_name]] <- densityData[[col_name]][["y"]]
}


# Plot table results to check
timeOfDayTable %>%
  pivot_longer(cols = starts_with("arrival")) %>%
  ggplot(aes(x = minute, y = value, colour = name)) + geom_line()

timeOfDayTable %>%
  pivot_longer(cols = starts_with("duration")) %>%
  ggplot(aes(x = minute, y = value, colour = name)) + geom_line()


######################## WRITE OUTPUTS ######################## 
write.csv(timeOfDayTable, file = "./time_of_day/timeOfDay.csv", quote = FALSE, row.names = FALSE)
