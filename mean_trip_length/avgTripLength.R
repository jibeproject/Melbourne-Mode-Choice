library(tidyverse)
library(fastDummies)
library(sf)


### Clear memory ###
rm(list = ls())


### load data ###
# # Preliminary step: run 'JIBE_Melbourne_Mode_Choice.qmd', and save 'trips' as './trips.rmd'
# and persons (survey$P) as './persons.rmd'
# saveRDS(trips, "./trips.rmd")
# saveRDS(survey$P, "./persons.rmd")

trips <- readRDS("./trips.rmd")
persons <- readRDS("./persons.rmd")

# filter trips to Greater Melbourne
melbSA1s <- st_read("./SA1_2016_AUST_MEL.shp")
trips <- trips %>%
  filter(origSA1 %in% melbSA1s$SA1_MAIN16 | destSA1 %in% melbSA1s$SA1_MAIN16)


### average trip length by purpose by person category ###

# calculate number of adults per household, from the 'persons' file
hh.adults <- persons %>%
  filter(age >= 18) %>%
  group_by(hhid) %>%
  summarise(adults = n()) %>%
  ungroup()

trips <- trips %>%
  left_join(hh.adults, by = c("hhid.x" = "hhid")) %>%
  # adults shouldn't be greater than household size, but if it is, use household size
  mutate(adults = ifelse(adults > hhsize, hhsize, adults)) %>%
  filter(!is.na(adults)) %>%
  mutate(personCategory = case_when(cars == 0 ~ "noCar",
                                    cars/adults < 1 ~ "autosPerAdult_under1",
                                    cars/adults >= 1 ~ "autosPerAdult_over1"))

# # alternative version of 'trips' which takes account of all vehicles
# trips <- trips %>%
#   left_join(hh.adults, by = c("hhid.x" = "hhid")) %>%
#   # adults shouldn't be greater than household size, but if it is, use household size
#   mutate(adults = ifelse(adults > hhsize, hhsize, adults)) %>%
#   filter(!is.na(adults)) %>%
#   mutate(personCategory = case_when(
#     totalvehs == 0 ~ "noCar",
#     totalvehs/adults < 1 ~ "autosPerAdult_under1",
#     totalvehs/adults >= 1 ~ "autosPerAdult_over1"
#   ))

# checking numbers of trips by people in each category
tripCars <- trips %>%
  group_by(personCategory) %>%
  summarise(n = n(),
            pct = n / nrow(trips) * 100)

# checking mean distances and mode used for trips by 'noCar' people
tripNocarMode <- trips %>%
  filter(personCategory == "noCar") %>%
  group_by(full_purpose, linkmode) %>%
  summarise(n = n(), triplength = mean(cumdist))

tripLength <- trips %>%
  # remove zero-length trips
  filter(cumdist > 0) %>%
  # remove modes other than vehicle/pt/walking/bike
  filter(linkmode %in% c("Vehicle Driver", "Vehicle Passenger", "Public Bus",
                         "School Bus", "Train", "Tram", "Walking", "Bicycle")) %>%
  # # optional step to remove trips where mode is 'Vehicle Driver' but household has no cars
  # filter(!(linkmode == "Vehicle Driver" & personCategory == "noCar")) %>%
  group_by(full_purpose, personCategory) %>%
  summarise(avgTripLength = mean(cumdist, na.rm = T)) %>%
  spread(personCategory, avgTripLength)

ggplot(trips) +
  geom_density(aes(x = cumdist, group = personCategory, color = personCategory)) +
  facet_wrap(.~full_purpose, nrow  = 4) +
  xlim(0,20)

write_csv(tripLength, "./mean_trip_length/avgTripLength.csv")
