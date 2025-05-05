library(tidyverse)
library(fastDummies)
library(sf)


### Clear memory ###
rm(list = ls())


### load data ###
# # Preliminary step: run 'JIBE_Melbourne_Mode_Choice.qmd', and save 'trips' as './trips.rds'
# saveRDS(trips, "./trips.rds")

trips <- readRDS("./trips_with_purpose.rds")

# filter to Greater Melbourne
melbSA1s <- st_read("./SA1_2016_AUST_MEL.shp")
trips <- trips %>%
  filter(origSA1 %in% melbSA1s$SA1_MAIN16 | destSA1 %in% melbSA1s$SA1_MAIN16)

# add region, based on LGA of trip origin
calibrationRegions <- read.csv("./mode_share/calibrationRegions.csv")
trips <- trips %>%
  left_join(calibrationRegions, by = c("origSA1" = "SA1_MAIN16"))

# # alternative method for adding region when VISTA data omits SA1s
# trips <- trips %>%
#   mutate(calibrationRegion = case_when(
#     origLGA %in% c("Maribyrnong (C)", "Melbourne (C)", "Port Phillip (C)",
#                     "Stonnington (C)", "Yarra (C)") ~ "Inner Melbourne",
#     origLGA %in% c("Banyule (C)", "Bayside (C)", "Boroondara (C)", "Brimbank (C)", 
#                    "Darebin (C)", "Glen Eira (C)", "Greater Dandenong (C)", 
#                    "Hobsons Bay (C)",  "Kingston (C) (Vic.)", 
#                    "Manningham (C)", "Moreland (C)", "Monash (C)", "Moonee Valley (C)", 
#                    "Whitehorse (C)") ~ "Middle Melbourne",
#     TRUE ~ "Outer Melbourne")
#   )


### mode share by purpose and by full_purpose ###
trips <- trips %>%
  mutate(mode = case_when(
    linkmode %in% c("Vehicle Passenger") ~ "autoPassenger",
    linkmode %in% c("Vehicle Driver") ~ "autoDriver",
    linkmode %in% c("Public Bus", "School Bus", "Train", "Tram") ~ "pt",
    linkmode %in% c("Walking") ~ "walk",
    linkmode %in% c("Bicycle") ~ "bicycle"
  ))

# visualisation of mode share by 'purpose'
modeSharePurpose <- trips%>%
  filter(!is.na(purpose)) %>%
  filter(!is.na(mode)) %>%
  group_by(calibrationRegion, purpose, mode) %>%
  summarise(count = n()) %>%
  mutate(share = count/sum(count)) %>%
  mutate(factor = 0)

ggplot(modeSharePurpose) +
  geom_bar(aes(x=calibrationRegion, y=share, 
               group = mode, fill = mode), 
           stat="identity", position="fill") +
  facet_wrap(.~purpose, nrow  = 4)


# visualisation of mode share by 'full_purpose'
modeShareFullPurpose <- trips%>%
  filter(!is.na(full_purpose)) %>%
  filter(!is.na(mode)) %>%
  group_by(calibrationRegion, full_purpose, mode) %>%
  summarise(count = n()) %>%
  mutate(share = count/sum(count))

ggplot(modeShareFullPurpose) +
  geom_bar(aes(x=calibrationRegion, y=share, 
               group = mode, fill = mode), 
           stat="identity", position="fill") +
  facet_wrap(.~full_purpose, nrow  = 4)


# write 'purpose' output (used as mito input)
write_csv(modeSharePurpose,"./mode_share/calibration_initial.csv")
