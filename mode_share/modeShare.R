library(tidyverse)
library(fastDummies)
library(sf)


### Clear memory ###
rm(list = ls())


### load data ###
# # Preliminary step: run 'JIBE_Melbourne_Mode_Choice.qmd', and save 'trips' as './trips.rds'
# saveRDS(trips, "./trips.rds")

trips <- readRDS("./trips.rds")

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
  mutate(t.m_main_agg5 = case_when(
    linkmode %in% c("Vehicle Passenger") ~ "autoPassenger",
    linkmode %in% c("Vehicle Driver") ~ "autoDriver",
    linkmode %in% c("Public Bus", "School Bus", "Train", "Tram") ~ "pt",
    linkmode %in% c("Walking") ~ "walk",
    linkmode %in% c("Bicycle") ~ "bicycle"
  ))

modeSharePurpose <- trips%>%
  filter(!is.na(purpose)) %>%
  filter(!is.na(t.m_main_agg5)) %>%
  group_by(calibrationRegion, purpose, t.m_main_agg5) %>%
  summarise(n = n()) %>%
  mutate(share = n/sum(n))

ggplot(modeSharePurpose) +
  geom_bar(aes(x=calibrationRegion, y=share, 
               group = t.m_main_agg5, fill = t.m_main_agg5), 
           stat="identity", position="fill") +
  facet_wrap(.~purpose, nrow  = 4)

write_csv(modeSharePurpose,"./mode_share/mode_share_purpose.csv")

modeShareFullPurpose <- trips%>%
  filter(!is.na(full_purpose)) %>%
  filter(!is.na(t.m_main_agg5)) %>%
  group_by(calibrationRegion, full_purpose, t.m_main_agg5) %>%
  summarise(n = n()) %>%
  mutate(share = n/sum(n))

ggplot(modeShareFullPurpose) +
  geom_bar(aes(x=calibrationRegion, y=share, 
               group = t.m_main_agg5, fill = t.m_main_agg5), 
           stat="identity", position="fill") +
  facet_wrap(.~full_purpose, nrow  = 4)

write_csv(modeShareFullPurpose,"./mode_share/mode_share_full_purpose.csv")

# TO DO: 
# - determine whether 'purpose' or 'full purpose' should be used
# - save the final output as 'calibration_initial.csv' (instead of 
#   'mode_share_purpose.csv' and 'mode_share_full_purpose.csv)
