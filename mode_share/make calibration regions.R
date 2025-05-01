library(tidyverse)
library(sf)

# clear memory
rm(list = ls())

# load SA1s and LGAs
LGAs <- st_read("./AD_LGA_AREA_POLYGON.shp")
SA1s <- st_read("./SA1_2016_AUST_MEL.shp") %>%
  st_transform(st_crs(LGAs))

# intersect SA1 centroids with LGAs, and add region
calibrationRegions <- SA1s %>%
  st_centroid() %>%
  st_join(LGAs, join = st_intersects) %>%
  # manual adjustment for 3 coastal SA1s where the centroid is outside the LGA boundary
  mutate(NAME = case_when(
    SA1_MAIN16 %in% c("20605113038", "20605113325") ~ "PORT PHILLIP",
    SA1_MAIN16 == "20801117237"                     ~ "BAYSIDE",
    TRUE                                            ~ NAME
  )) %>%
  # allocate to region based on LGA
  mutate(calibrationRegion = case_when(
    NAME %in% c("MARIBYRNONG", "MELBOURNE", "PORT PHILLIP",
                   "STONNINGTON", "YARRA") ~ "Inner Melbourne",
    NAME %in% c("BANYULE", "BAYSIDE", "BOROONDARA", "BRIMBANK", 
                   "DAREBIN", "GLEN EIRA", "GREATER DANDENONG", 
                   "HOBSONS BAY",  "KINGSTON", 
                   "MANNINGHAM", "MERRI-BEK", "MONASH", "MOONEE VALLEY", 
                   "WHITEHORSE") ~ "Middle Melbourne",
    TRUE ~ "Outer Melbourne"
   )) %>%
  mutate(LGA = case_when(
    NAME == "MERRI-BEK" ~ "Merri-bek",
    TRUE                ~ str_to_title(NAME))
  ) %>%
  # keep required details
  st_drop_geometry() %>%
  dplyr::select(SA1_MAIN16, LGA, calibrationRegion)

# write output
write.csv(calibrationRegions, "./mode_share/calibrationRegions.csv", row.names = F)
