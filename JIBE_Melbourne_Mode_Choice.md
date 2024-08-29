# JIBE Melbourne Mode Choice Preparation and Analysis


## Background

To conduct the JIBE mode choice modelling for Melbourne requires data
preparation using travel survey data. The analysis draws on the
[Victorian Integrated Survey of Travel and
Activity](https://www.vic.gov.au/victorian-integrated-survey-travel-and-activity)
(VISTA[^1]) for 2012-20, used with permission from the Victorian
Department of Transport.

The analysis is conducted for the JIBE project by Carl Higgs in
August/September 2024, in coordination with Dr Qin Zhang, Corin Staves
and Dr Belen Zapata-Diomedi.

It expands on an [earlier mode choice
analysis](https://github.com/carlhiggs/multimodal_analysis_example)
demonstration using the VISTA travel survey using the [Rapid Realisting
Routing with R5 (r5r)](https://ipeagit.github.io/r5r/) routing engine,
conducted by Carl Higgs for the JIBE project in March 2022 on behalf of
Dr Tayebeh Saghapour. This was subsequently
[adapted](https://github.com/jibeproject/odCalculationsMelbourne) by Dr
Alan Both for the JIBE project in February 2024.

The purpose of this analysis is to create appropriate data to inform
travel demand generation using the [Microscopic Transport Orchestrator
(MITO)](https://www.mos.ed.tum.de/tb/forschung/models/travel-demand/mito/)
model by Dr Qin Zhang, [transport simulation
modelling](https://github.com/jibeproject/matsim-jibe) by Corin Staves,
and [microsimulation of downstream health
impacts](https://github.com/jibeproject/health_microsim) related to mode
choice and environmental exposures by Dr Belen Zapata-Diomedi, Dr Ali
Abbas and Steve Pemberton.

A Quarto markdown document
[`JIBE_Melbourne_Mode_Choice.qmd`](JIBE_Melbourne_Mode_Choice.qmd) will
be used to document the analysis and updated as the project progresses.
This shall also be rendered as a GitHub-Flavoured-Markdown document
[`JIBE_Melbourne_Mode_Choice.md`](JIBE_Melbourne_Mode_Choice.md) for
convenient online viewing.

Code (at least at first) will draw on example code prepared by Dr Qin
Zhang and Corin Staves, adapting as required for use with the VISTA
travel survey.

No data or data-related outputs will be included in this document or
repository. By default, output is set to False. For non-sensitive
aspects, e.g. displaying the `sessionInfo()` after running analysis,
this may be over-ridden.

## Status

27 August 2024: commenced, in progress

## System environment set up

``` r
rm(list = ls()) # clear memory
```

Project dependencies are describe in the [`renv.lock`](./renv.lock)
file; see [renv](https://rstudio.github.io/renv/) for more information.
Instructions on installing dependencies using an `renv` lock file are
[here](https://rstudio.github.io/renv/articles/renv.html#installing-packages).

``` r
library(conflicted) # package conflict handling https://stackoverflow.com/a/75058976
library(janitor) # data cleaning
library(knitr) # presentation
library(tidyverse) # data handling
library(fastDummies) # binary dummy variable utility

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
```

## Classification of trips

Trips will be classified using a standard abbreviation schema, as
advised by Dr Qin Zhang and Corin Staves [^2] [^3] [^4]

| Classification | Description                                                                  |
|:---------------|:-----------------------------------------------------------------------------|
| HBW            | Home based work                                                              |
| HBE            | Home based education                                                         |
| HBA            | Home based accompanying/escort trip                                          |
| HBS            | Home based shopping                                                          |
| HBR            | Home based recreational                                                      |
| HBO            | Home based other (e.g. health care, religious activity, visit friend/family) |
| NHBW           | Non-home based work (e.g. from workplace to restaurant)                      |
| NHBO           | Non-home based other (e.g. from supermarket to restaurant)                   |
| RRT            | Round Trip                                                                   |

## Data preparation

### Read data

``` r
data <- c(
  'H_VISTA_1220_Coord.csv',
  'JTE_VISTA_1220_Coord.csv',
  'JTW_VISTA_1220_Coord.csv',
  'P_VISTA_1220_Coord.csv',
  'S_VISTA_1220_Coord.csv',
  'T_VISTA_1220_Coord.csv'
)
survey<-list()
for (d in data) {
  survey[[sapply(strsplit(d,split='_',1),`[`,1)]]<-read_csv(glue::glue('../../{d}')) 
}
survey
```

### Check data, ensuring that dataset IDs are unique

``` r
# Person identifier must uniquely identify persons (it does; there are 82,118 unique persons)
table(duplicated(survey$P$persid))
stopifnot(!any(duplicated(survey$P$persid)))

# Trip identifier must uniquely identify trips (it does not)
table(duplicated(survey$T$tripid))

# Look at duplicate tripids (4,104 are NA)
survey$T[duplicated(survey$T$tripid),]

# I visually checked the CSV file and confirmed that these rows just consist of commas
# This can occur if someone exports a CSV from Excel that inadvertently includes blank rows
# I am surprised that the read csv default 'skip_empty_rows = TRUE' did not deal with this.

# Omit NA tripid rows from tripid dataset
survey$T <- survey$T %>% filter(!sapply(tripid, is.na))

# Trip id must be unique  (now, it is; there are 221,819 unique trips)
stopifnot(!any(duplicated(survey$T$tripid)))

# Household ID must uniquely identify households (it does; there are 32,133 households)
table(duplicated(survey$H$hhid))
stopifnot(!any(duplicated(survey$H$hhid)))
```

### Merge data on trips, persons and households

Left joining households to persons, and left joining that to the trip
dataset so we have trips\>persons\>households, respectively joined using
their unique identifiers.

``` r
trips <- survey$T %>% left_join(survey$P  %>% left_join(survey$H, by='hhid'), by='persid')
```

### List columns present in the merged Trip dataset

``` r
trips %>% colnames()
##   [1] "tripid"             "persid"             "hhid.x"            
##   [4] "stops"              "tripno"             "starthour"         
##   [7] "startime"           "arrhour"            "arrtime"           
##  [10] "triptime"           "travtime"           "waitime"           
##  [13] "duration"           "cumdist"            "origplace1"        
##  [16] "origplace2"         "origpurp1"          "origpurp2"         
##  [19] "destplace1"         "destplace2"         "destpurp1"         
##  [22] "destpurp2"          "origsa1"            "origsa2"           
##  [25] "origsa2_name"       "origsa3"            "origsa3_name"      
##  [28] "origsa4"            "origsa4_name"       "origlga"           
##  [31] "destsa1"            "destsa2"            "destsa2_name"      
##  [34] "destsa3"            "destsa3_name"       "destsa4"           
##  [37] "destsa4_name"       "destlga"            "trippurp"          
##  [40] "linkmode"           "dist1"              "dist2"             
##  [43] "dist3"              "dist4"              "dist5"             
##  [46] "dist6"              "dist7"              "dist8"             
##  [49] "dist9"              "dist10"             "mode1"             
##  [52] "mode2"              "mode3"              "mode4"             
##  [55] "mode5"              "mode6"              "mode7"             
##  [58] "mode8"              "mode9"              "mode10"            
##  [61] "time1"              "time2"              "time3"             
##  [64] "time4"              "time5"              "time6"             
##  [67] "time7"              "time8"              "time9"             
##  [70] "time10"             "wdtripwgt_sa3"      "wetripwgt_sa3"     
##  [73] "wdtripwgt_lga"      "wetripwgt_lga"      "origmb"            
##  [76] "origlat"            "origlong"           "destmb"            
##  [79] "destlat"            "destlong"           "hhid.y"            
##  [82] "persno"             "numstops"           "monthofbirth"      
##  [85] "yearofbirth"        "age"                "sex"               
##  [88] "relationship"       "persinc"            "carlicence"        
##  [91] "mbikelicence"       "otherlicence"       "nolicence"         
##  [94] "fulltimework"       "parttimework"       "casualwork"        
##  [97] "anywork"            "studying"           "activities"        
## [100] "mainact"            "worktype"           "emptype"           
## [103] "anzsco1"            "anzsco2"            "anzsic1"           
## [106] "anzsic2"            "startplace"         "additionaltravel"  
## [109] "cycledwork"         "cycledshopping"     "cycledexercise"    
## [112] "cycledother"        "nocycled"           "wdperswgt_sa3"     
## [115] "weperswgt_sa3"      "wdperswgt_lga"      "weperswgt_lga"     
## [118] "surveyperiod"       "travdow"            "travmonth"         
## [121] "daytype"            "owndwell"           "hhsize"            
## [124] "hhinc"              "visitors"           "aveage"            
## [127] "youngest"           "oldest"             "yearslived"        
## [130] "monthslived"        "adultbikes"         "kidsbikes"         
## [133] "totalbikes"         "cars"               "fourwds"           
## [136] "utes"               "vans"               "trucks"            
## [139] "mbikes"             "othervehs"          "totalvehs"         
## [142] "wdhhwgt_sa3"        "wehhwgt_sa3"        "wdhhwgt_lga"       
## [145] "wehhwgt_lga"        "homesa1"            "homesa2"           
## [148] "homesa2_name"       "homesa3"            "homesa3_name"      
## [151] "homesa4"            "homesa4_name"       "homelga"           
## [154] "homesubregion_asgc" "homeregion_asgc"    "homesubregion_asgs"
## [157] "homeregion_asgs"    "homepc"             "homemb"            
## [160] "centroid_long"      "centroid_lat"
```

## Assign trip purpose

### Review trip variables relevant to trip purpose

The example TRADS analysis conducted by Dr Qin Zhang (not included in
this repo) derived trip purpose through classification of origin and
destination categories.

The VISTA Trips dataset variables (see
[dictionary](./dictionaries/T_VISTA_1220%20_data_dictionary_machine_readable.csv))
has this information in the `origplace1` (‘Origin Place Type (Summary)’)
and `destplace1` (‘Destination Place Type (Summary)’) variables. The
variables `origplace2` and `destplace2` contain more detail if this is
required for any downstream analyses.

``` r
places<- full_join(
  trips$origplace1%>%replace_na('NA')%>%table()%>%sort(decreasing=TRUE,na.last=TRUE)%>%as.data.frame()%>%`colnames<-`(c('Place','Origins (n)')),
  trips$destplace1%>%replace_na('NA')%>%table()%>%sort(decreasing=TRUE,na.last=TRUE)%>%as.data.frame()%>%`colnames<-`(c('Place','Destinations (n)'))
  )%>%adorn_totals()
kable(places)
```

| Place                      | Origins (n) | Destinations (n) |
|:---------------------------|------------:|-----------------:|
| Accommodation              |      101767 |           103321 |
| Shops                      |       28722 |            28742 |
| Workplace                  |       27808 |            27450 |
| Place of Education         |       20481 |            20609 |
| Social Place               |       12442 |            12468 |
| Recreational Place         |       12333 |            12380 |
| Place of Personal Business |        8299 |             8350 |
| Natural Feature            |        4320 |             4440 |
| Other                      |        3356 |             1622 |
| Transport Feature          |        2289 |             2436 |
| NA                         |           1 |               NA |
| Not Stated                 |           1 |                1 |
| Total                      |      221819 |           221819 |

In the above, ‘Accommodation’ may be thought of as ‘Home’.

There was one trip record missing an origin category (`origplace1` and
`origplace2` both as NA), with destination of Accommodation (i.e. home).

``` r
 trips[is.na(trips$origplace1),][c('origplace1','destplace1','origpurp1','destpurp1')]
```

There were coordinates recorded for this record (manual check), and the
Vista data contained ‘Purpose at Start of Trip Stage (Summary)’
(`origpurp1`) as “Work related” and ‘Purpose at End of Trip (Summary)’
(`destpurp1`) as ‘Go home’. Following the TRADS example, this means, for
derived purpose purposes this record would be an NA (i.e. return trip)
and for derived full purpose, a home-based work trip.

As noted above, VISTA also has origin and destination trip purpose
summary variables recorded, `origpurp1` and `destpurp1`. My
understanding, beyond the data dictionary definitions in the above
paragraph, is that these provide the reason for being at the origin, and
the reason for going to the destination. Hence in the above example,
even though the above was an NA for origin (for whatever reason) it is
to be interpreted as ‘Work’.

Here are the list of given trip start (origin) and end (destination)
purposes:

``` r
purpose<- full_join(
  trips$origpurp1%>%replace_na('NA')%>%table()%>%sort(decreasing=TRUE,na.last=TRUE)%>%as.data.frame()%>%`colnames<-`(c('Purpose','Start (n)')),
  trips$destpurp1%>%replace_na('NA')%>%table()%>%sort(decreasing=TRUE,na.last=TRUE)%>%as.data.frame()%>%`colnames<-`(c('Purpose','End (n)'))
  )%>%adorn_totals()
kable(purpose)
```

| Purpose                           | Start (n) | End (n) |
|:----------------------------------|----------:|--------:|
| At Home                           |     87031 |      NA |
| Work Related                      |     28465 |   28485 |
| Buy Something                     |     23742 |   23748 |
| Social                            |     22758 |   22950 |
| Pick-up or Drop-off Someone       |     15347 |   15372 |
| Recreational                      |     11533 |   11631 |
| Personal Business                 |     11009 |   12672 |
| Education                         |      8344 |    8365 |
| Accompany Someone                 |      7623 |    7725 |
| Pick-up or Deliver Something      |      3120 |    3122 |
| Unknown Purpose (at start of day) |      1832 |      NA |
| Other Purpose                     |       998 |     697 |
| Change Mode                       |        12 |     142 |
| Not Stated                        |         4 |       4 |
| NA                                |         1 |       1 |
| At or Go Home                     |        NA |   86905 |
| Total                             |    221819 |  221819 |

There were recorded instances of NA for one origin and one destination
trip purpose.

``` r
trips[is.na(trips$origpurp1)|is.na(trips$destpurp1),][c('origplace1','destplace1','origpurp1','destpurp1')]
```

One of these had a recorded origin of ‘Transport Feature’ (purpose: to
‘Pick-up or Drop-off Someone’) with destination of Workplace (purpose:
NA). Arguably if you’re going into work, that’s a work-related trip.
Perhaps there are grey areas (e.g. its your day off and you’re picking
up the sandwich you left in the fridge), but it would be consistent with
TRADS to interpret this as a ‘Non-Home Based Work Trip’.

The other is similar: from Workplace (purpose NA) to Place of Education
(purpose: Education). That’s a ‘Non-home based work’ trip’ (there isn’t
a non-home based education trip in the above classification schema, so
the choice is easy).

***The question is**: do we derive trip purpose using destinations like
in the TRADS example, or do we adapt the given VISTA purposes (and
perhaps, just the destination purpose as that’s really the reason for
the trip)?*

For now, I think I will derive a MITO purpose using origin, destination
and destination purpose. We can always revise the classification
mapping.

### Further trip cleaning prior to assignment, based on above review

To ensure proper coding, `origplace1` will be recorded as work related
(**for now, pending review with colleagues**)

``` r
trips[is.na(trips$origplace1) & trips$origpurp1=='Work Related','origplace1'] <- 'Workplace'
```

Also, for now we will replace these respective NA values as ‘workplace’
(will run this past colleagues later for thoughts on this) to ensure we
have the appropriate fields in order to categorise as NHBW, later.

``` r
trips[is.na(trips$destpurp1) & trips$destplace1=='Workplace','destpurp1'] <- 'Work Related'
trips[is.na(trips$origpurp1) & trips$origplace1=='Workplace','origpurp1'] <- 'Work Related'
```

Revised origin and destinations, following cleaning:

``` r
places<- full_join(
  trips$origplace1%>%replace_na('NA')%>%table()%>%sort(decreasing=TRUE,na.last=TRUE)%>%as.data.frame()%>%`colnames<-`(c('Place','Origins (n)')),
  trips$destplace1%>%replace_na('NA')%>%table()%>%sort(decreasing=TRUE,na.last=TRUE)%>%as.data.frame()%>%`colnames<-`(c('Place','Destinations (n)'))
  )%>%adorn_totals()
kable(places)
```

| Place                      | Origins (n) | Destinations (n) |
|:---------------------------|------------:|-----------------:|
| Accommodation              |      101767 |           103321 |
| Shops                      |       28722 |            28742 |
| Workplace                  |       27809 |            27450 |
| Place of Education         |       20481 |            20609 |
| Social Place               |       12442 |            12468 |
| Recreational Place         |       12333 |            12380 |
| Place of Personal Business |        8299 |             8350 |
| Natural Feature            |        4320 |             4440 |
| Other                      |        3356 |             1622 |
| Transport Feature          |        2289 |             2436 |
| Not Stated                 |           1 |                1 |
| Total                      |      221819 |           221819 |

Revised purposes, following cleaning:

``` r
purpose<- full_join(
  trips$origpurp1%>%replace_na('NA')%>%table()%>%sort(decreasing=TRUE,na.last=TRUE)%>%as.data.frame()%>%`colnames<-`(c('Purpose','Start (n)')),
  trips$destpurp1%>%replace_na('NA')%>%table()%>%sort(decreasing=TRUE,na.last=TRUE)%>%as.data.frame()%>%`colnames<-`(c('Purpose','End (n)'))
  )%>%adorn_totals()
kable(purpose)
```

| Purpose                           | Start (n) | End (n) |
|:----------------------------------|----------:|--------:|
| At Home                           |     87031 |      NA |
| Work Related                      |     28466 |   28486 |
| Buy Something                     |     23742 |   23748 |
| Social                            |     22758 |   22950 |
| Pick-up or Drop-off Someone       |     15347 |   15372 |
| Recreational                      |     11533 |   11631 |
| Personal Business                 |     11009 |   12672 |
| Education                         |      8344 |    8365 |
| Accompany Someone                 |      7623 |    7725 |
| Pick-up or Deliver Something      |      3120 |    3122 |
| Unknown Purpose (at start of day) |      1832 |      NA |
| Other Purpose                     |       998 |     697 |
| Change Mode                       |        12 |     142 |
| Not Stated                        |         4 |       4 |
| At or Go Home                     |        NA |   86905 |
| Total                             |    221819 |  221819 |

We have now imputed values for the NA places and purposes.

### Mapping broad purpose alignments

#### HBW Home based work

#### HBE Home based education

#### HBA Home based accompanying/escort trip

#### HBS Home based shopping

#### HBR Home based recreational

#### HBO Home based other

(e.g. health care, religious activity, visit friend/family)

#### NHBW Non-home based work

(e.g. from workplace to restaurant)

#### NHBO Non-home based other

(e.g. from supermarket to restaurant)

#### RRT Round Trip

## Attach travel time

## Filter out invalid trips records

## Deal with intrazonal trips

For Manchester there were a substantial number of intrazonal trips,
having origin and destination are recorded with the same output area
(OA).

For Melbourne, we need to check if intrazonal trips exist, sharing
identical origin and destination coordinates.

## Write to CSV

## MORE ANALYSIS TO FOLLOW; THIS IS NOT COMPLETE

## System information for the above analysis

The following information records the version of R used for this
analysis:

``` r
sessionInfo()
## R version 4.4.1 (2024-06-14 ucrt)
## Platform: x86_64-w64-mingw32/x64
## Running under: Windows 10 x64 (build 19045)
## 
## Matrix products: default
## 
## 
## locale:
## [1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8   
## [3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                      
## [5] LC_TIME=English_Australia.utf8    
## 
## time zone: Australia/Sydney
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices datasets  utils     methods   base     
## 
## other attached packages:
##  [1] fastDummies_1.7.4 lubridate_1.9.3   forcats_1.0.0     stringr_1.5.1    
##  [5] dplyr_1.1.4       purrr_1.0.2       readr_2.1.5       tidyr_1.3.1      
##  [9] tibble_3.2.1      ggplot2_3.5.1     tidyverse_2.0.0   knitr_1.48       
## [13] janitor_2.2.0     conflicted_1.2.0 
## 
## loaded via a namespace (and not attached):
##  [1] utf8_1.2.4        generics_0.1.3    renv_1.0.7        stringi_1.8.4    
##  [5] hms_1.1.3         digest_0.6.37     magrittr_2.0.3    evaluate_0.24.0  
##  [9] grid_4.4.1        timechange_0.3.0  fastmap_1.2.0     jsonlite_1.8.8   
## [13] fansi_1.0.6       scales_1.3.0      cli_3.6.3         crayon_1.5.3     
## [17] rlang_1.1.4       bit64_4.0.5       munsell_0.5.1     withr_3.0.1      
## [21] cachem_1.1.0      yaml_2.3.10       parallel_4.4.1    tools_4.4.1      
## [25] tzdb_0.4.0        memoise_2.0.1     colorspace_2.1-1  vctrs_0.6.5      
## [29] R6_2.5.1          lifecycle_1.0.4   snakecase_0.11.1  bit_4.0.5        
## [33] vroom_1.6.5       pkgconfig_2.0.3   pillar_1.9.0      gtable_0.3.5     
## [37] glue_1.7.0        xfun_0.47         tidyselect_1.2.1  rstudioapi_0.16.0
## [41] htmltools_0.5.8.1 rmarkdown_2.28    compiler_4.4.1
```

[^1]: Victorian Government Department of Transport. 2022. Victorian
    Integrated Survey of Travel and Activity 2012-2020.
    https://www.vic.gov.au/victorian-integrated-survey-travel-and-activity

[^2]: Ortúzar JdD and Willumsen LG (2011) Modelling Transport.
    Chichester: John Wiley & Sons, Ltd

[^3]: Staves, Corin. (2020). Physical Activity Assessment and Modelling
    using Household Travel Surveys. 10.13140/RG.2.2.14307.84009.

[^4]: Staves C, Zhang Q, Moeckel R, et al. (2023) Integrating health
    effects within an agent-based land use and transport model. J Transp
    Health 33: 101707.
