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
library(conflicted) # to explicitly handle Tidyverse conflicts https://stackoverflow.com/a/75058976
library(tidyverse)
library(fastDummies)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
```

## Classification of trips

Trips will be classified using a standard abbreviation schema, as
advised by Dr Qin Zhang \[reference?\]

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

### Check data

``` r
# Person identifier must be unique (it is)
stopifnot(!any(duplicated(survey$P$persid)))

# Trip identifier must be unique (it is not)
table(duplicated(survey$T$tripid))

# Make Trip identifier unique
survey$T <- survey$T %>% unite("trip.ID",c("persid","tripid"), sep = '', na.rm = TRUE, remove = FALSE)%>% relocate(trip.ID, .after = tripid)

# Derived trip identifier must be unique (it is not!!!)
table(duplicated(survey$T$trip.ID))
```

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
##  [9] tibble_3.2.1      ggplot2_3.5.1     tidyverse_2.0.0   conflicted_1.2.0 
## 
## loaded via a namespace (and not attached):
##  [1] bit_4.0.5         gtable_0.3.5      jsonlite_1.8.8    crayon_1.5.3     
##  [5] compiler_4.4.1    renv_1.0.7        tidyselect_1.2.1  parallel_4.4.1   
##  [9] scales_1.3.0      yaml_2.3.10       fastmap_1.2.0     R6_2.5.1         
## [13] generics_0.1.3    knitr_1.48        munsell_0.5.1     pillar_1.9.0     
## [17] tzdb_0.4.0        rlang_1.1.4       utf8_1.2.4        stringi_1.8.4    
## [21] cachem_1.1.0      xfun_0.47         bit64_4.0.5       timechange_0.3.0 
## [25] memoise_2.0.1     cli_3.6.3         withr_3.0.1       magrittr_2.0.3   
## [29] digest_0.6.37     grid_4.4.1        vroom_1.6.5       rstudioapi_0.16.0
## [33] hms_1.1.3         lifecycle_1.0.4   vctrs_0.6.5       evaluate_0.24.0  
## [37] glue_1.7.0        fansi_1.0.6       colorspace_2.1-1  rmarkdown_2.28   
## [41] tools_4.4.1       pkgconfig_2.0.3   htmltools_0.5.8.1
```

[^1]: Victorian Government Department of Transport. 2022. Victorian
    Integrated Survey of Travel and Activity 2012-2020.
    https://www.vic.gov.au/victorian-integrated-survey-travel-and-activity
