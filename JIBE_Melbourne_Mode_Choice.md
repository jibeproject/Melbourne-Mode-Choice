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

It expands on an earlier mode choice analysis demonstration using the
VISTA travel survey using the [Rapid Realisting Routing with R5
(r5r)](https://ipeagit.github.io/r5r/) routing engine, conducted by Carl
Higgs for the JIBE project
(https://github.com/carlhiggs/multimodal_analysis_example) in March 2022
on behalf of Dr Tayebeh Saghapour. This was subsequently adapted by Dr
Alan Both (https://github.com/jibeproject/odCalculationsMelbourne) for
the JIBE project in February 2024.

The purpose of this analysis is to create appropriate data to inform
travel demand generation using the [Microscopic Transport Orchestrator
(MITO)](https://www.mos.ed.tum.de/tb/forschung/models/travel-demand/mito/)
model by Dr Qin Zhang, [transport simulation
modelling](https://github.com/jibeproject/matsim-jibe) by Corin Staves,
and [microsimulation of downstream health
impacts](https://github.com/jibeproject/health_microsim) related to mode
choice and environmental exposures by Dr Belen Zapata-Diomedi, Dr Ali
Abbas and Steve Pemberton.

## System information

The following information records the version of R used for this
analysis:

``` r
version
```

                   _                                
    platform       x86_64-w64-mingw32               
    arch           x86_64                           
    os             mingw32                          
    crt            ucrt                             
    system         x86_64, mingw32                  
    status                                          
    major          4                                
    minor          4.1                              
    year           2024                             
    month          06                               
    day            14                               
    svn rev        86737                            
    language       R                                
    version.string R version 4.4.1 (2024-06-14 ucrt)
    nickname       Race for Your Life               

## Classification of trips

Trips will be classified using an a standard abbreviation schema, as
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

[^1]: Victorian Government Department of Transport. 2022. Victorian
    Integrated Survey of Travel and Activity 2012-2020.
    https://www.vic.gov.au/victorian-integrated-survey-travel-and-activity
