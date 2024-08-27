# JIBE Melbourne Mode Choice Analysis
Travel survey data preparation to inform JIBE mode choice modelling.

Analysis conducted as an [R Quarto project](https://quarto.org/docs/projects/quarto-projects.html), documented in the [`JIBE_Melbourne_Mode_Choice.md`](./JIBE_Melbourne_Mode_Choice.md) markdown document.

Project dependencies are describe in the [`renv.lock`](./renv.lock) file; see [renv](https://rstudio.github.io/renv/) for more information.

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

## Contributors

Carl Higgs, Qin Zhang, Corin Staves, Belen Zapata-Diomedi


[^1]: Victorian Government Department of Transport. 2022. Victorian
    Integrated Survey of Travel and Activity 2012-2020.
    https://www.vic.gov.au/victorian-integrated-survey-travel-and-activity
