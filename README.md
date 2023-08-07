# Event-based Buffelgrass Forecast
This repository contains the R code used to generate the USA-NPN's prototyped updated buffelgrass green-up forecast.

## Overview
The R shiny app contained within this repo is designed to generate a forecast for the green-up of buffelgrass in Arizona. This prototype calculates the number of distinct precipitation events in a 30-day window. In contrast, the existing [NPN buffelgrass forecast](https://usanpn.org/data/forecasts/Buffelgrass) uses cumulative preciptation in a 24-day window. Precipitation data used is from [PRISM](http://prism.oregonstate.edu/) (for Rasters) and [RCC-ACIS.](https://www.rcc-acis.org/index.html) (for spatial point data). 

## Rules
This app counts rainfall events. Within it, an event is defined as a calendar day with .25+ inches of precipitation. It extends until the first day with no precipitation. After an event, a buffer of at least 3 days with precipitation below .25 inches must occur in order for the next event to count. For clarification, a buffer will always start on a day with no precipitation, and will include days with no or little (less than .25in) precipitation up until the next day with sufficient precipitation.

## Access
An in-browser instance of the Shiny App can be run at [https://travismatlock.shinyapps.io/buffelgrass_forecast/](https://travismatlock.shinyapps.io/buffelgrass_forecast/). The app is limited to 25 active hours per month.

## Future Developments
There are a number of possible improvements that are being considered for this app. They are as follows:
1. Slider tools to allow for user-controlled timeframes, precipitation threshold, and buffer length.
2. A caching system for the precipitation data to improve the efficiency of the code.

## Author
* **Travis Matlock** - *Coding* - Student intern at [USA-NPN](https://github.com/usa-npn).
