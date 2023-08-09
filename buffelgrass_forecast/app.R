# Author: Travis Matlock

# This script produces a forecast for buffelgrass green-up based on rainfall events in AZ.

library(raster)
library(rgdal)
library(rnpn)
library(lubridate)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(jsonlite)
library(ggmap)
library(sf)
rm(list=ls())

# Establish today's date and the last 30 dates
# If testing the script, set today as 2022-07-31
today <- today() 
lookback <- as_date(today - days(30:1))

# Create a list of ppt raster layers for past 30 days
AZ <- getData(country="USA", level=1) %>%
  subset(NAME_1=="Arizona") # Obtain AZ borders for masking raster later
rasters <- list() # Create empty list
for (past_day in lookback) { #Download and store raster data for past 30 days
  rasters[as_date(past_day)] <- crop(npn_download_geospatial("climate:prism_ppt", as_date(past_day)), extent(-115, -109, 31.3, 37))
}

# Function that counts events (ppt > 6.35mm) as separated by 3 days no ppt
wBuffer <- function(vals, na.rm=T) { # This function uses mm as per PRISM data
  # Initialize events and buffer trackers
  events <- 0
  low_ppt_days <- 4
  # For each previous day, examine its ppt value
  for (ppt in vals) {
    if (!is.na(ppt)) {
      # Case if ppt and buffer exceed thresholds
      if (ppt >= 6.35 & low_ppt_days >= 3) {
        # Count new event and reset buffer
        events <- events + 1
        low_ppt_days <- 0
        # Case if no ppt
      } else if (ppt == 0) {
        # Increment buffer
        low_ppt_days <- low_ppt_days + 1
        # Case if ppt occurred but is below threshold AND at least one no ppt day occurred since last event
      } else if (ppt < 6.35 & low_ppt_days != 0) {
        # Increment buffer
        low_ppt_days <- low_ppt_days + 1
        # Case if ppt occurred (above or below threshold) AND ppt occurred continuously since last event
      } else {
        # Reset buffer
        low_ppt_days <- 0
      }
    }
  }
  # After all days for a pixel have been checked, normalize scale by resetting
  # event counters above 4 to 4.
  if (events > 4) {
    events <- 4
  }
  return (events)
}

# Function that imitates wBuffer but with modifications for potential data values
wBufferStations <- function(vals, na.rm=T) { # This function uses inches as per RCC-ACIS
  # Initialize event and buffer trackers
  events <- 0
  low_ppt_days <- 4
  # Examine each preivous day's ppt value at a station
  for (ppt in vals) {
    if (!is.na(ppt)) {
      # Treat missing or delayed records as no ppt
      if (ppt == 'M' | ppt == 'S') {
        ppt <- 0
        # Treat trace records as low ppt
      } else if (ppt == 'T') {
        ppt <- 0.01
        # Treat accumulated records as true records
      } else if (substr(ppt, nchar(ppt), nchar(ppt)) == 'A') {
        ppt <- substr(ppt, 1, nchar(ppt)-1)
      }
      # Ensure value is numeric
      ppt <- as.numeric(ppt)
      
      # Case if ppt and buffer exceed thresholds
      if (ppt >= 0.25 & low_ppt_days >= 3) {
        # Count new event and reset buffer
        events <- events + 1
        low_ppt_days <- 0
        # Case if no ppt
      } else if (ppt == 0) {
        # Increment buffer
        low_ppt_days <- low_ppt_days + 1
        # Case if ppt occurred but is below threshold AND at least one no ppt day occurred since last event
      } else if (ppt < 0.25 & low_ppt_days != 0) {
        # Increment buffer
        low_ppt_days <- low_ppt_days + 1
        # Case if ppt occurred (above or below threshold) AND ppt occurred continuously since last event
      } else {
        # Reset buffer
        low_ppt_days <- 0
      }
    }
  }
  # After all days for a station have been checked, normalize scale by resetting
  # event counters above 4 to 4.
  if (events > 4) {
    events <- 4
  }
  return (events)
}

# Calculate forecast from wBuffer and mask
stacked_data <- stack(rasters[lookback]) # Create RasterStack of last 30 days precip info
forecast1 <- calc(stacked_data, fun=wBuffer, na.rm=T) %>% mask(AZ) # Calculate number of rainfall events in past 30 days for each pixel.

# Obtain and store station data -- used for point locations
url <- paste0('http://data.rcc-acis.org/MultiStnData?state=AZ&sdate=',as_date(lookback[1]),'&edate=',as_date(today),'&elems=pcpn')
dest <- paste0(getwd(),'/station_data.json') 
download.file(url, dest)
station_data <- fromJSON('station_data.json')

# Format df_stations data frame
df_stations <- data.frame( # Initialize an empty df
  longitude=replicate(length(station_data$data$meta$ll),NA),
  latitude=replicate(length(station_data$data$meta$ll),NA))
df_stations$name <- station_data[["data"]][["meta"]][["name"]]
df_stations <- mutate(df_stations, coordinates = station_data$data$meta$ll) # Populate df_stations$coordinates with vectors in form c(long, lat)
df_stations <- mutate(df_stations, longitude = lapply(df_stations$coordinates, FUN = function(coords) {return(coords[1])})) # Separate longitude
df_stations <- mutate(df_stations, latitude = lapply(df_stations$coordinates, FUN = function(coords) {return(coords[2])}))  # Separate latitude
station_values <- lapply(station_data[["data"]][["data"]], FUN = wBufferStations) # Calculate likelihood values
df_stations <- mutate(df_stations, values = station_values) # Assign these values to df
df_stations <- filter(df_stations, longitude != 'NULL') # Remove null objects -- they refer to areas such as "Greater Tucson Area"
df_stations$longitude <- unlist(df_stations$longitude) # Convert longitude from list of length 1 to numeric
df_stations$latitude <- unlist(df_stations$latitude) # Convert latitude from list of length 1 to numeric
df_stations <- select(df_stations, -coordinates) # Remove coordinates in c(long, lat) form
df_stations$values <- unlist(df_stations$values) # Convert values from single-item list to numeric
write.csv(df_stations, file = 'stations.csv') # Write usable data into csv

# Initialize objects needed for leaflet
factors <- as.factor(c(0,1,2,3,4))
color_obj <- colorFactor(palette=c('#fbf3de','#bae4b3','#74c476','#238b41', '#105e1e'), domain=levels(factors), na.color=NA)
stations <- read.csv('stations.csv')

l <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>% # Base layer map-- relief + roads
  addRasterImage(forecast1, colors = color_obj, opacity=.8, project=F) %>% #Overplot the forecast
  addLegend(pal= color_obj, values=levels(factors), opacity = .8, title='# of Events') %>% # Include legend 
  addCircles(data=stations, lng= ~longitude, lat= ~latitude, # Overplot station data
             stroke = T, color='black', weight = 1, radius = 750,
             fillColor = ~color_obj(values), fillOpacity = .8, 
             popup = ~name) # Allow to click on points and bring up station names
l






ui <- fluidPage(
  titlePanel("Rainfall Event Based Buffelgrass Forecast -- Beta Version"), # App title
  sidebarLayout(
    sidebarPanel(
      textOutput("description"), # Description of leaflet on left of page
      imageOutput("logo")),
    mainPanel(leafletOutput("mymap"))))


server <- function(input, output) {
  output$logo <- renderImage({list(src=file.path('USA-NPN-logo-RGB.png'), 
                                   width = 240, height = 240*21/67
                                   )}, deleteFile = F) # NPN logo image
  output$description <- renderText("This map is a product of the USA National Phenology Network
                                   at the University of Arizona and is intended as a prototype 
                                   only for the purpose of predicting green up of invasive buffelgrass
                                   (more at usanpn.org/data/forecasts/Buffelgrass). It
                                   combines PRISM raster and RCC-ACIS spatial point precipitation
                                   data to track the number of rainfall events in a 30-day rolling
                                   window prior to today's date. A rainfall event must exceed 0.25 inches
                                   in a single calendar day to be counted. It extends until the next 
                                   calendar day with no precipitation. Following an event, a 
                                   minimum of 3 consecutive days with precipitation under .25 
                                   inches is required for the next event over .25 inches to be counted.
                                   If you have any questions or feedback, please email info@usanpn.org.")
  output$mymap <- renderLeaflet({
    l
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)