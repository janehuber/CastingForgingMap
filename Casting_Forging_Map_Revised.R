#----- Setup -----

## Load libraries
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(mapview)
library(leaflet.providers)
library(htmlwidgets)
library(htmltools)

## Set options, remove scientific notation, other options
options(scipen = 999, stringsAsFactors = FALSE)
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  rows.print = 25
)

## Set working directory

setwd("~/Developer/CastingForgingMap")

# ----- Import Data -----
map_data <- read.csv("Company_Data.csv")

# ----- Transform Data -----
## Fix bad lat/long
map_data <- map_data %>%
  mutate(
    "longtitude" = case_when(duns_number == "96-966-4429" ~
                               -87.720890,
                             TRUE ~
                               longtitude),
    "latitude" = case_when(duns_number == "96-966-4429" ~
                             42.028545,
                           TRUE ~
                             latitude)
  )

## Convert to spatial object
forge_sf <- st_as_sf(map_data,
                     coords = c("longtitude",
                                "latitude"),
                     crs = 4326)

## Fix CRS & Ensure that Legend_Specialization is a factor (this is important because otherwise we will be unable to manipulate this variable in mapping)
forge_sf <- st_transform(forge_sf,
                         crs = 4326) %>%
  mutate(Legend_Specialization = factor(Legend_Specialization))

## Encoding
Encoding(x = forge_sf$certification) <- "UTF-8"

## Replace all non UTF-8 character strings with an empty space
forge_sf$certification <-
  iconv(
    x = forge_sf$certification
    ,
    from = "UTF-8"
    ,
    to = "UTF-8"
    ,
    sub = ""
  )


# ----- Create Map Helper Functions -----

map_layer_names <- levels(forge_sf$Legend_Specialization)


## Function that maps a distinct color to each Specialization
pal <- colorFactor(palette = 'Set1',
                   levels = map_layer_names)

## Set CSS for the title of the map
tag_map_title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    margin-top: -3px;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-size: 20px;
  }
"))

## Create the HTML title
title <- tags$div(
  tag_map_title, HTML("Establishments in Casting, Forging, and Related Sectors in the ILDMC Region")
)


## Function to create the popup text (with HTML basic styling) for the map (if we want to update styling, would need to set CSS)
generate_popup_text <- function(data) {
    popup_text <- paste0(
      "<h3>", data$region, "</h3>",
    "<b>Company Name: </b>",
    data$company_name,
    "<br> <b>DUNS #: </b>",
    data$duns_number,
    "<br> <b>Region: </b>",
    data$region,
    "<br> <b>Specialization: </b>",
    data$Legend_Specialization,
    "<br> <b>Certification: </b>",
    data$certification,
    "<br> <b>Subsidiary: </b>",
    data$subsidiary,
    "<br> <b>Overseas Location: </b>",
    data$overseas,
    "<br> <b>Number of Employees: </b>",
    data$employee_this_sites_2020,
    "<br> <b>Sales: </b>",
    data$sales_volume_2020,
    "<br> <b>Average Annual DoD Revenue: </b>",
    data$avg_ann_dod_total_revenue,
    "<br> <b>Website: </b>",
    data$website,
    "<br> <b>Web Contact: </b>",
    data$contact,
    "<br> <b>Phone Number: </b>",
    data$phone_no,
    "<br> <b>Address: </b>",
    data$address
  )

  popup_text
}

# ----- Create Map -----

map_layers <- function() {

  #base map
  map <- leaflet() %>%
    addControl(title, position = "topleft", className = "map-title") %>%
    addProviderTiles(providers$CartoDB.Positron)

  opacity <- 0.8

  #loop through all groups and add one layer one at a time
  for (i in map_layer_names) {

    layer_data <- forge_sf %>% filter(Legend_Specialization == as.character(i))

    map <- map %>%
      addCircleMarkers(
        data = layer_data,
        group = i,
        label =  ~ as.character(region),
        popup = ~ generate_popup_text(layer_data),
        fillColor = ~ pal(Legend_Specialization),
        color = "black",
        fillOpacity = opacity,
        weight = 1,
        stroke = TRUE,
        radius = 5
      )
  }

  #create layer control
  map %>%
    addLayersControl(
      overlayGroups = map_layer_names,
      options = layersControlOptions(collapsed = FALSE),
      position = "topleft"
    ) %>%
    addLegend("topright",
              pal,
              values = map_layer_names,
              title = "Specialization",
              opacity = opacity) %>%
    leaflet.extras::addSearchOSM()

}


# ----- Export map and data -----

## Export map
mapshot(map_layers(),
        url = ("index.html"),
        title = "Interactive Map")

# notes
# source
# https://stackoverflow.com/questions/51397728/invalid-utf-8-error-when-saving-leaflet-widget-in-r
# https://rdrr.io/cran/tmap/man/tm_symbols.html
