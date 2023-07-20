#----- Setup -----

## Load libraries
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(mapview)

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

## Fix CRS
forge_sf <- st_transform(forge_sf,
                         crs = 4326)

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


## Create a split dataset that has sub-data sets for each specialization; allows us to map each specialization as a separate layer
split_df_forge <- split(forge_sf, forge_sf$Legend_Specialization)

# ----- Create Map -----

map_layer_names <- names(split_df_forge)


## Function that maps a distinct color to each Specialization
pal <- colorFactor(palette = 'Set1',
                   domain = map_layer_names)

## Function to create the popup text (with HTML styling) for the map
generate_popup_text <- function(row) {
  popup_text <- paste0(
    "<b>Company Name: </b>",
    row$company_name,
    "<br> <b>DUNS #: </b>",
    row$duns_number,
    "<br> <b>Region: </b>",
    row$region,
    "<br> <b>Specialization: </b>",
    row$Legend_Specialization,
    "<br> <b>Certification: </b>",
    row$certification,
    "<br> <b>Subsidiary: </b>",
    row$subsidiary,
    "<br> <b>Overseas Location: </b>",
    row$overseas,
    "<br> <b>Number of Employees: </b>",
    row$employee_this_sites_2020,
    "<br> <b>Sales: </b>",
    row$sales_volume_2020,
    "<br> <b>Average Annual DoD Revenue: </b>",
    row$avg_ann_dod_total_revenue,
    "<br> <b>Website: </b>",
    row$website,
    "<br> <b>Web Contact: </b>",
    row$contact,
    "<br> <b>Phone Number: </b>",
    row$phone_no,
    "<br> <b>Address: </b>",
    row$address
  )

  popup_text
}

## Define the Leaflet map
l <-
  leaflet() %>%
  addTiles()

## Add a circle to the leaflet map defined above, iteratively for each specialization
names(split_df_forge) %>%
  purrr::walk(function(df) {
    l <<- l %>%
      addCircles(
        stroke = TRUE,
        data = split_df_forge[[df]],
        label =  ~ as.character(company_name),
        popup = ~ generate_popup_text(split_df_forge[[df]]),
        group = df,
        color = pal(split_df_forge[[df]]$Legend_Specialization),
        labelOptions = labelOptions(noHide = F,
                                    direction = 'auto')
      )
  })

## Finally, with all of our layers added, include layer control, legend, etc.
l <-
  l %>%
  addLayersControl(overlayGroups = map_layer_names,
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("topright",
            pal,
            values = map_layer_names,
            title = "Specialization") %>%
  # Search box for companies
  addSearchFeatures(
    targetGroups = map_layer_names,
    options = searchFeaturesOptions(
      position = "topleft",
      propertyLoc = "company_name",
      zoom = 15,
      openPopup = TRUE,
      textPlaceholder = "Search Company Name...",
      collapsed = FALSE
    )
  ) %>%
  # Search box for address
  leaflet.extras::addSearchOSM()


# ----- Export map and data -----

## Export map
mapshot(l,
        url = ("index.html"),
        title = "Interactive Map")
# notes
# select all button and unselect all in the box
# source
# https://stackoverflow.com/questions/51397728/invalid-utf-8-error-when-saving-leaflet-widget-in-r
# https://rdrr.io/cran/tmap/man/tm_symbols.html
