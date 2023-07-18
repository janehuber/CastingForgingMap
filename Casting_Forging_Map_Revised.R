
# Load libraries
library(tidyverse)
library(sf)
library(tmap) #???
library(tmaptools) #???
library(leafpop)
library(mapview) #???
library(geojsonsf)
library(leaflet) #???
library(leaflet.extras) #???
library(scales)
library(hrbrthemes) #???
library(readxl)
library(openxlsx)
library(janitor)

# Set options, remove scientific notation, other options
options(scipen = 999, stringsAsFactors = FALSE)
knitr::opts_chunk$set(echo = FALSE, message = FALSE,
                      warning = FALSE, rows.print = 25)
tmap_mode("view")
tmap_options(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas"))

### set work directory ###

# setwd("D:/OEA/DMC/2023/Analysis/R/Final")
setwd("~/Developer/CastingForgingMap")

### ----- Import data -----
map_data <- read.csv("Company_Data.csv")

# Fix bad lat/long
map_data <- map_data %>%
  mutate("longtitude" = case_when(
    duns_number == "96-966-4429" ~
      -87.720890,
    TRUE ~
      longtitude),
    "latitude" = case_when(
      duns_number == "96-966-4429" ~
        42.028545,
      TRUE ~
        latitude))

# Convert to spatial object
forge_sf <- st_as_sf(map_data,
                     coords = c("longtitude",
                                "latitude"),
                     crs = 4326)

# Fix CRS
forge_sf <- st_transform(forge_sf,
                         crs = 4326)

### ----- Interactive map -----

# Get layer names for search feature groups
map_layer_names <- forge_sf %>%
  st_drop_geometry() %>%
  tabyl(Legend_Specialization) %>%
  pull(Legend_Specialization)

map_layer_names_2 <- unique(forge_sf[c("Legend_Specialization")])

map_layer_names_3 <-
  forge_sf %>%
  st_drop_geometry() %>%
  pull("Legend_Specialization") %>%
  unique() %>%
  sort


# encoding
Encoding(x = forge_sf$certification) <- "UTF-8"

# replace all non UTF-8 character strings with an empty space
forge_sf$certification <-
  iconv( x = forge_sf$certification
         , from = "UTF-8"
         , to = "UTF-8"
         , sub = "")

# Final company map
final_map <- forge_sf %>%
  tm_shape() +
  tm_dots(col = "Legend_Specialization", alpha = 0.7,
          size = 0.15, border.col = "black",
          legend.is.portrait = TRUE,legend.show = TRUE,
          palette = "Set1",
          popup.vars = c("Company Name" = "company_name",
                         "DUNS #" = "duns_number",
                         "Region" = "region",
                         "Specialization" = "Legend_Specialization",
                         "Certification" = "certification",
                         "Subsidiary" = "subsidiary",
                         "Overseas Location" = "overseas",
                         "Number of Employees" = "employee_this_sites_2020",
                         "Sales" = "sales_volume_2020",
                         "Average Annual DoD Revenue" = "avg_ann_dod_total_revenue",
                         "Website" = "website",
                         "Web Contact" = "contact",
                         "Phone Number" = "phone_no",
                         "Address" = "address"))+
  tm_facets("Legend_Specialization", as.layers = TRUE) +
  tm_layout(title = "Establishments in Casting, Forging, and Related Sectors in the ILDMC Region")

# Convert to leaflet and add search box, checkbox for all teams
final_map_leaflet <- tmap_leaflet(final_map) %>%
  # Layer control
  addLayersControl(baseGroups = c("OpenStreetMap",
                                  "Esri.WorldGrayCanvas"),
                   overlayGroups = map_layer_names,
                   position = "topleft",
                   options = layersControlOptions(collapsed = FALSE)) %>%

  # Search box for companies
  addSearchFeatures(targetGroups = map_layer_names,
                    options = searchFeaturesOptions(
                      position = "topleft",
                      propertyLoc = "company_name",
                      zoom = 15,
                      openPopup = TRUE,
                      textPlaceholder = "Search Company Name...",
                      collapsed = FALSE)) %>%
  # Search box for address
  leaflet.extras::addSearchOSM()

### ----- Export map and data -----

# Export map
mapshot(final_map_leaflet,
        url = ("Interactive Map.html"),
        title = "Interactive Map")
# notes
# select all button and unselect all in the box
# source
# https://stackoverflow.com/questions/51397728/invalid-utf-8-error-when-saving-leaflet-widget-in-r
# https://rdrr.io/cran/tmap/man/tm_symbols.html
