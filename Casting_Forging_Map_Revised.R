
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
# tmap_mode("view")
# tmap_options(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas"))

### set work directory ###

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
# map_layer_names <- forge_sf %>%
#   st_drop_geometry() %>%
#   tabyl(Legend_Specialization) %>%
#   pull(Legend_Specialization)
#
# map_layer_names_2 <- unique(forge_sf[c("Legend_Specialization")])

map_layer_names <-
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
# final_map <- forge_sf %>%
#   tm_shape() +
#   tm_dots(col = "Legend_Specialization", alpha = 0.7,
#           size = 0.15, border.col = "black",
#           legend.is.portrait = TRUE,legend.show = TRUE,
#           palette = "Set1",
#           popup.vars = c("Company Name" = "company_name",
#                          "DUNS #" = "duns_number",
#                          "Region" = "region",
#                          "Specialization" = "Legend_Specialization",
#                          "Certification" = "certification",
#                          "Subsidiary" = "subsidiary",
#                          "Overseas Location" = "overseas",
#                          "Number of Employees" = "employee_this_sites_2020",
#                          "Sales" = "sales_volume_2020",
#                          "Average Annual DoD Revenue" = "avg_ann_dod_total_revenue",
#                          "Website" = "website",
#                          "Web Contact" = "contact",
#                          "Phone Number" = "phone_no",
#                          "Address" = "address"))+
#   tm_facets("Legend_Specialization", as.layers = TRUE) +
#   tm_layout(title = "Establishments in Casting, Forging, and Related Sectors in the ILDMC Region")
#
# # Convert to leaflet and add search box, checkbox for all teams
# final_map_leaflet <- tmap_leaflet(final_map) %>%
#   # Layer control
#   addLayersControl(baseGroups = c("OpenStreetMap",
#                                   "Esri.WorldGrayCanvas"),
#                    overlayGroups = map_layer_names,
#                    position = "topleft",
#                    options = layersControlOptions(collapsed = FALSE)) %>%
#
#   # Search box for companies
#   addSearchFeatures(targetGroups = map_layer_names,
#                     options = searchFeaturesOptions(
#                       position = "topleft",
#                       propertyLoc = "company_name",
#                       zoom = 15,
#                       openPopup = TRUE,
#                       textPlaceholder = "Search Company Name...",
#                       collapsed = FALSE)) %>%
#   # Search box for address
#   leaflet.extras::addSearchOSM()
#
#
# add_circles_for_group <- function(group_name) {
#
#   # Want to filter for the gropu name in the columns
#   # Then, want to return a command that says "add circles"
#
#
#
# }


# leaflet(forge_sf) %>%
#   addTiles(group = "OpenStreetMap") %>%
#   purrr::walk()
#   addCircles(group = "Legend_Specialization") %>%
#   addLayersControl(overlayGroups = map_layer_names,
#                    position = "topleft",
#                    options = layersControlOptions(collapsed = FALSE))

install.packages("RColorBrewer")                              # Install & load RColorBrewer package
library("RColorBrewer")

# Create list of colors to be used for dots that will be displayed in the map description thing





palette3_info <- brewer.pal.info[forge_sf$Legend_Specialization == "qual", ]

scale_colour_brewer(palette = "Set1")

pal <- colorFactor(
  palette = 'Set1',
  domain = names(split_df_forge)
)

sf_2 <- forge_sf

sf_2$color <- pal(sf_2$Legend_Specialization)

pal <- colorFactor(palette = "Set1", levels = levels(forge_sf$Legend_Specialization))


  split_df_forge <- split(forge_sf, forge_sf$Legend_Specialization)


  add_circles_to_map <- function() {

    temp_data <- forge_sf
    pal <- colorFactor(palette = "Set1", levels = levels(forge_sf$Legend_Specialization))
    temp_data$colors <- pal(temp_data$Legend_Specialization)

    head(temp_data$colors)
  }


  # MAJOR EXPERIMENT 1
  l <-
    leaflet() %>%
    addTiles() %>%
    addCircles(
      data = ,
      weight = 3,
      radius = 5,
      label = ~ company_name,
      popup = ~ generate_popup(forge_sf),
      color = ~ pal(Legend_Specialization),
      group = "Test"
    ) %>%
    addLegend(
      data = forge_sf,
      "topright",
      pal,
      values =  ~ Legend_Specialization,
      labels = names(forge_sf$Legend_Specialization),
      title = "Specialization"
    ) %>%
    addLayersControl(
      data = forge_sf,
      overlayGroups = c("Test"),
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
  l

  # %>%
  #   addLegend(position = "bottomright",
  #             colors = names(split_df_forge))

  generate_popup <- function(row) {

    popup_text <- paste0("<b>Company Name: </b>", row$company_name,
                         "<br> <b>DUNS #: </b>", row$duns_number)

    popup_text
  }





  # Here is where we go in and add circles for each thing... Do we want a better way to do this?
  l <-
    leaflet(forge_sf) %>%
    addTiles() %>%
    addLegend(
      "topright",
      pal,
      values =  ~ Legend_Specialization,
      labels = names(forge_sf$Legend_Specialization),
      title = "Specialization"
    ) %>%
    hideGroup(group = "Test") %>%
    addLayersControl(overlayGroups = c("Test"),
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
  l


  l <-
    leaflet(split_df_forge) %>%
    addTiles() %>%
    addLegend(
      "topright",
      pal,
      values =  ~ names(split_df_forge),
      labels = names(split_df_forge),
      title = "Specialization"
    ) %>%
    addLayersControl(overlayGroups = names(split_df_forge),
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

  # EXPERIMENT 3

  l <-
    leaflet() %>%
    addTiles() %>%
    addTitle(text = "Title")

    names(split_df_forge) %>%
    purrr::walk(function(df) {
      l <<- l %>%
        addCircles(
          stroke = TRUE,
          data = split_df_forge[[df]],
          label =  ~ as.character(company_name),
          popup = ~ generate_popup(split_df_forge[[df]]),
          group = df,
          color = pal(split_df_forge[[df]]$Legend_Specialization),
          labelOptions = labelOptions(noHide = F,
                                      direction = 'auto')
        )
    })

    l %>%
      addLayersControl(
        overlayGroups = names(split_df_forge),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        "topright",
        pal,
        values = c("Casting",
                     "Die and tool",
                     "Energy storage",
                     "Fastener-connection-rivet-washer",
                     "Forging",
                     "Machining",
                     "Metal closure",
                     "Metal stamping",
                     "Roll forming"),
        title = "Specialization"
      ) %>%
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
mapshot(l,
        url = ("index.html"),
        title = "Interactive Map")
# notes
# select all button and unselect all in the box
# source
# https://stackoverflow.com/questions/51397728/invalid-utf-8-error-when-saving-leaflet-widget-in-r
# https://rdrr.io/cran/tmap/man/tm_symbols.html
