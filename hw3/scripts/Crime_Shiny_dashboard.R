library(shiny)
library(ggmap)
library(leaflet)
library(readr)
library(dplyr)
library(geosphere)
library(stringr)
library(shinydashboard)

df = read_tsv(file = "hw3/data/scpd_incidents_cleaned.tsv")

MILE_TO_METER = 1609.34
METER_TO_MILE = 0.000621371
EARTH_RADIUS = 6371000

# Creates UI --------------------------------------------------------------

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "San Francisco Crime"),
  dashboardSidebar(
    
    sliderInput('radius', 'Radius in Miles', 
                min = .5, max = 2, value = 1, step = .5
    ),
    
    dateRangeInput('dateRange',
                   label = 'Date Range',
                   start = "2016-03-02",
                   end   = "2016-03-31",
                   min = "2016-03-01",
                   max = "2016-04-30"
    ),
    
    selectInput("categoryInput", "Crime Category",
                choices = c("ALL","ARSON","ASSAULT","BRIBERY","BURGLARY","DISORDERLY CONDUCT",
                            "DRUG/NARCOTIC","DRUNKENNESS","EMBEZZLEMENT","EXTORTION",
                            "FAMILY OFFENSES","FORGERY/COUNTERFEITING","FRAUD","KIDNAPPING",
                            "LIQUOR LAWS","LOITERING","MISSING PERSON","NON-CRIMINAL",
                            "OTHER OFFENSES","PROSTITUTION","ROBBERY","RUNAWAY",                    
                            "SECONDARY CODES","SEX OFFENSES, FORCIBLE",
                            "SEX OFFENSES, NON FORCIBLE","STOLEN PROPERTY","SUICIDE",                     
                            "SUSPICIOUS OCC","TRESPASS","VANDALISM",
                            "VEHICLE THEFT","WARRANTS","WEAPON LAWS" )),
    
    selectInput("resolutionInput", "Crime Resolution",
                choices = c("ALL", "ARREST, BOOKED", "ARREST, CITED",
                            "CLEARED-CONTACT JUVENILE FOR MORE INFO",
                            "COMPLAINANT REFUSES TO PROSECUTE",
                            "EXCEPTIONAL CLEARANCE", "JUVENILE BOOKED",
                            "LOCATED", "NONE", "NOT PROSECUTED", 
                            "PSYCHOPATHIC CASE", "UNFOUNDED"))
  ),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    leafletOutput("map")
  )
)

# Creates Server  ---------------------------------------------------------

server <- function(input, output) {
  v = reactiveValues(
    click1 = NULL,  # Represents the first mouse click, if any
    click1_radius = NULL,
    map_filtered_crimes = NULL,
    click2 = NULL    # After two clicks, this stores the range of x
  )
  
# Initializes Map ---------------------------------------------------------

  output$map = renderLeaflet({
    leaflet() %>%
      setView(lng = -122.4237, lat = 37.7734, zoom = 12) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      
      # Add base maps
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap.Mapnik") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      
      # Add base map selector
      addLayersControl(
        baseGroups = c("OpenStreetMap.Mapnik", "Esri.WorldImagery"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  
  output$radius = renderPrint({ input$radius })
  
# Updates input selection filters  ----------------------------------------
  
  # Defines function to clear and update markers on map
  draw_new_markers <- function(input_data) {
    selected_crimes <- input_data
    leafletProxy('map') %>% 
      clearMarkers() %>%
      clearMarkerClusters() %>% 
      addMarkers(data = selected_crimes, 
                 popup = str_c(selected_crimes$category, 
                               selected_crimes$description, sep = ": "),
                 clusterOptions = markerClusterOptions())
  }
  
  # Defines function to update filters based on UI input selections
  update_filters <- function() {
      if(!is.null(v$map_filtered_crimes)) {
        # Applies date range filter 
        selected_crimes <- v$map_filtered_crimes %>% 
          filter(date >= as.POSIXct(input$dateRange[1]), 
                 date <= as.POSIXct(input$dateRange[2]))
        # Applies crime category filter
        if(input$categoryInput != "ALL") {
          selected_crimes <- selected_crimes %>%
            filter(category == input$categoryInput) %>%
            select(latitude, longitude, category, description, date, resolution)
        } 
        # Applies crime resolution filter
        if(input$resolutionInput != "ALL") {
          selected_crimes <- selected_crimes %>% 
            filter(resolution == input$resolutionInput) %>%
            select(latitude, longitude, category, description, date, resolution)
          
        } 
        draw_new_markers(selected_crimes)
      }
  }
  
  # Observes filters based on UI events
  observeEvent(input$dateRange, update_filters())
  observeEvent(input$categoryInput, update_filters())
  observeEvent(input$resolutionInput, update_filters())
  
# Observes mouse clicks and add circles ------------------------------------
  
  observeEvent(input$map_click, {
    
    # Resets map if it already has 2 circles
    if(!is.null(v$click2)) {
      leafletProxy('map') %>% 
        clearShapes() %>% 
        clearMarkers() %>%
        clearMarkerClusters()
     
      v$click1 = NULL
      v$click1_radius = NULL
      v$click2 = NULL
      v$click2_radius = NULL
    }
    
    # Gets 1st map click
    if (is.null(v$click1)) {
      v$click1 = input$map_click
      v$click1_radius = input$radius
      click1 =  v$click1
      clat1 = click1$lat
      clng1 = click1$lng
      
      # Adds first circle to map
      leafletProxy('map') %>% 
        addCircles(lng = clng1, lat = clat1, group = 'circles',
                   weight = 1, radius = input$radius * MILE_TO_METER, 
                   color = 'black', fillColor = 'gray',
                   popup = FALSE, fillOpacity = 0.5, opacity = 1) 
      
      # Gets 2nd map click
    } else if(is.null(v$click2)) {
      v$click2 = input$map_click
      v$click2_radius = input$radius
      click2 = v$click2
      click1 =  v$click1
      
      # Gets click 1 and 2 radii and coordinates 
      click1_radius = v$click1_radius
      click2_radius = v$click2_radius
      clat1 = click1$lat
      clng1 = click1$lng
      clat2 = click2$lat
      clng2 = click2$lng
      
      # If circle 1 is smaller: compute max/min coordinates from circle 1's center
      if(click1_radius <= click2_radius) {
        max_lat <- clat1  + (click1_radius * MILE_TO_METER / EARTH_RADIUS) * (180 / pi)
        min_lat <- clat1  - (click1_radius * MILE_TO_METER / EARTH_RADIUS) * (180 / pi)
        max_lng <- clng1  + (click1_radius * MILE_TO_METER / EARTH_RADIUS) * (180 / pi)
        min_lng <- clng1  - (click1_radius * MILE_TO_METER / EARTH_RADIUS) * (180 / pi)
        
        smaller_circle_lat <- clat1
        smaller_circle_lng <- clng1
        larger_circle_lat <- clat2
        larger_circle_lng <- clng2
        smaller_radius <- click1_radius
        larger_radius <- click2_radius

      # If circle 2 is smaller: compute max/min coordinates from circle 2's center
      } else {
        max_lat <- clat2  + (click1_radius * MILE_TO_METER / EARTH_RADIUS) * (180 / pi)
        min_lat <- clat2  - (click1_radius * MILE_TO_METER / EARTH_RADIUS) * (180 / pi)
        max_lng <- clng2  + (click1_radius * MILE_TO_METER / EARTH_RADIUS) * (180 / pi)
        min_lng <- clng2  - (click1_radius * MILE_TO_METER / EARTH_RADIUS) * (180 / pi)

        smaller_circle_lat <- clat2
        smaller_circle_lng <- clng2
        larger_circle_lat <- clat1
        larger_circle_lng <- clng1
        smaller_radius <- click2_radius
        larger_radius <- click1_radius
      }
  
      # Filter data frame to points within the min/max lat/long
      crimes_in_range <- df %>%
        filter(latitude <= max_lat, latitude >= min_lat,
               longitude <= max_lng, longitude >= min_lng)
     
      # Compute distances of these filtered points to the smaller circle's center
      crime_coords <- crimes_in_range %>% 
        select(longitude, latitude)
      smaller_circle_center <- c(smaller_circle_lng, smaller_circle_lat)
      distances_to_smaller_circle_center <- 
        distm(smaller_circle_center, crime_coords, distHaversine) %>% 
        t() %>% 
        as.data.frame()
      crimes_in_range <- bind_cols(crimes_in_range, as.data.frame(distances_to_smaller_circle_center))
      crimes_in_range <- crimes_in_range %>%
        rename(distance_to_smaller_circle_center = V1)
      
      # Filter crimes to only those within smaller circle 
      crimes_in_range <- crimes_in_range %>%
        filter(distance_to_smaller_circle_center <= smaller_radius * MILE_TO_METER)

      # Compute distances of these points to the larger circle's center
      crime_coords <- crimes_in_range %>% 
        select(longitude, latitude)
      larger_circle_center <- c(larger_circle_lng, larger_circle_lat)
      distances_to_larger_circle_center <- 
        distm(larger_circle_center, crime_coords, distHaversine) %>%
        t() %>% 
        as.data.frame()
      crimes_in_range <- bind_cols(crimes_in_range, as.data.frame(distances_to_larger_circle_center))
      crimes_in_range <- crimes_in_range %>%
        rename(distance_to_larger_circle_center = V1)
      
      # Filter crimes to those within the overlap of both circles
      crimes_in_range <- crimes_in_range %>%
        filter(distance_to_larger_circle_center <= larger_radius * MILE_TO_METER)
     
      # Store the crimes in the proper lat/long range into a reactive variable
      v$map_filtered_crimes <- crimes_in_range

# Applies initial input selection filters -------------------------------------- 
      
      # Applies crime category filter
      if(input$categoryInput != "ALL") {
        crimes_in_range <- crimes_in_range %>%
          filter(category == input$categoryInput) %>%
          select(latitude, longitude, category, description, date, resolution)
      } else {
        crimes_in_range <- crimes_in_range %>%
          select(latitude, longitude, category, description, date, resolution)
      }
      
      # Applies crime resolution filter 
      if(input$resolutionInput != "ALL") {
        crimes_in_range <- crimes_in_range %>% 
          filter(resolution == input$resolutionInput) %>%
          select(latitude, longitude, category, description, date, resolution)
      } 
      
      # Applies date range filter
      crimes_in_range <- crimes_in_range %>% 
        filter(date >= as.POSIXct(input$dateRange[1]), 
               date <= as.POSIXct(input$dateRange[2])) %>%
        select(latitude, longitude, category, description, date, resolution)
      
# Adds initial markers to map --------------------------------------------------
      
      leafletProxy('map') %>% 
        clearMarkers() %>%
        clearMarkerClusters()
      
      # Adds second circle to map
      leafletProxy('map') %>% 
        addCircles(lng = clng2, lat = clat2, group = 'circles',
                   weight = 1, radius = input$radius * MILE_TO_METER, 
                   color = 'black', fillColor = 'gray',
                   popup = FALSE, fillOpacity = 0.5, opacity = 1) %>% 
        
        # Adds crime markers to map
        addMarkers(data = crimes_in_range, 
                   popup = str_c(crimes_in_range$category, 
                                 crimes_in_range$description, sep = ": "),
                   clusterOptions = markerClusterOptions()
        )
    }
  })
}

shinyApp(ui = ui, server = server)