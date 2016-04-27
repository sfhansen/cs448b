library(shiny)
library(ggmap)
library(leaflet)
library(readr)
library(dplyr)
library(geosphere)
library(stringr)
library(shinydashboard)

df = read_tsv(file = "hw3/data/scpd_incidents_cleaned.tsv")
ID = seq(1, nrow(df), 1)
df = bind_cols(df, as.data.frame(ID))

MILE_TO_METER = 1609.34
METER_TO_MILE = 0.000621371


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
                   end   = "2016-04-30",
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
    df_dist1 = NULL,
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
  
# Observe mouse clicks and add circles ------------------------------------
  
  observeEvent(input$map_click, {
    
    # Resets map if it already has 2 circles
    if(!is.null(v$click2)) {
      leafletProxy('map') %>% 
        clearShapes() %>% 
        clearMarkers()
      v$click1 = NULL
      v$click1_radius = NULL
      v$df_dist1 = NULL
      v$click2 = NULL
    }
    
    # Gets 1st map click
    if (is.null(v$click1)) {
      v$click1 = input$map_click
      v$click1_radius = input$radius
      click1 =  v$click1
      clat = click1$lat
      clng = click1$lng

      # Try to make this run faster 
      df_dist1 = df
      pos = select(df, longitude, latitude)
      pos1 = c(clng, clat)
      dist = distm(pos, pos1, distHaversine)
      df_dist1 = bind_cols(df_dist1, as.data.frame(dist))
      df_dist1 = rename(df_dist1, dist = V1)
      df_dist1 = filter(df_dist1, dist <= input$radius*MILE_TO_METER)
      v$df_dist1 = df_dist1
      
      ## Add the circle to the map proxy
      ## so you dont need to re-render the whole thing
      ## I also give the circles a group, "circles", so you can
      ## then do something like hide all the circles with hideGroup('circles')
      leafletProxy('map') %>% # use the proxy to save computation
        addCircles(lng = clng, lat = clat, group = 'circles',
                   weight = 1, radius = input$radius*MILE_TO_METER, 
                   color = 'black', fillColor = 'gray',
                   popup = FALSE, fillOpacity = 0.5, opacity = 1) 
      
      # Gets 2nd map click
    } else if(is.null(v$click2)) {
      v$click2 = input$map_click
      click2 = v$click2
      clat = click2$lat
      clng = click2$lng

      ##Now find the overlap in the circles
      # click1 = v$click1
      click1_radius = v$click1_radius
      
      
      df_dist1 = v$df_dist1
      # View(df_dist1)
      
      df_dist2 = df      
      pos = select(df, longitude, latitude)
      pos1 = c(clng, clat)
      dist = distm(pos, pos1, distHaversine)
      df_dist2 = bind_cols(df_dist2, as.data.frame(dist))
      df_dist2 = rename(df_dist2, dist = V1)
      df_dist2 = filter(df_dist2, dist <= input$radius*MILE_TO_METER)

      df_dist = bind_rows(df_dist1, df_dist2)
      df_dist = select(df_dist, -dist)
      dup = duplicated(df_dist)
      df_dist = bind_cols(df_dist, as.data.frame(dup))
      df_dist = filter(df_dist, TRUE == dup)

# Applies input selection filters  ----------------------------------------
      
      # Applies crime category filter 
      if(input$categoryInput != "ALL") {
        pos_all <- df_dist %>%
          filter(category == input$categoryInput) %>%
          select(latitude, longitude, category, description, date, resolution)
      } else {
          pos_all <- df_dist %>%
            select(latitude, longitude, category, description, date, resolution)
      }
      
      # Applies crime resolution filter 
      if(input$resolutionInput != "ALL") {
        pos_all <- pos_all %>% 
          filter(resolution == input$resolutionInput)
      } 
      
      # Applies date range filter
      pos_all <- pos_all %>% 
        filter(date >= as.POSIXct(input$dateRange[1]), 
                                  date <= as.POSIXct(input$dateRange[2]))

# Adds markers to map -----------------------------------------------------
      
      leafletProxy('map') %>% 
        clearMarkers()
      
      ## Add the circle to the map proxy
      ## so you dont need to re-render the whole thing
      ## I also give the circles a group, "circles", so you can
      ## then do something like hide all the circles with hideGroup('circles')
      leafletProxy('map') %>% # use the proxy to save computation
        addCircles(lng = clng, lat = clat, group = 'circles',
                   weight=1, radius = input$radius*MILE_TO_METER, 
                   color = 'black', fillColor = 'gray',
                   popup = FALSE, fillOpacity = 0.5, opacity = 1) %>% 
        addMarkers(data = pos_all, 
                   popup = str_c(pos_all$category, 
                                 pos_all$description, sep = ": "))
      # clusterOptions = markerClusterOptions()
    }
  })
}

shinyApp(ui = ui, server = server)