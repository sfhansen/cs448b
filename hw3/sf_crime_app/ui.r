# AUTHORS: Shirbi Ish-Shalom, Samuel Hansen
# PURPOSE: UI script for SF Crime Explorer RShiny Application 

library(shiny)
library(ggmap)
library(leaflet)
library(readr)
library(dplyr)
library(geosphere)
library(stringr)
library(shinydashboard)

dashboardPage(skin = "black",
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