library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
library(timevis)
library(leaflet)


# build ui
header <- dashboardHeader(
  
  title = HTML("Energy consumption of New York City"), 
  disable = FALSE, 
  titleWidth  = 400,
  dropdownMenu( type = 'message',
               
                messageItem(
                from = "Feedback and suggestions",
                message =  "",
                icon = icon("envelope"),
                href = "mailto:cl4259@columbia.edu"
                      ),
                icon = icon('comment')
  ),
  dropdownMenu( type = 'message',
                icon = icon("share-alt"),
                messageItem(
                  from = 'Twitter',
                  message = "",
                  icon = icon("twitter"),
                  href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                ),
                messageItem(
                  from = 'Facebook',
                  message = "",
                  icon = icon("facebook"),
                  href = "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                ),
                messageItem(
                  from = 'Google+',
                  message = "",
                  icon = icon("google-plus"),
                  href = "https://plus.google.com/share?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                )
  )
)

sidebar <- dashboardSidebar(
  
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home")),
    menuItem("Map", tabName = "Map", icon = icon("compass")),
    menuItem("New Business", tabName = "New_Business", icon = icon("dollar-sign")),
    menuItem("Closed Business", tabName = "Closed_Business", icon = icon("dollar-sign")),
    menuItem("Appendix", tabName = "Appendix", icon = icon("fas fa-asterisk"))
  ))
)

body <- dashboardBody(
  
  
  tabItems(
    
    
    tabItem(tabName = "Home", fluidPage(
      fluidRow(box(width = 15, title = "Introduction", status = "primary",
                   solidHeader = TRUE, h3("Covid-19 and NYC business"),
                   h4("By Wendy Doan, Qizhen Yang, Qiao Li, Yandong Xiong, James Bergin Smiley"),
                   h5("Drawing data from multiple sources, this application provides insight into the economic impact of coronavirus 2019 (COVID-19) on New York’s city economy. The results shed light on both the financial fragility of many businesses, and the significant impact COVID-19 had on these businesses in the weeks after the COVID-19–related disruptions began."),
                   h5("The application will mainly track down the change in the number of businesses being closed or newly opened across Covid timeline. We divided the businesses into 4 types:", strong("Retail, Service, Food and Beverage, Entertainment")))),
      fluidRow(box(width = 15, title = "Targeted User", status = "primary", solidHeader=TRUE,
                   h5("We believe that the application would be useful for anyone who are interested in learning more about the effects of Covid 19"))),
      fluidRow(box(width = 15, title = "How to Use The App", status = "primary",
                   solidHeader = TRUE,
                   h5("The application is divided into 5 separate tabs"),
                   tags$div(tags$ul(
                     tags$li("The", strong("first"), "tab: Introduction"),
                     tags$li("The", strong("second"), "tab: The detailed ZIP code map shows the extent of Covid 19 outbreak in NYC. It provided key information including: confirmed cases, infection rate, number of business that are closed in the neighborhood"),
                     tags$li("The", strong("third and fourth"), "tab: stats on recently opened/ closed business during Covid 19, tracked separately for different industries"),
                     tags$li("The", strong("fifth"),"tab: Appendix and data sources")
                     
                   ))
      ))
    )), # end of home 
    
    tabItem(tabName = "Map",                                                 # Map plot
            fluidPage(
              fluidRow(
                
                column(6,
                       selectInput("map_year",
                                   label = "Choose a year",
                                   choices = c(2018,2019,2020,2021), 
                                   selected = 2018)
                ),
                
                column(6,
                       selectInput("map_energy",
                                   label = "Choose a energy",
                                   choices = c("Electricity","Natural Gas","Water"), 
                                   selected = "Electricity")
                ),
                
                column(6,
                       selectInput("map_type",
                                   label = "Choose a property type",
                                   choices = c("Office", "Multifamily Housing","Hotel","Retail Store","Manufacturing/Industrial Plant"), 
                                   selected = "Office")
                       )
                
              
            ),
            column(12,
                   leafletOutput("mymap", height = '600px'))
    )
  
  
  
)
  

)
)
  
  
  
  
ui <- dashboardPage(skin="green",header,sidebar,body )
    
    
  
