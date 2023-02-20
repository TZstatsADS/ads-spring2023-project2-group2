if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("tidyr")) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("highcharter")) {
  install.packages("highcharter")
  library(highcharter)
}
if (!require("stringr")) {
  install.packages("stringr")
  library(stringr)
}
if (!require("withr")) {
  install.packages("withr")
  library(withr)
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}
if (!require("treemap")) {
  install.packages("treemap")
  library(treemap)
}
if (!require("DT")) {
  install.packages("DT")
  library(DT)
}
if (!require("shinyBS")) {
  install.packages("shinyBS")
  library(shinyBS)
}
if (!require("shinyjs")) {
  install.packages("shinyjs")
  library(shinyjs)
}
if (!require("WDI")) {
  install.packages("WDI")
  library(WDI)
}
if (!require("geosphere")) {
  install.packages("geosphere")
  library(geosphere)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}
if (!require("shinycssloaders")) {
  install.packages("shinycssloaders")
  library(shinycssloaders)
}
if (!require("timevis")) {
  install.packages("timevis")
  library(timevis)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}


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
    menuItem("Green Energy", tabName = "Green_Energy", icon = icon("compass")),
    menuItem("Appendix", tabName = "Appendix", icon = icon("fas fa-asterisk"))
  ))
)

body <- dashboardBody(
  
  
  tabItems(
    # ------------------ Home ----------------------------------------------------------------
    
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
    
    # ------------------ Map -----------------------------------
    tabItem(tabName = "Map",                                                 
            fluidPage(
        
                column(4,
                       img(src = "New_York_City_District_Map.svg",height=400)
                ),
                column(8,
                       leafletOutput("mymap", height = '550px')),
  
            
              fluidRow(
                
                column(4,
                       selectInput("map_year",
                                   label = "Choose a year",
                                   choices = c(2018,2019,2020,2021), 
                                   selected = 2018)
                ),
                
                column(4,
                       selectInput("map_energy",
                                   label = "Choose a energy",
                                   choices = c("Electricity","Natural Gas","Water"), 
                                   selected = "Electricity")
                ),
                
                column(4,
                       selectInput("map_type",
                                   label = "Choose a property type",
                                   choices = c("Office", "Multifamily Housing","Hotel","Retail Store","Manufacturing/Industrial Plant"), 
                                   selected = "Office")
                       ),

                p("*The radius of the circle reflects the number of a certain building type")
  
              ))),
    # ------------------ Green Energy -----------------------------------
    tabItem(tabName = "Green_Energy", 
      fluidPage(
      
      # App title ----
      titlePanel("Buildings Using Green Energy in New York"),
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          # Input: Select for year ----
          selectInput("year",
                      label = "Choose a year",
                      choices = c(2018,2019,2020,2021),
                      selected = 2018)
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          # Output
          leafletOutput("green_e")
        )
      )
    )
    ) 
  

)
)
  
  
  
  
ui <- dashboardPage(skin="green",header,sidebar,body )
    
    
  
