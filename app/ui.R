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

    menuItem("Green Energy", tabName = "Green_Energy", icon = icon("compass")),
    menuItem("Appendix", tabName = "Appendix", icon = icon("fas fa-asterisk"))
  ))
)

body <- dashboardBody(
  
  
  tabItems(
    # ------------------ Home ----------------------------------------------------------------
    
    tabItem(tabName = "Home", fluidPage(
      fluidRow(box(width = 15, title = "Introduction", status = "primary",
                   solidHeader = TRUE, h3("NYC Energy Consumption"),
                   h4("By Chenghao Lu, Tianxiao He, Chenbohan Zhang, Yuxin Liang, Chengqi Cao, Chenyi Jiang "),
                   h5("Collecting data from Mayor's Office of Climate and Sustainability, this application provides insight into how coronavirus impacted energy consumption in New York’s city. 
                      provides a comprehensive overview of how energy consumption in New York City changed in response to the COVID-19 pandemic, and sheds light on the potential opportunities 
                      and challenges for achieving a more sustainable and resilient energy system in the future."),
                   h5("The application will mainly track down the change in the amount of energy being used across the city. We mainly focus on 3 types of energy:", strong("Water, Natural gas, Electricity")))),
      fluidRow(box(width = 15, title = "Method Used", status = "primary", solidHeader=TRUE,
                   h5("///////method//////"))),
      fluidRow(box(width = 15, title = "How to Use The App", status = "primary",
                   solidHeader = TRUE,
                   h5("The application is divided into 5 separate tabs"),
                   tags$div(tags$ul(
                     tags$li("The", strong("first"), "tab: Home"),
                     tags$li("The", strong("second"), "tab: Map"),
                     tags$li("The", strong("third and fourth"), "tab: Interesting barplot and regression"),
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
    ), 



#------------------Bar Plot----------------------------
tabItem(tabName = "Time_Series_Analysis", fluidPage(
  
  # App title ----
  titlePanel("Energy Usage Before and During Covid"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select for the energy type ----
      selectInput(inputId = "Energy_Type",
                  label = "Choose an Energy Type:",
                  choices = c("Water", "Electricity", "Natural_Gas"))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      plotOutput(outputId = "bar_plot")
      
      
    )
  )
  
  
)


),#end of bar plot

#-----------------------regression Analysis-----------------

tabItem(tabName = "Regression_Analysis",                                                 # Map plot
        fluidPage(
          sidebarPanel(
            selectInput("independent_var", label="Choose an independent variable",
                        choices = c("Floor Area")),
            selectInput("dependent_var", label="Choose an energy type",
                        choices = c("Water", "Gas", "Electricity")),
          ),
          mainPanel(
            plotOutput("regression")
          )
        ),
), # end of regression 


#------------------appendix-----------------------

tabItem(tabName = "Appendix", fluidPage( 
  titlePanel("Data Sources "),
  HTML(
    "
      <h4> <p> Our data source comes from NYC publicly available dataset on energy and water data for the following years: </h4>          
      <h4> <p><li>Calendar Year 2018: <a href='https://data.cityofnewyork.us/Environment/Energy-and-Water-Data-Disclosure-for-Local-Law-84-/4tys-3tzj'>https://data.cityofnewyork.us/Environment/Energy-and-Water-Data-Disclosure-for-Local-Law-84-/4tys-3tzj</a></li></h4>
      <h4><li>Calendar Year 2019 : <a href='https://data.cityofnewyork.us/Environment/Energy-and-Water-Data-Disclosure-for-Local-Law-84-/wcm8-aq5w' target='_blank'> https://data.cityofnewyork.us/Environment/Energy-and-Water-Data-Disclosure-for-Local-Law-84-/wcm8-aq5w</a></li></h4>
      <h4><li>Calendar Year 2020 : <a href='https://data.cityofnewyork.us/Environment/Energy-and-Water-Data-Disclosure-for-Local-Law-84-/usc3-8zwd' target='_blank'>https://data.cityofnewyork.us/Environment/Energy-and-Water-Data-Disclosure-for-Local-Law-84-/usc3-8zwd</a></li></h4>
      <h4><li>Calendar Year 2021 : <a hsref='https://data.cityofnewyork.us/Environment/Energy-and-Water-Data-Disclosure-for-Local-Law-84-/7x5e-2fxh' target='_blank'> https://data.cityofnewyork.us/Environment/Energy-and-Water-Data-Disclosure-for-Local-Law-84-/7x5e-2fxh</a></li></h4>"
  ),

  
  titlePanel("Code & Dependencies"),
  
  HTML(
    " <p>The source code for this project can be found at <a href='https://github.com/TZstatsADS/ads-spring2023-project2-group2' target='_blank'> Github link here </a></p>",
    "<p>This RShiny application was created using the following R packages</p>
                <table>
                <tr> <td>
                <li>shiny</li>
                <li>dyplr</li>
                <li>tidyr</li>
                <li>ggplot2</li>
                <li>highcharter</li>
                <li>stringr</li>
                <li>withr</li>
                <li>lubridate</li>
                <li>treemap</li> </td>
                <td>
                <li>DT</li>
                <li>shinyBS</li>
                <li>shinyjs</li>
                <li>WDI</li>
                <li>geosphere</li>
                <li>magrittr</li>
                <li>shinycssloaders</li>
                <li>timevis</li>
                <li>leaflet</li> </td>
            </tr></table>"
  ),
  
  titlePanel("Suggestions & Feedbacks"),
  
  HTML(
    " <p>If there are any bugs or feedbacks you want to share, please feel free to contact</p>",
    " <p>Zhang, Chenbohan (cz2738@columbia.edu)</p>",
    " <p>He, Tianxiao (th2946@columbia.edu)</p>",
    " <p>Cao, Shengqi (sc5124@columbia.edu)</p>",
    " <p>Liang, Yuxin (yl5140@columbia.edu)</p>",
    " <p>Lu, Chenghao (cl4259@columbia.edu)</p>",
    " <p>Jiang, Chenyi (cj2740@columbia.edu)</p>")
)) # end of appendix 

    

)
)
  
  
  
  
ui <- dashboardPage(skin="green",header,sidebar,body )
    
    
  
