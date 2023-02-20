#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#-------------------------------------------------App Server----------------------------------


# source("global.R") 
# Define server ----

source("../lib/HelperFunction.R")
shinyServer(function(input, output) {

  ####################### Map Tap ##################  
  
  output$mymap <- renderLeaflet({
    
    E = input$map_energy
    P = input$map_type
    Y = input$map_year
    df = target_gen(Y,E,P)
    qpal <- colorNumeric("RdYlBu", domain =df$Avg_consumption)
    
    if (E =="Electricity"){
    m <- leaflet(data = df) %>%
      addTiles() %>%
      addCircles(lng = ~Longitude, 
                 lat = ~Latitude,
                 color = ~qpal(df$Avg_consumption),fillOpacity = 1,
                 radius = 750+df$number,
                 popup = paste("Average_Consumption", df$Avg_consumption, "<br>",
                               "Borough:", df$Borough,"<br>",
                               "Number of Property type buildings:", df$number)
      ) %>%
      
      addLegend("bottomright", pal=qpal, values = ~df$Avg_consumption, labFormat = labelFormat(suffix = "kwh"),
                title = "Electricity Consumption", opacity = 1)
    
    }
    
  
    if (E == "Natural Gas"){
      m <- leaflet(data = df) %>%
        addTiles() %>%
        addCircles(lng = ~Longitude, 
                   lat = ~Latitude,
                   color = ~qpal(df$Avg_consumption),fillOpacity = 1,
                   radius = 750+df$number,
                   popup = paste("Average_Consumption", df$Avg_consumption, "<br>",
                                 "Borough:", df$Borough,"<br>",
                                 "Number of Property type buildings:", df$number)
        ) %>%
        
        addLegend("bottomright", pal=qpal, values = ~df$Avg_consumption, labFormat = labelFormat(suffix = "kBtu"),
                  title = "Natural Gas Consumption", opacity = 1)
      
    }
    
    if (E == "Water"){
      m <- leaflet(data = df) %>%
        addTiles() %>%
        addCircles(lng = ~Longitude, 
                   lat = ~Latitude,
                   color = ~qpal(df$Avg_consumption),fillOpacity = 1,
                   radius = 750+df$number,
                   popup = paste("Average_Consumption", df$Avg_consumption, "<br>",
                                 "Borough:", df$Borough,"<br>",
                                 "Number of Property type buildings:", df$number)
        ) %>%
        
        addLegend("bottomright", pal=qpal, values = ~df$Avg_consumption, labFormat = labelFormat(suffix = "kgal"),
                  title = "Water Consumption", opacity = 1)
      
    }
    
    m
  })
  
  ####################### Green Energy Tap ##################
  
  output$green_e <- renderLeaflet({
    if (input$year == 2018){
      out <- mapview(df_green18, xcol = "long", ycol = "lat", crs = 4326, grid = FALSE)@map
    }
    if (input$year == 2019){
      out <- mapview(df_green19, xcol = "long", ycol = "lat", crs = 4326, grid = FALSE)@map
    }
    if (input$year == 2020){
      out <- mapview(df_green20, xcol = "long", ycol = "lat", crs = 4326, grid = FALSE)@map
    }
    if (input$year == 2021){
      out <- mapview(df_green21, xcol = "long", ycol = "lat", crs = 4326, grid = FALSE)@map
    }
    return(out)
  }
  )
  
  
})