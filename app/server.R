#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
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
  
  
  ###############Bar Plot###################
  output$bar_plot <- renderPlot({
    
    User_Input = input$Energy_Type
    plot_df <- bar_df(User_Input)
    
    ggplot(data =plot_df, aes(x=year, y=avg_consumption)) + geom_line(aes(colour=Type))
    
  })
  
  

  
  ##################regression analysis###############
  output$regression <- renderPlot({
    X=input$independent_var
    Y=input$dependent_var
    df = reg_gen(X, Y)
    
    mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20)),
                          axis.title = element_text(family = "Helvetica", size = (15), colour = "steelblue4"))
    
    if (X=="Floor Area"){
      if (Y == "Water") {
        g = ggplot(data = df, mapping = aes(x = floor_area, y = Water, color=Type)) + 
          geom_point(alpha = 0.1) + geom_smooth(formula = y ~ x, method=lm, se=FALSE) +
          labs(title=paste("Relationship between", X, "and", Y, "usage"),
               y="Water Usage (log kGal)", x="Floor Area (log square ft)") + mynamestheme
      }
      
      if (Y == "Electricity") {
        g = ggplot(data = df, mapping = aes(x = floor_area, y = Electricity, color=Type)) +
          geom_point(alpha = 0.1) + geom_smooth(formula = y ~ x, method=lm, se=FALSE) +
          labs(title=paste("Relationship between", X, "and", Y, "usage"),
               y="Electricity Usage (log kWh)", x="Floor Area (log square ft)") + mynamestheme
      }
      
      if (Y == "Gas") {
        g = ggplot(data = df, mapping = aes(x = floor_area, y = Natural_Gas, color=Type)) +
          geom_point(alpha = 0.1) + geom_smooth(formula = y ~ x, method=lm, se=FALSE) +
          labs(title=paste("Relationship between", X, "and", Y, "usage"),
               y="Gas Usage (log kBtu)", x="Floor Area (log square ft)") + mynamestheme
      }
    }
    
    if (X=="Number of Living Units"){
      if (Y == "Water") {
        g = ggplot(data = df, mapping = aes(x = n_living_units, y = Water)) +
          geom_point(alpha = 0.1) + geom_smooth(formula = y ~ x, method=lm) +
          labs(title=paste("Relationship between", X, "and", Y, "usage"),
               y="Water Usage (log kGal)", x="Log Number of Residential Units per Building") + mynamestheme
      }
      
      if (Y == "Electricity") {
        g = ggplot(data = df, mapping = aes(x = n_living_units, y = Electricity)) +
          geom_point(alpha = 0.1) + geom_smooth(formula = y ~ x, method=lm) +
          labs(title=paste("Relationship between", X, "and", Y, "usage"),
               y="Electricity Usage (log kWh)", x="Log Number of Residential Units per Building") + mynamestheme
      }
      
      if (Y == "Gas") {
        g = ggplot(data = df, mapping = aes(x = n_living_units, y = Natural_Gas)) +
          geom_point(alpha = 0.1) + geom_smooth(formula = y ~ x, method=lm) +
          labs(title=paste("Relationship between", X, "and", Y, "usage"),
               y="Gas Usage (log kBtu)", x="Log Number of Residential Units per Building") + mynamestheme
      }
    }
    g
    })
  
  
})