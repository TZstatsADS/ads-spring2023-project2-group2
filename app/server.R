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


target_gen =function(year,energy,property){
  
  
  if(year ==2018){
  
    if(energy =="Electricity"){
      
        result = df1_electricity %>% filter( Electricity!= "Not Available",Borough != "") %>% 
          group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Electricity),number=n(),.groups="drop") %>% 
          filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
     }
  
    if(energy =="Natural Gas"){
    
        result = df1_gas %>% filter( Natural_Gas!= "Not Available",Borough != "") %>% 
          group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Natural_Gas),number=n(),.groups="drop") %>% 
          filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
    
     }
    if(energy =="Water"){
    
        result = df1_water %>% filter( Water!= "Not Available",Borough != "") %>% 
          group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Water),number=n(),.groups="drop") %>% 
          filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
    
     }
  }
  
  if(year ==2019){
    
    if(energy =="Electricity"){
      
      result = df2_electricity %>% filter( Electricity!= "Not Available",Borough != "") %>% 
        group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Electricity),number=n(),.groups="drop") %>% 
        filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
    }
    
    if(energy =="Natural Gas"){
      
      result = df2_gas %>% filter( Natural_Gas!= "Not Available",Borough != "") %>% 
        group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Natural_Gas),number=n(),.groups="drop") %>% 
        filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
      
    }
    if(energy =="Water"){
      
      result = df2_water %>% filter( Water!= "Not Available",Borough != "") %>% 
        group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Water),number=n(),.groups="drop") %>% 
        filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
      
    }
  }
  
  if(year ==2020){
    
    if(energy =="Electricity"){
      
      result = df3_electricity %>% filter( Electricity!= "Not Available",Borough != "") %>% 
        group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Electricity),number=n(),.groups="drop") %>% 
        filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
    }
    
    if(energy =="Natural Gas"){
      
      result = df3_gas %>% filter( Natural_Gas!= "Not Available",Borough != "")
      result$Natural_Gas = suppressWarnings(as.numeric(result$Natural_Gas))
      result = result  %>% drop_na()
      result = result %>% group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Natural_Gas),number=n(),.groups="drop") %>% 
        filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
      
    }
    if(energy =="Water"){
      
    
      result = df3_water %>% filter( Water!= "Not Available",Borough != "") %>% 
        group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Water),number=n(),.groups="drop") %>% 
        filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
      
      
    }
  }
  
  if(year ==2021){
    
    if(energy =="Electricity"){
      
      
      result = df4_electricity %>% filter( Electricity!= "Not Available",Borough != "")
      result$Electricity = suppressWarnings(as.numeric(result$Electricity))
      result = result  %>% drop_na()
      result = result %>%
        group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Electricity),number=n(),.groups="drop") %>% 
        filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
    }
    
    if(energy =="Natural Gas"){
      
      result = df4_gas %>% filter( Natural_Gas!= "Not Available",Borough != "") 
      result$Natural_Gas = suppressWarnings(as.numeric(result$Natural_Gas))
      result = result  %>% drop_na()
      result = result %>%
        group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Natural_Gas),number=n(),.groups="drop") %>% 
        filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
      
    }
    if(energy =="Water"){
      
      result = df4_water %>% filter( Water!= "Not Available",Borough != "") %>% 
        group_by(Borough,Type) %>%  summarise(Avg_consumption=mean(Water),number=n(),.groups="drop") %>% 
        filter(Type==property) %>% mutate(Longtitude = Longitude,Latitude = Latitude)
      
    }
  }
  return(result)
}


reg_gen = function(x,energy){
  property_types = list("Multifamily Housing", "Office", "Hotel", 
                        "Retail Store", "Manufacturing/Industrial Plant")
  if (energy=="Water") {
    result = df4_floor_area %>% select("Water", "floor_area", "Type") %>% 
          filter(Type %in% property_types)
  }
  if (energy=="Gas") {
    result = df4_floor_area %>% select("Natural_Gas", "floor_area", "Type") %>% 
    filter(Type %in% property_types)
  }
  if (energy=="Electricity") {
    result = df4_floor_area %>% select("Electricity", "floor_area", "Type") %>% 
    filter(Type %in% property_types)
  }
  
  return(result)
}



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
    
    ggplot(data = df, mapping = aes(x = floor_area, y = Water, color=Type)) +
      geom_point(alpha = 0.1) + geom_smooth(method=lm, se=FALSE) +
      labs(title=paste("Relationship between", X, "and", Y, "usage"),
           y="Water Usage (log kGal)", x="Floor Area (log square ft)") + mynamestheme
    
    if (X=="Floor Area"){
      if (Y == "Water") {
        g = ggplot(data = df, mapping = aes(x = floor_area, y = Water, color=Type)) +
          geom_point(alpha = 0.1) + geom_smooth(method=lm, se=FALSE) +
          labs(title=paste("Relationship between", X, "and", Y, "usage"),
               y="Water Usage (log kGal)", x="Floor Area (log square ft)") + mynamestheme
      }
      
      if (Y == "Electricity") {
        g = ggplot(data = df, mapping = aes(x = floor_area, y = Electricity, color=Type)) +
          geom_point(alpha = 0.1) + geom_smooth(method=lm, se=FALSE) +
          labs(title=paste("Relationship between", X, "and", Y, "usage"),
               y="Electricity Usage (log kGal)", x="Floor Area (log square ft)") + mynamestheme
      }
      
      if (Y == "Gas") {
        g = ggplot(data = df, mapping = aes(x = floor_area, y = Natural_Gas, color=Type)) +
          geom_point(alpha = 0.1) + geom_smooth(method=lm, se=FALSE) +
          labs(title=paste("Relationship between", X, "and", Y, "usage"),
               y="Gas Usage (log kGal)", x="Floor Area (log square ft)") + mynamestheme
      }
    }
    g
    })
  
  
})