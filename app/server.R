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




shinyServer(function(input, output) {
  

 
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
  
  
})