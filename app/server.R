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

Longitude =c(-73.865433,-73.949997,-73.971321,-73.769417,-74.151535)
Latitude = c(40.837048,40.650002,40.776676,40.742054,40.579021)

#read four datasets
e_18 = read.csv("C:/Users/Chenghao Lu/Documents/GitHub/ads-spring2023-project2-group2/data/2018.csv",na.strings="Not Available")
e_19 = read.csv("C:/Users/Chenghao Lu/Documents/GitHub/ads-spring2023-project2-group2/data/2019.csv",na.strings="Not Available")
e_20 = read.csv("C:/Users/Chenghao Lu/Documents/GitHub/ads-spring2023-project2-group2/data/2020.csv",na.strings="Not Available")
e_21 = read.csv("C:/Users/Chenghao Lu/Documents/GitHub/ads-spring2023-project2-group2/data/2021.csv",na.strings="Not Available")

# data in 2018
df1 = data.frame(electricity_consumption = e_18$Electricity.Use...Grid.Purchase..kWh.,
                 gas_consumption = e_18$Natural.Gas.Use..kBtu.,
                 water_consumption = e_18$Water.Use..All.Water.Sources...kgal.,
                 Borough = e_18$Borough,
                 Type = e_18$Primary.Property.Type...Self.Selected
)
colnames(df1)[1] <- "Electricity"
colnames(df1)[2] <- "Natural_Gas"
colnames(df1)[3] <- "Water"
df1_electricity = df1 %>% select("Electricity","Borough","Type") 
df1_gas = df1 %>% select("Natural_Gas","Borough","Type") 
df1_water = df1 %>% select("Water","Borough","Type") 

# data in 2019

df2 = data.frame(electricity_consumption = e_19$Electricity.Use...Grid.Purchase..kWh.,
                 gas_consumption = e_19$Natural.Gas.Use..kBtu.,
                 water_consumption = e_19$Water.Use..All.Water.Sources...kgal.,
                 Borough = e_19$Borough,
                 Type = e_19$Primary.Property.Type...Self.Selected
)
colnames(df2)[1] <- "Electricity"
colnames(df2)[2] <- "Natural_Gas"
colnames(df2)[3] <- "Water"
df2_electricity = df2 %>% select("Electricity","Borough","Type") 
df2_gas = df2 %>% select("Natural_Gas","Borough","Type") 
df2_water = df2 %>% select("Water","Borough","Type")

# data in 2020

df3 = data.frame(electricity_consumption = e_20$Electricity.Use...Grid.Purchase..kWh.,
                 gas_consumption = e_20$Natural.Gas.Use..kBtu.,
                 water_consumption = e_20$Water.Use..All.Water.Sources...kgal.,
                 Borough = e_20$Borough,
                 Type = e_20$Primary.Property.Type...Self.Selected
)
colnames(df3)[1] <- "Electricity"
colnames(df3)[2] <- "Natural_Gas"
colnames(df3)[3] <- "Water"
df3_electricity = df3 %>% select("Electricity","Borough","Type") 
df3_gas = df3 %>% select("Natural_Gas","Borough","Type") 
df3_water = df3 %>% select("Water","Borough","Type")

# data in 2021

df4 = data.frame(electricity_consumption = e_21$Electricity.Use...Grid.Purchase..kWh.,
                 gas_consumption = e_21$Natural.Gas.Use..kBtu.,
                 water_consumption = e_21$Water.Use..All.Water.Sources...kgal.,
                 Borough = e_21$Borough,
                 Type = e_21$Primary.Property.Type...Self.Selected
)
colnames(df4)[1] <- "Electricity"
colnames(df4)[2] <- "Natural_Gas"
colnames(df4)[3] <- "Water"
df4_electricity = df4 %>% select("Electricity","Borough","Type")
df4_gas = df4 %>% select("Natural_Gas","Borough","Type") 
df4_water = df4 %>% select("Water","Borough","Type")




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