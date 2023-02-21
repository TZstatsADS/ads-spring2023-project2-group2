# Define function for getting required input for Map
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
  
  if (x=="Floor Area") {
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
  }
  
  if (x=="Number of Living Units") {
    if (energy=="Water") {
      result = df4_living_units %>% select("Water", "n_living_units", "Type")
    }
    if (energy=="Gas") {
      result = df4_living_units %>% select("Natural_Gas", "n_living_units", "Type")
    }
    if (energy=="Electricity") {
      result = df4_living_units %>% select("Electricity", "n_living_units", "Type")
    }
  }
  
  return(result)
}


#define a function to extract necessary data for each year based on energy type
bar_df <- function(energy_type){
  if(energy_type=='Water'){
    #df[df$state %in% c('CA','AZ','PH'),]
    result=bar_water[bar_water$Type %in% housing_type,]%>% filter(Water != "NA") %>% group_by(year,Type)%>% 
      summarise(avg_consumption=mean(Water),.groups='drop')
  }
  if(energy_type=='Natural_Gas'){
    result=bar_gas[bar_gas$Type %in% housing_type,]%>% filter(Natural_Gas != "Insufficient access") %>% group_by(year,Type)%>% 
      summarise(avg_consumption=mean(as.double(Natural_Gas)),.groups='drop')
  }
  if(energy_type=='Electricity'){
    result=bar_electricity[bar_electricity$Type %in% housing_type,]%>% filter(Electricity != "Insufficient access") %>% group_by(year,Type)%>% 
      summarise(avg_consumption=mean(as.double(Electricity)),.groups='drop')
  }
  return(result)
}