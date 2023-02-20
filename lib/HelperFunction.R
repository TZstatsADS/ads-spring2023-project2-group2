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
