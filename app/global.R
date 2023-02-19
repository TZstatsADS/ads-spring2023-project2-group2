#--------------------------------------------------------------------
###############################Install Related Packages #######################
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

#Data Processing
## Define Longitude and Latitude
Longitude =c(-73.865433,-73.949997,-73.971321,-73.769417,-74.151535)
Latitude = c(40.837048,40.650002,40.776676,40.742054,40.579021)
## Read four data sets
e_18 = read.csv("../data/2018.csv",na.strings="Not Available")
e_19 = read.csv("../data/2019.csv",na.strings="Not Available")
e_20 = read.csv("../data/2020.csv",na.strings="Not Available")
e_21 = read.csv("../data/2021.csv",na.strings="Not Available")

# Get data we need for map in 2018
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

# Get data we need for map in 2019
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

# Get data we need for map in 2020
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

# Get data we need for map in 2021
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
