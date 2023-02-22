#--------------------------------------------------------------------
############################### Install Related Packages #######################
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
if (!require("ggmap")) {
  install.packages("ggmap")
  library(ggmap)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("mapview")) {
  install.packages("mapview")
  library(mapview)
}

# Data Processing

## Define Longitude and Latitude
Longitude =c(-73.865433,-73.949997,-73.971321,-73.769417,-74.151535)
Latitude = c(40.837048,40.650002,40.776676,40.742054,40.579021)
## Read four data sets
e_18 = read.csv("../data/2018.csv",na.strings="Not Available")
e_19 = read.csv("../data/2019.csv",na.strings="Not Available")
e_20 = read.csv("../data/2020.csv",na.strings="Not Available")
e_21 = read.csv("../data/2021.csv",na.strings="Not Available")

## Get data we need for map in 2018
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

## Get data we need for map in 2019
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

## Get data we need for map in 2020
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

## Get data we need for map in 2021
df4 = data.frame(electricity_consumption = e_21$Electricity.Use...Grid.Purchase..kWh.,
                 gas_consumption = e_21$Natural.Gas.Use..kBtu.,
                 water_consumption = e_21$Water.Use..All.Water.Sources...kgal.,
                 Borough = e_21$Borough,
                 Type = e_21$Primary.Property.Type...Self.Selected,
                 floor_area=e_21$Property.GFA...Calculated..Buildings...ft..,
                 n_living_units=e_21$Multifamily.Housing...Total.Number.of.Residential.Living.Units
)
colnames(df4)[1] <- "Electricity"
colnames(df4)[2] <- "Natural_Gas"
colnames(df4)[3] <- "Water"
df4_electricity = df4 %>% select("Electricity","Borough","Type")
df4_gas = df4 %>% select("Natural_Gas","Borough","Type") 
df4_water = df4 %>% select("Water","Borough","Type")


## Get data we need for Green energy in 2018
df_green18 = data.frame(green_power = e_18$Green.Power...Onsite..kWh.,
                 long = e_18$Longitude,
                 lat = e_18$Latitude
)
df_green18 <- na.omit(df_green18)
df_green18 <- df_green18[df_green18$green_power!=0.0,]
df_green18 = df_green18 %>% select("long","lat")


## Get data we need for Green energy in 2019
df_green19 = data.frame(green_power = e_19$Green.Power...Onsite..kWh.,
                        long = e_19$Longitude,
                        lat = e_19$Latitude
)
df_green19 <- na.omit(df_green19)
df_green19 <- df_green19[df_green19$green_power!=0.0,]
df_green19 = df_green19 %>% select("long","lat")

## Get data we need for Green energy in 2020
df_green20 = data.frame(green_power = e_20$Green.Power...Onsite..kWh.,
                        long = e_20$Longitude,
                        lat = e_20$Latitude
)
df_green20 <- na.omit(df_green20)
df_green20 <- df_green20[df_green20$green_power!=0.0,]
df_green20 = df_green20 %>% select("long","lat")

## Get data we need for Green energy in 2018
df_green21 = data.frame(green_power = e_21$Green.Power...Onsite..kWh.,
                        long = e_21$Longitude,
                        lat = e_21$Latitude
)
colnames(df_green21)[0] <- "Green_Power"
df_green21 <- na.omit(df_green21)
df_green21 <- df_green21[df_green21$green_power!=0.0,]



# Get data we need for regression
df4_floor_area = df4 %>% select("Water", "Natural_Gas", "Electricity", "floor_area", "Type") %>% drop_na()
df4_floor_area$Natural_Gas = suppressWarnings(as.numeric(as.character(df4_floor_area$Natural_Gas)))
df4_floor_area$Electricity = suppressWarnings(as.numeric(as.character(df4_floor_area$Electricity)))
df4_floor_area[1:4] <- log(df4_floor_area[1:4])
df4_floor_area <- df4_floor_area[!is.infinite(rowSums(df4_floor_area[1:4])),]
df4_floor_area <- df4_floor_area[!is.na(df4_floor_area["Electricity"]), ]
df4_floor_area <- df4_floor_area[!is.na(df4_floor_area["Natural_Gas"]), ]

df4_living_units = df4 %>% select("Water", "Natural_Gas", "Electricity", "n_living_units", "Type") %>% drop_na()
df4_living_units$Natural_Gas = suppressWarnings(as.numeric(as.character(df4_living_units$Natural_Gas)))
df4_living_units$Electricity = suppressWarnings(as.numeric(as.character(df4_living_units$Electricity)))
df4_living_units[1:4] <- log(df4_living_units[1:4])
df4_living_units <- df4_living_units[df4_living_units["Type"]=="Multifamily Housing", ]
df4_living_units <- df4_living_units[!is.infinite(rowSums(df4_living_units[1:4])),]
df4_living_units <- df4_living_units[!is.na(df4_living_units["Electricity"]), ]
df4_living_units <- df4_living_units[!is.na(df4_living_units["Natural_Gas"]), ]

#----------------bar plot data--------------------------

#getting data and adding the year attribute

bar1_electricity <- df1_electricity
bar1_electricity$year <-2018

bar2_electricity <- df2_electricity
bar2_electricity$year <-2019

bar3_electricity <- df3_electricity
bar3_electricity$year <-2020

bar4_electricity <- df4_electricity
bar4_electricity$year <-2021

bar1_water <- df1_water
bar1_water$year <-2018

bar2_water <- df2_water
bar2_water$year <-2019

bar3_water <- df3_water
bar3_water$year <-2020

bar4_water <- df4_water
bar4_water$year <-2021

bar1_gas <- df1_gas
bar1_gas$year <-2018

bar2_gas <- df2_gas
bar2_gas$year <-2019

bar3_gas <- df3_gas
bar3_gas$year <-2020

bar4_gas <- df4_gas
bar4_gas$year <-2021

housing_type <- c("Office", "Multifamily Housing","Retail Store","Manufacturing/Industrial Plant")

#combining four years together

bar_water <- rbind(bar1_water,bar2_water,bar3_water,bar4_water)
bar_gas <- rbind(bar1_gas,bar2_gas,bar3_gas,bar4_gas)
bar_electricity <- rbind(bar1_electricity,bar2_electricity,bar3_electricity,bar4_electricity)

# end of bar plot

