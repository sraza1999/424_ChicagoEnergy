library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(scales)
library(leaflet.providers)
library(mapview)
library(tigris)
library(DT)
library(ggplot2)


e <- read.csv("energy-usage-2010.csv")
names(e)[names(e) == "TOTAL.KWH"] <- "Electricity"
names(e)[names(e) == "TOTAL.THERMS"] <- "Gas"
names(e)[names(e) == "AVERAGE.BUILDING.AGE"] <- "Building_Age"
names(e)[names(e) == "AVERAGE.STORIES"] <- "Building_Height"
names(e)[names(e) == "BUILDING.TYPE"] <- "Building_Type"
names(e)[names(e) == "TOTAL.POPULATION"] <- "Population"
cook <- blocks(state = "IL", county = "Cook", year = 2010)
cook_tract <- tracts(state = "IL", county = "Cook", year = 2010 )
print("Have got the goods")


names(e)[names(e) == "KWH.JANUARY.2010"] <- "JANUARY_Electricity"
names(e)[names(e) == "KWH.FEBRUARY.2010"] <- "FEBRUARY_Electricity"
names(e)[names(e) == "KWH.MARCH.2010"] <- "MARCH_Electricity"
names(e)[names(e) == "KWH.APRIL.2010"] <- "APRIL_Electricity"
names(e)[names(e) == "KWH.MAY.2010"] <- "MAY_Electricity"
names(e)[names(e) == "KWH.JUNE.2010"] <- "JUNE_Electricity"
names(e)[names(e) == "KWH.JULY.2010"] <- "JULY_Electricity"
names(e)[names(e) == "KWH.AUGUST.2010"] <- "AUGUST_Electricity"
names(e)[names(e) == "KWH.SEPTEMBER.2010"] <- "SEPTEMBER_Electricity"
names(e)[names(e) == "KWH.OCTOBER.2010"] <- "OCTOBER_Electricity"
names(e)[names(e) == "KWH.NOVEMBER.2010"] <- "NOVEMBER_Electricity"
names(e)[names(e) == "KWH.DECEMBER.2010"] <- "DECEMBER_Electricity"

names(e)[names(e) == "THERM.JANUARY.2010"] <- "JANUARY_Gas"
names(e)[names(e) == "THERM.FEBRUARY.2010"] <- "FEBRUARY_Gas"
names(e)[names(e) == "THERM.MARCH.2010"] <- "MARCH_Gas"
names(e)[names(e) == "THERM.APRIL.2010"] <- "APRIL_Gas"
names(e)[names(e) == "THERM.MAY.2010"] <- "MAY_Gas"
names(e)[names(e) == "THERM.JUNE.2010"] <- "JUNE_Gas"
names(e)[names(e) == "THERM.JULY.2010"] <- "JULY_Gas"
names(e)[names(e) == "THERM.AUGUST.2010"] <- "AUGUST_Gas"
names(e)[names(e) == "THERM.SEPTEMBER.2010"] <- "SEPTEMBER_Gas"
names(e)[names(e) == "THERM.OCTOBER.2010"] <- "OCTOBER_Gas"
names(e)[names(e) == "THERM.NOVEMBER.2010"] <- "NOVEMBER_Gas"
names(e)[names(e) == "THERM.DECEMBER.2010"] <- "DECEMBER_Gas"
