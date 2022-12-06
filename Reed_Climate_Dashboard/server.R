#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(countrycode)
library(shiny)

"%ni%" <- Negate("%in%")

country_carbon <- read.csv("country_carbon.csv") %>%
  filter(TIME == 2020,
         LOCATION %ni% c("EU27_2020", "EU28", "G20", "OECD", "OEU", "WLD")) %>%
  select(LOCATION, Value) %>%
  rename("value" = Value) %>%
  drop_na()

country_carbon$location <- countrycode(c(country_carbon$LOCATION), origin = 'genc3c', destination = 'country.name')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

})
