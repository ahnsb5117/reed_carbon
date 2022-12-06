#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Reed College Climate Footprint"),
    mainPanel(
      fluidRow(
               column(width = 4,
      radioButtons("input.drive", h3("Do You Drive?"),
                   choices = list("Yes" = 1, "No" = 0),
                   selected = 0),
      sliderInput(inputId = "drive_mi",
                                      label = "Miles Driven Per Week in a Car",
                                      min = 0,
                                      max = 100,
                                      value = 0
                                      
    ),
    sliderInput(inputId = "mpg",
                label = "Fuel Efficiency of Car (miles per gallon)",
                min = 0,
                max = 100,
                value = 0

    )
    
  ),
  column(width = 4,
         sliderInput(inputId = "red_meat",
                     label = "Servings of Red Meat Per Week (4 oz)",
                     min = 0,
                     max = 21,
                     value = 0
                     
         ),
         sliderInput(inputId = "white_meat",
                     label = "Servings of White Meat or Fish Per Week (4 oz)",
                     min = 0,
                     max = 21,
                     value = 0
                     
         ),
  ),
  column(width = 4,
         radioButtons("housing", h3("Housing"),
                      choices = list("On-Campus" = 1, "Off-Campus" = 0),
                                     selected = 1),
         conditionalPanel(
           condition = "input.housing == '0'",
           radioButtons("house_type", h2("House Type"),
                        choices = list("Single-Family" = "single", 
                                       "Duplex/Rowhome" = "dplx",
                                       "Apartment" = "aprt"
                                       ),
                        selected = "single")
         ),
  )
 )
)

))
