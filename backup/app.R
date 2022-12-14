### USE DASHBOARD CODEBASE

library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(countrycode)
library(airportr)
library(zipcodeR)

### USE DASHBOARD CODEBASE

# Tools
"%ni%" <- Negate("%in%")

# Data
country_carbon <- read.csv("country_carbon.csv") %>%
  filter(TIME == 2020,
         LOCATION %ni% c("EU27_2020", "EU28", "G20", "OECD", "OEU", "WLD")) %>%
  select(LOCATION, Value) %>%
  rename("value" = Value) %>%
  drop_na()

country_carbon$location <- countrycode(c(country_carbon$LOCATION), origin = 'genc3c', destination = 'country.name')

dat <- tibble(case = c(rep("user", 3), rep("ref", 3)), 
              metric = c("egg","ham","green","egg","ham","green"), 
              value = c(1,2,4,5,5,5))

# User Interface

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", icon = icon("info-circle"), tabName = "about"),
    menuItem("Disclaimer", icon = icon("question-circle"), tabName = "disclaimer")
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidPage(
              fluidRow(
                column(6,
                       plotOutput("plot_nom")
                ),
                column(6, 
                       plotOutput("plot_prop")
                       # tableOutput("table")
                ),
              ),
              br(),
              h3("Please Input Your Personal Habits/Consumption Below to Calculate Your Carbon Footprint"),
              br(),
              fluidRow(
                column(3,
                       radioButtons(inputId = "meat", 
                                    label = "Are you vegeterian or vegan?",
                                    choices = list("Yes" = "yes", 
                                                   "No" = "no"),
                                    selected = "no"),
                ),
                column(3,
                       radioButtons(inputId = "travel", 
                                    label = "Do you drive or fly to Reed? (from your home)",
                                    choices = list("Drive" = "drive", 
                                                   "Fly" = "fly"),
                                    selected = "fly")
                ),
                column(3,
                       radioButtons(inputId = "drive", 
                                    label = "Do you drive weekly?",
                                    choices = list("Yes" = "yes", 
                                                   "No" = "no"),
                                    selected = "yes")
                ),
                column(3,
                       radioButtons(inputId = "home", 
                                    label = "Do you live on campus?",
                                    choices = list("Yes" = 1, 
                                                   "No" = 0),
                                    selected = 0),
                )
              ),
              fluidRow(
                column(width = 3,
                       conditionalPanel(
                         condition = "input.meat == 'no'",
                         sliderInput(inputId = "meat_red",
                                     label = "Servings of Red Meat Per Week (100g)",
                                     min = 0,
                                     max = 21,
                                     value = 0)
                       ),
                       conditionalPanel(
                         condition = "input.meat == 'no'",
                         sliderInput(inputId = "meat_white",
                                     label = "Servings of White Meat or Fish Per Week (100g)",
                                     min = 0,
                                     max = 21,
                                     value = 0)
                       )
                ),
                column(width = 3,
                       conditionalPanel(
                         condition = "input.travel == 'fly'",
                         textInput(inputId = "travel_airport",
                                   label = "Home airport code (e.g. PDX for Portland Intl. Airport)",
                                   value = "PDX"
                         )
                       ),
                       conditionalPanel(
                         condition = "input.travel == 'drive'",
                         textInput(inputId = "travel_zip",
                                   label = "Home zipcode (e.g. 97202)",
                                   value = 97202
                                   
                         )
                       ),
                       sliderInput(inputId = "travel_freq",
                                   label = "How many times do you travel home per year? (Number of round trips)",
                                   min = 0,
                                   max = 10,
                                   value = 0
                       )
                ),
                column(width = 3,
                       conditionalPanel(
                         condition = "input.drive == 'yes'",
                         sliderInput(inputId = "drive_mi",
                                     label = "Miles Driven Per Week",
                                     min = 0,
                                     max = 200,
                                     value = 0
                         )
                       ),
                       conditionalPanel(
                         condition = "input.drive == 'yes'",
                         sliderInput(inputId = "drive_mpg",
                                     label = "Fuel Efficiency of Car (miles per gallon)",
                                     min = 10,
                                     max = 100,
                                     value = 10
                         )
                       )
                ),
                column(width = 3,
                       conditionalPanel(
                         condition = "input.home == 0",
                         radioButtons(inputId = "home_type",
                                      label = "What type of home do you live in?",
                                      choices = list("Single-Family" = "single_family", 
                                                     "Apartment" = "aprt"),
                                      selected = "single_family"
                         )
                       ),
                       conditionalPanel(
                         condition = "input.home == 0",
                         sliderInput(inputId = "home_roomates",
                                     label = "How many people live in your home?",
                                     min = 1,
                                     max = 10,
                                     value = 1
                         )
                       )
                )
              )
            )
    ),
    
    tabItem(tabName = "about",
            h2("About tab content")
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Reed Carbon Calculator"),
  sidebar,
  body
)

# Server

server <- function(input, output) {
  
  dat1 <- reactive({
    tibble(
      Case = c("User", "Oregonian", "American"),
      `Air Travel` = c((zip_distance(97202, input$travel_zip, units = "meters")$distance * 0.000251034585607 * 2
                        + airport_distance("PDX", input$travel_airport) * 0.000099208 * 2)
                       * input$travel_freq, 
                       0,
                       0),
      Food = c(input$meat_red * 0.0155 * 52 
               + input$meat_white * 0.00182 * 52, 
               0.14 * 9.235,
               0.14 * 48),
      Transportation = c(1/(input$drive_mpg / input$drive_mi) * 0.00887 * 52,
                         0.28 * 9.235,
                         0.28 * 48),
      Housing = c(as.numeric(input$home) * 8.20
                  + (0.32 * 9.235)/input$home_roomates,
                  0.32 * 9.235,
                  0.32 * 48),
      Other = c(0,
                0.26 * 9.235,
                0.26 * 48)
    ) %>%
      pivot_longer(cols = 2:6, names_to = "Metric", values_to = "Value")
  })
  
  output$plot_nom <- renderPlot({
    dat <- dat1()
    ggplot(data = dat, aes(x = factor(Case, level = c("User", "Oregonian", "American")), y = Value, fill = Metric)) +
      geom_col() +
      ylab("Tons of CO2e") +
      xlab("Person")
  })
  
  output$plot_prop <- renderPlot({
    dat <- dat1()
    ggplot(data = dat, aes(x = factor(Case, level = c("User", "Oregonian", "American")), y = Value, fill = Metric)) +
      geom_col(position = "fill") +
      ylab("Proportion") +
      xlab("Person")
  })
  
  output$table <- renderTable({
    dat1() %>% tibble() %>% print()
  })
  
}

# sources: 
# https://www.epa.gov/energy/greenhouse-gases-equivalencies-calculator-calculations-and-references
# https://www.co2everything.com/categories
# https://8billiontrees.com/carbon-offsets-credits/reduce-carbon-footprint/average-footprint-per-person/american/ Oregon 9.235 tonnes per person
# https://css.umich.edu/publications/factsheets/sustainability-indicators/carbon-footprint-factsheet 48 tonnes avg American

shinyApp(ui, server)