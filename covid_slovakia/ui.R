library(shiny)
library(plotly)

# load static parameters
source("config.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stochastic Simulation of the Initial Phase of the COVID-19 Epidemic in Slovakia", 
             windowTitle = "COVID SLOVAKIA"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("b0v", 
                  label = h4("Slider for b0v"), 
                  min = 30, 
                  max = 300, 
                  value = c(60, 180),
                  step = b0v_step),
      numericInput("gammav", 
                   label = h4("Gammav"), 
                   value = 1.06,
                   min = 0.7, 
                   max = 1.3,
                   step = gammav_step),
      sliderInput("tmaxv", 
                  label = h4("Slider for tmaxv"), 
                  min = 26, 
                  max = 45, 
                  value = c(25, 38),
                  step = tmaxv_step),
      submitButton("Apply", icon("refresh")),
      br(),
      hr(),
      p("By Math Knights")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("sim_err"),
      br(),
      br(),
      plotlyOutput("sim_tests"),
      br(),
      br(),
      plotlyOutput("sim_infi"),
      br(),
      br(),
      plotlyOutput("sim_deaths"),
      br(),
      br(),
      plotlyOutput("best_fit_1"),
      br(),
      br(),
      plotlyOutput("best_fit_2")
     )
  )
))
