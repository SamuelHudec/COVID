library(shiny)

# load static parameters
source("static_params.R")

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
                  min = 50, 
                  max = 200, 
                  value = c(60, 180),
                  step = b0v_step),
      numericInput("gammav", 
                   label = h4("Gammav"), 
                   value = 1.06,
                   step = gammav_step),
      sliderInput("tmaxv", 
                  label = h4("Slider for tmaxv"), 
                  min = 20, 
                  max = 45, 
                  value = c(25, 38),
                  step = tmaxv_step),
      submitButton("Apply", icon("refresh")),
      br(),
      hr(),
      p("Radoslav Harman")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("sim_plots")
    )
  )
))
