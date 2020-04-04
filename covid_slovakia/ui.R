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
      # panel for grid search
      conditionalPanel(condition = "input.tabb == 'gridd'",
        sliderInput(inputId ="b0v", 
                    label = h4("Slider for b0v"), 
                    min = 30, 
                    max = 400, 
                    value = c(44, 80),
                    step = b0v_step),
        numericInput(inputId ="gammav", 
                     label = h4("Gammav"), 
                     value = 1.06,
                     min = 0.7, 
                     max = 1.3,
                     step = gammav_step),
        sliderInput(inputId ="tmaxv", 
                    label = h4("Slider for tmaxv"), 
                    min = 26, 
                    max = 45, 
                    value = c(25, 30),
                    step = tmaxv_step),
        actionButton(inputId = "update",
                     label = "Apply",
                     icon = icon("refresh"))
        ),
      
      # panels for one separate fit, default by best params find in grid search
      conditionalPanel(condition = "input.tabb == 'fitt'",
        numericInput(inputId ="b0",
                     label = h4("b0 parameter"),
                     value = 0,
                     min = 30,
                     max = 400,
                     step = 1),
        numericInput(inputId ="gamma",
                     label = h4("Gamma parameter"),
                     value = 0,
                     min = 0.7,
                     max = 1.3,
                     step = gammav_step),
        numericInput(inputId ="tmax",
                     label = h4("Tmax parameter"),
                     value = 0,
                     min = 26,
                     max = 45,
                     step = 1),
        p("Po simuláciach vstupy vyššie obsahujú najlepšie nájdené parametre.")
        ),
      br(),
      hr(),
      p("By Math Knights")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "tabb",
        tabPanel(title = "Grid",
          br(),
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
          icon = icon("dna"), value = "gridd"
        ),
        tabPanel(title = "Fit",
          br(),
          plotlyOutput("best_fit_1"),
          br(),
          br(),
          plotlyOutput("best_fit_2"),
          icon = icon("chart-line"), value = "fitt"
          )
        )
      )
    )
  )
)
