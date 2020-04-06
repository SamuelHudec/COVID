library(shiny)
library(plotly)

# load static parameters
source("config.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stochastic Simulation of the COVID-19 Epidemic in Slovakia", 
             windowTitle = "COVID SLOVAKIA"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # panels for one separate fit, default by best params find in grid search
      conditionalPanel(condition = "input.tabb == 'fitt'",
        numericInput(inputId ="b0",
                     label = h4("b0 parameter"),
                     value = 150,
                     min = 30,
                     max = 400,
                     step = 1),
        numericInput(inputId ="gamma",
                     label = h4("Gamma parameter"),
                     value = 1.04,
                     min = 0.7,
                     max = 1.3,
                     step = gammav_step),
        numericInput(inputId ="tmax",
                     label = h4("Tmax parameter"),
                     value = 32,
                     min = 26,
                     max = 45,
                     step = 1),
        actionButton(inputId = "update_fit",
                     label = "Apply",
                     icon = icon("refresh")),
        hr(),
        p("Po simuláciach vstupy vyššie obsahujú najlepšie nájdené parametre. Ak obrazok nie aktulany použite Apply")
        ),
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
                                    icon = icon("refresh")),
                       hr(),
                       p("týmto updatnete najlepšie parametre vzhladom na chybu v založke fit"),
                       actionButton(inputId = "send_params",
                                    label = "To Fit",
                                    icon = icon("share-square"))
      ),
      # panel for grid search
      conditionalPanel(condition = "input.tabb == 'code'",
                       p("Karta obsahuje akutalny kod na zaklade ktorého su počítané simulácie v interaktívnom prostredí")
        ),
      br(),
      hr(),
      p("By Math Knights")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "tabb",
        tabPanel(title = "Fit",
          br(),
          plotlyOutput("best_fit_1"),
          br(),
          br(),
          plotlyOutput("best_fit_2"),
          br(),
          icon = icon("chart-line"), value = "fitt"
          ),
        tabPanel(title = "Grid search",
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
        tabPanel(title = "Code",
           p("to be add"),
           icon = icon("code-branch"), value = "code"
          )
        )
      )
    )
  )
)
