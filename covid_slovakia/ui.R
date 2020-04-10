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
      # panels for one separate fit, default by best params find in grid search ####
      conditionalPanel(condition = "input.tabb == 'fitt'",
        numericInput(inputId ="pred",
                     label = "Počet dní pred fazou kedy začala efektivne epidemia (pred)",
                     value = 9,
                     min = 3,
                     max = 50,
                     step = 1),
        numericInput(inputId ="b0",
                     label = "Parameter miery priznakov pre neinfikovanych (b0)",
                     value = 80,
                     min = 11,
                     step = 1),
        numericInput(inputId ="gamma1",
                     label = "Exp. Miera denneho prirastku realne infikovanych v období pred zavadením reštrikcii (Gamma1)",
                     value = 1.25,
                     min = 1,
                     max = 2,
                     step = gammav_step),
        numericInput(inputId ="fin1",
                     label = "Deň zavedenia reštrikcii",
                     value = 11,
                     min = 2,
                     step = fin_step),
        numericInput(inputId ="gamma2",
                     label = "Exp. Miera denneho prirastku realne infikovanych v období po (Gamma2)",
                     value = 0.95,
                     min = 0.5,
                     max = 1.3,
                     step = gammav_step),
        hr(),
        numericInput(inputId ="n_sim",
                     label = "Počet simulačných behov",
                     value = 5,
                     min = 3,
                     max = 100,
                     step = 1),
        hr(),
        p("Na aktualizovanie obrázka po zmene parametrov alebo po prechode zo záložky grid search použite"),
        actionButton(inputId = "update_fit",
                     label = "Apply",
                     icon = icon("refresh"))
        ),
      # panel for grid search ####
      conditionalPanel(condition = "input.tabb == 'gridd'",
        sliderInput(inputId ="predv",
                    label = "Počet dní pred fazou kedy začala efektivne epidemia (pred)",
                    value = c(1, 5),
                    min = 1,
                    max = 30,
                    step = 1),
        sliderInput(inputId ="b0v",
                    label = "Parameter miery priznakov pre neinfikovanych (b0)",
                    value = c(100, 300),
                    min = 20,
                    max = 500,
                    step = b0v_step),
        numericInput(inputId ="gamma1v",
                    label = "Exp. Miera denneho prirastku realne infikovanych v období pred zavadením reštrikcii (Gamma1)",
                    value = 1.25,
                    min = 1,
                    max = 2,
                    step = gammav_step),
        numericInput(inputId ="fin1v",
                    label = "Deň zavedenia reštrikcii",
                    value = 11,
                    min = 2,
                    step = fin_step),
        numericInput(inputId ="gamma2v",
                    label = "Exp. Miera denneho prirastku realne infikovanych v období po (Gamma2)",
                    value = 1.06,
                    min = 0.5,
                    max = 1.3,
                    step = gammav_step),
        hr(),
        numericInput(inputId ="n_simv",
                    label = "Počet simulačných behov",
                    value = 3,
                    min = 3,
                    max = 100,
                    step = 1),
        hr(),
        p("Na spustenie grid searchu po zmene parametrov použite"),
        actionButton(inputId = "update_grid",
                    label = "Apply",
                    icon = icon("refresh")),
        hr(),
        p("Poslať všetky parametre do záložky fit"),
        actionButton(inputId = "send_params",
                    label = "To Fit",
                    icon = icon("share-square"))
      ),
      # panel for grid search
      conditionalPanel(condition = "input.tabb == 'code'",
                       p("Karta obsahuje akutalny kod na zaklade ktorého su počítané simulácie v interaktívnom prostredí"),
                       p("Celú app aj s prídavnými skriptami najdete na GitHub-e")
        ),
      br(),
      hr(),
      p("By Math Knights")
    ),
    
    # main panel ####
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
          br(),
          plotlyOutput("best_fit_3"),
          br(),
          br(),
          plotlyOutput("best_fit_4"),
          br(),
          icon = icon("chart-line"), value = "fitt"
          ),
        tabPanel(title = "Grid search",
          br(),
          plotlyOutput("sim_err"),
          icon = icon("dna"), value = "gridd"
          ),
        tabPanel(title = "Code",
           htmlOutput("string_code"),
           icon = icon("code-branch"), value = "code"
          )
        )
      )
    )
  )
)
