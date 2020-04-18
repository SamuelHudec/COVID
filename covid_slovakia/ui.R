library(shiny)
library(knitr)
library(plotly)

# load static parameters
source("config.R")

# rmd -> md
rmdfiles <- c("info.Rmd")
sapply(rmdfiles, knit, quiet = T)

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
                     label = "The number of days before the stage when the epidemic started (pred)",
                     value = 9,
                     min = 3,
                     max = 50,
                     step = 1),
        numericInput(inputId ="b0",
                     label = "The rate of symptoms for uninfected (b0)",
                     value = 80,
                     min = 11,
                     step = 1),
        numericInput(inputId ="gamma1",
                     label = "The rate of daily increment actually infected in the pre-restriction period (Gamma1)",
                     value = 1.25,
                     min = 1,
                     max = 2,
                     step = gammav_step),
        numericInput(inputId ="fin1",
                     label = "Restrictions day",
                     value = 11,
                     min = 2,
                     step = fin_step),
        numericInput(inputId ="gamma2",
                     label = "The daily increment rate actually infected in the post-restriction period (Gamma2)",
                     value = 0.95,
                     min = 0.5,
                     max = 1.3,
                     step = gammav_step),
        hr(),
        numericInput(inputId ="n_sim",
                     label = "Number of simulation runs",
                     value = 5,
                     min = 3,
                     max = 100,
                     step = 1),
        hr(),
        p("To update an image after changing a parameters or after moving from the grid search tab."),
        actionButton(inputId = "update_fit",
                     label = "Apply",
                     icon = icon("refresh")),
        br(),
        hr(),
        p("By Math Knights")
        ),
      # panel for grid search ####
      conditionalPanel(condition = "input.tabb == 'gridd'",
        sliderInput(inputId ="predv",
                    label = "The number of days before the stage when the epidemic started (pred)",
                    value = c(1, 5),
                    min = 1,
                    max = 30,
                    step = 1),
        sliderInput(inputId ="b0v",
                    label = "The rate of symptoms for uninfected (b0)",
                    value = c(100, 300),
                    min = 20,
                    max = 500,
                    step = b0v_step),
        numericInput(inputId ="gamma1v",
                    label = "The rate of daily increment actually infected in the pre-restriction period (Gamma1)",
                    value = 1.25,
                    min = 1,
                    max = 2,
                    step = gammav_step),
        numericInput(inputId ="fin1v",
                    label = "Restrictions day",
                    value = 11,
                    min = 2,
                    step = fin_step),
        numericInput(inputId ="gamma2v",
                    label = "The daily increment rate actually infected in the post-restriction period (Gamma2)",
                    value = 1.06,
                    min = 0.5,
                    max = 1.3,
                    step = gammav_step),
        hr(),
        numericInput(inputId ="n_simv",
                    label = "Number of simulation runs",
                    value = 3,
                    min = 3,
                    max = 100,
                    step = 1),
        hr(),
        p("To run grid search after changing parameters use"),
        actionButton(inputId = "update_grid",
                    label = "Apply",
                    icon = icon("refresh")),
        hr(),
        p("Send all parameters to fit tab"),
        actionButton(inputId = "send_params",
                    label = "To Fit",
                    icon = icon("share-square")),
        br(),
        hr(),
        p("By Math Knights")
      ),
      # panel for grid search
      conditionalPanel(condition = "input.tabb == 'info'",
                       p(strong("Math Knights")),
                       hr(),
                       p("The Head of Model", 
                         a(href="http://www.iam.fmph.uniba.sk/ospm/Harman/", "Radoslav Harman"), 
                         "and colective."),
                       hr(),
                       p("Technical Implementation etc.", 
                         a(href="https://www.linkedin.com/in/samuel-hudec-bb7a72108/", "Samuel Hudec.")),
                       p("We are open to any valid remark please do not hesitate to contact me.")
        )
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
          plotlyOutput("best_fit_5"),
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
        tabPanel(title = "info",
          withMathJax(includeMarkdown("info.md")),
          icon = icon("file-alt"), value = "info"
          )
        )
      )
    )
  )
)
