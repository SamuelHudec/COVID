library(shiny)

# load static parameters
source("config.R")

# load neural functions
source("../harmans_code.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$sim_plots <- renderPlot({
    # draw the simulated plots with dynamic parametes
    withProgress({
      setProgress(message = "Processing ...")
      corona_explore(seq(input$b0v[1], input$b0v[2], by = b0v_step), 
                     input$gammav, 
                     seq(input$tmaxv[1], input$tmaxv[2], by = tmaxv_step))
    })
  })
})
