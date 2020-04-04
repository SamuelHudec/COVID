library(shiny)
library(tidyverse)
library(plotly)

# load static parameters
source("config.R")

# load neural functions
source("model.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # grid search controls ####
  # parameters
  b0v = reactive({
    seq(input$b0v[1], input$b0v[2], by = b0v_step)
  })
  
  tmaxv = reactive({
    seq(input$tmaxv[1], input$tmaxv[2], by = tmaxv_step)
  })
  
  # reacrtive simulationg
  sim_data = reactive({
    
    input$update
    # draw the simulated plots with dynamic parametes
    isolate({
      withProgress({
        setProgress(message = "Processing ...")
        sims = corona_explore(b0v(),
                              input$gammav, 
                              tmaxv())
      })
      sims
    })
  })
  
  # one model controls ####
  # extract best parameters from grid
  best_params = reactive({
    Vch = sim_data()$Vch[,1,]
    cor = which(1/Vch == max(1/Vch), arr.ind = TRUE)
    c(b0v()[cor[1]], input$gammav, tmaxv()[cor[2]])
  })
  
  # update control panel for one fit by best parameters
  observe({
    updateNumericInput(session, "b0", value = best_params()[1])
    updateNumericInput(session, "gamma", value = best_params()[2])
    updateNumericInput(session, "tmax", value = best_params()[3])
    })
  
  # reactive one model fit
  fit_data = reactive({
    # todo: replace by input$update
    if(input$b0 != 0){
      fit = corona_sim(c(input$b0, input$gamma, input$tmax))
    }else{
      fit = corona_sim(c(best_params()[1], best_params()[2], best_params()[3]))
    }
    df = data.frame(days = 1:length(fit$Zt), 
                    actual = cumsum(fit$Ct), 
                    fit = cumsum(fit$Zt),
                    Zt = fit$Zt, Ct = fit$Ct)
    df
  })
  
  # grid tab plots ####
  output$sim_err <- renderPlotly({
    s = sim_data()
    plot_ly(y = tmaxv(), x = b0v(), z = t(1/(s$Vch[,1,])), type = "contour",
            contours = list(showlabels = TRUE)) %>%
      layout(title = "1/Err",
             xaxis = list(title = "b0v"),
             yaxis = list(title = "tmaxv"))
  })
  
  output$sim_tests <- renderPlotly({
    s = sim_data()
    plot_ly(y = tmaxv(), x = b0v(), z = t(s$Vpp[,1,]), type = "contour",
            contours = list(showlabels = TRUE),
            colorscale = "Greens") %>%
      layout(title = "Percento pozitivnych testov",
             xaxis = list(title = "b0v"),
             yaxis = list(title = "tmaxv"))
  })

  output$sim_infi <- renderPlotly({
    s = sim_data()
    plot_ly(y = tmaxv(), x = b0v(), z = t(s$VNi[,1,]), type = "contour",
            contours = list(showlabels = TRUE),
            colorscale = "Blues") %>%
      layout(title = "Počet infikovaných jedincov",
             xaxis = list(title = "b0v"),
             yaxis = list(title = "tmaxv"))
  })

  output$sim_deaths <- renderPlotly({
    s = sim_data()
    plot_ly(y = tmaxv(), x = b0v(), z = t(s$Vs[,1,]), type = "contour",
            contours = list(showlabels = TRUE),
            colorscale = "Reds") %>%
      layout(title = "Počet umrti",
             xaxis = list(title = "b0v"),
             yaxis = list(title = "tmaxv"))
  })
  
  # fit tab plots ####
  output$best_fit_1 <- renderPlotly({
    ggplotly(
      fit_data() %>% select(days, actual, fit) %>% 
        gather("legend", "value", -days) %>%
        ggplot(aes(x = days, y = value, col = legend)) +
        theme_minimal() +
        labs(y = "Počet infikovaných cumulatívne",
             x = "dni") +
        geom_point() + 
        geom_line()
    ) %>% layout(title = paste("Najlepší Fit cummulatívne"))
  })
  
  output$best_fit_2 <- renderPlotly({
    ggplotly(
      fit_data() %>% select(days, Ct, Zt) %>% 
        gather("legend", "value", -days) %>%
        ggplot(aes(x = days, y = value, col = legend)) +
        theme_minimal() +
        labs(y = "Počet infikovaných denne",
             x = "dni") +
        geom_point() + 
        geom_line()
    ) %>% layout(title = paste("Najlepší Fit denne"))
  })
})
