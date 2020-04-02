library(shiny)
library(tidyverse)
library(plotly)

# load static parameters
source("config.R")

# load neural functions
source("model.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # parameters
  b0v = reactive({
    seq(input$b0v[1], input$b0v[2], by = b0v_step)
  })
  
  tmaxv = reactive({
    seq(input$tmaxv[1], input$tmaxv[2], by = tmaxv_step)
  })
  
  # reacrtive simulationg
  sim_data = reactive({
    # draw the simulated plots with dynamic parametes
    withProgress({
      setProgress(message = "Processing ...")
      sims = corona_explore(b0v(),
                            input$gammav, 
                            tmaxv())
      sims
    })
  })
  
  # reactive fitting
  fit_data = reactive({
    Vch = sim_data()$Vch[,1,]
    cor = which(1/Vch == max(1/Vch), arr.ind = TRUE)
    # params
    b0 = b0v()[cor[1]]
    tmax = tmaxv()[cor[2]]
    fit = corona_sim(c(b0, input$gammav, tmax))
    df = data.frame(days = 1:length(fit$Zt), 
                    actual = cumsum(fit$Ct), 
                    fit = cumsum(fit$Zt),
                    Zt = fit$Zt, Ct = fit$Ct)
    df
  })
  
  # plotting part
  output$sim_err <- renderPlotly({
    s = sim_data()
    plot_ly(x = tmaxv(), y = b0v(), z = 1/(s$Vch[,1,]), type = "contour",
            contours = list(showlabels = TRUE)) %>%
      colorbar(title = "1/Err") %>%
      layout(title = paste("1/Err, gamma2 =", input$gammav),
             xaxis = list(title = "tmaxv"),
             yaxis = list(title = "b0v"))
  })
  
  output$sim_tests <- renderPlotly({
    s = sim_data()
    plot_ly(x = tmaxv(), y = b0v(), z = s$Vpp[,1,], type = "contour",
            contours = list(showlabels = TRUE),
            colorscale = "Greens") %>%
      colorbar(title = "Percent") %>%
      layout(title = paste("Percento pozitivnych testov, gamma2 =", input$gammav),
             xaxis = list(title = "tmaxv"),
             yaxis = list(title = "b0v"))
  })

  output$sim_infi <- renderPlotly({
    s = sim_data()
    plot_ly(x = tmaxv(), y = b0v(), z = s$VNi[,1,], type = "contour",
            contours = list(showlabels = TRUE),
            colorscale = "Blues") %>%
      layout(title = paste("Počet infikovaných jedincov, gamma2 =", input$gammav),
             xaxis = list(title = "tmaxv"),
             yaxis = list(title = "b0v"))
  })

  output$sim_deaths <- renderPlotly({
    s = sim_data()
    plot_ly(x = tmaxv(), y = b0v(), z = s$Vs[,1,], type = "contour",
            contours = list(showlabels = TRUE),
            colorscale = "Reds") %>%
      layout(title = paste("Počet umrti, gamma2 =", input$gammav),
             xaxis = list(title = "tmaxv"),
             yaxis = list(title = "b0v"))
  })
  
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
