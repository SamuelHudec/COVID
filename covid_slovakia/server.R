library(shiny)
library(tidyverse)
library(plotly)

# load static parameters
source("config.R")
x_days = "Days from the Start of The Epidemic"

# load neural functions
source("model2.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # one model controls ####
  # extract best parameters from grid
  best_params = reactive({
    Vch = t(sim_data())
    cor = which(1/Vch == max(1/Vch), arr.ind = TRUE)
    
    c(predv()[cor[1]], b0v()[cor[2]], input$gamma1v,
      input$fin1v, input$gamma2v, input$n_simv)
  })

  # update control panel for one fit by best parameters
  observeEvent(input$send_params, {
    updateNumericInput(session, "pred", value = best_params()[1])
    updateNumericInput(session, "b0", value = best_params()[2])
    updateNumericInput(session, "gamma1", value = best_params()[3])
    updateNumericInput(session, "fin1", value = best_params()[4])
    updateNumericInput(session, "gamma2", value = best_params()[5])
    updateNumericInput(session, "n_sim", value = best_params()[6])
  }, ignoreInit = TRUE)
  
  # reactive one model fit
  fit_data = reactive({
    input$update_fit
    
    isolate({
      fit = corona_sim2(input$pred, 
                        c(input$b0, input$gamma1, input$fin1),
                        c(input$b0, input$gamma2, Inf),
                        input$n_sim, pic = T)
    
      # get parameters
      i1 = fit$I1
      ii = fit$I
      tmaxx = fit$tmax
      n_I1 = nrow(i1)
      Ctc <- cumsum(fit$Ct)
      
      # calculate neccessary metrics
      rad1 <- hist(i1[, tmaxx + 5], 
                   breaks = 0:(tmaxx + 1) - 0.5, 
                   plot = FALSE)$counts[2:(tmaxx + 1)]
      rad1c <- cumsum(rad1)
      
      I1.su51 <- I1.su26 <- I1.su01 <- I1.boliI <- matrix(0, nrow = n_I1, ncol = tmaxx)
      for (i in 1:n_I1) {
        I1.boliI[i, ] <- sign(cumsum(abs(i1[i, 1:tmaxx])))
      }
  
      for (i in 1:n_I1) {
        I1.su01[i, ] <- sign(as.integer(i1[i, 1:tmaxx] != 0))
        I1.su26[i, ] <- sign(as.integer(abs(i1[i, 1:tmaxx]) > 0.25))
        I1.su51[i, ] <- sign(as.integer(abs(i1[i, 1:tmaxx]) > 0.51))
      }
      
      I1.boliI <- apply(I1.boliI, 2, sum)
      I1.su01 <- apply(I1.su01, 2, sum)
      I1.su26 <- apply(I1.su26, 2, sum)
      I1.su51 <- apply(I1.su51, 2, sum)
      
      his <- sign(ii[, tmaxx + 5])*ii[, tmaxx + 2]
      
      # put togeather
      df1 = data.frame(days = 1:tmaxx, actual = Ctc, fit = rad1c,
                       actualv = fit$Ct, fitv = rad1,
                       days0 = 0:(tmaxx - 1), inf = I1.boliI,
                       I1.su01 = I1.su01, I1.su26 = I1.su26, I1.su51 = I1.su51)
      df2 = data.frame(hist = his[his != 0])
      df2$cut <- cut(df2$hist, seq(0, 10, by=1))
  
      dfs = list(df1 = df1, df2 = df2)
      dfs
    })
  })
  
  # fit tab plots ####
  output$best_fit_1 <- renderPlotly({
    dd = fit_data()
    ggplotly(
      dd$df1 %>% 
        select(days, actual, fit) %>% 
        gather("legend", "value", -days) %>%
        ggplot(aes(x = days, y = value, col = legend)) +
        theme_minimal() +
        labs(y = "Cumulative Number of Positive Tests",
             x = x_days) +
        geom_point() + 
        geom_line()
    )
  })
  
  output$best_fit_2 <- renderPlotly({
    dd = fit_data()
    ggplotly(
      dd$df1 %>% 
        ggplot(aes(x = days0, y = inf)) + 
        theme_minimal() +
        labs(y = "Cumulative number of symptomatically infected",
             x = x_days) +
        geom_point(alpha = 0.6) + 
        geom_line(alpha = 0.2)
    )
  })
  
  output$best_fit_33 <- renderPlotly({
    dd = fit_data()
    ggplotly(
      dd$df1 %>% 
        ggplot(aes(x = days, y = I1.su51)) +
        theme_minimal() +
        labs(y = "Number of symptomatically infected",
             x = x_days) +
        geom_point(alpha = 0.6, col = "#666666") + 
        geom_line(alpha = 0.2)
    )
  })
  
  output$best_fit_32 <- renderPlotly({
    dd = fit_data()
    ggplotly(
      dd$df1 %>% 
        ggplot(aes(x = days, y = I1.su26)) +
        theme_minimal() +
        labs(y = "Number of symptomatically infected",
             x = x_days) +
        geom_point(alpha = 0.6, col = "#CC0000") + 
        geom_line(alpha = 0.2)
    )
  })
  
  output$best_fit_31 <- renderPlotly({
    dd = fit_data()
    ggplotly(
      dd$df1 %>% 
        ggplot(aes(x = days, y = I1.su01)) +
        theme_minimal() +
        labs(y = "Number of symptomatically infected",
             x = x_days) +
        geom_point(alpha = 0.6, col = "#006600") + 
        geom_line(alpha = 0.2)
    )
  })
  
  output$best_fit_4 <- renderPlotly({
    dd = fit_data()
    ggplotly(
      dd$df2 %>% ggplot(aes(cut)) +
        geom_histogram(stat = "count", aes(fill = ..count..)) +
        theme_minimal() + 
        labs(x = "Age decade")
    )
  })
  
  output$best_fit_5 <- renderPlotly({
    dd = fit_data()
    ggplotly(
      dd$df1 %>% 
        select(days, actualv, fitv) %>% 
        gather("legend", "value", -days) %>%
        ggplot(aes(x = days, y = value, col = legend)) +
        theme_minimal() +
        labs(y = "Daily Number of Positive Tests",
             x = x_days) +
        geom_point() + 
        geom_line()
    )
  })
  
  # grid search controls ####
  # parameters
  b0v = reactive({
    seq(input$b0v[1], input$b0v[2], by = b0v_step)
  })
  
  predv = reactive({
    seq(input$predv[1], input$predv[2], by = fin_step)
  })
  
  # reacrtive simulationg
  sim_data = reactive({
    input$update_grid
    # draw the simulated plots with dynamic parametes
    isolate({
      withProgress({
        setProgress(message = "Simulating grid search ...")
        sims = corona_explore2(predv = predv(), 
                               b0v = b0v(), 
                               gamma1 = input$gamma1v, 
                               fin1 = input$fin1v,
                               gamma2 = input$gamma2v, 
                               n = input$n_simv)
      })
      sims
    })
  })
  
  # grid tab plots ####
  output$sim_err <- renderPlotly({
    s = sim_data()
    plot_ly(y = predv(), x = b0v(), z = t(1/s), type = "contour",
            contours = list(showlabels = TRUE)) %>% 
      layout(xaxis = list(title = "Grid b0 param."), 
             yaxis = list(title = "Grid pred param."))
    
  })
})
