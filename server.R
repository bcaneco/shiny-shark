
# Preamble ~~~~~~~~~
library(shiny)
require(ggplot2)
require(dplyr)
require(data.table)

source("code/aux_functions.r")


# set theme for ggplot
theme_set(theme_bw())


# R components to output
shinyServer(function(input, output) {

  # render a plot for the catches
  output$catchPlot <- renderPlot({
    
    # join hyperparameters of catches in a list (catch per 100 hooks)
    ctby <- list(shkln  = list(mu_shkln = input$mu_shkln, sd_shkln = input$sd_shkln, label = "Shark line"),
                 shllwR = list(mu_shll  = input$mu_shll,  sd_shll  = input$sd_shll,  label = "Shallow"),
                 deep   = list(mu_deep  = input$mu_deep,  sd_deep  = input$sd_deep,  label = "Deep"))
    
    
    print(plot.cbty(ctby))
    
  })
  
  

})
