
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
    ctby <- list(shkln  = list(mu_shkln = input$shkln_mu, sd_shkln = input$shkln_sd, label = "Shark-line"),
                 shllwR = list(mu_shll  = input$shll_mu,  sd_shll  = input$shll_sd,  label = "Shallow"),
                 deep   = list(mu_deep  = input$deep_mu,  sd_deep  = input$deep_sd,  label = "Deep"))
    
    print(plot.cbty(ctby))
    
  })
  
  
#   output$distPlot <- renderPlot({
# 
#     # generate bins based on input$bins from ui.R
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
#   })

})
