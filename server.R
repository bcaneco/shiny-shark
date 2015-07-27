
library(shiny)

shinyServer(function(input, output) {

  # render a plot for the catches
  output$catchPlot <- renderPlot({
    x <- seq(1,3)
    y <- c(input$shll_mu, input$deep_mu, input$shkln_mu)
    par(mar = c(5, 4, 0, 1))
    plot(x, y, main = "")
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
