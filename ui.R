library(shiny)


# Define input widget for lognormal distribution
logN.Input <- function(title, prefix, mu.value, sd.value){
  column(2, wellPanel(
    h4(title),
    sliderInput(paste0(prefix, "_mu"), "Expected Value:", min = 0, max = 1, step = 0.1, value = mu.value),
    sliderInput(paste0(prefix, "_sd"), "Standard deviation:", min = 0, max = 1, step = 0.1, value = sd.value)
  ))
}



shinyUI(

  navbarPage("Impact of longlining in sharks: simulation of mitigation measures",
             
             tabPanel("Input Distributions",
                      
                      tags$style(type="text/css",
                                 "label {font-size: 12px;}",
                                 ".recalculating {opacity: 1.0;}"
                      ),
                      
                      tabsetPanel(
                        tabPanel("Catch Model",
                                 fluidRow(column(6, plotOutput("catchPlot"))),
                                 fluidRow(
                                   logN.Input("Shark-line Hooks", "shkln", mu.value = 0.1, sd.value = 0.6),
                                   logN.Input("Shallow Hooks", "shll", mu.value = 0.001, sd.value = 0.06),
                                   logN.Input("Deep Hooks", "deep", mu.value = 0.7, sd.value = 0.06)
                                 )),
                        
                        tabPanel("Fate Model",
                                 fluidRow()
                        ))),
             
             tabPanel("Management Scenarios"),
             tabPanel("Simulation Outputs")
             
  ))


