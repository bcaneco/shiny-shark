library(shiny)

shinyUI(
  navbarPage("Impact of longlining in sharks: simulation of mitigation measures",
             
             tabPanel("Input Distributions",
                      fluidRow(
                        h3("Catch model"),
                       
                        column(4, wellPanel(
                               h4("Shallow hooks"),
                               sliderInput("shl_mu", "Expected Value",  
                                           min = 0, max = 1, value = 0.02),
                               sliderInput("shl_sd", "Standard deviation:",  
                                           min = 0, max = 1, value = 0.06)
                        )),
                        column(4, wellPanel(
                          h4("Deep hooks"),
                          sliderInput("deep_mu", "Expected Value",  
                                      min = 0, max = 1, value = 0.02),
                          sliderInput("deep_sd", "Standard deviation:",  
                                      min = 0, max = 1, value = 0.06)
                        )),
                        column(4,wellPanel(
                          h4("Shark-lines hooks"),
                          sliderInput("shkln_mu", "Expected Value",  
                                      min = 0, max = 1, value = 0.02),
                          sliderInput("shkln_sd", "Standard deviation:",  
                                      min = 0, max = 1, value = 0.06)
                        ))),
                      
                      fluidRow(
                        h3("Fate model")
                        
                      )
             ),
             
             tabPanel("Management Scenarios"),
             tabPanel("Simulation Outputs")
             
  ))


