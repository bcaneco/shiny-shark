
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

  # render a plot for catch model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$cbtyPlot <- renderPlot({
    
    # join hyperparameters of catches in a list (catch per 100 hooks)
    ctby.hyp <- list(list(input$mu_shkln, input$cv_shkln, label = "Shark line"),
                     list(input$mu_shll,  input$cv_shll,  label = "Shallow"),
                     list(input$mu_deep,  input$cv_deep,  label = "Deep"))
    
    print(plot.LogN(ctby.hyp))
  })
  
  
  
  # render plots for fate model components ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Lip-hook probability 
  output$LHP <- renderPlot({
    LHP.hyp <- list(list(input$p_LHP.J, input$n_LHP.J, label = "J-Hook"),
                    list(input$p_LHP.T, input$n_LHP.T, label = "T-Hook"),
                    list(input$p_LHP.C, input$n_LHP.C, label = "C-Hook"))
    print(dnsPlot.beta(LHP.hyp, main = 'Lip-hooking given hook type'))
  })
  
  # Bite-off probability
  output$BOP <- renderPlot({
    BOP.hyp <- list(list(input$p_BOP.ML, input$n_BOP.ML, label = "Mono & Lip"),
                    list(input$p_BOP.MG, input$n_BOP.MG, label = "Mono & Gut"),
                    list(input$p_BOP.WL, input$n_BOP.WL, label = "Wire & Lip"),
                    list(input$p_BOP.WG, input$n_BOP.WG, label = "Wire & Gut"))
    print(dnsPlot.beta(BOP.hyp, main = 'Bite-off given hook trace & hooking location'))
  })
  
  
  # Bite-off mortality
  output$BOM <- renderPlot({
    BOM.hyp <- list(list(input$p_BOM.L, input$n_BOM.L, label = "Lip-hooked"),
                    list(input$p_BOM.G, input$n_BOM.G, label = "Gut-hooked"))
    print(dnsPlot.beta(BOM.hyp, main = 'Bite-off shark is dead given hooking location'))
  })

  
  # On-board retrieval mortality
  output$RM <- renderPlot({
    RM.hyp <- list(list(input$p_RM.L, input$n_RM.L, label = "Lip-hooked"),
                    list(input$p_RM.G, input$n_RM.G, label = "Gut-hooked"))
    print(dnsPlot.beta(RM.hyp, main = 'Retrieved shark is dead given hooking location'))
  })
  
  
  # In-water release probability
  output$WRP <- renderPlot({
    WRP.hyp <- list(list(input$p_WRP, input$n_WRP, label = ""))
    print(dnsPlot.beta(WRP.hyp, main = 'In-water release'))
  })
    
  
  # Upon Release Mortality
  output$URM <- renderPlot({
    URM.hyp <- list(list(input$p_URM.WL, input$n_URM.WL, label = "In-water & Lip"),
                    list(input$p_URM.WG, input$n_URM.WG, label = "In-water & Gut"),
                    list(input$p_URM.LL, input$n_URM.LL, label = "Landed & Lip"),
                    list(input$p_URM.LG, input$n_URM.LG, label = "Landed & Gut"))
    print(dnsPlot.beta(URM.hyp, main = 'Released shark is dead given hooking location and where released'))
  })
  
})


