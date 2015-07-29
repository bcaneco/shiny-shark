
# Preamble ~~~~~~~~~
library(shiny)
require(ggplot2)
require(dplyr)
require(data.table)
require(reshape2)

source("code/aux_functions.r")


# set theme for ggplot
theme_set(theme_bw())



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fake MC outputs for place holding results
MC.fakeData <- data.frame(
  Scenario = c(rep("Base.SQ", 1000), rep("No_Sharkline", 1000), rep("No_wire", 1000), rep("Only_Circle", 1000), rep("No_Sallow", 1000)),
  Catch = c(rpois(1000, 70000), rpois(1000, 65000), rpois(1000, 50000), rpois(1000, 55000), rpois(1000, 60000)),
  M_total = c(rpois(1000, 50000), rpois(1000, 40000), rpois(1000, 30000), rpois(1000, 35000), rpois(1000, 45000))
)

MC.fakeData <- mutate(fakeMC, M_ret_gut = M_total * 0.2,  M_ret_lip = M_total * 0.15, 
                      M_water_lip = M_total * 0.1, M_water_gut = M_total * 0.05,
                      M_Boff_lip = M_total * 0.15, M_Boff_gut = M_total * 0.1,
                      M_boat_lip = M_total * 0.1, M_boat_gut = M_total * 0.15,
                      Mort_rate = M_total/Catch)

MC.fakeDataSummary <- group_by(MC.fakeData, Scenario) %>% summarise("10th Perc" = signif(quantile(Mort_rate, 0.1), 2), 
                                              "50th Perc" = signif(quantile(Mort_rate, 0.5), 2), 
                                              "90th Perc" = signif(quantile(Mort_rate, 0.1), 2))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# R components to output
shinyServer(function(input, output) {
  
  
  # -------------------------------------------------------- #
  # ---- Elements for navPanel "Input Distributions"  ------ #
  # -------------------------------------------------------- #

  # render a plot for input catch model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$cbtyPlot <- renderPlot({
    
    # join hyperparameters of catches in a list (catch per 100 hooks)
    ctby.hyp <- list(list(input$mu_shkln, input$cv_shkln, label = "Shark line"),
                     list(input$mu_shll,  input$cv_shll,  label = "Shallow"),
                     list(input$mu_deep,  input$cv_deep,  label = "Deep"))
    
    print(plot.LogN(ctby.hyp))
  })
  
  
  
  # render plots for fate model input components ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
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
  
  
  
  
  
  # --------------------------------------------------------- #
  # ---- Elements for navPanel "Management Scenarios"  ------ #
  # --------------------------------------------------------- #
  
  
  
  
  
  
  
  # -------------------------------------------------------- #
  # ----  Elements for navPanel "Simulation Outputs"  ------ #
  # -------------------------------------------------------- #
  
  output$CM_baseVsNoShallow <- renderPlot({
    print(plot.catchAndMort(filter(MC.fakeData, Scenario %in% c("Base.SQ", "No_Sallow")),
                                   xlab = 'Number of sharks'))
  })
  
  output$CM_baseVsNoShkline <- renderPlot({
    print(plot.catchAndMort(filter(MC.fakeData, Scenario %in% c("Base.SQ", "No_Sharkline")),
                            xlab = 'Number of sharks'))
  })
  
  
  
  # Generate a summary table of results
  output$table <- renderTable({
    MC.fakeDataSummary
  })
  
})




