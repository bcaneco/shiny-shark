
# Preamble ~~~~~~~~~
library(shiny)
require(ggplot2)
require(dplyr)
require(data.table)
require(reshape2)

source("code/aux_functions.r")
source("data/MC_Analysis_InputDistributionsHyperpars_FAL&OCS.r")

# set theme for ggplot
theme_set(theme_bw())



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fake MC outputs for place holding results
MC.fakeData <- data.frame(
  Scenario = c(rep("Base.SQ", 1000), rep("No_Sharkline", 1000), rep("No_wire", 1000), rep("Only_Circle", 1000), rep("No_Sallow", 1000)),
  Catch = c(rpois(1000, 70000), rpois(1000, 65000), rpois(1000, 50000), rpois(1000, 55000), rpois(1000, 60000)),
  M_total = c(rpois(1000, 50000), rpois(1000, 40000), rpois(1000, 30000), rpois(1000, 35000), rpois(1000, 45000))
)

MC.fakeData <- mutate(MC.fakeData, M_ret_gut = M_total * 0.2,  M_ret_lip = M_total * 0.15, 
                      M_water_lip = M_total * 0.1, M_water_gut = M_total * 0.05,
                      M_Boff_lip = M_total * 0.15, M_Boff_gut = M_total * 0.1,
                      M_boat_lip = M_total * 0.1, M_boat_gut = M_total * 0.15,
                      Mort_rate = M_total/Catch)

MC.fakeDataSummary <- group_by(MC.fakeData, Scenario) %>% summarise("10th Perc" = signif(quantile(Mort_rate, 0.1), 2), 
                                              "50th Perc" = signif(quantile(Mort_rate, 0.5), 2), 
                                              "90th Perc" = signif(quantile(Mort_rate, 0.1), 2))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# R components to output
shinyServer(function(input, output, session) {
  
  
  # --------------------------------------------------------------------------------------------- #
  # ----    Setting species-specific hyperparameter values based on several studies        ------ #
  # --------------------------------------------------------------------------------------------- #
  # 
  # After choosing the species for the reference hyper values, the user is able to change them through the slidebars
  observe({
    
    hypParVals <- switch(input$spp,
                         "Oceanic whitetip shark" = inputsHyperPars$OCS,
                         "Silky shark" = inputsHyperPars$FAL)
    
    updateSliderInput(session, "E_shkln",  value = hypParVals$cbty$shkln$E_shkln)
    updateSliderInput(session, "cv_shkln", value = hypParVals$cbty$shkln$cv_shkln)
    updateSliderInput(session, "E_shll",  value = hypParVals$cbty$shllwR$E_shll)
    updateSliderInput(session, "cv_shll", value = hypParVals$cbty$shllwR$cv_shll)
    updateSliderInput(session, "E_deep",  value = hypParVals$cbty$deep$E_deep)
    updateSliderInput(session, "cv_deep", value = hypParVals$cbty$deep$cv_deep)
    
    updateSliderInput(session, "p_LHP.J", value = hypParVals$lhk$j$p_j)
    updateSliderInput(session, "cv_LHP.J", value = hypParVals$lhk$j$cv_j)
    updateSliderInput(session, "p_LHP.C", value = hypParVals$lhk$c$p_c)
    updateSliderInput(session, "cv_LHP.C", value = hypParVals$lhk$c$cv_c)
    updateSliderInput(session, "p_LHP.T", value = hypParVals$lhk$t$p_t)
    updateSliderInput(session, "cv_LHP.T", value = hypParVals$lhk$t$cv_t)
    
    updateSliderInput(session, "p_BOP.ML", value = hypParVals$bo$ML$p_ML)
    updateSliderInput(session, "cv_BOP.ML", value = hypParVals$bo$ML$cv_ML)
    updateSliderInput(session, "p_BOP.MG", value = hypParVals$bo$MG$p_MG)
    updateSliderInput(session, "cv_BOP.MG", value = hypParVals$bo$MG$cv_MG)
    updateSliderInput(session, "p_BOP.WL", value = hypParVals$bo$WL$p_WL)
    updateSliderInput(session, "cv_BOP.WL", value = hypParVals$bo$WL$cv_WL)
    updateSliderInput(session, "p_BOP.WG", value = hypParVals$bo$WG$p_WG)
    updateSliderInput(session, "cv_BOP.WG", value = hypParVals$bo$WG$cv_WG)
    
    updateSliderInput(session, "p_BOM.L", value = hypParVals$mbo$L$p_L)
    updateSliderInput(session, "cv_BOM.L", value = hypParVals$mbo$L$cv_L)
    updateSliderInput(session, "p_BOM.G", value = hypParVals$mbo$G$p_G)
    updateSliderInput(session, "cv_BOM.G", value = hypParVals$mbo$G$cv_G)
    
    updateSliderInput(session, "p_RM.L", value = hypParVals$mrt$L$p_L)
    updateSliderInput(session, "cv_RM.L", value = hypParVals$mrt$L$cv_L)
    updateSliderInput(session, "p_RM.G", value = hypParVals$mrt$G$p_G)
    updateSliderInput(session, "cv_RM.G", value = hypParVals$mrt$G$cv_G)
    
    updateSliderInput(session, "p_WRP", value = hypParVals$iw$IwR$p_iw)
    updateSliderInput(session, "cv_WRP", value = hypParVals$iw$IwR$cv_iw)
    
    updateSliderInput(session, "p_URM.WL", value = hypParVals$mrl$LW$p_LW)
    updateSliderInput(session, "cv_URM.WL", value = hypParVals$mrl$LW$cv_LW)
    updateSliderInput(session, "p_URM.WG", value = hypParVals$mrl$GW$p_GW)
    updateSliderInput(session, "cv_URM.WG", value = hypParVals$mrl$GW$cv_GW)
    updateSliderInput(session, "p_URM.LL", value = hypParVals$mrl$LB$p_LB)
    updateSliderInput(session, "cv_URM.LL", value = hypParVals$mrl$LB$cv_LB)
    updateSliderInput(session, "p_URM.LG", value = hypParVals$mrl$GB$p_GB)
    updateSliderInput(session, "cv_URM.LG", value = hypParVals$mrl$GB$cv_GB)
    
  })
  

  
  
  # --------------------------------------------------------------------------------------------- #
  # ---- Setting interactively the upper limits of slidebars for the CV input parameters   ------ #
  # --------------------------------------------------------------------------------------------- #
  #
  # The constrain CV < sqrt(1-mean)/mean in Beta dstn forces an upper limit in the user's choice of CV
  observe({
    updateSliderInput(session, "cv_LHP.C", max = BetaCV_UppLim(input$p_LHP.C))
    updateSliderInput(session, "cv_LHP.J", max = BetaCV_UppLim(input$p_LHP.J))
    updateSliderInput(session, "cv_LHP.T", max = BetaCV_UppLim(input$p_LHP.T))
    updateSliderInput(session, "cv_BOP.ML", max = BetaCV_UppLim(input$p_BOP.ML))
    updateSliderInput(session, "cv_BOP.MG", max = BetaCV_UppLim(input$p_BOP.MG))
    updateSliderInput(session, "cv_BOP.WL", max = BetaCV_UppLim(input$p_BOP.WL))
    updateSliderInput(session, "cv_BOP.WG", max = BetaCV_UppLim(input$p_BOP.WG))
    updateSliderInput(session, "cv_BOM.L", max = BetaCV_UppLim(input$p_BOM.L))
    updateSliderInput(session, "cv_BOM.G", max = BetaCV_UppLim(input$p_BOM.G))
    updateSliderInput(session, "cv_RM.L", max = BetaCV_UppLim(input$p_RM.L))
    updateSliderInput(session, "cv_RM.G", max = BetaCV_UppLim(input$p_RM.G))
    updateSliderInput(session, "cv_WRP", max = BetaCV_UppLim(input$p_WRP))
    updateSliderInput(session, "cv_URM.WL", max = BetaCV_UppLim(input$p_URM.WL))
    updateSliderInput(session, "cv_URM.WG", max = BetaCV_UppLim(input$p_URM.WG))
    updateSliderInput(session, "cv_URM.LL", max = BetaCV_UppLim(input$p_URM.LL))
    updateSliderInput(session, "cv_URM.LG", max = BetaCV_UppLim(input$p_URM.LG))
    
  })
  
  
  
  
  
  # -------------------------------------------------------- #
  # ---- Elements for navPanel "Input Distributions"  ------ #
  # -------------------------------------------------------- #

  output$value3 <- renderPrint({ input$E_shkln})
  output$value4 <- renderPrint({ input$cv_shkln})
  
  
  # render a plot for input catch model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$cbtyPlot <- renderPlot({
    
    # join hyperparameters of catches in a list (catch per 100 hooks)
    ctby.hyp <- list(list(input$E_shkln, input$cv_shkln, label = "Shark line"),
                     list(input$E_shll,  input$cv_shll,  label = "Shallow"),
                     list(input$E_deep,  input$cv_deep,  label = "Deep"))
    
    print(plot.LogN(ctby.hyp))
  })
  
  
  
  # render plots for fate model input components ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  
  output$value1 <- renderPrint({ input$p_LHP.J})
  output$value2 <- renderPrint({ input$cv_LHP.J})
  
  # Lip-hook probability 
  output$LHP <- renderPlot({
    LHP.hyp <- list(list(input$p_LHP.J, input$cv_LHP.J, label = "J-Hook"),
                    list(input$p_LHP.T, input$cv_LHP.T, label = "T-Hook"),
                    list(input$p_LHP.C, input$cv_LHP.C, label = "C-Hook"))
    print(dnsPlot.beta(LHP.hyp, main = 'Lip-hooking given hook type'))
  })
  
  # Bite-off probability
  output$BOP <- renderPlot({
    BOP.hyp <- list(list(input$p_BOP.ML, input$cv_BOP.ML, label = "Mono & Lip"),
                    list(input$p_BOP.MG, input$cv_BOP.MG, label = "Mono & Gut"),
                    list(input$p_BOP.WL, input$cv_BOP.WL, label = "Wire & Lip"),
                    list(input$p_BOP.WG, input$cv_BOP.WG, label = "Wire & Gut"))
    print(dnsPlot.beta(BOP.hyp, main = 'Bite-off given hook trace & hooking location'))
  })
  
  
  # Bite-off mortality
  output$BOM <- renderPlot({
    BOM.hyp <- list(list(input$p_BOM.L, input$cv_BOM.L, label = "Lip-hooked"),
                    list(input$p_BOM.G, input$cv_BOM.G, label = "Gut-hooked"))
    print(dnsPlot.beta(BOM.hyp, main = 'Bite-off shark is dead given hooking location'))
  })

  
  # On-board retrieval mortality
  output$RM <- renderPlot({
    RM.hyp <- list(list(input$p_RM.L, input$cv_RM.L, label = "Lip-hooked"),
                    list(input$p_RM.G, input$cv_RM.G, label = "Gut-hooked"))
    print(dnsPlot.beta(RM.hyp, main = 'Retrieved shark is dead given hooking location'))
  })
  
  
  # In-water release probability
  output$WRP <- renderPlot({
    WRP.hyp <- list(list(input$p_WRP, input$cv_WRP, label = ""))
    print(dnsPlot.beta(WRP.hyp, main = 'In-water release'))
  })
    
  
  # Upon Release Mortality
  output$URM <- renderPlot({
    URM.hyp <- list(list(input$p_URM.WL, input$cv_URM.WL, label = "In-water & Lip"),
                    list(input$p_URM.WG, input$cv_URM.WG, label = "In-water & Gut"),
                    list(input$p_URM.LL, input$cv_URM.LL, label = "Landed & Lip"),
                    list(input$p_URM.LG, input$cv_URM.LG, label = "Landed & Gut"))
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




