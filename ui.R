library(shiny)


# Define input widget for lognormal distribution
logN.input <- function(title, suffix, mu.value, cv.value){
  wellPanel(
    style = "padding: 5px;",
    h5(title),
    sliderInput(paste0("mu_", suffix), "Mean catch:", min = 0, max = 5, step = 0.1, value = mu.value, ticks = FALSE),
    sliderInput(paste0("cv_", suffix), "Coef. variation (%):", min = 0, max = 100, step = 5, value = cv.value, ticks = FALSE)
    )
}



# Define input widget for beta distribution
beta.input <- function(title, suffix, p.value, cv.value){
  wellPanel(
    style = "padding: 5px;",
    h5(title),
    sliderInput(paste0("p_", suffix), "Expected probability:", min = 0, max = 1, step = 0.05, value = p.value, ticks = FALSE),
    sliderInput(paste0("cv_", suffix), "Coef. variation (%):", min = 0, max = 100, step = 5, value = cv.value, ticks = FALSE)
  )
}





shinyUI(

  navbarPage("Impact of longlining in sharks: simulation of mitigation measures",
             
             tabPanel("Input Distributions",
                      
                      tags$style(type="text/css",
                                 "label {font-size: 12px;}",
                                 ".recalculating {opacity: 1.0;}"
                      ),
                      
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     #h3("Hey, try me out"),
                                     selectInput("spp", label = h4("Select the species"), 
                                                 choices = list("Oceanic whitetip shark" = 1, "Silky shark" = 2), 
                                                 selected = 1),
                                     em("NOTE: The upper limits of the CV's for the probability inputs are specified by the Beta distn constraint that 
                                        CV < sqrt((1-mean)/mean)")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Catch Model",
                                     h4("Catch Rate per 100 hooks in:"),
                                     fluidRow(
                                       column(3, logN.input("Shark lines", "shkln", mu.value = 0.1, cv.value = 50), offset = 2),
                                       column(3, logN.input("Shallow Hooks", "shll", mu.value = 0.2, cv.value = 60)),
                                       column(3, logN.input("Deep Hooks", "deep", mu.value = 0.7, cv.value = 30))
                                       ),
                                     fluidRow(column(10, plotOutput("cbtyPlot"), offset = 2))
                                     ),
                            
                            tabPanel("Fate Model",
                                     h4("Probability of lip hook (vs. gut hook) given:"),
                                     fluidRow(
                                       column(3, beta.input("C-Hook", "LHP.C", p.value = 0.1, cv.value = 25), offset = 2),
                                       column(3, beta.input("J-Hook", "LHP.J", p.value = 0.2, cv.value = 25)),
                                       column(3, beta.input("T-Hook", "LHP.T", p.value = 0.3, cv.value = 25))
                                     ),

                                     fluidRow(column(10, plotOutput("LHP"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of bite-off given:"),
                                     fluidRow(
                                       column(3, beta.input("Mono Leader & gut-hooked", "BOP.MG", p.value = 0.3, cv.value = 25)),
                                       column(3, beta.input("Mono Leader & lip-hooked", "BOP.ML", p.value = 0.1, cv.value = 25)),
                                       column(3, beta.input("Wire Leader & gut-hooked", "BOP.WG", p.value = 0.8, cv.value = 10)),
                                       column(3, beta.input("Wire Leader & lip-hooked", "BOP.WL", p.value = 0.5, cv.value = 25))
                                     ),
                                     fluidRow(column(10, plotOutput("BOP"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality given bite-off and:"),
                                     fluidRow(
                                       column(3, beta.input("Gut-hooked", "BOM.G", p.value = 0.1, cv.value = 25), offset = 3),
                                       column(3, beta.input("Lip-hooked", "BOM.L", p.value = 0.4, cv.value = 10))
                                     ), 
                                     fluidRow(column(10, plotOutput("BOM"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality at retrieval given:"),
                                     fluidRow(
                                       column(3, beta.input("Gut-hooked", "RM.G", p.value = 0.1, cv.value = 25), offset = 3),
                                       column(3, beta.input("Lip-hooked", "RM.L", p.value = 0.3, cv.value = 10))
                                     ), 
                                     fluidRow(column(10, plotOutput("RM"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of release in-water (vs. brought-on then released):"),
                                     fluidRow(column(3, beta.input("", "WRP", p.value = 0.1, cv.value = 25), offset = 5)), 
                                     fluidRow(column(9, plotOutput("WRP"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality upon release given:"),
                                     fluidRow(column(3, beta.input("In-water release & gut-hooked", "URM.WG", p.value = 0.4, cv.value = 25)),
                                              column(3, beta.input("In-water release & lip-hooked", "URM.WL", p.value = 0.1, cv.value = 25)),
                                              column(3, beta.input("Landed release & gut-hooked", "URM.LG", p.value = 0.2, cv.value = 25)),
                                              column(3, beta.input("Landed release & lip-hooked", "URM.LL", p.value = 0.9, cv.value = 25))
                                     ),
                                     fluidRow(column(10, plotOutput("URM"), offset = 2))
                            ))))),
             
             tabPanel("Management Scenarios"),
             
             tabPanel("Simulation Outputs",
                      tabsetPanel(
                        tabPanel("Contrast Plots",
                                 fluidRow(column(5, plotOutput("CM_baseVsNoShallow"), offset = 1),
                                          column(5, plotOutput("CM_baseVsNoShkline"))
                                          )),
                        
                        tabPanel("Contrast summary tables",
                                 
                                 h4("Overall mortality rate (deaths/catch)"),
                                 tableOutput("table"))
                      ))
             
  ))


