library(shiny)


# Define input widget for lognormal distribution
logN.input <- function(title, suffix, E.value, cv.value){
  wellPanel(
    style = "padding: 5px;",
    h5(title),
    sliderInput(paste0("E_", suffix), "Mean catch:", min = 0, max = 2, step = 0.05, value = E.value, ticks = FALSE),
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
                      
#                       tags$head(
#                         tags$link(rel = "stylesheet", type = "text/css", href = "slidebarColours.css")
#                       ),
                      
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     selectInput("spp", label = h4("Choose a species:"), 
                                                 choices = list("Oceanic whitetip shark", "Silky shark"), 
                                                 selected = "Oceanic whitetip shark"),
                                     em("NOTE 1: The initial Hyperparameter values are based on a compilation of several studies"),
                                     hr(),
                                     em("NOTE 2: The upper limits of CV's for probability inputs are specified by the Beta distn constraint: 
                                        CV < sqrt((1-mean)/mean)")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Catch Model",
                                     h4("Catch Rate per 100 hooks in:"),
                                     fluidRow(
                                       column(3, logN.input("Shark lines", "shkln", E.value = 0.620, cv.value = 5), offset = 2),
                                       column(3, logN.input("Shallow Hooks", "shll", E.value = 0.008, cv.value = 4)),
                                       column(3, logN.input("Deep Hooks", "deep", E.value = 0.016, cv.value = 3))
                                       ),
                                     
                                     fluidRow(column(3, verbatimTextOutput("value3"))),
                                     fluidRow(column(3, verbatimTextOutput("value4"))),
                                     
                                     fluidRow(column(10, plotOutput("cbtyPlot"), offset = 2))
                                     ),
                            
                            tabPanel("Fate Model",
                                     h4("Probability of lip hoo
                                        k (vs. gut hook) given:"),
                                     fluidRow(
                                       column(3, beta.input("C-Hook", "LHP.C", p.value = 0.9, cv.value = 10), offset = 2),
                                       column(3, beta.input("J-Hook", "LHP.J", p.value = 0.3, cv.value = 43)),
                                       column(3, beta.input("T-Hook", "LHP.T", p.value = 0.33, cv.value = 40))
                                     ),
                                     
                                     fluidRow(column(10, plotOutput("LHP"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of bite-off given:"),
                                     fluidRow(
                                       column(3, beta.input("Mono Leader & gut-hooked", "BOP.MG", p.value = 0.33, cv.value = 10)),
                                       column(3, beta.input("Mono Leader & lip-hooked", "BOP.ML", p.value = 0.33, cv.value = 10)),
                                       column(3, beta.input("Wire Leader & gut-hooked", "BOP.WG", p.value = 0.01, cv.value = 90)),
                                       column(3, beta.input("Wire Leader & lip-hooked", "BOP.WL", p.value = 0.01, cv.value = 90))
                                     ),
                                     fluidRow(column(10, plotOutput("BOP"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality given bite-off and:"),
                                     fluidRow(
                                       column(3, beta.input("Gut-hooked", "BOM.G", p.value = 0.06, cv.value = 80), offset = 3),
                                       column(3, beta.input("Lip-hooked", "BOM.L", p.value = 0.03, cv.value = 95))
                                     ), 
                                     fluidRow(column(10, plotOutput("BOM"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality at retrieval given:"),
                                     fluidRow(
                                       column(3, beta.input("Gut-hooked", "RM.G", p.value = 0.19, cv.value = 5), offset = 3),
                                       column(3, beta.input("Lip-hooked", "RM.L", p.value = 0.19, cv.value = 5))
                                     ), 
                                     fluidRow(column(10, plotOutput("RM"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of release in-water (vs. brought-on then released):"),
                                     fluidRow(column(3, beta.input("", "WRP", p.value = 0.5, cv.value = 30), offset = 5)), 
                                     fluidRow(column(9, plotOutput("WRP"), offset = 2)),
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality upon release given:"),
                                     fluidRow(column(3, beta.input("In-water release & gut-hooked", "URM.WG", p.value = 0.15, cv.value = 25)),
                                              column(3, beta.input("In-water release & lip-hooked", "URM.WL", p.value = 0.15, cv.value = 25)),
                                              column(3, beta.input("Landed release & gut-hooked", "URM.LG", p.value = 0.19, cv.value = 20)),
                                              column(3, beta.input("Landed release & lip-hooked", "URM.LL", p.value = 0.19, cv.value = 20))
                                     ),
                                     fluidRow(column(10, plotOutput("URM"), offset = 2))
                            ))))),
             
             tabPanel("Management Scenarios",
                      
                      fluidRow(column(3, verbatimTextOutput("value1"))),
                      fluidRow(column(3, verbatimTextOutput("value2")))
                      
                      ),
             
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


