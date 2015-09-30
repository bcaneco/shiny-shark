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


# Define input widget for management options
mng_input <- function(id, title){
  wellPanel(
    checkboxGroupInput(id, label = h4(title),
                       choices = list("Ban Shark lines" = "NoShkln", "Ban wire trace" = "NoWire", 
                                      "Ban shallow hooks" = "NoShallow", "Restrict to Circle-hooks only" = "AllCircle"),
                       selected = NULL, inline = FALSE)
  )
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyUI(

  navbarPage("Impact of longlining in sharks: simulation of mitigation measures",
             
             
             # 1st Tab ------------------------------------------------------------------    
             
             tabPanel("Step 1: Choose species & input distributions",
                      
                      tags$style(type="text/css",
                                 "label {font-size: 12px;}",
                                 ".recalculating {opacity: 1.0;}"
                      ),
                      
                       tags$head(
                         tags$link(rel = "stylesheet", type = "text/css", href = "slidebarColours.css")
                       ),
                      
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     selectInput("spp", label = h3("Choose species"), 
                                                 choices = list("Oceanic whitetip shark", "Silky shark"), 
                                                 selected = "Oceanic whitetip shark"),
                                     em("NOTE: Hyperparameter's default values as specified in ",
                                        a("Shelton et al. (2015)", href = "https://dl.dropboxusercontent.com/u/250971/EB-WP-02-%5BMC_sharks%5D.pdf"))
                        ),
                        mainPanel(
                          br(),
                          h3("Specify the input distributions"),
                          br(),
                          p("<Some text here>"),
                          br(),
                          tabsetPanel(
                            tabPanel("Catch Component",
                                     h4("Catch Rate per 100 hooks in:"),
                                     br(),
                                     fluidRow(
                                       column(5, 
                                              fluidRow(
                                                column(6, logN.input("Shark lines", "shkln", E.value = 0.620, cv.value = 5)),
                                                column(6, logN.input("Shallow Hooks", "shll", E.value = 0.008, cv.value = 4))
                                              ),
                                              fluidRow(
                                                column(6, logN.input("Deep Hooks", "deep", E.value = 0.016, cv.value = 3))
                                              )),
                                       br(),
                                       column(7, plotOutput("cbtyPlot"))
                                     )
                                     
                                     #fluidRow(column(3, verbatimTextOutput("value3"))),
                                     #fluidRow(column(3, verbatimTextOutput("value4"))),
                                     ),
                            
                            tabPanel("Fate Component",
                                     
                                     br(),
                                     p(em("NOTE: The upper limit of the CV for probability inputs are defined by the Beta distn constraint: 
                                        CV < sqrt((1-p)/p)")),
                                     br(),
                                     h4("Probability of lip hook (vs. gut hook) given:"),
                                     br(),
                                     fluidRow(
                                       column(5,
                                              fluidRow(
                                                column(6, beta.input("J-Hook", "LHP.J", p.value = 0.3, cv.value = 43)),
                                                column(6, beta.input("T-Hook", "LHP.T", p.value = 0.33, cv.value = 40))
                                              ),
                                              fluidRow(
                                                column(6, beta.input("C-Hook", "LHP.C", p.value = 0.9, cv.value = 10))
                                              )),
                                       br(),
                                       column(7, plotOutput("LHP"))
                                     ),
                                     
                                     hr(),
                                     
                                     h4("Probability of bite-off given:"),
                                     br(),
                                     fluidRow(
                                       column(5,
                                              fluidRow(
                                                column(6, beta.input("Mono Leader & lip-hooked", "BOP.ML", p.value = 0.33, cv.value = 10)),
                                                column(6, beta.input("Mono Leader & gut-hooked", "BOP.MG", p.value = 0.72, cv.value = 20))
                                              ),
                                              fluidRow(
                                                column(6, beta.input("Wire Leader & lip-hooked", "BOP.WL", p.value = 0.01, cv.value = 10)),
                                                column(6, beta.input("Wire Leader & gut-hooked", "BOP.WG", p.value = 0.01, cv.value = 10))
                                              )),
                                       br(),
                                       column(7, plotOutput("BOP"))
                                       ),
                                     
                                     hr(),
                                     
                                     
                                     h4("Probability of mortality given bite-off and:"),
                                     br(),
                                     fluidRow(
                                       column(5,
                                              fluidRow(
                                                column(6, beta.input("Lip-hooked", "BOM.L", p.value = 0.03, cv.value = 95)),
                                                column(6, beta.input("Gut-hooked", "BOM.G", p.value = 0.06, cv.value = 80))
                                              )),
                                       br(),
                                       column(7, plotOutput("BOM"))
                                     ),
                                     
                                     hr(),
                                     
                                     
                                     h4("Probability of mortality at retrieval given:"),
                                     br(),
                                     fluidRow(
                                       column(5,
                                              fluidRow(
                                                column(6, beta.input("Lip-hooked", "RM.L", p.value = 0.19, cv.value = 5)),
                                                column(6, beta.input("Gut-hooked", "RM.G", p.value = 0.19, cv.value = 5))
                                              )),
                                       br(),
                                       column(7, plotOutput("RM"))
                                     ),
                                     
                                     hr(),
                                     
                                     h4("Probability of release in-water (vs. brought-on then released):"),
                                     br(),
                                     fluidRow(
                                       column(5,
                                              fluidRow(
                                                column(6, beta.input("", "WRP", p.value = 0.5, cv.value = 30))
                                              )),
                                       br(),
                                       column(7, plotOutput("WRP"))
                                     ),
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality upon release given:"),
                                     br(),
                                     fluidRow(
                                       column(5,
                                              fluidRow(
                                                column(6, beta.input("In-water release & lip-hooked", "URM.WL", p.value = 0.15, cv.value = 25)),
                                                column(6, beta.input("In-water release & gut-hooked", "URM.WG", p.value = 0.19, cv.value = 20))
                                              ),
                                              fluidRow(
                                                column(6, beta.input("Landed release & lip-hooked", "URM.LL", p.value = 0.34, cv.value = 15)),
                                                column(6, beta.input("Landed release & gut-hooked", "URM.LG", p.value = 0.44, cv.value = 12))
                                              )),
                                              br(),
                                     column(7, plotOutput("URM"))
                                     )
                            ))))),
             
             
             # 2nd Tab ------------------------------------------------------------------    
             
             tabPanel("Step 2: Choose management scenario(s)",
                      
                      h3("Select management scenarios"),
                      
                      br(),
                      p("Within each management scenario frame:"),
                      tags$ul(
                        tags$li("Select one or a combination of options"), 
                        tags$li("If none of the boxes is selected, the management scenario is not considered")
                      ),
                      br(),
                      hr(),
                      
                      fluidRow(
                        column(3, mng_input("MngScn1", "Management Scenario 1")),
                        column(3, mng_input("MngScn2", "Management Scenario 2")),
                        column(3, mng_input("MngScn3", "Management Scenario 3")),
                        column(3, mng_input("MngScn4", "Management Scenario 4"))
                        ),
                      hr(),
                      
                      fluidRow(
                        column(4, numericInput("nsims", label = h3("Number of simulations"), value = 1000)),
                        column(4,  selectInput("bskSize", label = h3("Basket Size"),
                                             choices = list("20", "25", "30", "35", "40"), 
                                             selected = "30"))
                      ),
                      br(),
                      fluidRow(
                        column(4,  actionButton("simButton", "Run Simulation"))
                      ),
                      br(),
                      fluidRow(
                        column(6, verbatimTextOutput("value1"))
                        )
                      ),
             
             
             
             
             # 3nd Tab ------------------------------------------------------------------    
             
             tabPanel("Step 3: Run simulation & Outputs",
                      tabsetPanel(
                        tabPanel("Contrast Plots",
                                 h4("Monte Carlo distributions of catch and mortality under each scenario"),
                                 fluidRow(column(6, plotOutput("MCplots_catchMort", height = "650px"), offset = 2)),
                                 br(),
                                 
                                 hr(),
                                 br(),
                                 h4("Monte Carlo distributions of mortality rate (i.e. deaths/catch) under each scenario"),
                                 fluidRow(
                                   column(5, plotOutput("MCplots_MortRate", height = "650px"), offset = 2)
#                                    column(4, 
#                                           br(), br(), br(), br(),
#                                           tableOutput("table"))
                                   ),
                                 
                                 hr(),
                                 br(),
                                 h4("Median of Monte Carlo distributions of mortality components under each scenario"),
                                 fluidRow(column(8, plotOutput("MCplots_MedianMortElem", height = "650px"), offset = 2))
                                 
                                 ),
                        
                        
                        tabPanel("Contrast summary tables",
                                 
                                 h4("Overall mortality rate (i.e. deaths/catch)"),
                                 tableOutput("table")
                                 )
                      ))
             
  ))


