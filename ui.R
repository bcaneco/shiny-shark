library(shiny)


# Define input widget for lognormal distribution
logN.input <- function(title, suffix, mu.value, sd.value){
  wellPanel(
    h5(title),
    sliderInput(paste0("mu_", suffix), "Expected Value:", min = 0, max = 1, step = 0.1, value = mu.value),
    sliderInput(paste0("sd_", suffix), "Standard deviation:", min = 0, max = 1, step = 0.1, value = sd.value)
  )
}



# Define input widget for beta distribution
beta.input <- function(title, suffix, p.value, n.value){
  wellPanel(
    h5(title),
    sliderInput(paste0("p_", suffix), "Expected Probability:", min = 0, max = 1, step = 0.1, value = p.value),
    sliderInput(paste0("n_", suffix), "Sample Size:", min = 0, max = 10000, step = 25, value = n.value)
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
                                     h3("Hey, try me!"),
                                     selectInput("spp", label = h4("Select a species"), 
                                                 choices = list("Oceanic whitetip shark" = 1, "Silky shark" = 2), 
                                                 selected = 1),
                                     p("Here we can make general comments about setting things up, etc")
                        ),
                        
                        mainPanel(
                          
                          tabsetPanel(
                            tabPanel("Catch Model",
                                     
                                     fluidRow(
                                       column(10, plotOutput("catchPlot"), offset = 2)
                                     ),
                                     br(),
                                     fluidRow(
                                       column(3, logN.input("Shark line Hooks", "shkln", mu.value = 0.1, sd.value = 0.6), offset = 2),
                                       column(3, logN.input("Shallow Hooks", "shll", mu.value = 0.001, sd.value = 0.06)),
                                       column(3, logN.input("Deep Hooks", "deep", mu.value = 0.7, sd.value = 0.06))
                                     )),
                            
                            tabPanel("Fate Model",
                                     h4("Probability of lip hook (vs. gut hook) given:"),
                                     fluidRow(),
                                     fluidRow(
                                       column(3, beta.input("J-Hook", "Jhk", p.value = 0.1, n.value = 200), offset = 2),
                                       column(3, beta.input("T-Hook", "Thk", p.value = 0.1, n.value = 200)),
                                       column(3, beta.input("C-Hook", "Chk", p.value = 0.1, n.value = 200))
                                     ),
                                     
                                     hr(),
                                     
                                     h4("Probability of bite-off given:"),
                                     fluidRow(),
                                     fluidRow(
                                       column(3, beta.input("Mono Leader & lip-hooked", "MnLp", p.value = 0.1, n.value = 200)),
                                       column(3, beta.input("Mono Leader & gut-hooked", "MnGp", p.value = 0.1, n.value = 200)),
                                       column(3, beta.input("Wire Leader & lip-hooked", "WrLp", p.value = 0.1, n.value = 200)),
                                       column(3, beta.input("Wire Leader & gut-hooked", "WrLp", p.value = 0.1, n.value = 200))
                                     ), 
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality given bite-off and:"),
                                     fluidRow(),
                                     fluidRow(
                                       column(3, beta.input("Lip-hooked", "BLp", p.value = 0.1, n.value = 200), offset = 3),
                                       column(3, beta.input("Gut-hooked", "BGt", p.value = 0.1, n.value = 200))
                                     ), 
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality at retrieval given:"),
                                     fluidRow(),
                                     fluidRow(
                                       column(3, beta.input("Lip-hooked", "OLp", p.value = 0.1, n.value = 200), offset = 3),
                                       column(3, beta.input("Gut-hooked", "OGt", p.value = 0.1, n.value = 200))
                                     ), 
                                     
                                     hr(),
                                     
                                     h4("Probability of release in-water (vs. brought-on then released):"),
                                     fluidRow(),
                                     fluidRow(column(3, beta.input("", "InWtRel", p.value = 0.1, n.value = 200), offset = 4)), 
                                     
                                     hr(),
                                     
                                     h4("Probability of mortality upon release given:"),
                                     fluidRow(),
                                     fluidRow(column(3, beta.input("In-water release & lip-hooked", "WtLp", p.value = 0.1, n.value = 200)),
                                              column(3, beta.input("In-water release & gut-hooked", "WtGt", p.value = 0.1, n.value = 200)),
                                              column(3, beta.input("Landed release & lip-hooked", "LnLp", p.value = 0.1, n.value = 200)),
                                              column(3, beta.input("Landed release & gut-hooked", "LnGt", p.value = 0.1, n.value = 200)))
                                     )
                            )))),
             
             tabPanel("Management Scenarios"),
             tabPanel("Simulation Outputs")
             
  ))


