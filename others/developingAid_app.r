library(shiny)



#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Testing updating the value of slidebar based on choice of species

beta.input <- function(title, suffix, p.value, cv.value){
  wellPanel(
    style = "padding: 5px;",
    h5(title),
    sliderInput(paste0("p_", suffix), "Expected probability:", min = 0, max = 1, step = 0.05, value = p.value, ticks = FALSE),
    sliderInput(paste0("cv_", suffix), "Coef. variation (%):", min = 0, max = .1, step = 5, value = cv.value, ticks = FALSE)
  )
}


shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(
          
          radioButtons("spp", label = h4("Select the species"), 
                      choices = list("Oceanic whitetip shark", "Silky shark"), 
                      selected = "Oceanic whitetip shark")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("tab1",           
                     fluidRow(column(3, beta.input("C-Hook", "LHP_C", p.value = 0.1, cv.value = 25))),
                     hr(),
                     fluidRow(column(3, verbatimTextOutput("value1"))),
                     fluidRow(column(3, verbatimTextOutput("value2")))),
            tabPanel("tab2", 
                     fluidRow(column(3, beta.input("J-Hook", "LHP_J", p.value = 0.3, cv.value = 40))),
                     hr()
                     #fluidRow(column(3, verbatimTextOutput("value3"))),
                     #fluidRow(column(3, verbatimTextOutput("value4")))
                     )))
        )
    ),
    
    server = function(input, output, session) {
      observe({
        hypParVals <- switch(input$spp,
                             "Oceanic whitetip shark" = inputsHyperPars$OCS,
                             "Silky shark" = inputsHyperPars$FAL)
        
        updateSliderInput(session, "p_LHP_C", value = hypParVals$lhk$c$p_c)
        updateSliderInput(session, "cv_LHP_C", value = hypParVals$lhk$c$n_c)
      })
      
      observe({
        val <- input$p_LHP_C
        uppLim <- min(floor(sqrt((1-val)/val) * 99.9), 100)
        # Control the value, min, max, and step.
        updateSliderInput(session, "cv_LHP_C", max = uppLim)
      })
      output$value1 <- renderPrint({ input$p_LHP_C})
      output$value2 <- renderPrint({ input$cv_LHP_C})
    }
  )



inputsHyperPars$OCS$lhk$c$p_c
inputsHyperPars$OCS$lhk$c$n_c







#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Testing updating the value of slidebar based on choice of species

p <- 1
n <- 0.001
gridx <- seq(0.00001,0.99999, length.out = 100)
plot(gridx, dbeta(gridx, p*n, (1-p)*n), type = "l")

rbeta(1000, p*n, (1-p)*n)



library(dplyr)
library(data.table)
library(ggplot2)


p <- 0.85
cv1 <- 0

#v <- (cv1/100)^2 * p^2

#sqrt((1-p)/p)*100
#p*(1-p)

# If Beta's constraint p(1-p)>var is not met, adjust the input cv to highest possible under the constraint
cv <- ifelse(cv1/100 < sqrt((1-p)/p), cv1/100, sqrt((1-p)/p) * 0.999)
#cv <- cv1/100
succ <- (1-p)/cv^2 - p
fail <- (1-p)^2/(cv^2*p) + p - 1
#pRange <- qbeta(c(0.000001, 0.999999), succ, fail)
pGrid = seq(0, 1, length = 200)
dat <- data.frame(pGrid, dens = dbeta(pGrid, succ, fail), 
                  parLabel = "test")

if(p == 1) dat$dens <- c(rep(0, 299), Inf)
if(cv == 0) dat <- rbind(dat, data.frame(pGrid = p, dens = Inf, parLabel = "test")) %>% arrange(pGrid)



# ~~ plotting  
parDstnPlots <- ggplot(dat, aes(x=pGrid, y=dens)) + 
  geom_line(aes(col = parLabel))+
  geom_area(aes(fill=parLabel), position = "dodge", alpha=0.4) + 
  labs(x ='Probability', y = 'Density') +
  guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL))

parDstnPlots


summary(dat)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

E_x <- 1.3
CV_x <- 0
sigma <- sqrt(log(1 + CV_x^2))
mu <- log(E_x) - sigma^2/2
ctchRge <- qlnorm(c(0.0001, 0.9999), mu, sigma)
dat <- data.frame(cRateGrid = seq(ctchRge[1], ctchRge[2], length = 300))
dat <- mutate(dat, dens = dlnorm(cRateGrid, mu, sigma), 
              hookCateg = "test")

if(CV_x == 0){
  dat <- data.frame(cRateGrid = c(E_x, seq(ctchRge[1]*0.97, ctchRge[2]*1.03, length = 100)),
                    dens = c(Inf, rep(0, 100)), 
                    hookCateg = "test") %>% arrange(cRateGrid)

}
  
 
# ~~ plotting  
cbtyPlots <- ggplot(dat, aes(x=cRateGrid, y=dens)) + 
  geom_line(aes(col = hookCateg)) +
  geom_area(aes(fill=hookCateg), position = "dodge", alpha=0.4)+
  labs(x ='Catch per 100 hooks', y = 'Density', title = 'Catch rate by hook position') +
  guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL))

cbtyPlots






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Developing the interactions with the choice of management scenarios

library(shiny)


MngScnMatrix <- read.csv("C:/Users/Bruno/Dropbox/SPC/Shark MC project/shiny-shark/data/Mngt_Scenarios.csv")
load("C:/Users/Bruno/Dropbox/SPC/Shark MC project/shiny-shark/data/EffortByFleet.robj")
load("C:/Users/Bruno/Dropbox/SPC/Shark MC project/shiny-shark/data/StatQuo_PropGearUseByFlag.robj") ; StatQuo_PropGearUse
load("C:/Users/Bruno/Dropbox/SPC/Shark MC project/shiny-shark/data/StatQuo_EffbyGearCnfg.robj")


source("C:/Users/Bruno/Dropbox/SPC/Shark MC project/shiny-shark/code/MC_Analysis_functions.r")
source("C:/Users/Bruno/Dropbox/SPC/Shark MC project/shiny-shark/code/shinyAux_functions.r")

mng_input <- function(id, title){
  wellPanel(
    checkboxGroupInput(id, label = h4(title),
                       choices = list("Ban Shark lines" = "NoShkln", "Ban wire trace" = "NoWire", 
                                      "Ban shallow hooks" = "NoShallow", "Restrict to Circle-hooks only" = "AllCircle"),
                       selected = NULL, inline = FALSE)
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



shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
      ),
      mainPanel(
        fluidRow(column(4, mng_input("MngScn1", "Management Scenario 1")),
                 column(4, mng_input("MngScn2", "Management Scenario 2"))),
        
        verbatimTextOutput("choice"),
        
        hr(),
        fluidRow(column(6, beta.input("J-Hook", "LHP.J", p.value = 0.3, cv.value = 43))),
        
        br(),
        actionButton("button", "An action button"),
        br(),
        hr(),
        verbatimTextOutput("beta_rand")
        
      ))),
  
  server = function(input, output, session) {
    
    output$choice <- renderPrint({
      
      out <- list(
        sq_effGear = StatQuo_effGear,
        mng1_effGear = effGearCnfg_fun(input$MngScn1, StatQuo_PropGearUse, MngScnMatrix, eff_flag),
        mng2_effGear = effGearCnfg_fun(input$MngScn2, StatQuo_PropGearUse, MngScnMatrix, eff_flag))
      
      out
    
    })
    
    randomVals <- eventReactive(input$button, {
      test(10, input = input)
    })
    
    output$beta_rand <- renderPrint({
      randomVals()
    })
    
  }
)




sum(c(StatQuo_effGear$effort))
t(StatQuo_effGear$effort)
sum(t(effGearCnfg_fun("NoShkln", StatQuo_PropGearUse, MngScnMatrix, eff_flag)$effort))

x1 <- apply.opt(StatQuo_PropGearUse, MngScnMatrix, "NoShkln")
x2 <- sapply(x1,build.probs)
x3 <- eff_flag %*% t(x2) 
sum(x3)


test <- function(n, input){
  rbeta_rptzd(n, input$p_LHP.J, input$cv_LHP.J)
}




#
rlnorm_rptzd(10, 2, 0)

rbeta_rptzd(20, 0.1, 0)






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Testing plotting functions for MC outputs


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






