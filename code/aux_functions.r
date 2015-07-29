# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Colours for plots
DsctCols <- c("dodgerblue2", "olivedrab3", "firebrick2", "gold2")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Handy function for generating breaks when ggplotting on sqrt transformed axis
sqrt_breaks <- function(x){
  x <- as.numeric(x)
  # construct breaks using function from "scales" library
  library(scales)
  out <- cbreaks(x, breaks = trans_breaks("sqrt", function(y) y^2), 
                    labels = scientific_format())
  return(out$breaks)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Density plots of lognormally distributed MC input parameters
plot.LogN <- function(cbty.hypar){ 
  
  # ~~ Function's arguments
  # inputs: list object specifying the hyperparameters of logNormal input distributions for each MC parameter

  # ~~ computing the probability densities for cbty parameters, which follow log-N
  cbtyDstn <- lapply(cbty.hypar, 
                     function(x){
                       mu <- x[[1]]
                       sd <- x[[2]]
                       ctchRge <- qlnorm(c(0.0001, 0.9999), mu, sd)
                       dat <- data.frame(cRateGrid = seq(ctchRge[1], ctchRge[2], length = 200))
                       dat <- mutate(dat, dens = dlnorm(cRateGrid, mu, sd), 
                                     hookCateg = x[[3]])
                       return(dat)
                     })
  cbtyDstn <- rbindlist(cbtyDstn)
  
  # ~~ data prep for plotting
  cbtyDstn$hookCateg <- factor(cbtyDstn$hookCateg, levels = c("Shark line", "Shallow", "Deep"))
  
  # ~~ plotting  
  cbtyPlots <- ggplot(cbtyDstn, aes(x=cRateGrid, y=dens)) + 
    geom_line(aes(col = hookCateg)) +
    geom_area(aes(fill=hookCateg), position = "dodge", alpha=0.4)+
    labs(x ='Catch per 100 hooks', y = 'Density', title = 'Catch rate by hook position') +
    #scale_x_continuous(trans = "sqrt", breaks = sqrt_breaks) + 
    #scale_y_continuous(trans = "sqrt", breaks = sqrt_breaks) +
    guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    scale_colour_manual(values = DsctCols) +
    scale_fill_manual(values = DsctCols)
  

}







# plot up the input distributions
dnsPlot.beta <- function(betaParList, main = ""){ 
  
  # ~~ Function's arguments
  # inputs: list object specifying the hyperparameters of input distributions for each MC parameter
  
  
#   # MC parameters with beta distributions (i.e. probability parameters) --------------------------------------------
#   ProbLayers <- list(layer    = c("lhk", "bo", "mbo", "mrt", "iw", "mrl"), 
#                      plotTitle  = c('Probability of lip-hooking given hook type', 
#                                     'Probability of bite-off given hook trace & hooking location', 
#                                     'Probability of bitten-off dying given hooking location',
#                                     'Probability of on-hook dying given hooking location',
#                                     'Probability of released in-water',
#                                     'Probability of released dying given hooking location and where released'),
#                      fileSuffix = c("Lip-hooking", "Bite-off_prob", "Bite-off_mort", "On-Hook_mort",
#                                     "In-Water_release", "Released_mort"))
  
  
  
  layerDstn <- lapply(betaParList, 
                      function(x){
                        p <- x[[1]] 
                        n <- x[[2]]
                        succ <- n*p
                        fail <- n*(1-p) 
                        #CV <- sqrt((1-p)/((n + 1) * p)) * 100
                        pRange <- qbeta(c(0.000001, 0.999999), succ, fail)
                        dat <- data.frame(pGrid = seq(pRange[1], pRange[2], length = 500))
                        dat <- mutate(dat, dens = dbeta(pGrid, succ, fail), 
                                      parLabel = x[[3]])
                        return(dat)
                      })
  
  layerDstn <- rbindlist(layerDstn)
  
    # ~~ plotting  
  parDstnPlots <- ggplot(layerDstn, aes(x=pGrid, y=dens)) + 
    geom_line(aes(col = parLabel))+
    geom_area(aes(fill=parLabel), position = "dodge", alpha=0.4) + 
    labs(x ='Probability', y = 'Density', title = main) +
    guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    scale_colour_manual(values = DsctCols) +
    scale_fill_manual(values = DsctCols)
  
  
  # remove plot's legend if only one parameter involved in current layer
  if(length(betaParList) == 1){ 
    parDstnPlots <- parDstnPlots + theme(legend.position="none")
  }else parDstnPlots
  
  
}

