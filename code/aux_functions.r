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
# Density plots of lognormal distributed input parameters
plot.LogN <- function(cbty.hypar){ 
  
  # ~~ Function's arguments
  # cbty.hypar: list object specifying the hyperparameters of logNormal input distributions for each MC parameter

  # ~~ computing the probability densities for cbty parameters, which follow log-N
  cbtyDstn <- lapply(cbty.hypar, 
                     function(x){
                       E_x <- x[[1]]
                       CV_x <- x[[2]]/100
                       sigma <- sqrt(log(1 + CV_x^2))
                       mu <- log(E_x) - sigma^2/2
                       ctchRge <- qlnorm(c(0.0001, 0.9999), mu, sigma)
                       dat <- data.frame(cRateGrid = seq(ctchRge[1], ctchRge[2], length = 300))
                       dat <- mutate(dat, dens = dlnorm(cRateGrid, mu, sigma), 
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
    guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    scale_colour_manual(values = DsctCols) +
    scale_fill_manual(values = DsctCols)
  
  return(cbtyPlots)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Density plots for beta distributed input parameters
dnsPlot.beta <- function(betaParList, main = ""){ 
  
  # ~~ Function's arguments
  # betaParList: list object specifying the hyperparameters of distributions for each MC parameter, as well as the parameter's label
  
  layerDstn <- lapply(betaParList, 
                      function(x){
                        p <- x[[1]] 
                        # If Beta's constraint p(1-p)>var is not met, adjust the input cv to highest possible under the constraint
                        cv <- ifelse(x[[2]]/100 < sqrt((1-p)/p), x[[2]]/100, sqrt((1-p)/p) * 0.999)
                        succ <- (1-p)/cv^2 - p
                        fail <- (1-p)^2/(cv^2*p) + p - 1
                        pRange <- qbeta(c(0.000001, 0.999999), succ, fail)
                        dat <- data.frame(pGrid = seq(pRange[1], pRange[2], length = 300))
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
  }
  
  return(parDstnPlots)
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot up the distribution of catch and mortality generated from the MC simulation
plot.catchAndMort <- function(MCSims, xlab){
  
  # function's args
  # MCSims: simulated values from the chosen SoN scenario
  # xlab: x-axis label
  
  data2plot <- MCSims %>% select(Catch, M_total, Scenario)
  names(data2plot) <- c("Total Catch", "Total Mortality", "Scenario")
  data2plot <- melt(data2plot, "Scenario")
  
  p <- ggplot(data2plot) + 
    geom_density(aes(x = value, fill = factor(Scenario), col = factor(Scenario)), position = "dodge", alpha = 0.5) +
    guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    labs(y = "Density", x = xlab) +
    theme(legend.position="top") +
    scale_colour_manual(values = DsctCols) +
    scale_fill_manual(values = DsctCols) +
    #facet_grid(variable~.) 
    #facet_grid(Scenario~variable) 
    facet_wrap(~variable) 
  return(p)
  
}
