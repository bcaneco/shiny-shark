# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Colours for plots
DsctCols <- c("dodgerblue2", "olivedrab3", "firebrick2", "gold2", "purple")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Computing the upper limit of the CV in beta dstns, given the constraint CV < sqrt(1-mean)/mean
BetaCV_UppLim <- function(chosenP){
  uppLim <- min(floor(sqrt((1-chosenP)/chosenP) * 99.99), 100)
  return(uppLim)
}



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
                       # hack to produce spike at expected x (as above steps return all values == Inf -> no good for plotting)
                       if(CV_x == 0){
                         dat <- data.frame(cRateGrid = c(E_x, seq(ctchRge[1]*0.90, ctchRge[2]*1.10, length = 99)),
                                           dens = c(Inf, rep(0, 99)), 
                                           hookCateg = x[[3]]) %>% arrange(cRateGrid)
                       }
                       
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
    guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    scale_colour_manual(values = DsctCols) +
    scale_fill_manual(values = DsctCols) +
    theme(legend.position="bottom")
  
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
                        pGrid <- seq(0, 1, length = 300)
                        dat <- data.frame(pGrid, dens = dbeta(pGrid, succ, fail), parLabel = x[[3]])
                        # hack to return spike at p=1 (as above steps return NaNs)
                        if(p == 1) dat$dens <- c(rep(0, 299), Inf)
                        # hack to return spike at chosen p when CV = 0 (as above steps return NaNs)
                        if(cv == 0) dat <- rbind(dat, data.frame(pGrid = p, dens = Inf, parLabel = x[[3]])) %>% arrange(pGrid)
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
    scale_fill_manual(values = DsctCols) +
    theme(legend.position="bottom")
  
  # remove plot's legend if only one parameter involved in current layer
  if(length(betaParList) == 1){ 
    parDstnPlots <- parDstnPlots + theme(legend.position="none")
  }
  
  return(parDstnPlots)
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot the MC distribution of catch and mortality under each scenario
plot.catchAndMort <- function(MCSims, xlab, main){
  
  # ~~~~~~~~~~~~~~~~~
  # Args
  #
  # MCSims: simulated values from the chosen SoN scenario
  # xlab: x-axis label
  # main: Plot title
  # ~~~~~~~~~~~~~~~~~
  
  data2plot <- MCSims %>% select(Catch, M_total, Scenario)
  setnames(data2plot, old = c("Catch", "M_total", "Scenario"), new = c("Total Catch", "Total Mortality", "Scenario"))
  data2plot <- melt(data2plot, "Scenario")
  
  p <- ggplot(data2plot) + 
    geom_density(aes(x = value, fill = factor(Scenario), col = factor(Scenario)), position = "dodge", alpha = 0.5) +
    #geom_histogram(aes(x = value, fill = factor(Scenario), col = factor(Scenario)), position = "dodge", alpha = 0.5) +
    guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    labs(y = "Density", x = xlab, title = main) +
    theme(legend.position="none") +
    scale_colour_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    facet_grid(Scenario~variable)
  return(p)
  
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot the MC distributions of mortality rate under each scenario
plot.MortRate <- function(MCSims, xlab, main){
  
  # ~~~~~~~~~~~~~~~~~
  # Args
  #
  # MCSims: simulated values from the chosen SoN scenario
  # xlab: x-axis label
  # ~~~~~~~~~~~~~~~~~
  
  data2plot <- MCSims %>% select(Mort_rate,  Scenario)
  setnames(data2plot, old = c("Mort_rate", "Scenario"), new = c("Mortality rate", "Scenario"))
  data2plot <- melt(data2plot, "Scenario")
  
  p <- ggplot(data2plot) + 
    #geom_density(aes(x = value, fill = factor(Scenario), col = factor(Scenario)), position = "dodge", alpha = 0.5) +
    geom_histogram(aes(x = value, fill = factor(Scenario), col = factor(Scenario)), position = "dodge", alpha = 0.5, 
                   binwidth = diff(range(data2plot$value))/30) +
    guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    labs(x = xlab, title = main) +
    scale_colour_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    theme(legend.position="none") +
    facet_grid(Scenario ~ .)
    
  return(p)
  
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot the median of MC distributions of mortality components
plot.MortElements <- function(MCSims, main){
  
  # ~~~~~~~~~~~~~~~~~
  # Args
  #
  # MCSims: simulated values from the chosen SoN scenario
  # xlab: x-axis label
  # ~~~~~~~~~~~~~~~~~
  
  subdata <- MCSims %>% select(M_Boff_lip, M_Boff_gut, M_ret_lip, M_ret_gut, M_water_lip, M_water_gut, M_boat_lip, M_boat_gut, Scenario)
  subdata <- subdata %>% group_by(Scenario) %>% 
    summarise(M_Boff_lip = median(M_Boff_lip), M_Boff_gut = median(M_Boff_gut), M_ret_lip = median(M_ret_lip), 
              M_ret_gut = median(M_ret_gut), M_water_lip = median(M_water_lip), M_water_gut = median(M_water_gut), 
              M_boat_lip = median(M_boat_lip), M_boat_gut = median(M_boat_gut))
  
  data2plot <- melt(subdata, "Scenario")
  
  p <- ggplot(data2plot) + 
    geom_bar(aes(x = Scenario, y = value, fill = variable), stat = "identity") + 
    #guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    labs(y = "Mortality (Number of sharks)", x = NULL, title = main) +
    scale_colour_brewer(palette="Set2") +
    scale_fill_brewer(palette="Set2") +
    coord_flip()
  
  return(p)
  
}





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate effort by gear configuration for a given management scenario
effGearCnfg_fun <- function(mngScn, SQ_pgu, mngscn_mat, eff){
  
  # ~~~~~~~~~~~~~
  # Args
  # 
  # mngScn: character vector from UI specifying the chosen management option(s)
  # SQ_pgu: list with proportion of gear use by flag under the status quo 
  # mngscn_mat: matrix with proportion of gear use by flag under all possible management scenarios
  # eff: effort in number of hooks (hundred) by flag
  # ~~~~~~~~~~~~~
  
  if (is.null(mngScn))
    return()
  
  mngCode <- paste(mngScn, collapse = "_")
  # Compute proportion of gear use by flag
  mng_PropGearUse <- apply.opt(SQ_pgu, mngscn_mat, mngCode)
  # Compute proportion of gear use by gear configuration and flag
  mng_PropGearCnfg <- sapply(mng_PropGearUse, build.probs)
  # Compute effort by gear configuration
  effort <- eff %*% t(mng_PropGearCnfg)
  
  return(list(mngCode = mngCode, effort = effort))
  
}







