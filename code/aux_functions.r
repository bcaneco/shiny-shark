
# plot up the input distributions
plot.inputs <- function(inputs){ 
  
  # ~~ Function's arguments
  # inputs: list object specifying the hyperparameters of input distributions for each MC parameter
  
  # Function's dependent packages  -------------------------------------------
  require(ggplot2)
  require(dplyr)
  require(data.table)
  
  theme_set(theme_bw())
  
  
  # Catchability distributions ------------------------------------
  #
  # ~~ computing the probability densities for cbty parameters, which follow log-N
  cbtyDstn <- lapply(inputs$cbty, 
                     function(x){
                       mu <- x[[1]]
                       sd <- x[[2]]
                       ctchRge <- qlnorm(c(0.0000001, 0.999999999), mu, sd)
                       dat <- data.frame(cRateGrid = seq(ctchRge[1], ctchRge[2], length = 500))
                       dat <- mutate(dat, dens = dlnorm(cRateGrid, mu, sd), 
                                     hookCateg = x[[3]])
                       return(dat)
                     })
  cbtyDstn <- rbindlist(cbtyDstn)
  
  # ~~ data prep for plotting
  cbtyDstn$hookCateg <- factor(cbtyDstn$hookCateg, levels = c("Shark-line", "Shallow", "Deep"))
  
  
  # ~~ plotting  
  cbtyPlots <- ggplot(cbtyDstn, aes(x=cRateGrid, y=dens)) + 
    geom_line(aes(col = hookCateg)) +
    geom_area(aes(fill=hookCateg), position = "dodge", alpha=0.4)+
    labs(x ='Catch per 100 hooks', y = 'Density', title = 'Catch rate by hook position') +
    scale_x_continuous(trans = "sqrt", breaks = sqrt_breaks) + 
    #scale_y_continuous(trans = "sqrt", breaks = sqrt_breaks) +
    guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    scale_colour_manual(values = DsctCols) +
    scale_fill_manual(values = DsctCols)
  

}



