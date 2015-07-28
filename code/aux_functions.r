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
# plot up input distributions of catchability parameters
plot.cbty <- function(cbty.hypar){ 
  
  # ~~ Function's arguments
  # inputs: list object specifying the hyperparameters of input distributions for each MC parameter

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



