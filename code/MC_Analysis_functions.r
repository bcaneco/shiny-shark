read_excel2 <- function(x) as.data.frame(read_excel(x))


get.son <- function(lstobj,son)
{
  # Get specific state of nature for each country
  a <- lstobj[lstobj[,1]==son,]
  a
}

build.probs <- function(lstobj)
{
  # Build the gear configuration probabilities
  tmp <- list(Leader=c("W"=lstobj[1,2],"M"=lstobj[1,3]),
              Hook=c("J"=lstobj[1,4],"T"=lstobj[1,5],"C"=lstobj[1,6]),
              ShkLn=c("SLY"=lstobj[1,7],"SLN"=lstobj[1,8]),
              Shllw=c("SHY"=lstobj[1,9],"SHN"=lstobj[1,10]))  
  
  ct <- expand.grid(tmp)
  dt <- apply(ct, 1, prod)
  names(dt) <- apply(expand.grid(lapply(tmp,names)),1,paste,collapse="-")
  dt
}

apply.opt <- function(lstobj,optfl,opt)
{
  # modifies gear probabilities consistent with management option considered
  xt <- optfl[which(optfl[,1]==opt),2:10] # need to exclude scenario col  
  
  jnk.fun <- function(x,xt) 
  {
    x[which(xt!="NA")+1]  <- as.numeric(xt[which(xt!="NA")]) #as x has extra col
    x
  }    
  lapply(lstobj,jnk.fun,xt)    
}



build.obj <- function(iobj,sims)
{
  # builds an object to fill with simul;ation results
  
  #  "Effort" effort for that gear configuration
  #  "Catch" predicted catch
  #  "C_lip","C_gut" catch split into gut hooked and lip hooked
  #  "Boff_lip","Boff_gut" bite-offs based on hooking location
  #  "M_Boff_lip","M_Boff_gut" - mortality of those that bite-off and escaped
  #  "C_ret_lip","C_ret_gut" -- retained on the gear (not bite-off)
  #  "M_ret_lip","M_ret_gut" -- dead ones at the side of the boat
  #  "C_water_lip","C_water_gut","C_boat_lip","C_boat_gut" alive ones by nature of release and hooking location
  #  "M_water_lip","M_water_gut","M_boat_lip","M_boat_gut","M_total"  -- mortality for released sharks
  
  xscen <-array(NA,dim=c(length(iobj),4),dimnames=list(dimnames(iobj)[[2]],c("Leader","Hook","SHK","SHL"))) # the options in the scenario
  
  
  x <- array(NA,dim=c(length(iobj),24,sims))
  dimnames(x) <- list(dimnames(iobj)[[2]],
                      c("tot_Effort", "SHK_Effort", "SHL_effort", "deep_Effort", "Catch","C_lip","C_gut","Boff_lip","Boff_gut",
                        "M_Boff_lip","M_Boff_gut","C_ret_lip","C_ret_gut","M_ret_lip","M_ret_gut",
                        "C_water_lip","C_water_gut","C_boat_lip","C_boat_gut",
                        "M_water_lip","M_water_gut","M_boat_lip","M_boat_gut","M_total"),
                      1:sims)
  
  xscen[,"Leader"] <-substring(dimnames(iobj)[[2]],1,1)
  xscen[,"Hook"] <-substring(dimnames(iobj)[[2]],3,3)
  xscen[,"SHK"] <-substring(dimnames(iobj)[[2]],7,7)
  xscen[,"SHL"] <-substring(dimnames(iobj)[[2]],11,11)
  x[,"tot_Effort",] <- iobj
  
  # browser() 
  out <-list(xscen,x)
  return(out)
}



effortPerHookCateg <- function(hhooks, SHK, bsktSize){
  # args:
  # hhooks = number of hundred hooks for a given gear configuration
  # bsktSize = basket size, i.e. hooks between floats
  # SHK = "Y" or "N", are sharklines present?
  
  bsktNum <- hhooks*100/bsktSize                     # Number of baskets
  SHKNum <- ifelse(SHK == "Y", bsktNum/100, 0)       # if present, number of hundred sharkline hooks (1 sharkline per basket)
  SHLNum <- bsktNum*6/100                            # Number of hundred shallow hooks (6 per basket)
  deepNum <- hhooks - (SHKNum + SHLNum)              # Number of hundred deep hooks (= the remaining ones)
  
  return(cbind(SHKNum, SHLNum, deepNum))  
}






do.evaluation <- function(inp, nsims, bsktSize, inputs)
{
  
  # Read-in hyperparamneters and generate random sample of each input distribution
  # cbty - catchability
  cbty = list(shkln  = rlnorm(nsims, inputs$cbty$shkln[["mu_shkln"]], inputs$cbty$shkln[["sd_shkln"]]),
              shllwR = rlnorm(nsims, inputs$cbty$shllwR[["mu_shll"]], inputs$cbty$shllwR[["sd_shll"]]),
              deep   = rlnorm(nsims, inputs$cbty$deep[["mu_deep"]], inputs$cbty$deep[["sd_deep"]]))

  # lhk - probability of hooking location (lip or gut) depending on hook type
  lhk = list(j = rbeta(nsims, inputs$lhk$j[["p_j"]]*inputs$lhk$j[["n_j"]], (1 - inputs$lhk$j[["p_j"]])*inputs$lhk$j[["n_j"]]), 
             t = rbeta(nsims, inputs$lhk$t[["p_t"]]*inputs$lhk$t[["n_t"]], (1 - inputs$lhk$t[["p_t"]])*inputs$lhk$t[["n_t"]]),
             c = rbeta(nsims, inputs$lhk$c[["p_c"]]*inputs$lhk$c[["n_c"]], (1 - inputs$lhk$c[["p_c"]])*inputs$lhk$c[["n_c"]]))
  
  # bo - probability of bite-off depending on leader material and hooking location
  bo = list(ML = rbeta(nsims, inputs$bo$ML[["p_ML"]]*inputs$bo$ML[["n_ML"]], (1 - inputs$bo$ML[["p_ML"]])*inputs$bo$ML[["n_ML"]]),  
            MG = rbeta(nsims, inputs$bo$MG[["p_MG"]]*inputs$bo$MG[["n_MG"]], (1 - inputs$bo$MG[["p_MG"]])*inputs$bo$MG[["n_MG"]]), 
            WL = rbeta(nsims, inputs$bo$WL[["p_WL"]]*inputs$bo$WL[["n_WL"]], (1 - inputs$bo$WL[["p_WL"]])*inputs$bo$WL[["n_WL"]]), 
            WG = rbeta(nsims, inputs$bo$WG[["p_WG"]]*inputs$bo$WG[["n_WG"]], (1 - inputs$bo$WG[["p_WG"]])*inputs$bo$WG[["n_WG"]]))
  
  # mbo - mortality associated with bite-offs depends on hooking location
  mbo = list(L = rbeta(nsims, inputs$mbo$L[["p_L"]]*inputs$mbo$L[["n_L"]], (1 - inputs$mbo$L[["p_L"]])*inputs$mbo$L[["n_L"]]),  
             G = rbeta(nsims, inputs$mbo$G[["p_G"]]*inputs$mbo$G[["n_G"]], (1 - inputs$mbo$G[["p_G"]])*inputs$mbo$G[["n_G"]]))
  
  # mrt - mortality of those fish retained on the hook depends on hooking location
  mrt = list(L = rbeta(nsims, inputs$mrt$L[["p_L"]]*inputs$mrt$L[["n_L"]], (1 - inputs$mrt$L[["p_L"]])*inputs$mrt$L[["n_L"]]),
             G = rbeta(nsims, inputs$mrt$G[["p_G"]]*inputs$mrt$G[["n_G"]], (1 - inputs$mrt$G[["p_G"]])*inputs$mrt$G[["n_G"]]))
  
  # iw - probability that release of fish occurs in the water versus the fish coming aboard
  iw = rbeta(nsims, inputs$iw$IwR[["p_iw"]]*inputs$iw$IwR[["n_iw"]], (1 - inputs$iw$IwR[["p_iw"]])*inputs$iw$IwR[["n_iw"]])
  
  # mrl - mortality of release depending on hooking location and where released
  mrl = list(LB = rbeta(nsims, inputs$mrl$LB[["p_LB"]]*inputs$mrl$LB[["n_LB"]], (1 - inputs$mrl$LB[["p_LB"]])*inputs$mrl$LB[["n_LB"]]),  
             GB = rbeta(nsims, inputs$mrl$GB[["p_GB"]]*inputs$mrl$GB[["n_GB"]], (1 - inputs$mrl$GB[["p_GB"]])*inputs$mrl$GB[["n_GB"]]),
             LW = rbeta(nsims, inputs$mrl$LW[["p_LW"]]*inputs$mrl$LW[["n_LW"]], (1 - inputs$mrl$LW[["p_LW"]])*inputs$mrl$LW[["n_LW"]]),   
             GW = rbeta(nsims, inputs$mrl$GW[["p_GW"]]*inputs$mrl$GW[["n_GW"]], (1 - inputs$mrl$GW[["p_GW"]])*inputs$mrl$GW[["n_GW"]]))


  
  tmp <- build.obj(inp,nsims)
  inds <- tmp[[1]]
  out <- tmp[[2]]
  
  
  # Compute hundred hooks sitting in each hook category - dependent on basket size
  out[, c("SHK_Effort", "SHL_effort", "deep_Effort"), ] <- t(mapply(effortPerHookCateg, 
                                                                    hhooks = out[, "tot_Effort", 1], 
                                                                    SHK = inds[, "SHK"], 
                                                                    bsktSize))
  
  for(i in 1:nsims)
  {
    ################# 1. Catchability - depends on hook depth category, i.e. shallow hooks (positions 1:3), 
    #                                   deep hooks (remainiing positions) and shark lines
    #  
    #SHK-Y SHL-N
    jnk <- names(which(inds[,"SHK"] =="Y" & inds[,"SHL"]=="N"))
    out[jnk,"SHL_effort",i] <- -out[jnk,"SHL_effort",i]               # "-" sign stands for hooks removed
    out[jnk,"Catch",i] <- out[jnk,"SHK_Effort",i]*cbty$shkln[i] + out[jnk,"deep_Effort",i]*cbty$deep[i]
    
    #SHK-y SHL-Y
    jnk <- names(which(inds[,"SHK"] =="Y" & inds[,"SHL"]=="Y"))
    out[jnk,"Catch",i] <- out[jnk,"SHK_Effort",i]*cbty$shkln[i] + out[jnk,"SHL_effort",i]*cbty$shllwR[i] + 
      out[jnk,"deep_Effort",i]*cbty$deep[i]
    
    #SHK-N SHL-N
    jnk <- names(which(inds[,"SHK"] =="N" & inds[,"SHL"]=="N"))
    out[jnk,"SHL_effort",i] <- -out[jnk,"SHL_effort",i]               # "-" sign stands for # hooks removed
    out[jnk,"Catch",i] <- out[jnk,"deep_Effort",i]*cbty$deep[i]
    
    #SHK-N SHL-Y
    jnk <- names(which(inds[,"SHK"] =="N" & inds[,"SHL"]=="Y"))
    out[jnk,"Catch",i] <- out[jnk,"SHL_effort",i]*cbty$shllwR[i] + out[jnk,"deep_Effort",i]*cbty$deep[i]
    
    
    
    ########################## 2. Lip hook prob beta(4,12) for J,beta(6,12) for tuna, and beta(12,4) for circle
    #
    
    #
    #J-hook
    jnk <- names(which(inds[,"Hook"] =="J" ))
    out[jnk,"C_lip",i] <- out[jnk,"Catch",i]*lhk$j[i]
    out[jnk,"C_gut",i] <- out[jnk,"Catch",i]*(1-lhk$j[i])
    #T-hook
    jnk <- names(which(inds[,"Hook"] =="T" ))
    out[jnk,"C_lip",i] <- out[jnk,"Catch",i]*lhk$t[i]
    out[jnk,"C_gut",i] <- out[jnk,"Catch",i]*(1-lhk$t[i])
    #C-hook
    jnk <- names(which(inds[,"Hook"] =="C" ))
    out[jnk,"C_lip",i] <- out[jnk,"Catch",i]*lhk$c[i]
    out[jnk,"C_gut",i] <- out[jnk,"Catch",i]*(1-lhk$c[i])
    
    
    ############################# 3. Bite-off's -- probability of bite-off depends on leader material and hooking location
    
    #M-leader
    jnk <- names(which(inds[,"Leader"] == "M"))
    out[jnk,"Boff_lip",i] <- out[jnk,"C_lip",i]*bo$ML[i]
    out[jnk,"Boff_gut",i] <- out[jnk,"C_gut",i]*bo$MG[i]
    #W_leader
    jnk <- names(which(inds[,"Leader"] =="W"))
    out[jnk,"Boff_lip",i] <- out[jnk,"C_lip",i]*bo$WL[i]
    out[jnk,"Boff_gut",i] <- out[jnk,"C_gut",i]*bo$WG[i]
    
    # Dead bite-offs - depends on hooking location
    out[,"M_Boff_lip",i] <- out[,"Boff_lip",i]*mbo$L[i]
    out[,"M_Boff_gut",i] <- out[,"Boff_gut",i]*mbo$G[i]
    
    
    ######################### 4. Retained by gear boat - then alive/dead which depends on hook position
    # Catch at the boat first --> Initial catch - bite-offs
    
    out[,"C_ret_lip",i] <- out[,"C_lip",i]   #-out[,"Boff_lip",i] BC: Bite-offs unseen, thus *not* removed from catch
    out[,"C_ret_gut",i] <- out[,"C_gut",i]   #-out[,"Boff_gut",i] BC: same as above
    
    # mortality at boat
    
    # Dead at boat - depends on hooking location - just for the moment
    out[,"M_ret_lip",i] <- out[,"C_ret_lip",i]*mrt$L[i]
    out[,"M_ret_gut",i] <- out[,"C_ret_gut",i]*mrt$G[i]
    
    
    
    ######################### 5. Still alive - are you released in the water or on deck -- need some info on this!
    
    # lets have a fat beta distribution for released in the water
    
    
    out[,"C_water_lip",i] <- (out[,"C_ret_lip",i]-out[,"M_ret_lip",i])*iw[i]
    out[,"C_boat_lip",i] <- (out[,"C_ret_lip",i]-out[,"M_ret_lip",i])*(1-iw[i])
    
    out[,"C_water_gut",i] <- (out[,"C_ret_gut",i]-out[,"M_ret_gut",i])*iw[i]
    out[,"C_boat_gut",i] <- (out[,"C_ret_gut",i]-out[,"M_ret_gut",i])*(1-iw[i])
    
    
    
    ################################ 6. Probability of survival for final released sharks - depends on leader material and hook location and how released
    
    # Dead at boat - depends on hooking location - just for the moment  (-- BC: Guess it should be "dead **after** released"?; not dependent on leader yet?)
    out[,"M_water_lip",i] <- out[,"C_water_lip",i]*mrl$LW[i]
    out[,"M_water_gut",i] <- out[,"C_water_gut",i]*mrl$GW[i]
    #
    out[,"M_boat_lip",i] <- out[,"C_boat_lip",i]*mrl$LB[i]
    out[,"M_boat_gut",i] <- out[,"C_boat_gut",i]*mrl$GB[i]
    #
    # all deaths
    out[,"M_total",i] <- out[,"M_Boff_lip",i]+out[,"M_Boff_gut",i]+
      out[,"M_ret_lip",i]+out[,"M_ret_gut",i]+out[,"M_water_lip",i]+
      out[,"M_water_gut",i]+out[,"M_boat_lip",i]+out[,"M_boat_gut",i]
  }  
  return(out)  
}






##############################          DMP FUNCTIONS       ##########################################

# Colours for plots
DsctCols <- c("dodgerblue2", "olivedrab3", "firebrick2", "gold2")



# Handy function for generating breaks when ggplotting on sqrt transformed axis
sqrt_breaks <- function(x){
  x <- as.numeric(x)
  # construct breaks using function from "scales" library
  library(scales)
  output <- cbreaks(x, breaks = trans_breaks("sqrt", function(y) y ^ 2), 
                    labels = scientific_format())
  return(output$breaks)
}



# plot up the input distributions
plot.inputs <- function(outFolder = file.path(getwd(), "figs"), fileHeader, inputs){ 
  
  # ~~ Function's arguments
  # outFolder: Folder path to receive generated plots
  # fileHeader: prefix for files where generated plots will be saved
  # inputs: list object specifying the hyperparameters of input distributions for each MC parameter
  
  # Function's dependent packages  -------------------------------------------
  require(ggplot2)
  require(dplyr)
  require(data.table)
  
  theme_set(theme_bw())
  
  # Function's local parameters -------------------------------------------
  ppi <- 150     # plot's image resolution in pixels per inch
  pwidth <- 12   # plot's width in inches
  pheight <- 8   # plot's height in inches
  
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
  
  
  # ~~ save plot in png
  png(file=file.path(outFolder, paste0(fileHeader,"Catchability.png")), width=pwidth*ppi,
      height=pheight*ppi, unit = "px", res = ppi)
  print(cbtyPlots)
  dev.off()
  # ~~ save plot in pdf
  pdf(file=file.path(outFolder, paste0(fileHeader,"Catchability.pdf")), width=pwidth, height=pheight)
  print(cbtyPlots)
  dev.off()
  
  
  
  # MC parameters with beta distributions (i.e. probability parameters) --------------------------------------------
  ProbLayers <- list(layer    = c("lhk", "bo", "mbo", "mrt", "iw", "mrl"), 
                     plotTitle  = c('Probability of lip-hooking given hook type', 
                                    'Probability of bite-off given hook trace & hooking location', 
                                    'Probability of bitten-off dying given hooking location',
                                    'Probability of on-hook dying given hooking location',
                                    'Probability of released in-water',
                                    'Probability of released dying given hooking location and where released'),
                     fileSuffix = c("Lip-hooking", "Bite-off_prob", "Bite-off_mort", "On-Hook_mort",
                                    "In-Water_release", "Released_mort"))
  
  
  for(i in 1:length(ProbLayers$layer)){
    
    layerDstn <- lapply(inputs[[ ProbLayers$layer[i] ]], 
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
      #geom_polygon(fill='blue', alpha=0.5)+
      geom_line(aes(col = parLabel))+
      geom_area(aes(fill=parLabel), position = "dodge", alpha=0.4) + 
      labs(x ='Probability', y = 'Density', title = ProbLayers$plotTitle[i]) +
      guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
      scale_colour_manual(values = DsctCols) +
      scale_fill_manual(values = DsctCols)
    
    
    # remove plot's legend if only one parameter involved in current layer
    if(length(inputs[[ ProbLayers$layer[i] ]]) == 1){ 
      parDstnPlots <- parDstnPlots + theme(legend.position="none")
    }
    
    
    # ~~ save plot in png
    png(file=file.path(outFolder, paste0(fileHeader, ProbLayers$fileSuffix[i], ".png")), width=pwidth*ppi, 
        height=pheight*ppi, unit = "px", res = ppi)
    print(parDstnPlots)
    dev.off()
    # ~~ save plot in pdf
    pdf(file=file.path(outFolder, paste0(fileHeader, ProbLayers$fileSuffix[i], ".pdf")), width=pwidth, height=pheight)
    print(parDstnPlots)
    dev.off()
  }
  
  
}




# plot up the distribution of catch and mortality generated from the MC simulation
plot.outputs <- function(son_MCSims, mngOpt_MCSims, xlab){
  
  theme_set(theme_bw())
  
  # function's args
  # son_MCSims: simulated values from the chosen SoN scenario
  # mngOpt_MCSims: simulated values from the chosen management option scenario
  # xlab: x-axis label
  
  data2plot <- rbind(son_MCSims, mngOpt_MCSims) %>% select(Catch, M_total, Scenario)
  names(data2plot) <- c("Total catch", "Total Mortality", "Scenario")
  data2plot <- melt(data2plot, "Scenario")
  
  p <- ggplot(data2plot) + 
    geom_density(aes(x = value, fill = factor(Scenario), col = factor(Scenario)), position = "dodge", alpha = 0.5) +
    guides(fill=guide_legend(title=NULL), col=guide_legend(title=NULL)) +
    labs(y = "Density", x = xlab) +
    theme(legend.position="top") +
    scale_colour_manual(values = DsctCols) +
    scale_fill_manual(values = DsctCols) +
    facet_grid(variable~.) 
    #facet_grid(Scenario~variable) 
    #facet_wrap(~variable) 
  return(p)
  
}







