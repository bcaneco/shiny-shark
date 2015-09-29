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
  #
  # NOTE: function as been changed from the version used for the scientific committee report, specifically how effort in sharklines is dealt with
  #       Sharkline effort is now treated as "additional" effort on the top of the initial total effort. In the old version, banning sharklines would 
  #       increase deep hook effort, which is undesirable. Now, switching sharklines on-off doesn't change effort in deep hooks anymore.
  #       Thus, the total effort is simply divided between shallow and deep hooks
  
  bsktNum <- hhooks*100/bsktSize                     # Number of baskets
  SHKNum <- ifelse(SHK == "Y", bsktNum/100, 0)       # if present, number of hundred sharkline hooks (1 sharkline per basket)
  SHLNum <- bsktNum*6/100                            # Number of hundred shallow hooks (6 per basket)
  deepNum <- hhooks - SHLNum                         # Number of hundred deep hooks (= the remaining ones)
  
  return(cbind(SHKNum, SHLNum, deepNum))  
}






do.evaluation <- function(eff, input, bsktSize, nsims)
{
  
  # Read-in hyperparameters and generate random sample of each input distribution
  # cbty - catchability
  cbty = list(shkln  = rlnorm_rptzd(nsims, input$E_shkln, input$cv_shkln),
              shllwR = rlnorm_rptzd(nsims, input$E_shll, input$cv_shll),
              deep   = rlnorm_rptzd(nsims, input$E_deep, input$cv_deep))

  # lhk - probability of hooking location (lip or gut) depending on hook type
  lhk = list(j = rbeta_rptzd(nsims, input$p_LHP.J, input$cv_LHP.J), 
             t = rbeta_rptzd(nsims, input$p_LHP.T, input$cv_LHP.T),
             c = rbeta_rptzd(nsims, input$p_LHP.C, input$cv_LHP.C))
  
  # bo - probability of bite-off depending on leader material and hooking location
  bo = list(ML = rbeta_rptzd(nsims, input$p_BOP.ML, input$cv_BOP.ML),  
            MG = rbeta_rptzd(nsims, input$p_BOP.MG, input$cv_BOP.MG), 
            WL = rbeta_rptzd(nsims, input$p_BOP.WL, input$cv_BOP.WL), 
            WG = rbeta_rptzd(nsims, input$p_BOP.WG, input$cv_BOP.WG))
  
  # mbo - mortality associated with bite-offs depends on hooking location
  mbo = list(L = rbeta_rptzd(nsims, input$p_BOM.L, input$cv_BOM.L),  
             G = rbeta_rptzd(nsims, input$p_BOM.G, input$cv_BOM.G))
  
  # mrt - mortality of those fish retained on the hook depends on hooking location
  mrt = list(L = rbeta_rptzd(nsims, input$p_RM.L, input$cv_RM.L),
             G = rbeta_rptzd(nsims, input$p_RM.G, input$cv_RM.G))
  
  # iw - probability that release of fish occurs in the water versus the fish coming aboard
  iw = rbeta_rptzd(nsims, input$p_WRP, input$cv_WRP)
  
  # mrl - mortality of release depending on hooking location and where released
  mrl = list(LB = rbeta_rptzd(nsims, input$p_URM.LL, input$cv_URM.LL),  
             GB = rbeta_rptzd(nsims, input$p_URM.LG, input$cv_URM.LG),
             LW = rbeta_rptzd(nsims, input$p_URM.WL, input$cv_URM.WL),   
             GW = rbeta_rptzd(nsims, input$p_URM.WG, input$cv_URM.WG))


  
  tmp <- build.obj(eff,nsims)
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
    
    
    
    ########################## 2. Lip hook prob
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
    
    out[,"C_ret_lip",i] <- out[,"C_lip",i]-out[,"Boff_lip",i] # deprecated <BC: Bite-offs unseen, thus *not* subtracted from catch>
    out[,"C_ret_gut",i] <- out[,"C_gut",i]-out[,"Boff_gut",i] # deprecated <BC: same as above>
    
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
    
    
    ################################# all deaths
    out[,"M_total",i] <- out[,"M_Boff_lip",i]+out[,"M_Boff_gut",i]+
      out[,"M_ret_lip",i]+out[,"M_ret_gut",i]+out[,"M_water_lip",i]+
      out[,"M_water_gut",i]+out[,"M_boat_lip",i]+out[,"M_boat_gut",i]
    
  }  
  return(out)  
}








##############################          DMP FUNCTIONS       ##########################################


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Random generation of a Log-normal re-parameterized as expected value of x (i.e. on natural scale instead of the default log-scale mu) and CV
rlnorm_rptzd <- function(n, E_x, cv_x){
  sigma <- sqrt(log(1 + (cv_x/100)^2))
  mu <- log(E_x) - sigma^2/2
  rand <- rlnorm(n, mu, sigma)
  return(rand)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Random generation of a Beta re-parameterized as expected value and CV of p
rbeta_rptzd <- function(n, p_x, cv_x){
  # If Beta's constraint p(1-p)>var is not met, adjust the input cv to highest possible under the constraint
  cv_adj <- ifelse(cv_x/100 < sqrt((1-p_x)/p_x), cv_x/100, sqrt((1-p_x)/p_x) * 0.999)
  succ <- (1-p_x)/cv_adj^2 - p_x
  fail <- (1-p_x)^2/(cv_adj^2*p_x) + p_x - 1
  rand <- rbeta(n, succ, fail)
  # if chosen p = 1, then all possible outcomes equal 1
  if(p_x == 1) rand <- rep(1, n)
  # if chosen CV = 0, then all possible outcomes equal to chosen p
  if(cv_adj == 0) rand <- rep(p_x, n)
  return(rand)
}



