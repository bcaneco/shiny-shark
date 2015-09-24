
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
## ~~ SHARK MITIGATION MONTE CARLO ANALYSIS - SHINY APP
##
## ~~~~ Calculation of fishing effort in hundreds of hooks
## ~~~~ 
## ~~~~ This code produces data that feeds in the Shiny app and remains unchanged while the apps is runnig
## ~~~~   Two outputs:
## ~~~~     (i) the total effort by flag
## ~~~~     (ii) The effort bey gear configuration for a given status-quo, i.e. our best estimate of the current gear specifications.
##
##
## ~~~~ NOTE: the process applied here is outdated. We need the latest version of the method used to estimate the effort spatial surface (with the associated relative abundance surface estimation)
##
##
## BC, 24-09-2015
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# Preamble -----------------------------------------------------------------------------------------
require(readxl)
require(dplyr)
require(ggplot2)
require(reshape2)

path2Proj <- "C:/Users/Bruno/Dropbox/SPC/Shark MC project/shiny-shark/"
p2cd <- file.path(path2Proj,"code")
p2dt <- file.path(path2Proj,"data")
#setwd(path2Proj)

source(file.path(p2cd, 'MC_Analysis_functions.r'))

# Load up the raised 5x5 longline effort
load(file.path(p2dt, "lbest-extract_yr2000-2015_20-04-2015.Rdata"))


# Read in the SON and scenario files
scens <- list.files(path="data/cnt_scen/",pattern="*.xlsx",full.names=T)
mf <- substring(list.files(path="data/cnt_scen/",pattern="*.xlsx"),1,2)
# Create list object of SON
cnt_scn <- lapply(as.list(scens),read_excel2)
names(cnt_scn) <- mf
# # Read in management options
# manopt <- read_excel2("data/scenarios.xlsx")




## STEP 1. compute flag-based effort layer, the spatial abundance layer and collapse ---------------
#
# Flag-based effort
# filter out years and areas and then aggregate over space again
dat <- lbest %>% filter(flag_id %in% mf,abs(latd)<21,yy %in% 2008:2012,lon>139,lon < 211)
#dat <- lbest %>% filter(abs(latd)<21,yy %in% 2008:2012,lon>139,lon < 211)
dat <- dat %>% select(yy,flag_id,fleet_id,hhooks,lond,latd)

#
### Relative abundance surface to rescale effort based on species abundance - currently filled with random unif(0,1)
llsurf <- expand.grid(lond=unique(dat$lond),latd=unique(dat$latd))
llsurf$ab <-runif(n=nrow(llsurf))

# weight (i.e. rescale) effort according to abundance in area
eff2 <- dat %>% group_by(latd, lond, flag_id) %>% summarize(hhooks=sum(hhooks)) %>% inner_join(llsurf)
eff2 <- eff2 %>% mutate(hhooks=hhooks*ab) %>% select(latd,lond,flag_id,hhooks)

# sum up effective effort by flag 
eff <- with(eff2,tapply(hhooks,list(flag_id),sum,na.rm=T))


# Save data
write.csv(eff, file.path(p2dt, "EffortByFleet.csv"))




## STEP 2. Compute effort by gear configuration for a given status quo --------------------------------------------------

StatQuo_descr <- "Base"

# Generate matrix with proportion of longline gear use by flag
StatQuo <- lapply(cnt_scn, get.son, son=StatQuo_descr)

# Compute matrix with proportion of longline gear use by flag and different gear configuration
StatQuo_p <- sapply(StatQuo,build.probs)

# Compute effort by gear configuration
StatQuo_EffbyGear <- eff %*% t(StatQuo_p)

# Save data
write.csv(StatQuo_EffbyGear, file.path(p2dt, "StatQuo_EffbyGear.csv"))






















# SoN vs Management contrast 1
son1_descr <- "Base"
mng1_descr <- "Mono_Circle_No_SH_SHK"

son1 <- lapply(cnt_scn, get.son, son=son1_descr)
mng1 <- apply.opt(son1, manopt, mng1_descr)


# SoN vs Management contrast 2
son2_descr <- "High_wire"
mng2_descr <- "No_wire"

son2 <- lapply(cnt_scn, get.son, son=son2_descr)
mng2 <- apply.opt(son2, manopt, mng2_descr)




## STEP 3. Build flag-based gear-configuration matrices --------------------------------------------------

# Contrast 1
son1_p <- sapply(son1,build.probs)
mng1_p <- sapply(mng1,build.probs)

# Contrast 2
son2_p <- sapply(son2,build.probs)
mng2_p <- sapply(mng2,build.probs)



## STEP 4. Compute effort by gear configuration (hundred of hooks)  --------------------------------------------------

# Contrast 1
son1_inp <- eff %*% t(son1_p)
mng1_inp <- eff %*% t(mng1_p) 


# Contrast 2
son2_inp <- eff %*% t(son2_p)
mng2_inp <- eff %*% t(mng2_p)

