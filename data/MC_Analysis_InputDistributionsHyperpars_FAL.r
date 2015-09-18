

# ~~ List object specifying the hyperparameters of parameters defining the MC simulation
inputsHyperPars <- list(
  
  # ~~~ List object details
  # a) Take x as an MC parameter. i.e. a random variable determining a given process in the catch or mortality of caught sharks
  #
  # b) If x denotes a catch rate, x~log-Normal(mu_x, sd_x), with hyperparameters:
  #     (i) mu_x, the expected value of log(x)
  #    (ii) sd_x, standard deviation of log(x)
  #
  # c) If x denotes the probability of an event, x~Beta(n_x*p_x, n_x(1-p_x)) with hyperparameters:
  #     (i) p_x, the expected probability of event x happening, E[x]
  #    (ii) n_x, the sample size (i.e. number of trials), expressing the dispersion/confidence about the chosen p_x. 
  #         Bigger n_x => bigger confidence on p_x
  
  
  # ~~~~~~~
  # Hyperparameters for catchability parameters (catch per 100 hooks)
  # i.e. x: catch rate in: shkln = sharkline hooks, shllwR = shallow hooks (hook positions 1:3), deep = deep hooks (hook positions >= 4) ~ log-N
  #
  cbty = list(shkln  = list(mu_shkln = -0.78330, sd_shkln = 0.05189, label = "Shark-line"),  # source: from catch vs hook depth models
              shllwR = list(mu_shll  = -4.56537, sd_shll  = 0.03520, label = "Shallow"),     # idem
              deep   = list(mu_deep  = -3.98790, sd_deep  = 0.02983, label = "Deep")),       # idem
  
  
  # ~~~~~~~
  # Hyperparameters for lip-hooking probability parameters
  # dependent on: hook type
  # i.e. x: j = P(Lip|J-hook), t = P(Lip|Tuna-hook), c = P(Lip|Circle-hook) ~ Beta
  #
  lhk  = list(j = list(p_j = 0.2 , n_j = 14, label = "J-hook"),         # p_j = E[P(Lip|J-hook)]       ; source: best guess (p based on SJH's code)
              t = list(p_t = 0.33 , n_t = 14, label = "Tuna-hook"),     # p_t = E[P(Lip|Tuna-hook)]    ; idem
              c = list(p_c = 0.7, n_c = 14,  label = "Circle-hook")),   # p_c = E[P(Lip|Circle-hook)] ; idem
  
  
  # ~~~~~~~
  # Hyperparameters for Bite-Off probability parameters
  # dependent on: hooking location and trace
  # i.e x: ML = P(Bite-off|Mono,lip), MG = P(Bite-off|Mono,gut), WL = P(Bite-off|Wire,lip), WG = P(Bite-off|Wire,gut)  ~ Beta
  #
  bo = list(ML = list(p_ML = 0.33 , n_ML = 190, label = "Mono & Lip"),   # p_ML = E[P(Bite-off|Mono,lip)] ; source: best guess (p based on SJH's code)
            MG = list(p_MG = 0.33 , n_MG = 190, label = "Mono & Gut"),   # p_MG = E[P(Bite-off|Mono,gut)] ; idem
            WL = list(p_WL = 0.01, n_WL = 100, label = "Wire & Lip"),    # p_WL = E[P(Bite-off|Wire,lip)] ; idem
            WG = list(p_WG = 0.01, n_WG = 100, label = "Wire & Gut")),   # p_WG = E[P(Bite-off|Wire,gut)] ; idem
  
  
  # ~~~~~~~
  # Hyperparameters for bite-off mortality parameters
  # dependent on: hooking location
  # i.e. x: L = P(Dead|BO,lip), G = P(Dead|BO,gut) ~ Beta
  #
  mbo = list(L = list(p_L = 1/(1+30), n_L = 20, label = "Lip-hooked"),   # p_L = E[P(Dead|BO,lip)] ; source: best guess (p based on SJH's code)
             G = list(p_G = 1/(1+15), n_G = 20, label = "Gut-hooked")),  # p_G = E[P(Dead|BO,gut)] ; idem
  
  
  # ~~~~~~~
  # Hyperparameters for on-hook mortality parameters
  # dependent on: hooking location
  # i.e. x: L = P(Dead|OH,lip), G = P(Dead|OH,gut) ~ Beta
  mrt = list(L = list(p_L = 0.1974, n_L = 11470, label = "Lip-hooked"),   # p_L = E[P(Dead|OH,lip)] ; higher for lip hooked as they didnt bite-off 
             G = list(p_G = 0.1974, n_G = 11470, label = "Gut-hooked")),  # p_G = E[P(Dead|OH,gut)]
  
  
  # ~~~~~~~
  # Hyperparameters for probability of release in water parameter iw ~ Beta
  # i.e. x: iw = P(release of fish in water), 1-iw = P(fish coming on board)
  iw = list(IwR = list(p_iw = 0.5, n_iw = 10, label = "Released in-water")),  # p_iw = E[P(release of fish in water)] ; fat distribution
  
  
  # ~~~~~~~
  # Hyperparameters for release mortality parameters 
  # dependent on: hooking location and where released
  # i.e. x: LB = P(Dead|onboard, lip), GB = P(Dead|onboard, gut), LW = P(Dead|inwater, lip), GW = P(Dead|inwater, gut) ~ Beta
  #
  mrl = list(LB = list(p_LB = 0.19, n_LB = 100, label = "Onboard-lip"),   # p_LB = E[P(Dead|onboard, lip)] ; source: best guess (p based on SJH's code)
             GB = list(p_GB = 0.19, n_GB = 100, label = "Onboard-gut"),   # p_GB = E[P(Dead|onboard, gut)] ; idem
             LW = list(p_LW = 0.15  , n_LW = 100, label = "Water-lip"),   # p_LW = E[P(Dead|inwater, lip)] ; idem  
             GW = list(p_GW = 0.15  , n_GW = 100, label = "Water-gut"))   # p_GW = E[P(Dead|inwater, gut)] ; idem
)
