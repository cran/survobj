## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(survobj)
library(survival)

## ----simulation1, fig.align='center', fig.width= 7, fig.height=5--------------

# Number of simulations
nsim = 1000

# Participants in each group
nsubjects = 250

# Follow-up time
ftime <- 12

# Vaccine efficacy
ve_start = 80
ve_end = 10

# Hazard ratio
hr <- function(t){
  vm <- ve_start - (ve_start-ve_end)/(ftime-1)*(t-1)
  1-vm/100
}

# Fail events in controls 
fail_control = 0.4

# Define Object with weibull distribution for events in controls
s_ctrl <- s_weibull(fail = fail_control, t = ftime, shape = 0.8)


# Define Object with Picewise exponential distribution in vaccinated

s_vacc <- s_piecewise(
            breaks = c(1:12,Inf), 
            hazards = c(s_ctrl$hfx(1:12)*hr(1:12), s_ctrl$hfx(12)*hr(12)))


## ----simulation2, fig.align='center', fig.width= 7, fig.height=5--------------
compare_survival(s_ctrl, s_vacc, timeto = 12)

## ----simulation3, echo=TRUE, eval=FALSE---------------------------------------
#  set.seed(12345)
#  
#  # Define the group for the subjects
#  group = c(rep(0, nsubjects), rep(1, nsubjects))
#  
#  
#  # Loop
#  sim <- lapply(
#    1:nsim,
#    function(x){
#      # Simulate survival times for event
#      # Using one distribution for the controls and other for the vaccinated
#      sim_time_event <- c(s_ctrl$rsurv(nsubjects), s_vacc$rsurv(nsubjects))
#  
#      # Censor events at end of follow-up.
#      cevent <- censor_event(censor_time = ftime, time = sim_time_event, event = 1)
#      ctime <- censor_time(censor_time = ftime, time = sim_time_event)
#  
#      # Analyze the data using cox regression
#      reg <- coxph(Surv(ctime, cevent)~ group)
#      sreg <- summary(reg)
#      phz <- cox.zph(reg)
#  
#      # Collect the information
#      pval = phz$table["group","p"]
#      ve = (1- exp(sreg$coefficients["group","coef"]))*100
#      nevents = sreg$nevent
#  
#      # return values
#      return(data.frame(simid = x, pval,ve, nevents))
#    }
#  )
#  
#  # Join all the simulations in a single data frame
#  sim_df <- do.call(rbind, sim)

## ----loadsimul, include=FALSE-------------------------------------------------
 # The simulation takes to much time to be included in CRAN
 # Load a previous simulation
 load("sim_df2.rda")

## ----analyze------------------------------------------------------------------
empirical_power = binom.test(sum(sim_df$pval <= 0.05), length(sim_df$pval))
empirical_power$estimate
empirical_power$conf.int

# Distribution of the simulated VE estimated under PH assumpation
summary(sim_df$ve)

# Distribution of the simulated number of events
summary(sim_df$nevents)

