## ---- include = FALSE---------------------------------------------------------
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

# Vaccine efficacy
ve = 40

# Hazard ratio
hr = 1-ve/100

# Follow-up time
ftime <- 12

# Fail events in controls 
fail_control = 0.4

# Define Object with exponential distribution for events in controls
s_events <- s_exponential(fail = fail_control, t = ftime)


## ----simulation, eval= FALSE--------------------------------------------------
#  set.seed(12345)
#  
#  # Define the group for the subjects
#  group = c(rep(0, nsubjects), rep(1, nsubjects))
#  
#  # Define the hazard ratio according to the group
#  hr_vector <- ifelse(group ==0,1,hr)
#  
#  # Loop
#  sim <- lapply(
#    1:nsim,
#    function(x){
#      # Simulate survival times for event
#      sim_time_event <- s_events$rsurvhr(hr_vector)
#  
#      # Censor events at end of follow-up.
#      cevent <- censor_event(censor_time = ftime, time = sim_time_event, event = 1)
#      ctime <- censor_time(censor_time = ftime, time = sim_time_event)
#  
#      # Analyze the data using cox regression
#      reg <- summary(coxph(Surv(ctime, cevent)~ group))
#  
#      # Collect the information
#      pval = reg$coefficients["group","Pr(>|z|)"]
#      ve = (1- exp(reg$coefficients["group","coef"]))*100
#      nevents = reg$nevent
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
 load("sim_df.rda")

## ----analyze------------------------------------------------------------------
empirical_power = binom.test(sum(sim_df$pval <= 0.05), length(sim_df$pval))
empirical_power$estimate
empirical_power$conf.int

# Distribution of the simulated VEs}
summary(sim_df$ve)

# Distribution of the simulated number of events
summary(sim_df$nevents)

