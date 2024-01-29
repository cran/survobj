## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(survobj)
library(survival)
library(dplyr)

## ----eval= FALSE--------------------------------------------------------------
#  nsim = 1000
#  s_obj = s_weibull(fail = 0.4, t=1, shape = 0.5)
#  n = 250
#  subjid = seq(1, 2*n)
#  group = c(rep(0,n), rep(1,n))
#  hr = c(rep(1,n), rep(0.7,n))
#  tmax = 1
#  set.seed = 12345
#  sim <- lapply(
#    1:nsim,
#    function(x){
#      # simulate survival times for one trial
#      tsim <- matrix(rsurvhr(s_obj, hr), ncol = 1)
#      i = 1
#      while(min(tsim[,i]) < tmax) {
#        i = i+1
#        tsim<- cbind(tsim,renewhr(s_obj, hr, tsim[,i-1]))
#      }
#      # Analysis data.frame
#      df <- data.frame(
#        subjid = rep(subjid,i),
#        group = rep(group, i),
#        time = as.vector(tsim)
#        ) |>
#        arrange(subjid, time)  |>
#        group_by(subjid)  |>
#        mutate(ncase = row_number()) |>
#        mutate(start = lag(time, default = 0)) |>
#        filter(start < tmax) |>
#        mutate(event = censor_event(tmax, time, 1)) |>
#        mutate(end = censor_time(tmax, time))
#  
#      # Analysis multiple episodes
#      mult <- summary(
#                coxph(Surv(start,end,event)~group,
#                  method = "breslow",
#                  id = subjid,
#                  robust = T,
#                  data = df,
#                  control = coxph.control(timefix = FALSE)))
#  
#      # Analysis first or only episode
#      sing <- summary(
#                coxph(Surv(start,end,event)~group,
#                  method = "breslow",
#                  id = subjid,
#                  robust = T,
#                  data = filter(df, ncase == 1),
#                  control = coxph.control(timefix = FALSE)))
#  
#      # Export results for analysis
#      return(
#        data.frame(
#        simid = c(x,x),
#        res = c("recurrent","single"),
#        events = c(mult$nevent, sing$nevent),
#        hr = c(mult$coefficients[1,"exp(coef)"],sing$coefficients[1,"exp(coef)"]),
#        pvalue =  c(mult$coefficients[1,"Pr(>|z|)"],sing$coefficients[1,"Pr(>|z|)"])
#        )
#      )
#    }
#  )
#  
#  # Join all the simulations in a single data frame
#  sim_rec <- do.call(rbind, sim)

## ----include=FALSE------------------------------------------------------------
 # The simulation takes to much time to be included in CRAN
 # Load a previous simulation
 load("sim_rec.rda")

## -----------------------------------------------------------------------------
rec_empirical_power = 
  binom.test(
    sum(sim_rec$pvalue[sim_rec$res == "recurrent"] <= 0.05), 
    length(sim_rec$pval[sim_rec$res == "recurrent"] ))
rec_empirical_power$estimate
rec_empirical_power$conf.int

# Distribution of the simulated VEs}
summary(sim_rec$hr[sim_rec$res == "recurrent"])

# Distribution of the simulated number of events
summary(sim_rec$events[sim_rec$res == "recurrent"])

## -----------------------------------------------------------------------------
sing_empirical_power = 
  binom.test(
    sum(sim_rec$pvalue[sim_rec$res == "single"] <= 0.05), 
    length(sim_rec$pval[sim_rec$res == "single"] ))
sing_empirical_power$estimate
sing_empirical_power$conf.int

# Distribution of the simulated VEs}
summary(sim_rec$hr[sim_rec$res == "single"])

# Distribution of the simulated number of events
summary(sim_rec$events[sim_rec$res == "single"])

