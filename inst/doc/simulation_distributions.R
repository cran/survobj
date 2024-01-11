## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(survobj)
library(survival)

## ----fig.height=4, fig.width=7, fig.align='center'----------------------------
s_obj <- s_exponential(fail = 0.4, t = 2)
ggplot_survival_random(s_obj, timeto =2, subjects = 1000, nsim= 10, alpha = 0.3)

## ----fig.height=4, fig.width=7, fig.align='center'----------------------------
s_obj <- s_exponential(fail = 0.4, t = 2)
group <- c(rep(0,500), rep(1,500))
hr_vector <- c(rep(1,500),rep(2,500))
times <- rsurvhr(s_obj, hr_vector)
plot(survfit(Surv(times)~group), xlim=c(0,5))

## ----fig.height=4, fig.width=7, fig.align='center'----------------------------
s_obj <- s_exponential(fail = 0.4, t = 2)
ggplot_survival_hr(s_obj, hr = 2, nsim = 10, subjects = 1000, timeto = 5)

## ----fig.height=4, fig.width=7, fig.align='center'----------------------------
s_obj <- s_lognormal(scale = 2, shape = 0.5)
ggplot_survival_aft(s_obj, aft = 2, nsim = 10, subjects = 1000, timeto = 5)

