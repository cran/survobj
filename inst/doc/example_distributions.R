## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(survobj)
library(survival)
library(ggplot2)

## ----exponential, fig.height=6, fig.width=7, fig.align='center'---------------
# Instanciate an object of class SURVIVAL with the Exponential distribution
obj1 <- s_factory(s_exponential, lambda = 3)
obj1

# Survival at time 1
sfx(obj1,1)

# Hazard at time 1
hfx(obj1,1)

# Cumulative hazard at time 1
Cum_Hfx(obj1,1)

# Inverse of the cumulative hazard 0.6
invCum_Hfx(obj1, 0.6)

# Plot of the distribution
plot(obj1)

## ----exponential2-------------------------------------------------------------
obj2 <- s_exponential(surv = 0.8, t = 1)
obj2

obj3 <- s_exponential(fail = 0.2, t = 1)
obj3

## ----exponential3,  fig.height=4, fig.width=7, fig.align='center'-------------
obj4 <- s_exponential(surv = 0.25, t = 10)
ggplot_survival_random(obj4, timeto=10, subjects=1000, nsim=100, alpha = 0.1)

## ----weibull, fig.height=4, fig.width=7, fig.align='center'-------------------
wobj1 <- s_weibull(scale = 3, shape = 0.5)
wobj2 <- s_weibull(scale = 3, shape = 1)
wobj3 <- s_weibull(scale = 3, shape = 1.5)

par(mfrow=c(2,3))
plot(
  wobj1$sfx,
  from = 0,
  to = 1,
  main = "Weibull with shape 0.5",
  xlab = "Time",
  ylab = "Proportion without events",
  ylim = c(0,1))
plot(
  wobj2$sfx,
  from = 0,
  to = 1,
  main = "Weibull with shape 1",
  xlab = "Time",
  ylab = "Proportion without events",
  ylim = c(0,1))
plot(
  wobj3$sfx,
  from = 0,
  to = 1,
  main = "Weibull with shape 1.5",
  xlab = "Time",
  ylab = "Proportion without events",
  ylim = c(0,1))
plot(
  wobj1$hfx,
  from = 0,
  to = 1,
  xlab = "Time",
  ylab = "hazard")
plot(
  wobj2$hfx,
  from = 0,
  to = 1,
  xlab = "Time",
  ylab = "hazard")
plot(
  wobj3$hfx,
  from = 0,
  to = 1,
  xlab = "Time",
  ylab = "hazard")
par(mfrow=c(1,1))

## ----gomperz, fig.height=4, fig.width=7, fig.align='center'-------------------

# define a function to generate and plot Gompertz distributions
plot_sfx_gompertz<- function(shape, scale = 3, timeto = 1){
  plot(
    s_gompertz(shape = shape, scale = scale)$sfx,
    from = 0,
    to = timeto,
    main = paste("Shape: ", shape),
    xlab = "Time",
    ylab = "Proportion without events",
    ylim = c(0,1)
    )
}

plot_hfx_gompertz<- function(shape, scale = 3, timeto = 1){
  plot(
    s_gompertz(shape = shape, scale = scale)$hfx,
    from = 0,
    to = timeto,
    xlab = "Time",
    ylab = "hazard",
    ylim = c(2,4)
    )
}

par(mfrow=c(2,4))
plot_sfx_gompertz(shape = -0.25)
plot_sfx_gompertz(shape = -0.10)
plot_sfx_gompertz(shape = 0.10)
plot_sfx_gompertz(shape = 0.25)
plot_hfx_gompertz(shape = -0.25)
plot_hfx_gompertz(shape = -0.10)
plot_hfx_gompertz(shape = 0.10)
plot_hfx_gompertz(shape = 0.25)
par(mfrow = c(1,1))

## ----piecewise, fig.height=6, fig.width=7, fig.align='center'-----------------
pobj <- s_piecewise(surv = 0.2, breaks = c(1,2,3,Inf), segments = c(1,2,3,1))
pobj
pobj$sfx(3)
plot_survival(pobj, timeto = 3)

## ----compare, fig.height=6, fig.width=7, fig.align='center'-------------------

cobj1<- s_exponential(lambda = 3)
cobj2<- s_gompertz(scale = 3, shape = 0.4)
compare_survival(cobj1, cobj2, timeto = 2)

