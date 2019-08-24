## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(qctoy)

## ----echo = TRUE, include = TRUE-----------------------------------------
fr <- 0 # flow rate
fl <- 0 # fluorescence
ff <- sim.sample.linear_increase_in_signal(out.flow_rate = fr, out.fluorescence = fl)
sim.plot(fr); sim.plot(fl, title = "Fluorescence", y.lab = "fluorescence")

## ----echo = TRUE, include = TRUE-----------------------------------------
fr <- 0 # flow rate
fl <- 0 # fluorescence
ff <- sim.sample.measuring_air(out.flow_rate = fr, out.fluorescence = fl)
sim.plot(fr); sim.plot(fl, title = "Fluorescence", y.lab = "fluorescence")

## ----echo = TRUE, include = TRUE-----------------------------------------
fr <- 0 # flow rate
fl <- 0 # fluorescence
ff <- sim.sample.permanent_rate_change(out.flow_rate = fr, out.fluorescence = fl)
sim.plot(fr); sim.plot(fl, title = "Fluorescence", y.lab = "fluorescence")

## ----echo = TRUE, include = TRUE-----------------------------------------
fr <- 0 # flow rate
fl <- 0 # fluorescence
ff <- sim.sample.hole_in_flow_cell(out.flow_rate = fr, out.fluorescence = fl)
sim.plot(fr); sim.plot(fl, title = "Fluorescence", y.lab = "fluorescence")

## ------------------------------------------------------------------------
flow_rate <- sim.baseline(N = 3000, mu = 150, sigma = 3) %>% 
             sim.exponential(range = 1500:2000, strength = 4) %>% 
             sim.sharp_flex(range = 2000:3000, strength = 4) %>%
             sim.prune(i = 19, N = 20) %>%
             sim.round()

## ------------------------------------------------------------------------
fluo1 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 4000, sigma = 200) %>%
         sim.sharp_push(range = c(2000, Inf), strength = -3000)

## ------------------------------------------------------------------------
fluo2 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 0, sigma = 50) %>%
         sim.sharp_push(range = c(2000, Inf), strength = -2000)

## ------------------------------------------------------------------------
fluo <- sim.concatenate(fluo1, fluo2)

