## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
class.output = "output",
class.message = "message"
)
library(EValue)
# # TEMP ONLY
# setwd("~/Dropbox/Personal computer/Independent studies/EValue package/evalue_package_git/EValue/data")
# load("soyMeta.RData")
# setwd("~/Dropbox/Personal computer/Independent studies/EValue package/evalue_package_git/EValue/R")
# source("meta-analysis.R")

library(metafor)
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
data(soyMeta)
( m = rma.uni(yi = soyMeta$est,
              vi = soyMeta$var,
              method = "PM",
              test = "knha") )
yr = as.numeric(m$b)  # returned estimate is on log scale
vyr = as.numeric(m$vb) 
t2 = m$tau2
vt2 = m$se.tau2^2 

## -----------------------------------------------------------------------------
( res0 = confounded_meta(method = "parametric",
                         q = log(0.9),
                         tail = "below",
                         muB = 0,
                         sigB = 0,
                         yr = yr, 
                         vyr = vyr,
                         t2 = t2,
                         vt2 = vt2) )

## -----------------------------------------------------------------------------
( res1 = confounded_meta(method = "parametric",
                         q = log(0.9),
                         tail = "below",
                         r = 0.10,
                         muB = log(1.3),
                         sigB = 0.2,
                         yr = yr, 
                         vyr = vyr,
                         t2 = t2,
                         vt2 = vt2) )

## -----------------------------------------------------------------------------
sens_plot(method = "parametric",
          type = "line",
          q = log(0.9),
          sigB = 0,
          tail = "below",
          
          yr = yr, 
          vyr = vyr,
          t2 = t2,
          vt2 = vt2)

## -----------------------------------------------------------------------------
sens_plot(method = "calibrated",
          type = "line",
          q = log(0.9),
          tail = "below",
          sigB = 0,
          dat = soyMeta,
          yi.name = "est",
          vi.name = "var",
          give.CI = FALSE)

