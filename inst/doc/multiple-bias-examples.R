## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  class.output = "output",
  class.message = "message"
)

## ----setup, include = FALSE----------------------------------------------
library(EValue)

## ------------------------------------------------------------------------
HIV_biases <- multi_bias(confounding(), 
                         selection("general", "increased risk"))

## ------------------------------------------------------------------------
HIV_biases

## ------------------------------------------------------------------------
multi_bound(biases = HIV_biases, 
            RRAUc = 2.3, RRUcY = 2.5, RRUsYA1 = 3, RRSUsA1 = 2)

## ------------------------------------------------------------------------
multi_evalue(biases = HIV_biases, 
             est = OR(6.75, rare = TRUE), 
             lo = 2.79, hi = 16.31)

## ------------------------------------------------------------------------
leuk_biases <- multi_bias(confounding(), 
                          misclassification("exposure", 
                                            rare_outcome = TRUE, 
                                            rare_exposure = FALSE))

## ------------------------------------------------------------------------
leuk_biases

## ------------------------------------------------------------------------
multi_bound(biases = leuk_biases, RRAUc = 2, RRUcY = 1.22, ORYAa = 1.59) 

## ------------------------------------------------------------------------
multi_evalue(biases = leuk_biases, 
             est = OR(0.51, rare = TRUE), 
             lo = 0.3, hi = 0.89)

