## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  class.output = "output",
  class.message = "message"
)

## ----setup, include = FALSE---------------------------------------------------
library(EValue)

## -----------------------------------------------------------------------------
biases <- multi_bias(confounding(), 
                     selection("general", "increased risk"), 
                     misclassification("exposure", rare_outcome = TRUE))

## ----result = "asis", echo = FALSE--------------------------------------------
tab <- EValue:::get_arg_tab()
tab$latex <- sub("^\\$", "$$", tab$latex)
tab$latex <- sub("\\$$", "$$", tab$latex)
tab$bias <- ifelse(
  tab$bias == "selection" & !grepl("Y\\*", tab$output), "selection after outcome misclassification",
  ifelse(
    tab$bias == "selection" & !grepl("A\\*", tab$output), "selection after exposure misclassification", tab$bias
  )
)

tab$bias <- ifelse(
  !tab$rare_outcome & !tab$rare_exposure, tab$bias,
  ifelse(
    tab$rare_outcome & tab$rare_exposure, paste0(tab$bias, " (rare exposure and outcome)"),
    ifelse(
      tab$rare_outcome, paste0(tab$bias, " (rare outcome)"),
      ifelse(
        tab$rare_exposure, paste0(tab$bias, " (rare exposure)"), NA
      )
    )
  )
)

tab <- tab[c("bias", "latex", "output", "argument")]
tab <- tab[!duplicated(tab),]
tab$output <- paste0("`", tab$output, "`")
tab$argument <- paste0("`", tab$argument, "`")
names(tab) <- c("Bias", "Parameter", "R Output", "Function argument")
knitr::kable(tab)

## -----------------------------------------------------------------------------
summary(biases)

## -----------------------------------------------------------------------------
summary(
  multi_bias(confounding(), 
             misclassification("exposure", rare_outcome = TRUE),
             selection("general", "increased risk"))
  )

## -----------------------------------------------------------------------------
print(biases)

## ----eval = FALSE-------------------------------------------------------------
#  multi_bound(biases,
#              RRUcY = 2, RRAUc = 1.5,
#              RRSUsA1 = 1.25, RRUsYA1 = 2.5,
#              ORYAaS = 1.75)

## -----------------------------------------------------------------------------
param_vals <- seq(1, 3, by = 0.5)

# create every combination of values
params <- expand.grid(
  RRUcY = param_vals, RRAUc = param_vals,
  RRSUsA1 = param_vals, RRUsYA1 = param_vals,
  ORYAaS = param_vals
)

params$bound <- mapply(multi_bound,
  RRUcY = params$RRUcY, RRAUc = params$RRAUc,
  RRSUsA1 = params$RRSUsA1, RRUsYA1 = params$RRUsYA1,
  ORYAaS = params$ORYAaS,
  MoreArgs = list(biases = biases)
)

## -----------------------------------------------------------------------------
hist(params$bound, main = NULL, xlab = "Bound")

## -----------------------------------------------------------------------------
multi_evalue(biases, est = RR(4))

## -----------------------------------------------------------------------------
# square-root approximation of the odds ratio
multi_evalue(biases, est = OR(4, rare = FALSE))

## -----------------------------------------------------------------------------
# use verbose = FALSE to suppress message about parameters
multi_evalue(biases, est = RR(4), lo = 2.5, hi = 6, verbose = FALSE)

## -----------------------------------------------------------------------------
multi_evalue(biases, est = RR(0.25), lo = 0.17, hi = 0.4, verbose = FALSE)

## -----------------------------------------------------------------------------
summary(multi_evalue(biases, est = RR(4)))

