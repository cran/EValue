% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effect_measures.R
\name{effect_measures}
\alias{effect_measures}
\alias{RR}
\alias{OR}
\alias{HR}
\alias{RD}
\alias{OLS}
\alias{MD}
\title{Declare an effect measure}
\usage{
RR(est)

OR(est, rare)

HR(est, rare)

RD(est)

OLS(est, sd)

MD(est)
}
\arguments{
\item{est}{The effect estimate (numeric).}

\item{rare}{Logical. Whether the outcome is sufficiently rare for use of risk
ratio approximates; if not, approximate conversions are used. Used only for
\code{\link[=HR]{HR()}} and \code{\link[=OR]{OR()}}; see Details.}

\item{sd}{The standard deviation of the outcome (or residual standard
deviation). Used only for \code{\link[=OLS]{OLS()}}; see Details.}
}
\value{
An object of classes "estimate" and the measure of interest,
containing the effect estimate and any other attributes to be used in
future calculations.
}
\description{
These functions allow the user to declare that an estimate is a
certain type of effect measure: risk ratio (\code{RR}), odds ratio (\code{OR}),
hazard ratio (\code{HR}), risk difference (\code{RD}), linear regression coefficient
(\code{OLS}), or mean standardized difference (\code{MD}).
}
\details{
The \link[=convert_measures]{conversion functions} use these objects to
convert between effect measures when necessary to calculate E-values. Read
more about the conversions in Table 2 of VanderWeele TJ, Ding P.
\emph{Sensitivity Analysis in Observational Research: Introducing the E-Value.}
Annals of Internal Medicine. 2017;167(4):268–75.

See also VanderWeele TJ. \emph{Optimal approximate conversions of odds ratios
and hazard ratios to risk ratios.} Biometrics. 2019 Jan 6;(September
2018):1–7.

For \code{\link[=OLS]{OLS()}}, \code{sd} must be specified. A true standardized mean difference
for linear regression would use \code{sd} = SD( Y | X, C ), where Y is the
outcome, X is the exposure of interest, and C are any adjusted covariates.
See Examples for how to extract this from \code{lm}. A conservative
approximation would instead use \code{sd} = SD( Y ). Regardless, the
reported E-value for the confidence interval treats \code{sd} as known, not
estimated.
}
\examples{
# Both odds ratios are 3, but will be treated differently in E-value calculations
# depending on whether rare outcome assumption is reasonable
OR(3, rare = FALSE)
OR(3, rare = TRUE)
evalue(OR(3, rare = FALSE))
evalue(OR(3, rare = TRUE))
attributes(OR(3, rare = FALSE))

# If an estimate was constructed via conversion from another effect measure,
# we can see the history of a conversion using the summary() function
summary(toRR(OR(3, rare = FALSE)))
summary(toRR(OLS(3, sd = 1)))

# Estimating sd for an OLS estimate
# first standardizing conservatively by SD(Y)
data(lead)
ols = lm(age ~ income, data = lead)
est = ols$coefficients[2]
sd = sd(lead$age)
summary(evalue(OLS(est, sd)))
# now use residual SD to avoid conservatism
# here makes very little difference because income and age are
# not highly correlated
sd = summary(ols)$sigma
summary(evalue(OLS(est, sd)))
}
