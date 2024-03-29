% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply-treatment-effect.R
\name{apply_treatment_effect}
\alias{apply_treatment_effect}
\title{apply specified treatment effect (percent change) to outcome}
\usage{
apply_treatment_effect(
  x,
  model_formula,
  model_call,
  te,
  effect_direction,
  concurrent
)
}
\arguments{
\item{x}{data}

\item{model_formula}{formula for sim run used to identify outcome}

\item{te}{true effect as proportion of change (e.g., 0.05 = 5%)}

\item{effect_direction}{"null", "pos", or "neg"}

\item{concurrent}{bool for whether this is concurrent run or not}
}
\description{
TODO
}
