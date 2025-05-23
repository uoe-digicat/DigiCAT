% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hedges_g.R
\name{hedges_g}
\alias{hedges_g}
\title{Function to calculate Hedge's G}
\usage{
hedges_g(
  treatment_variable,
  missing_method,
  outcome_variable,
  balanced_data,
  outcome_results,
  weighting_variable = NULL,
  cluster_variable = NULL,
  strata_variable = NULL
)
}
\arguments{
\item{treatment_variable}{Name of treatment variable}

\item{missing_method}{}

\item{outcome_variable}{Name of outcome variable}

\item{balanced_data}{Balanced data object}

\item{outcome_results}{Outcome model object}

\item{weighting_variable}{Character string matching column name of your weighting variable}

\item{cluster_variable}{Character string matching column name of your clustering variable}

\item{strata_variable}{Character string matching column name of your stratification variable}
}
\description{
Function to calculate Hedge's G
}
