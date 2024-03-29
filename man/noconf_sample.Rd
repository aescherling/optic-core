% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/no-confounding-methods.R
\name{noconf_sample}
\alias{noconf_sample}
\title{perform sampling and coding of treatment for no confounding policy simulations}
\usage{
noconf_sample(single_simulation)
}
\arguments{
\item{single_simulation}{object created from SimConfig$setup_single_simulation()}
}
\description{
samples treated units (e.g., states) randomly; 
    Once treated units are identified, codes level and change version of
    treatment that are used in various modeling approaches later on.
}
