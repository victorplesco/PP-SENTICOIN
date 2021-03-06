% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/supp_exe.monitoring.R
\name{supp_exe.monitoring}
\alias{supp_exe.monitoring}
\title{supp_exe.monitoring}
\usage{
supp_exe.monitoring(exe.data, api.data, par.data, safe.dir)
}
\arguments{
\item{exe.data}{list (e.g. requests = 10, tweets = 150, ect.). List of execution parameters.}

\item{api.data}{object of class response(). Information captured from a request.}

\item{safe.dir}{string (e.g. "~/home/..."). Path to an existing directory (serving as a data backup).}

\item{param_list}{list (e.g. max_results = 10, ect.). List of a request's parameters.}
}
\value{
An updated set of execution parameters.
}
\description{
Updates the values of the execution parameters and prints a short process summary.
}
\author{
Victor Plesco
}
