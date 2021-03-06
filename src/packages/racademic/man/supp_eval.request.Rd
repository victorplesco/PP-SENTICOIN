% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/supp_eval.request.R
\name{supp_eval.request}
\alias{supp_eval.request}
\title{supp_eval.request}
\usage{
supp_eval.request(v2.endpoint, product_track, ...)
}
\arguments{
\item{v2.endpoint}{string \{"/2/tweets/search/all" or "/2/tweets/search/recent"\}. Twitter API v2 Search Endpoint.}

\item{product_track}{string \{"academic" or "standard"\}. Type of product track.}

\item{...}{mix (e.g. max_results = 10, query = "btc -is:retweet", ect.). Set of a request's parameters.}
}
\value{
A list of a request's validated parameters.
}
\description{
Given a set of a request's parameters, validates their values based on a pre-defined set of conditions.
}
\author{
Victor Plesco
}
