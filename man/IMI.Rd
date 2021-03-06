% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesian_checking.R
\name{imi}
\alias{imi}
\title{Calculate IMI}
\usage{
imi(state, w = NULL)
}
\arguments{
\item{state}{A topic model state file}

\item{w}{A tbl_df with types and topics to calculate IMI for. Default is NULL.}

\item{k}{The topic to calculate IMI}
}
\description{
Function to calculate instantanueous mutual information for types for a given topic.

See reference for details.
}
\details{

}
\examples{
# Load the state of the union topic model
load(system.file("extdata/sotu_50.Rdata", package = "topicmodeltoolbox"))
w <- type_probability(sotu_50, 10)

}
\references{
Mimno, D. and Blei, D. Bayesian Checking for Topic Models
}

