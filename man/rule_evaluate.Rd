% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rule.R
\name{rule_evaluate}
\alias{rule_evaluate}
\title{Evaluate a treatment allocation rule}
\usage{
rule_evaluate(rule, samples)
}
\arguments{
\item{rule}{A treatment allocation \link{rule}.}

\item{samples}{A matrix of samples from the posterior distribution of the
parameter of interest, with a column for each treatment arm.}
}
\description{
An implementation must populate the rule with the \verb{$result} element,
including at least the next allocation in \verb{$result$A}.
}
