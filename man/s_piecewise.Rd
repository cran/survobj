% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/piecewise_exponential_class.R
\name{s_piecewise}
\alias{s_piecewise}
\title{Factory of SURVIVAL objects with Piecewise Exponential distributions}
\usage{
s_piecewise(...)
}
\arguments{
\item{...}{Parameters to define the distribution. See the Parameters for details}
}
\value{
a SURVIVAL object of the piecewise exponential distribution family. See the
documentation of \code{s_factory} for the methods available for SURVIVAL objects
}
\description{
Creates a SURVIVAL object with an Piecewise Exponential distribution.
}
\section{Parameters}{


To create an piecewise exponential survival object the following
options are available:

\emph{\code{breaks}} and \emph{\code{hazards}} to specify the exponential (constant) hazard until each break, or

\emph{\code{surv}}, \emph{\code{breaks}} and \emph{\code{segments}} for the proportion surviving (no events) at the end of last segment or

\emph{\code{fail}}, \emph{\code{breaks}} and \emph{\code{segments}} for the proportion failing (events) at the end of last segment

If \emph{\code{surv}} or \emph{\code{fail}} parameters are indicated, the \emph{\code{segments}} are scaled to hazards in order
to mach the surviving or failing proportion at the end of the last segment.

Define the last break point as \code{Inf} to fully define the distribution, otherwise
an error will be produce if function after the last break is requested

The parameters should be spell correctly as partial matching is not available
}

\examples{
s_piecewise(breaks = c(1,2,3,Inf), hazards = c(0.5,0.6,0.5,0.1))
s_piecewise(surv = 0.4, breaks = c(1,2,3,Inf), segments = c(1,2,1,2))
s_piecewise(fail = 0.6, breaks = c(1,2,3,Inf), segments = c(1,2,1,2))
}
