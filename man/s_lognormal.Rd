% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lognormal_class.R
\name{s_lognormal}
\alias{s_lognormal}
\title{Factory of SURVIVAL objects with Log Normal distributions}
\usage{
s_lognormal(...)
}
\arguments{
\item{...}{Parameters to define the distribution. See the Parameters for details}
}
\value{
a SURVIVAL object of the log-normal distribution family. See the
documentation of \code{s_factory} for the methods available for SURVIVAL objects
}
\description{
Creates a SURVIVAL object with a Log Normal distribution.
}
\section{Parameters}{


To create an exponential survival object the following
options are available:

\emph{\code{scale}} and \emph{\code{shape}} to specify the canonical parameters of the distribution, or

\emph{\code{surv}}, \emph{\code{t}} and \emph{\code{shape}} for the proportion surviving (no events) at time t and the shape parameter, or

\emph{\code{fail}}, \emph{\code{t}} and \emph{\code{shape}} for the proportion failing (events) at time t and the shape parameter or

\emph{\code{intercept}} and \emph{\code{shape}} for the parameters returned by \code{survreg(.., dist = "lognormal")} models.

The scale parameter is the median value of the distribution, and the shape is the log standard deviation

The parameters should be spell correctly as partial matching is not available
}

\examples{
s_lognormal(scale = 2,shape = 2)
s_lognormal(surv = 0.6, t= 12, shape = 0.5)
s_lognormal(fail = 0.4, t = 12, shape =0.5)
s_lognormal(intercept = 0.4, scale = 0.5)
}
