% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weibull_class.R
\name{s_weibull}
\alias{s_weibull}
\title{Factory of SURVIVAL objects with Weibull distributions}
\usage{
s_weibull(...)
}
\arguments{
\item{...}{Parameters to define the distribution. See the Parameters for details}
}
\value{
a SURVIVAL object of the Weibull distribution family. See the
documentation of \code{s_factory} for the methods available for SURVIVAL objects
}
\description{
Creates a SURVIVAL object with a Weibull distribution.
}
\section{Parameters}{


To create an exponential survival object the following
options are available:

\emph{\code{scale}} and \emph{\code{shape}} to specify the canonical parameters of the distribution, or

\emph{\code{surv}}, \emph{\code{t}} and \emph{\code{shape}} for the proportion surviving (no events) at time t and the shape parameter, or

\emph{\code{fail}}, \emph{\code{t}} and \emph{\code{shape}} for the proportion failing (events) at time t and the shape parameter or

\emph{\code{intercept}} and \emph{\code{scale}} for the parameters returned by \code{survreg(.., dist = "weibull")} models.

scale = -log(surv)/(t^shape)

scale = -log(1-fail)/(t^shape)

The parameters should be spell correctly as partial matching is not available
}

\examples{
s_weibull(scale = 2,shape = 2)
s_weibull(surv = 0.6, t= 12, shape = 0.5)
s_weibull(fail = 0.4, t = 12, shape =0.5)
s_weibull(intercept = 0.4, scale = 0.5)
}
