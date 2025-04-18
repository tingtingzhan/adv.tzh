% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ns.R
\name{ns_name}
\alias{ns_name}
\title{Attach \link[base:getNamespace]{Namespace} and \code{:} to Function Name}
\usage{
ns_name(x)
}
\arguments{
\item{x}{a \link[utils]{getAnywhere} object}
}
\value{
Function \code{\link[=ns_name]{ns_name()}} returns a \link[base]{character} scalar or \link[base]{vector}.
}
\description{
Attach \link[base:getNamespace]{Namespace} and \code{:} to Function Name
}
\examples{
'rlnorm' |> getAnywhere() |> ns_name() # exported
'format_perc' |> getAnywhere() |> ns_name() # not exported
library(dplyr); 'filter' |> getAnywhere() |> ns_name() # conflict
}
\keyword{internal}
