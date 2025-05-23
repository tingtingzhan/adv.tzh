% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/byte_format.R
\name{byte_format}
\alias{byte_format}
\title{Format Byte Size}
\usage{
byte_format(x, units = "auto")
}
\arguments{
\item{x}{\link[base]{numeric} scalar or \link[base]{vector}, byte size}

\item{units}{\link[base]{character} scalar,
see parameter \code{units} of function \link[utils]{format.object_size}}
}
\value{
Function \code{\link[=byte_format]{byte_format()}} returns a \link[base]{character} scalar or \link[base]{vector}.
}
\description{
Format byte size.
}
\details{
Function \code{\link[=byte_format]{byte_format()}} formats byte size
in the same manner as
function \link[utils]{format.object_size} does to \link[utils]{object.size}.
}
\examples{
# './R' |> list.files(full.names = TRUE) |> file.size() |> byte_format(units = 'auto')
}
\keyword{internal}
