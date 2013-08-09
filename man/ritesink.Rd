\name{ritesink}
\alias{ritesink}
\title{ritesink}
\description{An experimental tcl/tk output widget}
\usage{
ritesink(evalenv=.GlobalEnv, fontFamily="Courier", fontSize=10)
}
\arguments{
  \item{evalenv}{The environment in which the script should be evaluated, e.g., \code{.GlobalEnv}. If \code{NULL}, the default is a hidden environment, internal to \code{rite} meaning that, e.g., variables created/modified in \code{rite} are not accessible via the console.}
  \item{fontFamily}{The font family used in rite. Default is \dQuote{\code{Courier}}. Available fonts can be retrieved by \code{.Tcl("font families")}.}
  \item{fontSize}{The font size used in rite. Default is \code{10}.}
}
\details{
The beginnings of an experimental widget for displaying R output with highlighting. Not really ready for primetime yet.
}
%\value{}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{\dontrun{ritesink()}}
\keyword{ IO }
