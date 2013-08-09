\name{ritesink}
\alias{ritesink}
\title{ritesink}
\description{An experimental output tool}
\usage{
ritesink(evalenv=.GlobalEnv)
}
\arguments{
  \item{evalenv}{The environment in which the script should be evaluated, e.g., \code{.GlobalEnv}. If \code{NULL}, the default is a hidden environment, internal to \code{rite} meaning that, e.g., variables created/modified in \code{rite} are not accessible via the console.}
}
\details{
The beginnings of an experimental tool for formatting R output. Not really ready for primetime.
}
%\value{}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{\dontrun{ritesink()}}
\keyword{ IO }
