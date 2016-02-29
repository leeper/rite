\name{ritesink}
\alias{sinkstart}
\alias{sinkstop}
\title{ritesink}
\description{An experimental tcl/tk output widget}
\usage{
sinkstart(echo = TRUE, print.eval = TRUE, split = FALSE,
          fontFamily = 'Courier', fontSize = 10,
          col.bg = 'white', col.call = c('black',col.bg),
          col.result = c('black',col.bg), col.err = c('red',col.bg),
          col.warn = c('purple',col.bg), col.msg = c('blue',col.bg))

sinkstop(quiet = TRUE)
}
\arguments{
  \item{echo}{A logical indicating whether calls should be output to the sink. Default is \code{TRUE}.}
  \item{echo}{A logical indicating whether evaluated output from calls and/or messages should be sent to the sink. Default is \code{TRUE}.}
  \item{split}{A logical indicating whether output (but not messages) should be split between the console and the sink. Default is \code{FALSE}.}
  \item{fontFamily}{The font family used in rite. Default is \dQuote{\code{Courier}}. Available fonts can be retrieved by \code{.Tcl("font families")}.}
  \item{fontSize}{The font size used in rite. Default is \code{10}.}
  \item{col.bg}{A one-element character string indicating a tcl/tk color for the background color of the sink. A list of available tcl/tk colors can be found here: \url{http://www.tcl.tk/man/tcl8.5/TkCmd/colors.htm}.}
  \item{col.call}{A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of evaluated R calls (only visible if \code{echo=TRUE}..}
  \item{col.result}{A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of standard output.}
  \item{col.err}{A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of errors.}
  \item{col.warn}{A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of warnings.}
  \item{col.msg}{A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of messages.}
  \item{quiet}{A logical indicating whether to suppress confirmation that everything is cleaned up. Default is \code{TRUE}.}
}
\details{
These functions make use of a couple of different R features to build a color-coded output window for the R console. While the console is limited to displaying output in monochrome plain text, the rite sink allows multi-colored output and messages to be piped to a single widget that highlights errors, warning, and messages. Unlike a traditional sink, rite sink is a tcl/tk widget that updates output as commands and messages occur. Accomplishing this requires the use of \code{sink}, task callbacks, and a custom error handler. (This is similar to the R2HTML package.) 

\code{sinkstart} starts the sink and \code{sinkstop} stops the sink without destroying the widget. Closing the sink widget invisibly calls \code{sinkstop} and cleans up. The sink can be turned on and off repeatedly without closing the widget.
}
\value{NULL}
\section{Shortcut keys in widget}{
	\code{<Ctrl-c>}: Copy
	
	\code{<Ctrl-x>}: Cut
	
	\code{<Ctrl-p>}: Paste
	
	\code{<Ctrl-a>}: Select all
	
	\code{<Ctrl-s>}: Save output
	
	\code{<Ctrl-l>}: Clear output
}
\references{
\href{http://developer.r-project.org/TaskHandlers.pdf}{Top-level Task Callbacks in R}

\href{http://cran.r-project.org/web/packages/R2HTML/index.html}{R2HTML package}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
sinkstart() # open the sink
sinkstop() # close the sink
}
}
\keyword{ IO }
