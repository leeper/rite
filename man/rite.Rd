\name{rite}
\alias{rite}
\alias{riteout}
\title{rite}
\description{Open rite}
\usage{
rite(filename=NULL, catchOutput=FALSE, evalenv=.GlobalEnv,
     fontFamily="Courier", fontSize=10, orientation="horizontal", 
	 highlight="r", color=NULL, autosave=TRUE, echo=FALSE, ...)

riteout(...)
}
\arguments{
  \item{filename}{Optionally, a character string specifying a file name in the working directory (or file path) to open in rite.}
  \item{catchOutput}{A logical specifying whether output and event handling (errors, warnings, messages, interrupts) should be sent to the rite output viewer panel rather than the R console. Default is \code{FALSE}.}
  \item{evalenv}{The environment in which the script should be evaluated, e.g., \code{.GlobalEnv}. If \code{NULL}, the default is a hidden environment, internal to \code{rite} meaning that, e.g., variables created/modified in \code{rite} are not accessible via the console.}
  \item{fontFamily}{The font family used in rite. Default is \dQuote{\code{Courier}}. Available fonts can be retrieved by \code{.Tcl("font families")}.}
  \item{fontSize}{The font size used in rite. Default is \code{10}.}
  \item{orientation}{If \code{catchOutput=TRUE}, whether the output and error panels should be oriented \dQuote{horizontal} (to the right) or \dQuote{vertical} (below) the script editing panel. Default is \dQuote{horizontal}.}
  \item{highlight}{A character vector containing one or more of \dQuote{r}, \dQuote{latex}, \dQuote{markdown}, \dQuote{xml}, \dQuote{roxygen}, \dQuote{brew}, and \dQuote{rest} to indicate what should be higlighted in script. Default is \dQuote{r}. Note that using more than a few of these simultaneously may cause unexpected highlighting.}
  \item{color}{Either \code{NULL}, or a named list specifying Tcl/Tk colors for highlighting. See details below.}
  \item{autosave}{A logical specifying whether the script should automatically be saved whenever any of the script is run. Default is \code{TRUE}. Note: if \code{filename} is \code{NULL}, the result is saved in the current R session's temporary directory and will be deleted when the R session terminates normally.}
  \item{echo}{Whether the R calls should be copied to output during \code{source} (only when \code{catchOutput=FALSE}). Default is \code{FALSE}.}
  \item{...}{Ignored by \code{rite}. Used by \code{riteout} to pass arguments to \code{rite}.}
}
\details{
Create, edit, and save R scripts (or any text-based file) and, optionally, sink output to an output viewer rather than the R console.

Scripts can be loaded, appended, referenced (via \code{source}) in the current script, and can be saved.

Scripts can be run by selection, line, or the entire contents of the script editor from the \dQuote{Run} menu. Scripts can also be run from the context menu (via a right mouse click) or with \code{<Control-r>}. \code{<F8>} runs the entire script, whereas \code{<F7>} checks for parsing errors in the entire script without evaluating it.

\code{riteout} is a wrapper for \code{rite} that defaults to \code{catchOutput=TRUE}.
}

\section{Syntax highlighting}{
By default, the following colors are used for syntax highlighting:
  \code{Normal text (normal)}: {\dQuote{black}}
  \code{Editor background (background)}: {\dQuote{white}}
  \code{R functions (functions)}: {\dQuote{purple} (this applies to both base functions and those from packages loaded within \code{rite})}
  \code{R comments (rcomments)}: {\dQuote{darkgreen}}
  \code{Operators (operators)}: {\dQuote{blue}}
  \code{Brackets (brackets)}: {\dQuote{darkblue}}
  \code{Digits (digits)}: {\dQuote{orange}}
  \code{Character strings (characters)}: {\dQuote{darkgray}}
  \code{LaTeX macros (latexmacros)}: {\dQuote{darkred}}
  \code{LaTeX equations (latexequations)}: {\dQuote{black}}
  \code{LaTeX comments (latexcomments)}: {\dQuote{red}}
  \code{Sweave/knitr code chunks (rnwchunks)}: {\dQuote{blue}}
  \code{Rtex code chunks (rtexchunks)}: {\dQuote{blue}}
  \code{Markdown (rmd)}: {\dQuote{darkred}} % not supported yet
  \code{Markdown code chunks (rmdchunks)}: {\dQuote{blue}}
  \code{XML/HTML tags (xml)}: {\dQuote{darkred}}
  \code{XML/HTML comments (xmlcomments)}: {\dQuote{red}}
  \code{Roxygen text (roxygentext)}: {\dQuote{black}}
  \code{Roxygen code chunks (roxygenchunks)}: {\dQuote{blue}}
  \code{Brew comments (brewcomments)}: {\dQuote{red}}
  \code{Brew chunks (brewchunks)}: {\dQuote{blue}}
  \code{Brew templates (brewtemplate)}: {\dQuote{black}}
  \code{reST chunks (restchunks)}: {\dQuote{blue}}

The use of highlighting in general can be regulated by the \code{highlight} parameter. To specify alternative specific colors for any of the above highlighting rules, the \code{color} parameter accepts a named list of content types (listed in parentheses above) and their corresponding colors (in quotes). For example, calling \code{rite(color=list(rcomments='pink'))}, would open \code{rite} with R comments highlighted in pink but leave all other highlighting rules at their default settings.
}

\section{Shortcut keys in widget}{
	\subsection{Key combinations}{
	
	\code{<Ctrl-o>}: Open script
	
	\code{<Ctrl-s>}: Save script as
	
	\code{<Ctrl-r>}: Run/evaluate line (or selection, if applicable)
	
	\code{<Control-Return>}: Run/evaluate line (or selection, if applicable)
	
	\code{<Ctrl-c>}: Copy
	
	\code{<Ctrl-x>}: Cut
	
	\code{<Ctrl-p>}: Paste
	
	\code{<Ctrl-a>}: Select all
	
	\code{<Ctrl-e>}: Move cursor to end of current line
	
	\code{<Shift-e>}: Adjust selection to end of current line
	
	\code{<Ctrl-f>}: Find/replace
	
	\code{<Ctrl-g>}: Go to line
	
	\code{<Ctrl-z>}: Undo
	
	\code{<Ctrl-y>}: Redo
	
	\code{<Tab>}: Indent line
	
	\code{<Ctrl-i>}: Indent line(s)
	
	\code{<Ctrl-u>}: Unindent line
	
	\code{<Ctrl-k>}: Toggle comment on/off for line(s)
	
	\code{<Shift-Tab>}: Open command completion context menu (based on cursor position)
	
	\code{<Ctrl-l>}: Clear output panel
	
	\code{<Shift>} and \code{<Left>}, \code{<Right>}, or drag left mouse: Adjust selection by character
	
	\code{<Control-Shift-Left>} or \code{<Control-Shift-Right>}: Adjust selection by word
	
	\code{<Control-Up>} or \code{<Control-Down>}: Move insertion cursor by paragraphs
	
	\code{<Control-Shift-Up>} or \code{<Control-Shift-Down>}: Adjust selection by paragraph
	}

	\subsection{Function keys}{
	
	\code{<F1>}: Open help for current function, if a known function (based on cursor position); or the results of help.search()
	
	\code{<F2>}: Open command completion context menu (based on cursor position)
	
	\code{<F3>}: Find
	
	\code{<F7>}: Try to parse the script and return any syntax errors (does not evaluate the script)
	
	\code{<F8>}: Run/evaluate all code
	}

	\subsection{Mouse shortcuts}{
	
	Left mouse click (1 time): Move cursor
	
	Left mouse click (2 times): Select word
	
	Left mouse click (3 times): Select line
	
	Right mouse click (1 time): Open context menu
	}
}

\section{Report generation with knitr}{
If \code{catchOutput=TRUE}, \code{rite} provides a number of report generation capabilities provided by \code{knitr}. Specifically, a \dQuote{Report Generation} menu becomes available that includes the following options. The first four options simply return the results of \code{knit} or \code{purl} to the output panel (without saving the results). Results can be saved using \dQuote{Save Output} from the \dQuote{Output} menu.

\code{knit}: Runs \code{knit} on the contents of the script panel and returns them in the output panel.

\code{knit (from Sweave)}: Runs \code{Sweave2knitr} on the contents of the script panel and runs \code{knit} on the results, returning the output as above.

\code{purl}: Runs \code{purl} on the contents of the script panel and returns them in the output panel.

\code{purl (from Sweave)}: Runs \code{Sweave2knitr} on the contents of the script panel and runs \code{purl} on the results, returning the output as above.

\code{knit to pdf}: Runs \code{knit} on the contents of the script panel, returning the results to the output panel (and saving them locally) before running \code{pdflatex} and \code{bibtex} on the results.

\code{knit to pdf (from Sweave)}: The same as \code{knit to pdf}, except running \code{Sweave2knitr} before processing, as above.

\code{pdflatex (from rite script)}: Runs \code{pdflatex} and \code{bibtex} on the contents of the script panel. If the script has not yet been saved, \code{rite} will prompt you to do so before proceeding. The log files are printed to the message panel.

\code{pdflatex (from .tex file)}: Runs \code{pdflatex} and \code{bibtex} on a selected file, which does not need to be open in the script panel. The log files are printed to the message panel.

}
%\value{}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{\dontrun{rite(catchOutput=TRUE)}}
\keyword{ IO }
