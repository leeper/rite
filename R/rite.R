#' @rdname rite
#' @title rite
#' @description Open rite
#' @param filename Optionally, a character string specifying a file name in the working directory (or file path) to open in rite.
#' @param catchOutput A logical specifying whether output and event handling (errors, warnings, messages, interrupts) should be sent to the rite output viewer panel rather than the R console. Default is \code{FALSE}.
#' @param evalenv The environment in which the script should be evaluated, e.g., \code{.GlobalEnv}. If \code{NULL}, the default is a hidden environment, internal to \code{rite} meaning that, e.g., variables created/modified in \code{rite} are not accessible via the console.
#' @param fontFamily The font family used in rite. Default is \dQuote{\code{Courier}}. Available fonts can be retrieved by \code{.Tcl("font families")}.
#' @param fontSize The font size used in rite. Default is \code{10}.
#' @param orientation If \code{catchOutput=TRUE}, whether the output and error panels should be oriented \dQuote{horizontal} (to the right) or \dQuote{vertical} (below) the script editing panel. Default is \dQuote{horizontal}.
#' @param fastinsert A logical, specifying whether insertion (from opening a script, appending a script, or pasting a script) should be done \dQuote{fast} (i.e., without running syntax highlighting. This can significantly improve load times but has the consequence of leaving added text unhighlighted unless modified. Default is \code{FALSE}.
#' @param highlight A character vector containing one or more of \dQuote{r}, \dQuote{latex}, \dQuote{markdown}, \dQuote{xml}, \dQuote{roxygen}, \dQuote{brew}, and \dQuote{rest} to indicate what should be higlighted in script. Default is \dQuote{r}. Note that using more than a few of these simultaneously may cause unexpected highlighting.
#' @param color Either \code{NULL}, or a named list specifying Tcl/Tk colors for highlighting. See details below.
#' @param autosave A logical specifying whether the script should automatically be saved whenever any of the script is run. Default is \code{TRUE}. Note: if \code{filename} is \code{NULL}, the result is saved in the current R session's temporary directory and will be deleted when the R session terminates normally.
#' @param echo Whether the R calls should be copied to output during \code{source} (only when \code{catchOutput=FALSE}). Default is \code{TRUE}.
#' @param tab A character string indicating what text should be inserted when the \code{<TAB>} key is pressed. Default is the \code{\\t} character. Alternatively, a numeric value (e.g., \code{4}) is treated as a number of spaces.
#' @param comment A character string indicating what text should be used for commenting. Default is the hash/pound character.
#' @param url If \code{filename=NULL}, \code{url} is considered as the URL for a remote script to load.
#' @param \dots Ignored by \code{rite}. Used by \code{riteout} to pass arguments to \code{rite}.
#' @details Create, edit, and save R scripts (or any text-based file) and, optionally, sink output to an output viewer rather than the R console.
#' 
#' Scripts can be loaded, appended, referenced (via \code{source}) in the current script, and can be saved. As of rite version 0.3, scripts can also be saved to and loaded from anonymous GitHub \href{https://gist.github.com/}{gists} to facilitate code sharing.
#' 
#' Scripts can be run by selection, line, or the entire contents of the script editor from the \dQuote{Run} menu. Scripts can also be run from the context menu (via a right mouse click) or with \code{<Control-r>} or \code{<Control-Return>} to run a selection or the current line. \code{<F8>} runs the entire script, whereas \code{<F7>} checks for parsing errors in the entire script without evaluating it (all code is parsed before running, automatically).
#' 
#' Command completion is now fully supported by pressing \code{<F2>} (a right mouse button click on the menu will close it if no selection is desired). If a function is complete and followed by an open parenetheses (e.g., \code{data.frame(}, the command completion keys bring up a selectable list named arguments for the function. If a dataframe or environment name is complete and followed by a dollar sign (e.g., \code{mydf$}), the command completion keys bring up a list of named elements in the dataframe or environment. \code{<F2>} also opens code chunk arguments after code chunk openings, argument formals after function names, and working directory files and directories after open quotes.
#' 
#' \code{riteout} is a wrapper for \code{rite} that defaults to \code{catchOutput=TRUE}, which provides access to various report generation tools, described in detail below.
#' @section Syntax highlighting:
#' rite supports syntax highlighting for R (by default) and, optionally, a number of other R-related languages LaTeX, \href{http://www.rstudio.com/ide/docs/authoring/using_markdown}{R-flavored markdown}, XML and HTML, \href{http://yihui.name/knitr/demo/stitch/}{roxygen}, \href{http://cran.r-project.org/web/packages/brew/index.html}{brew}, and restructured text (reST).
#' 
#' Functions and other objects from loaded packages are highlighted by default on-the-fly.
#' 
#' By default, the following colors are used for syntax highlighting:
#'   \code{Normal text (normal)}: {\dQuote{black}}
#'   \code{Editor background (background)}: {\dQuote{white}}
#'   \code{R functions (functions)}: {\dQuote{purple} (this applies to both base functions and those from packages loaded within \code{rite})}
#'   \code{R comments (rcomments)}: {\dQuote{darkgreen}}
#'   \code{Operators (operators)}: {\dQuote{blue}}
#'   \code{Brackets (brackets)}: {\dQuote{darkblue}}
#'   \code{Digits (digits)}: {\dQuote{orange}}
#'   \code{Character strings (characters)}: {\dQuote{darkgray}}
#'   \code{LaTeX macros (latexmacros)}: {\dQuote{darkred}}
#'   \code{LaTeX equations (latexequations)}: {\dQuote{black}}
#'   \code{LaTeX comments (latexcomments)}: {\dQuote{red}}
#'   \code{Sweave/knitr code chunks (rnwchunks)}: {\dQuote{blue}}
#'   \code{Rtex code chunks (rtexchunks)}: {\dQuote{blue}}
#'   \code{Markdown (rmd)}: {\dQuote{darkred}} % not supported yet
#'   \code{Markdown code chunks (rmdchunks)}: {\dQuote{blue}}
#'   \code{XML/HTML tags (xml)}: {\dQuote{darkred}}
#'   \code{XML/HTML comments (xmlcomments)}: {\dQuote{red}}
#'   \code{Roxygen text (roxygentext)}: {\dQuote{black}}
#'   \code{Roxygen code chunks (roxygenchunks)}: {\dQuote{blue}}
#'   \code{Brew comments (brewcomments)}: {\dQuote{red}}
#'   \code{Brew chunks (brewchunks)}: {\dQuote{blue}}
#'   \code{Brew templates (brewtemplate)}: {\dQuote{black}}
#'   \code{reST chunks (restchunks)}: {\dQuote{blue}}
#' 
#' The use of highlighting in general can be regulated by the \code{highlight} parameter. To specify alternative specific colors for any of the above highlighting rules, the \code{color} parameter accepts a named list of content types (listed in parentheses above) and their corresponding colors (in quotes). For example, calling \code{rite(color=list(rcomments='pink'))}, would open \code{rite} with R comments highlighted in pink but leave all other highlighting rules at their default settings.
#' @section Shortcut keys in widget:
#'     \subsection{Key combinations}{
#'     
#'     \kbd{<Ctrl-o>}: Open script
#'     
#'     \kbd{<Ctrl-s>}: Save script as
#'     
#'     \kbd{<Ctrl-r>}: Run/evaluate line (or selection, if applicable)
#'     
#'     \kbd{<Control-Return>}: Run/evaluate line (or selection, if applicable)
#'     
#'     \kbd{<Ctrl-c>}: Copy
#'     
#'     \kbd{<Ctrl-x>}: Cut
#'     
#'     \kbd{<Ctrl-p>}: Paste
#'     
#'     \kbd{<Ctrl-a>}: Select all
#'     
#'     \kbd{<Ctrl-e>}: Move cursor to end of current line
#'     
#'     \kbd{<Shift-e>}: Adjust selection to end of current line
#'     
#'     \kbd{<Ctrl-f>}: Find/replace
#'     
#'     \kbd{<Ctrl-g>}: Go to line
#'     
#'     \kbd{<Ctrl-z>}: Undo
#'     
#'     \kbd{<Ctrl-y>}: Redo
#'     
#'     \kbd{<Tab>}: Indent line
#'     
#'     \kbd{<Ctrl-i>}: Indent line(s)
#'     
#'     \kbd{<Ctrl-u>}: Unindent line
#'     
#'     \kbd{<Ctrl-k>}: Toggle comment on/off for line(s)
#'     
#'     %\kbd{<Shift-Tab>}: Open command completion context menu (based on cursor position)
#'     
#'     \kbd{<Ctrl-l>}: Clear output panel
#'     
#'     \kbd{<Shift>} and \kbd{<Left>}, \kbd{<Right>}, or drag left mouse: Adjust selection by character
#'     
#'     \kbd{<Control-Shift-Left>} or \kbd{<Control-Shift-Right>}: Adjust selection by word
#'     
#'     \kbd{<Control-Up>} or \kbd{<Control-Down>}: Move insertion cursor by paragraphs
#'     
#'     \kbd{<Control-Shift-Up>} or \kbd{<Control-Shift-Down>}: Adjust selection by paragraph
#'     }
#' 
#'     \subsection{Function keys}{
#'     
#'     \kbd{<F1>}: Open help for current function, if a known function (based on cursor position); or the results of help.search()
#'     
#'     \kbd{<F2>}: Open command completion context menu (based on cursor position). A right-button mouse click closes the context menu if no selection is desired (not necessary on all platforms).
#'     
#'     \kbd{<F3>}: Find
#'     
#'     \kbd{<F7>}: Try to parse the script and return any syntax errors (does not evaluate the script)
#'     
#'     \kbd{<F8>}: Run/evaluate all code
#'     }
#' 
#'     \subsection{Mouse shortcuts}{
#'     
#'     Left mouse click (1 time): Move cursor
#'     
#'     Left mouse click (2 times): Select word
#'     
#'     Left mouse click (3 times): Select line
#'     
#'     Right mouse click (1 time): Open context menu
#'     }
#' @section Report generation with knitr:
#' If \code{catchOutput=TRUE} (or rite is loaded with \code{riteout}), \code{rite} provides a number of report generation capabilities provided by \code{knitr}. Specifically, a \dQuote{Report Generation} menu becomes available that includes the following options. By default, the available tools generate reports from the currently open script in rite, but all the tools can also be run on local files (which opens those files into rite). The general pattern of report generation behavior is to load the input file, display the output file in the \code{riteout} output tab, display any relevant processing information in the \code{riteout} message tab, and open the resulting output file (in the case of functions that produce PDFs or HTML files). The resulting output files can be saved using \dQuote{Save Output} from the \dQuote{Output} menu.
#' 
#' The following tools are available:
#' 
#' \code{knit}: Runs \code{knit} on the contents of the script panel and returns them in the output panel. Optionally, an R markdown (Rmd) file can be converted directly to HTML and opened. And, optionally, an Rnw file (for knitr or Sweave) can be converted directly to PDF and opened. Support for converting Sweave documents to knitr format can prevent compatibility issues between the two formats.
#' 
#' \code{purl}: Runs \code{purl} on the contents of the script panel and returns them in the output panel. This is the knitr equivalent of Stangle, producing code-only output from the original document. Again, optionally, an Rnw file (for knitr or Sweave) can be converted directly to PDF and opened. Support for converting Sweave documents to knitr format can prevent compatibility issues between the two formats.
#' 
#' \code{stitch}: Runs \code{stitch} on the contents of the script panel, embedding it into an LaTeX file (and opens the resulting PDF), markdown file (and opens the resulting HTML), or HTML file (and opens the resulting HTML), based on a simple template. This is helpful for converting an unformatted R script into a simple report.
#' 
#' \code{spin}: Runs \code{spin} on the contents of the script panel, which should be \href{http://yihui.name/knitr/demo/stitch/}{a specially formatted roxygen-style script}. The output is a knitr file, which can optionally be \code{knit} or simply displayed into the output tab.
#' 
#' \code{markdown}: Convert an R markdown document to either a full HTML document or an HTML fragment (without \code{header} and \code{body} tags) to embed, e.g., in a blog post.
#' 
#' \code{LaTeX} and \code{XeLaTeX}: Run LaTeX or XeLaTeX and, optionally, BibTex as a system call. knitr compiles LaTeX to PDF via DVI using the functionality supplied by the \code{tools} package, so these report generation tools provide functionality for .tex scripts that are not compatible with DVI.
#' 
#' @return \code{NULL}
#' @author Thomas J. Leeper
#' @examples
#' \dontrun{
#' # run the simple script editor
#' rite()
#' }
#' 
#' \dontrun{
#' # run the script editor with output tools
#' riteout()
#' }
#' @keywords IO 
#' @import tcltk
#' @import knitr
#' @importFrom tcltk2 tk2notebook tk2notetab is.ttk
#' @importFrom utils available.packages install.packages
#' @importFrom curl curl_fetch_memory new_handle
#' @importFrom tools texi2pdf file_path_sans_ext
#' @importFrom markdown rpubsUpload
#' @importFrom formatR tidy_source tidy_eval
#' @importFrom stats na.omit
#' @importFrom grDevices bringToTop
#' @importFrom utils Sweave apropos browseURL capture.output help help.search
#' @importFrom utils loadhistory packageDescription savehistory update.packages
#' @export
rite <- function(filename=NULL, catchOutput=FALSE, evalenv=.GlobalEnv,
                fontFamily="Courier", fontSize=10, orientation="horizontal",
                fastinsert=FALSE, highlight="r", color=NULL,
                autosave=TRUE, echo=TRUE, tab='    ', comment='#', 
                url = NULL, ...) {
    ## STARTUP OPTIONS ##
    filename <- filename # script filename (if loaded or saved)
    scriptSaved <- TRUE # a logical for whether current edit file is saved
    searchopts <- list()
        searchopts$searchterm <- ""
        searchopts$replaceterm <- ""
        searchopts$casevar <- 1
        searchopts$regoptvar <- 0
        searchopts$updownvar <- 1
        searchopts$searchfromtop <- 0
    ritetmpfile <- tempfile(pattern="rite",fileext=".r")
    riteenv$wmtitle <- packagetitle <- "rite"
    if (isTRUE(echo)) {
        echorun <- tclVar(1)
    } else {
        echorun <- tclVar(0)
    }
    echoprompt <- tclVar(1)
    if (!Sys.info()['sysname'] == "Darwin")
        addtohistory <- tclVar(1)
    everWarn <- tclVar(1)
    # optionally setup evaluation environment
    if (is.null(evalenv)) {
        editenv <- new.env()
        evalenv <- editenv
    }
    # handle tab value
    if (is.null(tab)) {
        tab <- '\t'
    } else if (is.numeric(tab)) {
        tab <- paste(rep(' ',round(tab)),collapse='')
    }
    hcolors <- list(normal = "black", # txt_edit normal font color
                    background = "white", # txt_edit background color
                    functions = "purple",
                    rcomments = "darkgreen",
                    operators = "blue",
                    brackets = "darkblue",
                    digits = "orange",
                    characters = "darkgray",
                    latexmacros = "darkred",
                    latexequations = "blue",
                    latexcomments = "red",
                    rnwchunks = "blue",
                    rtexchunks = "blue",
                    rmd = "darkred",
                    rmdchunks = "blue",
                    xml = "darkred",
                    xmlcomments = "red",
                    roxygentext = "black",
                    roxygenchunks = "blue",
                    brewcomments = "red",
                    brewchunks = "blue",
                    brewtemplate = "black",
                    restchunks = "blue"
                    )
    filetypelist <- paste(  "{{R Script} {*.R}}",
                            "{{brew} {*.brew}}",
                            "{{HTML} {*.html}}",
                            "{{R HTML} {*.Rhtml}}",
                            "{{Markdown} {*.md}}",
                            "{{R markdown} {*.Rmd}}",
                            "{{reST} {*.rst}}",
                            "{{R reST} {*.Rrst}}",
                            "{{Sweave} {*.Rnw}}",
                            "{{TeX} {*.tex}}",
                            "{{R TeX} {*.Rtex}}",
                            "{{Text file} {*.txt}}",
                            "{{All files} {*.*}}",
                        sep=" ")
    defaultfiletype <- "{{R Script} {.R}}"
    if (!is.null(color)) {
        for(i in 1:length(color)) {
            if (!is.null(color[[i]]) && !is.na(color[[i]]) && !color[[i]]=="" && is.character(color[[i]]))
                hcolors[[names(color)[i]]] <- color[[i]]
        }
    }
    if (isTRUE(catchOutput)) {
        outputSaved <- TRUE # a logical for whether current output file is saved
        outsink <- textConnection("osink", "w") # create connection for stdout
        sink(outsink, type="output") # sink stdout
        errsink <- textConnection("esink", "w") # create connection for stderr
        sink(errsink, type="message") # sink stderr
        ritecat <- textConnection("riteoutcon","w")
        cat <- function(..., sep=" ", catchOutput=catchOutput)
            writeLines(text=paste(as.character(unlist(list(...))), collapse=sep), sep="\n", con=ritecat)
    }

    ## EXIT PROCEDURE ##
    exitWiz <- function() {
        if (isTRUE(catchOutput)) {
            if (!isTRUE(outputSaved)) {
                exit <- tkmessageBox(message = "Do you want to save the output?", icon = "question", type = "yesnocancel", default = "yes")            
                if (tclvalue(exit) == "yes") {
                    saveOutput()
                } else if (tclvalue(exit) == "no") {
                } else {
                    tkfocus(txt_edit)
                    return()
                }
            }
        }
        if (!isTRUE(scriptSaved)) {
            exit <- tkmessageBox(message = "Do you want to save the script?", icon = "question", type = "yesnocancel", default = "yes")            
            if (tclvalue(exit) == "yes") {
                saveScript()
            } else if (tclvalue(exit)=="no") {
            } else {
                tkfocus(txt_edit)
                return()
            }
        } else {
            exit <- tkmessageBox(message = "Are you sure you want to close rite?", icon = "question", type = "yesno", default = "yes")
            if (tclvalue(exit) == "yes") {
            } else if (tclvalue(exit) == "no") {
                tkfocus(txt_edit)
                return()
            }
        }
        if (catchOutput) {
            if (!sink.number() == 0) {
                sink(type="output")
            }
            if (!sink.number('message') == 2) {
                sink(type="message")
            }
            if ('riteoutcon' %in% showConnections()) {
                thiscon <- rownames(showConnections())[which('riteoutcon'==showConnections()[,1])]
                close(getConnection(thiscon))
            }
            cat <<- base::cat
        }
        if ('ksink1' %in% showConnections()) {
            thiscon <- rownames(showConnections())[which('ksink1'==showConnections()[,1])]
            close(getConnection(thiscon))
        }
        if ('ksink2' %in% showConnections()) {
            thiscon <- rownames(showConnections())[which('ksink2'==showConnections()[,1])]
            close(getConnection(thiscon))
        }
        if ('osink' %in% showConnections()) {
            thiscon <- rownames(showConnections())[which('osink'==showConnections()[,1])]
            close(getConnection(thiscon))
        }
        if ('esink' %in% showConnections()) {
            thiscon <- rownames(showConnections())[which('esink'==showConnections()[,1])]
            close(getConnection(thiscon))
        }
        if (.Platform$OS.type %in% "windows") {
            bringToTop(-1)
        }
        tkdestroy(riteenv$editor)
        unlink(ritetmpfile)
    }
    
    ## FILE MENU FUNCTIONS ##
    newScript <- function() {
        if (!scriptSaved & !tclvalue(tkget(txt_edit,"0.0","end")) %in% c('','\n')) {
            exit <- tkmessageBox(message = "Do you want to save the current script?",
                                 icon = "question", type = "yesnocancel",
                                 default = "yes")
            if (tclvalue(exit) == "yes") {
                saveScript()
            } else if (tclvalue(exit) == "no") {
            } else {
                tkfocus(txt_edit)
                return(1)
            }
        }
        tkdelete(txt_edit,"0.0","end")
        filename <<- NULL
        scriptSaved <<- TRUE
        riteenv$wmtitle <<- packagetitle
        tkwm.title(riteenv$editor, riteenv$wmtitle)
        return(0)
    }
    getRawGistURL <- function(entry) {
        if (is.numeric(entry) || grepl("^[0-9a-f]+$", entry)) {
            entry <- paste("https://api.github.com/gists/", entry, sep = "")
        } else if (grepl("((^https://)|^)gist.github.com/([^/]+/)?[0-9a-f]+$", entry)) {
            entry <- paste("https://api.github.com/gists/",
                        regmatches(entry, regexpr("[0-9a-f]+$", entry)), sep = "")
        } else {
            return(NULL)
        }
        content <- try(rawToChar(curl_fetch_memory(entry)[["content"]]))
        if (!inherits(content,"try-error")) {
            tmp <- regmatches(content, gregexpr('"raw_url": ?"(.*?\\.[[:alpha:]]*)"', content))[[1]]
            rawurl <- sapply(tmp,function(x) strsplit(x,'"')[[1]][4])
            return(rawurl)
        } else {
            return(NULL)
        }
    }
    
    loadScript <- function(fname=NULL, locals=TRUE, gist=FALSE, refonly=FALSE, new=TRUE) {
        if (is.null(fname) & new) {
            if (newScript()) {
                return(1)
            }
        }
        if (isTRUE(locals)) {
            if (is.null(fname) & new) {
                fname <- tclvalue(tkgetOpenFile(title="Load Script",filetypes=filetypelist))
            } else if (is.null(fname) & !new) {
                fname <- tclvalue(tkgetOpenFile(title="Append Script",filetypes=filetypelist))
            }
            if (!length(fname) || fname == "") {
                return()
            } else {
                if (isTRUE(refonly)) {
                    tkinsert(txt_edit, "insert", paste("source(\"",filename,"\")\n",sep=""))
                } else {
                    chn <- tclopen(fname, "r")
                    insertText(tclvalue(tclread(chn)), txt_edit, fastinsert)
                    tclclose(chn)
                }
                if (isTRUE(new)) {
                    scriptSaved <<- TRUE
                    filename <<- fname
                    riteenv$wmtitle <<- paste(filename,"-",packagetitle)
                    tkwm.title(riteenv$editor, riteenv$wmtitle)
                } else {
                    scriptSaved <<- FALSE
                }
            }
        } else {
            if (!is.null(fname)) {
                content <- try(rawToChar(curl_fetch_memory(fname)[["content"]]))
                if (!inherits(content,"try-error")) {
                    insertText(content, txt_edit, fastinsert)
                } else {
                    riteMsg(output = output, errorout = err_out, "Script not loaded!", error=TRUE)
                }
            } else {
                processEntry <- function() {
                    tkdestroy(gistDialog)
                    entry <- tclvalue(entry)
                    if (isTRUE(gist)) {
                        if (grepl("((^https://)|^)gist.github.com/([^/]+/)?[0-9a-f]+$", entry))
                            entry <- regmatches(entry, regexpr("[0-9a-f]+$", entry))
                        entry <- getRawGistURL(entry)
                        if (is.null(entry)) {
                            riteMsg(output = output, errorout = err_out, "Gist not loaded!", error=TRUE)
                            return()
                        } else if (length(entry)>1) {
                            gistDialog <- tktoplevel()
                            tkwm.title(gistDialog, "Select file from gist...")
                            scr_gist <- tkscrollbar(gistDialog, repeatinterval=5, command=function(...) tkyview(gist_list,...))
                            gist_list <- tklistbox(gistDialog, height=10, width=50, selectmode="single",
                                                yscrollcommand=function(...) tkset(scr_gist,...), background="white")
                            tkgrid(gist_list, sticky="nsew", column=1, row=1)
                            tkgrid(scr_gist, sticky="nsew", column=2, row=1)
                            tkgrid.columnconfigure(gistDialog,1,weight=1)
                            tkgrid.columnconfigure(gistDialog,2,weight=0)
                            tkgrid.rowconfigure(gistDialog,1,weight=1)
                            for (i in 1:length(entry)) {
                                tkinsert(gist_list, "end", entry[i])
                            }
                            buttons <- tkframe(gistDialog)
                                OKbutton <- tkbutton(buttons, text="   OK   ",
                                    command = function() {
                                        entry <<- entry[as.numeric(tclvalue(tkcurselection(gist_list)))+1]
                                        tkdestroy(gistDialog)
                                        tkfocus(riteenv$editor)
                                    })
                                Cancelbutton <- tkbutton(buttons,text=" Cancel ",
                                    command = function() {tkdestroy(gistDialog); tkfocus(riteenv$editor)})
                                tkgrid(OKbutton, row = 1, column = 1)
                                tkgrid(Cancelbutton, row = 1, column = 2)
                            tkgrid(buttons, row=3, column=1, columnspan=3)
                            tkwait.window(gistDialog)
                        }
                    } else {
                        entry <- entry[1]
                    }
                    if (isTRUE(refonly)) {
                        if (gist || grepl("https",entry)) {
                            tkinsert(txt_edit, "insert", "source(rawConnection(curl::curl_fetch_memory('",entry,"')[['content']]))\n")
                        } else {
                            tkinsert(txt_edit, "insert", paste0("source(\"",entry,"\")\n"))
                        }
                    } else {
                        content <- try(rawToChar(curl_fetch_memory(fname)[["content"]]))
                        if (!inherits(content,"try-error")) {
                            insertText(content, txt_edit, fastinsert)
                        } else {
                            riteMsg(output = output, errorout = err_out, "Script not loaded!", error=TRUE)
                        }
                    }
                    scriptSaved <<- FALSE
                }
                gistDialog <- tktoplevel()
                if (isTRUE(gist)) {
                    tkwm.title(gistDialog, "Enter Gist ID or raw URL")
                } else {
                    tkwm.title(gistDialog, "Enter URL")
                }
                entryform <- tkframe(gistDialog, relief="groove", borderwidth=2)
                    entry <- tclVar()
                    tkgrid(ttklabel(entryform, text = "     "), row=1)
                    urlentry <- tkentry(entryform, width = 50, textvariable=entry)
                    if (isTRUE(gist)) {
                        tkgrid(tklabel(entryform, text = "ID/URL: "), row=2, column=1)
                    } else {
                        tkgrid(tklabel(entryform, text = "URL: "), row=2, column=1)
                    }
                    tkgrid(urlentry, row=2, column=2, columnspan=4)
                    tkgrid(ttklabel(entryform, text = "     "), row=3)
                tkgrid(entryform)
                buttons <- tkframe(gistDialog)
                    OKbutton <- tkbutton(buttons, text="   OK   ", command=processEntry)
                    Cancelbutton <- tkbutton(buttons, text=" Cancel ",
                        command=function() {tkdestroy(gistDialog); tkfocus(txt_edit)})
                    tkgrid(OKbutton, row=1, column=2)
                    tkgrid(Cancelbutton, row=1, column=3)
                tkgrid(buttons)
                tkbind(urlentry, "<Return>", processEntry)
                tkfocus(gistDialog)
            }
        }
    }
    if (!Sys.info()['sysname'] == "Darwin") {        
        loadHistory <- function() {
            tmphistory <- tempfile("Rrawhist")
            savehistory(tmphistory)
            loadScript(tmphistory)
            unlink(tmphistory)
        }
    }
    saveScript <- function() {
        if (is.null(filename) || !length(filename) || filename=="") {
            return(saveAsScript())
        } else {
            chn <- tclopen(filename, "w")
            tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
            tclclose(chn)
            scriptSaved <<- TRUE
            riteenv$wmtitle <<- packagetitle
            riteenv$wmtitle <<- paste(filename,"-",riteenv$wmtitle)
            tkwm.title(riteenv$editor, riteenv$wmtitle)
            return(0)
        }
    }
    saveAsScript <- function() {
        fname <- tclvalue(tkgetSaveFile(initialdir=getwd(),
                                        title="Save Script As",
                                        filetypes=filetypelist,
                                        defaultextension='.R'))
        if (!length(fname) || fname=="") {
            filename <<- ""
            return(1)
        } else {
            chn <- tclopen(fname, "w")
            tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
            tclclose(chn)
            scriptSaved <<- TRUE
            filename <<- fname
            riteenv$wmtitle <<- packagetitle
            riteenv$wmtitle <<- paste(fname,"-",riteenv$wmtitle)
            tkwm.title(riteenv$editor, riteenv$wmtitle)
            return(0)
        }
    }
    saveGist <- function(browse=FALSE) {
        gisturl <- "https://api.github.com/gists"
        if (!is.null(filename) && filename=="") {
            description <- filename
        } else {
            description <- "rite script"
        }
        content <- tclvalue(tkget(txt_edit,"0.0","end"))
        #gistbody2 <- RJSONIO::toJSON(list(description="rite script", public="true",
        #                    files=list("file1.txt"=list(content=content))),collapse="")
        content <- gsub("\n","\\\\n",content)
        content <- gsub("\t","\\\\t",content)
        content <- gsub("\r","\\\\r",content)
        gistbody <- paste('{',    '"description":"',description,
                                '","public":"true"',
                                ',"files":{"file1.txt":{"content":"',content,'"}}}',sep="")
        h <- new_handle(postfields = gistbody)
        gistout <- charToRaw(curl_fetch_memory(url = gisturl, handle = h)[["content"]])
        outsplit <- strsplit(gistout,'","')[[1]] # parse JSON
        gistid <- strsplit(outsplit[grep("id",outsplit)],':"')[[1]][2]
        gistouturl <- paste("https://gist.github.com/",gistid,sep="")
        results <- paste("Script saved as Gist ",gistid," at: ",gistouturl,sep="")
        riteMsg(output = output, errorout = err_out, results, error=TRUE)
        if (catchOutput) {
            tkselect(nb2, 1)
            tkfocus(txt_edit)
        }
        if (browse) {
            browseURL(gistouturl)
        }
        TRUE
    }
    
    uploadToRPubs <- function(new = TRUE, render='html') {
        saveScript()
        wasopen <- openreports
        if (render=='md2html') {
            openreports <- tclVar(0)
            h <- knittxt(genmode="md2html", use='current')
            openreports <- wasopen
        } else if (render=='rmd2html') {
            openreports <- tclVar(0)
            h <- knittxt(genmode="rmd2html", use='current')
            openreports <- wasopen
        } else {
            h <- filename
        }
        if (isTRUE(new)) {
            # interactively specify title
            processEntry <- function() {
                tkdestroy(rpubsDialog)
                u <- rpubsUpload(title = tclvalue(entry), htmlFile = h,
                                 id = NULL, method='internal')
                                 # temporarily 'internal' due to SSL error
                riteMsg(output = output, errorout = err_out, paste("RPubs ID is:", u$id))
                browseURL(u$continueUrl) # browse to continueUrl
                tkfocus(txt_edit)
                return()
            }

            rpubsDialog <- tktoplevel()
            tkwm.title(rpubsDialog, "Enter Title for Upload")
            entryform <- tkframe(rpubsDialog, relief="groove", borderwidth=2)
                entry <- tclVar()
                tkgrid(ttklabel(entryform, text = "     "), row=1)
                urlentry <- tkentry(entryform, width = 50, textvariable=entry)
                tkgrid(tklabel(entryform, text = "Title: "), row=2, column=1)
                tkgrid(urlentry, row=2, column=2, columnspan=4)
                tkgrid(ttklabel(entryform, text = "     "), row=3)
            tkgrid(entryform)
            buttons <- tkframe(rpubsDialog)
                OKbutton <- tkbutton(buttons, text="   OK   ", command=processEntry)
                Cancelbutton <- tkbutton(buttons, text=" Cancel ",
                    command=function() {tkdestroy(rpubsDialog); tkfocus(txt_edit)})
                tkgrid(OKbutton, row=1, column=2)
                tkgrid(Cancelbutton, row=1, column=3)
            tkgrid(buttons)
            tkbind(urlentry, "<Return>", processEntry)
            tkfocus(rpubsDialog)
        } else {
            # interactively specify RPubs id
            processEntry <- function() {
                tkdestroy(rpubsDialog)
                u <- rpubsUpload(title = NULL, htmlFile = h,
                                 id = tclvalue(entry), method='internal')
                                 # temporarily 'internal' due to SSL error
                riteMsg(output = output, errorout = err_out, paste("RPubs ID is:", u$id))
                browseURL(u$continueUrl) # browse to continueUrl
                tkfocus(txt_edit)
                return()
            }

            rpubsDialog <- tktoplevel()
            tkwm.title(rpubsDialog, "Enter RPubs ID")
            entryform <- tkframe(rpubsDialog, relief="groove", borderwidth=2)
                entry <- tclVar()
                tkgrid(ttklabel(entryform, text = "     "), row=1)
                urlentry <- tkentry(entryform, width = 50, textvariable=entry)
                tkgrid(tklabel(entryform, text = "RPubs ID: "), row=2, column=1)
                tkgrid(urlentry, row=2, column=2, columnspan=4)
                tkgrid(ttklabel(entryform, text = "     "), row=3)
            tkgrid(entryform)
            buttons <- tkframe(rpubsDialog)
                OKbutton <- tkbutton(buttons, text="   OK   ", command=processEntry)
                Cancelbutton <- tkbutton(buttons, text=" Cancel ",
                    command=function() {tkdestroy(rpubsDialog); tkfocus(txt_edit)})
                tkgrid(OKbutton, row=1, column=2)
                tkgrid(Cancelbutton, row=1, column=3)
            tkgrid(buttons)
            tkbind(urlentry, "<Return>", processEntry)
            tkfocus(rpubsDialog)
        }
    }
        
    ## RUN FUNCTIONS ##
    runCode <- function(code, chunks=FALSE) {
        if (isTRUE(autosave) & (is.null(filename) || filename=="")) {
            chn <- tclopen(ritetmpfile, "w")
            tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
            tclclose(chn)
            #scriptSaved <<- TRUE
            #tkwm.title(riteenv$editor, riteenv$wmtitle)
        } else if (isTRUE(autosave) & (!is.null(filename) && !filename=="")) {
            chn <- tclopen(filename, "w")
            tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
            tclclose(chn)
            scriptSaved <<- TRUE
            tkwm.title(riteenv$editor, riteenv$wmtitle)
        }
        if (chunks) {
            code <- purl(text=code, quiet=TRUE)
        } else {
            parsed <- tryparse(verbose=FALSE)
            if (!parsed)
                return()
        }
        search1 <- search()
        if (isTRUE(catchOutput)) {
            length2 <- length(osink)
        }
        runtemp <- tempfile()
        writeLines(code,runtemp)
        writeError <- function(errmsg, type, focus=TRUE) {
            if (type=="Error") {
                tkinsert(err_out,"end",paste(type,": ",errmsg,"\n",sep=""),("error"))
            } else if (type=="Warning") {
                tkinsert(err_out,"end",paste(type,": ",errmsg,"\n",sep=""),("warning"))
            } else if (type=="Message") {
                tkinsert(err_out,"end",paste(type,": ",errmsg,"\n",sep=""),("message"))
            } else {
                tkinsert(err_out,"end",paste(type,": ",errmsg,"\n",sep=""), ("text"))
            }
            if (focus) {
                tkselect(nb2, 1)
                tkfocus(txt_edit)
            }
        }
        displayWarningDialog <- tclVar(1)
        out <- withRestarts(withCallingHandlers(source(runtemp, print.eval=TRUE,
                echo = as.logical(as.numeric(tclvalue(echorun))),
                prompt.echo = if (as.numeric(tclvalue(echoprompt))) "> " else "",
                continue.echo = if (as.numeric(tclvalue(echoprompt))) "+ " else ""),
            error = function(errmsg) {
                errmsg <- strsplit(as.character(errmsg),": ")[[1]]
                errmsg <- paste(errmsg[-1],collapse=":")
                if (catchOutput)
                    writeError(errmsg,"Error")
            },
            warning = function(errmsg) {
                errmsg <- strsplit(as.character(errmsg),": ")[[1]]
                errmsg <- paste(errmsg[-1],collapse=":")
                if (catchOutput)
                    writeError(errmsg,"Warning")
                a <- as.logical(as.numeric(tclvalue(displayWarningDialog)))
                b <- as.logical(as.numeric(tclvalue(everWarn)))
                if (a & b) {
                    warnmess <- paste("Warning:", errmsg,
                                  "Do you want to continue evaluation?",
                                  "Choose \"Cancel\" to continue and suppress further warnings.",
                                  sep="\n")
                    errbox <- tkmessageBox(message = warnmess,
                                           icon = "error", 
                                           type = "yesnocancel", 
                                           default = "no")
                    if (tclvalue(errbox) == "no") {
                        invokeRestart("discontinue")
                    } else if (tclvalue(errbox) == "cancel") {
                        tclvalue(displayWarningDialog) <- 0
                    }
                }
            },
            message = function(errmsg) {
                errmsg <- strsplit(as.character(errmsg),": ")[[1]]
                errmsg <- paste(errmsg[-1],collapse=":")
                if (isTRUE(catchOutput)) {
                    writeError(errmsg, "Message")
                } else {
                    cat(errmsg)
                }
            },
            interrupt = function() {
                if (catchOutput) {
                    writeError("", "Interruption")
                } else {
                    riteMsg(output = output, errorout = err_out, "Evaluation interrupted!", error=TRUE)
                }
            }
        ),
        # a restart to 'discontinue' source(.)-ing
        discontinue = function() {invisible()}
        )
        if (!Sys.info()['sysname'] == "Darwin" && as.logical(as.numeric(tclvalue(addtohistory)))) {
            tmphistory <- tempfile()
            savehistory(tmphistory)
            histcon <- file(tmphistory, open="a")
            writeLines(code, histcon)
            close(histcon)
            loadhistory(tmphistory)
            unlink(tmphistory)
        }
        if (catchOutput) {
            # output to `output`
            if (length(osink)>length2) {
                tryCatch(tkinsert(output,"end",paste(osink[(length2+1):length(osink)],"\n",collapse="")),
                    error = function(e) {
                        riteMsg(output = output, errorout = err_out, paste("Printing error:",e,"!"), error=TRUE)
                    },
                    interrupt = function() {
                        riteMsg(output = output, errorout = err_out, "Printing interrupt!", error=TRUE)
                    }
                )
                tkselect(nb2, 0)
            }
            tkmark.set(output,"insert","end")
            tksee(output,"insert")
            outputSaved <<- FALSE
        }
        unlink(runtemp)
        # syntax highlighting for new packages
        search2 <- search()[!search() %in% search1]
        if (length(search2)>0) {
            for (i in seq_along(search2)) {
                packagename <- strsplit(search2[i],":")[[1]][2]
                funs <- objects(search2[i])
                if (!inherits(funs,"try-error")) {
                    tmpx <- sort(rep(1:ceiling(length(funs)/30),30))
                    tmpsplit <- split(funs,tmpx[seq_along(funs)])
                    uniqtmp <- sapply(tmpsplit, FUN=function(x) { paste(" [list",paste(x,collapse=" ")," ]") })
                    for (j in seq_along(uniqtmp)) {
                        .Tcl(paste("ctext::addHighlightClass ",.Tk.ID(txt_edit),
                                    " basefunctions",j," ", hcolors$functions, uniqtmp[j], sep=""))
                    }
                }
            }
        }
    }
    runLine <- function() {
        code <- tclvalue(tkget(txt_edit, "insert linestart", "insert lineend"))
        if (!code=="") {
            runCode(code)
        }
    }
    runSelection <- function() {
        if (!tclvalue(tktag.ranges(txt_edit,"sel"))=="") {
            runCode(tclvalue(tkget(txt_edit,"sel.first","sel.last")))
        }
    }
    runAll <- function() {
        runCode(tclvalue(tkget(txt_edit,"1.0","end")))
    }
    runAllChunks <- function() {
        runCode(tclvalue(tkget(txt_edit,"1.0","end")), chunks=TRUE)
    }

    tidyScript <- function() {
        script <- tclvalue(tkget(txt_edit,"1.0","end"))
        tkdelete(txt_edit, "1.0", "end")
        tkinsert(txt_edit, "1.0", 
                 paste0(tidy_source(text = script)$text.tidy, collapse="\n"))
    }
    
    ## OUTPUT FUNCTIONS ##
    if (isTRUE(catchOutput)) {
        saveOutput <- function(outfilename = "") {
            if (outfilename == "") {
                outfilename <- tclvalue(tkgetSaveFile(    initialdir=getwd(),
                                                        title="Save Output",
                                                        filetypes=filetypelist))
            }
            if (!length(outfilename) || outfilename=="") {
                invisible()
            }
            chn <- tclopen(outfilename, "w")
            tclputs(chn, tclvalue(tkget(output,"0.0","end")))
            tclclose(chn)
            invisible(outfilename)
        }
        clearOutput <- function() {
            tkdelete(output,"0.0","end")
            tkselect(nb2, 0)
        }
        clearError <- function() {
            tkdelete(err_out,"0.0","end")
            tkselect(nb2, 1)
        }
    }
        
    ## KNITR, etc. INTEGRATION ##
    knittxt <- function(genmode="knit", use='text', spinformat=NULL) {
        if (isTRUE(catchOutput)) {
            clearError()
        }
        ksink1 <- ""
        ksink2 <- ""
        if ('ksink1' %in% showConnections()) {
            thiscon <- rownames(showConnections())[which('ksink1'==showConnections()[,1])]
            close(getConnection(thiscon))
        }
        if ('ksink2' %in% showConnections()) {
            thiscon <- rownames(showConnections())[which('ksink2'==showConnections()[,1])]
            close(getConnection(thiscon))
        }
        knitsink1 <- textConnection("ksink1", "w") # create connection for stdout
        knitsink2 <- textConnection("ksink2", "w") # create connection for stderr
        sink(knitsink1, type="output") # sink stdout
        sink(knitsink2, type="message") # sink stderr
        
        if (use == 'text') {
            if (saveScript()) {
                return(1)
            }
            txtvalue <- tclvalue(tkget(txt_edit,"0.0","end"))
            inputvalue <- NULL
        } else if (use == 'current') {
            if (saveScript()) {
                return(1)
            }
            txtvalue <- NULL
            inputvalue <- filename
        } else if (use == 'file') {
            fname <- tclvalue(tkgetOpenFile(title="Load Script",filetypes=filetypelist))
            if (!length(fname) || fname=="") {
                riteMsg(output = output, errorout = err_out, 'No file selected', error=TRUE)
                return()
            } else {
                txtvalue <- NULL
                inputvalue <- fname
            }
        }
        
        riteMsg(output = output, errorout = err_out, "Generating report...", error=TRUE)
        
        if (genmode == "knit") {
            knit_out <- try(knit(input=inputvalue, text=txtvalue))
        } else if (genmode == "purl") {
            knit_out <- try(purl(input=inputvalue, text=txtvalue))
        } else if (genmode == "sweave") {
            sweavesty <- file.path(R.home(),"share","texmf","tex","latex","Sweave.sty")
            cdir <- dirname(inputvalue)
            if ((!"Sweave.sty" %in% cdir) && file.exists(sweavesty)) {
                file.copy(from = sweavesty, to = file.path(cdir, "Sweave.sty"))
            }
            if (is.null(txtvalue)) {
                knit_out <- Sweave(file=inputvalue, quiet=TRUE)
            } else if (!saveScript()) {
                knit_out <- Sweave(file=filename, quiet=TRUE)
            } else {
                riteMsg(output = output, errorout = err_out, "script not saved.", error=TRUE)
                return()
            }
        } else if (genmode == "knitsweave") {
            sweave_out <- try(Sweave2knitr(file=inputvalue, text=txtvalue))
            if (inherits(sweave_out, "try-error")) {
                riteMsg(output = output, errorout = err_out, "Could not convert Sweave to knitr!", error=TRUE)
                return()
            } else if (!is.null(inputvalue)) {
                knit_out <- try(knit(input=gsub("[.]([^.]+)$", "-knitr.\\1", inputvalue), text=txtvalue))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(knit(text=sweave_out))
            }
        } else if (genmode == "tangle") {
            sweave_out <- try(Sweave2knitr(file=inputvalue, text=txtvalue))
            if (inherits(sweave_out, "try-error")) {
                riteMsg(output = output, errorout = err_out, "Could not convert Sweave to knitr!", error=TRUE)
                return()
            } else if (!is.null(inputvalue)) {
                knit_out <- try(purl(input=gsub("[.]([^.]+)$", "-knitr.\\1", inputvalue), text=txtvalue))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(purl(text=sweave_out))
            }
        } else if (genmode == "rmd2html") {
            if (!is.null(inputvalue)) {
                knit_out <- try(knit2html(input=inputvalue))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(knit2html(text=txtvalue))
            }
        } else if (genmode == "md2html") {
            if (!is.null(inputvalue)) {
                outfile <- substring(basename(inputvalue),1,regexpr("\\.[[:alnum:]]+$",basename(inputvalue))-1)
                outfile <- paste(outfile,'html',sep='.')
                knit_out <- try(markdown::markdownToHTML(file=inputvalue, output=outfile))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(markdown::markdownToHTML(text=txtvalue))
            }
        } else if (genmode == "md2html.fragment") {
            if (!is.null(inputvalue)) {
                outfile <- substring(basename(inputvalue),1,regexpr("\\.[[:alnum:]]+$",basename(inputvalue))-1)
                outfile <- paste(outfile,'html',sep='.')
                knit_out <- try(markdown::markdownToHTML(file=inputvalue,output=outfile,fragment.only=TRUE))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(markdown::markdownToHTML(text=txtvalue,fragment.only=TRUE))
            }
        } else if (genmode == "stitch.rnw") {
            if (!is.null(inputvalue)) {
                knit_out <- try(stitch(script=inputvalue))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(stitch(text=txtvalue))
            }
            if (!inherits(knit_out,"try-error")) {
                knit_out_pdf <- paste(file_path_sans_ext(knit_out),"pdf",sep=".")
            } else {
                knit_out_pdf <- NULL
            }
        } else if (genmode == "stitch.rhtml") {
            if (!is.null(inputvalue)) {
                knit_out <- try(stitch_rhtml(script=inputvalue))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(stitch_rhtml(text=txtvalue))
            }
        } else if (genmode == "stitch.rmd") {
            if (!is.null(inputvalue)) {
                knit_out <- try(stitch_rmd(script=inputvalue))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(stitch_rmd(text=txtvalue))
            }
        } else if (genmode == "spin") {
            if (!is.null(inputvalue)) {
                knit_out <- try(spin(hair=inputvalue, text=NULL, knit=FALSE, format=spinformat))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(spin(hair=NULL, text=txtvalue, knit=FALSE, format=spinformat))
            }
        } else if (genmode == "spinknit") {
            if (!is.null(inputvalue)) {
                knit_out <- try(spin(hair=inputvalue, text=NULL, knit=TRUE, format=spinformat))
            } else if (!is.null(txtvalue)) {
                knit_out <- try(spin(hair=NULL, text=txtvalue, knit=TRUE, format=spinformat))
            }
        } else {
            riteMsg(output = output, errorout = err_out, "Unrecognized report type!", error=TRUE)
            return(invisible(NULL))
        }
        sink(type="output")
        sink(type="message")
        if ('ksink1' %in% showConnections()) {
            thiscon <- rownames(showConnections())[which('ksink1'==showConnections()[,1])]
            close(getConnection(thiscon))
        }
        if ('ksink2' %in% showConnections()) {
            thiscon <- rownames(showConnections())[which('ksink2'==showConnections()[,1])]
            close(getConnection(thiscon))
        }
        if (isTRUE(catchOutput)) {
            tkselect(nb2, 1)
            riteMsg(output = output, errorout = err_out, paste(ksink1,collapse="\n"), error=TRUE)
            riteMsg(output = output, errorout = err_out, paste(ksink2,collapse="\n"), error=TRUE)
            tkfocus(txt_edit)
        } else {
            riteMsg(output = output, errorout = err_out, paste(ksink1,collapse="\n"))
            riteMsg(output = output, errorout = err_out, paste(ksink2,collapse="\n"))
        }
        if (isTRUE(catchOutput)) {
            sink(errsink, type="message")
        }
        if (inherits(knit_out,"try-error")) {
            riteMsg(output = output, errorout = err_out, "Report generation failed!", error=TRUE)
            return(knit_out)
        } else {
            riteMsg(output = output, errorout = err_out, 'Report finished!\n', error=TRUE)
            if (isTRUE(catchOutput)) {
                clearOutput()
            } 
            if (use %in% c('current','file') || genmode %in% c("stitch.rnw","stitch.rhtml","stitch.rmd","sweave")) {
                if (catchOutput) {
                    chn <- tclopen(knit_out, "r")
                    riteMsg(output = output, errorout = err_out, tclvalue(tclread(chn)))
                    tclclose(chn)
                } else if (use=='current' | (use=='file' & as.numeric(tclvalue(openreports))))
                    file.show(knit_out, title='Output')
            } else {
                if (isTRUE(catchOutput)) {
                    riteMsg(output = output, errorout = err_out, knit_out)
                } else {
                    tmp <- tempfile()
                    writeLines(knit_out, tmp)
                    file.show(tmp, title='Output')
                }
            }
            if (as.numeric(tclvalue(openreports))) {
                if (use=='file' & genmode %in% c('md2html','md2html.fragment')) {
                    browseURL(outfile)
                } else if (genmode %in% c("rmd2html","stitch.rhtml","stitch.rmd")) {
                    browseURL(knit_out)
                } else if (genmode == "stitch.rnw") {
                    browseURL(knit_out_pdf)
                }
            }
            if (catchOutput) {
                tkselect(nb2, 0)
                tkfocus(txt_edit)
            }
            return(knit_out)
        }
    }
    pdffromfile <- function(filetopdf=NULL, texttopdf=FALSE, textype="texi2pdf", bibtex=TRUE) {
        if (isTRUE(texttopdf)) {
            if (!isTRUE(scriptSaved)) {
                saveScript()
            }
            if (filename=="") {
                invisible()
            } else {
                filetopdf <- filename
            }
        } else if (is.null(filetopdf)) {
            filetopdf <- tclvalue(tkgetOpenFile(title="Open File",
                                                filetypes=filetypelist))
        }
        if (!filetopdf=="") {
            if (dirname(filetopdf) %in% c(".",getwd())) {
            } else {
                file.copy(filetopdf,paste(getwd(),basename(filetopdf),sep="/"), overwrite=TRUE)
                filetopdf <- paste(getwd(),basename(filetopdf),sep="/")
            }
            fstem <- substring(basename(filetopdf),1,regexpr("\\.[[:alnum:]]+$",basename(filetopdf))-1)
            fstem <- paste(fstem,".pdf",sep="")
            if (catchOutput) {
                tkmark.set(err_out, "insert", "end")
                tkselect(nb2, 1)
                tkfocus(txt_edit)
            }
            if (textype %in% c('latex','xelatex')) {
                if (textype=="latex") {
                    tex1 <- system(paste("pdflatex",filetopdf), intern=TRUE)
                } else {
                    tex1 <- system(paste("xelatex",filetopdf), intern=TRUE)
                }
                riteMsg(output = output, errorout = err_out, paste(tex1,collapse="\n"), error=TRUE)
                if (is.null(attributes(tex1)$status) && bibtex==TRUE) {
                    tex2 <- system(paste("bibtex",filetopdf), intern=TRUE)
                    riteMsg(output = output, errorout = err_out, paste(tex2,collapse="\n"), error=TRUE)
                    if (is.null(attributes(tex2)$status)) {
                        tex3 <- system(paste("pdflatex",filetopdf), intern=TRUE)
                        riteMsg(output = output, errorout = err_out, paste(tex3,collapse="\n"), error=TRUE)
                        if (is.null(attributes(tex3)$status)) {
                            tex4 <- system(paste("pdflatex",filetopdf), intern=TRUE)
                            riteMsg(output = output, errorout = err_out, paste(tex4,collapse="\n"), error=TRUE)
                        }
                    }
                }
            } else if (textype=="texi2pdf") {
                tex1 <- texi2pdf(filetopdf, clean=TRUE)
            }
            if (fstem %in% list.files()) {
                if (as.numeric(tclvalue(openreports))) {
                    riteMsg(output = output, errorout = err_out, paste("\nOpening pdf ",fstem,"...\n",sep=''), error=TRUE)
                    system2(getOption("pdfviewer"),fstem)
                }
            } else {
                riteMsg(output = output, errorout = err_out, "PDF not created!\n", error=TRUE)
            }
        }
    }
    
    ## HELP MENU FUNCTIONS ##
    addHighlighting <- function() {
        addHighlight <- function() {
            if (!tclvalue(objectval) == "") {
                hl('class', 'functions', hcolors$functions,
                    paste(" [list ",tclvalue(objectval)," ]",sep=""))
            }
            if (!tclvalue(envirval)=="" && paste("package:",tclvalue(envirval),sep="") %in% search()) {
                packs <- c( tclvalue(envirval),
                            gsub(" ","",strsplit(packageDescription(tclvalue(envirval),
                                                                    fields="Depends"),",")[[1]]))
                packs <- na.omit(packs)
                for (i in seq_along(packs)) {
                    funs <- try(paste(unique(gsub("<-","",
                                      objects(paste("package:",tclvalue(envirval),sep="")))),collapse=" "),
                                silent=TRUE)
                    if (!inherits(funs,"try-error")) {
                        hl('class', 'functions', hcolors$functions,
                            paste(" [list ",funs," ]",sep=""))
                    }
                }
            }
        }
        highlightbox <- tktoplevel()
        tkwm.title(highlightbox, paste("Add Highlighting Class",sep=""))
        r <- 1
        tkgrid.columnconfigure(highlightbox,1,weight=3)
        tkgrid.columnconfigure(highlightbox,2,weight=10)
        tkgrid.columnconfigure(highlightbox,3,weight=3)
        r <- r + 1
        entryform <- tkframe(highlightbox, relief="groove", borderwidth=2)
            # entry fields
            objectval <- tclVar("")
            envirval <- tclVar("")
            obj.entry <- tkentry(entryform, width = 40, textvariable=objectval)
            env.entry <- tkentry(entryform, width = 40, textvariable=envirval)
            # grid
            tkgrid(tklabel(entryform, text = "        "), row=1, column=1)
            tkgrid(tklabel(entryform, text = "Space-separated object(s):   "), row=2, column=1)
            tkgrid(obj.entry, row=2, column=2)
            tkgrid.configure(obj.entry, sticky="ew")
            tkgrid(tklabel(entryform, text = "Attached package (and dependencies):"), row=3, column=1)
            tkgrid(env.entry, row=3, column=2)
            tkgrid.configure(env.entry, sticky="ew")
            tkbind(obj.entry,"<Return>",addHighlight)
            tkgrid(tklabel(entryform, text = "        "), row=4, column=2)
            tkgrid.columnconfigure(entryform,2,weight=10)
            tkgrid.columnconfigure(entryform,3,weight=1)
        tkgrid(entryform, row=r, column=1, columnspan=3)
        tkgrid.configure(entryform, sticky="nsew")
        r <- r + 1
        tkgrid(ttklabel(highlightbox, text= "     "), row=r, column=2)
        r <- r + 1
        buttons <- tkframe(highlightbox)
            tkgrid(tkbutton(buttons, text = "  Add  ", command = addHighlight), row=1, column=1)
            tkgrid(tkbutton(buttons, text = " Close ", command = function() {
                tkdestroy(highlightbox); tkfocus(txt_edit)}), row=1, column=2)
        tkgrid(buttons, row=r, column=2)
        r <- r + 1
        tkgrid(ttklabel(highlightbox, text= "     "), row=r, column=2)
        tkfocus(obj.entry)
    }
    about <- function() {
        aboutbox <- tktoplevel()
        tkwm.title(aboutbox, riteenv$wmtitle)
        tkgrid(ttklabel(aboutbox, text= "     "), row=1, column=1)
        tkgrid(ttklabel(aboutbox, text= "     "), row=1, column=3)
        tkgrid(ttklabel(aboutbox, text = paste("(C) Thomas J. Leeper ",
                        max("2013",format(Sys.Date(),"%Y")),", released under GPL 2",sep="")), row=2, column=2)
        tkgrid(ttklabel(aboutbox, text= "     "), row=3, column=2)
        tkgrid(ttklabel(aboutbox, text= "Special thanks to Yihui Xie for helpful input and assistance!"), row=6, column=2)
        tkgrid(ttklabel(aboutbox, text= "     "), row=7, column=2)
        tkgrid(website <- ttklabel(aboutbox, text = "For more information, visit: http://www.thomasleeper.com/software.html",
                                    foreground="blue"), row=8, column=2)
        tkgrid(ttklabel(aboutbox, text= "     "), row=9, column=2)
        tkgrid(tkbutton(aboutbox, text = "   OK   ", command = function() {
            tkdestroy(aboutbox); tkfocus(txt_edit)}), row=10, column=2)
        tkgrid(ttklabel(aboutbox, text= "     "), row=11, column=2)
        tkbind(website, "<ButtonPress>", function()
            browseURL("http://www.thomasleeper.com/software.html"))
        tkfocus(aboutbox)
    }
    
    ## EDITOR LAYOUT ##
    riteenv$editor <- tktoplevel(borderwidth=0)
    tkwm.title(riteenv$editor, riteenv$wmtitle)    # title
    tkwm.protocol(riteenv$editor, "WM_DELETE_WINDOW", exitWiz) # regulate exit

    ## EDITOR MENUS ##
    menuTop <- tkmenu(riteenv$editor)           # Create a menu
    tkconfigure(riteenv$editor, menu = menuTop) # Add it to the 'riteenv$editor' window
    menuFile <- tkmenu(menuTop, tearoff = FALSE)
        tkadd(menuFile, "command", label="New Script", command=newScript, underline = 0)
        tkadd(menuFile, "command", label="Load Script",
            command=function() loadScript(locals=TRUE), underline = 0)
        if (!Sys.info()['sysname'] == "Darwin") {
            tkadd(menuFile, "command", label="Load Command History",
                command=loadHistory)
        }
        tkadd(menuFile, "command", label="Save Script", command=saveScript, underline = 0)
        tkadd(menuFile, "command", label="SaveAs Script", command=saveAsScript, underline = 1)
        tkadd(menuFile, "command", label="Append Script",
            command=function() loadScript(locals=TRUE, new=FALSE), underline = 1)
        tkadd(menuFile, "command", label="Insert Script Reference",
            command=function() loadScript(locals=TRUE, refonly=TRUE, new=FALSE), underline = 0)
        tkadd(menuFile, "separator")
        menuTemplates <- tkmenu(menuFile, tearoff = FALSE)
            tkadd(menuTemplates, "command", label="knitr LaTeX (.Rnw)", 
                  command = function() loadScript(system.file("templates/knitr-minimal.Rnw", package = "rite")))
            tkadd(menuTemplates, "command", label="knitr markdown (.Rmd)", 
                  command = function() loadScript(system.file("templates/knitr-minimal.Rmd", package = "rite")))
            tkadd(menuTemplates, "command", label="knitr HTML (.Rnw)", 
                  command = function() loadScript(system.file("templates/knitr-minimal.Rhtml", package = "rite")))
            tkadd(menuTemplates, "command", label="Sweave (.Rnw)", 
                  command = function() loadScript(system.file("templates/Sweave.Rnw", package = "rite")))
            tkadd(menuTemplates, "command", label="LaTeX (.tex)", 
                  command = function() loadScript(system.file("templates/LaTeX.Rnw", package = "rite")))
        tkadd(menuFile, "cascade", label = "Load file templates", menu = menuTemplates)
        menuFileWeb <- tkmenu(menuFile, tearoff = FALSE)
            tkadd(menuFileWeb, "command", label="Load Remote Script",
                command=function() loadScript(locals=FALSE), underline = 0)
            tkadd(menuFileWeb, "command", label="Append Remote Script",
                command=function() loadScript(locals=FALSE, refonly=FALSE, new=FALSE), underline = 1)
            tkadd(menuFileWeb, "command", label="Insert Remote Script Reference",
                command=function() loadScript(locals=FALSE,refonly=TRUE,new=FALSE), underline = 0)
            tkadd(menuFileWeb, "separator")
            tkadd(menuFileWeb, "command", label="Load Script from Gist",
                command=function() loadScript(locals=FALSE,gist=TRUE))
            tkadd(menuFileWeb, "command", label="Append Script from Gist",
                command=function() loadScript(locals=FALSE,gist=TRUE,new=FALSE))
            tkadd(menuFileWeb, "command", label="Insert Gist Reference",
                command=function() loadScript(locals=FALSE,gist=TRUE,refonly=TRUE,new=FALSE))
            tkadd(menuFileWeb, "separator")
            tkadd(menuFileWeb, "command", label="Save Script as Gist",
                command=function() saveGist(), underline = 0)
            tkadd(menuFileWeb, "command", label="Save Script as Gist and Open",
                command=function() saveGist(browse=TRUE))
            tkadd(menuFileWeb, "separator")
            tkadd(menuFileWeb, "command", label="Upload HTML as new RPubs",
                command=function() uploadToRPubs(new=TRUE, render='html'))
            tkadd(menuFileWeb, "command", label="Upload HTML to update RPubs",
                command=function() uploadToRPubs(new=FALSE, render='html'))
            tkadd(menuFileWeb, "command", label="Render md & upload as new RPubs",
                command=function() uploadToRPubs(new=TRUE, render='md2html'))
            tkadd(menuFileWeb, "command", label="Render md & update RPubs",
                command=function() uploadToRPubs(new=FALSE, render='md2html'))
            tkadd(menuFileWeb, "command", label="knit Rmd & upload as new RPubs",
                command=function() uploadToRPubs(new=TRUE, render='rmd2html'))
            tkadd(menuFileWeb, "command", label="knit Rmd & update RPubs",
                command=function() uploadToRPubs(new=FALSE, render='rmd2html'))
            tkadd(menuFile, "cascade", label = "Remote scripts...", menu = menuFileWeb, underline = 0)
        tkadd(menuFile, "separator")
        tkadd(menuFile, "command", label="Change dir...", command=function(...) {
            tkdir <- tclvalue(tkchooseDirectory())
            if (!tkdir=="")
                setwd(tkdir)
            }, underline = 7)
        tkadd(menuFile, "separator")
        if (!isTRUE(catchOutput)) {
            tkadd(menuFile, "command", label = "Open another rite", command = function() { 
                do.call("rite", as.list(match.call(expand.dots=FALSE))[-1]) 
            }, underline = 0)
        }
        tkadd(menuFile, "command", label = "Close rite", command = exitWiz, underline = 0)
        tkadd(menuFile, "separator")
        tkadd(menuFile, "command", label = "Quit R", command = function() {exitWiz(); quit()}, underline = 0)
        tkadd(menuTop, "cascade", label = "File", menu = menuFile, underline = 0)
    menuRun <- tkmenu(menuTop, tearoff = FALSE)
        tkadd(menuRun, "command", label = "Run Line", command = runLine, underline = 4)
        tkadd(menuRun, "command", label = "Run Selection", command = runSelection, underline = 4)
        tkadd(menuRun, "command", label = "Run All", command = runAll, underline = 4)
        tkadd(menuRun, "command", label = "Run Code Chunks (All)", command = runAllChunks, underline = 4)
        tkadd(menuRun, "separator")
        tkadd(menuRun, "command", label = "Tidy R Script", command = tidyScript)
        tkadd(menuRun, "separator")
        tkadd(menuRun, 'checkbutton', label='Echo code?', onvalue=1L, variable=echorun)
        tkadd(menuRun, 'checkbutton', label="Print Prompt '>' and Continue '+'", onvalue=1L, variable=echoprompt)
        tkadd(menuRun, 'checkbutton', label='Wait on warnings?', onvalue=1L, variable=everWarn)
        if (!Sys.info()['sysname'] == "Darwin")
            tkadd(menuRun, 'checkbutton', label='Add commands to history?', onvalue=1L, variable=addtohistory)
        tkadd(menuRun, "separator")
        if (isTRUE(catchOutput)) {
            tkadd(menuRun, "command", label="List all objects", command=function()
                tkinsert(output,"end",capture.output(ls(envir=evalenv))))
            tkadd(menuRun, "command", label="List search path", command=function()
                tkinsert(output,"end",capture.output(search())))
        } else {
            tkadd(menuRun, "command", label="List all objects", command=function()
                print(ls(envir=evalenv)))
            tkadd(menuRun, "command", label="List search path", command=function()
                print(search()))
        }
        tkadd(menuRun, "command", label="Remove all objects", command=function() {
            check <- tkmessageBox(message = "Are you sure?", icon = "question", type = "yesno", default = "no")
            if (tclvalue(check)=="yes") {
                rm(list=ls(all.names=TRUE,envir=evalenv),envir=evalenv)
                tkmessageBox(message="All objects removed")
            }    })
        tkadd(menuRun, "separator")
        tkadd(menuRun, "command", label="Install package(s)", command=function()
            install.packages())
        tkadd(menuRun, "command", label="Update package(s)", command=function()
            update.packages(ask='graphics',checkBuilt=TRUE))
        #tkadd(menuRun, "separator")
        #tkadd(menuRun, "command", label = "Interrupt", command = function() {pskill(Sys.getpid(),SIGINT) }, underline = 0)
        #tkadd(menuRun, "command", label = "Interrupt", command = function() tkdestroy(txt_edit), underline = 0)
        tkadd(menuTop, "cascade", label = "Run", menu = menuRun, underline = 0)
    if (isTRUE(catchOutput)) {
        menuOutput <- tkmenu(menuTop, tearoff = FALSE)
            copyOutput <- function() {
                tkclipboard.clear()
                tkclipboard.append(tclvalue(tkget(output, "0.0", "end")))
            }
            tkadd(menuOutput, "command", label = "Copy Output", command = copyOutput, underline = 0)
            tkadd(menuOutput, "command", label = "Save Output",
                command = function() saveOutput(outfilename=""), underline = 0)
            tkadd(menuOutput, "command", label = "Clear Output", command = clearOutput, underline = 1)
            tkadd(menuOutput, "separator")
            copyMessage <- function() {
                tkclipboard.clear()
                tkclipboard.append(tclvalue(tkget(err_out, "0.0", "end")))
            }
            tkadd(menuOutput, "command", label = "Copy Message", command = copyMessage, underline = 0)
            tkadd(menuOutput, "command", label = "Clear Message", command = clearError, underline = 1)
            tkadd(menuTop, "cascade", label = "Output", menu = menuOutput, underline = 0)
    }
    menuReport <- tkmenu(menuTop, tearoff = FALSE)
        menuKnit <- tkmenu(menuReport, tearoff = FALSE)
            tkadd(menuKnit, "command", label = "knit",
                command = function() knittxt(genmode="knit", use='text'), underline = 0)
            tkadd(menuKnit, "command", label = "Sweave",
                command = function() knittxt(genmode="sweave", use='text'))
            tkadd(menuKnit, "command", label = "knit (from Sweave source)",
                command = function() knittxt(genmode="knitsweave", use='text'))
            tkadd(menuKnit, "separator")
            tkadd(menuKnit, "command", label = "knit Rmd to HTML",
                command = function() knittxt(genmode="rmd2html", use='current'))
            #tkadd(menuKnit, "command", label = "knit and Slidify",
            #    command = function() knittxt(genmode="knit2slidify", usefile=FALSE, usetxt=TRUE))
            tkadd(menuKnit, "separator")
            tkadd(menuKnit, "command", label = "knit to pdf",
                command = function() {
                    k <- knittxt(genmode="knit", use='current')
                    pdffromfile(filetopdf=paste(file_path_sans_ext(k),"tex",sep="."))
                })
            tkadd(menuKnit, "command", label = "Sweave to pdf",
                command = function() {
                    k <- knittxt(genmode="sweave", use='text')
                    pdffromfile(filetopdf=k)
                })
            tkadd(menuKnit, "command", label = "knit to pdf (from Sweave source)",
                command = function() {
                    k <- knittxt(genmode="knitsweave", use='current')
                    pdffromfile(filetopdf=paste(file_path_sans_ext(k),"tex",sep="."))
                })
            tkadd(menuReport, "cascade", label = "Knit", menu = menuKnit, underline = 0)
        menuPurl <- tkmenu(menuReport, tearoff = FALSE)
            tkadd(menuPurl, "command", label = "purl",
                command = function() knittxt(genmode="purl", use='text'), underline = 0)
            tkadd(menuPurl, "command", label = "purl (from Sweave source)",
                command = function() knittxt(genmode="tangle", use='text'))
            tkadd(menuReport, "cascade", label = "Purl", menu = menuPurl, underline = 0)
        menuStitch <- tkmenu(menuReport, tearoff = FALSE)
            tkadd(menuStitch, "command", label = "stitch (tex)",
                command = function() knittxt(genmode="stitch.rnw", use='current'), underline = 0)
            tkadd(menuStitch, "command", label = "stitch (HTML)",
                command = function() knittxt(genmode="stitch.rhtml", use='current'))
            tkadd(menuStitch, "command", label = "stitch (markdown)",
                command = function() knittxt(genmode="stitch.rmd", use='current'))
            tkadd(menuReport, "cascade", label = "Stitch", menu = menuStitch, underline = 0)
        menuSpin <- tkmenu(menuReport, tearoff = FALSE)
            tkadd(menuSpin, "command", label = "spin to Rmd",
                command = function() knittxt(genmode="spin", use='text', spinformat="Rmd"))
            tkadd(menuSpin, "command", label = "spin to Rnw",
                command = function() knittxt(genmode="spin", use='text', spinformat="Rnw"))
            tkadd(menuSpin, "command", label = "spin to Rhtml",
                command = function() knittxt(genmode="spin", use='text', spinformat="Rhtml"))
            tkadd(menuSpin, "command", label = "spin to Rtex",
                command = function() knittxt(genmode="spin", use='text', spinformat="Rtex"))
            tkadd(menuSpin, "command", label = "spin to Rrst",
                command = function() knittxt(genmode="spin", use='text', spinformat="Rrst"))
            tkadd(menuSpin, "separator")
            tkadd(menuSpin, "command", label = "spin to Rmd and knit",
                command = function() knittxt(genmode="spinknit", use='text', spinformat="Rmd"))
            tkadd(menuSpin, "command", label = "spin to Rnw and knit",
                command = function() knittxt(genmode="spinknit", use='text', spinformat="Rnw"), state="disabled")
            tkadd(menuSpin, "command", label = "spin to Rhtml and knit",
                command = function() knittxt(genmode="spinknit", use='text', spinformat="Rhtml"))
            tkadd(menuSpin, "command", label = "spin to Rtex and knit",
                command = function() knittxt(genmode="spinknit", use='text', spinformat="Rtex"), state="disabled")
            tkadd(menuSpin, "command", label = "spin to Rrst and knit",
                command = function() knittxt(genmode="spinknit", use='text', spinformat="Rrst"))
            tkadd(menuReport, "cascade", label = "Spin", menu = menuSpin)
        tkadd(menuReport, "separator")
        #menuSlidify <- tkmenu(menuReport, tearoff = FALSE)
            #tkadd(menuSlidify, "command", label = "Slidify",
            #    command = function() knittxt(genmode="slidify", usefile=FALSE, usetxt=TRUE))
            #tkadd(menuSlidify, "command", label = "knit and Slidify",
            #    command = function() knittxt(genmode="knit2slidify", usefile=FALSE, usetxt=TRUE))
            #tkadd(menuSlidify, "command", label = "Open Slidify template",
            #    command = function() {newScript(); ??? }) # DO THIS
        #tkadd(menuReport, "separator")
        menuMD <- tkmenu(menuReport, tearoff = FALSE)
            tkadd(menuMD, "command", label = "Convert md to HTML",
                command = function() knittxt(genmode="md2html", use='text'))
            tkadd(menuMD, "command", label = "Convert md to HTML fragment",
                command = function() knittxt(genmode="md2html.fragment", use='text'))
            tkadd(menuMD, "command", label = "knit Rmd to HTML",
                command = function() knittxt(genmode="rmd2html", use='current'))
            # tkadd(menuMD, "separator")
            #tkadd(menuMD, "command", label = "Slidify",
            #    command = function() knittxt(genmode="slidify", use='text'))
            #tkadd(menuMD, "command", label = "knit and Slidify",
            #    command = function() knittxt(genmode="knit2slidify", use='text'))
            tkadd(menuReport, "cascade", label = "Markdown", menu = menuMD, underline = 0)
        tkadd(menuReport, "separator")
        #texstatus <- ifelse(!system("pdflatex -version",show.output.on.console=FALSE), "enabled","disabled")
        menuTex <- tkmenu(menuReport, tearoff = FALSE)
            tkadd(menuTex, "command", label = "Compile LaTeX PDF",
                command = function() pdffromfile(texttopdf=TRUE))
            tkadd(menuTex, "separator")
            tkadd(menuTex, "command", label = "pdflatex",
                command = function() pdffromfile(texttopdf=TRUE, textype='latex', bibtex=FALSE))
            tkadd(menuTex, "command", label = "pdflatex+bibtex",
                command = function() pdffromfile(texttopdf=TRUE, textype='latex', bibtex=TRUE))
            tkadd(menuTex, "separator")
            tkadd(menuTex, "command", label = "xelatex",
                command = function() pdffromfile(texttopdf=TRUE, textype="xelatex", bibtex=FALSE))
            tkadd(menuTex, "command", label = "xelatex+bibtex",
                command = function() pdffromfile(texttopdf=TRUE, textype="xelatex", bibtex=TRUE))
            tkadd(menuReport, "cascade", label = "TeX", menu = menuTex, underline = 0)
        tkadd(menuReport, "separator")
        menuFromFile <- tkmenu(menuReport, tearoff = FALSE)
            tkadd(menuFromFile, "command", label = "knit",
                command = function() knittxt(genmode="knit", use='file'), underline = 0)
            tkadd(menuFromFile, "command", label = "purl",
                command = function() knittxt(genmode="purl", use='file'), underline = 0)
            tkadd(menuFromFile, "command", label = "Sweave",
                command = function() knittxt(genmode="sweave", use='file'), underline = 0)
            tkadd(menuFromFile, "separator")
            tkadd(menuFromFile, "command", label = "knit to pdf",
                command = function() {
                    k <- knittxt(genmode="knit", use='file')
                    pdffromfile(filetopdf=paste(file_path_sans_ext(k),"tex",sep="."))
                })
            tkadd(menuFromFile, "command", label = "Sweave to pdf",
                command = function() {
                    k <- knittxt(genmode="sweave", use='file')
                    pdffromfile(filetopdf=k)
                })
            tkadd(menuFromFile, "command", label = "knit to pdf (from Sweave source)",
                command = function() {
                    k <- knittxt(genmode="knitsweave", use='file')
                    pdffromfile(filetopdf=paste(file_path_sans_ext(k),"tex",sep="."))
                })
            tkadd(menuFromFile, "command", label = "knit Rmd to HTML",
                command = function() knittxt(genmode="rmd2html", use='file'))
            tkadd(menuFromFile, "separator")
            tkadd(menuFromFile, "command", label = "stitch (tex)",
                command = function() knittxt(genmode="stitch.rnw", use='file'))
            tkadd(menuFromFile, "command", label = "stitch (HTML)",
                command = function() knittxt(genmode="stitch.rhtml", use='file'))
            tkadd(menuFromFile, "command", label = "stitch (markdown)",
                command = function() knittxt(genmode="stitch.rmd", use='file'))
            tkadd(menuFromFile, "separator")
            tkadd(menuFromFile, "command", label = "spin to Rmd",
                command = function() knittxt(genmode="spin", use='file', spinformat="Rmd"))
            tkadd(menuFromFile, "command", label = "spin to Rnw",
                command = function() knittxt(genmode="spin", use='file', spinformat="Rnw"))
            tkadd(menuFromFile, "command", label = "spin to Rhtml",
                command = function() knittxt(genmode="spin", use='file', spinformat="Rhtml"))
            tkadd(menuFromFile, "command", label = "spin to Rtex",
                command = function() knittxt(genmode="spin", use='file', spinformat="Rtex"))
            tkadd(menuFromFile, "command", label = "spin to Rrst",
                command = function() knittxt(genmode="spin", use='file', spinformat="Rrst"))
            tkadd(menuFromFile, "separator")
            tkadd(menuFromFile, "command", label = "Convert md to HTML",
                command = function() knittxt(genmode="md2html", use='file'))
            tkadd(menuFromFile, "command", label = "Convert md to HTML fragment",
                command = function() knittxt(genmode="md2html.fragment", use='file'))
            # update when slidify is on CRAN
            #tkadd(menuMD, "command", label = "slidify md to HTML",
            #    command = function() knittxt(genmode="slidify", use='file'))
            #tkadd(menuMD, "command", label = "knit Rmd and slidify to HTML",
            #    command = function() knittxt(genmode="knit2slidify", use='file'))
            tkadd(menuFromFile, "separator")
            tkadd(menuFromFile, "command", label = "Compile LaTeX PDF",
                command = function() pdffromfile(texttopdf=TRUE))
            tkadd(menuFromFile, "command", label = "pdflatex",
                command = function() pdffromfile(texttopdf=FALSE, textype='latex', bibtex=FALSE))
            tkadd(menuFromFile, "command", label = "pdflatex+bibtex",
                command = function() pdffromfile(texttopdf=FALSE, textype='latex', bibtex=TRUE))
            tkadd(menuFromFile, "command", label = "xelatex",
                command = function() pdffromfile(texttopdf=FALSE, textype="xelatex", bibtex=FALSE))
            tkadd(menuFromFile, "command", label = "xelatex+bibtex",
                command = function() pdffromfile(texttopdf=FALSE, textype="xelatex", bibtex=TRUE))
            tkadd(menuReport, "cascade", label = "Generate from file...", menu = menuFromFile, underline = 0)
        tkadd(menuReport, "separator")
        openreports <- tclVar(1)
        tkadd(menuReport, 'checkbutton', label='Open finished reports', onvalue=1L, variable=openreports)
        tkadd(menuTop, "cascade", label = "Report Generation", menu = menuReport, underline = 0)
    menuHelp <- tkmenu(menuTop, tearoff = FALSE)
        tkadd(menuHelp, "command", label = "Add Package Highlighting", command = addHighlighting, underline = 0)
        #tkadd(menuHelp, "separator")
        #tkadd(menuHelp, "command", label = "R language help", underline = 0, command = help.start)
        tkadd(menuHelp, "separator")
        tkadd(menuHelp, "command", label = "rite Documentation", command = function() help('rite','rite'))
        tkadd(menuHelp, "command", label = "About rite Script Editor", command = about, underline = 0)
        tkadd(menuTop, "cascade", label = "Help", menu = menuHelp, underline = 0)

    pw <- ttkpanedwindow(riteenv$editor, orient = orientation)
    nb1 <- tk2notebook(pw, tabs = c("Script")) # left pane
        if (!is.ttk()) {
            stop("Tcl/Tk >= 8.5 is required")
        }
        tclRequire("ctext")
        add_texttab <- function() {
            edittab <- tk2notetab(nb1, "Script")
            editscr <- tkscrollbar(edittab, repeatinterval = 25, command = function(...) { tkyview(txtedit,...) })
            txtedit <- tkwidget(edittab, "ctext", bg = hcolors$background, fg = hcolors$normal, undo = "true",
                                 yscrollcommand = function(...) tkset(editscr,...),
                                 font = tkfont.create(family = fontFamily, size = fontSize))
            # check for bracket completion
            tktag.configure(txtedit, 'tmpbracketclose', foreground = 'red',
                font = tkfont.create(family = fontFamily, size = fontSize, weight = 'bold'))
            
            tkbind(txtedit, "<Right>", function() editkeypress(txtedit, 'right'))
            tkbind(txtedit, "<Left>", function() editkeypress(txtedit, 'left'))
            tkbind(txtedit, "<Key>", function() tktag.remove(txtedit,'tmpbracketclose', '1.0', 'end'))
            
            tkbind(txtedit, "<<Modified>>", function() editModified(txtedit))
            
            tkgrid(txtedit, sticky="nsew", column=1, row=1)
            tkgrid(editscr, sticky="nsew", column=2, row=1)
            tkgrid.columnconfigure(edittab,1,weight=1)
            tkgrid.columnconfigure(edittab,2,weight=0)
            tkgrid.rowconfigure(edittab,1,weight=1)
            
            return(txtedit)
        }
        # script editor
        txt_edit <- add_texttab()
    
    # pack left notebook
    tkadd(pw, nb1, weight=1) # left pane
    
    if (isTRUE(catchOutput)) {
        nb2 <- tk2notebook(pw, tabs = c("Output", "Message"))#, "Plot")) # right pane
            # output
            out_tab1 <- tk2notetab(nb2, "Output")
            out_scr <- tkscrollbar(out_tab1, repeatinterval=25, command=function(...)tkyview(output,...))
            output <- tktext(out_tab1, height=25, bg="white", font="courier", yscrollcommand=function(...)tkset(out_scr,...),
                                        font=tkfont.create(family=fontFamily,size=fontSize))
            outModified <- function() {
                outputSaved <<- FALSE
            }
            tkbind(output, "<<Modified>>", outModified)
            tkgrid(output, column=1, row=1, sticky="nsew")
            tkgrid(out_scr, column=2, row=1, sticky="nsew")
            tkgrid.columnconfigure(out_tab1,1,weight=1)
            tkgrid.columnconfigure(out_tab1,2,weight=0)
            tkgrid.rowconfigure(out_tab1,1,weight=1)
            
            # message
            out_tab2 <- tk2notetab(nb2, "Message")
            err_scr <- tkscrollbar(out_tab2, repeatinterval=25, command=function(...)tkyview(err_out,...))
            err_out <- tktext(out_tab2, height=25, bg="gray90", font="courier", yscrollcommand=function(...)tkset(err_scr,...),
                                        font=tkfont.create(family=fontFamily,size=fontSize))
            errModified <- function() {}
            tkbind(err_out, "<<Modified>>", errModified)
            tkgrid(err_out, column=1, row=1, sticky="nsew")
            tkgrid(err_scr, column=2, row=1, sticky="nsew")
            tkgrid.columnconfigure(out_tab2,1,weight=1)
            tkgrid.columnconfigure(out_tab2,2,weight=0)
            tkgrid.rowconfigure(out_tab2,1,weight=1)

            tktag.configure(err_out, "text", foreground="black", underline=0)
            tktag.configure(err_out, "error", foreground="red", underline=0)
            tktag.configure(err_out, "warning", foreground="purple", underline=0)
            tktag.configure(err_out, "message", foreground="blue", underline=0)
            
            # bind option('width') to window resize
            resize <- function() {
                w <- tkwinfo('width',output)
                m <- tkfont.measure(tkfont.create(family=fontFamily,size=fontSize),'m')
                nw <- round((as.numeric(w)-20)/as.numeric(m))
                options(width=nw)
            }
            tkbind(output, '<Configure>', resize)
        
        # pack right notebook
        tkadd(pw, nb2, weight=1) # right pane
    }
    tkpack(pw, fill="both", expand = "yes") # pack panedwindow to editor
    
    ## KEY BINDINGS ##
    f1 <- function() {
        iwordstart <- tclvalue(tkindex(txt_edit,"insert-1char wordstart"))
        iwordend <- tclvalue(tkindex(txt_edit,"insert-1char wordend"))
        sel <- tclvalue(tktag.ranges(txt_edit,"sel"))
        if (!sel == "") {
            command <- tclvalue(tkget(txt_edit,"sel.first","sel.last"))
        } else {
            # periods
            if (tclvalue(tkget(txt_edit, iwordstart, iwordend))=='.') {
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
                iwordend <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordend')))
            }
            # preceding periods
            if (tclvalue(tkget(txt_edit, paste(iwordstart,'-1char'), iwordstart))=='.') {
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
            }
            if (tclvalue(tkget(txt_edit, paste(iwordstart,'-1char'), iwordstart))=='.') {
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
            }
            if (tclvalue(tkget(txt_edit, paste(iwordstart,'-1char'), iwordstart))=='.') {
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
            }
            # following periods
            if (tclvalue(tkget(txt_edit, iwordend, paste(iwordend,'+1char')))=='.') {
                iwordend <- tclvalue(tkindex(txt_edit, paste(iwordend,'+2char wordend')))
            }
            if (tclvalue(tkget(txt_edit, iwordend, paste(iwordend,'+1char')))=='.') {
                iwordend <- tclvalue(tkindex(txt_edit, paste(iwordend,'+2char wordend')))
            }
            if (tclvalue(tkget(txt_edit, iwordend, paste(iwordend,'+1char')))=='.') {
                iwordend <- tclvalue(tkindex(txt_edit, paste(iwordend,'+2char wordend')))
            }
            command <- tclvalue(tkget(txt_edit, iwordstart, iwordend))
        }
        if (command %in% c("","\n","\t"," ",")","]","}","=",".",",","%")) {
            return()
        } else if (command %in% c("[","(","*","/","+","-","^","$","{","~",
                                "function","if","else","for","in",
                                "repeat","while","break","next")) {
            command <- paste("`",command,"`",sep="")
        } else {
            command <- gsub(" ","",command)
        }
        helpresults <- help(command)
        if (length(helpresults) > 0) {
            print(helpresults)
        } else {
            print(help.search(command))
        }
        invisible(NULL)
    }
    tkbind(txt_edit, "<F1>", f1)
    
    commandCompletion <- function() {
        iwordstart <- tclvalue(tkindex(txt_edit,"insert-1char wordstart"))
        iwordend <- tclvalue(tkindex(txt_edit,"insert-1char wordend"))
        sel <- tclvalue(tktag.ranges(txt_edit,"sel"))
        if (!sel=="") {
            iwordstart <- strsplit(sel," ")[[1]][1]
            iwordend <- strsplit(sel," ")[[1]][2]
            command <- sel
        } else {
            if (tclvalue(tkget(txt_edit, iwordstart, iwordend))=="(") {
                # parentheses
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
            } else if (tclvalue(tkget(txt_edit, iwordstart, iwordend))=="$") {
                # dollar signs
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
            } else if (tclvalue(tkget(txt_edit, iwordstart, iwordend))=='.') {
                # periods
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
                iwordend <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordend')))
            }
            # preceding periods
            if (tclvalue(tkget(txt_edit, paste(iwordstart,'-1char'), iwordstart))=='.') {
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
            }
            if (tclvalue(tkget(txt_edit, paste(iwordstart,'-1char'), iwordstart))=='.') {
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
            }
            if (tclvalue(tkget(txt_edit, paste(iwordstart,'-1char'), iwordstart))=='.') {
                iwordstart <- tclvalue(tkindex(txt_edit, paste(iwordstart,'-2char wordstart')))
            }
            # following periods
            if (tclvalue(tkget(txt_edit, iwordend, paste(iwordend,'+1char')))=='.') {
                iwordend <- tclvalue(tkindex(txt_edit, paste(iwordend,'+2char wordend')))
            }
            if (tclvalue(tkget(txt_edit, iwordend, paste(iwordend,'+1char')))=='.') {
                iwordend <- tclvalue(tkindex(txt_edit, paste(iwordend,'+2char wordend')))
            }
            if (tclvalue(tkget(txt_edit, iwordend, paste(iwordend,'+1char')))=='.') {
                iwordend <- tclvalue(tkindex(txt_edit, paste(iwordend,'+2char wordend')))
            }
            command <- tclvalue(tkget(txt_edit, iwordstart, iwordend))
        }
        fnlist <- vector(mode="character")
        #if (command %in% c("\n","\t"," ","(",")","[","]","{","}","=",",","*","/","+","-","^","%","$","<",">"))
        #    return()
        if (tclvalue(tkget(txt_edit, "insert linestart", "insert")) %in%
                c("<<","```{r","<!--begin.rcode","..~{r","% begin.rcode")) {
            fnlist <- c("eval","echo","results","tidy","cache",
                        "fig.width","fig.height","out.width","out.height",
                        "include","child","engine")
            insertCommand <- function(x)
                tkinsert(txt_edit, "insert", paste(" ",fnlist[x],"=",sep=""))
            fnContextMenu <- tkmenu(txt_edit, tearoff = FALSE)
        } else if (substring(command,nchar(command),nchar(command))=="(") {
            fnlist <- try(names(formals(substring(command,1,nchar(command)-1))),silent=TRUE)
            if (!inherits(fnlist,"try-error")) {
                insertCommand <- function(x)
                    tkinsert(txt_edit, "insert", paste(fnlist[x],"=",sep=""))
            }
        } else if (substring(command,nchar(command),nchar(command))=="$") {
            fnlist <- try(eval(parse(text=paste("objects(",substring(command,1,nchar(command)-1),")",sep=""))),silent=TRUE)
            if (!inherits(fnlist,"try-error")) {
                insertCommand <- function(x)
                    tkinsert(txt_edit, "insert", fnlist[x])
            }
        } else if (substring(command,nchar(command),nchar(command)) %in% c("'",'"')) {
            fnlist <- try(list.files(),silent=TRUE)
            if (!inherits(fnlist,"try-error")) {
                insertCommand <- function(x)
                    tkinsert(txt_edit, "insert", fnlist[x])
            }
        } else {
            insertpos <- strsplit(tclvalue(tkindex(txt_edit,"insert")),".", fixed=TRUE)[[1]]
            fnlist <- apropos(paste("^", command,sep=""))
            if (length(fnlist<15)) {
                fnlist <- unique(c(fnlist, apropos(command)))
            }
            insertCommand <- function(x) {
                tkdelete(txt_edit, iwordstart, iwordend)
                tkinsert(txt_edit, "insert", fnlist[x])
            }
        }
        if (length(fnlist) > 0) {
            fnContextMenu <- tkmenu(txt_edit, tearoff = FALSE)
            # conditionally add menu items
            ## adding them programmatically failed to work (always added last command)
                if (length(fnlist)>0) {
                    tkadd(fnContextMenu, "command", label = fnlist[1], command = function() insertCommand(1))
                }
                if (length(fnlist)>1) {
                    tkadd(fnContextMenu, "command", label = fnlist[2], command = function() insertCommand(2))
                }
                if (length(fnlist)>2) {
                    tkadd(fnContextMenu, "command", label = fnlist[3], command = function() insertCommand(3))
                }
                if (length(fnlist)>3) {
                    tkadd(fnContextMenu, "command", label = fnlist[4], command = function() insertCommand(4))
                }
                if (length(fnlist)>4) {
                    tkadd(fnContextMenu, "command", label = fnlist[5], command = function() insertCommand(5))
                }
                if (length(fnlist)>5) {
                    tkadd(fnContextMenu, "command", label = fnlist[6], command = function() insertCommand(6))
                }
                if (length(fnlist)>6) {
                    tkadd(fnContextMenu, "command", label = fnlist[7], command = function() insertCommand(7))
                }
                if (length(fnlist)>7) {
                    tkadd(fnContextMenu, "command", label = fnlist[8], command = function() insertCommand(8))
                }
                if (length(fnlist)>8) {
                    tkadd(fnContextMenu, "command", label = fnlist[9], command = function() insertCommand(9))
                }
                if (length(fnlist)>9) {
                    tkadd(fnContextMenu, "command", label = fnlist[10], command = function() insertCommand(10))
                }
                if (length(fnlist)>10) {
                    tkadd(fnContextMenu, "command", label = fnlist[11], command = function() insertCommand(11))
                }
                if (length(fnlist)>11) {
                    tkadd(fnContextMenu, "command", label = fnlist[12], command = function() insertCommand(12))
                }
                if (length(fnlist)>12) {
                    tkadd(fnContextMenu, "command", label = fnlist[13], command = function() insertCommand(13))
                }
                if (length(fnlist)>13) {
                    tkadd(fnContextMenu, "command", label = fnlist[14], command = function() insertCommand(14))
                }
                if (length(fnlist)>14) {
                    tkadd(fnContextMenu, "command", label = fnlist[15], command = function() insertCommand(15))
                }
            # root x,y
            rootx <- as.integer(tkwinfo("rootx", txt_edit))
            rooty <- as.integer(tkwinfo("rooty", txt_edit))
            # line height
            font <- strsplit(tclvalue(tkfont.metrics(fontFamily))," -")[[1]]
            lheight <- as.numeric(strsplit(font[grepl("linespace",font)]," ")[[1]][2])
            nl <- floor(as.numeric(iwordstart))
            # font width
            wordnchar <- as.numeric(strsplit(as.character(as.numeric(iwordend) %% 1),".",fixed=TRUE)[[1]][2])
            fontwidth <- as.numeric(tkfont.measure("m", fontFamily))
            # @x,y position
            xTxt <- rootx + wordnchar
            yTxt <- rooty + lheight*nl
            tkpost(fnContextMenu, xTxt, yTxt)
            tkbind(fnContextMenu, "<Button-3>", function() tkunpost(fnContextMenu))
        }
    }
    #tkbind(txt_edit, "<Shift-Tab>", commandCompletion)
    tkbind(txt_edit, "<F2>", commandCompletion)
    
    findreplace <- function() {
        casevar <- tclVar(searchopts$casevar)
        regoptvar <- tclVar(searchopts$regoptvar)
        updownvar <- tclVar(searchopts$updownvar)
        searchfromtop <- tclVar(searchopts$searchfromtop)
        startpos <- tclvalue(tkindex(txt_edit,"insert"))
        faillabeltext <- tclVar("")
        
        findtext <- function(string, startpos, replaceval = NULL) {
            searchopts$searchterm <<- string
            if (!is.null(replaceval)) {
                searchopts$replaceterm <<- replaceval
            }
            searchopts$casevar <- tclvalue(casevar)
            searchopts$regoptvar <- tclvalue(regoptvar)
            searchopts$updownvar <- tclvalue(updownvar)
            searchopts$searchfromtop <- tclvalue(searchfromtop)
            if (string == "") {
                return()
            } else {
                found <- ""
                if (tclvalue(updownvar) == 1) {
                    ud1 <- "-forwards"
                    si1 <- "end"
                } else {
                    ud1 <- "-backwards"
                    si1 <- "0.0"
                }
                if (tclvalue(regoptvar) == 0) {
                    reg1 <- "-exact"
                } else {
                    reg1 <- "-regexp"
                }
                if (tclvalue(casevar) == 1) {
                    case1 <- "-nocase"
                } else {
                    case1 <- ""
                }
                found <- tclvalue(.Tcl(paste(.Tk.ID(txt_edit),"search",ud1,reg1,case1,string,startpos,si1)))
                if (!found == "") {
                    tktag.add(txt_edit, "sel", found, paste(found," +",nchar(string),"char",sep=""))
                    if (!is.null(replaceval)) {
                        if (tclvalue(updownvar) == 1) {
                            tkdelete(txt_edit, "insert", paste(found," +",nchar(string),"char",sep=""))
                        } else {
                            tkdelete(txt_edit, "insert", paste(found," -",nchar(string),"char",sep=""))
                        }
                        tkinsert(txt_edit, "insert", replaceval)
                    } else {
                        if (tclvalue(updownvar) == 1) {
                            tkmark.set(txt_edit, "insert", paste(found," +",nchar(string),"char",sep=""))
                        } else {
                            tkmark.set(txt_edit, "insert", found)
                        }
                    }
                    tkdestroy(searchDialog)
                    # if (tclvalue(searchfromtop)==1 && tclvalue(updownvar)==1) {
                        # findtext(string, "0.0")
                    # } else if (tclvalue(searchfromtop)==1 && tclvalue(updownvar)==0) {
                        # findtext(string, "end")
                    # }
                } else {
                    tclvalue(faillabeltext) <- "Text not found"
                }
            }
        }
        findval <- tclVar(searchopts$searchterm)
        replaceval <- tclVar(searchopts$replaceterm)
        searchDialog <- tktoplevel()
        tcl("wm", "attributes", searchDialog, topmost=TRUE)
        searchFocus <- function() {
            tcl("wm", "attributes", searchDialog, alpha="1.0")
        }
        searchTrans <- function() {
            tcl("wm", "attributes", searchDialog, alpha="0.4")
        }
        tkbind(searchDialog, "<FocusIn>", searchFocus)
        tkbind(searchDialog, "<FocusOut>", searchTrans)
        tkwm.title(searchDialog, paste("Search", sep=""))    # title
        lframe <- tkframe(searchDialog)
            sframe <- ttklabelframe(lframe, text = "Find", relief="groove", borderwidth=2)
                find.entry <- tkentry(sframe, width = 40, bg = "white", textvariable=findval)
                tkgrid(find.entry)
            rframe <- ttklabelframe(lframe, text = "Replace", relief="groove", borderwidth=2)
                replace.entry <- tkentry(rframe, width = 40, bg = "white", textvariable=replaceval)
                tkgrid(replace.entry)
            tkgrid(sframe, row=1, column=1, columnspan = 2)
            tkgrid(rframe, row=2, column=1, columnspan = 2)
            oframe <- ttklabelframe(lframe, text = "Options")
                regexopt <- tkcheckbutton(oframe, variable=regoptvar)
                tkgrid(regexopt, row=1, column=1, sticky="nsew")
                tkgrid(relabel <- tklabel(oframe, text = "Use RegExp?"), row=1, column=2, sticky="nsew")
                tkbind(relabel, "<Button-3>", function() browseURL("http://www.tcl.tk/man/tcl8.4/TclCmd/re_syntax.htm"))
                caseopt <- tkcheckbutton(oframe, variable=casevar)
                tkgrid(caseopt, row=2, column=1, sticky="nsew")
                tkgrid(tklabel(oframe, text = "Ignore case?"), row=2, column=2, sticky="nsew")
            tkgrid(oframe, row=3, column=1)
            searchoptions <- ttklabelframe(lframe, text = "Search Direction")
                tkgrid(tkradiobutton(searchoptions, variable=updownvar, value=0), row = 1, column = 1) 
                tkgrid(tklabel(searchoptions, text = "Up"),  row = 1, column = 2)
                tkgrid(tkradiobutton(searchoptions, variable=updownvar, value=1), row = 1, column = 3)
                tkgrid(tklabel(searchoptions, text = "Down"), row = 1, column = 4)
                tkgrid(tkcheckbutton(searchoptions, variable=searchfromtop), row = 2, column = 1)
                tkgrid(tklabel(searchoptions, text = "Search from top?"), row = 2, column = 2, columnspan = 3)
            tkgrid(searchoptions, row=3, column=2, sticky="nsew")
        tkgrid(lframe, row = 1, column = 1)
        buttons <- tkframe(searchDialog)
            # buttons
            Findbutton <- tkbutton(buttons, text = " Find Next ", width=12, 
                                   command = function() findtext(tclvalue(findval),startpos))
            Replacebutton <- tkbutton(buttons, text = "  Replace  ", width=12, 
                                      command = function() {
                                          findtext(tclvalue(findval),startpos, tclvalue(replaceval))
                                      })
            Cancelbutton <- tkbutton(buttons, text = "     Close     ", width=12, 
                                     command = function() { tkdestroy(searchDialog); tkfocus(txt_edit) } )
            tkgrid(tklabel(buttons, text = "        "), row=1, column=1)
            tkgrid(Findbutton, row=2, column=2)
            faillabel <- tklabel(buttons, text=tclvalue(faillabeltext), foreground="red")
            tkconfigure(faillabel,textvariable=faillabeltext)
            tkgrid(faillabel, row=3, column=1, columnspan=3)
            tkgrid(Replacebutton, row=4, column=2)
            tkgrid(tklabel(buttons, text = "        "), row=5, column=2)
            tkgrid(Cancelbutton, row=6, column=2)
            tkgrid(tklabel(buttons, text = "        "), row=7, column=3)
            tkgrid.columnconfigure(buttons,1,weight=2)
            tkgrid.columnconfigure(buttons,2,weight=10)
            tkgrid.columnconfigure(buttons,3,weight=2)
        tkgrid(buttons, row=1, column=3)
        tkgrid.configure(buttons, sticky="nsew")
        tkgrid.columnconfigure(searchDialog,2,weight=1)
        tkgrid.columnconfigure(searchDialog,3,weight=1)
        tkgrid.rowconfigure(searchDialog,1,weight=2)
        tkwm.resizable(searchDialog,0,0)
        tkbind(find.entry, "<Return>", function() findtext(tclvalue(findval),startpos))
        tkbind(find.entry, "<KeyPress>", function() tclvalue(faillabeltext) <- "")
        tkfocus(find.entry)
    }
    tkbind(txt_edit, "<F3>", findreplace)
    tkbind(txt_edit, "<Control-F>", findreplace)
    tkbind(txt_edit, "<Control-f>", findreplace)
    
    gotoline <- function() {
        jump <- function() {
            lineval <- tclvalue(lineval)
            if (!lineval == "") {
                tkmark.set(txt_edit,"insert",paste(lineval,".0",sep=""))
            }
            tkdestroy(goDialog)
            tksee(txt_edit,"insert")
        }
        goDialog <- tktoplevel()
        tkwm.title(goDialog, paste("Go to line",sep=""))    # title
        entryform <- ttklabelframe(goDialog, text = "Go to line", relief="groove", borderwidth=2, width = 20)
            lineval <- tclVar("")
            line.entry <- tkentry(entryform, width = 10, bg = "white", textvariable = lineval)
            tkgrid(line.entry, row = 1, column = 1)
            tkbind(line.entry, "<Return>", jump)
        tkgrid(tkbutton(entryform, text = " Go ", width = 10, command = jump), row = 1, column = 2)
        tkgrid(tkbutton(entryform, text = "Cancel", command = function() {
            tkdestroy(goDialog)
            tksee(txt_edit,"insert")
        }, width = 10), row = 1, column = 3)
        tkgrid(entryform)
        tkfocus(line.entry)
    }
    tkbind(txt_edit, "<Control-G>", gotoline)
    tkbind(txt_edit, "<Control-g>", gotoline)
    
    tryparse <- function(verbose = TRUE) {
        sel <- tclvalue(tktag.ranges(txt_edit,"sel"))
        if (!sel == "") {
            e <- try(parse(text=tclvalue(tkget(txt_edit,"sel.first","sel.last"))), silent=TRUE)
        } else {
            e <- try(parse(text=tclvalue(tkget(txt_edit,"1.0","end"))), silent=TRUE)
        }
        if (inherits(e, "try-error")) {
            e <- strsplit(e,"<text>")[[1]][2]
            if (!sel == "") {
                linen <- paste(    (as.numeric(strsplit(e,":")[[1]][2]) + as.numeric(strsplit(sel,"[.]")[[1]][1]) - 1), 
                                (as.numeric(strsplit(e,":")[[1]][3])-1), sep=".")
            } else {
                linen <- paste(    strsplit(e,":")[[1]][2], strsplit(e,":")[[1]][3], sep=".")
            }
            content <- strsplit(e,":")[[1]]
            tktag.add(txt_edit,"sel",paste(linen,"linestart"),paste(linen,"lineend"))
            tkmark.set(txt_edit,"insert",paste(linen,"-1char",sep=""))
            cat("\a")
            invisible(FALSE)
        } else {
            if (isTRUE(verbose)) {
                riteMsg(output = output, errorout = err_out, "No syntax errors found", error=TRUE)
                tkfocus(txt_edit)
            }
            invisible(TRUE)
        }
    }
    tkbind(txt_edit, "<F7>", tryparse)
    
    runkey <- function() {
        if (!tclvalue(tktag.ranges(txt_edit,"sel")) == "") {
            runCode(tclvalue(tkget(txt_edit,"sel.first","sel.last")))
        } else {
            runLine()
        }
    }
    tkbind(txt_edit, "<Control-r>", runkey)
    tkbind(txt_edit, "<Control-R>", runkey)
    tkbind(txt_edit, "<F8>", runAll)
    tkbind(txt_edit, "<Control-Return>", expression(runkey, break))
    
    tkbind(txt_edit, "<Control-s>", saveScript)
    tkbind(txt_edit, "<Control-S>", saveScript)
    
    tkbind(txt_edit, "<Control-n>", newScript)
    tkbind(txt_edit, "<Control-N>", newScript)
    
    tkbind(txt_edit, "<Control-o>", expression(loadScript(fname=NULL), break))
    tkbind(txt_edit, "<Control-O>", expression(loadScript(fname=NULL), break))
    
    if (catchOutput) {
        tkbind(txt_edit, "<Control-l>", clearOutput)
        tkbind(output, "<Control-l>", clearOutput)
        tkbind(txt_edit, "<Control-L>", clearOutput)
        tkbind(output, "<Control-L>", clearOutput)
    } else {
        tkbind(txt_edit, "<Control-l>", function() {cat(rep("\n",50),collapse="")})
        tkbind(txt_edit, "<Control-L>", function() {cat(rep("\n",50),collapse="")})
    }
    
    toggleComment <- function() {
        checkandtoggle <- function(pos) {
            n <- nchar(comment)
            if (tclvalue(tkget(txt_edit, pos, paste(pos,"+",n+1,"char",sep=""))) == paste(comment," ",sep="")) {
                tkdelete(txt_edit, pos, paste(pos,"+",n+1,"char",sep=""))
            } else if (tclvalue(tkget(txt_edit, pos, paste(pos,"+",n,"char",sep=""))) == comment) {
                tkdelete(txt_edit, pos, paste(pos,"+",n,"char",sep=""))
            } else {
                tkmark.set(txt_edit,"insert",pos)
                tkinsert(txt_edit, "insert", paste(comment," ",sep=""))
            }
        }
        selrange <- tclvalue(tktag.ranges(txt_edit,"sel"))
        if (!selrange == "") {
            selrange <- floor(as.numeric(strsplit(selrange," ")[[1]]))
            for (i in selrange[1]:selrange[2]) {
                checkandtoggle(paste(i,".0 linestart",sep=""))
            }
            tktag.add(txt_edit,"sel",paste(selrange[1],'.0',sep=''), paste(selrange[2],".0 lineend",sep=''))
        } else {
            checkandtoggle("insert linestart")
        }
    }
    tkbind(txt_edit, "<Control-k>", expression(toggleComment, break))
    tkbind(txt_edit, "<Control-k>", expression(toggleComment, break))
    
    multitab <- function() {
        insertpos <- strsplit(tclvalue(tkindex(txt_edit,"insert")),".", fixed=TRUE)[[1]]
        insertpos2 <- paste(insertpos[1],".",as.numeric(insertpos[2])+1,sep="")
        selrange <- tclvalue(tktag.ranges(txt_edit,"sel"))
        if (selrange == "") {
            tkinsert(txt_edit, 'insert', tab)
        } else {
            selrange2 <- floor(as.numeric(strsplit(selrange," ")[[1]]))
            if (selrange2[1] == selrange2[2]) {
                tkinsert(txt_edit, 'insert', tab)
            } else {
                for (i in selrange2[1]:selrange2[2]) {
                    tkinsert(txt_edit, paste(i,".0 linestart",sep=""), tab)
                }
            }
            tktag.add(txt_edit,"sel",paste(selrange2[1],'.0',sep=''), paste(selrange2[2],".0 lineend",sep=''))
        }
        #tkmark.set(txt_edit, 'insert', paste('insert+',nchar(tab),'char',sep=''))
    }
    multiuntab <- function() {
        insertpos <- strsplit(tclvalue(tkindex(txt_edit,"insert")),".", fixed=TRUE)[[1]]
        insertpos2 <- paste(insertpos[1],".",as.numeric(insertpos[2])-1,sep="")
        selrange <- tclvalue(tktag.ranges(txt_edit,"sel"))
        if (selrange=="") {
            check <- tclvalue(tkget(txt_edit, "insert linestart", "insert linestart+1char"))
            if (grepl('^[[:space:]]+',check)[[1]]) {
                tkdelete(txt_edit, "insert linestart", "insert linestart+1char")
            }
        } else {
            selrange2 <- round(as.numeric(strsplit(selrange," ")[[1]]),0)
            for (i in selrange2[1]:selrange2[2]) {
                pos <- paste(i,".0 linestart",sep="")
                check <- tclvalue(tkget(txt_edit, pos, paste(pos,"+1char",sep="")))
                if (grepl('^[[:space:]]+',check)[[1]]) {
                    tkdelete(txt_edit, pos, paste(pos,"+1char",sep=""))
                }
            }
            tktag.add(txt_edit,"sel",paste(selrange2[1],'.0',sep=''), paste(selrange2[2],".0 lineend",sep=''))
            #tkmark.set(txt_edit, "insert", insertpos2)
        }
    }
    tkbind(txt_edit, "<Control-i>", expression(multitab, break))
    tkbind(txt_edit, "<Control-I>", expression(multitab, break))
    tkbind(txt_edit, "<Tab>", expression(multitab, break))
    tkbind(txt_edit, "<Shift-Tab>", expression(multiuntab, break))
    tkbind(txt_edit, "<Control-u>", expression(multiuntab, break))
    tkbind(txt_edit, "<Control-U>", expression(multiuntab, break))
    
    tabreturn <- function() {
        # detect tab(s) and other whitespace
        thisline <- tclvalue(tkget(txt_edit, "insert linestart", "insert lineend"))
        spaces <- gregexpr('[[:space:]]',thisline)[[1]]
        if (spaces[1] == -1) {
            lead <- ""
        } else {
            linechars <- strsplit(thisline,'')[[1]]
            leadn <- which(spaces[1:length(linechars)]==seq_along(linechars))
            if (!is.na(leadn[1])) {
                lead <- substring(thisline,range(leadn)[1],range(leadn)[2])
            } else {
                lead <- ""
            }
            if (is.na(lead)) {
                lead <- ""
            }
        }
        tkinsert(txt_edit, "insert ", paste("\n",lead,sep=""))
        tksee(txt_edit, "insert")
    }
    tkbind(txt_edit, "<Return>", expression(tabreturn, break))
    
    togglehome <- function() {
        insertpos <- strsplit(tclvalue(tkindex(txt_edit,"insert")),".", fixed=TRUE)[[1]]
        if (insertpos[2]=='0') {
            thisline <- tclvalue(tkget(txt_edit, "insert linestart", "insert lineend"))
            firstchar <- regexpr('[[:alnum:]]',thisline)[1]
            if (!is.null(firstchar) && length(firstchar)>0 && !firstchar == -1) {
                tkmark.set(txt_edit,'insert',paste(insertpos[1],firstchar-1,sep='.'))
            }
        } else {
            tkmark.set(txt_edit,'insert','insert linestart')
        }
        tksee(txt_edit, 'insert')
        # handle selection
        selrange <- tclvalue(tktag.ranges(txt_edit,"sel"))
        if (!selrange == "") {
            selrange <- strsplit(selrange,' ')[[1]]
            tktag.remove(txt_edit,'sel',selrange[1],selrange[2])
            if (strsplit(selrange[1],'.',fixed=TRUE)[[1]][1] == insertpos[1]) {
                tktag.add(txt_edit,"sel",'insert',selrange[2])
            }
            if (strsplit(selrange[2],'.',fixed=TRUE)[[1]][1] == insertpos[1]) {
                tktag.add(txt_edit,"sel",selrange[1],'insert')
            }
        }
    }
    tkbind(txt_edit, "<Home>", expression(togglehome, break))
    ctrlhome <- function() {
        tkmark.set(txt_edit,'insert','1.0')
        tksee(txt_edit, 'insert')
    }
    tkbind(txt_edit, "<Control-Home>", expression(ctrlhome, break))
    shifthome <- function() {
        tktag.add(txt_edit,"sel",'insert linestart','insert')
        togglehome()
    }
    tkbind(txt_edit, "<Shift-Home>", expression(shifthome, break))
    
    
    ### CONTEXT MENU ###
    selectAllEdit <- function() {
        tktag.add(txt_edit,"sel","0.0","end")
        tkmark.set(txt_edit,"insert","end")
    }
    tkbind(txt_edit, "<Control-A>", expression(selectAllEdit, break))
    tkbind(txt_edit, "<Control-a>", expression(selectAllEdit, break))
    
    if (isTRUE(catchOutput)) {
        selectAllOutput <- function() {
            tktag.add(output,"sel","0.0","end")
            tkmark.set(output,"insert","end")
        }
        tkbind(output, "<Control-A>", expression(selectAllOutput, break))
        tkbind(output, "<Control-a>", expression(selectAllOutput, break))
    }
    
    copyText <- function(docut=FALSE) {
        selrange <- strsplit(tclvalue(tktag.ranges(txt_edit,"sel"))," ")[[1]]
        if (!tclvalue(tktag.ranges(txt_edit,"sel"))=="") {
            tkclipboard.clear()
            tkclipboard.append(tclvalue(tkget(txt_edit, selrange[1], selrange[2])))
            if (isTRUE(docut)) {
                tkdelete(txt_edit, selrange[1], selrange[2])
            }
        } else {
            cat("\a")
        }
    }
    pasteText <- function() {
        if (.Platform$OS.type %in% "windows") {
            cbcontents <- readLines("clipboard")
        } else if (Sys.getenv("OS") %in% "unix") {
            cbcontents <- readLines(pipe("pbpaste"))
        } else {
            cbcontents <- ""
        }
        tkinsert(txt_edit, "insert", paste(cbcontents,collapse="\n"))
    }
    
    editcase <- function(type) {
        if (!tclvalue(tktag.ranges(txt_edit,"sel"))=="") {
            seltxt <- tclvalue(tkget(txt_edit,"sel.first","sel.last"))
            if (type == "toupper") {
                seltxt <- toupper(seltxt)
            } else {
                seltxt <- tolower(seltxt)
            }
            tkdelete(txt_edit, "sel.first", "sel.last")
            tkinsert(txt_edit, "insert", seltxt)
        }
    }
    
    contextMenu <- tkmenu(txt_edit, tearoff = FALSE)
        tkadd(contextMenu, "command", label = "Run line/selection <Ctrl-R>", command = runkey)
        tkadd(contextMenu, "command", label = "Parse all <F7>", command = tryparse)
        tkadd(contextMenu, "command", label = "Run all <F8>", command = runAll)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label = "Select All <Ctrl-A>", command = selectAllEdit)
        tkadd(contextMenu, "command", label = "Copy <Ctrl-C>", command = copyText)
        tkadd(contextMenu, "command", label = "Cut <Ctrl-X>", command = function() copyText(docut=TRUE))
        tkadd(contextMenu, "command", label = "Paste <Ctrl-V>", command = pasteText)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label = "TO UPPER", command = function() editcase("toupper"))
        tkadd(contextMenu, "command", label = "to lower", command = function() editcase("tolower"))
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label = "Find <Ctrl-F>", command = findreplace)
        tkadd(contextMenu, "command", label = "Go to line <Ctrl-G>", command = gotoline)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label = "Lookup Function <F1>", command = f1)
    rightClick <- function(x, y) {
        rootx <- as.integer(tkwinfo("rootx", txt_edit))
        rooty <- as.integer(tkwinfo("rooty", txt_edit))
        xTxt <- as.integer(x) + rootx
        yTxt <- as.integer(y) + rooty
        tkmark.set(txt_edit,"insert",paste("@",xTxt,",",yTxt,sep=""))
        .Tcl(paste("tk_popup", .Tcl.args(contextMenu, xTxt, yTxt)))
    }
    tkbind(txt_edit, "<Button-3>", rightClick)
    
    ## SYNTAX HIGHLIGHTING ##
    highlight(txt_edit, highlight = highlight, hcolors = hcolors)
    
    ## DISPLAY EDITOR ##
    if (!is.null(filename)) {
        loadScript(fname=filename)
    } else if (!is.null(url)) {
        loadScript(fname=url, locals=FALSE)
    }
    tkmark.set(txt_edit,"insert","1.0")
    tkfocus(txt_edit)
    tksee(txt_edit, "insert")
    if (isTRUE(catchOutput) && .Platform$OS.type %in% "windows") {
        tcl("wm", "state", riteenv$editor, "zoomed")
        #tcl("wm", "attributes", riteenv$editor, "fullscreen")
    }
}

#' @rdname rite
#' @export
riteout <- function(catchOutput = TRUE, ...) {
    rite(catchOutput = catchOutput, ...)
}

if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("osink", "riteoutcon", "addHighlighting"))
}
