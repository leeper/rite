#' @rdname sink
#' @title ritesink
#' @description An experimental tcl/tk output widget
#' @param echo A logical indicating whether calls should be output to the sink. Default is \code{TRUE}.
#' @param print.eval A logical indicating whether evaluated output from calls and/or messages should be sent to the sink. Default is \code{TRUE}.
#' @param split A logical indicating whether output (but not messages) should be split between the console and the sink. Default is \code{FALSE}.
#' @param prompt.echo A character string indicating the text to use for the prompt in the widget. The default is drawn from \code{options("prompt")}.  Ignored if \code{echo = FALSE}.
#' @param fontFamily The font family used in rite. Default is \dQuote{\code{Courier}}. Available fonts can be retrieved by \code{.Tcl("font families")}.
#' @param fontSize The font size used in rite. Default is \code{10}.
#' @param col.bg A one-element character string indicating a tcl/tk color for the background color of the sink. A list of available tcl/tk colors can be found here: \url{http://www.tcl.tk/man/tcl8.5/TkCmd/colors.htm}.
#' @param col.call A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of evaluated R calls (only visible if \code{echo=TRUE}..
#' @param col.result A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of standard output.
#' @param col.err A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of errors.
#' @param col.warn A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of warnings.
#' @param col.msg A two-element character vector indicating tcl/tk colors for the foreground and background, respectively, of messages.
#' @param quiet A logical indicating whether to suppress confirmation that everything is cleaned up. Default is \code{TRUE}.
#' @details
#' These functions make use of a couple of different R features to build a color-coded output window for the R console. While the console is limited to displaying output in monochrome plain text, the rite sink allows multi-colored output and messages to be piped to a single widget that highlights errors, warning, and messages. Unlike a traditional sink, rite sink is a tcl/tk widget that updates output as commands and messages occur. Accomplishing this requires the use of \code{sink}, task callbacks, and a custom error handler. (This is similar to the R2HTML package.) 
#' 
#' \code{sinkstart} starts the sink and \code{sinkstop} stops the sink without destroying the widget. Closing the sink widget invisibly calls \code{sinkstop} and cleans up. The sink can be turned on and off repeatedly without closing the widget.
#' @return \code{NULL}
#' @section Shortcut keys in widget:
#'   \code{<Ctrl-c>}: Copy
#' 
#'   \code{<Ctrl-x>}: Cut
#' 
#'   \code{<Ctrl-p>}: Paste
#' 
#'   \code{<Ctrl-a>}: Select all
#' 
#'   \code{<Ctrl-s>}: Save output
#' 
#'   \code{<Ctrl-l>}: Clear output
#' @references
#' \href{http://developer.r-project.org/TaskHandlers.pdf}{Top-level Task Callbacks in R}
#' 
#' \href{http://cran.r-project.org/web/packages/R2HTML/index.html}{R2HTML package}
#' @author Thomas J. Leeper 
#' @examples
#' \dontrun{
#' sinkstart() # open the sink
#' sinkstop() # close the sink
#' }
#' @keywords IO 
#' @export
sinkstart <- function(
    echo=TRUE, print.eval=TRUE, split=FALSE,
    prompt.echo=getOption("prompt", "> "), 
    fontFamily='Courier', fontSize=10,
    col.bg='white', col.call=c('black',col.bg), col.result=c('black',col.bg),
    col.err=c('red',col.bg), col.warn=c('purple',col.bg), col.msg=c('blue',col.bg))
{
    if ('outsink' %in% getTaskCallbackNames()) {
        stop('ritesink is already active')
    }
    
    ritesinkenv$echo <- echo
    ritesinkenv$print.eval <- print.eval
    ritesinkenv$prompt.echo <- prompt.echo
    ritesinkenv$prompt.echo.old <- prompt.echo
    ritesinkenv$split <- split
    
    # setup colors
    if (is.null(col.bg) || is.na(col.bg) || col.bg == '') {
        col.bg <- 'white'
    }
    if (length(col.bg) > 1) {
        col.bg <- col.bg[1]
        warning('More than one color specified for background. Only first is used.')
    }
    if (is.null(col.call)) {
        col.call <- c('black',col.bg)
    }
    if (is.null(col.result)) {
        col.result <- col.call
    }
    if (is.null(col.err)) {
        col.err <- c('red',col.bg)
    }
    if (is.null(col.warn)) {
        col.warn <- c('purple',col.bg)
    }
    if (is.null(col.msg)) {
        col.msg <- c('blue',col.bg)
    }
    if (length(col.call) == 1) {
        col.call <- c(col.call,col.bg)
    }
    if (length(col.result) == 1) {
        col.result <- c(col.result,col.bg)
    }
    if (length(col.err) == 1) {
        col.err <- c(col.err,col.bg)
    }
    if (length(col.warn) == 1) {
        col.warn <- c(col.warn,col.bg)
    }
    if (length(col.msg) == 1) {
        col.msg <- c(col.msg,col.bg)
    }
    if (col.bg == col.call[1]) {
        stop('Background and call foreground colors are the same')
    }
    if (col.bg == col.result[1]) {
        stop('Background and result foreground colors are the same')
    }
    
    # sinks
    ritesinkenv$stdsink <- file(ritesinkenv$otmp <- tempfile(),'w+')
    sink(ritesinkenv$stdsink, split=split, type='output') # output
    writeLines('# rite output sink', ritesinkenv$stdsink)
    ritesinkenv$lengtho <- paste(scan(ritesinkenv$stdsink, what='character',
                                 sep='\n', quiet=TRUE), collapse='\n')
    
    ritesinkenv$errsink <- file(ritesinkenv$etmp <- tempfile(),'w+')
    sink(ritesinkenv$errsink, type='message') # message
    writeLines('# rite error sink', ritesinkenv$errsink)
    ritesinkenv$lengthe <- paste(scan(ritesinkenv$errsink, what='character',
                                 sep='\n', quiet=TRUE), collapse='\n')
    
    # error handler
    ritesinkenv$errhandler <- function() {
        tkinsert(ritesinkenv$output, 'insert', paste('\n', geterrmessage(), sep=''), ('error'))
        tksee(ritesinkenv$output, 'insert')
        invisible(NULL)
    }
        
    # callback function
    outsink <- function() {
        function(expr, value, ok, visible) {
            #e <- as.character(as.expression(expr))
            e <- deparse(expr)
            if (isTRUE(ok)) {
                if (isTRUE(ritesinkenv$echo)) {
                    tkinsert(ritesinkenv$output, 'insert', paste0('\n', ritesinkenv$prompt.echo, e), ('call'))
                }
                # Output sink (for `cat` and `print`)
                osink <- paste(scan(ritesinkenv$otmp, what='character',
                                sep='\n', quiet=TRUE),collapse='\n')
                if (!identical(osink, ritesinkenv$lengtho) && length(osink)>0) {
                    last <- substr(osink, nchar(ritesinkenv$lengtho)+1, nchar(osink))
                    # handle `simpleError` etc. that trigger callback
                    if (ritesinkenv$print.eval) {
                        if (grepl("simpleError", last)) {
                            tkinsert(ritesinkenv$output, 'insert', last, ('error'))
                        } else if (grepl("simpleWarning", last)) {
                            tkinsert(ritesinkenv$output, 'insert', last, ('warning'))
                        } else if (grepl("simpleMessage", last)) {
                            tkinsert(ritesinkenv$output, 'insert', last, ('message'))
                        } else if (grepl("simpleCondition", last)) {
                            tkinsert(ritesinkenv$output, 'insert', last, ('message'))
                        } else {
                            tkinsert(ritesinkenv$output, 'insert', last, ('result'))
                        }
                    }
                    ritesinkenv$lengtho <- osink
                }
                #tkinsert(ritesinkenv$output,'insert','ok\n', ('message')) # print confirm 'ok' on non-printing calls
                ritesinkenv$lengtho <- paste(scan(ritesinkenv$otmp, what='character',
                                        sep='\n', quiet=TRUE),collapse='\n')
            } else if (isTRUE(visible) && !isTRUE(ok)) { # !ok doesn't happen
                tkinsert(ritesinkenv$output,'insert','Error\n', ('error'))
            } else if (!isTRUE(visible) && !isTRUE(ok)) { # !ok doesn't happen
                tkinsert(ritesinkenv$output,'insert','Non-printing error\n', ('error'))
            }
            
            # Error sink (for `warning` and `message`)
            esink <- paste(scan(ritesinkenv$etmp, what='character',
                            sep='\n', quiet=TRUE), collapse='\n')
            if (!identical(esink, ritesinkenv$lengthe) && length(esink)>0) {
                fromerr <- substr(esink,nchar(ritesinkenv$lengthe)+1,nchar(esink))
                if (any(grepl('error', fromerr))){
                    tkinsert(ritesinkenv$output, 'insert',
                        paste(fromerr, '\n', collapse='\n'), ('error'))
                } else if (any(grepl('Warning', fromerr))){
                    tkinsert(ritesinkenv$output, 'insert',
                        paste(fromerr, collapse='\n'), ('warning'))
                } else {
                    tkinsert(ritesinkenv$output, 'insert',
                        paste(fromerr, collapse='\n'), ('message'))
                }
                ritesinkenv$lengthe <- esink
            }
            tksee(ritesinkenv$output, 'insert')
            TRUE
        }
    }
    addTaskCallback(outsink(), name = 'outsink')
        
    if (!'thesink' %in% ls(ritesinkenv)) {
        ritesinkenv$thesink <- tktoplevel(borderwidth = 0)
        exitsink <- function() {
            tkdestroy(ritesinkenv$thesink)
            if (!sink.number() == 0) {
                sink()
            }
            if (!sink.number('message') == 2) {
                sink(type = 'message')
            }
            rm(thesink, output, scr, stdsink, errsink, envir = ritesinkenv)
            sinkstop()
            invisible()
        }

        # widget
        tkwm.protocol(ritesinkenv$thesink, 'WM_DELETE_WINDOW', exitsink)
        tkwm.title(ritesinkenv$thesink, 'rite sink')        # title
        ritesinkenv$scr <- tkscrollbar(ritesinkenv$thesink, 
                                       repeatinterval=25,
                                       command = function(...){ tkyview(ritesinkenv$output,...) })
        ritesinkenv$output <- tktext(ritesinkenv$thesink, bg=col.bg, fg=col.result[1], undo='true',
                                     yscrollcommand = function(...) tkset(ritesinkenv$scr,...),
                                     font=tkfont.create(family=fontFamily, size=fontSize))
        tcl('wm', 'attributes', ritesinkenv$thesink, topmost=TRUE)
        tkgrid(ritesinkenv$output, sticky='nsew', column=1, row=1)
        tkgrid(ritesinkenv$scr, sticky='nsew', column=2, row=1)
        tkgrid.columnconfigure(ritesinkenv$thesink,1,weight=1)
        tkgrid.columnconfigure(ritesinkenv$thesink,2,weight=0)
        tkgrid.rowconfigure(ritesinkenv$thesink,1,weight=1)
        
        # tags/fonts
        if (!exists('ritesinkenv$defaultfont')) {
            ritesinkenv$defaultfont <- tkfont.create(family=fontFamily, size=fontSize)
        }
        tktag.configure(ritesinkenv$output, 'call',
            foreground=col.call[1],
            background=col.call[2],
            font=ritesinkenv$defaultfont,
            underline=0)
        tktag.configure(ritesinkenv$output, 'result',
            foreground=col.result[1],
            background=col.result[2],
            font=ritesinkenv$defaultfont,
            underline=0)
        if (!exists('ritesinkenv$boldfont')) {
            ritesinkenv$boldfont <- tkfont.create(family=fontFamily, size=fontSize, weight='bold')
        }
        tktag.configure(ritesinkenv$output, 'error',
            foreground=col.err[1],
            background=col.err[2],
            font=ritesinkenv$boldfont,
            underline=0)
        tktag.configure(ritesinkenv$output, 'warning',
            foreground=col.warn[1],
            background=col.warn[2],
            font=ritesinkenv$boldfont,
            underline=0)
        tktag.configure(ritesinkenv$output, 'message',
            foreground=col.msg[1],
            background=col.msg[2],
            font=ritesinkenv$boldfont,
            underline=0)
        
        # bind option('width') to window resize
        resize <- function() {
            w <- tkwinfo('width',ritesinkenv$output)
            m <- tkfont.measure(ritesinkenv$defaultfont,'m')
            nw <- round((as.numeric(w)-20)/as.numeric(m))
            options(width=nw)
        }
        tkbind(ritesinkenv$thesink, '<Configure>', resize)
        
        # context menu (and associated functions and bindings)
        saveSink <- function() {
            outfilename <- tclvalue(tkgetSaveFile(initialdir=getwd(),
                            title='Save Output',
                            filetypes=paste('{{Text file} {*.txt}} {{All files} {*.*}}'),
                            defaultextension='.txt'))
            if (!length(outfilename) || outfilename=="") {
                invisible()
            }
            chn <- tclopen(outfilename, 'w')
            tclputs(chn, tclvalue(tkget(ritesinkenv$output,'0.0','end')))
            tclclose(chn)
        }
        tkbind(ritesinkenv$output, '<Control-S>', expression(saveSink, break))
        tkbind(ritesinkenv$output, '<Control-s>', expression(saveSink, break))
        selectAll <- function() {
            tktag.add(ritesinkenv$output,'sel','0.0','end')
            tkmark.set(ritesinkenv$output,'insert','end')
        }
        tkbind(ritesinkenv$output, "<Control-A>", expression(selectAll, break))
        tkbind(ritesinkenv$output, "<Control-a>", expression(selectAll, break))
        copyText <- function(docut = FALSE){
            selrange <- strsplit(tclvalue(tktag.ranges(ritesinkenv$output,"sel"))," ")[[1]]
            if (!tclvalue(tktag.ranges(ritesinkenv$output,"sel"))=="") {
                tkclipboard.clear()
                tkclipboard.append(tclvalue(tkget(ritesinkenv$output, selrange[1], selrange[2])))
                if (isTRUE(docut)) {
                    tkdelete(ritesinkenv$output, selrange[1], selrange[2])
                }
            } else {
                selectAll()
                copyText()
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
            tkinsert(ritesinkenv$output, "insert", paste(cbcontents,collapse="\n"))
        }
        clearSink <- function() tkdelete(ritesinkenv$output, '1.0', 'end')
        tkbind(ritesinkenv$output, "<Control-L>", expression(clearSink, break))
        tkbind(ritesinkenv$output, "<Control-l>", expression(clearSink, break))
        
        contextMenu <- tkmenu(ritesinkenv$output, tearoff = FALSE)
            tkadd(contextMenu, "command", label = "Save <Ctrl-S>", command = saveSink)
            tkadd(contextMenu, "command", label = "Clear All <Ctrl-L>", command = clearSink)
            tkadd(contextMenu, "separator")
            tkadd(contextMenu, "command", label = "Select All <Ctrl-A>", command = selectAll)
            tkadd(contextMenu, "command", label = "Copy <Ctrl-C>", command = copyText)
            tkadd(contextMenu, "command", label = "Cut <Ctrl-X>", command = function() copyText(docut=TRUE))
            tkadd(contextMenu, "command", label = "Paste <Ctrl-V>", command = pasteText)
            tkadd(contextMenu, "separator")
            tkadd(contextMenu, "command",
                label = paste("Toggle echo on/off"),
                command = function() {
                    if (ritesinkenv$echo) {
                        ritesinkenv$echo <- FALSE
                    } else {
                        ritesinkenv$echo <- TRUE
                    }
                })
            tkadd(contextMenu, "command",
                label = paste("Toggle print.eval on/off"),
                command = function() {
                    if (ritesinkenv$print.eval) {
                        ritesinkenv$print.eval <- FALSE
                    } else {
                        ritesinkenv$print.eval <- TRUE
                    }
                })
            tkadd(contextMenu, "command",
                label = paste("Toggle prompt.echo on/off"),
                command = function() {
                    if (ritesinkenv$prompt.echo == "") {
                        if (!exists(ritesinkenv$prompt.echo.old)) {
                            ritesinkenv$prompt.echo.old <- getOption("prompt", "> ")
                        }
                        ritesinkenv$prompt.echo <- ritesinkenv$prompt.echo.old
                    } else {
                        ritesinkenv$prompt.echo.old <- ritesinkenv$prompt.echo
                        ritesinkenv$prompt.echo <- ""
                    }
                })
        rightClick <- function(x, y) {
            rootx <- as.integer(tkwinfo("rootx", ritesinkenv$output))
            rooty <- as.integer(tkwinfo("rooty", ritesinkenv$output))
            xTxt <- as.integer(x) + rootx
            yTxt <- as.integer(y) + rooty
            tkmark.set(ritesinkenv$output,"insert",paste("@",xTxt,",",yTxt,sep=""))
            .Tcl(paste("tk_popup", .Tcl.args(contextMenu, xTxt, yTxt)))
        }
        tkbind(ritesinkenv$output, "<Button-3>", rightClick)
        tkbind(contextMenu, "<Button-3>", function() tkunpost(contextMenu))
    
    }
    
    options('show.error.messages' = FALSE) # default TRUE
    options('error' = ritesinkenv$errhandler) # default NULL
    
    invisible(NULL)
}

#' @rdname sink
#' @export
sinkstop <- function(quiet = TRUE){
    # check for active sink
    if (!exists('ritesinkenv')) {
        stop('sink already closed and removed')
    }
    # reset options defaults
    options('show.error.messages'=TRUE) # default TRUE
    options('error'=NULL) # default NULL
    options('width'=80) # default 80
    
    # stop sinks
    if (!sink.number()==0) {
        sink()
    }
    if (!sink.number('message')==2) {
        sink(type='message')
    }
    
    # remove call back
    if ('outsink' %in% getTaskCallbackNames()) {
        removeTaskCallback('outsink')
    }
    
    # close connections
    if (ritesinkenv$otmp %in% showConnections()) {
        thiscon <- rownames(showConnections())[which(ritesinkenv$otmp==showConnections()[,1])]
        close(getConnection(thiscon))
    }
    if (ritesinkenv$etmp %in% showConnections()) {
        thiscon <- rownames(showConnections())[which(ritesinkenv$etmp==showConnections()[,1])]
        close(getConnection(thiscon))
    }
    
    # remove temporary sink files
    unlink(ritesinkenv$otmp)
    unlink(ritesinkenv$etmp)
    
    # remove `ritesinkenv`
    if (!"thesink" %in% ls(ritesinkenv)) {
        if (!isTRUE(quiet)) {
            message('rite sink closed and removed')
        }
    } else if (!isTRUE(quiet)) {
        message('rite sink closed')
    }
    
    invisible(NULL)
}

ritesinkenv <- new.env()

if (getRversion() >= "2.15.1") {
    utils::globalVariables(c('thesink', 'output', 'scr', 'stdsink', 'errsink'))
}
