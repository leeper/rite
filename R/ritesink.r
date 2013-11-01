sinkstart <- function(
    echo=TRUE, split=FALSE,
    fontFamily='Courier', fontSize=10,
    col.bg='white', col.call='black', col.result='black',
    col.err='red', col.warn='purple', col.msg='blue')
{
    require(tcltk)
    if('outsink' %in% getTaskCallbackNames())
        stop('ritesink is already active')
    
    if(!exists('riteenv')){
        #riteenv <- new.env(parent = as.environment('package:rite'))
        #assignInMyNamespace('riteenv', new.env())
        assign('riteenv', new.env(parent = .GlobalEnv), .GlobalEnv)
    }
    riteenv$echo <- echo
    riteenv$split <- split
    
    # sinks
    riteenv$stdsink <- file(riteenv$otmp <- tempfile(),'w+')
    sink(riteenv$stdsink, split=split, type='output') # output
    writeLines('# rite output sink', riteenv$stdsink)
    riteenv$lengtho <- paste(scan(riteenv$stdsink, what='character',
                                sep='\n', quiet=TRUE),collapse='\n')
    
    riteenv$errsink <- file(riteenv$etmp <- tempfile(),'w+')
    sink(riteenv$errsink, type='message') # message
    writeLines('# rite error sink', riteenv$errsink)
    riteenv$lengthe <- paste(scan(riteenv$errsink, what='character',
                                sep='\n', quiet=TRUE),collapse='\n')
    
    # error handler
    riteenv$errhandler <- function(){
        tkinsert(riteenv$output,'insert',paste('\n', geterrmessage(), sep=''), ('error'))
        tksee(riteenv$output, 'insert')
        invisible(NULL)
    }
        
    # callback function
    outsink <- function() {
        function(expr, value, ok, visible) {
            e <- as.character(as.expression(expr))
            last <- paste(capture.output(.Last.value),collapse='\n')
            if(visible && ok){
                if(riteenv$echo){
                    tkinsert(riteenv$output, 'insert', paste('\n>',e,'\n'), ('call'))
                    # handle `simpleError` etc. that trigger callback
                    if(grepl("simpleError", last))
                        tkinsert(riteenv$output, 'insert', last, ('error'))
                    else if(grepl("simpleWarning", last))
                        tkinsert(riteenv$output, 'insert', last, ('warning'))
                    else if(grepl("simpleMessage", last))
                        tkinsert(riteenv$output, 'insert', last, ('message'))
                    else if(grepl("simpleCondition", last))
                        tkinsert(riteenv$output, 'insert', last, ('message'))
                    else
                        tkinsert(riteenv$output, 'insert', last, ('result'))
                }
                else{
                    tkinsert(riteenv$output, 'insert', last, ('result'))
                }
                riteenv$lengtho <- paste(scan(riteenv$otmp, what='character',
                                        sep='\n', quiet=TRUE),collapse='\n')
            }
            else if(visible && !ok) # !ok doesn't happen
                tkinsert(riteenv$output,'insert','Error\n', ('error'))
            else if(!visible && !ok) # !ok doesn't happen
                tkinsert(riteenv$output,'insert','Non-printing error\n', ('error'))
            else {
                # Output sink (for `cat` and `print`)
                osink <- paste(scan(riteenv$otmp, what='character',
                                sep='\n', quiet=TRUE),collapse='\n')
                if(!identical(osink, riteenv$lengtho) && length(osink)>0){
                    fromout <- substr(osink, nchar(riteenv$lengtho)+1, nchar(osink))
                    tkinsert(riteenv$output, 'insert',
                        paste(fromout, collapse='\n'), ('result'))
                    riteenv$lengtho <- osink
                }
                #tkinsert(riteenv$output,'insert','ok\n', ('message')) # print confirm 'ok' on non-printing calls
            }
            # Error sink (for `warning` and `message`)
            esink <- paste(scan(riteenv$etmp, what='character',
                            sep='\n', quiet=TRUE),collapse='\n')
            if(!identical(esink, riteenv$lengthe) && length(esink)>0){
                fromerr <- substr(esink,nchar(riteenv$lengthe)+1,nchar(esink))
                if(any(grepl('error', fromerr))){
                    tkinsert(riteenv$output, 'insert',
                        paste(fromerr, '\n', collapse='\n'), ('error'))
                }
                else if(any(grepl('Warning', fromerr))){
                    tkinsert(riteenv$output, 'insert',
                        paste(fromerr, collapse='\n'), ('warning'))
                }
                else{
                    tkinsert(riteenv$output, 'insert',
                        paste(fromerr, collapse='\n'), ('message'))
                }
                riteenv$lengthe <- esink
            }
            tksee(riteenv$output, 'insert')
            TRUE
        }
    }
    addTaskCallback(outsink(), name='outsink')
        
    if(!'thesink' %in% ls(riteenv)){
        riteenv$thesink <- tktoplevel(borderwidth=0)
        exitsink <- function() {
            tkdestroy(riteenv$thesink)
            if('windows'==.Platform$OS.type)
                bringToTop(-1)
            if(!sink.number()==0)
                sink()
            if(!sink.number('message')==2)
                sink(type='message')
            rm(thesink, output, scr, stdsink, errsink, envir = riteenv)
            sinkstop()
            invisible()
        }

        # widget
        tkwm.protocol(riteenv$thesink, 'WM_DELETE_WINDOW', exitsink)
        tkwm.title(riteenv$thesink, 'rite sink')        # title
        riteenv$scr <- tkscrollbar(riteenv$thesink, 
                        repeatinterval=25,
                        command=function(...){ tkyview(riteenv$output,...) })
        riteenv$output <- tktext(riteenv$thesink, bg=col.bg, fg=col.result, undo='true',
                                   yscrollcommand=function(...) tkset(riteenv$scr,...),
                                   font=tkfont.create(family=fontFamily, size=fontSize))
        tcl('wm', 'attributes', riteenv$thesink, topmost=TRUE)
        tkgrid(riteenv$output, sticky='nsew', column=1, row=1)
        tkgrid(riteenv$scr, sticky='nsew', column=2, row=1)
        tkgrid.columnconfigure(riteenv$thesink,1,weight=1)
        tkgrid.columnconfigure(riteenv$thesink,2,weight=0)
        tkgrid.rowconfigure(riteenv$thesink,1,weight=1)
        
        # tags/fonts
        if(!exists('riteenv$defaultfont'))
            riteenv$defaultfont <- tkfont.create(family=fontFamily, size=fontSize)
        tktag.configure(riteenv$output, 'call',
            foreground=col.call,
            font=riteenv$defaultfont,
            underline=0)
        tktag.configure(riteenv$output, 'result',
            foreground=col.result,
            font=riteenv$defaultfont,
            underline=0)
        if(!exists('riteenv$boldfont'))
            riteenv$boldfont <- tkfont.create(family=fontFamily, size=fontSize, weight='bold')
        tktag.configure(riteenv$output, 'error',
            foreground=col.err,
            font=riteenv$boldfont,
            underline=0)
        tktag.configure(riteenv$output, 'warning',
            foreground=col.warn,
            font=riteenv$boldfont,
            underline=0)
        tktag.configure(riteenv$output, 'message',
            foreground=col.msg,
            font=riteenv$boldfont,
            underline=0)
        
        # bind option('width') to window resize
        resize <- function(){
            w <- tkwinfo('width',riteenv$output)
            m <- tkfont.measure(riteenv$defaultfont,'m')
            nw <- round((as.numeric(w)-20)/as.numeric(m))
            options(width=nw)
        }
        tkbind(riteenv$thesink, '<Configure>', resize)
        
        # context menu (and associated functions and bindings)
        selectAll <- function(){
            tktag.add(riteenv$output,"sel","0.0","end")
            tkmark.set(riteenv$output,"insert","end")
        }
        tkbind(riteenv$output, "<Control-A>", expression(selectAll, break))
        tkbind(riteenv$output, "<Control-a>", expression(selectAll, break))
        copyText <- function(docut=FALSE){
            selrange <- strsplit(tclvalue(tktag.ranges(riteenv$output,"sel"))," ")[[1]]
            if(!tclvalue(tktag.ranges(riteenv$output,"sel"))==""){
                tkclipboard.clear()
                tkclipboard.append(tclvalue(tkget(riteenv$output, selrange[1], selrange[2])))
                if(docut==TRUE)
                    tkdelete(riteenv$output, selrange[1], selrange[2])
            }
            else {
                selectAll()
                copyText()
            }
        }
        pasteText <- function(){
            if("windows"==.Platform$OS.type)
                cbcontents <- readLines("clipboard")
            else if("unix"==Sys.getenv("OS"))
                cbcontents <- readLines(pipe("pbpaste"))
            else
                cbcontents <- ""
            tkinsert(riteenv$output, "insert", paste(cbcontents,collapse="\n"))
        }
        clearSink <- function() tkdelete(riteenv$output, '1.0', 'end')
        tkbind(riteenv$output, "<Control-L>", expression(clearSink, break))
        tkbind(riteenv$output, "<Control-l>", expression(clearSink, break))
        
        contextMenu <- tkmenu(riteenv$output, tearoff = FALSE)
            tkadd(contextMenu, "command", label = "Clear All <Ctrl-L>",
                command = clearSink)
            tkadd(contextMenu, "separator")
            tkadd(contextMenu, "command", label = "Select All <Ctrl-A>",
                command = selectAll)
            tkadd(contextMenu, "command", label = "Copy <Ctrl-C>",
                command = copyText)
            tkadd(contextMenu, "command", label = "Cut <Ctrl-X>",
                command = function() copyText(docut=TRUE))
            tkadd(contextMenu, "command", label = "Paste <Ctrl-V>",
                command = pasteText)
        rightClick <- function(x, y) {
            rootx <- as.integer(tkwinfo("rootx", riteenv$output))
            rooty <- as.integer(tkwinfo("rooty", riteenv$output))
            xTxt <- as.integer(x) + rootx
            yTxt <- as.integer(y) + rooty
            tkmark.set(riteenv$output,"insert",paste("@",xTxt,",",yTxt,sep=""))
            .Tcl(paste("tk_popup", .Tcl.args(contextMenu, xTxt, yTxt)))
        }
        tkbind(riteenv$output, "<Button-3>", rightClick)
    
    }
    
    options('show.error.messages'=FALSE) # default TRUE
    options('error'=riteenv$errhandler) # default NULL
    
    invisible(NULL)
}

sinkstop <- function(quiet = TRUE){
    # reset options defaults
    options('show.error.messages'=TRUE) # default TRUE
    options('error'=NULL) # default NULL
    options('width'=80) # default 80
    
    # stop sinks
    if(!sink.number()==0)
        sink()
    if(!sink.number('message')==2)
        sink(type='message')
    
    # remove call back
    if('outsink' %in% getTaskCallbackNames())
        removeTaskCallback('outsink')
    
    # close connections
    if(riteenv$otmp %in% showConnections()){
        thiscon <- rownames(showConnections())[which(riteenv$otmp==showConnections()[,1])]
        close(getConnection(thiscon))
    }
    if(riteenv$etmp %in% showConnections()){
        thiscon <- rownames(showConnections())[which(riteenv$etmp==showConnections()[,1])]
        close(getConnection(thiscon))
    }
    
    # remove temporary sink files
    unlink(riteenv$otmp)
    unlink(riteenv$etmp)
    
    # remove `riteenv`
    if(!"thesink" %in% ls(riteenv)){
        #riteenv <- NULL
        #rm(riteenv, envir=asNamespace('package:rite'))
        rm(riteenv, envir=.GlobalEnv)
        if(!quiet)
            message('rite sink closed and removed')
    }
    else if(!quiet)
        message('rite sink closed')
    
    invisible(NULL)
}
