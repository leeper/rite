sinkstart <- function(
    echo=TRUE, split=FALSE,
    fontFamily='Courier', fontSize=10,
    col.bg='white', col.call='black', col.result='black',
    col.err='red', col.warn='purple', col.msg='blue')
{
    if(!exists('riteenv'))
        assign('riteenv', new.env(parent = .GlobalEnv), .GlobalEnv)
    riteenv$echo <- echo
    riteenv$split <- split
    
    # sinks
    riteenv$stdsink <- file(riteenv$otmp <- tempfile(),'w+')
    sink(riteenv$stdsink, split=split, type='output') # output
    writeLines('# rite output sink', riteenv$stdsink)
    riteenv$lengtho <- paste(scan(riteenv$stdsink, what='character', sep='\n', quiet=TRUE),collapse='\n')
    
    riteenv$errsink <- file(riteenv$etmp <- tempfile(),'w+')
    sink(riteenv$errsink, type='message') # message
    writeLines('# rite error sink', riteenv$errsink)
    riteenv$lengthe <- paste(scan(riteenv$errsink, what='character', sep='\n', quiet=TRUE),collapse='\n')
    
    # error handler
    riteenv$errhandler <- function()
        tkinsert(riteenv$output,'insert',geterrmessage(), ('error'))
        
    # callback function
    outsink <- function() {
        function(expr, value, ok, visible) {
            e <- as.character(as.expression(expr))
            last <- capture.output(.Last.value)
            if(visible && ok){
                if(riteenv$echo){
                    tkinsert(riteenv$output, 'insert', paste('>',e,'\n'), ('call'))
                    i <- paste(last,'\n',collapse='\n',sep='')
                    tkinsert(riteenv$output, 'insert', i, ('result'))
                }
                else{
                    i <- paste(last,'\n\n',sep='')
                    tkinsert(riteenv$output, 'insert', i, ('result'))
                }
                riteenv$lengtho <- paste(scan(riteenv$otmp, what='character', sep='\n', quiet=TRUE),collapse='\n')
            }
            else if(visible && !ok) # !ok doesn't happen
                tkinsert(riteenv$output,'insert','Error\n', ('error'))
            else if(!visible && !ok) # !ok doesn't happen
                tkinsert(riteenv$output,'insert','Non-printing error\n', ('error'))
            #else
            #    tkinsert(riteenv$output,'insert','ok\n', ('message')) # print confirm 'ok' on non-printing calls
            
            # Output sink (for `cat`)
            osink <- paste(scan(riteenv$otmp, what='character', sep='\n', quiet=TRUE),collapse='\n')
            if(!identical(osink, riteenv$lengtho) && length(osink)>0){
                fromout <- substr(osink,nchar(riteenv$lengtho)+1,nchar(osink))
                if(last=='NULL'){
                    tkinsert(riteenv$output,'insert',   
                        paste(fromout,collapse='\n'), ('result'))
                }
                riteenv$lengtho <- osink
            }
            # Error sink (for `warning` and `message`)
            esink <- paste(scan(riteenv$etmp, what='character', sep='\n', quiet=TRUE),collapse='\n')
            if(!identical(esink, riteenv$lengthe) && length(osink)>0){
                fromerr <- substr(esink,nchar(riteenv$lengthe)+1,nchar(esink))
                if(any(grepl('error',fromerr))){
                    tkinsert(riteenv$output,'insert',
                        paste(fromerr,'\n',collapse='\n'), ('error'))
                }
                else if(any(grepl('Warning',fromerr))){
                    tkinsert(riteenv$output,'insert',
                        paste(fromerr,'\n',collapse=''), ('warning'))
                }
                else{
                    tkinsert(riteenv$output,'insert',
                        paste(fromerr,collapse='\n'), ('message'))
                }
                riteenv$lengthe <- esink
            }
            tksee(riteenv$output, 'insert')
            TRUE
        }
    }
        
    if(!'outsink' %in% getTaskCallbackNames())
        addTaskCallback(outsink(), name='outsink')
    else
        stop('ritesink is already active')
        
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
        
        # tags
        tktag.configure(riteenv$output, 'call', foreground=col.call, underline=0)
        tktag.configure(riteenv$output, 'result', foreground=col.result, underline=0)
        tktag.configure(riteenv$output, 'error',
            foreground=col.err,
            font=tkfont.create(family=fontFamily, size=fontSize, weight='bold'),
            underline=0)
        tktag.configure(riteenv$output, 'warning',
            foreground=col.warn,
            font=tkfont.create(family=fontFamily, size=fontSize, weight='bold'),
            underline=0)
        tktag.configure(riteenv$output, 'message',
            foreground=col.msg,
            font=tkfont.create(family=fontFamily, size=fontSize, weight='bold'),
            underline=0)
        
        # add context menu and key bindings here
        ## clear, copy, cut, paste, etc. on the sink
    }
    
    options('show.error.messages'=FALSE) # default TRUE
    options('error'=riteenv$errhandler) # default NULL
    
    invisible(NULL)
}

sinkstop <- function(quiet = TRUE){
    options('show.error.messages'=TRUE) # default TRUE
    options('error'=NULL) # default NULL
    if(!sink.number()==0)
        sink()
    if(!sink.number('message')==2)
        sink(type='message')
    if('outsink' %in% getTaskCallbackNames())
        removeTaskCallback('outsink')
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
    if(quiet)
        message('rite sink closed')
    invisible(NULL)
}

# 
