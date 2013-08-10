ritesink <- function(..., evalenv=.GlobalEnv, fontFamily="Courier", fontSize=10){
	# setup
	stdsink <- textConnection("osink", "w") # create connection for stderr
	sink(stdsink,"output")
	lengtho <- length(osink)
	
	thesink <- tktoplevel(borderwidth=0)
	exitsink <- function() {
		tkdestroy(thesink)
		if("windows"==.Platform$OS.type)
			bringToTop(-1)
		sink() # end stdsink
		close(stdsink)
		invisible()
	}
	on.exit(exitsink)
	tkwm.protocol(thesink, "WM_DELETE_WINDOW", exitsink)
	tkwm.title(thesink, "rite sink")	# title
	scr <- tkscrollbar(thesink, repeatinterval=25, command=function(...){ tkyview(output,...) })
	output <- tk2ctext(thesink, bg="white", fg="black", undo="true",
							yscrollcommand=function(...) tkset(scr,...),
							font=tkfont.create(family=fontFamily, size=fontSize))
	tcl("wm", "attributes", thesink, topmost=TRUE)
	tkgrid(output, sticky="nsew", column=1, row=1)
	tkgrid(scr, sticky="nsew", column=2, row=1)
	tkgrid.columnconfigure(thesink,1,weight=1)
	tkgrid.columnconfigure(thesink,2,weight=0)
	tkgrid.rowconfigure(thesink,1,weight=1)
	
	tktag.configure(output, "text", foreground="black", underline=0)
	tktag.configure(output, "error", foreground="red", underline=0)
	tktag.configure(output, "warning", foreground="purple", underline=0)
	tktag.configure(output, "message", foreground="blue", underline=0)
	
	internalsink <- function(){
		lengtho <<- length(osink)
		z <- readline(prompt="R> ")
			
		if(z=="EXIT")
			exitsink()
		else{
			out <- tryCatch(withVisible(eval(parse(text=z),envir=evalenv)),
					error = function(e){
						if(any(grepl("unexpected end of input",e$message))){
							invisible("unexpectedEOL")
						}
						else{
							tkinsert(output,"end",paste(e,"\n",collapse=""), ("error"))
							tksee(output,"end")
							invisible("othererror")
						}
					},
					warning = function(w){
						tkinsert(output,"end",paste(w,"\n",collapse=""), ("warning"))
						tksee(output,"end")
					},
					message = function(m){
						tkinsert(output,"end",paste(m,"\n",collapse=""), ("message"))
						tksee(output,"end")
					}
				)
			if(any(grepl("unexpectedEOL",out))){
				complete <- FALSE
				while(!complete){
					z <- paste(z, readline(prompt=" + "),sep="")
					out <- tryCatch(withVisible(eval(parse(text=z),envir=evalenv)),
								error = function(e){
									if(any(grepl("unexpected end of input",e$message))){
										invisible("unexpectedEOL")
									}
									else{
										tkinsert(output,"end",paste(e,"\n",collapse=""), ("error"))
										tksee(output,"end")
										invisible("othererror")
									}
								},
								warning = function(w){
									tkinsert(output,"end",paste(w,"\n",collapse=""), ("warning"))
									tksee(output,"end")
								},
								message = function(m){
									tkinsert(output,"end",paste(m,"\n",collapse=""), ("message"))
									tksee(output,"end")
								}
							)
					if(any(grepl("unexpectedEOL",out)))
						next
					else if(any(grepl("othererror",out)))
						complete <- TRUE
					else if(is.null(names(out))){
						tkinsert(output,"end",paste(out,"\n",collapse=""), ("text"))
						tksee(output,"end")
						complete <- TRUE
					}
					else if(out$visible && !is.null(out$value)){
						tkinsert(output,"end",paste(capture.output(out$value),"\n",collapse=""), ("text"))
						tksee(output,"end")
						complete <- TRUE
					}
				}
			}
			else if(!any(grepl("othererror",out)) && out$visible && !is.null(out$value)){
				tkinsert(output,"end",paste(capture.output(out$value),"\n",collapse=""), ("text"))
				tksee(output,"end")
			}
			if(exists("osink") && length(osink)>lengtho){
				tkinsert(output,"end",paste(paste(osink[(lengtho+1):length(osink)],collapse="\n"),"\n"))
				tksee(output,"end")
			}
			Recall()
		}
	}
	internalsink()
}
