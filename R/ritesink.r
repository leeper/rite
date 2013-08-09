ritesink <- function(..., evalenv=.GlobalEnv, fontFamily="Courier", fontSize=10){
	# setup
	errsink <- textConnection("esink", "w") # create connection for stderr
	
	thesink <- tktoplevel(borderwidth=0)
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
	
	tktag.configure(output, "text", foreground="black", underline=1)
	tktag.configure(output, "error", foreground="red", underline=1)
	tktag.configure(output, "warning", foreground="purple", underline=1)
	tktag.configure(output, "message", foreground="blue", underline=1)
	
	internalsink <- function(){
		z <- readline(prompt="R> ")
	
		if(z=="EXIT")
			invisible()
		else{
			out <- try(withVisible(eval(parse(text=z),envir=evalenv)),silent=TRUE) # convert to tryCatch
			if(!inherits(out,"try-error")){
				if(out$visible && !is.null(out$value)){
					tkinsert(output,"end",paste(capture.output(out$value),"\n",collapse=""))
					tksee(output,"end")
				}
			}
			else if(grepl("unexpected end of input",out)){
				complete <- FALSE
				while(!complete){
					z <- paste(z, readline(prompt=" + "),sep="")
					out <- try(withVisible(eval(parse(text=z),envir=evalenv)),silent=TRUE) # convert to tryCatch
					if(inherits(out,"try-error")){
						if(!grepl("unexpected end of input",out))
							complete <- TRUE
						else{
							tkinsert(output,"end",paste(out,"\n",collapse=""), ("error"))
							tksee(output,"end")
						}
					}
					else{
						complete <- TRUE
						if(out$visible && !is.null(out$value)){
							tkinsert(output,"end",paste(capture.output(out$value),"\n",collapse=""))
							tksee(output,"end")
						}
					}
				}
			}
			else if(inherits(out,"try-error")){
				tkinsert(output,"end",paste(out,"\n",collapse=""), ("error"))
				tksee(output,"end")
			}
			else{
				tkinsert(output,"end",paste(capture.output(out$value),"\n",collapse=""))
				tksee(output,"end")
			}
			Recall()
		}
	}
	internalsink()
}
