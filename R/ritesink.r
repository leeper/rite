ritesink <- function(...,evalenv=.GlobalEnv){
	#if(newsink)
		# trigger ctext output window on first load
	z <- readline(prompt="R> ")
	if(z=="EXIT")
		invisible()
	else{
		out <- try(withVisible(eval(parse(text=z),envir=evalenv)),silent=TRUE)
		if(!inherits(out,"try-error")){
			if(out$visible && !is.null(out$value))
				print(out$value)
		}
		else if(grepl("unexpected end of input",out)){
			complete <- FALSE
			while(!complete){
				z <- paste(z, readline(prompt=" + "),sep="")
				out <- try(withVisible(eval(parse(text=z),envir=evalenv)),silent=TRUE)
				if(inherits(out,"try-error")){
					if(!grepl("unexpected end of input",out))
						complete <- TRUE
					else
						cat(out) # convert to tk output
				}
				else{
					complete <- TRUE
					if(out$visible && !is.null(out$value))
						print(out$value) # convert to tk output
				}
			}
		}
		else if(inherits(out,"try-error"))
			cat(out) # convert to tk output
		else
			cat(out$value) # convert to tk output
		ritesink(newsink=FALSE)
	}
}

#ritesink()

