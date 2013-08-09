ritesink <- function(new=TRUE){
	#textcon <- textConnection("thesink","w+")
	#z <- scan(what="character", n=1, quiet=TRUE)
	z <- readLines(n=1)
	#writeLines(z,textcon)
	if(z=="EXIT"){
		#close(textcon)
		invisible()
	}
	else{
		#close(textcon)
		#source(textcon)
		out <- try(withVisible(eval(parse(text=z),envir=.GlobalEnv)))
		if(!inherits(out,"try-error")){
			if(out$visible && !is.null(out$value))
				print(out$value)
		}
		ritesink(new=FALSE)
	}
}

#ritesink()

