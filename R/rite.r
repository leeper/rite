rite <- function(filename=NULL, catchOutput=FALSE, evalenv=.GlobalEnv,
				fontFamily="Courier", fontSize=10, orientation="horizontal",
				highlight="r", color=NULL, autosave=TRUE, echo=FALSE, ...){	
	## STARTUP OPTIONS ##
	filename <- filename # script filename (if loaded or saved)
	scriptSaved <- TRUE # a logical for whether current edit file is saved
	searchterm <- ""
	ritetmpfile <- tempfile(pattern="rite",fileext=".r")
	wmtitle <- packagetitle <- "rite"
	# optionally setup evaluation environment
	if(is.null(evalenv)){
		editenv <- new.env()
		evalenv <- editenv
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
	filetypelist <- paste(	"{{R Script} {.R}}",
							"{{brew} {.brew}}",
							"{{HTML} {.html}}",
							"{{R HTML} {.Rhtml}}",
							"{{Markdown} {.md}}",
							"{{R markdown} {.Rmd}}",
							"{{reST} {.rst}}",
							"{{R reST} {.Rrst}}",
							"{{Sweave} {.Rnw}}",
							"{{TeX} {.tex}}",
							"{{R TeX} {.Rtex}}",
							"{{Text} {.txt}}",
							"{{All files} {*.*}}",
						sep=" ")
	defaultfiletype <- "{{R Script} {.R}}"
	if(!is.null(color)){
		for(i in 1:length(color)){
			if(!is.null(color[[i]]) && !is.na(color[[i]]) && !color[[i]]=="" && is.character(color[[i]]))
				hcolors[[names(color)[i]]] <- color[[i]]
		}
	}
	if(catchOutput){
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
		if(catchOutput){
			if(!outputSaved){
				exit <- tkmessageBox(message = "Do you want to save the output?", icon = "question", type = "yesnocancel", default = "yes")			
				if(tclvalue(exit)=="yes")
					saveOutput()
				else if(tclvalue(exit)=="no"){}
				else{
					tkfocus(txt_edit)
					return()
				}
			}
		}
		if(!scriptSaved){
			exit <- tkmessageBox(message = "Do you want to save the script?", icon = "question", type = "yesnocancel", default = "yes")			
			if(tclvalue(exit)=="yes")
				saveScript()
			else if(tclvalue(exit)=="no"){}
			else{
				tkfocus(txt_edit)
				return()
			}
		}
		else{
			exit <- tkmessageBox(message = "Are you sure you want to close rite?", icon = "question", type = "yesno", default = "yes")
			if(tclvalue(exit)=="yes"){}
			else if(tclvalue(exit)=="no"){
				tkfocus(txt_edit)
				return()
			}
		}
		if(catchOutput){
			sink(NULL, type="output")
			close(outsink)
			sink(NULL, type="message")
			close(errsink)
			close(ritecat)
			cat <<- base::cat
		}
		if("windows"==.Platform$OS.type)
			bringToTop(-1)
		tkdestroy(editor)
		unlink(ritetmpfile)
	}
	
	## FILE MENU FUNCTIONS ##
	newScript <- function(){
		if(!scriptSaved){
			exit <- tkmessageBox(message = "Do you want to save the current script?", icon = "question", type = "yesnocancel", default = "yes")			
			if(tclvalue(exit)=="yes")
				saveScript()
			else if(tclvalue(exit)=="no"){}
			else{
				tkfocus(txt_edit)
				return()
			}
		}
		tkdelete(txt_edit,"0.0","end")
		filename <<- NULL
		scriptSaved <<- TRUE
		wmtitle <<- packagetitle
		tkwm.title(editor, wmtitle)
	}
	loadScript <- function(fname=NULL, locals=TRUE, gist=FALSE){
		if(is.null(fname))
			newScript()
		if(locals==TRUE){
			if(is.null(fname)){
				tkgeto <- tkgetOpenFile(title="Load Script",
										filetypes=filetypelist)
				fname <- tclvalue(tkgeto)
			}
			if(!length(fname) || fname=="")
				invisible()
			chn <- tclopen(fname, "r")
			tkinsert(txt_edit, "end", tclvalue(tclread(chn)))
			tclclose(chn)
			scriptSaved <<- TRUE
			filename <<- fname
			wmtitle <<- paste(filename,"-",packagetitle)
			tkwm.title(editor, wmtitle)
		}
		else{
			processEntry <- function() {
				tkdestroy(gistDialog)
				entry <- tclvalue(entry)
				if(gist){
					# load from gist ( code adapted from httr::source_gist )
					if (is.numeric(entry) || grepl("^[0-9a-f]+$", entry)) {
						entry <- paste("https://raw.github.com/gist/", entry, sep = "")
					}
					else if (grepl("((^https://)|^)gist.github.com/([^/]+/)?[0-9a-f]+$", entry)) {
						entry <- paste("https://raw.github.com/gist/",
									regmatches(entry, regexpr("[0-9a-f]+$", entry)), sep = "")
					}
				}
				content <- try(RCurl::getURL(entry,ssl.verifypeer=FALSE,followlocation=TRUE))
				if(!inherits(content,"try-error")){
					tkinsert(txt_edit, "end", content)
					scriptSaved <<- FALSE
				}
				else
					tkmessageBox(message="Gist not loaded!", icon="error")
			}
			gistDialog <- tktoplevel()
			if(gist)
				tkwm.title(gistDialog, "Enter Gist ID or raw URL")
			else
				tkwm.title(gistDialog, "Enter URL")
			entryform <- tkframe(gistDialog, relief="groove", borderwidth=2)
				entry <- tclVar()
				tkgrid(ttklabel(entryform, text = "     "), row=1)
				urlentry <- tkentry(entryform, width = 50, textvariable=entry)
				if(gist)
					tkgrid(tklabel(entryform, text = "ID/URL: "), row=2, column=1)
				else
					tkgrid(tklabel(entryform, text = "URL: "), row=2, column=1)
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
			tkfocus(gistDialog)
		}
	}
	saveScript <- function(){
		if(is.null(filename) || !length(filename) || filename=="")
			saveAsScript()
		else{
			chn <- tclopen(filename, "w")
			tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
			tclclose(chn)
			scriptSaved <<- TRUE
			wmtitle <<- packagetitle
			wmtitle <<- paste(filename,"-",wmtitle)
			tkwm.title(editor, wmtitle)
		}
	}
	saveAsScript <- function() {
		fname <- tclvalue(tkgetSaveFile(initialdir=getwd(),
										title="Save Script As",
										filetypes=filetypelist))
		if(!length(fname) || fname==""){
			filename <<- ""
			return()
		}
		else{
			chn <- tclopen(fname, "w")
			tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
			tclclose(chn)
			scriptSaved <<- TRUE
			filename <<- fname
			wmtitle <<- packagetitle
			wmtitle <<- paste(fname,"-",wmtitle)
			tkwm.title(editor, wmtitle)
		}
	}
	saveGist <- function(browse=FALSE) {
		gisturl <- "https://api.github.com/gists"
		if(!is.null(filename) && filename=="")
			description <- filename
		else
			description <- "rite script"
		content <- tclvalue(tkget(txt_edit,"0.0","end"))
		#gistbody2 <- RJSONIO::toJSON(list(description="rite script", public="true",
		#					files=list("file1.txt"=list(content=content))),collapse="")
		content <- gsub("\n","\\\\n",content)
		content <- gsub("\t","\\\\t",content)
		content <- gsub("\r","\\\\r",content)
		gistbody <- paste('{',	'"description":"',description,
								'","public":"true"',
								',"files":{"file1.txt":{"content":"',content,'"}}}',sep="")
		gistout <- RCurl::postForm(uri=gisturl, .opts=list(postfields = gistbody,
						followlocation = TRUE, ssl.verifypeer = TRUE, ssl.verifyhost = TRUE,
						cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
						httpheader = c('Content-Type' = 'application/json',
										Accept = 'application/json')))
		outsplit <- strsplit(gistout,'","')[[1]] # parse JSON
		gistid <- strsplit(outsplit[grep("id",outsplit)],':"')[[1]][2]
		gistouturl <- paste("https://gist.github.com/",gistid,sep="")
		results <- paste("Script saved as Gist ",gistid," at: ",gistouturl,sep="")
		if(catchOutput){
			tkconfigure(err_out, state="normal")
			tkinsert(err_out,"end",results)
			tkconfigure(err_out, state="disabled")
			tkselect(nb2, 1)
			tkfocus(txt_edit)
		}
		else
			message(results)
		if(browse)
			browseURL(gistouturl)
	}
	includeScript <- function(locals=TRUE,refonly=FALSE,gist=FALSE){
		if(locals){
			filename <- tclvalue(tkgetOpenFile(	title="Append Script",
												filetypes=filetypelist))
			if (!length(filename) || filename=="")
				return()
			else{
				if(refonly)
					tkinsert(txt_edit, "insert", paste("source(\"",filename,"\")\n",sep=""))
				else{
					chn <- tclopen(filename, "r")
					tkinsert(txt_edit, "insert", tclvalue(tclread(chn)))
					tclclose(chn)
				}
				scriptSaved <<- FALSE
			}
		}
		else{
			processEntry <- function() {
				tkdestroy(gistDialog)
				entry <- tclvalue(entry)
				if(gist) {
					# load from gist ( code adapted from httr::source_gist )
					if (is.numeric(entry) || grepl("^[0-9a-f]+$", entry)) {
						entry <- paste("https://raw.github.com/gist/", entry, sep = "")
					}
					else if (grepl("((^https://)|^)gist.github.com/([^/]+/)?[0-9a-f]+$", entry)) {
						entry <- paste("https://raw.github.com/gist/",
									regmatches(entry, regexpr("[0-9a-f]+$", entry)), sep = "")
					}
				}
				if(refonly){
					if(gist || grepl("https",entry))
						tkinsert(txt_edit, "insert", paste(
							"library(RCurl)\n",
							"writeLines(getURL('",entry,
								"',\n\tssl.verifypeer=FALSE,followlocation=TRUE),\n\ttemp_file <- tempfile())\n",
							"source(temp_file)\n",
							"unlink(temp_file)\n",sep=""))
					else
						tkinsert(txt_edit, "insert", paste("source(\"",entry,"\")\n",sep=""))
				}
				else{
					content <- try(RCurl::getURL(entry,ssl.verifypeer=FALSE,followlocation=TRUE))
					if(!inherits(content,"try-error"))
						tkinsert(txt_edit, "insert", content)
					else
						tkmessageBox(message="Script not loaded!", icon="error")
				}
				scriptSaved <<- FALSE
			}
			gistDialog <- tktoplevel()
			if(gist)
				tkwm.title(gistDialog, "Enter Gist ID or raw URL")
			else
				tkwm.title(gistDialog, "Enter Script URL")
			entryform <- tkframe(gistDialog, relief="groove", borderwidth=2)
				entry <- tclVar()
				tkgrid(ttklabel(entryform, text = "     "), row=1)
				urlentry <- tkentry(entryform, width = 50, textvariable=entry)
				if(gist)
					tkgrid(tklabel(entryform, text = "ID/URL: "), row=2, column=1)
				else
					tkgrid(tklabel(entryform, text = "URL: "), row=2, column=1)
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
			tkfocus(gistDialog)
		}
	}
	
	## RUN FUNCTIONS ##
	runCode <- function(code){
		parsed <- tryparse(verbose=FALSE)
		if(!parsed)
			return()
		search1 <- search()
		length2 <- length(osink)
		runtemp <- tempfile()
		writeLines(code,runtemp)
		writeError <- function(errmsg, type, focus=TRUE){
			tkconfigure(err_out, state="normal")
			if(type=="Error")
				tkinsert(err_out,"end",paste(type,": ",errmsg,"\n",sep=""),("error"))
			else if(type=="Warning")
				tkinsert(err_out,"end",paste(type,": ",errmsg,"\n",sep=""),("warning"))
			else if(type=="Message")
				tkinsert(err_out,"end",paste(type,": ",errmsg,"\n",sep=""),("message"))
			else
				tkinsert(err_out,"end",paste(type,": ",errmsg,"\n",sep=""), ("text"))
			tkconfigure(err_out, state="disabled")
			if(focus){
				tkselect(nb2, 1)
				tkfocus(txt_edit)
			}
		}
		out <- withRestarts(withCallingHandlers(source(runtemp, print.eval=TRUE, echo=echo),
			error = function(errmsg){
				errmsg <- strsplit(as.character(errmsg),": ")[[1]]
				errmsg <- paste(errmsg[-1],collapse=":")
				if(catchOutput)
					writeError(errmsg,"Error")
				#errbox <- tkmessageBox(message = paste("Error:",errmsg,"\nDo you want to continue evaluation?"),
				#						icon = "error", type = "yesno", default = "no")
				#if(tclvalue(errbox)=="no")
				#	invokeRestart("discontinue")
			},
			warning = function(errmsg){
				errmsg <- strsplit(as.character(errmsg),": ")[[1]]
				errmsg <- paste(errmsg[-1],collapse=":")
				if(catchOutput)
						writeError(errmsg,"Warning")
				errbox <- tkmessageBox(message = paste("Warning:",errmsg,"\nDo you want to continue evaluation?"),
										icon = "error", type = "yesno", default = "no")
				if(tclvalue(errbox)=="no")
					invokeRestart("discontinue")
			},
			message = function(errmsg){
				errmsg <- strsplit(as.character(errmsg),": ")[[1]]
				errmsg <- paste(errmsg[-1],collapse=":")
				if(catchOutput)
					writeError(errmsg,"Message")
				else
					cat(errmsg)
			},
			interrupt = function(){
				if(catchOutput)
					writeError("","Interruption")
				else
					tkmessageBox(message="Evaluation interrupted!", icon="error")
			}
		),
		# a restart to 'discontinue' source(.)-ing
		discontinue = function(){invisible()}
		)
		if(catchOutput){
			# output to `output`
			tkconfigure(output, state="normal")
			if(length(osink)>length2){
				tryCatch(tkinsert(output,"end",paste(osink[(length2+1):length(osink)],"\n",collapse="")),
					error = function(e){
						tkmessageBox(message=paste("Printing error:",e,"!"), icon="error")
					},
					interrupt = function(){
						tkmessageBox(message="Printing interrupt!", icon="error")
					}
				)
				tkselect(nb2, 0)
			}
			tkmark.set(output,"insert","end")
			tksee(output,"insert")
			tkconfigure(output, state="disabled")
			outputSaved <<- FALSE
		}
		unlink(runtemp)
		# syntax highlighting for new packages
		search2 <- search()[!search() %in% search1]
		if(length(search2)>0){
			for(i in 1:length(search2)){
				packagename <- strsplit(search2[i],":")[[1]][2]
				funs <- objects(search2[i])
				if(!inherits(funs,"try-error")){
					tmpx <- sort(rep(1:ceiling(length(funs)/30),30))
					tmpsplit <- split(funs,tmpx[1:length(funs)])
					uniqtmp <- sapply(tmpsplit, FUN=function(x) { paste(" [list",paste(x,collapse=" ")," ]") })
					for(j in 1:length(uniqtmp)){
						.Tcl(paste("ctext::addHighlightClass ",.Tk.ID(txt_edit),
									" basefunctions",j," ", hcolors$functions, uniqtmp[j], sep=""))
					}
				}
			}
		}
	}
   runLine <- function(){
		if(autosave & (is.null(filename) || filename=="")){
			chn <- tclopen(ritetmpfile, "w")
			tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
			tclclose(chn)
		}
		else if(autosave & (!is.null(filename) && !filename=="")){
			chn <- tclopen(filename, "w")
			tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
			tclclose(chn)
		}
		code <- tclvalue(tkget(txt_edit, "insert linestart", "insert lineend"))
		if(!code=="")
			runCode(code)
	}
	runSelection <- function(){
		if(autosave & (is.null(filename) || filename=="")){
			chn <- tclopen(ritetmpfile, "w")
			tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
			tclclose(chn)
		}
		else if(autosave & (!is.null(filename) && !filename=="")){
			chn <- tclopen(filename, "w")
			tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
			tclclose(chn)
		}
		if(!tclvalue(tktag.ranges(txt_edit,"sel"))=="")
			runCode(tclvalue(tkget(txt_edit,"sel.first","sel.last")))
	}
	runAll <- function(){
		if(autosave & (is.null(filename) || filename=="")){
			chn <- tclopen(ritetmpfile, "w")
			tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
			tclclose(chn)
		}
		else if(autosave & (!is.null(filename) && !filename=="")){
			chn <- tclopen(filename, "w")
			tclputs(chn, tclvalue(tkget(txt_edit,"0.0","end")))
			tclclose(chn)
		}
		runCode(tclvalue(tkget(txt_edit,"1.0","end")))
	}

	## OUTPUT FUNCTIONS ##
	if(catchOutput){
		saveOutput <- function(outfilename="") {
			if(outfilename=="")
				outfilename <- tclvalue(tkgetSaveFile(	initialdir=getwd(),
														title="Save Output",
														filetypes=filetypelist))
			if(!length(outfilename) || outfilename=="")
				invisible()
			chn <- tclopen(outfilename, "w")
			tkconfigure(output, state="normal")
			tclputs(chn, tclvalue(tkget(output,"0.0","end")))
			tkconfigure(output, state="disabled")
			tclclose(chn)
			invisible(outfilename)
		}
		clearOutput <- function(){
			tkconfigure(output, state="normal")
			tkdelete(output,"0.0","end")
			tkconfigure(output, state="disabled")
			tkselect(nb2, 0)
		}
		clearError <- function(){
			tkconfigure(err_out, state="normal")
			tkdelete(err_out,"0.0","end")
			tkconfigure(err_out, state="disabled")
			tkselect(nb2, 1)
		}
		
		# convert script to .tex or tangles with knitr
		knittxt <- function(genmode="knit", usetxt=TRUE, usefile=FALSE, spinformat=NULL){
			if(!require(knitr)){
				install <- try(install.packages("knitr"), silent=TRUE)
				if(inherits(install, "try-error")){
					tkmessageBox(message="knitr not installed and not installable")
					return()
				}
			}
			if(genmode %in% c("md2html","rmd2html")){
				if(!require(markdown)){
					install <- try(install.packages("markdown"), silent=TRUE)
					if(inherits(install, "try-error")){
						tkmessageBox(message="markdown not installed and not installable")
						return()
					}
				}
			}
			clearError()
			tkconfigure(err_out, state="normal")
			tkinsert(err_out, "end", "Generating report...")
			tkconfigure(err_out, state="disabled")
			ksink1 <- ""
			ksink2 <- ""
			knitsink1 <- textConnection("ksink1", "w") # create connection for stdout
			knitsink2 <- textConnection("ksink2", "w") # create connection for stderr
			sink(knitsink1, type="output") # sink stdout
			sink(knitsink2, type="message") # sink stderr
			
			if(usetxt){
				txtvalue <- tclvalue(tkget(txt_edit,"0.0","end"))
				inputvalue <- NULL
			}
			else if(usefile){
				loadScript()
				txtvalue <- NULL
				inputvalue <- filename
			}
			if(genmode=="knit")
				knit_out <- try(knit(input=inputvalue, text=txtvalue))
			else if(genmode=="purl")
				knit_out <- try(purl(input=inputvalue, text=txtvalue))
			else if(genmode=="sweave"){
				sweave_out <- try(Sweave2knitr(file=inputvalue, text=txtvalue))
				if(inherits(sweave_out, "try-error")){
					tkmessageBox(message="Could not convert Sweave to knitr!")
					return()
				}
				else if(!is.null(inputvalue))
					knit_out <- try(knit(input=gsub("[.]([^.]+)$", "-knitr.\\1", inputvalue), text=txtvalue))
				else if(!is.null(txtvalue))
					knit_out <- try(knit(text=sweave_out))
			}
			else if(genmode=="tangle"){
				sweave_out <- try(Sweave2knitr(file=inputvalue, text=txtvalue))
				if(inherits(sweave_out, "try-error")){
					tkmessageBox(message="Could not convert Sweave to knitr!")
					return()
				}
				else if(!is.null(inputvalue))
					knit_out <- try(purl(input=gsub("[.]([^.]+)$", "-knitr.\\1", inputvalue), text=txtvalue))
				else if(!is.null(txtvalue))
					knit_out <- try(purl(text=sweave_out))
			}
			else if(genmode=="rmd2html"){
				if(!is.null(inputvalue))
					knit_out <- try(knit2html(input=inputvalue))
				else if(!is.null(txtvalue))
					knit_out <- try(knit2html(text=txtvalue))
			}
			else if(genmode=="md2html"){
				if(!is.null(inputvalue))
					knit_out <- try(markdown::markdownToHTML(file=inputvalue))
				else if(!is.null(txtvalue))
					knit_out <- try(markdown::markdownToHTML(text=txtvalue))
			}
			else if(genmode=="md2html.fragment"){
				if(!is.null(inputvalue))
					knit_out <- try(markdown::markdownToHTML(file=inputvalue,fragment.only=TRUE))
				else if(!is.null(txtvalue))
					knit_out <- try(markdown::markdownToHTML(text=txtvalue,fragment.only=TRUE))
			}
			else if(genmode=="stitch.rnw"){
				if(!is.null(inputvalue))
					knit_out <- try(stitch(script=inputvalue))
				else if(!is.null(txtvalue))
					knit_out <- try(stitch(text=txtvalue))
				if(!inherits(knit_out,"try-error"))
					knit_out_pdf <- paste(tools::file_path_sans_ext(knit_out),"pdf",sep=".")
			}
			else if(genmode=="stitch.rhtml"){
				if(!is.null(inputvalue))
					knit_out <- try(stitch_rhtml(script=inputvalue))
				else if(!is.null(txtvalue))
					knit_out <- try(stitch_rhtml(text=txtvalue))
			}
			else if(genmode=="stitch.rmd"){
				if(!is.null(inputvalue))
					knit_out <- try(stitch_rmd(script=inputvalue))
				else if(!is.null(txtvalue))
					knit_out <- try(stitch_rmd(text=txtvalue))
			}
			else if(genmode=="spin"){
				if(!is.null(inputvalue))
					knit_out <- try(spin(hair=inputvalue, text=NULL, knit=FALSE, format=spinformat))
				else if(!is.null(txtvalue))
					knit_out <- try(spin(hair=NULL, text=txtvalue, knit=FALSE, format=spinformat))
			}
			else if(genmode=="spinknit"){
				if(!is.null(inputvalue)){
					knit_out <- try(spin(hair=inputvalue, text=NULL, knit=TRUE, format=spinformat))
				}
				else if(!is.null(txtvalue)){
					knit_out <- try(spin(hair=NULL, text=txtvalue, knit=TRUE, format=spinformat))
				}
			}
			else{
				tkmessageBox(message=paste("Unrecognized report type!",sep=""))
				invisible()
			}
			sink(type="output")
			sink(type="message")
			close(knitsink1)
			close(knitsink2)
			tkselect(nb2, 1)
			tkconfigure(err_out, state="normal")
			tkinsert(err_out, "end", paste(ksink1,collapse="\n"))
			tkinsert(err_out, "end", paste(ksink2,collapse="\n"))
			rm(ksink1) # cleanup
			rm(ksink2) # cleanup
			tkconfigure(err_out, state="disabled")
			sink(errsink, type="message")
			tkfocus(txt_edit)
			if(inherits(knit_out,"try-error")){
				tkconfigure(err_out, state="normal")
				tkinsert(err_out, "end", "Report generation failed!")
				tkconfigure(err_out, state="disabled")
				tkmessageBox(message=paste("Report generation failed:\n",knit_out))
				invisible(knit_out)
			}
			else{
				tkconfigure(err_out, state="normal")
				tkinsert(err_out, "end", "Report finished!")
				tkconfigure(err_out, state="disabled")
				clearOutput()
				tkconfigure(output, state="normal")
				if(usefile || genmode %in% c("stitch.rnw","stitch.rhtml","stitch.rmd")){
					chn <- tclopen(knit_out, "r")
					tkinsert(output, "end", tclvalue(tclread(chn)))
					tclclose(chn)
					if(genmode %in% c("md2html","rmd2html","stitch.rhtml","stitch.rmd"))
						browseURL(knit_out)
					else if(genmode=="stitch.rnw")
						browseURL(knit_out_pdf)
				}
				else if(genmode=="spin")
					tkinsert(output, "end", paste(knit_out,collapse="\n"))
				else
					tkinsert(output, "end", knit_out)
				tkconfigure(output, state="disabled")
				tkselect(nb2, 0)
				tkfocus(txt_edit)
				invisible(knit_out)
			}
		}
		pdffromfile <- function(filetopdf=NULL, texttopdf=FALSE, textype="latex", bibtex=TRUE){
			if(texttopdf){
				if(!scriptSaved)
					saveScript()
				if(filename=="")
					invisible()
				else
					filetopdf <- filename
			}
			else if(is.null(filetopdf))
				filetopdf <- tclvalue(tkgetOpenFile(title="Open File",
													filetypes=filetypelist))
			if(!filetopdf==""){
				if(dirname(filetopdf) %in% c(".",getwd())) {}
				else{
					file.copy(filetopdf,paste(getwd(),basename(filetopdf),sep="/"), overwrite=TRUE)
					filetopdf <- paste(getwd(),basename(filetopdf),sep="/")
				}
				fstem <- substring(basename(filetopdf),1,regexpr("\\.[[:alnum:]]+$",basename(filetopdf))-1)
				fstem <- paste(fstem,".pdf",sep="")
				clearError()
				tkconfigure(err_out, state="normal")
				tkmark.set(err_out, "insert", "end")
				if(textype=="latex")
					tex1 <- system(paste("pdflatex",filetopdf), intern=TRUE)
				else
					tex1 <- system(paste("xelatex",filetopdf), intern=TRUE)
				tkselect(nb2, 1)
				tkfocus(txt_edit)
				tkinsert(err_out, "insert", paste(tex1,collapse="\n"))
				if(is.null(attributes(tex1)$status) && bibtex==TRUE){
					tex2 <- system(paste("bibtex",filetopdf), intern=TRUE)
					tkinsert(err_out, "insert", paste(tex2,collapse="\n"))
					if(is.null(attributes(tex2)$status)){
						tex3 <- system(paste("pdflatex",filetopdf), intern=TRUE)
						tkinsert(err_out, "insert", paste(tex3,collapse="\n"))
						if(is.null(attributes(tex3)$status)){
							tex4 <- system(paste("pdflatex",filetopdf), intern=TRUE)
							tkinsert(err_out, "insert", paste(tex4,collapse="\n"))
						}
					}
				}
				if(fstem %in% list.files()){
					tkinsert(err_out, "insert", "\n\nOpening pdf ",fstem,"...\n\n")
					system2(getOption("pdfviewer"),fstem)
				}
				else
					tkmessageBox(message="PDF not created!", icon="error")
				tkconfigure(err_out, state="disabled")
			}
		}
		knitpdf <- function(textype="latex",...){
			if(!scriptSaved)
				saveScript()
			knit_out <- knittxt(...)
			if(!inherits(knit_out,"try-error"))
				pdffromfile(filetopdf=filename, textype=textype)
		}
	}
	
	## HELP MENU FUNCTIONS ##
	addHighlighting <- function(){
		addHighlight <- function(){
			if(!tclvalue(objectval)=="")
				.Tcl(paste(	"ctext::addHighlightClass ",.Tk.ID(txt_edit),
							" functions ","purple","  [list ",tclvalue(objectval)," ]",sep=""))
			if(!tclvalue(envirval)=="" && paste("package:",tclvalue(envirval),sep="") %in% search()){
				packs <- c(	tclvalue(envirval),
							gsub(" ","",strsplit(packageDescription(tclvalue(envirval), fields="Depends"),",")[[1]]))
				packs <- na.omit(packs)
				for(i in 1:length(packs)){
					funs <- try(paste(	unique(gsub("<-","",
										objects(paste("package:",tclvalue(envirval),sep="")))),collapse=" "), silent=TRUE)
					if(!inherits(funs,"try-error"))
						.Tcl(paste("ctext::addHighlightClass ",.Tk.ID(txt_edit)," ",tclvalue(envirval),
									"functions ","purple","  [list ",funs," ]",sep=""))
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
			tkgrid(tkbutton(buttons, text = " Close ", command = function(){
				tkdestroy(highlightbox); tkfocus(txt_edit)}), row=1, column=2)
		tkgrid(buttons, row=r, column=2)
		r <- r + 1
		tkgrid(ttklabel(highlightbox, text= "     "), row=r, column=2)
		tkfocus(obj.entry)
	}
	about <- function(){
		aboutbox <- tktoplevel()
		tkwm.title(aboutbox, wmtitle)
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
		tkgrid(tkbutton(aboutbox, text = "   OK   ", command = function(){
			tkdestroy(aboutbox); tkfocus(txt_edit)}), row=10, column=2)
		tkgrid(ttklabel(aboutbox, text= "     "), row=11, column=2)
		tkbind(website, "<ButtonPress>", function()
			browseURL("http://www.thomasleeper.com/software.html"))
		tkfocus(aboutbox)
	}
	
	## EDITOR LAYOUT ##
	editor <- tktoplevel(borderwidth=0)
	tkwm.title(editor, wmtitle)	# title
	tkwm.protocol(editor, "WM_DELETE_WINDOW", exitWiz) # regulate exit

	## EDITOR MENUS ##
	menuTop <- tkmenu(editor)           # Create a menu
	tkconfigure(editor, menu = menuTop) # Add it to the 'editor' window
	menuFile <- tkmenu(menuTop, tearoff = FALSE)
		tkadd(menuFile, "command", label="New Script", command=newScript, underline = 0)
		tkadd(menuFile, "command", label="Load Script",
			command=function() loadScript(locals=TRUE), underline = 0)
		tkadd(menuFile, "command", label="Save Script", command=saveScript, underline = 0)
		tkadd(menuFile, "command", label="SaveAs Script", command=saveAsScript, underline = 1)
		tkadd(menuFile, "command", label="Append Script",
			command=function() includeScript(locals=TRUE), underline = 1)
		tkadd(menuFile, "command", label="Insert Script Reference",
			command=function() includeScript(locals=TRUE, refonly=TRUE), underline = 0)
		tkadd(menuFile, "separator")
		menuFileWeb <- tkmenu(menuFile, tearoff = FALSE)
			tkadd(menuFileWeb, "command", label="Load Remote Script",
				command=function() loadScript(locals=FALSE), underline = 0)
			tkadd(menuFileWeb, "command", label="Append Remote Script",
				command=function() includeScript(locals=FALSE, refonly=FALSE), underline = 1)
			tkadd(menuFileWeb, "command", label="Insert Remote Script Reference",
				command=function() includeScript(locals=FALSE,refonly=TRUE), underline = 0)
			tkadd(menuFileWeb, "separator")
			tkadd(menuFileWeb, "command", label="Load Script from Gist",
				command=function() loadScript(locals=FALSE,gist=TRUE), underline = 0)
			tkadd(menuFileWeb, "command", label="Append Script from Gist",
				command=function() includeScript(locals=FALSE,gist=TRUE), underline = 0)
			tkadd(menuFileWeb, "command", label="Insert Gist Reference",
				command=function() includeScript(locals=FALSE,gist=TRUE,refonly=TRUE), underline = 0)
			tkadd(menuFileWeb, "separator")
			tkadd(menuFileWeb, "command", label="Save Script as Gist",
				command=function() saveGist(), underline = 0)
			tkadd(menuFileWeb, "command", label="Save Script as Gist and Open",
				command=function() saveGist(browse=TRUE), underline = 0)
			tkadd(menuFile, "cascade", label = "Remote scripts...", menu = menuFileWeb, underline = 0)
		tkadd(menuFile, "separator")
		tkadd(menuFile, "command", label="Change dir...", command=function(...){
			tkdir <- tclvalue(tkchooseDirectory())
			if(!tkdir=="")
				setwd(tkdir)
			}, underline = 7)
		tkadd(menuFile, "separator")
		tkadd(menuFile, "command", label = "Close rite", command = exitWiz, underline = 0)
		tkadd(menuFile, "separator")
		tkadd(menuFile, "command", label = "Quit R", command = function() {exitWiz(); quit()}, underline = 0)
		tkadd(menuTop, "cascade", label = "File", menu = menuFile, underline = 0)
	menuRun <- tkmenu(menuTop, tearoff = FALSE)
		tkadd(menuRun, "command", label = "Run Line", command = runLine, underline = 4)
		tkadd(menuRun, "command", label = "Run Selection", command = runSelection, underline = 4)
		tkadd(menuRun, "command", label = "Run All", command = runAll, underline = 4)
		tkadd(menuRun, "separator")
		if(catchOutput){
			tkadd(menuRun, "command", label="List all objects", command=function()
				tkinsert(output,"end",capture.output(ls(envir=evalenv))))
			tkadd(menuRun, "command", label="Remove all objects", command=function() {
				check <- tkmessageBox(message = "Are you sure?", icon = "question", type = "yesno", default = "no")
				if(tclvalue(check)=="yes"){
					rm(list=ls(all.names=TRUE,envir=evalenv),envir=evalenv)
					tkmessageBox(message="All objects removed")
				}	})
			tkadd(menuRun, "command", label="List search path", command=function()
				tkinsert(output,"end",capture.output(search())))
			#tkadd(menuRun, "separator")
			#tkadd(menuRun, "command", label="Install package(s)", command=function()
			#	install.packages())
			#tkadd(menuRun, "command", label="Update package(s)", command=function()
			#	update.packages(ask='graphics',checkBuilt=TRUE))
		} else {
			tkadd(menuRun, "command", label="List all objects", command=function()
				print(ls(envir=evalenv)))
			tkadd(menuRun, "command", label="Remove all objects", command=function()
				rm(list=ls(all.names=TRUE,envir=evalenv),envir=evalenv))
			tkadd(menuRun, "command", label="List search path", command=function()
				print(search()))
			tkadd(menuRun, "separator")
			tkadd(menuRun, "command", label="Install package(s)", command=function()
				install.packages())
			tkadd(menuRun, "command", label="Update package(s)", command=function()
				update.packages(ask='graphics',checkBuilt=TRUE))
		}
		#tkadd(menuRun, "separator")
		#tkadd(menuRun, "command", label = "Interrupt", command = function(){pskill(Sys.getpid(),SIGINT) }, underline = 0)
		#tkadd(menuRun, "command", label = "Interrupt", command = function() tkdestroy(txt_edit), underline = 0)
		tkadd(menuTop, "cascade", label = "Run", menu = menuRun, underline = 0)
	if(catchOutput){
		menuOutput <- tkmenu(menuTop, tearoff = FALSE)
			copyOutput <- function(){
				tkconfigure(output, state="normal")
				tkclipboard.clear()
				tkclipboard.append(tclvalue(tkget(output, "0.0", "end")))
				tkconfigure(output, state="disabled")
			}
			tkadd(menuOutput, "command", label = "Copy Output", command = copyOutput, underline = 0)
			tkadd(menuOutput, "command", label = "Save Output",
				command = function() saveOutput(outfilename=""), underline = 0)
			tkadd(menuOutput, "command", label = "Clear Output", command = clearOutput, underline = 1)
			tkadd(menuOutput, "separator")
			copyMessage <- function(){
				tkconfigure(err_out, state="normal")
				tkclipboard.clear()
				tkclipboard.append(tclvalue(tkget(err_out, "0.0", "end")))
				tkconfigure(err_out, state="disabled")
			}
			tkadd(menuOutput, "command", label = "Copy Message", command = copyMessage, underline = 0)
			tkadd(menuOutput, "command", label = "Clear Message", command = clearError, underline = 1)
			tkadd(menuTop, "cascade", label = "Output", menu = menuOutput, underline = 0)
		menuReport <- tkmenu(menuTop, tearoff = FALSE)
			menuKnit <- tkmenu(menuReport, tearoff = FALSE)
				tkadd(menuKnit, "command", label = "knit",
					command = function() knittxt(genmode="knit", usefile=FALSE, usetxt=TRUE), underline = 0)
				tkadd(menuKnit, "command", label = "knit (from Sweave source)",
					command = function() knittxt(genmode="sweave", usefile=FALSE, usetxt=TRUE))
				tkadd(menuKnit, "separator")
				tkadd(menuKnit, "command", label = "knit Rmd to HTML",
					command = function() knittxt(genmode="rmd2html", usefile=FALSE, usetxt=TRUE))
				tkadd(menuKnit, "separator")
				tkadd(menuKnit, "command", label = "knit to pdf",
					command = function() knitpdf(genmode="knit", usefile=FALSE, usetxt=TRUE))
				tkadd(menuKnit, "command", label = "knit to pdf (from Sweave source)",
					command = function() knitpdf(genmode="sweave", usefile=FALSE, usetxt=TRUE))
				tkadd(menuReport, "cascade", label = "Knit", menu = menuKnit, underline = 0)
			menuPurl <- tkmenu(menuReport, tearoff = FALSE)
				tkadd(menuPurl, "command", label = "purl",
					command = function() knittxt(genmode="purl", usefile=FALSE, usetxt=TRUE), underline = 0)
				tkadd(menuPurl, "command", label = "purl (from Sweave source)",
					command = function() knittxt(genmode="tangle", usefile=FALSE, usetxt=TRUE))
				tkadd(menuReport, "cascade", label = "Purl", menu = menuPurl, underline = 0)
			menuStitch <- tkmenu(menuReport, tearoff = FALSE)
				tkadd(menuStitch, "command", label = "stitch (tex)",
					command = function() knittxt(genmode="stitch.rnw", usefile=FALSE, usetxt=TRUE), underline = 0)
				tkadd(menuStitch, "command", label = "stitch (HTML)",
					command = function() knittxt(genmode="stitch.rhtml", usefile=FALSE, usetxt=TRUE))
				tkadd(menuStitch, "command", label = "stitch (markdown)",
					command = function() knittxt(genmode="stitch.rmd", usefile=FALSE, usetxt=TRUE))
				tkadd(menuReport, "cascade", label = "Stitch", menu = menuStitch, underline = 0)
			menuSpin <- tkmenu(menuReport, tearoff = FALSE)
				tkadd(menuSpin, "command", label = "spin to Rmd",
					command = function() knittxt(genmode="spin", usefile=FALSE, usetxt=TRUE, spinformat="Rmd"))
				tkadd(menuSpin, "command", label = "spin to Rnw",
					command = function() knittxt(genmode="spin", usefile=FALSE, usetxt=TRUE, spinformat="Rnw"))
				tkadd(menuSpin, "command", label = "spin to Rhtml",
					command = function() knittxt(genmode="spin", usefile=FALSE, usetxt=TRUE, spinformat="Rhtml"))
				tkadd(menuSpin, "command", label = "spin to Rtex",
					command = function() knittxt(genmode="spin", usefile=FALSE, usetxt=TRUE, spinformat="Rtex"))
				tkadd(menuSpin, "command", label = "spin to Rrst",
					command = function() knittxt(genmode="spin", usefile=FALSE, usetxt=TRUE, spinformat="Rrst"))
				tkadd(menuSpin, "separator")
				tkadd(menuSpin, "command", label = "spin to Rmd and knit",
					command = function() knittxt(genmode="spinknit", usefile=FALSE, usetxt=TRUE, spinformat="Rmd"))
				tkadd(menuSpin, "command", label = "spin to Rnw and knit",
					command = function() knittxt(genmode="spinknit", usefile=FALSE, usetxt=TRUE, spinformat="Rnw"), state="disabled")
				tkadd(menuSpin, "command", label = "spin to Rhtml and knit",
					command = function() knittxt(genmode="spinknit", usefile=FALSE, usetxt=TRUE, spinformat="Rhtml"))
				tkadd(menuSpin, "command", label = "spin to Rtex and knit",
					command = function() knittxt(genmode="spinknit", usefile=FALSE, usetxt=TRUE, spinformat="Rtex"), state="disabled")
				tkadd(menuSpin, "command", label = "spin to Rrst and knit",
					command = function() knittxt(genmode="spinknit", usefile=FALSE, usetxt=TRUE, spinformat="Rrst"))
				tkadd(menuReport, "cascade", label = "Spin", menu = menuSpin)
			tkadd(menuReport, "separator")
			menuMD <- tkmenu(menuReport, tearoff = FALSE)
				tkadd(menuMD, "command", label = "Convert md to HTML",
					command = function() knittxt(genmode="md2html", usefile=FALSE, usetxt=TRUE))
				tkadd(menuMD, "command", label = "Convert md to HTML fragment",
					command = function() knittxt(genmode="md2html.fragment", usefile=FALSE, usetxt=TRUE))
				tkadd(menuMD, "command", label = "knit Rmd to HTML",
					command = function() knittxt(genmode="rmd2html", usefile=FALSE, usetxt=TRUE))
				tkadd(menuMD, "separator")
				# update when slidify is on CRAN
				#tkadd(menuMD, "command", label = "slidify md to HTML",
				#	command = function() knittxt(genmode="slidify", usefile=FALSE, usetxt=TRUE))
				#tkadd(menuMD, "command", label = "knit Rmd and slidify to HTML",
				#	command = function() knittxt(genmode="knit2slidify", usefile=FALSE, usetxt=TRUE))
				tkadd(menuReport, "cascade", label = "Markdown", menu = menuMD, underline = 0)
			tkadd(menuReport, "separator")
			#texstatus <- ifelse(!system("pdflatex -version",show.output.on.console=FALSE), "enabled","disabled")
			menuLatex <- tkmenu(menuReport, tearoff = FALSE)
				tkadd(menuLatex, "command", label = "pdflatex",
					command = function() pdffromfile(texttopdf=TRUE, bibtex=FALSE))
				tkadd(menuLatex, "command", label = "pdflatex+bibtex",
					command = function() pdffromfile(texttopdf=TRUE, bibtex=TRUE))
				tkadd(menuReport, "cascade", label = "LaTeX", menu = menuLatex, underline = 0)
			menuXetex <- tkmenu(menuReport, tearoff = FALSE)
				tkadd(menuXetex, "command", label = "xelatex",
					command = function() pdffromfile(texttopdf=TRUE, textype="xelatex", bibtex=FALSE))
				tkadd(menuXetex, "command", label = "xelatex+bibtex",
					command = function() pdffromfile(texttopdf=TRUE, textype="xelatex", bibtex=TRUE))
				tkadd(menuReport, "cascade", label = "XeLaTeX", menu = menuXetex, underline = 0)
			tkadd(menuReport, "separator")
			menuFromFile <- tkmenu(menuReport, tearoff = FALSE)
				tkadd(menuFromFile, "command", label = "knit",
					command = function() knittxt(genmode="knit", usefile=TRUE, usetxt=FALSE), underline = 0)
				tkadd(menuFromFile, "command", label = "purl",
					command = function() knittxt(genmode="purl", usefile=TRUE, usetxt=FALSE), underline = 0)
				tkadd(menuFromFile, "command", label = "knit Rmd to HTML",
					command = function() knittxt(genmode="rmd2html", usefile=TRUE, usetxt=FALSE))
				tkadd(menuFromFile, "separator")
				tkadd(menuFromFile, "command", label = "stitch (tex)",
					command = function() knittxt(genmode="stitch.rnw", usefile=TRUE, usetxt=FALSE))
				tkadd(menuFromFile, "command", label = "stitch (HTML)",
					command = function() knittxt(genmode="stitch.rhtml", usefile=TRUE, usetxt=FALSE))
				tkadd(menuFromFile, "command", label = "stitch (markdown)",
					command = function() knittxt(genmode="stitch.rmd", usefile=TRUE, usetxt=FALSE))
				tkadd(menuFromFile, "separator")
				tkadd(menuFromFile, "command", label = "spin to Rmd",
					command = function() knittxt(genmode="spin", usefile=TRUE, usetxt=FALSE, spinformat="Rmd"))
				tkadd(menuFromFile, "command", label = "spin to Rnw",
					command = function() knittxt(genmode="spin", usefile=TRUE, usetxt=FALSE, spinformat="Rnw"))
				tkadd(menuFromFile, "command", label = "spin to Rhtml",
					command = function() knittxt(genmode="spin", usefile=TRUE, usetxt=FALSE, spinformat="Rhtml"))
				tkadd(menuFromFile, "command", label = "spin to Rtex",
					command = function() knittxt(genmode="spin", usefile=TRUE, usetxt=FALSE, spinformat="Rtex"))
				tkadd(menuFromFile, "command", label = "spin to Rrst",
					command = function() knittxt(genmode="spin", usefile=TRUE, usetxt=FALSE, spinformat="Rrst"))
				tkadd(menuFromFile, "separator")
				tkadd(menuFromFile, "command", label = "Convert md to HTML",
					command = function() knittxt(genmode="md2html", usefile=TRUE, usetxt=FALSE))
				tkadd(menuFromFile, "command", label = "Convert md to HTML fragment",
					command = function() knittxt(genmode="md2html.fragment", usefile=TRUE, usetxt=FALSE))
				# update when slidify is on CRAN
				#tkadd(menuMD, "command", label = "slidify md to HTML",
				#	command = function() knittxt(genmode="slidify", usefile=TRUE, usetxt=FALSE))
				#tkadd(menuMD, "command", label = "knit Rmd and slidify to HTML",
				#	command = function() knittxt(genmode="knit2slidify", usefile=TRUE, usetxt=FALSE))
				tkadd(menuFromFile, "separator")
				tkadd(menuFromFile, "command", label = "pdflatex",
					command = function() pdffromfile(texttopdf=FALSE, bibtex=FALSE))
				tkadd(menuFromFile, "command", label = "pdflatex+bibtex",
					command = function() pdffromfile(texttopdf=FALSE, bibtex=TRUE))
				tkadd(menuFromFile, "command", label = "xelatex",
					command = function() pdffromfile(texttopdf=FALSE, textype="xelatex", bibtex=FALSE))
				tkadd(menuFromFile, "command", label = "xelatex+bibtex",
					command = function() pdffromfile(texttopdf=FALSE, textype="xelatex", bibtex=TRUE))
				tkadd(menuReport, "cascade", label = "Generate from file...", menu = menuFromFile, underline = 0)
			tkadd(menuTop, "cascade", label = "Report Generation", menu = menuReport, underline = 0)
	}
	menuHelp <- tkmenu(menuTop, tearoff = FALSE)
		tkadd(menuHelp, "command", label = "Add Package Highlighting", command = addHighlighting, underline = 0)
		#tkadd(menuHelp, "separator")
		#tkadd(menuHelp, "command", label = "R language help", underline = 0, command = help.start)
		tkadd(menuHelp, "separator")
		tkadd(menuHelp, "command", label = "rite Documentation", command = function() help(rite))
		tkadd(menuHelp, "command", label = "About rite Script Editor", command = about, underline = 0)
		tkadd(menuTop, "cascade", label = "Help", menu = menuHelp, underline = 0)

	pw <- ttkpanedwindow(editor, orient = orientation)
	nb1 <- tk2notebook(pw, tabs = c("Script")) # left pane
		# script editor
		edit_tab1 <- tk2notetab(nb1, "Script")
		edit_scr <- tkscrollbar(edit_tab1, repeatinterval=25, command=function(...){ tkyview(txt_edit,...) })
		txt_edit <- tk2ctext(edit_tab1, bg=hcolors$background, fg=hcolors$normal, undo="true",
								yscrollcommand=function(...) tkset(edit_scr,...),
								font=tkfont.create(family=fontFamily, size=fontSize))
		editModified <- function(){
			scriptSaved <<- FALSE
			tkwm.title(editor, paste("*",wmtitle))
		}
		tkbind(txt_edit, "<<Modified>>", editModified)
		tkgrid(txt_edit, sticky="nsew", column=1, row=1)
		tkgrid(edit_scr, sticky="nsew", column=2, row=1)
		tkgrid.columnconfigure(edit_tab1,1,weight=1)
		tkgrid.columnconfigure(edit_tab1,2,weight=0)
		tkgrid.rowconfigure(edit_tab1,1,weight=1)		
	# pack left notebook
	tkadd(pw, nb1, weight=1) # left pane

	if(catchOutput){
		nb2 <- tk2notebook(pw, tabs = c("Output", "Message"))#, "Plot")) # right pane
			# output
			out_tab1 <- tk2notetab(nb2, "Output")
			out_scr <- tkscrollbar(out_tab1, repeatinterval=25, command=function(...)tkyview(output,...))
			output <- tktext(out_tab1, height=25, bg="white", font="courier", yscrollcommand=function(...)tkset(out_scr,...),
										font=tkfont.create(family=fontFamily,size=fontSize))
			outModified <- function()
				outputSaved <<- FALSE
			tkbind(output, "<<Modified>>", outModified)
			tkconfigure(output, state="disabled")
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
			tkconfigure(err_out, state="disabled")
			tkgrid(err_out, column=1, row=1, sticky="nsew")
			tkgrid(err_scr, column=2, row=1, sticky="nsew")
			tkgrid.columnconfigure(out_tab2,1,weight=1)
			tkgrid.columnconfigure(out_tab2,2,weight=0)
			tkgrid.rowconfigure(out_tab2,1,weight=1)

			tktag.configure(err_out, "text", foreground="black", underline=0)
			tktag.configure(err_out, "error", foreground="red", underline=0)
			tktag.configure(err_out, "warning", foreground="purple", underline=0)
			tktag.configure(err_out, "message", foreground="blue", underline=0)
	
		# pack right notebook
		tkadd(pw, nb2, weight=1) # right pane
	}
	tkpack(pw, fill="both", expand = "yes") # pack panedwindow to editor

	## KEY BINDINGS ##
	f1 <- function(){
		if(!tclvalue(tktag.ranges(txt_edit,"sel"))=="")
			command <- tclvalue(tkget(txt_edit,"sel.first","sel.last"))
		else {
			command <- tclvalue(tkget(txt_edit, "insert wordstart", "insert wordend"))
			if(command %in% c("","\n","("))
				command <- tclvalue(tkget(txt_edit, "insert-1char wordstart", "insert-1char wordend"))
			if(	command=="." |
				tclvalue(tkget(txt_edit, "insert wordstart-1char", "insert wordstart"))=="." |
				tclvalue(tkget(txt_edit, "insert wordend", "insert wordend+1char"))==".")
				command <- tclvalue(tkget(txt_edit, "insert wordstart-2char wordstart", "insert wordend+2char wordend"))
			command <- gsub("[()=]","",command)
		}
		if(command %in% c("","\n","\t"," ",")","]","}","=",".",",","%"))
			return()
		else if(command %in% c("[","(","*","/","+","-","^","$","{","~"))
			command <- paste("`",command,"`",sep="")
		else
			command <- gsub(" ","",command)
		helpresults <- help(command)
		if(length(helpresults)>0)
			help(command)
		else
			help.search(command)
	}
	tkbind(txt_edit, "<F1>", f1)
	
	commandCompletion <- function(){
		iwordstart <- tclvalue(tkindex(txt_edit,"insert-1char wordstart"))
		iwordend <- tclvalue(tkindex(txt_edit,"insert-1char wordend"))
		sel <- tclvalue(tktag.ranges(txt_edit,"sel"))
		if(!sel=="")
			command <- sel
		else
			command <- tclvalue(tkget(txt_edit, iwordstart, iwordend))
		tkmark.set(txt_edit, "temp", "insert-2char")
		if(command=="(")
			command <- paste(tclvalue(tkget(txt_edit, "temp wordstart", "temp wordend")),"(",sep="")
		else if(command=="$")
			command <- paste(tclvalue(tkget(txt_edit, "temp wordstart", "temp wordend")),"$",sep="")
		if(tclvalue(tkget(txt_edit, "temp wordstart-1char", "temp wordstart"))=="."){
			tkmark.set(txt_edit, "temp", "temp wordstart-2char")
			command <- paste(tclvalue(tkget(txt_edit, "temp wordstart", "temp wordend")),command,sep=".")
		}
		fnlist <- vector(mode="character")
		#if(command %in% c("\n","\t"," ","(",")","[","]","{","}","=",",","*","/","+","-","^","%","$","<",">"))
		#	return()
		if(tclvalue(tkget(txt_edit, "insert linestart", "insert")) %in%
				c("<<","```{r","<!--begin.rcode","..~{r","% begin.rcode")){
			fnlist <- c("eval","echo","results","tidy","cache",
						"fig.width","fig.height","out.width","out.height",
						"include","child","engine")
			insertCommand <- function(x)
				tkinsert(txt_edit, "insert", paste(" ",fnlist[x],"=",sep=""))
			fnContextMenu <- tkmenu(txt_edit, tearoff = FALSE)
		}
		else if(substring(command,nchar(command),nchar(command))=="("){
			fnlist <- try(names(formals(substring(command,1,nchar(command)-1))),silent=TRUE)
			if(!inherits(fnlist,"try-error")) {
				insertCommand <- function(x)
					tkinsert(txt_edit, "insert", paste(fnlist[x],"=",sep=""))
			}
		}
		else if(substring(command,nchar(command),nchar(command))=="$"){
			fnlist <- try(eval(parse(text=paste("objects(",substring(command,1,nchar(command)-1),")",sep=""))),silent=TRUE)
			if(!inherits(fnlist,"try-error")) {
				insertCommand <- function(x)
					tkinsert(txt_edit, "insert", fnlist[x])
			}
		}
		else{
			insertpos <- strsplit(tclvalue(tkindex(txt_edit,"insert")),".", fixed=TRUE)[[1]]
			fnlist <- apropos(paste("^", command,sep=""))
			if(length(fnlist<15))
				fnlist <- unique(c(fnlist, apropos(command)))
			insertCommand <- function(x){
				tkdelete(txt_edit, "temp wordstart", iwordend)
				tkinsert(txt_edit, "insert", fnlist[x])
			}
		}
		if(length(fnlist)>0){
			fnContextMenu <- tkmenu(txt_edit, tearoff = FALSE)
			# conditionally add menu items
			## adding them programmatically failed to work (always added last command)
				if(length(fnlist)>0)
					tkadd(fnContextMenu, "command", label = fnlist[1], command = function() insertCommand(1))
				if(length(fnlist)>1)
					tkadd(fnContextMenu, "command", label = fnlist[2], command = function() insertCommand(2))
				if(length(fnlist)>2)
					tkadd(fnContextMenu, "command", label = fnlist[3], command = function() insertCommand(3))
				if(length(fnlist)>3)
					tkadd(fnContextMenu, "command", label = fnlist[4], command = function() insertCommand(4))
				if(length(fnlist)>4)
					tkadd(fnContextMenu, "command", label = fnlist[5], command = function() insertCommand(5))
				if(length(fnlist)>5)
					tkadd(fnContextMenu, "command", label = fnlist[6], command = function() insertCommand(6))
				if(length(fnlist)>6)
					tkadd(fnContextMenu, "command", label = fnlist[7], command = function() insertCommand(7))
				if(length(fnlist)>7)
					tkadd(fnContextMenu, "command", label = fnlist[8], command = function() insertCommand(8))
				if(length(fnlist)>8)
					tkadd(fnContextMenu, "command", label = fnlist[9], command = function() insertCommand(9))
				if(length(fnlist)>9)
					tkadd(fnContextMenu, "command", label = fnlist[10], command = function() insertCommand(10))
				if(length(fnlist)>10)
					tkadd(fnContextMenu, "command", label = fnlist[11], command = function() insertCommand(11))
				if(length(fnlist)>11)
					tkadd(fnContextMenu, "command", label = fnlist[12], command = function() insertCommand(12))
				if(length(fnlist)>12)
					tkadd(fnContextMenu, "command", label = fnlist[13], command = function() insertCommand(13))
				if(length(fnlist)>13)
					tkadd(fnContextMenu, "command", label = fnlist[14], command = function() insertCommand(14))
				if(length(fnlist)>14)
					tkadd(fnContextMenu, "command", label = fnlist[15], command = function() insertCommand(15))
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
			tkbind(fnContextMenu, "<Shift-Tab>", function() tkunpost(fnContextMenu))
		}
	}
	tkbind(txt_edit, "<Shift-Tab>", commandCompletion)
	tkbind(txt_edit, "<F2>", commandCompletion)
	
	casevar <- tclVar(1)
	regoptvar <- tclVar(0)
	updownvar <- tclVar(1)
	findreplace <- function(){
		startpos <- tclvalue(tkindex(txt_edit,"insert"))
		faillabeltext <- tclVar("")
		findtext <- function(string,startpos){
			searchterm <<- string
			if(string=="")
				return()
			else{
				found <- ""
				if(tclvalue(updownvar)==1){
					ud1 <- "-forwards"
					si1 <- "end"
				}
				else{
					ud1 <- "-backwards"
					si1 <- "0.0"
				}
				if(tclvalue(regoptvar)==0)
					reg1 <- "-exact"
				else
					reg1 <- "-regexp"
				if(tclvalue(casevar)==1)
					case1 <- "-nocase"
				else
					case1 <- ""
				found <- tclvalue(.Tcl(paste(.Tk.ID(txt_edit),"search",ud1,reg1,case1,string,startpos,si1)))
				if(!found==""){
					tkdestroy(searchDialog)
					tktag.add(txt_edit, "sel", found, paste(found," +",nchar(string),"char",sep=""))
					if(tclvalue(updownvar)==1)
						tkmark.set(txt_edit, "insert", paste(found," +",nchar(string),"char",sep=""))
					else
						tkmark.set(txt_edit, "insert", found)
				}
				else
					tclvalue(faillabeltext) <- "Text not found"
			}
		}
		replacetxt <- function(){
			# delete selection, if present
			# insert find text
			# find-next
		}
		if(searchterm=="")
			findval <- tclVar("")
		else
			findval <- tclVar(searchterm)
		searchDialog <- tktoplevel()
		tcl("wm", "attributes", searchDialog, topmost=TRUE)
		search1 <- function()
			tcl("wm", "attributes", searchDialog, alpha="1.0")
		searchTrans <- function()
			tcl("wm", "attributes", searchDialog, alpha="0.4")
		tkbind(searchDialog, "<FocusIn>", search1)
		tkbind(searchDialog, "<FocusOut>", searchTrans)
		tkwm.title(searchDialog, paste("Search", sep=""))	# title
		entryform <- tkframe(searchDialog, relief="groove", borderwidth=2)
			find.entry <- tkentry(entryform, width = 40, textvariable=findval)
			#replace.entry <- tkentry(entryform, width = 40, textvariable=replaceval)
			# grid
			tkgrid(tklabel(entryform, text = "   "), row=1, column=1, sticky="nsew")
			tkgrid(tklabel(entryform, text = "Find:   "), row=2, column=1, sticky="nsew")
			tkgrid(find.entry, row=2, column=2)
			tkgrid.configure(find.entry, sticky="nsew")
			#tkgrid(tklabel(entryform, text = "Replace:"), row=3, column=1, sticky="nsew")
			#tkgrid(replace.entry, row=3, column=2)
			#tkgrid.configure(replace.entry, sticky="nsew")
			tkgrid(tklabel(entryform, text = "   "), row=4, column=3)
			regform <- tkframe(entryform)
				regexopt <- tkcheckbutton(regform, variable=regoptvar)
				tkgrid(relabel <- tklabel(regform, text = "Use RegExp:   "), row=1, column=1, sticky="nsew")
				tkgrid(regexopt, row=1, column=2, sticky="nsew")
				tkbind(relabel, "<Button-3>", function() browseURL("http://www.tcl.tk/man/tcl8.4/TclCmd/re_syntax.htm"))
			tkgrid(regform, row=5, column=2, sticky="nsew")
			caseform <- tkframe(entryform)
				caseopt <- tkcheckbutton(caseform, variable=casevar)
				tkgrid(tklabel(caseform, text = "Ignore case? "), row=1, column=1, sticky="nsew")
				tkgrid(caseopt, row=1, column=2, sticky="nsew")	
			tkgrid(caseform, row=6, column=2, sticky="nsew")
			searchoptions <- tkframe(entryform)
				updown.up <- tkradiobutton(searchoptions, variable=updownvar, value=0)
				updown.down <- tkradiobutton(searchoptions, variable=updownvar, value=1)
				tkgrid(	tklabel(searchoptions, text = "Direction:   "),
						updown.up, 
						tklabel(searchoptions, text = "Up"), 
						updown.down,
						tklabel(searchoptions, text = "Down") )
			tkgrid(searchoptions, row=7, column=2, sticky="nsew")
			tkgrid.columnconfigure(entryform,1,weight=6)
			tkgrid.columnconfigure(entryform,2,weight=10)
			tkgrid.columnconfigure(entryform,3,weight=2)
		tkgrid(entryform, row=1, column=2)
		tkgrid.configure(entryform, sticky="nsew")
		buttons <- tkframe(searchDialog)
			# buttons
			Findbutton <- tkbutton(buttons, text = " Find Next ", width=12, command = function() findtext(tclvalue(findval),startpos))
			#Replacebutton <- tkbutton(buttons, text = "  Replace  ", width=12, command = function() replacetext(tclvalue(replaceval)))
			Cancelbutton <- tkbutton(buttons, text = "     Close     ", width=12, command = function(){ tkdestroy(searchDialog); tkfocus(txt_edit) } )
			tkgrid(tklabel(buttons, text = "        "), row=1, column=1)
			tkgrid(Findbutton, row=2, column=2)
			faillabel <- tklabel(buttons, text=tclvalue(faillabeltext), foreground="red")
			tkconfigure(faillabel,textvariable=faillabeltext)
			tkgrid(faillabel, row=3, column=1, columnspan=3)
			#tkgrid(Replacebutton, row=3, column=2)
			tkgrid(tklabel(buttons, text = "        "), row=4, column=2)
			tkgrid(Cancelbutton, row=5, column=2)
			tkgrid(tklabel(buttons, text = "        "), row=6, column=3)
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
	
	gotoline <- function(){
		jump <- function(){
			lineval <- tclvalue(lineval)
			if(!lineval=="")
				tkmark.set(txt_edit,"insert",paste(lineval,".0",sep=""))
			tkdestroy(goDialog)
			tksee(txt_edit,"insert")
		}
		goDialog <- tktoplevel()
		tkwm.title(goDialog, paste("Go to line",sep=""))	# title
		entryform <- tkframe(goDialog, relief="groove", borderwidth=2)
			lineval <- tclVar("")
			line.entry <- tkentry(goDialog, width = 5, textvariable=lineval)
			gobutton <- tkbutton(entryform, text = " Go ", command = jump)
			tkgrid(tklabel(entryform, text = "    Line: "), line.entry, gobutton)
			tkbind(line.entry, "<Return>", jump)
		tkgrid(entryform)
		tkfocus(line.entry)
	}
	tkbind(txt_edit, "<Control-G>", gotoline)
	tkbind(txt_edit, "<Control-g>", gotoline)
	
	tryparse <- function(verbose=TRUE){
		sel <- tclvalue(tktag.ranges(txt_edit,"sel"))
		if(!sel=="")
			e <- try(parse(text=tclvalue(tkget(txt_edit,"sel.first","sel.last"))), silent=TRUE)
		else
			e <- try(parse(text=tclvalue(tkget(txt_edit,"1.0","end"))), silent=TRUE)
		if(inherits(e, "try-error")) {
			e <- strsplit(e,"<text>")[[1]][2]
			if(!sel=="")
				linen <- paste(	(as.numeric(strsplit(e,":")[[1]][2]) + as.numeric(strsplit(sel,"[.]")[[1]][1]) - 1), 
								(as.numeric(strsplit(e,":")[[1]][3])-1), sep=".")
			else
				linen <- paste(	strsplit(e,":")[[1]][2], strsplit(e,":")[[1]][3], sep=".")
			content <- strsplit(e,":")[[1]]
			tktag.add(txt_edit,"sel",paste(linen,"linestart"),paste(linen,"lineend"))
			tkmark.set(txt_edit,"insert",paste(linen,"-1char",sep=""))
			cat("\a")
			invisible(FALSE)
		}
		else{
			if(verbose==TRUE){
				tkmessageBox(message="No syntax errors found")
				tkfocus(txt_edit)
			}
			invisible(TRUE)
		}
	}
	tkbind(txt_edit, "<F7>", tryparse)
	
	runkey <- function() {
		if(!tclvalue(tktag.ranges(txt_edit,"sel"))=="")
			runCode(tclvalue(tkget(txt_edit,"sel.first","sel.last")))
		else
			runLine()
	}
	tkbind(txt_edit, "<Control-r>", runkey)
	tkbind(txt_edit, "<Control-R>", runkey)
	tkbind(txt_edit, "<F8>", runAll)
	tkbind(txt_edit, "<Control-Return>", expression(runkey, break))
	
	tkbind(txt_edit, "<Control-s>", saveScript)
	tkbind(txt_edit, "<Control-S>", saveScript)
	
	tkbind(txt_edit, "<Control-o>", expression(loadScript(fname=NULL), break))
	tkbind(txt_edit, "<Control-O>", expression(loadScript(fname=NULL), break))
	
	if(catchOutput){
		tkbind(txt_edit, "<Control-l>", clearOutput)
		tkbind(output, "<Control-l>", clearOutput)
		tkbind(txt_edit, "<Control-L>", clearOutput)
		tkbind(output, "<Control-L>", clearOutput)
	}
	else{
	    tkbind(txt_edit, "<Control-l>", function() {cat(rep("\n",50),collapse="")})
	    tkbind(txt_edit, "<Control-L>", function() {cat(rep("\n",50),collapse="")})
	}
	
	toggleComment <- function(){
		checkandtoggle <- function(pos){
			check <- tclvalue(tkget(txt_edit, pos, paste(pos,"+2char",sep="")))
			if(check=="# ")
				tkdelete(txt_edit, pos, paste(pos,"+2char",sep=""))
			else if(substring(check,1,1)=="#")
				tkdelete(txt_edit, pos, paste(pos,"+1char",sep=""))
			else{
				tkmark.set(txt_edit,"insert",pos)
				tkinsert(txt_edit, "insert", "# ")
			}
		}
		selrange <- tclvalue(tktag.ranges(txt_edit,"sel"))
		if(!selrange==""){
			selrange <- round(as.numeric(strsplit(selrange," ")[[1]]),0)
			for(i in selrange[1]:(selrange[2]-1))
				checkandtoggle(paste(i,".0 linestart",sep=""))
		}
		else
			checkandtoggle("insert linestart")
	}
	tkbind(txt_edit, "<Control-k>", expression(toggleComment, break))
	tkbind(txt_edit, "<Control-k>", expression(toggleComment, break))
	
	multitab <- function(){
		insertpos <- strsplit(tclvalue(tkindex(txt_edit,"insert")),".", fixed=TRUE)[[1]]
		insertpos2 <- paste(insertpos[1],".",as.numeric(insertpos[2])+1,sep="")
		selrange <- tclvalue(tktag.ranges(txt_edit,"sel"))
		if(selrange=="")
			tkinsert(txt_edit, paste(insertpos[1],".0",sep=""), "\t")
		else{
			selrange <- floor(as.numeric(strsplit(selrange," ")[[1]]))
			if(selrange[1]==selrange[2])
				tkinsert(txt_edit, paste(selrange[1],".0 linestart"), "\t")
			else{
				for(i in selrange[1]:selrange[2])
					tkinsert(txt_edit, paste(i,".0 linestart",sep=""), "\t")
			}
		}
		tkmark.set(txt_edit, "insert", insertpos2)
	}
	multiuntab <- function(){
		insertpos <- strsplit(tclvalue(tkindex(txt_edit,"insert")),".", fixed=TRUE)[[1]]
		insertpos2 <- paste(insertpos[1],".",as.numeric(insertpos[2])-1,sep="")
		selrange <- tclvalue(tktag.ranges(txt_edit,"sel"))
		if(!selrange==""){
			selrange <- round(as.numeric(strsplit(selrange," ")[[1]]),0)
			for(i in selrange[1]:selrange[2]){
				pos <- paste(i,".0 linestart",sep="")
				check <- tclvalue(tkget(txt_edit, pos, paste(pos,"+1char",sep="")))
				if(check=="\t")
					tkdelete(txt_edit, pos, paste(pos,"+1char",sep=""))
			}
			tkmark.set(txt_edit, "insert", insertpos2)
		}
		else{
			check <- tclvalue(tkget(txt_edit, "insert linestart", "insert linestart+1char"))
			if(check=="\t"){
				tkmark.set(txt_edit, "insert", "insert linestart")
				tkdelete(txt_edit, "insert linestart", "insert linestart+1char")
				tkmark.set(txt_edit, "insert", insertpos2)
			}
		}
	}
	tkbind(txt_edit, "<Control-i>", expression(multitab, break))
	tkbind(txt_edit, "<Control-I>", expression(multitab, break))
	tkbind(txt_edit, "<Control-u>", multiuntab)
	tkbind(txt_edit, "<Control-U>", multiuntab)
	
	tabreturn <- function(){
		# detect tab(s)
		tab1 <- tclvalue(tkget(txt_edit, "insert linestart", "insert linestart+1char"))
		tabs <- 0
		if(tab1=="\t"){
			tabs <- tabs + 1
			more <- TRUE
			while(more){
				tab2 <- tclvalue(tkget(txt_edit, 	paste("insert linestart+",tabs,"char",sep=""),
													paste("insert linestart+",tabs+1,"char",sep="")))
				if(tab2=="\t")
					tabs <- tabs + 1
				else
					more <- FALSE
			}
		}
		tkinsert(txt_edit, "insert ", paste("\n",paste(rep("\t",tabs),collapse=""),sep=""))
		tksee(txt_edit, "insert")
	}
	tkbind(txt_edit, "<Return>", expression(tabreturn, break))
	
	### CONTEXT MENU ###
	selectAllEdit <- function(){
		tktag.add(txt_edit,"sel","0.0","end")
		tkmark.set(txt_edit,"insert","end")
	}
	tkbind(txt_edit, "<Control-A>", expression(selectAllEdit, break))
	tkbind(txt_edit, "<Control-a>", expression(selectAllEdit, break))
	
	copyText <- function(docut=FALSE){
		selrange <- strsplit(tclvalue(tktag.ranges(txt_edit,"sel"))," ")[[1]]
		if(!tclvalue(tktag.ranges(txt_edit,"sel"))==""){
			tkclipboard.clear()
			tkclipboard.append(tclvalue(tkget(txt_edit, selrange[1], selrange[2])))
			if(docut==TRUE)
				tkdelete(txt_edit, selrange[1], selrange[2])
		}
		else
			cat("\a")
	}
	pasteText <- function(){
		if("windows"==.Platform$OS.type)
			cbcontents <- readLines("clipboard")
		else if("unix"==Sys.getenv("OS"))
			cbcontents <- readLines(pipe("pbpaste"))
		else
			cbcontents <- ""
		tkinsert(txt_edit, "insert", paste(cbcontents,collapse="\n"))
	}
	
	editcase <- function(type){
		if(!tclvalue(tktag.ranges(txt_edit,"sel"))==""){
			seltxt <- tclvalue(tkget(txt_edit,"sel.first","sel.last"))
			if(type=="toupper")
				seltxt <- toupper(seltxt)
			else
				seltxt <- tolower(seltxt)
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
	
	## SYNTAX HIGHLIGHTING RULES ##
	# latex
	if("latex" %in% highlight){
		# a macro without any brackets
		.Tcl(paste('ctext::addHighlightClassForRegexp ',.Tk.ID(txt_edit),' latex1 ',hcolors$latexmacros,' {\\\\[[:alnum:]|[:punct:]]+}',sep=''))
		# a macro with following brackets (and optionally [] brackets)
		.Tcl(paste('ctext::addHighlightClassForRegexp ',.Tk.ID(txt_edit),
			' latex3 ',hcolors$latexmacros,' {\\\\[[:alnum:]|[:punct:]]+\\[[[:alnum:]*|[:punct:]*|[:space:]*|=*]*\\]\\{[[:alnum:]*|[:punct:]*|[:space:]*]*\\}}',
			sep=''))
		# a macro with preceding brackets
		.Tcl(paste('ctext::addHighlightClassForRegexp ',
			.Tk.ID(txt_edit), ' latex4 ',hcolors$latexmacros,
			' {\\{\\\\[[:alnum:]|[:punct:]]*[[:space:]]*[[:alnum:]|[:punct:]|[:space:]]*\\}}',sep=''))
		# comments
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," latexcomments ",hcolors$latexcomments,
			" {(^%[^%[:alnum:]?[:punct:]?].+|[^%[:alnum:]?[:punct:]?]%.+)}",sep=""))
		## AMEND ABOVE TO DEAL WITH %*% %in% type constructions
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rnwchunk1a ', hcolors$rnwchunk,
			' {<{2}[[:alnum:]?|[:punct:]?|[:space:]?|=?]*>{2}}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rnwchunk1b ', hcolors$rnwchunk, ' @', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rnwchunk2 ', hcolors$rnwchunk, ' \\\\Sexpr\\{.?\\}', sep=''))
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," texchunks1 ",hcolors$rtexchunks,
			" {%% begin.rcode.?}",sep=""))
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," texchunks2 ",hcolors$rtexchunks,
			" {%% end.rcode.?}",sep=""))
		# equations
		#.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' latexeq ', hcolors$latexequations, ' \\${.+}\\$', sep=''))
	}
	# markdown
	if("markdown" %in% highlight){
		message("Highlighting for markdown is only minimally supported")
		# something for the various kinds of markdown syntax
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdheader1 ', hcolors$rmd, ' =+', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdheader2 ', hcolors$rmd, ' -+', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdheader3 ', hcolors$rmd, ' {#{1,6} *.+ *#{0,6}$}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdlist1 ', hcolors$rmd, ' {^ *[-] .+$}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdlist2 ', hcolors$rmd, ' {^ *[+] .+$}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdlist3 ', hcolors$rmd, ' {^ *[*] .+$}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdlist4 ', hcolors$rmd, ' {[0-9]+[.] .+$}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdquote ', hcolors$rmd, ' {^ *[>].+$}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdlink ', hcolors$rmd, ' {\\[.+\\]\\(.+\\)}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdcode ', hcolors$rmd, ' {`[^r].+`}', sep=''))
		# code chunks of the form ```{} ... ```
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdchunk1 ', hcolors$rmdchunks, ' `{3}\\{r.+\\}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdchunk2 ', hcolors$rmdchunks, ' `{3}', sep=''))
		# inline code chunks of the form `r ...`
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rmdchunk3 ', hcolors$rmdchunks, ' {`r .+`}', sep=''))
	}
	# html
	if("xml" %in% highlight){
		# xml/html tags <...>, </...>, and <.../>
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' xml1 ', hcolors$xml,
			' {</?[[:alnum:]]*(\\s+[[:alnum:]]+=(\\\'|")?\\w*(\\\'|")?)*\\s*/?>}', sep=''))
		# xml/html comments
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' xml2 ', hcolors$xmlcomments,
			' {<!{1}-{2}.*(\\s+[[:alnum:]]+=(\\\'|")?\\w*(\\\'|")?)*\\s*-{2}>}', sep=''))
	}
	# roxygen
	if("roxygen" %in% highlight){
		# comments
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," comments ",hcolors$rcomments," {#[^\n\r]*}",sep=""))
		# text
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," roxygen1 ",hcolors$roxygentext," {#'[^\n\r]*}",sep=""))
		# chunks
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," roxygen2a ",hcolors$roxygenchunks," {#[+|-][^\n\r]*}",sep=""))
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," roxygen2b ",hcolors$roxygenchunks," {# (@knitr)[^\n\r]*}",sep=""))
	}
	# brew
	if("brew" %in% highlight){
		# chunks
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' brew1a ', hcolors$brewchunks, ' <%.+%>', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' brew1b ', hcolors$brewchunks, ' <%=.+%>', sep=''))
		# comments
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' brew2 ', hcolors$brewcomments, ' <%#.+%>', sep=''))
		# template
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' brew3 ', hcolors$brewtemplate, ' <%%.+%%>', sep=''))
	}
	# reST
	if("rest" %in% highlight){
		# chunks
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rest1 ', hcolors$restchunks, ' {[.]{2} \\{r.+\\}}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rest2 ', hcolors$restchunks, ' {[.]{2} [.]{2}}', sep=''))
		.Tcl(paste('ctext::addHighlightClassForRegexp ', .Tk.ID(txt_edit), ' rest3 ', hcolors$restchunks, ' {:r:`.+`.}', sep=''))
	}
	# r
	if("r" %in% highlight){
		# functions
		HLfuns <- lapply(search(),FUN=function(x) { paste(unique(gsub("<-","",objects(x))),collapse=" ") })
		uniq <- sort(unique(unlist(lapply(search(), FUN=function(x) {strsplit(gsub("<-","",objects(x)),".",fixed=TRUE)} ))))
		uniq <- uniq[grep("abbreviate",uniq):length(uniq)]
		tmpx <- sort(rep(1:ceiling(length(uniq)/30),30))
		tmpsplit <- split(uniq,tmpx[1:length(uniq)])
		uniqtmp <- sapply(tmpsplit, FUN=function(x) { paste(" [list",paste(x,collapse=" ")," ]") })
		for(j in 1:length(uniqtmp)){
			.Tcl(paste("ctext::addHighlightClass ",.Tk.ID(txt_edit),
						" basefunctions",j," ", hcolors$functions, uniqtmp[j], sep=""))
		}
		rm(HLfuns,uniq,tmpx,tmpsplit,uniqtmp)
		# operators
		.Tcl(paste("ctext::addHighlightClass ",.Tk.ID(txt_edit)," specials ",hcolors$operators,"  [list TRUE FALSE NULL NA if else ]",sep=""))
		.Tcl(paste("ctext::addHighlightClassForSpecialChars ",.Tk.ID(txt_edit)," operators ",hcolors$operators," {@-+!~?:;*/^<>=&|$,.}",sep=""))
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," percoperators ",hcolors$operators," {%[[:alnum:][:punct:]]+%}",sep=""))
		# brackets
		.Tcl(paste("ctext::addHighlightClassForSpecialChars ",.Tk.ID(txt_edit)," brackets ",hcolors$brackets," {[]{}()}",sep=""))
		# floating point numbers
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," digits ",hcolors$digits," {\\m[-+]?[0-9]*\\.?[0-9]+\\M}",sep=""))
		# numbers before letters
		#.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," digits2 ",hcolors$normal," {\\d+[A-Za-z]+[:space:]?}",sep=""))
		# character
		.Tcl(paste('ctext::addHighlightClassForRegexp ',.Tk.ID(txt_edit),' character1 ',hcolors$characters,' {"(?:[^\\"]|\\.)*"}',sep=""))
		.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," character2 ",hcolors$characters," {'(?:[^\\']|\\.)*'}",sep=""))
		# comments
		if(!"roxygen" %in% highlight)
			.Tcl(paste("ctext::addHighlightClassForRegexp ",.Tk.ID(txt_edit)," comments ",hcolors$rcomments," {#[^\n\r]*}",sep=""))
	}
	
	## DISPLAY EDITOR ##
	if(!is.null(filename))
		loadScript(fname=filename)
	tkmark.set(txt_edit,"insert","1.0")
	tkfocus(txt_edit)
	tksee(txt_edit, "insert")
	if(catchOutput && "windows"==.Platform$OS.type){
		tcl("wm", "state", editor, "zoomed")
		#tcl("wm", "attributes", editor, "fullscreen")
	}
}

riteout <- function(...)
	rite(catchOutput=TRUE,...)

if(getRversion() >= "2.15.1")
	utils::globalVariables(c("osink", "riteoutcon"))
