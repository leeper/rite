riteenv <- new.env()


checkbrackets <- function(txtedit, direction='right'){
    insertpos <- tclvalue(tkindex(txtedit,"insert"))
    lastchar <- switch(direction, right=tclvalue(tkget(txtedit, "insert", "insert+1char")),
                                  left=tclvalue(tkget(txtedit, "insert-1char", "insert")))
    if(lastchar %in% c('{','[','(')){
        check <- switch(lastchar, `{`='\\}', `[`='\\]', `(`=')')
        lastchar <- switch(lastchar, `{`='\\{', `[`='\\[', `(`='(')
        startpos <- switch(direction, right='insert+1char', left='insert')
        counter <- 1
        while(counter > 0){
            foundcheck <- lastcheck <- tclvalue(.Tcl(paste(.Tk.ID(txtedit),"search",
                                                "-forwards",check,startpos,"end")))
            if(foundcheck=='')
                counter <- 0
            else{
                counter2 <- TRUE
                while(counter2){
                    foundbracket <- tclvalue(.Tcl(paste(.Tk.ID(txtedit),"search",
                                            "-forwards",lastchar,startpos,foundcheck)))
                    if(foundbracket=='')
                        counter2 <- FALSE
                    else {
                        counter <- counter+1
                        startpos <- paste(foundbracket,'+1char',sep='')
                    }
                }
                counter <- counter-1
                startpos <- paste(foundcheck,'+1char',sep='')
            }
        }
        if(lastcheck=='')
            return()
        else{
            tktag.add(txtedit,'tmpbracketclose',lastcheck,paste(lastcheck,'+1char'))
            tktag.raise(txtedit,'tmpbracketclose','brackets')
            if(direction=='right')
                tktag.add(txtedit,'tmpbracketclose',insertpos,paste(insertpos,'+1char'))
            else if(direction=='left')
                tktag.add(txtedit,'tmpbracketclose',paste(insertpos,'-1char'),insertpos)
        }
    }
    if(lastchar %in% c('}',']',')')){
        check <- switch(lastchar, `}`='\\{', `]`='\\[', `)`='(')
        lastchar <- switch(lastchar, `}`='\\}', `]`='\\[', `)`=')')
        startpos <- switch(direction, right='insert', left='insert-1char')
        counter <- 1
        while(counter > 0){
            foundcheck <- lastcheck <- tclvalue(.Tcl(paste(.Tk.ID(txtedit),"search",
                                        "-backwards",check,startpos,"1.0")))
            if(foundcheck=='')
                counter <- 0
            else{
                counter2 <- TRUE
                while(counter2){
                    foundbracket <- tclvalue(.Tcl(paste(.Tk.ID(txtedit),"search",
                                        "-backwards",lastchar,startpos,foundcheck)))
                    if(foundbracket=='')
                        counter2 <- FALSE
                    else {
                        counter <- counter+1
                        startpos <- foundbracket
                    }
                }
                counter <- counter-1
                startpos <- foundcheck
            }
        }
        if(lastcheck=='')
            return()
        else{
            tktag.add(txtedit,'tmpbracketclose',lastcheck,paste(lastcheck,'+1char'))
            if(direction=='right')
                tktag.add(txtedit,'tmpbracketclose',insertpos,paste(insertpos,'+1char'))
            else if(direction=='left')
                tktag.add(txtedit,'tmpbracketclose',paste(insertpos,'-1char'),insertpos)
        }
    }
}

editkeypress <- function(txtedit, direction = 'right'){
    tktag.remove(txtedit,'tmpbracketclose', '1.0', 'end')
    checkbrackets(txtedit, direction)
}

editModified <- function(txtedit){
    scriptSaved <<- FALSE
    tkwm.title(riteenv$editor, paste("*",riteenv$wmtitle))
    editkeypress(txtedit)
}

highlight <- function(txtedit, highlight, hcolors) {
    ## SYNTAX HIGHLIGHTING RULES ##
    hl <- function(type, name = NULL, color, pattern){
        if(is.null(name))
            name <- paste('hl',paste(sample(letters,6,TRUE),collapse=''), sep='')
        if(type=='regexp') {
            .Tcl(paste('ctext::addHighlightClassForRegexp ',.Tk.ID(txtedit), name, color, pattern))
            return()
        } else if(type=='class'){
            .Tcl(paste('ctext::addHighlightClass ',.Tk.ID(txtedit), name, color, pattern))
            return()
        } else if(type=='chars') {
            .Tcl(paste('ctext::addHighlightClassForSpecialChars ',.Tk.ID(txtedit), name, color, pattern))
            return()
        } else
            return()
    }
    
    # latex
    if("latex" %in% highlight){
        # a macro without any brackets
        hl('regexp', 'latex1', hcolors$latexmacros,'{\\\\[[:alnum:]|[:punct:]]+}')
        # a macro with following brackets (and optionally [] brackets)
        hl('regexp', 'latex3', hcolors$latexmacros,
            '{\\\\[[:alnum:]|[:punct:]]+\\[[[:alnum:]*|[:punct:]*|[:space:]*|=*]*\\]\\{[[:alnum:]*|[:punct:]*|[:space:]*]*\\}}')
        # a macro with preceding brackets
        hl('regexp', 'latex4', hcolors$latexmacros,
            '{\\{\\\\[[:alnum:]|[:punct:]]*[[:space:]]*[[:alnum:]|[:punct:]|[:space:]]*\\}}')
        # comments
        hl('regexp', 'latexcomments', hcolors$latexcomments,
            "{(^%[^%[:alnum:]?[:punct:]?].+|[^%[:alnum:]?[:punct:]?]%.+)}")
        ## AMEND ABOVE TO DEAL WITH %*% %in% type constructions
        hl('regexp', 'rnwchunk1a', hcolors$rnwchunk,'{<{2}[[:alnum:]?|[:punct:]?|[:space:]?|=?]*>{2}}')
        hl('regexp', 'rnwchunk1b', hcolors$rnwchunk,'@')
        hl('regexp', 'rnwchunk1c', hcolors$rnwchunk,'\\\\Sexpr\\{.?\\}')
        hl('regexp', 'rtexchunk1a', hcolors$rtexchunks,'{%% begin.rcode.?}')
        hl('regexp', 'rtexchunk1b', hcolors$rtexchunks,'{%% end.rcode.?}')
        
        # equations
        #hl('regexp', 'latexeq', hcolors$rtexchunks,'\\${.+}\\$')
    }
    # markdown
    if("markdown" %in% highlight){
        riteMsg("Highlighting for markdown is only minimally supported")
        # something for the various kinds of markdown syntax
        hl('regexp', 'rmdheader1', hcolors$rmd,'=+')
        hl('regexp', 'rmdheader2', hcolors$rmd,'=-')
        hl('regexp', 'rmdheader2', hcolors$rmd,'{#{1,6} *.+ *#{0,6}$}')
        hl('regexp', 'rmdlist1', hcolors$rmd,'{^ *[-] .+$}')
        hl('regexp', 'rmdlist2', hcolors$rmd,'{^ *[+] .+$}')
        hl('regexp', 'rmdlist3', hcolors$rmd,'{^ *[*] .+$}')
        hl('regexp', 'rmdlist4', hcolors$rmd,'{[0-9]+[.] .+$}')
        hl('regexp', 'rmdquote', hcolors$rmd,'{^ *[>].+$}')
        hl('regexp', 'rmdlink', hcolors$rmd,'{\\[.+\\]\\(.+\\)}')
        hl('regexp', 'rmdcode', hcolors$rmd,'{`[^r].+`}')
        # code chunks of the form ```{} ... ```
        hl('regexp', 'rmdchunk1a', hcolors$rmd,'`{3}\\{r.+\\}')
        hl('regexp', 'rmdchunk1b', hcolors$rmd,'`{3}')
        hl('regexp', 'rmdchunk1c', hcolors$rmd,'{`r .+`}')
    }
    # html
    if("xml" %in% highlight){
        # xml/html tags <...>, </...>, and <.../>
        hl('regexp', 'xml1', hcolors$xml,
            '{</?[[:alnum:]]*(\\s+[[:alnum:]]+=(\\\'|")?\\w*(\\\'|")?)*\\s*/?>}')
        # xml/html comments
        hl('regexp', 'xml2', hcolors$xmlcomments,
            '{<!{1}-{2}.*(\\s+[[:alnum:]]+=(\\\'|")?\\w*(\\\'|")?)*\\s*-{2}>}')
    }
    # roxygen
    if("roxygen" %in% highlight){
        hl('regexp', 'comments', hcolors$rcomments,'{#[^\n\r]*}')
        hl('regexp', 'roxygen1', hcolors$roxygentext,"{#'[^\n\r]*}")
        hl('regexp', 'roxygen2a', hcolors$roxygenchunks,"{#[+|-][^\n\r]*}")
        hl('regexp', 'roxygen2b', hcolors$roxygenchunks,"{# (@knitr)[^\n\r]*}")
    }
    # brew
    if("brew" %in% highlight){
        hl('regexp', 'brew1a', hcolors$brewchunks,'<%.+%>')
        hl('regexp', 'brew1b', hcolors$brewchunks,'<%=.+%>')
        hl('regexp', 'brew2', hcolors$brewcomments,'<%#.+%>')
        hl('regexp', 'brew3', hcolors$brewtemplate,'<%%.+%%>')
    }
    # reST
    if("rest" %in% highlight){
        hl('regexp', 'rest1a', hcolors$restchunks,'{[.]{2} \\{r.+\\}}')
        hl('regexp', 'rest1b', hcolors$restchunks,'{[.]{2} [.]{2}}')
        hl('regexp', 'rest1c', hcolors$restchunks,'{:r:`.+`.}')
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
            hl('class', paste("basefunctions",j,sep=''), hcolors$functions,uniqtmp[j])
        }
        rm(HLfuns,uniq,tmpx,tmpsplit,uniqtmp)
        hl('class', 'specials', hcolors$operators, '[list TRUE FALSE NULL NA if else ]')
        hl('chars', 'operators', hcolors$operators, '{@-+!~?:;*/^<>=&|$,.}')
        hl('regexp', 'infix', hcolors$operators, '{%[[:alnum:][:punct:]]+%}')
        hl('chars', 'brackets', hcolors$brackets, '{[]{}()}')
        hl('regexp', 'digits', hcolors$digits, '{\\m[-+]?[0-9]*\\.?[0-9]+\\M}')
        # numbers before letters
        #hl('regexp', 'digits2', hcolors$normal, '{\\d+[A-Za-z]+[:space:]?}')
        hl('regexp', 'character1', hcolors$characters, '{"(?:[^\\"]|\\.)*"}')
        hl('regexp', 'character2', hcolors$characters, " {'(?:[^\\']|\\.)*'}")
        if(!"roxygen" %in% highlight)
            hl('regexp', 'comments', hcolors$rcomments, '{#[^\n\r]*}')
    }
}
