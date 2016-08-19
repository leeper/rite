checkBrackets <- function(txtedit, direction='right'){
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
