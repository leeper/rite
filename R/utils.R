riteenv <- new.env()

editkeypress <- function(txtedit, direction = 'right'){
    tktag.remove(txtedit,'tmpbracketclose', '1.0', 'end')
    checkBrackets(txtedit, direction)
}

editModified <- function(txtedit){
    scriptSaved <<- FALSE
    tkwm.title(riteenv$editor, paste("*",riteenv$wmtitle))
    editkeypress(txtedit)
}
