editkeypress <- function(where, direction = 'right'){
    tktag.remove(where,'tmpbracketclose', '1.0', 'end')
    checkBrackets(where, direction)
}

editModified <- function(where){
    scriptSaved <<- FALSE
    tkwm.title(riteenv$editor, paste("*",riteenv$wmtitle))
    editkeypress(where)
}
