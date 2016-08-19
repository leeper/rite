insertText <- function(what, where, fastinsert = TRUE) {
    if (fastinsert) {
        .Tcl(.Tcl.args(.Tk.ID(where), 'fastinsert', 'insert', what))
    } else {
        tkinsert(where, "insert", what)
    }
    TRUE
}
