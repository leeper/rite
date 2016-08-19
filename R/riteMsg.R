# FUNCTION TO CONTROL PRINTING TO OUTPUT VERSUS CONSOLE
riteMsg <- function(output, errorout, value, error =FALSE) {
    if (catchOutput & error) {
        tkinsert(errorout, "end", value)
    } else if (catchOutput) {
        tkinsert(output, "end", value)
    } else {
        message(value, appendLF = FALSE)
    }
    return(invisible(NULL))
}
