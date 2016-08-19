hl <- function(where, type, name = NULL, color, pattern){
    if (is.null(name)) {
        name <- paste('hl',paste(sample(letters,6,TRUE),collapse=''), sep='')
    }
    if (type == 'regexp') {
        .Tcl(paste('ctext::addHighlightClassForRegexp ',.Tk.ID(where), name, color, pattern))
    } else if (type == 'class'){
        .Tcl(paste('ctext::addHighlightClass ',.Tk.ID(where), name, color, pattern))
    } else if (type == 'chars') {
        .Tcl(paste('ctext::addHighlightClassForSpecialChars ',.Tk.ID(where), name, color, pattern))
    }
    retrun()
}
    

highlight <- function(where, highlight, hcolors) {
    ## SYNTAX HIGHLIGHTING RULES ##
    
    # latex
    if ("latex" %in% highlight) {
        # a macro without any brackets
        hl(where = where, 'regexp', 'latex1', hcolors$latexmacros,'{\\\\[[:alnum:]|[:punct:]]+}')
        # a macro with following brackets (and optionally [] brackets)
        hl(where = where, 'regexp', 'latex3', hcolors$latexmacros,
            '{\\\\[[:alnum:]|[:punct:]]+\\[[[:alnum:]*|[:punct:]*|[:space:]*|=*]*\\]\\{[[:alnum:]*|[:punct:]*|[:space:]*]*\\}}')
        # a macro with preceding brackets
        hl(where = where, 'regexp', 'latex4', hcolors$latexmacros,
            '{\\{\\\\[[:alnum:]|[:punct:]]*[[:space:]]*[[:alnum:]|[:punct:]|[:space:]]*\\}}')
        # comments
        hl(where = where, 'regexp', 'latexcomments', hcolors$latexcomments,
            "{(^%[^%[:alnum:]?[:punct:]?].+|[^%[:alnum:]?[:punct:]?]%.+)}")
        ## AMEND ABOVE TO DEAL WITH %*% %in% type constructions
        hl(where = where, 'regexp', 'rnwchunk1a', hcolors$rnwchunk,'{<{2}[[:alnum:]?|[:punct:]?|[:space:]?|=?]*>{2}}')
        hl(where = where, 'regexp', 'rnwchunk1b', hcolors$rnwchunk,'@')
        hl(where = where, 'regexp', 'rnwchunk1c', hcolors$rnwchunk,'\\\\Sexpr\\{.?\\}')
        hl(where = where, 'regexp', 'rtexchunk1a', hcolors$rtexchunks,'{%% begin.rcode.?}')
        hl(where = where, 'regexp', 'rtexchunk1b', hcolors$rtexchunks,'{%% end.rcode.?}')
        
        # equations
        #hl(where = where, 'regexp', 'latexeq', hcolors$rtexchunks,'\\${.+}\\$')
    }
    # markdown
    if ("markdown" %in% highlight) {
        # something for the various kinds of markdown syntax
        hl(where = where, 'regexp', 'rmdheader1', hcolors$rmd,'=+')
        hl(where = where, 'regexp', 'rmdheader2', hcolors$rmd,'=-')
        hl(where = where, 'regexp', 'rmdheader2', hcolors$rmd,'{#{1,6} *.+ *#{0,6}$}')
        hl(where = where, 'regexp', 'rmdlist1', hcolors$rmd,'{^ *[-] .+$}')
        hl(where = where, 'regexp', 'rmdlist2', hcolors$rmd,'{^ *[+] .+$}')
        hl(where = where, 'regexp', 'rmdlist3', hcolors$rmd,'{^ *[*] .+$}')
        hl(where = where, 'regexp', 'rmdlist4', hcolors$rmd,'{[0-9]+[.] .+$}')
        hl(where = where, 'regexp', 'rmdquote', hcolors$rmd,'{^ *[>].+$}')
        hl(where = where, 'regexp', 'rmdlink', hcolors$rmd,'{\\[.+\\]\\(.+\\)}')
        hl(where = where, 'regexp', 'rmdcode', hcolors$rmd,'{`[^r].+`}')
        # code chunks of the form ```{} ... ```
        hl(where = where, 'regexp', 'rmdchunk1a', hcolors$rmd,'`{3}\\{r.+\\}')
        hl(where = where, 'regexp', 'rmdchunk1b', hcolors$rmd,'`{3}')
        hl(where = where, 'regexp', 'rmdchunk1c', hcolors$rmd,'{`r .+`}')
    }
    # html
    if ("xml" %in% highlight) {
        # xml/html tags <...>, </...>, and <.../>
        hl(where = where, 'regexp', 'xml1', hcolors$xml,
            '{</?[[:alnum:]]*(\\s+[[:alnum:]]+=(\\\'|")?\\w*(\\\'|")?)*\\s*/?>}')
        # xml/html comments
        hl(where = where, 'regexp', 'xml2', hcolors$xmlcomments,
            '{<!{1}-{2}.*(\\s+[[:alnum:]]+=(\\\'|")?\\w*(\\\'|")?)*\\s*-{2}>}')
    }
    # roxygen
    if ("roxygen" %in% highlight) {
        hl(where = where, 'regexp', 'comments', hcolors$rcomments,'{#[^\n\r]*}')
        hl(where = where, 'regexp', 'roxygen1', hcolors$roxygentext,"{#'[^\n\r]*}")
        hl(where = where, 'regexp', 'roxygen2a', hcolors$roxygenchunks,"{#[+|-][^\n\r]*}")
        hl(where = where, 'regexp', 'roxygen2b', hcolors$roxygenchunks,"{# (@knitr)[^\n\r]*}")
    }
    # brew
    if ("brew" %in% highlight) {
        hl(where = where, 'regexp', 'brew1a', hcolors$brewchunks,'<%.+%>')
        hl(where = where, 'regexp', 'brew1b', hcolors$brewchunks,'<%=.+%>')
        hl(where = where, 'regexp', 'brew2', hcolors$brewcomments,'<%#.+%>')
        hl(where = where, 'regexp', 'brew3', hcolors$brewtemplate,'<%%.+%%>')
    }
    # reST
    if ("rest" %in% highlight) {
        hl(where = where, 'regexp', 'rest1a', hcolors$restchunks,'{[.]{2} \\{r.+\\}}')
        hl(where = where, 'regexp', 'rest1b', hcolors$restchunks,'{[.]{2} [.]{2}}')
        hl(where = where, 'regexp', 'rest1c', hcolors$restchunks,'{:r:`.+`.}')
    }
    # r
    if ("r" %in% highlight) {
        # functions
        HLfuns <- lapply(search(),FUN=function(x) { paste(unique(gsub("<-","",objects(x))),collapse=" ") })
        uniq <- sort(unique(unlist(lapply(search(), FUN=function(x) {strsplit(gsub("<-","",objects(x)),".",fixed=TRUE)} ))))
        uniq <- uniq[grep("abbreviate",uniq):length(uniq)]
        tmpx <- sort(rep(1:ceiling(length(uniq)/30),30))
        tmpsplit <- split(uniq,tmpx[1:length(uniq)])
        uniqtmp <- sapply(tmpsplit, FUN=function(x) { paste(" [list",paste(x,collapse=" ")," ]") })
        for (j in 1:length(uniqtmp)) {
            hl(where = where, 'class', paste("basefunctions",j,sep=''), hcolors$functions,uniqtmp[j])
        }
        rm(HLfuns,uniq,tmpx,tmpsplit,uniqtmp)
        hl(where = where, 'class', 'specials', hcolors$operators, '[list TRUE FALSE NULL NA if else ]')
        hl(where = where, 'chars', 'operators', hcolors$operators, '{@-+!~?:;*/^<>=&|$,.}')
        hl(where = where, 'regexp', 'infix', hcolors$operators, '{%[[:alnum:][:punct:]]+%}')
        hl(where = where, 'chars', 'brackets', hcolors$brackets, '{[]{}()}')
        hl(where = where, 'regexp', 'digits', hcolors$digits, '{\\m[-+]?[0-9]*\\.?[0-9]+\\M}')
        # numbers before letters
        #hl(where = where, 'regexp', 'digits2', hcolors$normal, '{\\d+[A-Za-z]+[:space:]?}')
        hl(where = where, 'regexp', 'character1', hcolors$characters, '{"(?:[^\\"]|\\.)*"}')
        hl(where = where, 'regexp', 'character2', hcolors$characters, " {'(?:[^\\']|\\.)*'}")
        if (!"roxygen" %in% highlight) {
            hl(where = where, 'regexp', 'comments', hcolors$rcomments, '{#[^\n\r]*}')
        }
    }
}
