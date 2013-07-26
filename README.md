# The Right Editor to Write R


## What is rite? ##

**rite** is a simple yet powerful R script editor built natively in R with tcltk.

**rite** is designed to substitute for the built-in script editor provided by R and improve upon it by offering functionality more commonly found in standalone editors and IDEs (e.g., syntax highlighting, command completion, shortcut keys, find and go-to-line commands, direct access to R help files, etc.), as well as an optional output viewer that eliminates the need to toggle between the script editor and the R console.

## Why a script editor internal to R? ##

As the popularity of R has increased, it has become increasingly easy to find advanced text editors that support the R language (ESS, Notepad++, TINN-R, WinEdt, gedit, and many others) and tools like RStudio IDE provide a large range of tools to organize, simplify, and clarify for R programming. Yet, many R beginners are statistics students and/or users of other statistical packages with little programming experience, which often means they have no experience with (and do not have immediate access to) the advanced text editors programmers rely on day-to-day.

rite tries to streamline beginners' use of R by providing a sophsticated script editor that helps them see R code (through syntax highlighting), get help with code (through command completion and command help), and produce simple, clear output.

But beyond a tool for beginners, **rite** is also designed to help advanced users do more with R. Specifically, rite can substitute for R's internal editor to provide a more user-friendly tool for putting together and testing quick R code and, perhaps more importantly, it provides a simple point-and-click access to the core reproducible research tools provided by [knitr](http://yihui.name/knitr/) that allow users to seamlessly move from `Sweave`/`knitr` markup with embedded R code to LaTeX and PDF output.
