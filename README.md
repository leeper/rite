# The Right Editor to Write R


## What is rite? ##

**rite** is a simple yet powerful script editor built natively in R with tcltk, currently in beta release.

**rite** is designed to substitute for the built-in script editor provided by R and improve upon it by offering functionality more commonly found in standalone editors and IDEs (e.g., syntax highlighting, command completion, shortcut keys, find and go-to-line commands, direct access to R help files, etc.), as well as an optional output viewer, inspired by [CodeSkulptor](http://www.codeskulptor.org/), that eliminates the need to toggle between the script editor and the R console.

## Why a script editor internal to R? ##

As the popularity of R has increased, it has become increasingly easy to find advanced text editors that support the R language (ESS, Notepad++, TINN-R, WinEdt, gedit, and many others) and tools like RStudio IDE provide a large range of tools to organize, simplify, and clarify for R programming. Yet, many R beginners are statistics students and/or users of other statistical packages with little programming experience, which often means they have no experience with (and do not have immediate access to) the advanced text editors programmers rely on day-to-day.

rite tries to streamline beginners' use of R by providing a sophsticated script editor that helps them see R code (through syntax highlighting), get help with code (through command completion and command help), and produce simple, clear output.

But beyond a tool for beginners, **rite** is also designed to help advanced users do more with R. Specifically, rite can substitute for R's internal editor to provide a more user-friendly tool for putting together and testing quick R code and, perhaps more importantly, it provides a simple point-and-click access to the core reproducible research tools provided by [knitr](http://yihui.name/knitr/) that allow users to seamlessly move from `Sweave`/`knitr` markup with embedded R code to LaTeX/PDF, markdown, and HTML output. **rite** also provides the ability to load local and web-based scripts and save scripts to anonymous [Gists](https://gist.github.com/).

## Installation ##

**rite** is [available on CRAN](http://cran.r-project.org/web/packages/rite/index.html) and can be installed from within R from your favorite CRAN mirror:

```
install.packages("rite")
```

And the latest development version, available here can be installed directly using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package:

```
library(devtools)
install_github("rite","leeper")
```

If you find that you use **rite** often, you may want to consider adding it to your [R startup profile](http://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html).

## Integration with [knitr](http://yihui.name/knitr/) ##

**rite** provides the first visual interface for report generation directly in R through the `riteout` function (a simple wrapper for `rite(catchOutput=TRUE)`. Through `riteout`, users can generate reports in a variety of formats (including markdown, HTML, and LaTeX) through just a single mouse-click. `riteout` currently offers access to the **knitr** functions `knit` and `purl`, allowing users to convert documents embedded with R code chunks into either finished reports and analysis replication files, respectively. These functions work analogously to `Sweave` and `Stangle` provided by base R.

Additionally, `riteout` allows users to `stitch` simple reports from unformatted R scripts, making the quick production of readable R analyses a mouse click away. Support for **knitr**'s `spin` (using Roxygen-style syntax) has also been added.

To complement this functionality, **rite** provides syntax highlighting for R (by default) and, optionally, for LaTeX, markdown, HTML/XML, brew, roxygen, and reST. Higlighting of `Sweave`/`knitr`-style code chunks is supported in each case.

## Experimental highlighted output function ##

The current development release of **rite** also includes a function `ritesink` that provides an experimental window for viewing highlighted R output instead of (or in addition to) sending those results to the R console. This tool aims to offer a robust alternative to the deprecated [**colorout** package](http://cran.r-project.org/web/packages/colorout/index.html).
