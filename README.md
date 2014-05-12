# The Right Editor to Write R #

[![Build Status](https://travis-ci.org/leeper/rite.png?branch=master)](https://travis-ci.org/leeper/rite)

## What is rite? ##

**rite** is a simple yet powerful script editor built natively in R with tcltk.

**rite** is designed to substitute for the built-in script editor provided by R and improve upon it by offering functionality more commonly found in standalone editors and IDEs (e.g., syntax highlighting, command completion, shortcut keys, find and go-to-line commands, direct access to R help files, etc.), as well as an optional output viewer (called by `riteout`), inspired by [CodeSkulptor](http://www.codeskulptor.org/), that eliminates the need to toggle between the script editor and the R console.

**Screenshot of rite**

[![rite Screenshot](http://i.imgur.com/74TkHIn.png)](http://imgur.com/74TkHIn)


**Screenshot of riteout**

[![riteout Screenshot](http://i.imgur.com/P2mmwY7.png)](http://imgur.com/P2mmwY7)


## Installation ##

**rite** is [available on CRAN](http://cran.r-project.org/web/packages/rite/index.html) and can be installed from within R from your favorite CRAN mirror:

```
install.packages("rite")
```

And the latest development version, available here can be installed directly using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package:

```
# install.packages("devtools")
library("devtools")
install_github("leeper/rite")
```

If you find that you use **rite** often, you may want to consider adding it to your [R startup profile](http://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html).

## Why a script editor internal to R? ##

As the popularity of R has increased, it has become increasingly easy to find advanced text editors that support the R language (ESS, Notepad++, Sublime Text, TINN-R, WinEdt, gedit, and many others) and tools like RStudio IDE provide a large range of tools to organize, simplify, and clarify for R programming. Yet, many R beginners are statistics students and/or users of other statistical packages with little programming experience, which often means they have no experience with (and do not have immediate access to) the advanced text editors programmers rely on day-to-day.

**rite** tries to streamline beginners' use of R by providing a sophsticated script editor that helps them see R code (through syntax highlighting), get help with code (through command completion and command help), and produce simple, clear output.

But beyond a tool for beginners, **rite** is also designed to help advanced users do more with R. rite can substitute for R's internal editor to provide a more user-friendly tool for putting together and testing quick R code and provides simple point-and-click access to [knitr](http://yihui.name/knitr/). **rite** also provides the ability to load local and web-based scripts, save scripts to anonymous [GitHub Gists](https://gist.github.com/), and share markdown and HTML documents on [RPubs](http://rpubs.com/).

## Integration with [knitr](http://yihui.name/knitr/) ##

**rite** provides the first visual, point-and-click interface for literate document generation directly in R, with support for a variety of formats (including markdown, HTML, and LaTeX). **rite** supports the **knitr** functions `knit` and `purl` (analogous to `Sweave` and `Stangle` in base R), as well as `stitch` and `spin` to convert R script files into reports without investing in a particular markup language. The report generation tools also support TeX compilation and markdown to HTML conversion.

To complement this functionality, **rite** provides syntax highlighting for R (by default) and, optionally, for LaTeX, markdown, HTML/XML, brew, roxygen, and reST. Higlighting of `Sweave`/`knitr`-style code chunks is supported in each case.

The *Run* menu also supports execution of Sweave/knitr code chunks from within a literate document, making it easy to run analyses while directly editing a full report.

## Highlighted output function ##

**rite** also includes a pair of functions (`sinkstart` and `sinkstop`) that provide a widget for viewing highlighted R output instead of (or in addition to) sending those results to the R console. This tool aims to offer a robust, CRAN-compliant alternative to the deprecated [**colorout** package](http://cran.r-project.org/web/packages/colorout/index.html). In essence, the package builds a layer on top of R (using `sink`, task callbacks, and a custom error handler) to dynamically display R calls and output in a tcl/tk window. The sink can be turned on (`sinkstart`) and off (`sinkstop`) throughout an R session and the sink display is a fully featured and editable tcl/tk text widget.

Here's a screenshot of a trivial use of the sink:

[![rite sink Screenshot](http://i.imgur.com/pGjsgxF.png)](http://i.imgur.com/pGjsgxF)


