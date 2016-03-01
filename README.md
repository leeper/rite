# The Right Editor to Write R #

**rite** is a simple yet powerful script editor built natively in R with Tcl/tk. It is designed to substitute for the built-in script editor provided by R and improve upon it by offering functionality more commonly found in standalone editors and IDEs (e.g., syntax highlighting, command completion, shortcut keys, find and go-to-line commands, direct access to R help files, etc.). You can open the widget with `rite::rite()` and it looks like this:

[![rite Screenshot](http://i.imgur.com/74TkHInl.png)](http://imgur.com/74TkHIn)

The package also includes an optional output viewer (called by `rite::riteout()`) that prints to an easily edited Tcl/tk text widget, as well as a "sink" widget that offers an alternative read-eval-print that outputs to a widget instead of or in addition to the R console.

[![riteout Screenshot](http://i.imgur.com/P2mmwY7l.png)](http://imgur.com/P2mmwY7)

Another feature is a widget that modifies the read-eval-print loop for the R console by sendingout output to a color highlighted, easily edited Tcl/Tk text widget. This tool aims to offer a robust, CRAN-compliant alternative to the deprecated [**colorout** package](http://cran.r-project.org/web/packages/colorout/index.html). The sink can be turned on (`sinkstart()`) and off (`sinkstop()`) throughout an R session and the sink display is a fully featured and editable tcl/tk text widget. It looks like this:

[![rite sink Screenshot](http://i.imgur.com/pGjsgxFl.png)](http://i.imgur.com/pGjsgxF)

In essence, the package builds a layer on top of R (using `sink`, task callbacks, and a custom error handler) to dynamically display R calls and output in a tcl/tk window. 

## Why create any of this? ##

As the popularity of R has increased, it has become easy to find advanced text editors that support the R language and tools like RStudio provide great functionality for R coders and developers. Yet, many R beginners are statistics students and/or users of other statistical packages with little programming experience and no familiar with text editors other than Notepad. **rite** tries to streamline beginners' use of R by providing a set of useful tools that can help them see R code (through syntax highlighting), get help with code (through command completion and command help), and produce simple, clear output.

But beyond a tool for beginners, **rite** is also designed to help advanced users do more with R:

 1. `rite()` can substitute for R's internal editor to provide a more user-friendly tool for putting together and testing quick R code
 2. `rite()` provides simple point-and-click access to [knitr](http://yihui.name/knitr/) functions including: `knit()` and `purl()`, as well as `stitch()` and `spin()`, and the Sweave functions `Sweave()` and `Stangle()`. rite supports every format supported by knitr.
 3. `rite()` can load local and web-based scripts, save scripts to anonymous [GitHub Gists](https://gist.github.com/), and share markdown and HTML documents on [RPubs](http://rpubs.com/).
 4. `riteout()` provides a convenient way of editing executed R code. The R console and RStudio console are read-only. `riteout()` provides a way to interact with that output (which can be useful when you simply want to copy code but have made mistakes).
 5. The sink widget is incredibly useful to creating and editing R output directly from the console. Because it is designed for interactive use, it is easy to work from the console, selectively print some output to the sink, and then simply copy/paste that output into another format without having to open another text editor. It works on any platform, including from within RStudio.

## Installation ##

[![Build Status](https://travis-ci.org/leeper/rite.png?branch=master)](https://travis-ci.org/leeper/rite)
![Downloads](http://cranlogs.r-pkg.org/badges/rite)
[![Travis-CI Build Status](https://travis-ci.org/leeper/rite.png?branch=master)](https://travis-ci.org/leeper/rite)
[![codecov.io](http://codecov.io/github/leeper/rite/coverage.svg?branch=master)](http://codecov.io/github/leeper/rite?branch=master)

**rite** is [available on CRAN](http://cran.r-project.org/package=MTurkR) and can be installed from within R from your favorite CRAN mirror:

```
install.packages("rite")
```

To install the latest development version of **MTurkR** from GitHub:

```R
# latest (unstable) version from GitHub
if(!require("ghit")){
    install.packages("ghit")
}
ghit::install_github("leeper/MTurkR")
```

