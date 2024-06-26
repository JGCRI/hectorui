[![DOI](https://zenodo.org/badge/198255756.svg)](https://zenodo.org/badge/latestdoi/198255756) ![R-CMD](https://github.com/JGCRI/hectorui/workflows/R-CMD/badge.svg) ![build](https://github.com/JGCRI/hectorui/workflows/build/badge.svg) [![codecov](https://codecov.io/gh/JGCRI/hectorui/branch/main/graph/badge.svg?token=aOWN2ELixv)](https://codecov.io/gh/JGCRI/hectorui) [![DOI](https://joss.theoj.org/papers/10.21105/joss.02782/status.svg)](https://doi.org/10.21105/joss.02782)
[![docs](https://github.com/JGCRI/hectorui/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/JGCRI/hectorui/actions/workflows/pkgdown.yml)

#  hectorui

A web-based interactive scenario builder and visualization application for the Hector climate model

## Using `hectorui`

Just getting started with `hectorui`?  We have constructed a tutorial to examine some sample use-cases which are available here:  [Tutorial](https://jgcri.github.io/hectorui/articles/Tutorial.html)

To navigate directly to the `hectorui` app **CLICK the following image**:

[![`hectorui` map scenario interface](https://raw.githubusercontent.com/JGCRI/hectorui/main/paper/HectorUIv2.0_HomePage.png)](https://jgcri.shinyapps.io/HectorUI/)

## Installing Locally

To install hectorui as an R package for local use, please follow these steps in your R command line:

```R
library(devtools)
devtools::install_github("JGCRI/hectorui")
```

## Developing Locally
How you launch the app after installation depends on your R environment.  See the following.

#### For RStudio Users
If you are using RStudio, simply open the `server.r` or `ui.r` file and execute `Run App`.

#### For CMD Users
Start `R.exe` and enter the following command:

```R
shiny::runApp(system.file("shinyApp", package = "hectorui"))
```

#### Adding New Features
Users familiar with R Shiny can add new features by working directly in the `hectorui/inst/shinyApp` directory.

## Contributing to `hectorui`

We welcome contributions to `hectorui` from the development community. Join in 
on the conversation at the [`hector-ui` GitHub Discussions page](https://github.com/JGCRI/hectorui/discussions) or contact us if you want to
collaborate!

For more information about contributing, please contact Stephanie Pennington at stephanie.pennington@pnnl.gov or Chris Vernon at chris.vernon@pnnl.gov

## Learn More About Hector
Read more about the Hector simple climate model here:  [`Hector` Documentation](https://jgcri.github.io/hector/). `Hector` has been published in the following:

```
Hartin CA, Patel P, Schwarber A, Link R, Bond-Lamberty B (2015). “A simple object-oriented and open-source model for scientific and policy analyses of the global climate system–Hector v1.0.” Geoscientific Model Development, 8(4), 939–955.
```
