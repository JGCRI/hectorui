---
title: 'hectorui: A web-based interactive scenario builder and visualization application for the Hector climate model'
tags:
  - R
  - Hector
  - climate model
  - RShiny
authors:
  - name: Jason Evanoff
    orcid: 0000-0002-8858-2783
    affiliation: 1
  - name: Chris R. Vernon
    orcid: 0000-0002-3406-6214
    affiliation: 1
  - name: Stephanie Waldhoff
    orcid: 0000-0002-8073-0868
    affiliation: 1
  - name: Abigail Snyder
    orchid: 0000-0002-9034-9948
    affiliation: 1  
  - name: Corinne Hartin
    orcid: 0000-0003-1834-6539
    affiliation: 1
affiliations:
 - name: Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, MD, USA
   index: 1
date: 07 October 2020
bibliography: paper.bib
---

# Statement of need
In a world of increasingly online, distributed, and diverse interdisciplinary research, there is a need to provide accessible and user-friendly interactive visualization tools that elucidate complex models and their output products. `hectorui` is an R Shiny web interface built to enhance the user experience for the simple global climate model Hector [@Hartin2015], originally designed in C++ and later extended with an R interface. Before `hectorui`, Hector only had a command line-based interface available which requires fluency in C++ or R and a full understanding of Hector’s parameter space to run the model and create output. `hectorui` provides a fast, efficient solution that makes the model more accessible to a broader user base and offers enhanced functionality (Figure 1). This implementation allows users that may not be fluent in a programming language to interactively explore model scenarios and outputs in an easy to use, guided point and click interface.

![The map scenario interface to `hectorui` that allows the user to parameterize inputs and visualize run outputs interactively.](figure1.png)

# Supporting software
The `hectorui` application was built in R Studio using the Shiny package. Shiny is a package for R that makes it easy to build and deploy interactive web apps straight from R [@shiny]. The built in components provide easy to use controls and layout functionality for building web applications without writing HTML.

The supporting software for `hectorui` is Hector, an open source, object-oriented, simple global climate carbon-cycle model. It runs essentially instantaneously while still representing the most critical global scale earth system processes and is one of a class of models heavily used for emulating complex climate models and uncertainty analyses [@Hartin2016]. Hector is also an input component for the Global Change Analysis Model [GCAM; @calvin2019gcam]. To run the model, a Hector core object is instantiated from an input file that contains a time series of greenhouse gas emissions. Hector comes equipped with input files, or scenarios, based on the Representative Concentration Pathways (RCP) 2.6 [@van2011rcp2], 4.5 [@clarke2007scenarios; @thomson2011rcp4], 6.0 [@fujino2006multi], and 8.5 [@riahi2007scenarios; @riahi2011rcp]. These scenarios were developed to cover the full range of potential emissions scenarios and were designed to meet research needs, such as output from human-Earth systems and sectoral models, impact analysis, and policy relevant research.

# Summary
`hectorui` provides the full functionality of Hector in a user-friendly web interface that can create Hector cores on demand to view any number of scenarios in parallel. Graphs of up to four output variables can be viewed simultaneously with each scenario represented as a time series. In addition to supporting Hector’s full functionality, the following describes some new capabilities that the `hectorui` offers.  First, the user can configure Hector’s model input parameters to have the model emulate other well-known earth system models. This allows the user to seamlessly switch between models and see the changes reflected in the output. Another new feature provided are downscaled, gridded global maps of both temperature and precipitation that are dynamically generated from pattern files based on Hector and the other earth system models [@snyder2019joint]. Lastly, in addition to the included canonical RCP scenarios the user can upload a set of custom emissions and run the model using these emissions to generate new time series results tailored to their specific research needs.

The ability to visualize and explore the model scenarios and data output for Hector allows both a broader user base to access Hector and the ability to rapidly explore scenarios and output. By creating user-friendly software that is simple to use which offers a generous set of pre-defined functionality, `hectorui` enhances Hector’s accessibility and equips the end user with the capability to focus on discovery and analysis instead of data processing, transformation, and time-consuming graph and chart development.

`hectorui` can be accessed via the web at the public domain https://jgcri.shinyapps.io/HectorUI/ or it can be installed and run locally as a standalone package (see https://github.com/JGCRI/hectorui to clone or fork for collaborative purposes). We provide an R vignette step-by-step tutorial for users to get started with `hectorui` which is accessible here: [Tutorial](https://jgcri.github.io/hectorui/articles/Tutorial.html).

# Acknowledgements
This research was supported in part by the U.S. Department of Energy, Office of Science, as part of research in MultiSector Dynamics, Earth and Environmental System Modeling Program. The Pacific Northwest National Laboratory is operated for DOE by Battelle Memorial Institute under contract DE-AC05-76RL01830.  The authors also received support for this research through the U.S. Environmental Protection Agency, under Interagency Agreement DW08992459801. The views and opinions expressed in this paper are those of the authors alone.

# References
