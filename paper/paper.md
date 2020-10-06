---
title: 'hector-ui: A web-based interactive scenario builder and visualization application for the Hector climate model'
tags:
  - R
  - Hector
  - climate model
  - R Shiny
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
date: 30 September 2020
bibliography: paper.bib
---

# Introduction

In a world of increasingly online, distributed, and diverse interdisciplinary research, there is a need to provide accessible and user-friendly interactive visualization tools that elucidate complex models and their output products. Hector UI is an R Shiny web interface built to extend the simple global climate model Hector [@Hartin2015]), originally designed in C++ with an accompanying R interface. Traditionally, Hector has only had a command line interface available which requires fluency in C++ or R and a full understanding of Hector’s parameter space to run the model and create output. Hector UI provides a fast, efficient solution that makes the model more accessible to a broader user base. This implementation allows users that may not be fluent in R or C++ to interactively explore model scenarios and outputs in an easy to use, guided point and click interface.

# Supporting Software
The supporting software for Hector UI is the Hector climate model which is an open source, object-oriented, simple global climate carbon-cycle model. It runs essentially instantaneously while still representing the most critical global scale earth system processes and is one of a class of models heavily used for emulating complex climate models and uncertainty analyses [@Hartin2016]. To run the model, a Hector core object is instantiated from an input file that contains a time series of greenhouse gas emissions. Hector comes equipped with input files, or scenarios, based on the Representative Concentration Pathways (RCP) 2.6 [@van2011rcp2], 4.5 [@clarke2007scenarios; @thomson2011rcp4], 6.0 [@fujino2006multi], and 8.5 [@riahi2007scenarios; @riahi2011rcp]. These scenarios were developed to cover the full range of potential emissions scenarios and are used in climate change research and model intercomparison projects. In addition to these baseline scenarios the user can upload a set of custom emissions and run the model using these emissions to generate a time series tailored to research needs. Hector is also an input component for the Global Change Analysis Model [GCAM; @calvin2019gcam].

# Summary
Hector UI provides the full functionality of Hector in a user-friendly web interface that can create Hector cores on demand to view any number of scenarios in parallel. Graphs of up to four output variables can be viewed simultaneously with each scenario represented as a time series. In addition to core Hector functionality, the following describes some new capabilities that the Hector UI.  First, we provide the ability to configure Hector’s model input parameters to have the model emulate other well-known earth system models. This allows the user to seamlessly switch between models and see the changes reflected in the output. Another new feature provided are downscaled, gridded global maps of both temperature and precipitation based off patterns generated from Hector and the other earth system models [@snyder2019joint]. The system takes a model, scenario, year, and variable (near-surface air temperature or precipitation) and dynamically calculates a gridded output map by running the model output against pre-generated pattern files.

# Acknowledgements

This research was supported by the U.S. Department of Energy's Office of Science as a part of the MultiSector Dynamics program area within the Earth and Environmental System Modeling Program.

# References
