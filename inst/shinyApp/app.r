source("global.r", local = TRUE)

ui <- fluidPage(theme = shinythemes::shinytheme("readable"),
  includeCSS("./components/layout/style copy.css"),
  tags$div(class = "container",
           tags$img(src = "images/earth-header.png", height = "300px", width = "100%", class = "earth", alt = "Earth's atmosphere"),
           tags$div(
               a(
                   img(src = "images/GCIMS_logo_alt.svg", class = "logo"), href = "https://gcims.pnnl.gov/", target = "_blank"),
               h1("HectorUI - BETA", class = "header-text-title"),
               h2("An Interactive Climate Model", class = "header-text-sub", style = "font-weight:normal; "),
           ),
  ),
  navbarPage(
    id = "nav",
    title = "",
    collapsible = TRUE,
    tabPanel(title = "Home",
             fluidRow(
                 column(7,
                        includeHTML("./components/layout/homepage.html")
                 ),
                 column(4, div(
                     br(),
                     br(),
                     actionButton(inputId = "launch_scenario",
                                  label = "Explore Hector",
                                  style = "background: #4174C3; color: white;
                                                             font-size: 24px; padding: 32px 24px;
                                                             box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);"),
                     align = "center"
                 ))
             )
    ),
    tabPanel(title = "Explore Hector",
             fluidRow(
               run_ui("run_1")
             )
    ),
    tabPanel(title = "Custom Emissions",
             fluidRow(
               custom_ui("custom_1")
             )
    ),
    tabPanel(title = "Carbon Tracking",
             fluidRow(
               tracking_ui("tracking_1")
             )
    ),
    tabPanel(title = "Guides",
             mainPanel(
                 style="vertical-align: middle;",
                 h3("Ready to get started?",
                    tags$a("View the Guide/Tutorial", href="https://jgcri.github.io/hectorui/articles/Tutorial.html", target="blank")),
                 br(),
                 HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/fBHXS7pjZcI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')

             )
    ),
    tabPanel(title = "About",

             mainPanel
             (
                 width = 9,
                 class = "about-info",
                 tabsetPanel
                 (
                     # Information Tab Panel
                     tabPanel
                     (
                         p(icon("info-circle", "fa-2x"), "Hector Information", value="infoTab"),
                         h5("Background Information"), tags$hr(class="hrNav"),
                         tags$table(
                             tags$tr(
                                 tags$td(width = "50%",
                                         h5("Explore the Hector Product Family", style="text-align: left"),
                                         br(),
                                         h6("Source code and contribution is available on the ",
                                            a("HectorUI Github page", href = "https://github.com/JGCRI/hector", target = "_blank")),
                                 ),
                             )
                         ),
                         br(),
                         h5("Documentation/Downloads"),
                         tags$hr(class="hrNav"),
                         p("The primary link to the Hector model documentation is the ",
                           a("online manual", href="https://jgcri.github.io/hector/articles/manual", target="blank"),
                           ", which is included in the vignettes/manual directory. The code is also documented with ",
                           a("Doxygen-style", href="http://doxygen.org", target="blank"),
                           " comments."),
                         p("A formal model description paper via ",
                           a("Hartin et al. 2015", href="http://www.geosci-model-dev.net/8/939/2015/gmd-8-939-2015.html", target="blank"),
                           " documents its science internals and performance relative to observed data, the ",
                           a("CMIP5", href="http://cmip-pcmdi.llnl.gov/cmip5/",  target="blank"),
                           " archive, and the reduced-complexity ",
                           a("MAGICC",href="http://www.magicc.org", target="blank"),
                           " model (as of ",
                           a("version 1.0", href="https://github.com/JGCRI/hector/tree/v1.0", target="blank"),
                           "). In addition, we have developed two package vignettes demonstrating the ",
                           a("basics of the Hector R interface", href="http://jgcri.github.io/hector/articles/intro-to-hector.html", target="blank"),
                           ", and an example application of ",
                           a("solving for an emissions pathway", href="http://jgcri.github.io/hector/articles/hector_apply.html", target="blank"),
                           "."),
                         tags$ul(
                             tags$li(
                                 h5(tags$a("Hector User Interface package download/source link ", href="https://github.com/JGCRI/Hector-ui", target="blank"))),
                             tags$li(
                                 h5(tags$a("Hector R / Hector C++ package download/source link", href="https://github.com/JGCRI/Hector", target="blank"))),
                             tags$li(
                                 h5(tags$a("Code and data for Hector calibration papers", href="https://zenodo.org/record/3515153#.Xg9iGuhKiUl", target="blank"),
                                    tags$img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.3515153.svg", class="zenodo", alt="DOI")
                                 )
                             )
                         ),
                         br(),
                         h5("Tools and software that work with Hector"),
                         tags$hr(class="hrNav"),
                         tags$ul(
                             tags$li(
                                 a("GCAM", href="https://github.com/JGCRI/gcam-core", target="blank"),
                                 ": Hector can be used as the climate component in the GCAM integrated assessment model."),
                             tags$li(
                                 a("pyHector", href="https://github.com/openclimatedata/pyhector", target="blank"),
                                 ": A python interface to Hector."),
                             tags$li(
                                 a("R/Shiny", href="https://shiny.rstudio.com/", target="blank"),
                                 ": This application was built as an R-Shiny package wrapper over the existing model code.")
                         )
                     ),
                     # Citation Tab Panel
                     tabPanel
                     (
                         p(icon("copyright", "fa-2x"),
                           "License/How to Cite",
                           value="citeTab"),
                         h5("License Information"),
                         tags$hr(class="hrNav"),
                         tags$div(class="citationsDiv", style="width: 500px;",
                                  tags$table(class="citationsTable",
                                             tags$tr(
                                                 tags$td(rowspan=2, width=45, icon("balance-scale", "fa-2x")),
                                                 tags$td(("All Hector applications are licensed under the")
                                                 ),
                                                 tags$tr(
                                                     tags$td(
                                                         tags$a(
                                                             h6("GNU General Public License v3.0"), href="https://github.com/JGCRI/hector/blob/master/LICENSE.md", target="blank")
                                                     )
                                                 )
                                             ),
                                             tags$table(class="citationsTable",
                                                        tags$tr(
                                                            tags$td(p("Permissions of this strong copyleft license are conditioned on making available complete source code
                                                               of licensed works and modifications, which include larger works using a licensed work, under the same
                                                               license. Copyright and license notices must be preserved. Contributors provide an express grant of patent rights.")
                                                            )
                                                        )
                                             )
                                  )
                         ),
                         div(class="citationsDiv",
                             tags$span(class="citationsHeader", "Permissions"),
                             tags$ul(class="ul-None",
                                     tags$li(icon("check"),"Commercial use"),
                                     tags$li(icon("check"),"Modification"),
                                     tags$li(icon("check"),"Distribution"),
                                     tags$li(icon("check"),"Patent use"),
                                     tags$li(icon("check"),"Private use"))
                         ),
                         div(class="citationsDiv",
                             tags$span(class="citationsHeader", "Limitations"),
                             tags$ul(class="ul-None",
                                     tags$li(icon("times")," Liability"),
                                     tags$li(icon("times")," Warranty"))
                         ),
                         div(class="citationsDiv",
                             tags$span(class="citationsHeader", "Conditions"),
                             tags$ul(class="ul-None",
                                     tags$li(icon("info"),"License and copyright notice"),
                                     tags$li(icon("info"),"State changes"),
                                     tags$li(icon("info"),"Disclose source"),
                                     tags$li(icon("info"),"Same license"))
                         ),
                         h5("How to Cite Hector"),
                         tags$hr(class="hrNav"),
                         p("When using graphs, figures, or other output from this applicaton please cite both the Hector Core",
                           br(),
                           "application as well as the Hector User Interface (this application). The DOI for both is provided below:"),
                         tags$ul(
                             tags$li(
                                 h5(tags$a("Hector User Interface DOI", href="https://doi.org/10.5281/zenodo.3603216", target = "blank"),
                                    tags$img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.3603216.svg", alt="DOI", class = "imgNoPadding"))),
                             tags$li(
                                 h5(tags$a("Hector Core DOI", href="https://doi.org/10.5281/zenodo.3515153", target = "blank"),
                                    tags$img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.3515153.svg", alt="DOI", class = "imgNoPadding")))
                         )
                     ),
                     # Feedback Tab Panel
                     tabPanel
                     (
                         p(icon("comment", "fa-2x"), "Support", value="feedbackTab"),
                         h5("To contact the Hector team regarding any questions, concerns, suggestions, or problems, please use",
                           a("Github issues.", href = "https://github.com/JGCRI/hectorui/issues/new/choose"))
                     )
                 )
             )
             )
  ),
  hr(),
  includeHTML("./components/layout/footer.html")
)

server <- function(input, output, session) {
  r6 <- HectorInputs$new() # r6 class
  r6_tracking <- HectorInputs$new() # separate r6 class for carbon tracking
  r6_custom <- HectorInputs$new()
  observeEvent(input$launch_scenario, updateTabsetPanel(session, "nav", selected = "Explore Hector"), ignoreInit = TRUE)

  run_server("run_1", r6 = r6)
  summary_server("summary_1", r6 = r6)
  download_server("download_1", r6 = r6)
  tracking_server("tracking_1")
  custom_server("custom_1", r6 = r6_custom)
}

# Run the application
shinyApp(ui = ui, server = server)
