source("./global.r")

ui <- fluidPage(theme = shinythemes::shinytheme("readable"),
  includeCSS("./components/layout/style copy.css"),
  tags$div(class = "container",
           tags$img(src = "images/earth-header.png", height = "300px", width = "100%", class = "earth", alt = "Earth's atmosphere"),
           tags$div(
               a(
                   img(src = "images/GCIMS_logo_alt.svg", class = "logo"), href = "https://gcims.pnnl.gov/", target = "_blank"),
               h1("HectorUI", class = "header-text-title"),
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
    tabPanel(title = "Guides",
             mainPanel(
                 style="vertical-align: middle;",
                 h3("Ready to get started?",
                    tags$a("View the Guide/Tutorial", href="https://jgcri.github.io/hectorui/articles/Tutorial.html", target="blank")),
                 br(),
                 HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/fBHXS7pjZcI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')

             )
             ),
    tabPanel(title = "Run Hector",
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
    tabPanel(title = "About")
  ),
  hr(),
  includeHTML("./components/layout/footer.html")
)

server <- function(input, output, session) {
  r6 <- HectorInputs$new() # r6 class
  r6_tracking <- HectorInputs$new() # separate r6 class for carbon tracking
  observeEvent(input$launch_scenario, updateTabsetPanel(session, "nav", selected = "Run Hector"), ignoreInit = TRUE)

  run_server("run_1", r6 = r6)
  summary_server("summary_1", r6 = r6)
  graph_server("graph_1", r6 = r6)
  download_server("download_1", r6 = r6)
  tracking_server("tracking_1")
}

# Run the application
shinyApp(ui = ui, server = server)
