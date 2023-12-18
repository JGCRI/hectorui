source("./global.r")

ui <- fluidPage(
  includeCSS("./components/layout/style copy.css"),
  navbarPage(
    id = "nav",
    title = "",
    collapsible = TRUE,
    tabPanel(title = "Home",
             includeHTML("./components/layout/homepage.html")),
    tabPanel(title = "Guides"),
    tabPanel(title = "Explore Hector",
             fluidRow(
               run_ui("run_1"),
               #download_ui("download_1"),
             )
    ),
    tabPanel(title = "Carbon Tracking",
             fluidRow(
               tracking_ui("tracking_1")
             )
    ),
    tabPanel(title = "About")
  ),
)

server <- function(input, output, session) {
  r6 <- HectorInputs$new() # r6 class
  r6_tracking <- HectorInputs$new() # r6 class for carbon tracking

  run_server("run_1", r6 = r6)
  summary_server("summary_1", r6 = r6)
  graph_server("graph_1", r6 = r6)
  download_server("download_1", r6 = r6)
  tracking_server("tracking_1")
}

# Run the application
shinyApp(ui = ui, server = server)
