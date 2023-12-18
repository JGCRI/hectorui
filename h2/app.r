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
             includeHTML("./components/layout/homepage.html")),
    tabPanel(title = "Guides"),
    tabPanel(title = "Run Hector",
             fluidRow(
                 run_ui("run_1"),
                 #download_ui("download_1"),
                 )
             ),
    tabPanel(title = p(icon("chart-pie","fa-2x"), "Carbon Tracking", value="outputTab")),
    tabPanel(title = "About")
  ),
  hr(),
  includeHTML("./components/layout/footer.html")
)

server <- function(input, output, session) {
  r6 <- HectorInputs$new() # r6 class

  run_server("run_1", r6 = r6)
  summary_server("summary_1", r6 = r6)
  graph_server("graph_1", r6 = r6)
  download_server("download_1", r6 = r6)
}

# Run the application
shinyApp(ui = ui, server = server)
