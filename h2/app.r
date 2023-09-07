source("./global.r")

ui <- fluidPage(
    includeCSS("./components/layout/style copy.css"),
    navbarPage(id = "nav", title = "", collapsible = TRUE,
        tabPanel(title = "Home",
            includeHTML("./components/layout/homepage.html")
        ),
        tabPanel(title = "Guides",
            ),
        tabPanel(title = "Explore Hector",
            fluidRow(
              column(2,
                     wellPanel(
                       run_ui("run_1"),
                       download_ui("download_1")
                     )
              ),
              column(4,
                     summary_ui("summary_1")
                     ),
              column(4,
                     graph_ui("graph_1")
              )
            )
            ),
        tabPanel(title = "About"
            )
        ),
)

server <- function(input, output, session) {
  
    r6 <- HectorInputs$new() # r6 class
    i <- reactiveVal(1) # set up pseudo loop
    
    run_server("run_1", r6=r6, i=i)
    summary_server("summary_1", r6=r6, i=i)
    graph_server("graph_1", r6=r6, i=i)
    download_server("download_1", r6=r6)
}

# Run the application
shinyApp(ui = ui, server = server)