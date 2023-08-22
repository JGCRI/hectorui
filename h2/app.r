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
            # sidebarPanel(
            #   run_ui("run_1"), # buttons and sliders
            #   width = 4
            # ),
            # h4("Summary"),
            # summary_ui("summary_1"), # print summary
            # graph_ui("graph_1") # plot
            fluidRow(
              column(4,
                     wellPanel(
                       run_ui("run_1")
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
  
    r6 <- HectorInputs$new()
    
    run_server("run_1", r6=r6)
    summary_server("summary_1", r6=r6)
    graph_server("graph_1", r6=r6)
}

# Run the application
shinyApp(ui = ui, server = server)
