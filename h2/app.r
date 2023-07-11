
ui <- fluidPage(
    includeCSS("./components/layout/style copy.css"),
    navbarPage(id = "nav", title = "", collapsible = TRUE,
        tabPanel(title = "Home",
            includeHTML("./components/layout/homepage.html")
        ),
        tabPanel(title = "Guides",
            ),
        tabPanel(title = "Explore Hector",
            sidebarPanel(
                width = 4
            ),
            h4("Summary"),
            plot_ui("x")
            ),
        tabPanel(title = "About"
            )
        )
)

server <- function(input, output, session) {
    plot_server("x")
}

# Run the application
shinyApp(ui = ui, server = server)
