# test module

plot_ui <- function(x) {
    verbatimTextOutput((NS(x, "summary")))
}

plot_server <- function(x) {
    moduleServer(x, function(input, output, session) {
        ini_file <- system.file("input/hector_ssp245.ini", package = "hector")

        core <- newcore(ini_file)

        run(core)

        output$summary <- renderPrint({
            fetchvars(core, 2000:2300)
        })
    })
}
