# Download outputs as a csv file

download_ui <- function(id) {
  ns <- NS(id)
  
  tagList(downloadButton(ns("download"), "Download Outputs"))
}

download_server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = "hectorUI_output.csv",
      content = function(file) {
        write.csv(bind_rows(r6$output), file)
      }
    )
  })
}