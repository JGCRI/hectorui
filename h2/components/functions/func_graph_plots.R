

graph_plots <- function(r6) {

    if (r6$save == TRUE) {
        #browser()

        {ggplot(last(r6$output)) +
                geom_line(aes(x = year, y = value, color = Scenario)) +
                labs(x = "Year", y = last(r6$output)$variable[1],
                     title = paste0("Run Name: ",  last(r6$output)$run[1], "\n", "Variable: ", last(r6$output)$variable[1])) +
                theme(legend.position = "bottom")} %>%
            plotly::ggplotly()

    } else if(r6$save == FALSE) {

       {ggplot(r6$no_save_output) +
            geom_line(aes(x = year, y = value, color = Scenario)) +
            labs(x = "Year", y = r6$no_save_output$variable[1],
                 title = paste0("Run Name: Unsaved Run\n", "Variable: ", r6$no_save_output$variable[1]))} %>%
            plotly::ggplotly() %>%
            layout(
                legend = list(
                    orientation = 'h', x = 0
                )
            )

    }
}

