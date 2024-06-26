

graph_plots <- function(r6) {

    var <- last(r6$output)$variable[1]
    unit <- last(r6$output)$unit[1]

    if(r6$run_mode == "regular") {

        {ggplot(r6$output) +
                geom_line(aes(x = year, y = value, color = Scenario)) +
                labs(x = "Year", y = paste0(title[[var]], " (", unit, ")"),
                     title = paste0("Variable: ", last(r6$output)$variable[1],"\nPermafrost: ",r6$permafrost, "\n")) +
                theme(legend.position = "bottom")} %>%
            plotly::ggplotly()

    } else if(r6$run_mode == "custom") {

        {ggplot(r6$output) +
                geom_line(aes(x = year, y = value, color = Run)) +
                labs(x = "Year", y = paste0(title[[var]], " (", unit, ")"),
                     title = paste0("Variable: ", last(r6$output)$variable[1], "\n")) +
                theme(legend.position = "bottom")} %>%
            plotly::ggplotly()

    }


}

