

graph_plots <- function(r6) {


        {ggplot(r6$output) +
                geom_line(aes(x = year, y = value, color = Scenario)) +
                labs(x = "Year", y = last(r6$output)$variable[1],
                     title = paste0("Variable: ", last(r6$output)$variable[1],"\nPermafrost: ",r6$permafrost)) +
                theme(legend.position = "bottom")} %>%
            plotly::ggplotly()

}

