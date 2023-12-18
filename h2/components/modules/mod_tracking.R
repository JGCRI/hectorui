tracking_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    sidebarPanel(
      chooseSliderSkin(skin = "Flat", color = "#375a7f"),
      prettyRadioButtons(ns("ssp_path"), label="Select SSP:",
                         choices = list("SSP 1-1.9"="input/hector_ssp119.ini",
                                        "SSP 1-2.6"="input/hector_ssp126.ini",
                                        "SSP 2-4.5"="input/hector_ssp245.ini",
                                        "SSP 3-7.0"="input/hector_ssp370.ini",
                                        "SSP 4-3.4"="input/hector_ssp434.ini",
                                        "SSP 4-6.0"="input/hector_ssp460.ini",
                                        "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
                                        "SSP 5-8.5"="input/hector_ssp585.ini"),
                         selected = "input/hector_ssp245.ini", inline=TRUE,
                         shape = "square", width = "80%"),
      sliderInput(ns("start"), label="Select year to begin tracking:",
                  min = 1750, max = 2200, value = 1900, sep="",step=5),
      selectInput(ns("pool"), label="Select pool to view:",
                  choices = list("High latitude ocean"="HL Ocean",
                                 "Low latitude ocean"="LL Ocean",
                                 "Intermediate ocean"="Intermediate Ocean",
                                 "Deep ocean"="Deep Ocean",
                                 "Atmosphere"="Atmosphere",
                                 "Vegetation"="Vegetation",
                                 "Detritus"="Detritus",
                                 "Soil"="Soil"),
                  selected="Atmosphere"),
      radioButtons(ns("view"), label="View:",
                   choices = list("Carbon Amount"=1,
                                  "Carbon Fraction"=2),
                   selected = 1),
      radioButtons(ns("ff"), label="Toggle fossil fuels:",
                   choices = list("On"=1,"Off"=2)),
      radioButtons(ns("plotSelect"), label="Select plot to view:",
                   choices = list("Area Plot"=1,
                                  "Animated Bar Plot (Carbon Amount Only)"=2),
                   selected=1),
      actionButton(ns("generate"),"Generate"),
      downloadButton(ns("downloadGif"),"Download Plot"),
    ),
    mainPanel(
      withSpinner(plotlyOutput(ns("fig")),type=7)
    )
  )
}

tracking_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observe({
      #browser()
      # Run Hector w/ carbon tracking
      ini_file <- reactive({system.file(input$ssp_path,package="hector")})
      core <- newcore(ini_file())
      tunits <- getunits(TRACKING_DATE())
      setvar(core, NA, TRACKING_DATE(), input$start, tunits)
      reset(core, core$reset_date)
      print("Running Hector...")
      run(core, runtodate = 2300)
      print("Gathering data...")
      df <- get_tracking_data(core)
      
      # clean up pool names
      df[df=="atmos_co2"] <- "Atmosphere"
      df[df=="deep"] <- "Deep Ocean"
      df[df=="detritus_c"] <- "Detritus"
      df[df=="earth_c"] <- "Fossil Fuels"
      df[df=="HL"] <- "HL Ocean"
      df[df=="intermediate"] <- "Intermediate Ocean"
      df[df=="LL"] <- "LL Ocean"
      df[df=="soil_c"] <- "Soil"
      df[df=="veg_c"] <- "Vegetation"
      
      ## filter df to just selected pool
      selectedPool <- reactive({input$pool})
      df <- filter(df,pool_name==selectedPool())
      # filter out fossil fuels option
      if (input$ff == 2) {
        df <- subset(df, source_name!="Fossil Fuels")
      }
      # filter out permafrost rows (for now -- they're empty)
      df <- subset(df, source_name!="permafrost_c")
      df <- subset(df, source_name!="thawedp_c")
      
      # bar width for area plot
      #barwidth <- reactive({(2100-input$start)/50})
      #browser()
      # rank column for moving bar plot
      df <- df %>%
        group_by(year) %>%
        mutate(rank = rank(-source_fraction),
               frac_rel = source_fraction/source_fraction[rank==1],
               source_amt = source_fraction*pool_value,
               amt_lbl = paste0(format(round(source_amt,2), nsmall=2), " Pg C"),
               frac_lbl = paste0(format(round(source_fraction,2),nsmall=2))) %>%
        group_by(source_name) %>%
        ungroup()

      #browser()
      
      # Plotting
      plotSelect <- reactive({input$plotSelect})
      
      # Area plot
      if (plotSelect() == 1) {
        print("Generating plot...")
        area_plot <-
          plot_ly(
            filter(df, source_name == "HL Ocean"),
            x =  ~ year,
            y =  ~ source_amt,
            name = "HL Ocean",
            type = "scatter",
            mode = "none",
            stackgroup = "one",
            fillcolor = "#2C728EFF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "LL Ocean"),
            y =  ~ source_amt,
            name = "LL Ocean",
            fillcolor = "#3B528BFF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Intermediate Ocean"),
            y =  ~ source_amt,
            name = "Intermediate Ocean",
            fillcolor = "#472D7BFF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Deep Ocean"),
            y =  ~ source_amt,
            name = "Deep Ocean",
            fillcolor = "#440154FF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Atmosphere"),
            y =  ~ source_amt,
            name = "Atmosphere",
            fillcolor = "#21908CFF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Vegetation"),
            y =  ~ source_amt,
            name = "Vegetation",
            fillcolor = "#27AD81FF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Soil"),
            y =  ~ source_amt,
            name = "Soil",
            fillcolor = "#5DC863FF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Detritus"),
            y =  ~ source_amt,
            name = "Detritus",
            fillcolor = "#AADC32FF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Fossil Fuels"),
            y =  ~ source_amt,
            name = "Fossil Fuels",
            fillcolor = "#FDE725FF"
          )
        area_plot <-
          area_plot %>% layout(title = paste0("Amount of Carbon in ", selectedPool(), " by Source"),
                               xaxis = list(title=""),
                               yaxis = list(title="Carbon Pool (Pg C)"))
        
        output$fig <- renderPlotly(area_plot)
        
      } else if (plotSelect() == 2) {

        bar_plot <-
          plot_ly(
            df,
            y =  ~ source_name,
            x =  ~ source_amt,
            type = "bar",
            orientation = "h",
            frame =  ~ year
            #color =  ~ source_name
          )
        
        output$fig <- renderPlotly(bar_plot)
        
      }
      
      
      
    }) %>%
      bindEvent(input$generate)
  })
}