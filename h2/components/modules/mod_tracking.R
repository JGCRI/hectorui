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
      bsPopover(ns("ssp_path"), title="",content="Select a Shared Socioeconomic Pathway to plot.",
                placement = "top", trigger = "hover", options = NULL),
      
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
      
      prettyRadioButtons(ns("plotSelect"), label="Select plot to view:",
                   choices = list("Area Plot"=1,
                                  "Animated Bar Plot"=2),
                   selected=1),
      
      prettyRadioButtons(ns("ff"), label="Toggle fossil fuels:",
                         choices = list("On"=1,"Off"=2)),
      bsPopover(ns("ff"), title="",content="Select whether you want fossil fuels to be included (On), or only non-anthropogenic sources of carbon (Off).",
                placement = "top", trigger = "hover", options = NULL),
      
      conditionalPanel(
        condition = "input.plotSelect == 1",
        prettyRadioButtons(ns("view"), label="View:",
                           choices = list("Carbon Amount"=10,
                                          "Carbon Fraction"=7),
                           selected = 10),
        bsPopover(ns("view"), title="",content="Select whether you want to view the total amounts of carbon in each pool (Carbon Amount), or the fraction each pool has of the total amount in the system (Carbon Fraction).",
                  placement = "top", trigger = "hover", options = NULL),
        ns = NS("tracking_1")
      ),
      actionButton(ns("generate"),"Generate"),
      downloadButton(ns("downloadGif"),"Download Plot"),
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.plotSelect == 1",
        withSpinner(plotlyOutput(ns("fig"))),
        ns = NS("tracking_1")
      ),
      conditionalPanel(
        condition = "input.plotSelect == 2",
        withSpinner(imageOutput(ns("gif"))),
        ns = NS("tracking_1")
      )
    )
  )
}

tracking_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
     # Get shinycssloaders to not appear until plots are generated
    #output$fig <- renderPlotly(NULL)
    #output$gif <- renderImage(NULL)
    
    observe({
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
      
      # fill in any missing pools
      df <- df[order(df$source_name),] # sort by source, then year, so sources are always in same order each year
      df <- df[order(df$year),]
      
      
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
      
      # Plotting
      plotSelect <- reactive({input$plotSelect}) # area or bar
      view <- reactive({input$view}) # fraction or amount
      view <- as.numeric(view())
      #browser()
      
      # Area plot
      if (plotSelect() == 1) {
        
        print("Generating plot...")
        area_plot <-
          plot_ly(
            filter(df, source_name == "HL Ocean"),
            x =  ~ year,
            y = ~ source_amt,
            name = "HL Ocean",
            type = "scatter",
            mode = "none",
            stackgroup = "one",
            fillcolor = "#2C728EFF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "LL Ocean"),
            y = ~ source_amt,
            name = "LL Ocean",
            fillcolor = "#3B528BFF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Intermediate Ocean"),
            y = ~ source_amt,
            name = "Intermediate Ocean",
            fillcolor = "#472D7BFF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Deep Ocean"),
            y = ~ source_amt,
            name = "Deep Ocean",
            fillcolor = "#440154FF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Atmosphere"),
            y = ~ source_amt,
            name = "Atmosphere",
            fillcolor = "#21908CFF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Vegetation"),
            y = ~ source_amt,
            name = "Vegetation",
            fillcolor = "#27AD81FF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Soil"),
            y = ~ source_amt,
            name = "Soil",
            fillcolor = "#5DC863FF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Detritus"),
            y = ~ source_amt,
            name = "Detritus",
            fillcolor = "#AADC32FF"
          )
        area_plot <-
          area_plot %>% add_trace(
            data = filter(df, source_name == "Fossil Fuels"),
            y = ~ source_amt,
            name = "Fossil Fuels",
            fillcolor = "#FDE725FF"
          )
        area_plot <-
          area_plot %>% layout(title = paste0("Amount of Carbon in ", selectedPool(), " by Source"),
                               xaxis = list(title=""),
                               yaxis = list(title="Carbon Pool (Pg C)"))
        
        output$fig <- renderPlotly(area_plot)
        
      } else if (plotSelect() == 2) {
        
        output$gif <- renderImage({
          # Temp file to save output
          outfile <- tempfile(fileext='.gif')
          
          # Make animation
          p <- ggplot(df,aes(fill=source_name,color=source_name,
                             x=reorder(source_name,source_amt),
                             y=source_amt)) +
            geom_bar(stat="identity") +
            geom_text(aes(y=0, label = paste(source_name, " ")),
                      vjust = 0.2, hjust = 1, size = 6) +
            geom_text(aes(y = source_amt, label = paste(" ",amt_lbl), hjust=0), 
                      size = 6) +
            coord_flip(clip = "off", expand = FALSE) +
            theme_void() +
            theme(plot.title = element_text(size=20,face="bold"),
                  plot.subtitle = element_text(size=18),
                  legend.position="none",
                  panel.grid.major.x = element_line(size=.1,color="snow2"),
                  panel.grid.minor.x = element_line(size=.1,color="snow2"),
                  plot.margin = margin(1,6,1,6,"cm")) +
            ylab("Carbon (Pg)") +
            xlab("") +
            scale_fill_viridis_d() +
            scale_color_viridis_d() +
            
            # gganimate
            transition_time(year) +
            ease_aes('linear')
          
          # Animate
          anim <- p + transition_states(year,transition_length=4, 
                                        state_length=2,wrap=FALSE) +
            view_follow(fixed_x = TRUE) +
            labs(title=paste0(selectedPool()," Carbon Sources"),
               subtitle="Year: {closest_state}")
          
          anim_save("outfile.gif", animate(anim, height = 500, width = 800, 
                                           end_pause=30))
          list(src = 'outfile.gif',
               contentType = 'image/gif'
               # width = 800,
               # height = 300,
               # alt = "An animation tracking the sources of carbon in a chosen pool"
          )}, deleteFile = TRUE)
      }
      
    }) %>%
      bindEvent(input$generate)
  })
}