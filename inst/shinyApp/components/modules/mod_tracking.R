tracking_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
      sidebarPanel(
          chooseSliderSkin(skin = "Flat", color = "#375a7f"),
          # prettyRadioButtons(ns("ssp_path"), label="Select SSP:",
          #                    choices = list("SSP 1-1.9"="input/hector_ssp119.ini",
          #                                   "SSP 1-2.6"="input/hector_ssp126.ini",
          #                                   "SSP 2-4.5"="input/hector_ssp245.ini",
          #                                   "SSP 3-7.0"="input/hector_ssp370.ini",
          #                                   "SSP 4-3.4"="input/hector_ssp434.ini",
          #                                   "SSP 4-6.0"="input/hector_ssp460.ini",
          #                                   "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
          #                                   "SSP 5-8.5"="input/hector_ssp585.ini"),
          #                    selected = "input/hector_ssp245.ini", inline=TRUE,
          #                    shape = "square", width = "80%"),
          selectInput(ns("ssp_path"), "Baseline Scenario:",
                      choices = list("SSP 1-1.9"="input/hector_ssp119.ini",
                                     "SSP 1-2.6"="input/hector_ssp126.ini",
                                     "SSP 2-4.5"="input/hector_ssp245.ini",
                                     "SSP 3-7.0"="input/hector_ssp370.ini",
                                     "SSP 4-3.4"="input/hector_ssp434.ini",
                                     "SSP 4-6.0"="input/hector_ssp460.ini",
                                     "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
                                     "SSP 5-8.5"="input/hector_ssp585.ini"),
                      width=150, selected = "SSP 2-4.5"),
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

      prettyRadioButtons(ns("ff"), label="Toggle fossil fuels:",
                         choices = list("On"=1,"Off"=2)),
      bsPopover(ns("ff"), title="",content="Select whether you want fossil fuels to be included (On), or only non-anthropogenic sources of carbon (Off).",
                placement = "top", trigger = "hover", options = NULL),
      prettyRadioButtons(ns("view"), label="View:",
                         choices = list("Carbon Amount"=1,
                                        "Carbon Fraction"=2),
                         selected = 1),
      bsPopover(ns("view"), title="",content="Select whether you want to view the total amounts of carbon in each pool (Carbon Amount), or the fraction each pool has of the total amount in the system (Carbon Fraction).",
                placement = "top", trigger = "hover", options = NULL)
    ),
    mainPanel(
        fluidRow(
            actionButton(ns("generate"),"Generate", width = '150px', style = "background: #0B3F8F; color: white;"),
            downloadButton(ns("download"),"Download Plot", style = "background: #B8B8B8; color: black;")
        ),
        fluidRow(
            plotOutput(ns("fig"), width = "80%")#,
            # imageOutput(ns("gif"), width = "80%")
        )
    )
  )
}

tracking_server <- function(id) {
  moduleServer(id, function(input, output, session) {

      observe({
          withProgress(message = "Running Hector", value = 0, {
              # Run Hector w/ carbon tracking
              ini_file <- reactive({system.file(input$ssp_path,package="hector")})
              core <- newcore(ini_file())
              tunits <- getunits(TRACKING_DATE())
              setvar(core, NA, TRACKING_DATE(), input$start, tunits)
              reset(core, core$reset_date)
              print("Running Hector...")
              run(core, runtodate = 2300)

              incProgress(0.25, detail = "Gathering data...")
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

              incProgress(0.25, detail = "Setting variables...")
              ## filter df to just selected pool
              selectedPool <- reactive({input$pool})
              df <- filter(df,pool_name==selectedPool())
              # filter out fossil fuels if selected
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

              # Carbon amount
              if (input$view == 1) {
                  area_plot <-
                      ggplot(df, aes(x=year,y=source_amt,fill=source_name)) +
                      geom_area(stat="identity") +
                      scale_fill_viridis_d(name="Source") +
                      scale_color_viridis_d() +
                      ggtitle(paste0(selectedPool(), " Carbon Amount Sources")) +
                      xlab("") +
                      ylab("Carbon Pool (Pg C)") +
                      theme(plot.title = element_text(size=20),
                            legend.title=element_text(size=16),
                            legend.position="bottom",
                            legend.text=element_text(size=12),
                            axis.title.y=element_text(size=14),
                            axis.text=element_text(size=10),
                            plot.margin = margin(1,1,1,1,"cm"))
                  # save as file
                  ggsave("outfile_area.jpeg",plot=area_plot,device="jpeg",
                         dpi=300, width = 13, units = "in")

              }

              # Carbon fraction
              if (input$view == 2) {

                  print("Generating plot...")
                  area_plot <-
                      ggplot(df, aes(x=year,y=source_fraction,fill=source_name)) +
                      geom_area(stat="identity") +
                      scale_fill_viridis_d() +
                      scale_color_viridis_d() +
                      ggtitle(paste0(selectedPool(), " Carbon Fraction Sources")) +
                      xlab("") +
                      ylab("Carbon Pool (Fraction)") +
                      theme(plot.title = element_text(size=20),
                            legend.title=element_text(size=16),
                            legend.position="bottom",
                            legend.text=element_text(size=12),
                            axis.title.y=element_text(size=14),
                            axis.text=element_text(size=10),
                            plot.margin = margin(1,1,1,1,"cm"))

                  # save as file
                  ggsave("outfile_area.jpeg",plot=area_plot,device="jpeg",
                         dpi=300, width = 13, units = "in")

              }

              output$fig <- renderPlot(area_plot)

              # output$gif <- renderImage({
              #     withProgress(message = "Generating plots", value = 0, {
              #     # Make animation
              #     p <- ggplot(df,aes(fill=source_name,color=source_name,
              #                        x=reorder(source_name,source_amt),
              #                        y=source_amt)) +
              #         geom_bar(stat="identity") +
              #         geom_text(aes(y=0, label = paste(source_name, " ")),
              #                   vjust = 0.2, hjust = 1, size = 6) +
              #         geom_text(aes(y = source_amt, label = paste(" ",amt_lbl), hjust=0),
              #                   size = 6) +
              #         coord_flip(clip = "off", expand = FALSE) +
              #         theme_void() +
              #         theme(plot.title = element_text(size=20),
              #               plot.subtitle = element_text(size=18),
              #               legend.position="none",
              #               panel.grid.major.x = element_line(linewidth=.1,color="snow2"),
              #               panel.grid.minor.x = element_line(linewidth=.1,color="snow2"),
              #               plot.margin = margin(1,6,1,6,"cm")) +
              #         ylab("Carbon (Pg)") +
              #         xlab("") +
              #         scale_fill_viridis_d() +
              #         scale_color_viridis_d() +
              #
              #         # gganimate
              #         transition_time(year) +
              #         ease_aes('linear')
              #     incProgress(0.5, detail = "Rendering (please hold!)")
              #     # Animate
              #     anim <- p + transition_states(year,transition_length=4,
              #                                   state_length=2,wrap=FALSE) +
              #         view_follow(fixed_x = TRUE) +
              #         labs(title=paste0(selectedPool()," Carbon Sources"),
              #              subtitle="Year: {closest_state}")
              #
              #     anim_save("outfile_bar.gif", animate(anim, height = 350, width = 650,
              #                                          end_pause=30, renterer = gifski_renderer()))
              #     incProgress(0.5, detail = "Complete!")
              #
              #     list(src = 'outfile_bar.gif',
              #          contentType = 'image/gif'
              #          # width = 800,
              #          # height = 500,
              #          # alt = "An animation tracking the sources of carbon in a chosen pool"
              #     )
              #     })
              #     }, deleteFile = FALSE)
              Sys.sleep(0.2)
          })
      }) %>%
          bindEvent(input$generate)

    # Download plots
      output$download <- downloadHandler(
          #filename="myplots.zip",
          filename =  function() {
              paste0('HectorUI_CarbonTracking_', format(Sys.time(), "%Y-%m-%d_%H%M%S"), '.zip')
          },
          content=function(file){
              #zip(file,files=c('outfile_area.jpeg','outfile_bar.gif'))
              zip(file, files = 'outfile_area.jpeg')
          }
      )

  })
}
