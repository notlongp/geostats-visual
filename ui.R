# UI ====
ui <- navbarPage("Geostatistics in R Shiny",
                 tabPanel("Exploratory Analysis",
                          fluidPage(
                            fluidRow(
                              column(2),
                              column(8, 
                                     plotOutput(outputId = "ggpairs_plot")),
                              column(2)
                            ),
                            
                            fluidRow(
                              column(2, 
                                     selectInput("prop", "Input analyzing property",
                                                 choices = names(df[-c(1,2,3,4)]))),
                              column(3,
                                     plotOutput(outputId = "plot_hist")),
                              column(3,
                                     plotOutput(outputId = "plot_raw")),
                              column(3,
                                     plotOutput(outputId = "plot_log10"))
                              
                            )
                          )
                 ),
                 
                 
                 tabPanel("Geostats",
                          pageWithSidebar(
                            
                            headerPanel(""),
                            
                            sidebarPanel(
                              selectInput("geo.surface", "Select formation top:",
                                          choices = unique(df$F_Top), 
                                          selected = unique(df$F_Top)[1]),
                              
                              selectInput("xlon", "Select X Longitude:",
                                          choices = coord),
                              
                              selectInput("ylat", "Select Y Latitude:",
                                          choices = coord,
                                          selected = coord[2]),
                              
                              selectInput("property", "Select Property:",
                                          choices = names(df[-c(1,2,3,4,5)])),
                              
                              selectInput("dir", "Select the number of directions:",
                                          choices = c("Omnidirectional" = "omni",
                                                      "2-directional" = "2dir",
                                                      "3-directional" = "3dir",
                                                      "4-directional" = "4dir")),
                              
                              selectInput("structure" , "Select fitting structure (Select Terminate to submit):",
                                          choices = melem.name(),
                                          selected = melem.name()[2]),
                              
                              checkboxInput("log_transform", "Perform Log transformation", FALSE),
                              
                              checkboxInput("outlier_filter", "Remove Outliers", FALSE)
                            ),
                            
                            mainPanel(
                              fluidRow(
                                column(6, plotOutput(outputId = "variogram")),
                                column(6, plotOutput(outputId = "fitted_variogram")))
                            )
                          )
                 )
)