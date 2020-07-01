library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)

PLOT_HEIGHT <- 130
ROSE_HEIGHT <- 400

checkbox_vals <-  c('Wind speed', 'O3', 'NO', 'NO2', 'CO', 'CO2', 'CH4')
checkbox_labels <-  c('Wind rose', 'O3', 'NO', 'NO2', 'CO', 'CO2', 'CH4')

about_text <- "
<p>
This is air quality data from the National Centre for Atmospheric Science (NCAS) COZI* Laboratory at the <a href='https://www.york.ac.uk/chemistry/research/wacl/'>Wolfson Atmospheric Chemistry Laboratories (WACL)</a>, University of York.  
The instruments are housed in COZI and the measurements are made from the WACL rooftop at ~10m through a 1 inch sampling manifold. The data shown here is not final but raw instrument data, and so it may therefore be subject to further calibrations. In addition the COZI is a working instrument characterisation facility and so gaps in the measurements may be necessary when instruments are being used for other routine work.  
</p>
<p>
Please contact <a href='mailto:katie.read@ncas.ac.uk'>Katie Read</a> for more information about these measurements or what the <a href='https://amof.ac.uk/laboratory/carbon-monoxide-and-ozone-calibration-laboratory-cozi/ '>COZI has to offer the community</a>.
Meteorological data from the same location is also available on request.
</p>
<p>
*COZI - Characterisation (e.g. of Ozone measurements) and Intercomparison Laboratory 
</p>
"
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Time series data", tabName = "dashboard", icon = icon("chart-bar")),
        menuItem("Spatial data", tabName = "spatial", icon = icon("compass")),
        menuItem("About", icon = icon("info"), tabName = "about")
    )
)

body <- dashboardBody(
    useShinyjs(),
    tabItems(
        tabItem(tabName = "dashboard",
                hidden(
                    div(
                        id="missing_data",
                        uiOutput("missing_data_text")
                    )
                ),
                div(
                    id="main_content",
                    fluidRow(
                        box(
                            fluidRow(
                                radioButtons(
                                    "daterange",
                                    "Date range",
                                    c("Previous week" = "week",
                                      "All time" = "all"),
                                    inline=TRUE,
                                    width="100%",
                                ),
                            align="center"
                            ),
                            withSpinner(
                                plotOutput("aqbox", height=8*PLOT_HEIGHT),
                                color = "#28a745"
                            ),
                            solidHeader = TRUE,
                            width=12,
                            title="Time series",
                            status="success",
                        ),
                    )
                )
        ),
        tabItem(tabName = "spatial",
                div(
                    id="main_spatial",
                    fluidRow(
                        box(
                            fluidRow(
                                radioButtons(
                                    "daterange_spatial",
                                    "Date range",
                                    c("Previous week" = "week",
                                      "All time" = "all"),
                                    width="100%",
                                    inline=TRUE,
                                ),
                                radioButtons("windrose_var", "Colour by", 
                                             choiceNames=checkbox_labels,
                                             choiceValues=checkbox_vals,
                                             inline = TRUE
                                ),
                                align="center"
                            ),
                            withSpinner(
                                plotOutput("windrose",
                                           height=ROSE_HEIGHT),
                                color = "#28a745"
                            ),
                            solidHeader = TRUE,
                            width=6,
                            title="Meteorological effects",
                            status="success",
                        ),
                        box(
                            withSpinner(
                                    leafletOutput("leaflet")
                            ),
                            solidHeader = TRUE,
                            width=6,
                            title="COZI location",
                            status="primary",
                        )
                    )
                )
        ),
        tabItem(tabName = "about",
                div(id="about",
                    h2("About"),
                    HTML(about_text),
                )
        )
    )
)

# Put them together into a dashboardPage
ui <- tagList(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        dashboardPage(
                dashboardHeader(title = "COZI Reference Data",
                                tags$li(a(href = 'https://www.york.ac.uk/chemistry/research/wacl/',
                                          img(src = 'logos/wacl.png',
                                              title = "WACL", height = "50px")),
                                        class = "dropdown"),
                                tags$li(a(href = 'https://www.york.ac.uk/',
                                          img(src = 'logos/uoy.png',
                                              title = "University of York", height = "50px")),
                                        class = "dropdown"),
                                tags$li(a(href = 'https://amof.ac.uk/',
                                          img(src = 'logos/amof.png',
                                              title = "AMOF", height = "50px")),
                                        class = "dropdown"),
                                tags$li(a(href = 'https://www.ncas.ac.uk/en/',
                                          img(src = 'logos/ncas.png',
                                              title = "NCAS", height = "50px")),
                                        class = "dropdown")),
                sidebar,
                body,
                skin="green"
            ),
)
