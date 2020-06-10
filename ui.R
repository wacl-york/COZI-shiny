library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)

# Replaces the script used in shinycssloaders::withSpinner with our edited version
modified_spinner <- function(input_tags) {
    input_tags[[2]] <- tags$script(src="spinner_modified.js")
    input_tags
}

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
footer <- tags$footer("Footer", align = "center", 
             style = "
                      * {
                          margin: 0;
                      }
                      html, body {
                          height: 100%;
                      }
                      .wrapper { min-height: 100%; margin: 0 auto -300px; /* the bottom margin is the negative value of the footer's height */
                      }
                      .footer, .push {
                        height: 300px; /* .push must be the same height as .footer */
                      }
                    
                      /*

                      Sticky Footer by Ryan Fait
                      http://ryanfait.com/
                    
                      */"
                      )


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Live data", tabName = "dashboard", icon = icon("chart-bar")),
        menuItem("About", icon = icon("info"), tabName = "about")
    )
)

body <- dashboardBody(
    useShinyjs(),
    tabItems(
        tabItem(tabName = "dashboard",
                div(
                    id="loading_page",
                        h1("Loading data...", align="center")
                ),
                hidden(

                    div(id="missing_data",
                        uiOutput("missing_data_text")
                        )
                ),
                hidden(
                    div(
                        id="main_content",
                        fluidRow(
                                radioButtons(
                                    "daterange",
                                    "Date range",
                                    c("Previous week" = "week",
                                      "All data" = "all"),
                                    inline=TRUE,
                                    width="100%",
                                ),
                            align="center"
                        ),
                        fluidRow(
                            uiOutput("selectmeasurands"),
                            align="center",
                        ),
                        # Point the spinner JS at our modified version that correctly
                        # doesn't stop spinning before the plots are loaded
                        modified_spinner(withSpinner(uiOutput("plotui"),
                                                     color = "#28a745"))
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
                                              title = "WACL", height = "30px"),
                                          style = "padding-top:10px; padding-bottom:10px;"),
                                        class = "dropdown"),
                                tags$li(a(href = 'https://www.york.ac.uk/',
                                          img(src = 'logos/uoy.png',
                                              title = "University of York", height = "30px"),
                                          style = "padding-top:10px; padding-bottom:10px;"),
                                        class = "dropdown"),
                                tags$li(a(href = 'https://amof.ac.uk/',
                                          img(src = 'logos/amof.png',
                                              title = "AMOF", height = "30px"),
                                          style = "padding-top:10px; padding-bottom:10px;"),
                                        class = "dropdown"),
                                tags$li(a(href = 'https://www.ncas.ac.uk/en/',
                                          img(src = 'logos/ncas.png',
                                              title = "NCAS", height = "30px"),
                                          style = "padding-top:10px; padding-bottom:10px;"),
                                        class = "dropdown")),
                sidebar,
                body,
                skin="green"
            ),
        #footer TODO This is how can add footer when/if needed
)
