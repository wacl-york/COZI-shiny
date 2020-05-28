library(shiny)
library(shinydashboard)

disclaimer <- "<p>Disclaimer: this is raw uncalibrated reference data, please contact <a href='https://www.york.ac.uk/chemistry/staff/eotechs/kread/'>Katie Read</a> for further details.</p>"
footer <- tags$footer("Footer", align = "center", 
             style = "
                      * {
                          margin: 0;
                      }
                      html, body {
                          height: 100%;
                      }
                      .wrapper {
                          min-height: 100%;
                          margin: 0 auto -300px; /* the bottom margin is the negative value of the footer's height */
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
        menuItem("Live data", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("About", icon = icon("th"), tabName = "about")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
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
                div(style = "padding: 10px 10px;",
                    uiOutput("plotui"),
                )
        ),
        tabItem(tabName = "about",
                h2("About"),
                p("Text description of COZI, what this data represents, etc..."),
                HTML(disclaimer),
                p("Branding could go here. WACL logo, NERC logo, University logo?"),
                p("Or would branding be better in a footer so it appears on every page? Or on the right hand side of top bar? Or on bottom of sidebar?")
        )
    )
)

# Put them together into a dashboardPage
ui <- tagList(
        dashboardPage(
                dashboardHeader(title = "COZI Reference Data"),
                sidebar,
                body,
                skin="green"
            ),
        #footer TODO This is how can add footer when/if needed
)
