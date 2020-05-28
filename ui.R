library(shiny)
library(shinydashboard)

disclaimer <- "<h3>Disclaimer</h3>This is raw uncalibrated reference data, please contact <a href='https://www.york.ac.uk/chemistry/staff/eotechs/kread/'>Katie Read</a> for further details."

# define ui for application that draws a histogram
ui <- dashboardPage(

    dashboardHeader(title = "COZI Reference Data"),
    
    dashboardSidebar(
        sidebarMenu(
            radioButtons(
                "daterange",
                "Date range:",
                c("Previous week" = "week",
                  "All data" = "all")
            ),
            uiOutput("selectmeasurands")
        )
    ),
    
    dashboardBody(
        fluidRow(
            #plotOutput("timeseries", height = "800px"),
            uiOutput("plotui"),
            HTML(disclaimer)
        )
    ),
    skin="green"
)