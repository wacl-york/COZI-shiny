library(shiny)
library(shinyjs)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(lubridate)

# File where clean data is stored
DATA_FN <- "../vuetest/data/data.csv"

# Height of each individual plot in px
FACET_HEIGHT = 100

plot_data <- function(data, daterange, background_shading) {
    if (daterange == 'week') {
        x_axis_break <- '1 day'
        x_axis_minor_break <- '12 hours'
    } else if (daterange == 'all') {
        x_axis_break <- '1 week'
        x_axis_minor_break <- '1 day'
    } 
    
    plt <- data %>%
        ggplot()
    
    # Only add shading in for weekly data
    if (daterange == 'week') {
        plt <- plt + geom_rect(aes(xmin=xmin, 
                                   xmax=xmax, 
                                   ymin=ymin, 
                                   ymax=ymax),
                               alpha=0.2,
                               data=background_shading)
    }
    
    plt <- plt + 
            geom_line(aes(x=timestamp, y=value)) +
            facet_wrap(~measurand, ncol=1, scales="free_y",
                       strip.position = "left") +
            scale_x_datetime(date_breaks=x_axis_break,
                             date_labels = "%d %b %Y",
                             date_minor_breaks = x_axis_minor_break,
                             limits=c(as.POSIXct(as_date(min(data$timestamp))),
                                      as.POSIXct(now())),
                            expand=c(0,0)) +
            theme_bw() +
            theme(axis.title = element_blank(),
                  strip.placement = "outside",
                  strip.background = element_blank(),
                  panel.background = element_rect(fill = "transparent", color=NA), # bg of the panel
                  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                  panel.grid.major.y = element_line(colour='black', size=0.1),
                  #panel.grid.minor.x = element_line(colour='black', size=0.05),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text.x = element_text(size=12, angle=45, hjust=1),
                  axis.text.y = element_text(size=10),
                  strip.text = element_text(size=12),
                  panel.spacing.y = unit(1.5, "lines")
                 )
    plt
}

server <- function(input, output) {
        
    # Load raw data and all available measurands
    df <- read.csv(DATA_FN)
    df$timestamp <- as_datetime(df$timestamp)
    all_measurands <- unique(df$measurand)
    all_measurands_no_units <- gsub("\ .*", "", all_measurands)
    
    # Create second dataset with last week's data
    week_ago <- now(tzone="UTC") - days(7)
    df_week <- df %>%
        filter(timestamp >= as_datetime(week_ago))
    
    # Generate background shading by day
    start_date <- as_date(week_ago)
    day_shading <- do.call('rbind', lapply(seq(1, 5, by=2), function(i) {
        data.frame(xmin=as_datetime(start_date + days(i)),
                   xmax=as_datetime(start_date + days(i+1)),
                   ymin=-Inf, ymax=Inf)
    }))
    # Add shading for today
    day_shading <- rbind(day_shading,
                         data.frame(xmin=as_datetime(today()),
                                    xmax=as_datetime(now()),
                                    ymin=-Inf,
                                    ymax=Inf))
    
    # Data's loaded
    hide("loading_page")
    show("main_content")
    
    df_plot <- reactive({
        req(input$daterange)
        req(input$measurandscheckbox)
        if (is.null(df) || is.null(df_week)) {
            return(NULL)
        }
    
        
        # Filter to selected date range
        if (input$daterange == 'week') {
            to_plot <- df_week
        } else if (input$daterange == 'all') {
            to_plot <- df
        } else {
            print(sprintf("Unknown date range option '%s'.", input$daterange))
        }
        
        # Filter to selected measurands
        to_plot %>%
            filter(measurand %in% input$measurandscheckbox)
    })

    output$timeseries <- renderPlot({
        plot_data(df_plot(), input$daterange, day_shading)
    }, bg="transparent")
    
    output$plotui <- renderUI({
        # Height of plot changes dynamically by number of selected measurands
        req(input$measurandscheckbox)
        withSpinner(plotOutput("timeseries", height = FACET_HEIGHT * length(input$measurandscheckbox)),
                    color="#28a745")
    })
    
    output$selectmeasurands <- renderUI({
        checkboxGroupInput("measurandscheckbox",
                           "Pollutants to display",
                           choiceNames=all_measurands_no_units,
                           choiceValues=all_measurands,
                           selected=all_measurands,
                           inline=TRUE)
    })
}
