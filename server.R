library(shiny)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(data.table)
library(openair)
library(cowplot)

# File where clean data is stored
DATA_FN <- "/mnt/shiny/cozi/data.csv"

# Variables are grouped into 4: 
# AQ Time series, Met Time series, Nox combined time series, and wind rose
AQ_TIME_SERIES_VARS <- c('CO', 'CO2', 'CH4')
MET_TIME_SERIES_VARS <- c('Temperature', 'Relative humidity', 'Wind speed')
NOX_VARS <- c('NO', 'NO2', 'NOx')

# Earliest timepoint to plot in the full time series
FIRST_TIMEPOINT <- as_date("2020-04-08")

# Calculates date 7 days ago
week_ago <- function() {
    as_date(now(tzone="UTC") - days(7))
}

plot_NOx <- function(data, var, daterange, display_x_labels=FALSE) {
    plt <- plot_data_var(data, var, daterange, display_x_labels=display_x_labels)
    plt$layers[[2]] <- NULL
    plt <- plt + 
        geom_line(aes(x=timestamp, y=value, colour=measurand), na.rm=TRUE) +
        scale_colour_brewer("", palette = "Dark2") +
        theme(legend.background = element_rect(fill = "transparent", color=NA),
              legend.key = element_rect(fill = "transparent", color=NA),
              legend.position=c(0.90, 0.85),
              legend.text = element_text(size=10)) 
}

plot_data_var <- function(data, var, daterange, display_x_labels=FALSE) {
    if (daterange == 'week') {
        x_axis_break <- '1 day'
        x_axis_minor_break <- '12 hours'
        x_axis_label_fmt <- "%d %b %H:%S"
        minor_x_gridline <- element_line(colour='black', size=0.05)
        earliest_date <- week_ago()
    } else if (daterange == 'all') {
        x_axis_break <- '1 week'
        x_axis_minor_break <- '1 day'
        x_axis_label_fmt <- "%d %b %Y"
        minor_x_gridline <- element_blank()
        earliest_date <- FIRST_TIMEPOINT
    } else if (daterange == '2 days') {
        x_axis_break <- '1 day'
        x_axis_minor_break <- '12 hours'
        x_axis_label_fmt <- "%d %b %H:%S"
        minor_x_gridline <- element_line(colour='black', size=0.05)
        earliest_date <- as_date(now(tzone="UTC") - days(1))
    }
        
    ylabel <- sprintf("%s (%s)", var, unique(data$unit))
    
    # Generate background shading by alternate day
    num_days_plot <- floor(difftime(today(), earliest_date, units = "days")/2)
    shading_start <- POSIXct(num_days_plot+1)
    shading_start[1] <- today()
    for (i in 1:num_days_plot) {
        shading_start[i+1] <- today() - days(i*2)
    }
    background_shading <- data.frame(start=as_datetime(shading_start),
                                     end = as_datetime(ifelse(shading_start == today(), now(), shading_start + days(1))),
                                     ymin=-Inf,
                                     ymax=Inf)
    
    plt <- ggplot(data) +
            geom_rect(aes(xmin=start, 
                          xmax=end, 
                          ymin=ymin, 
                          ymax=ymax),
                      alpha=0.2,
                      data=background_shading,
                      na.rm=TRUE
                      ) +
            geom_line(aes(x=timestamp, y=value), na.rm=TRUE) +
            scale_x_datetime(date_breaks=x_axis_break,
                             date_labels = x_axis_label_fmt,
                             date_minor_breaks = x_axis_minor_break,
                             limits=c(as.POSIXct(earliest_date),
                                      as.POSIXct(now())),
                            expand=c(0,0)) +
            theme_bw() +
            labs(y=ylabel) +
            theme(axis.title.x = element_blank(),
                  panel.background = element_rect(fill = "transparent", color=NA), # bg of the panel
                  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                  panel.grid.major.y = element_line(colour='black', size=0.1),
                  panel.grid.minor.x = minor_x_gridline,
                  panel.grid.major.x = element_blank(),
                  plot.title=element_text(hjust=0.5, size=15, face="bold"),
                  axis.text.x = element_text(size=12, angle=45, hjust=1),
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=13, margin=margin(r=10)),
                  panel.spacing.y = unit(1.5, "lines")
                 )
            if (!display_x_labels) {
                plt <- plt + theme(
                  axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                )
            }
    
    plt
}


server <- function(input, output) {

    output$missing_data_text <- renderUI({
        h3(sprintf("Error: data file %s could not be found.", DATA_FN), class="missing_data_text")
    })
    
    if (!file.exists(DATA_FN)) {
        shinyjs::hide("main_content")
        shinyjs::show("missing_data")
        return(NULL)
    }
    
    # Load raw data and all available measurands
    df <- fread(DATA_FN)
    df[, timestamp := as_datetime(timestamp)]
    df[, c("measurand", "unit") := tstrsplit(measurand, "\ (", fixed=TRUE)]
    df[, unit := gsub(")", "", unit)]
    
    data_to_plot <- reactive({
        req(input$daterange)
        if (input$daterange == 'week') {
            data <- df[ timestamp >= week_ago() ]
        } else if (input$daterange == 'all') {
            data <- df
        } else {
            return(NULL)
        }
        
        data
    })
    
    # Plot UI element has reactive dependency only on the date range.
    # When the date range is changed, all the measurand plots are created
    output$aqbox <- renderPlot({
        data <- data_to_plot()
        plots <- list()
        # Ozone first
        plots[['O3']] <- plot_data_var(data[ measurand == 'O3' ], 'O3', input$daterange)
        # Combined NOx plot
        plots[["NOx"]] <- plot_NOx(data[ measurand %in% NOX_VARS ], "NOx", input$daterange)
        # Remaining 3 plots. Removed loop as would be less readable to have it in with the exception for x-labels on last plot
        plots[['CO']] <- plot_data_var(data[ measurand == 'CO' ], 'CO', input$daterange)
        plots[['CO2']] <- plot_data_var(data[ measurand == 'CO2' ], 'CO2', input$daterange)
        plots[['CH4']] <- plot_data_var(data[ measurand == 'CH4' ], 'CH4', input$daterange, display_x_labels = TRUE)
        
        grobs <- lapply(plots, ggplotGrob)
        # Ugly to have to pass in individual elements, but can't get rbind to work with a list and the size argument
        g <- rbind(grobs[[1]], grobs[[2]], grobs[[3]], grobs[[4]], grobs[[5]], size="first")
        grid.draw(g)
    })
    
    output$metbox <- renderPlot({
        data <- data_to_plot()
        plots <- list()
        # Met values
        for (var in MET_TIME_SERIES_VARS) {
            if (var == "Wind speed") {
                x_labs <- TRUE
            } else {
                x_labs <- FALSE
            }
            input_dr <- input$daterange
            if (input_dr == 'week') {
                input_dr <- "2 days"
            }
            plots[[var]] <- plot_data_var(data[ measurand == var ], var, input_dr,
                                          display_x_labels = x_labs)
        }
        grobs <- lapply(plots, ggplotGrob)
        # Ugly to have to pass in individual elements, but can't get rbind to work with a list and the size argument
        g <- rbind(grobs[[1]], grobs[[2]], grobs[[3]], size="first")
        grid.draw(g)
    })
    
    output$windrose <- renderPlot({
        data <- data_to_plot()
        plots <- list()
        wind_df <- dcast(data[ grepl("Wind", measurand) ], timestamp ~ measurand)
        setnames(wind_df, old=c('timestamp', 'Wind direction', 'Wind speed'), new=c('timestamp', 'wd', 'ws'))
        windRose(wind_df)$plot
    })
    
}
