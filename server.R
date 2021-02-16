library(shiny)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(data.table)
library(openair)
library(cowplot)
library(grid)

# File where clean data is stored
DATA_FN <- "/mnt/shiny/cozi/data.csv"

UNITS <- list("O3"="ppbV", 
              "NOx"="ppbV",
              "CO"="ppbV",
              "CO2"="ppmV",
              "CH4"="ppmV",
              "Temperature"="°C",
              "Relative humidity"="%",
              "Wind speed"="ms¹")

# Variables are grouped into 4: 
MET_TIME_SERIES_VARS <- c('Temperature', 'Relative humidity', 'Wind speed')
NOX_VARS <- c('NO', 'NO2')

# Earliest timepoint to plot in the full time series
FIRST_TIMEPOINT <- as_date("2020-04-08")

# Calculates date 7 days ago
week_ago <- function() {
    as_date(now(tzone="UTC") - days(7))
}

plot_NOx <- function(data, daterange, unit="", display_x_labels=FALSE) {
    melted <- melt(data, id.vars="timestamp", measure.vars=NOX_VARS)
    plt <- plot_data_var(melted, 'value', daterange, display_x_labels=display_x_labels, unit=unit)
    plt$layers[[2]] <- NULL
    
    plt +
        ylab(sprintf("%s (%s)", "NOx", unit)) +
        scale_color_manual("", values=c('#a0a0a0', 'black')) +
        geom_line(aes(x=timestamp, y=value, color=variable), na.rm=TRUE) +
        theme(legend.background = element_rect(fill = "transparent", color=NA),
              legend.key = element_rect(fill = "transparent", color=NA),
              legend.position=c(0.90, 0.85),
              legend.text = element_text(size=10)) 
}

plot_data_var <- function(data, var, daterange, unit="", display_x_labels=FALSE) {
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
        
    if (unit != "") {
        ylabel <- sprintf("%s (%s)", var, unit)
    } else {
        ylabel <- var
    }
    
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
    
    var <- sym(var)
    plt <- ggplot(data) +
            geom_rect(aes(xmin=start, 
                          xmax=end, 
                          ymin=ymin, 
                          ymax=ymax),
                      alpha=0.2,
                      data=background_shading,
                      na.rm=TRUE
                      ) +
            geom_line(aes(x=timestamp, y=!!var), 
                      colour="black",
                      na.rm=TRUE) +
            scale_x_datetime(date_breaks=x_axis_break,
                             date_labels = x_axis_label_fmt,
                             date_minor_breaks = x_axis_minor_break,
                             limits=c(as.POSIXct(earliest_date),
                                      as.POSIXct(now())),
                            expand=c(0,0)) +
            scale_y_continuous(labels = function(x) round(as.numeric(x), digits=1)) +
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
                  axis.text.y = element_text(size=10),
                  axis.title.y = element_text(size=11, margin=margin(r=20)),
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

deg2rad <- function(deg) {(deg * pi) / (180)}

plot_windrose <- function(d, var, breaks = 16, nbin = 5) {
    # Form bins
    d[, var_bin := cut(get(var), nbin) ]
    d[, var_bin := factor(var_bin, levels=rev(levels(var_bin)), 
                          labels=gsub(",", ", ", rev(levels(var_bin))))]
    
    var <- sym(var)
    ggplot(d, aes(x=`Wind direction`, fill=var_bin)) +
        geom_bar(colour='#333333', na.rm=TRUE) +
        coord_polar(start = deg2rad(360/(2*breaks))) +
        scale_x_continuous(breaks = c(90,180,270,360),
                           limits = c(0+(360/(2*breaks)),360+(360/(2*breaks))),
                           labels = c("    E","\nS","W    ","N\n")) +
        scale_fill_brewer(var, palette = "Dark2") +
        theme(
              panel.background = element_rect(fill = "transparent", color=NA),
              plot.background = element_rect(fill = "transparent", color = NA),
              legend.background = element_rect(fill="transparent", color=NA),
              panel.grid.major = element_line(colour = "#333333", size=0.3),
              panel.grid.minor = element_line(colour = "#333333", size=0.1),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(colour = "black", size = 13, hjust=0),
              legend.title = element_text(colour = "black", size = 13, hjust=0),
              legend.text = element_text(colour = "black", size = 12, hjust=0),
              axis.title = element_blank()
        )
}


server <- function(input, output) {

    if (!file.exists(DATA_FN)) {
        shinyjs::hide("main_content_timeseries")
        shinyjs::show("missing_data_timeseries")
        shinyjs::hide("main_content_spatial")
        shinyjs::show("missing_data_spatial")
        return(NULL)
    }
    
    # Load raw data and all available measurands
    df <- fread(DATA_FN)
    df[, timestamp := as_datetime(timestamp)]
    
    # Remove measurement units
    raw_measurands <- colnames(df)[2:ncol(df)]
    measurands_only <- gsub(" \\(.+", "", raw_measurands)
    setnames(df, old=raw_measurands, new=measurands_only)
    
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
    # NB: Shouldn't _really_ need the !is.na(get(var)) subset, as the geom_line()
    # call has the na.rm=T argument so missing values should be removed.
    # However, the plots wouldn't display correctly unless I explicitly removed
    # missing values at this step. It appears the large amount of missingness
    # caused issues with the plotting rendering
    output$aqbox <- renderPlot({
        data <- data_to_plot()
        plots <- list()
        # Ozone first
        plots[['O3']] <- plot_data_var(data[ !is.na(O3) ], 'O3', input$daterange, unit=UNITS[['O3']])

        # Combined NOx plot
        plots[["NOx"]] <- plot_NOx(data[ !is.na(NO2) ], input$daterange, unit=UNITS[['NOx']])
        
        # Remaining 3 plots. 
        for (var in c('CO', 'CO2', 'CH4')) {
            plots[[var]] <- plot_data_var(data[ !is.na(get(var)) ], var, input$daterange, unit=UNITS[[var]])
        }
        
        # 3 met time series
        for (var in MET_TIME_SERIES_VARS) {
            plots[[var]] <- plot_data_var(data[ !is.na(get(var)) ], var, input$daterange, unit=UNITS[[var]], display_x_labels = var == 'Wind speed')
        }
        
        grobs <- lapply(plots, ggplotGrob)
        # Ugly to have to pass in individual elements, but can't get rbind to work with a list and the size argument
        g <- rbind(grobs[[1]], grobs[[2]], grobs[[3]], grobs[[4]], grobs[[5]], grobs[[6]], grobs[[7]], grobs[[8]], size="first")
        grid.draw(g)
    })
    
    output$windrose <- renderPlot({
        req(input$windrose_var)
        data <- data_to_plot()
        var <- input$windrose_var
        
        if (var == 'Wind speed') {
            windRose(data[ !is.na(get(var)) ], ws="Wind speed", wd="Wind direction")
        } else {
            pollutionRose(data[ !is.na(get(var)) ], ws="Wind speed", wd="Wind direction", pollutant=var)
        }
    })
    
    output$download <- downloadHandler(
        filename = function() {
            sprintf("cozidata_%s_%s.csv", Sys.Date(), input$daterange)
        },
        content = function(file) {
           write_csv(data_to_plot()[order(-timestamp)], file) 
        }
    )
    
    ####### Spatial page
    output$leaflet <- renderLeaflet({
        m <- leaflet()
        m <- addTiles(m)
        m <- addMarkers(m, lat=53.947736, lng=-1.046247, popup="COZI location")
        m
    })
    
}
