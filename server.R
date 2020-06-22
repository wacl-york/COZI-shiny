library(shiny)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(data.table)
library(openair)

# File where clean data is stored
DATA_FN <- "/mnt/shiny/cozi/data.csv"

# Variables are grouped into 4: 
# AQ Time series, Met Time series, Nox combined time series, and wind rose
AQ_TIME_SERIES_VARS <- c('O3', 'CO', 'CO2', 'CH4')
MET_TIME_SERIES_VARS <- c('Temperature', 'Relative humidity', 'Wind speed')
NOX_VARS <- c('NO', 'NO2', 'NOx')

# Height of each individual plot in px
FACET_HEIGHT = 170

# Earliest timepoint to plot in the full time series
FIRST_TIMEPOINT <- as_date("2020-04-08")

# Calculates date 7 days ago
week_ago <- function() {
    as_date(now(tzone="UTC") - days(7))
}

generate_plot_id <- function(var) {
    sprintf("plot_%s", gsub("\ .*", "", var))
}

# Creates the div containing the plot.
# The code wouldn't generate a different plot in the loop
# unless the renderPlot() call was refactored outside of the 
# reactive context
create_plot_div <- function(data, var, daterange) {
    plt <- plot_data_var(data, var, daterange)
    if (is.null(plt)) {
        renderUI(p(sprintf("Error: cannot display plot for %s.", var),
                      class="missing_data_text"))
    } else {
        renderPlot(plt, bg="transparent", height=FACET_HEIGHT)
    }
}

# As with create_plot_div, this functionality needs to be separate from
# the code used to actually generate the plot, else plots
# won't be rendered correctly
create_nox_plot_div <- function(data, var, daterange) {
    plt <- plot_data_var(data, var, daterange)
    if (is.null(plt)) {
        renderUI(p(sprintf("Error: cannot display plot for %s.", var),
                      class="missing_data_text"))
    } else {
        plt$layers[[2]] <- NULL
        plt <- plt + 
            geom_line(aes(x=timestamp, y=value, colour=measurand), na.rm=TRUE) +
            scale_colour_brewer("", palette = "Dark2") +
            theme(legend.background = element_rect(fill = "transparent", color=NA),
                  legend.key = element_rect(fill = "transparent", color=NA),
                  legend.position=c(0.95, 0.85),
                  legend.text = element_text(size=10)) 
        renderPlot(plt, bg="transparent", height=FACET_HEIGHT)
    }
}

# won't be rendered correctly
create_windrose_div <- function(data) {
    plt <- windRose(data)
    if (is.null(plt$plot)) {
        renderUI(p(sprintf("Error: cannot display plot for %s.", var),
                      class="missing_data_text"))
    } else {
        renderPlot(plt$plot, bg="transparent", height=FACET_HEIGHT*2)
    }
}
    

plot_data_var <- function(data, var, daterange) {
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
            labs(y=ylabel, title=var) +
            theme(axis.title.x = element_blank(),
                  panel.background = element_rect(fill = "transparent", color=NA), # bg of the panel
                  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                  panel.grid.major.y = element_line(colour='black', size=0.1),
                  panel.grid.minor.x = minor_x_gridline,
                  panel.grid.major.x = element_blank(),
                  plot.title=element_text(hjust=0.5, size=15, face="bold"),
                  axis.text.x = element_text(size=12),#, angle=45, hjust=1),
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=13, margin=margin(r=10)),
                  panel.spacing.y = unit(1.5, "lines")
                 )
    plt
}


server <- function(input, output) {

    output$missing_data_text <- renderUI({
        h3(sprintf("Error: data file %s could not be found.", DATA_FN), class="missing_data_text")
    })
    
    if (!file.exists(DATA_FN)) {
        shinyjs::hide("loading_page")
        shinyjs::show("missing_data")
        return(NULL)
    }
    
    # Load raw data and all available measurands
    df <- fread(DATA_FN)
    df[, timestamp := as_datetime(timestamp)]
    df[, c("measurand", "unit") := tstrsplit(measurand, "\ (", fixed=TRUE)]
    df[, unit := gsub(")", "", unit)]
    
    # TODO Turn this into previous_plotted = sapply(get_div_id, c(TIME_SERIES_VARS, NOx, WindRose))
    all_measurands <- unique(df$measurand)
    previous_plotted <- all_measurands
    
    # When a measurand checkbox is checked or unchecked, 
    # toggle the corresponding plot div
    observeEvent(input$measurandscheckbox, {
        selected <- input$measurandscheckbox
        differences <- c(setdiff(selected, previous_plotted),
                         setdiff(previous_plotted, selected))
        
        for (var in differences) {
            toggleElement(generate_plot_id(var))
        }
        previous_plotted <<- selected
    }, ignoreNULL = F)
    
    # Plot UI element has reactive dependency only on the date range.
    # When the date range is changed, all the measurand plots are created
    output$plotui <- renderUI({
        output_tags <- tagList()
        
        req(input$daterange)
        if (input$daterange == 'week') {
            data <- df[ timestamp >= week_ago() ]
        } else if (input$daterange == 'all') {
            data <- df
        } else {
            return(NULL)
        }
        
        # O3 first
        var <- 'O3'
        div_name <- generate_plot_id(var)
        plt_tag <- div(id=div_name, 
                       create_plot_div(data[ measurand == var ], var, input$daterange),
                       style="padding-bottom: 20px;")
        
        if (!var %in% previous_plotted) {
            plt_tag <- hidden(plt_tag)
        }
        output_tags <- tagAppendChild(output_tags, plt_tag)
        
        # Combined NOx plot
        div_name <- generate_plot_id("NOx_combined")
        # TODO more efficient way of doing this %in%? maybe join?
        plt_tag <- div(id=div_name, 
                       create_nox_plot_div(data[ measurand %in% NOX_VARS ], "NOx", input$daterange),
                       style="padding-bottom: 20px;")
        
        if (!var %in% previous_plotted) {
            plt_tag <- hidden(plt_tag)
        }
        output_tags <- tagAppendChild(output_tags, plt_tag)
        
        # Straight forward time series
        for (var in AQ_TIME_SERIES_VARS) {
            div_name <- generate_plot_id(var)
            plt_tag <- div(id=div_name, 
                           create_plot_div(data[ measurand == var ], var, input$daterange),
                           style="padding-bottom: 20px;")
            
            if (!var %in% previous_plotted) {
                plt_tag <- hidden(plt_tag)
            }
            output_tags <- tagAppendChild(output_tags, plt_tag)
        }
        
        # Met values
        for (var in MET_TIME_SERIES_VARS) {
            div_name <- generate_plot_id(var)
            plt_tag <- div(id=div_name, 
                           create_plot_div(data[ measurand == var ], var, input$daterange),
                           style="padding-bottom: 20px;")
            
            if (!var %in% previous_plotted) {
                plt_tag <- hidden(plt_tag)
            }
            output_tags <- tagAppendChild(output_tags, plt_tag)
        }
        
        # Wind rose
        # TODO Cast dataframe to wide
        wind_df <- dcast(data[ grepl("Wind", measurand) ], timestamp ~ measurand)
        setnames(wind_df, old=c('timestamp', 'Wind direction', 'Wind speed'), new=c('timestamp', 'wd', 'ws'))
        wind_df
        
        div_name <- generate_plot_id("Wind rose")
        plt_tag <- div(id=div_name, 
                       create_windrose_div(wind_df),
                       style="padding-bottom: 20px;")
        output_tags <- tagAppendChild(output_tags, plt_tag)
        
       output_tags
    })
    
    # Data has loaded
    shinyjs::hide("loading_page")
    shinyjs::show("main_content")
    
}
