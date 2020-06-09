library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(gridExtra)

# File where clean data is stored
DATA_FN <- "clean.csv"

# Height of each individual plot in px
FACET_HEIGHT = 170

generate_plot_id <- function(var) {
    sprintf("plot_%s", gsub("\ .*", "", var))
}

# Creates the div containing the plot.
# The code wouldn't generate a different plot in the loop
# unless the renderPlot() call was refactored outside of the 
# reactive context
create_plot_div <- function(data, var, daterange) {
    plt <- plot_data_var(data, var, daterange)
    renderPlot(plt, bg="transparent", height=FACET_HEIGHT)
}
    

plot_data_var <- function(data, var, daterange) {
    if (daterange == 'week') {
        x_axis_break <- '1 day'
        x_axis_minor_break <- '12 hours'
    } else if (daterange == 'all') {
        x_axis_break <- '1 week'
        x_axis_minor_break <- '1 day'
    } 
    df_plt <- data %>%
            filter(measurand == var)
        
    ylabel <- paste(var, unique(df_plt$unit), sep=" ")
    
    # Generate background shading by alternate day
    start_date <- as_date(min(df_plt$timestamp))
    num_days_plot <- floor(difftime(today(), start_date, units = "days")/2)
    shading_start <- POSIXct(num_days_plot+1)
    shading_start[1] <- today()
    for (i in 1:num_days_plot) {
        shading_start[i+1] <- today() - days(i*2)
    }
    background_shading <- data.frame(start=shading_start) %>%
        mutate(end = ifelse(start == today(), now(), shading_start + days(1)),
               start = as_datetime(start),
               end = as_datetime(end),
               ymin=-Inf,
               ymax=Inf)
               
    plt <- df_plt %>%
            ggplot() +
            geom_rect(aes(xmin=start, 
                          xmax=end, 
                          ymin=ymin, 
                          ymax=ymax),
                      alpha=0.2,
                      data=background_shading) +
            geom_line(aes(x=timestamp, y=value), na.rm=TRUE) +
            scale_x_datetime(date_breaks=x_axis_break,
                             date_labels = "%d %b %Y",
                             date_minor_breaks = x_axis_minor_break,
                             limits=c(as.POSIXct(as_date(min(df_plt$timestamp))),
                                      as.POSIXct(now())),
                            expand=c(0,0)) +
            theme_bw() +
            labs(y=ylabel, title=gsub("\ .*", "", var)) +
            theme(axis.title.x = element_blank(),
                  panel.background = element_rect(fill = "transparent", color=NA), # bg of the panel
                  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                  panel.grid.major.y = element_line(colour='black', size=0.1),
                  #panel.grid.minor.x = element_line(colour='black', size=0.05),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  plot.title=element_text(hjust=0.5, size=15, face="bold"),
                  axis.text.x = element_text(size=12),#, angle=45, hjust=1),
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=13, margin=margin(r=10)),
                  panel.spacing.y = unit(1.5, "lines")
                 )
    plt
}

server <- function(input, output) {
        
    # Load raw data and all available measurands
    df <- read.csv(DATA_FN) %>%
            mutate(timestamp = as_datetime(timestamp)) %>%
            separate(measurand, c("measurand", "unit"), "\ ")
    all_measurands <- unique(df$measurand)
    previous_plotted <- all_measurands
    
    # Create second dataset with last week's data
    week_ago <- now(tzone="UTC") - days(7)
    df_week <- df %>%
        filter(timestamp >= as_datetime(week_ago))
    
    # When data has loaded can populate the checkbox with the names
    # of the measurands in the loaded data
    output$selectmeasurands <- renderUI({
        checkboxGroupInput("measurandscheckbox",
                           "Pollutants to display",
                           choiceNames=all_measurands,
                           choiceValues=all_measurands,
                           selected=all_measurands,
                           inline=TRUE)
    })
    
    
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
            data <- df_week
        } else if (input$daterange == 'all') {
            data <- df
        } else {
            return(NULL)
        }
        
        for (var in all_measurands) {
            div_name <- generate_plot_id(var)
            plt_tag <- div(id=div_name, create_plot_div(data, var, input$daterange),
                           style="padding-bottom: 20px;")
            
            if (!var %in% previous_plotted) {
                plt_tag <- hidden(plt_tag)
            }
            output_tags <- tagAppendChild(output_tags, plt_tag)
        }
        
       output_tags
    })
    
    # Data has loaded
    shinyjs::hide("loading_page")
    shinyjs::show("main_content")
    
}
