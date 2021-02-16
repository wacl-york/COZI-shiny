library(shiny)
library(shinyjs)
library(lubridate)
library(data.table)
library(openair)
library(plotly)

# File where clean data is stored
DATA_FN <- "/mnt/shiny/cozi/data.csv"

# Variables that will be plotted in the time-series
TIME_SERIES_VARS <- list(
    "NO2"=list(unit="ppbV", label="NOx"),
    "O3"=list(unit="ppbV", label="O3"),
    "CO"=list(unit="ppbV", label="CO"),
    "CO2"=list(unit="ppmV", label="CO2"),
    "CH4"=list(unit="ppmV", label="CH4"),
    "Temperature"=list(unit="°C", label="Temp"),
    "RelativeHumidity"=list(unit="%", label="RH"),
    "WindSpeed"=list(unit="ms¹", label="WS")
)

to_camel <- function(x){ 
    capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
    sapply(strsplit(x, "\\ "), function(x) paste(capit(x), collapse=""))
}

# Calculates date 7 days ago
week_ago <- function() {
    as_datetime(now(tzone="UTC") - days(7))
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
    # Set to camel case
    old <- colnames(df)
    new <- to_camel(old)
    setnames(df, old=old, new=new)
    
    # TODO Put in scraper if use
    # Resample to 10 min average
    df[, Timestamp := floor_date(Timestamp, "15 min")]
    df <- df[, lapply(.SD, mean, na.rm=T), by='Timestamp']
    
    output$aqbox <- renderPlotly({
        plots <- lapply(names(TIME_SERIES_VARS), function(var) {
            if (var == 'NO2') {
                p <- plot_ly(df[!is.na(get(var))], 
                        x=~Timestamp,
                        y=~NO2,
                        type='scatter',
                        mode='lines',
                        name=var,
                        legendgroup=as.formula(sprintf("~%s", var)),
                        showlegend=TRUE) %>%
                    add_lines(y=~NO,
                              name = "NO",
                              showlegend=TRUE)
            } else {
                p <- plot_ly(df[!is.na(get(var))], 
                        x=~Timestamp,
                        y=as.formula(sprintf("~%s", var)),
                        type='scatter',
                        mode='lines',
                        name=var,
                        legendgroup=as.formula(sprintf("~%s", var)),
                        line = list(color = 'rgb(93, 104, 250)'),
                        showlegend=FALSE)
            }
            p %>%
                layout(
                    yaxis=list(
                        title=sprintf("%s (%s)", 
                                      TIME_SERIES_VARS[[var]][["label"]], 
                                      TIME_SERIES_VARS[[var]][["unit"]])
                    )
                ) %>%
                toWebGL()
        })
        subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE,
                titleY=TRUE) %>%
            layout(
                dragmode="pan",
                xaxis=list(
                    range=list(
                        week_ago(),
                        now(tzone="UTC")
                    ),
                    rangeslider = list(type = "date"),
                    rangeselector = list(
                        buttons = list(
                            list(
                                count = 7,
                                label = "1 wk",
                                step = "day",
                                stepmode = "backward"),
                            list(
                                count = 3,
                                label = "3 mo",
                                step = "month",
                                stepmode = "backward"),
                            list(
                                count = 6,
                                label = "6 mo",
                                step = "month",
                                stepmode = "backward"),
                            list(
                                count = 1,
                                label = "1 yr",
                                step = "year",
                                stepmode = "backward"),
                            list(
                                count = 1,
                                label = "YTD",
                                step = "year",
                                stepmode = "todate"),
                            list(step = "all")
                            )
                        )
                )
            ) %>%
            config(displayModeBar="static",
                   displaylogo=FALSE,
                   modeBarButtonsToRemove=list(
                       "sendDataToCloud",
                       "toImage",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "zoomIn2d",
                       "zoomOut2d",
                       "toggleSpikelines",
                       "autoScale2d",
                       "select2d",
                       "lasso2d"
                   ))
    })
    
    spatial_data <- reactive({
        req(input$daterange_spatial)
        if (input$daterange_spatial == 'week') {
            data <- df[ Timestamp >= week_ago() ]
            
        } else if (input$daterange_spatial == 'all') {
            data <- df
        } else {
            return(NULL)
        }
        data
    })
    
    output$windrose <- renderPlot({
        req(input$windrose_var)
        data <- spatial_data()
        var <- input$windrose_var
        
        if (var == 'WindSpeed') {
            windRose(data[ !is.na(get(var)) ], ws="WindSpeed", wd="WindDirection")
        } else {
            pollutionRose(data[ !is.na(get(var)) ], ws="WindSpeed", wd="WindDirection", pollutant=var)
        }
    })
    
    ####### Spatial page
    output$leaflet <- renderLeaflet({
        m <- leaflet()
        m <- addTiles(m)
        m <- addMarkers(m, lat=53.947736, lng=-1.046247, popup="COZI location")
        m
    })
    
}

# TODO: