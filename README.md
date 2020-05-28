# COZI ShinyApp

This repository contains a Shiny web-application that displays the COZI instrumentation data.

# Dependencies

It requires a modern version of R (it has been tested on 4.0.0 but should work on 3.4.x) and the following packages:

  - `dplyr`
  - `shiny`
  - `shinydashboard`
  - `lubridate`
  
It also requires the data to be available in the file "data/data.csv", although in future releases this path may be configurable.
  
# Installation

Clone the repository into a folder that is read by the Shiny web service.
