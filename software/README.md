# Urli Lab Pressure-drop flow-meter mechanical informations


## Info
- E-mail: urli.morgane@uqam.ca
- Website: https://www.morganeurli.com

## Software

The following folder contains the software required to control and interact with the pressure-drop flowmeter. The software is designed using the **R** programming language and utilizes the **R Shiny** framework to provide an interactive application for data visualization and management.

### Software Requirements

To run the software, ensure that you have the following software and packages installed:

- **R** (version 4.2.1 or higher)
- **R Shiny**: This is the primary framework used for building interactive web applications within R.
- The following **R libraries** are required for the application to function:

```r
# List of required libraries
library(conflicted)
library(shiny)
library(shinyTime)
library(shinydashboard)
library(data.table)
library(shinyjs)
library(tidyverse)
library(DT)
library(shinyFiles)
library(fontawesome)
library(readODS)
library(gridExtra)
library(ggplot2)
library(dplyr)
```
For more detailed instructions on how to use the flowmeter and the R Shiny application, refer to the protocol, which is currently under submission to the Prometheus Protocols website (https://prometheusprotocols.net/).
