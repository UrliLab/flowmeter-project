# Urli Lab Pressure-drop Flowmeter

## Authors

- Urli M., Lambert M.-C., and Périé C.

## Info

- **E-mail**: [urli.morgane@uqam.ca](mailto:urli.morgane@uqam.ca)
- **Website**: [https://www.morganeurli.com](https://www.morganeurli.com)

## What is the Flowmeter Project?

This repository provides detailed information on the firmware and software (R Shiny application) required to assemble the electronic box of a pressure-drop flowmeter, adapted from Sack et al. (2011). The flowmeter can be used to measure flow rates based on the pressure drop within a system, and subsequently determine the hydraulic conductivity of various organs (e.g., stem, leaf, or root). Our software is designed for stems but can be modified to accommodate other organs.

Please note that this repository **does not include** the protocol for using the flowmeter or the R Shiny application. These protocols are **currently under submission** to the Prometheus Protocols website ([https://prometheusprotocols.net/](https://prometheusprotocols.net/)).

## Installation

### Electronic Box of the Flowmeter

To assemble the electronic box, follow the instructions provided in the repository, particularly in the `firmware`, `pcb`, and `mechanical` folders.

### Software

The repository includes the software necessary to control and interact with the flowmeter. Make sure to install the following dependencies:

- **R** (version 4.2.1 or higher)
- **Additional libraries**:
  
```r
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

## How to Use

Once the hardware is assembled and the software is installed, you can begin using the flowmeter following the step-by-step usage protocol available once the protocol is published on Prometheus.

## Licence

This repository uses the following licenses:

- **GPL-3.0 License** for the software (including code for controlling the flowmeter and interfacing with the firmware).
- **CC BY-SA License** for the documentation, including assembly instructions and explanatory content.


## Citation

If you use this repository in your research or project, please cite the following:

- Urli, M., Lambert, M.-C., & Périé, C. (2025). *Urli Lab Pressure-drop Flowmeter*. [GitHub Repository]. Available at: [https://github.com/UrliLab/flowmeter-project](https://github.com/UrliLab/flowmeter-project)




