#installation of packages
all_pkgs <-  c("gridExtra","htmltools","zip","stringr","plyr", "zoo", "shiny",   "shinyTime", "shinydashboard", "readODS", "data.table", "shinyjs", "tidyverse", "DT", "shinyFiles", "readODS")

already_installed <- rownames(installed.packages())
to_install <- setdiff(all_pkgs, already_installed)
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE, repos = 'http://cran.us.r-project.org')
}

#packages
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

conflicts_prefer(DT::dataTableOutput)


options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
options(shiny.usecairo = FALSE) #R4.2.0


# header ------------------------------------------------------------------
header <-
  dashboardHeader(title = span(tagList(
    img(src = 'signature-PIV.svg', width = "100px", align = "left"), " ",
    img(src = 'Twitter Header Photo TR.png', width = "100px", align = "left"))),
  titleWidth = 300)

# Sidebard ----------------------------------------------------------------
sidebard <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem(
      "Flow meter application",
      tabName = "info",
      icon = icon("question-circle")
    ),
    menuItem(
      'PEEK tubing calibration', 
      tabName = 'step1', 
      icon = icon("thermometer")
    ),
    menuItem(
      "Pressure sensors calibration",
      tabName = 'step2',
      icon = icon("cog")
    ),
    menuItem(
      "Pressure sensors calibration (reading)",
      tabName = 'step2i',
      icon = icon("cogs")
    ),
    menuItem(
      "Measurement",
      tabName = "step3",
      icon = icon("object-ungroup")
    ),
    menuItem(
      "Measurement (reading)",
      tabName = "step3i",
      icon = icon("object-group")
    ),
    menuItem(
      "Data compilation",
      tabName = "step4",
      icon = icon("tasks")
    ),

    br(),    br(),    br(),    br(),    br(),
    tags$button(
      id = 'close',
      type = "button",
      class = "btn action-button",
      icon("times"),
      onclick = "setTimeout(function(){window.close();},500);",
      # close browser
      "Close the application",
      style ="float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4"
    ),
    br(),    br(),
    tags$div(HTML("<h5>
                  V2024-11
                  </h5>"))
    )
    )


# body --------------------------------------------------------------------
body <- dashboardBody(withMathJax(),
                      #includeCSS("www/custom.css"),
                      tags$head(
                        tags$style(
                          type = "text/css ",
                          "#map {height: calc(100vh - 80px) !important;}",
                          "
                          #loadmessage {
                          position: fixed;
                          top: 70px;
                          left: 0px;
                          width: 100%;
                          padding: 5px 0px 5px 0px;
                          text-align: center;
                          font-weight: bold;
                          font-size: 100%;
                          color: #000000;
                          # background-color: #CCFF66;
                          z-index: 105;
                          }"
                          )
                        ),
                      tags$head(
                        tags$style(HTML("
        .skin-blue .main-header .navbar {
          background-color: #19406C;
        }
        .skin-blue .main-header .logo {
          background-color: #19406C;
        }
        .skin-blue .main-sidebar {
          background-color: #095797;
        }
      "))
                      ),

     tabItems(
# step1 ------------------------------------------------------------------
       #############################
       tabItem(
        tabName = 'step1',
        h1(strong("PEEK tubing calibration")),
        shinydashboard::box(width = NULL,
             background = "orange",
             height = 3),
        fluidPage(
          shinyjs::useShinyjs(),
          id = "myappm1",

          tabsetPanel(
            tabPanel("1- Raw data", DT::DTOutput(outputId = "tablePEEK"),
                     tagList(tags$p('Import raw data and click "Add"')),
                     splitLayout(cellWidths = c("25%", "25%","25%", "25%"), 
                                 {div(textInput(inputId = "IDMEASUREMENT1", label = "Id Measurement:"), style = "font-size:100%")}
                     ),
                     splitLayout(cellWidths = c("25%", "25%","25%", "25%"), 
                                 {div(textInput(inputId = "HeightPEEK", label = "Height (m):"), style = "font-size:100%")}, 
                                 {div(textInput(inputId = "TimePEEK", label = "Time (s):"), style = "font-size:100%")},
                                 {div(textInput(inputId = "VolPEEK", label = "Volume (mL):"), style = "font-size:100%")},
                                 {div(textInput(inputId = "TempPEEK", label = "Temperature of the solution (°C):"), style = "font-size:100%")}
                     ),
                     
                     actionButton("Add", "Add"),
                     actionButton("resetAllPEEK", "Reset")
            ),
            tabPanel("2- Hydraulic resistance", 
                     br(),
                     htmlOutput("txtOutputp1"),
                     plotOutput(outputId = "hist2P"),
                     htmlOutput("txtOutputp2")
            ),
            tabPanel("3- Summary of PEEK tubing resistance values", 
                     br(),
                     tagList(tags$p("Complete the information for the summary of PEEK tubing resistance values:")),
                     br(),
                     splitLayout( 
                       cellWidths = c("20%", "20%", "20%", "20%", "20%"), 
                       {div(textInput(inputId = "PARAMETER1", label = "Parameter 1:"), style = "font-size:70%")},
                       {div(textInput(inputId = "OPERATOR", label = "Operator:"), style = "font-size:70%")},
                       {div(textInput(inputId = "PEEK_tubing_ID", label = "PEEK tubing ID:"), style = "font-size:70%")},
                       {div(textInput(inputId = "DEVICE", label = "Device:"), style = "font-size:70%")}
                     ),
                     splitLayout(cellWidths = c("20%", "20%"), 
                                 verbatimTextOutput(outputId = "tablePEEKR"),
                                 verbatimTextOutput(outputId = "tablePEEKK")
                     ), 
                     {div(shinyFiles::shinyFilesButton('folderPC', 'Select a PEEK tubing file if exist', 'Select a PEEK tubing file if exist', FALSE))}, 
                     actionButton("Add2", "Add"),
                     
                     DT::DTOutput(outputId = "tablePEEKid"),
                     br(),br(),
                     splitLayout(cellWidths = c("20%", "20%"), 
                                 textAreaInput("nomid", label=NULL, value ="Data name for exportation", rows=1),
                                 actionButton("generateButton","Write Data"))
            ),
            tabPanel("4- Average resistance values per PEEK tubing", 
                     br(),
                     splitLayout(cellWidths = c("20%", "20%", "20%"), 
                                 {div(shinyFiles::shinyFilesButton('folderPC5id', 'Select a PEEK tubing file id', 'Select a PEEK tubing file id', FALSE))}, 
                                 {div(shinyFiles::shinyFilesButton('folderPC5', 'Select a PEEK tubing file if exist', 'Select a PEEK tubing file if exist', FALSE))}, 
                                 {div(actionButton("Add5", "Compute mean by PEEK tubing ID"))},
                                 actionButton('reset5', 'Reset Input')
                     ),
                     splitLayout(cellWidths = c("20%", "20%"), 
                                 {div(verbatimTextOutput('rawInputValue41'))},
                                 {div(verbatimTextOutput('rawInputValue42'))}
                     ),
                     br(),
                     DT::DTOutput(outputId = "tablePEEKtub"),
                     br(),
                     splitLayout(cellWidths = c("20%", "20%"), 
                                 textAreaInput("nomid5", label=NULL, value ="Data name for exportation", rows=1),
                                 actionButton("generateButton5","Write Data")
                     )
            )
          ) #tabsetPanel 
        )
      ), #tabItem step1

# step2 ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'step2',
        h1(strong("Pressure sensors calibration")),
        shinydashboard::box(width = NULL,
            background = "orange",
            height = 3),
        shinydashboard::box(
                solidHeader = TRUE,
                width = NULL,
                bstatut = "danger" ,

                column(
                  width = 3,
                  shinyjs::useShinyjs(),
                  div(id = "myapp",
                  div(dateInput(inputId = "date", label = "Date:", value = NULL), style = "font-size:70%"),
                  div(timeInput(inputId = "time", label = "Time:", value = Sys.time(),seconds = F), style = "font-size:70%"),
                  div(textInput(inputId = "operator", label = "Operator:"), style = "font-size:70%"),
                  div(textInput(inputId = "device", label = "Device:"), style = "font-size:70%"),
                  div(textInput("breaks", "Heights (cm, separated by a comma,\n ex: 5,10,15,20,25,30,35,40,45,50): ", placeholder = "Enter the heights..."), style = "font-size:70%"),
                  div(textAreaInput("pasted", "Paste data here:"), style = "font-size:70%")#,
 
                  ),
                  br(),
                  actionButton("recalc", "Submit", icon("paper-plane"), style =
                                 "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4")
                ),
                
                conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                 tags$div(
                                   img(src = "sablier.gif", height = "40"),
                                   h1(strong("This might take a while..."),
                                      id = "loadmessage")
                                 )),
                
                mainPanel(br(),div(DT::dataTableOutput("table"), style = "font-size:70%"),
                          br(),div(DT::dataTableOutput("slope2"), style = "font-size:70%"),
                          div(DT::dataTableOutput("slope1"), style = "font-size:70%"),
                          br(),mainPanel(
                                  fluidRow(
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("hist1"), plotOutput("hist2"))
                                  )),
                        br(),
                   div(DT::dataTableOutput("arduino"), style = "font-size:70%")
                    )
              ) #box
       ), #tabItem step2
      
# step2 reading ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'step2i',
        h1(strong("Pressure sensors calibration (reading)")),
        shinydashboard::box(width = NULL,
            background = "orange",
            height = 3),
        br(),
        shinydashboard::box(
            solidHeader = TRUE,
            width = NULL,
            bstatut = "danger" ,
            
            column(
              width = 3,
              p("Calibration files are in the folder: flowmeter\\Shiny\\OUTPUTS\\CALIBRATION"),
              shinyjs::useShinyjs(),
              div(id = "myapp",
                  shinyFilesButton('folder', 'Select a calibration file', 'Select a calibration file', FALSE)#,
              ),
              br(),
              actionButton("recalcL", "Submit", icon("paper-plane"), style =
                             "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4")
              ),
            
            mainPanel(br(),div(DT::dataTableOutput("tableL"), style = "font-size:70%"),
                      br(),div(DT::dataTableOutput("slope2L"), style = "font-size:70%"),
                      div(DT::dataTableOutput("slope1L"), style = "font-size:70%"),
                      
                      br(),mainPanel(
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"), plotOutput("hist1L"), plotOutput("hist2L"))
                        )),
                      br(),
                      div(DT::dataTableOutput("arduinoL"), style = "font-size:70%")
            )
          ) #box
      ), #tabItem step2i
      
# step3 ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'step3',
        h1(strong("Measurement")),
        shinydashboard::box(width = NULL,
            background = "orange",
            height = 3),
        br(),
        shinydashboard::box(
          solidHeader = TRUE,
          width = NULL,
          bstatut = "danger" ,
          column(
            width = 12,
            shinyjs::useShinyjs(),
            div(id = "myappm",

                  fluidRow(
                    p("Files are in the folder: flowmeter\\Shiny\\OUTPUTS\\SINGLE\\Experiment\\Device"),
                    tags$style(type='text/css', '#rawInputValue1 {background-color: #DAE6F0; color: #4A98D9;}'),
                    tags$style(type='text/css', '#rawInputValue2 {background-color: #DAE6F0; color: #4A98D9;}'),
                    splitLayout(cellWidths = c("50%", "50%"), 
                                {div(shinyFilesButton('folderPTm', 'Select a PEEK tubing calibration file', 'Select a PEEK tubing calibration file', FALSE))
                                }, 
                                {div(shinyFilesButton('folderPSm', 'Select a Pressure sensors calibration file', 'Select a Pressure sensors calibration file', FALSE))
                                }
                                ),
                    splitLayout(cellWidths = c("50%", "50%"), 
                                {div(verbatimTextOutput('rawInputValue1'))},
                                {div(verbatimTextOutput('rawInputValue2'))}
                    ),

                    fluidRow(
                      column(2, selectInput('PEEK_sel', label = 'PEEK tubing ID', choices = 'No choices yet', width="200px")),
                      column(2, div(textInput(inputId = "parm8", label = "Device:"), style = "font-size:70%")),
                      column(2, div(textInput(inputId = "T1_oC", label = "T1 (oC):"), style = "font-size:70%"))),
                    fluidRow(
                      column(2, selectInput('parm1', label = 'Parameter 1:', choices = 'No choices yet', width="200px"), style = "font-size:70%"),
                      column(2, selectInput('parm2', label = 'Parameter 2:', choices = 'No choices yet', width="200px"), style = "font-size:70%"),
                      column(2, selectInput('parm3', label = 'Parameter 3:', choices = 'No choices yet', width="200px"), style = "font-size:70%"),
                      column(2, selectInput('parm4', label = 'Parameter 4:', choices = 'No choices yet', width="200px"), style = "font-size:70%"),
                      column(2, div(textInput(inputId = "parm5", label = "Parameter 5:"), style = "font-size:70%")),
                      column(2, div(textInput(inputId = "parm6", label = "Parameter 6:"), style = "font-size:70%")),
                      column(2, div(dateInput(inputId = "parm7", label = "Date:", value = Sys.time()), style = "font-size:70%"))),
                div(textAreaInput("pastedm", "Paste data here:"), style = "font-size:70%",height = '50%'),
                div(textAreaInput("Comments", "Comments:"), style = "font-size:70%"),
                splitLayout(cellWidths = c("15%", "15%", "15%", "15%"), 
                            
                            {div(textInput(inputId = "T2_oC", label = "T2 (oC):"), style = "font-size:70%")
                              },
                            {div(numericInput(inputId="Stemdiameter1", label="Stem diameter 1 (mm):", value=NA, min = 0, max = 100), style = "font-size:70%")
                            }, 
                            {div(numericInput(inputId="Stemdiameter2", label="Stem diameter 2 (mm):", value=NA, min = 0, max = 100), style = "font-size:70%")
                            },
                            {div(numericInput(inputId="Stemlength", label="Stem length (mm):", value=NA, min = 0, max = 100), style = "font-size:70%")                 
                            }
                            ) 
                )
            ), 

            actionButton("recalcm", "Submit", icon("paper-plane"), style =
                           "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4"),
            actionButton("resetAllm", "Reset")
          ),
          conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                           tags$div(
                             img(src = "sablier.gif", height = "40"),
                             h1(strong("This might take a while..."),
                                id = "loadmessage")
                           )),
          mainPanel(
                    div(DT::dataTableOutput("table4m1"), style = "font-size:70%", width = 6),
                    div(DT::dataTableOutput("table4m2"), style = "font-size:70%", width = 6),
                    br(),
                    div(DT::dataTableOutput("arduinom"), style = "font-size:70%", width = 6),
                    br(),
                    div(DT::dataTableOutput("table1m"), style = "font-size:70%", width = 6),
                    br(),
                    div(DT::dataTableOutput("table2m"), style = "font-size:70%", width = 6),
                    br(),
                    div(DT::dataTableOutput("table3m"), style = "font-size:70%", width = 6),
                    br(),
                    div(plotOutput("scatter"), style = "font-size:70%", width = 6),
                    div(plotOutput("scatter_step3"), style = "font-size:70%", width = 6),
                    div(plotOutput("scatter_step3i"), style = "font-size:70%", width = 6)
          )
            
        ) #box
      ), #tabItem step3

# step3 reading ------------------------------------------------------------------
#############################
tabItem(
  tabName = 'step3i',
  h1(strong("Measurement (reading)")),
  shinydashboard::box(width = NULL,
      background = "orange",
      height = 3),
  br(),
  shinydashboard::box(
    solidHeader = TRUE,
    width = NULL,
    bstatut = "danger" ,
    
    column(
      width = 4,
      p("Files are in the folder: flowmeter\\Shiny\\OUTPUTS\\SINGLE"),
      shinyjs::useShinyjs(),
      tags$style(type='text/css', '#rawInputValue11 {background-color: #DAE6F0; color: #4A98D9;}'),
      div(id = "myapp3i",
          shinyFilesButton('folder3i', 'Select a measurement file', 'Select a measurement file', FALSE),
          verbatimTextOutput('rawInputValue11')
      ),
      br(),
      actionButton("recalcL3i", "Submit", icon("paper-plane"), style =
                     "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4"),
    br(),
    br(),
    p("To rename a file: Open the file, change parameters (parm1 to parm8 or comments) and click on Rename files"),
    actionButton("renamedata", "Rename files")
    ),
    
    mainPanel(
      div(DT::dataTableOutput("INPUTL1"), style = "font-size:70%", width = 6),
      div(DT::dataTableOutput("INPUTL2"), style = "font-size:70%", width = 6),
      br(),
      div(DT::dataTableOutput("table4mL1"), style = "font-size:70%", width = 6),
      div(DT::dataTableOutput("table4mL2"), style = "font-size:70%", width = 6),
      br(),
      div(DT::dataTableOutput("arduinomL"), style = "font-size:70%", width = 6),
      br(),
      div(DT::dataTableOutput("table1mL"), style = "font-size:70%", width = 6),
      br(),
      div(DT::dataTableOutput("table2mL"), style = "font-size:70%", width = 6),
      br(),
      div(DT::dataTableOutput("table3mL"), style = "font-size:70%", width = 6),
      br(),
      div(plotOutput("scatterL"), style = "font-size:70%", width = 6),
      div(plotOutput("scatterL_step3"), style = "font-size:70%", width = 6),
      div(plotOutput("scatterL_step3i"), style = "font-size:70%", width = 6)#,
    )
  ) #box
), #tabItem step3i
      
# step4 ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'step4',
        h1(strong("Reading Measurement files")),
        shinydashboard::box(width = NULL,
            background = "orange",
            height = 3),
        br(),
        shinydashboard::box(
          solidHeader = TRUE,
          width = NULL,
          bstatut = "danger" ,
           column(
            width = 3,
            textInput(inputId = "DIR", label = "Directory containing Measurement files: (e.g., OUTPUTS/SINGLE)"),
            br(),                                                                         
            actionButton("recalc4", "Submit", icon("paper-plane"), style =
                           "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4")
          ),
          
          mainPanel(textOutput("text"))
          ) #box
      ), #tabItem step4
      
# info ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'info',
        h1(strong("Flow meter application")),
        shinydashboard::box(width = NULL,
            background = "orange",
            height = 3),
        br(),
        h4(strong("Références :")),
        tags$div(
          HTML("<h4>
             <a href = 'https://github.com/UrliLab/flowmeter-project'
             target='_blank'>https://github.com/UrliLab/flowmeter-project</a>
             </h4>")),
        fluidRow(
          column(6,       tags$div(HTML("<center>
          <a href='https://www.quebec.ca/gouvernement/ministere/ressources-naturelles-forets' target='_blank'>
          <img src='MRNF_couleur.svg' width='250' height='80'>
          </a></center>"))),
          column(6,     tags$div(HTML("<center>
          <a href='https://www.morganeurli.com/' target='_blank'>
          <img src='Twitter Header Photo TR.png' width='250' height='80'>
          </a></center>")))),tags$div(HTML("<center><a href='https://www.quebec.ca/droit-auteur' target='_blank'><br>&copy; Gouvernement du Québec, 2023</a></center>"))
        ) #tabItem info
      
   ) #tabItems
 ) #dashboardBody

# App ---------------------------------------------------------------------
dashboardPage(header,
              title = "FLOW METER",
              #dashboardSidebar(disable = TRUE),
              sidebard,
              #skin = "blue",
              body)
