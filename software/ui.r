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
conflicts_prefer(DT::dataTableOutput)

options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
options(shiny.usecairo = FALSE) #R4.2.0

# header ------------------------------------------------------------------
header <-
  dashboardHeader(title = span(img(
    src = 'MFFPw3.gif', width = "100px", align = "left"
  )))

# Sidebard ----------------------------------------------------------------
sidebard <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      'Peek-tube calibration', 
      tabName = 'step1', 
      icon = icon("thermometer")
    ),
    menuItem(
      "Pressure sensor calibration",
      tabName = 'step2',
      icon = icon("cog")
    ),
    menuItem(
      "Pressure sensor calibration (reading)",
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
    menuItem(
      "Informations",
      tabName = "info",
      icon = icon("question-circle")
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
                  V2024-10
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

     tabItems(
# step1 ------------------------------------------------------------------
       #############################
       tabItem(
         tabName = 'step1',
        h2(strong("Peek-tube calibration")),
        shinydashboard::box(width = NULL,
             background = "black",
             height = 2),
         br()
      ), #tabItem step1

# step2 ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'step2',
        h2(strong("Pressure sensor calibration")),
        shinydashboard::box(width = NULL,
            background = "black",
            height = 2),
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
                  #div(numericInput(inputId="height", label="Height (cm):", 5, min = 5, max = 100, step=5), style = "font-size:70%"),
                  div(textInput("breaks", "Heights (cm, separated by a comma,\n ex: 5,10,15,20,25,30,35,40,45,50): ", placeholder = "Enter the heights..."), style = "font-size:70%"),
                  div(textAreaInput("pasted", "Paste data here:"), style = "font-size:70%"),
                  tags$script("
                Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                Shiny.onInputChange(variableName, null);});
                ")
                  ),
                  br(),
                  actionButton("recalc", "Submit", icon("paper-plane"), style =
                                 "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4"),

                  actionButton("resetAll", "Reset")
                ),
                
                conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                 tags$div(
                                   img(src = "sablier.gif", height = "40"),
                                   h1(strong("This might take a while..."),
                                      id = "loadmessage")
                                 )),
                
                mainPanel(br(),div(DT::dataTableOutput("table"), style = "font-size:70%"),
                          br(),div(DT::dataTableOutput("pente"), style = "font-size:70%"),
                        
                          br(),mainPanel(
                                  fluidRow(
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("hist1"), plotOutput("hist2"))
                                  )),
                        br(),
                   div(DT::dataTableOutput("arduino"), style = "font-size:70%"),
                        
                   br(),downloadButton("downloadData", "Download the results")
                          )
              ) #box
       ), #tabItem step2
      
# step2 reading ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'step2i',
        h2(strong("Pressure sensor calibration (reading)")),
        shinydashboard::box(width = NULL,
            background = "black",
            height = 2),
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
                  #div(dateInput(inputId = "dateL", label = "Date:", value = NULL), style = "font-size:70%"),
                  #div(timeInput(inputId = "timeL", label = "Time:", value = Sys.time(),seconds = F), style = "font-size:70%"),
                  #div(textInput(inputId = "operatorL", label = "Operator:"), style = "font-size:70%"),
                  #div(textInput(inputId = "deviceL", label = "Device:"), style = "font-size:70%")
              ),
              br(),
              actionButton("recalcL", "Submit", icon("paper-plane"), style =
                             "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4")
              ),
            
            mainPanel(br(),div(DT::dataTableOutput("tableL"), style = "font-size:70%"),
                      br(),div(DT::dataTableOutput("penteL"), style = "font-size:70%"),
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
        h2(strong("Measurement")),
        shinydashboard::box(width = NULL,
            background = "black",
            height = 1),
        br(),
        shinydashboard::box(
          solidHeader = TRUE,
          width = NULL,
          bstatut = "danger" ,
          column(
            width = 12,
            shinyjs::useShinyjs(),
            div(id = "myappm",
               # mainPanel(#"main panel",
                  fluidRow(
                    p("Files are in the folder: flowmeter\\Shiny\\OUTPUTS\\SINGLE\\project\\device"),
                    splitLayout(cellWidths = c("50%", "50%"), 
                                {div(shinyFilesButton('folderPTm', 'Select a Peek-tube calibration file', 'Select a Peek-tube calibration file', FALSE))
                                }, 
                                {div(shinyFilesButton('folderPSm', 'Select a Pressure Sensors Calibration file', 'Select a Pressure Sensors Calibration file', FALSE))
                                } ),

                    fluidRow(
                      column(2, selectInput('peek_sel', label = 'Peek-tube ID', choices = 'No choices yet', width="200px")),
                      column(2, div(textInput(inputId = "parm7", label = "Device:"), style = "font-size:70%")),
                      column(2, div(textInput(inputId = "T1_oC", label = "T1 (oC):"), style = "font-size:70%")),
                      column(2, div(textInput(inputId = "T2_oC", label = "T2 (oC):"), style = "font-size:70%"))),
                    fluidRow(
                      column(2, selectInput('parm1', label = 'Experiment (file):', choices = 'No choices yet', width="200px"), style = "font-size:70%"),
                      column(2, selectInput('parm2', label = 'Name of the operator:', choices = 'No choices yet', width="200px"), style = "font-size:70%"),
                      column(2, selectInput('parm3', label = 'Parameter3:', choices = 'No choices yet', width="200px"), style = "font-size:70%"),
                      column(2, selectInput('parm3i', label = 'Parameter3i:', choices = 'No choices yet', width="200px"), style = "font-size:70%"),
                      column(2, div(textInput(inputId = "parm4", label = "Sample ID:"), style = "font-size:70%")),
                      column(2, div(textInput(inputId = "parm5", label = "Measurement ID:"), style = "font-size:70%")),
                      column(2, div(dateInput(inputId = "parm6", label = "Date:", value = Sys.time()), style = "font-size:70%"))),
                div(textAreaInput("pastedm", "Paste data here:"), style = "font-size:70%",height = '50%'),
                div(textAreaInput("commentairem", "Comments:"), style = "font-size:70%"),
                splitLayout(cellWidths = c("15%", "15%", "15%"), 
                            {div(numericInput(inputId="Stemdiameter1", label="Stem diameter 1 (mm):", value=NA, min = 0, max = 100), style = "font-size:70%")
                            }, 
                            {div(numericInput(inputId="Stemdiameter2", label="Stem diameter 2 (mm):", value=NA, min = 0, max = 100), style = "font-size:70%")
                            },
                            {div(numericInput(inputId="Stemlength", label="Stem length (mm):", value=NA, min = 0, max = 100), style = "font-size:70%")                 
                            }
                            ) 
                )#)
            ), 
            #br(),
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
                    #div(DT::dataTableOutput("tablem"), style = "font-size:70%", width = 6),
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
                    #div(plotOutput("scatter_step2"), style = "font-size:70%", width = 6),
                    #div(plotOutput("scatter2"), style = "font-size:70%", width = 6),
                    div(plotOutput("scatter_step3"), style = "font-size:70%", width = 6),
                    div(plotOutput("scatter_step3i"), style = "font-size:70%", width = 6),
                    downloadButton("downloadDatam", "Download the results")
          )
            
        ) #box
      ), #tabItem step3

# step3 reading ------------------------------------------------------------------
#############################
tabItem(
  tabName = 'step3i',
  h2(strong("Measurement (reading)")),
  shinydashboard::box(width = NULL,
      background = "black",
      height = 2),
  br(),
  shinydashboard::box(
    solidHeader = TRUE,
    width = NULL,
    bstatut = "danger" ,
    
    column(
      width = 3,
      p("Files are in the folder: flowmeter\\Shiny\\OUTPUTS\\SINGLE"),
      shinyjs::useShinyjs(),
      div(id = "myapp3i",
          shinyFilesButton('folder3i', 'Select a measurement file', 'Select a measurement file', FALSE)#,
          #div(dateInput(inputId = "dateL", label = "Date:", value = NULL), style = "font-size:70%"),
          #div(timeInput(inputId = "timeL", label = "Time:", value = Sys.time(),seconds = F), style = "font-size:70%"),
          #div(textInput(inputId = "operatorrL", label = "Operator:"), style = "font-size:70%"),
          #div(textInput(inputId = "deviceL", label = "Device:"), style = "font-size:70%")
      ),
      br(),
      actionButton("recalcL3i", "Submit", icon("paper-plane"), style =
                     "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4"),
      
      #shinyjs::useShinyjs(),
      #shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
      actionButton("refresh", "Reset"),
      br(),
      br(),
      p("Open the file, modify the parameters and click Rename files"),
      actionButton("renamedata", "Rename files")
    ),
    
    mainPanel(
      div(DT::dataTableOutput("intrantL1"), style = "font-size:70%", width = 6),
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
      #div(plotOutput("scatterL_step2"), style = "font-size:70%", width = 6),
      #div(plotOutput("scatter2L"), style = "font-size:70%", width = 6),
      div(plotOutput("scatterL_step3"), style = "font-size:70%", width = 6),
      div(plotOutput("scatterL_step3i"), style = "font-size:70%", width = 6),
      #downloadButton("downloadDatamL", "Download the results")
    )
  ) #box
), #tabItem step3i
      
# step4 ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'step4',
        h2(strong("Reading Measurement files")),
        shinydashboard::box(width = NULL,
            background = "black",
            height = 2),
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
        h2(strong("INFORMATIONS")),
        shinydashboard::box(width = NULL,
            background = "black",
            height = 2),
        br()
        ) #tabItem info
      
   ) #tabItems
 ) #dashboardBody

# App ---------------------------------------------------------------------
dashboardPage(header,
              title = "FLOWMETER",
              #dashboardSidebar(disable = TRUE),
              sidebard,
              skin = "blue",
              body)
