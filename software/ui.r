#installation des packages
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
      'PEEKtubeCalibration', 
      tabName = 'etape1', 
      icon = icon("thermometer")
    ),
    menuItem(
      "PressureSensorCalibration",
      tabName = 'etape2',
      icon = icon("cog")
    ),
    menuItem(
      "PressureSensorCalibration(lecture)",
      tabName = 'etape2i',
      icon = icon("cogs")
    ),
    menuItem(
      "SingleKmeasurement",
      tabName = "etape3",
      icon = icon("object-ungroup")
    ),
    menuItem(
      "SingleKmeasurement(lecture)",
      tabName = "etape3i",
      icon = icon("object-group")
    ),
    menuItem(
      "ODS",
      tabName = "etape4",
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
      "Fermer l'application",
      style ="float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4"
    ),
    br(),    br(),
    tags$div(HTML("<h5>
                  V2024-08
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
# etape1 ------------------------------------------------------------------
       #############################
       tabItem(
         tabName = 'etape1',
        h2(strong("Étape1")),
        shinydashboard::box(width = NULL,
             background = "black",
             height = 2),
         br()
      ), #tabItem etape1

# etape2 ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'etape2',
        h2(strong("PressureSensorCalibration")),
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
                  div(timeInput(inputId = "heure", label = "Heure:", value = Sys.time(),seconds = F), style = "font-size:70%"),
                  div(textInput(inputId = "operateur", label = "Opérateur:"), style = "font-size:70%"),
                  div(textInput(inputId = "xylem", label = "Xylem:"), style = "font-size:70%"),
                  #div(numericInput(inputId="hauteur", label="Hauteur (cm):", 5, min = 5, max = 100, step=5), style = "font-size:70%"),
                  div(textInput("breaks", "Hauteurs (cm, séparées par une virgule,\n ex: 5,10,15,20,25,30,35,40,45,50): ", placeholder = "Entrer les hauteurs..."), style = "font-size:70%"),
                  div(textAreaInput("pasted", "Copier les données ici:"), style = "font-size:70%"),
                  tags$script("
                Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                Shiny.onInputChange(variableName, null);});
                ")
                  ),
                  br(),
                  actionButton("recalc", "Soumettre", icon("paper-plane"), style =
                                 "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4"),

                  actionButton("resetAll", "Réinitialiser")
                ),
                
                conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                 tags$div(
                                   img(src = "sablier.gif", height = "40"),
                                   h1(strong("Cela peut prendre un certain temps..."),
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
                        
                   br(),downloadButton("downloadData", "Téléchargez le résultat")
                          )
              ) #box
       ), #tabItem etape2
      
# etape2 lecture ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'etape2i',
        h2(strong("PressureSensorCalibration(lecture)")),
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
              p("Les Fichiers de la calibration sont dans le dossier: xylem\\Shiny\\OUTPUTS\\CALIBRATION"),
              shinyjs::useShinyjs(),
              div(id = "myapp",
                  shinyFilesButton('folder', 'Sélectionner un fichier de calibration', 'Sélectionner un fichier de calibration', FALSE)#,
                  #div(dateInput(inputId = "dateL", label = "Date:", value = NULL), style = "font-size:70%"),
                  #div(timeInput(inputId = "heureL", label = "Heure:", value = Sys.time(),seconds = F), style = "font-size:70%"),
                  #div(textInput(inputId = "operateurL", label = "Opérateur:"), style = "font-size:70%"),
                  #div(textInput(inputId = "xylemL", label = "Xylem:"), style = "font-size:70%")
              ),
              br(),
              actionButton("recalcL", "Soumettre", icon("paper-plane"), style =
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
      ), #tabItem etape2i
      
# etape3 ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'etape3',
        h2(strong("SingleKmeasurement")),
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
                    p("Les Fichiers sont dans le dossier: xylem\\Shiny\\OUTPUTS\\SINGLE\\projet\\appareil"),
                    splitLayout(cellWidths = c("50%", "50%"), 
                                {div(shinyFilesButton('folderPTm', 'Sélectionner un fichier de PEEKtubeCalibration', 'Sélectionner un fichier de PEEKtubeCalibration', FALSE))
                                }, 
                                {div(shinyFilesButton('folderPSm', 'Sélectionner un fichier de Preasure Sersors Calibration', 'Sélectionner un fichier de Preasure Sersors Calibration', FALSE))
                                } ),

                    fluidRow(
                      column(2, selectInput('peek_sel', label = 'Couleur', choices = 'Pas encore de choix', width="200px")),
                      column(2, div(textInput(inputId = "parm7", label = "Appareil:"), style = "font-size:70%")),
                      column(2, div(textInput(inputId = "T1_oC", label = "T1 (oC):"), style = "font-size:70%")),
                      column(2, div(textInput(inputId = "T2_oC", label = "T2 (oC):"), style = "font-size:70%"))),
                    fluidRow(
                      #column(2, selectInput('parm1', label = 'expérience (dossier):', c("EA2_E1V2"="EA2_E1V2",
                      #                                                                  "INFILTRATION"="INFILTRATION",
                      #                                                                  "DESPA"="DESPA",
                      #                                                                  "kmax_colo"="kmax_colo",
                      #                                                                  "exp_Morgan"="exp_Morgan"), width="200px"), style = "font-size:70%"),
                      column(2, selectInput('parm1', label = 'expérience (dossier):', choices = 'Pas encore de choix', width="200px"), style = "font-size:70%"),
                      
                      #column(2, div(textInput(inputId = "parm1", label = "expérience (dossier):"), style = "font-size:70%")),
                      #column(2, selectInput('parm2', label = 'nom du mesureur:', c("KG"="KG",
                      #                                                             "KTH"="KTH",
                      #                                                             "MC"="MC",
                      #                                                             "ID"="ID",
                      #                                                             "ND"="ND",
                      #                                                             "HM"="HM"
                      #                                                          ), width="200px"), style = "font-size:70%"),
                      column(2, selectInput('parm2', label = 'nom du mesureur:', choices = 'Pas encore de choix', width="200px"), style = "font-size:70%"),
                      
                      #column(2, div(textInput(inputId = "parm2", label = "nom du mesureur:"), style = "font-size:70%")),
                      column(2, selectInput('parm3', label = 'parm3:', choices = 'Pas encore de choix', width="200px"), style = "font-size:70%"),
                      #column(2, selectInput('parm3i', label = 'parm3i:', c("ki-colo"="ki-colo",
                      #                                                   "ki-kmaxdessicateur-colo"="ki-kmaxdessicateur-colo",
                      #                                                   "ki-kmaxseringue-colo"="ki-kmaxseringue-colo"), width="200px"), style = "font-size:70%"),
                      column(2, selectInput('parm3i', label = 'parm3i:', choices = 'Pas encore de choix', width="200px"), style = "font-size:70%"),
                      
                      #column(2, div(textInput(inputId = "parm3", label = "espèce:"), style = "font-size:70%")),
                      column(2, div(textInput(inputId = "parm4", label = "identifiant de l'échantillon:"), style = "font-size:70%")),
                      column(2, div(textInput(inputId = "parm5", label = "identifiant de la mesure:"), style = "font-size:70%")),
                      column(2, div(dateInput(inputId = "parm6", label = "date:", value = Sys.time()), style = "font-size:70%"))),
                #fileInput('myfileinput', label = 'Select File', accept = c(".csv")),
                div(textAreaInput("pastedm", "Copier les données ici:"), style = "font-size:70%",height = '50%'),
                div(textAreaInput("commentairem", "Commentaires:"), style = "font-size:70%"),
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
            actionButton("recalcm", "Soumettre", icon("paper-plane"), style =
                           "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4"),
            
            actionButton("resetAllm", "Réinitialiser")
          ),
          conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                           tags$div(
                             img(src = "sablier.gif", height = "40"),
                             h1(strong("Cela peut prendre un certain temps..."),
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
                    downloadButton("downloadDatam", "Téléchargez le résultat")
          )
            
        ) #box
      ), #tabItem etape3

# etape3 lecture ------------------------------------------------------------------
#############################
tabItem(
  tabName = 'etape3i',
  h2(strong("SingleKmeasurement(lecture)")),
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
      p("Les Fichiers sont dans le dossier: xylem\\Shiny\\OUTPUTS\\SINGLE"),
      shinyjs::useShinyjs(),
      div(id = "myapp3i",
          shinyFilesButton('folder3i', 'Sélectionner un fichier de mesure', 'Sélectionner un fichier de mesure', FALSE)#,
          #div(dateInput(inputId = "dateL", label = "Date:", value = NULL), style = "font-size:70%"),
          #div(timeInput(inputId = "heureL", label = "Heure:", value = Sys.time(),seconds = F), style = "font-size:70%"),
          #div(textInput(inputId = "operateurL", label = "Opérateur:"), style = "font-size:70%"),
          #div(textInput(inputId = "xylemL", label = "Xylem:"), style = "font-size:70%")
      ),
      br(),
      actionButton("recalcL3i", "Soumettre", icon("paper-plane"), style =
                     "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4"),
      
      #shinyjs::useShinyjs(),
      #shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
      actionButton("refresh", "Réinitialiser"),
      br(),
      br(),
      p("Ouvrir le fichier, faire la modification des paramètres et cliquer sur Renommer les fichiers"),
      actionButton("renamedata", "Renommer les fichiers")
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
      #downloadButton("downloadDatamL", "Téléchargez le résultat")
    )
  ) #box
), #tabItem etape3i
      
# etape4 ------------------------------------------------------------------
      #############################
      tabItem(
        tabName = 'etape4',
        h2(strong("Lecture des fichiers SingleKmeasurement")),
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
            #p("Répertoire qui contient tout les sous-dossiers avec les fichiers SingleKmeasurement: (ex: xylem app/xylem/Shiny/OUTPUTS/SINGLE)"),
            #shinyjs::useShinyjs(),
            #div(id = "myapp4",
            #  shinyDirButton('folder4', 'Sélectionner le dossier de mesure', 'Sélectionner le dossier de mesure', FALSE)
            #),
            textInput(inputId = "DIR", label = "Répertoire qui contient les fichiers SingleKmeasurement: (ex: OUTPUTS/SINGLE)"),
            br(),                                                                         
            actionButton("recalc4", "Soumettre", icon("paper-plane"), style =
                           "float:right; color: #fff; background-color: #8FBFDB; border-color: #2e6da4")
          ),
          
          mainPanel(textOutput("text"))
          ) #box
      ), #tabItem etape4
      
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
              title = "XYLEM",
              #dashboardSidebar(disable = TRUE),
              sidebard,
              skin = "blue",
              body)
