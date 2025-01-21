library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  id = "myappm1",
  splitLayout(cellWidths = c("25%", "25%","25%", "25%"), 
              {div(textInput(inputId = "HeightPeek", label = "Height (m):"), style = "font-size:70%")}, 
              {div(textInput(inputId = "TimePeek", label = "Time (s):"), style = "font-size:70%")},
              {div(textInput(inputId = "VolPeek", label = "Volume (ml):"), style = "font-size:70%")},
              {div(textInput(inputId = "TempPeek", label = "Temperature of the solution (°C):"), style = "font-size:70%")}
  ),
  
  actionButton("Add", "Add"),
  
  tabsetPanel(
    tabPanel("1- Data", DT::DTOutput(outputId = "tablepeek"),
             actionButton("resetAllpeek", "Réinitialiser")
             ),
    tabPanel("2- Resistance", plotOutput(outputId = "hist1P")),
    tabPanel("3- Conductance hydraulique", plotOutput(outputId = "hist2P")),
    tabPanel("4- Récapitualtif des valeurs de résistance des peek-tubes", 
             br(),
             tagList(tags$p("Completer informations pour le récapitulatif des valeurs de résistance des peek-tubes:")),
             br(),
             splitLayout( 
                        cellWidths = c("20%", "20%", "20%", "20%", "20%"), 
                           {div(textInput(inputId = "CAMPAGNE_MESURE", label = "CAMPAGNE_MESURE:"), style = "font-size:70%")},
                           {div(textInput(inputId = "MESUREUR", label = "MESUREUR:"), style = "font-size:70%")},
                           {div(textInput(inputId = "AN_MESURE", label = "AN_MESURE:"), style = "font-size:70%")},
                           {div(textInput(inputId = "ColorID", label = "ColorID:"), style = "font-size:70%")},
                           {div(textInput(inputId = "ID_DEBI", label = "ID_DEBI:"), style = "font-size:70%")}
                         ),
             splitLayout(cellWidths = c("20%", "20%"), 
                         verbatimTextOutput(outputId = "tablepeekR"),
                         verbatimTextOutput(outputId = "tablepeekK")
                         ), 
             {div(shinyFiles::shinyFilesButton('folderPC', 'Select a Peek-tube file', 'Select a Peek-tube file', FALSE))}, 
             actionButton("Add2", "Add2"),
             DT::DTOutput(outputId = "tablepeekid"),
             br(),br(),
             splitLayout(cellWidths = c("20%", "20%"), 
                         textAreaInput("nomid", label=NULL, value ="nom data here pour exporter", rows=1),
                          actionButton("generateButton","Write Data"))
             ),
    tabPanel("5- NOM", 
             br(),
             splitLayout(cellWidths = c("20%", "20%", "20%"), 
                         {div(shinyFiles::shinyFilesButton('folderPC5id', 'Select a Peek-tube file id', 'Select a Peek-tube file id', FALSE))}, 
                         {div(shinyFiles::shinyFilesButton('folderPC5', 'Select a Peek-tube file', 'Select a Peek-tube file', FALSE))}, 
                         {div(actionButton("Add5", "GO"))}
              ),
             br(),
              DT::DTOutput(outputId = "tablepeektub"),
             br(),
             splitLayout(cellWidths = c("20%", "20%"), 
                         textAreaInput("nomid5", label=NULL, value ="nom data here pour exporter", rows=1),
                         actionButton("generateButton5","Write Data")
                         )
            )
    ) #tabsetPanel
) #fluidPage



server <- function(input, output,session){
  print("start")  
  rv <- reactiveValues(
    df = data.frame(
      Height_Peek = character(),
      Time_Peek = character(),
      Vol_Peek = character(),
      Temp_Peek = character()
    )
  )
  
  dfpeek <- reactiveValues(
    dfpeek_chg =data.frame("HeightPeek","TimePeek","VolPeek","TempPeek",
                           "TIME_H","FLUX","FLUX_KGS","PRESSURE_PA","PRESSURE_MPA","K","N","FLUX_MMOLS")
  )
  dfresults <- reactiveValues(
    R_25_chg =data.frame(),
    K_25_chg =data.frame()
  )
  
  rv4 <- reactiveValues(
    df4 = data.frame(
      CAMPAGNE_MESURE4 = character(),
      MESUREUR4 = character(),
      AN_MESURE4 = character(),
      ColorID4 = character(),
      ID_DEBI4 = character(),
      K_25_chg4 = character(),
      R_25_chg4 = character()
    )
  )
  
  datasetInput <- reactiveValues(
    data_mean = data.frame(ColorID= character(), R_25_mean= numeric(), R_25_sd= numeric(), 
                           n= numeric(), R_25_se= numeric(), K_25_mean= numeric(),  K_25_sd= numeric(), K_25_se= numeric())
  )
  
observe({
  print("CALIBRATION peek color")
  shinyFiles::shinyFileChoose(input, 'folderPC', roots=c(wd='.'), defaultPath="OUTPUTS/PEAK/id/", pattern="_peekid", filetypes=c('csv'))
  
  folderPC<-substr(as.character(input$folderPC)[1],47,1000000L)
  print("folderPC")
  print(folderPC)
  x2<-unlist(gregexpr(pattern ='_peekid.csv',folderPC))
  namefile2=substr(folderPC,1,x2[1]-1)
  print("namefile2")#### 
  print(namefile2)#### 

  if (file.exists(paste0("OUTPUTS/PEAK/id/",namefile2,"_peekid.csv"))) {
    rv4$df4 <- read.csv(paste0("OUTPUTS/PEAK/id/",namefile2,"_peekid.csv"))
  }

})

observe({
  print("peek color id")
  shinyFiles::shinyFileChoose(input, 'folderPC5id', roots=c(wd='.'), defaultPath="OUTPUTS/PEAK/id/", pattern="_peekid.csv", filetypes=c('csv'))
})
observe({
  print("peek color")
  shinyFiles::shinyFileChoose(input, 'folderPC5', roots=c(wd='.'), defaultPath="www/color/", pattern="_peek.csv", filetypes=c('csv'))
})

  print("add")  
  observeEvent(input$Add, {
    
    cat("--Validate \n")
    validate(
      need(input$HeightPeek != "", 'Please choose a state.'),
      need(input$TimePeek != "", 'Please choose a state.'),
      need(input$VolPeek != "", 'Please choose a state.'),
      need(input$TempPeek != "", 'Please choose a state.')
    )
    
    rv$df <- rbind(rv$df, data.frame(
      Height_Peek = input$HeightPeek, 
      Time_Peek = input$TimePeek,
      Vol_Peek = input$VolPeek,
      Temp_Peek = input$TempPeek) %>%
      na.omit())

     df_char<-rv$df
      print("numeric")
      HeightPeek_<- as.numeric(df_char$Height_Peek)
      TimePeek_<- as.numeric(df_char$Time_Peek)
      VolPeek_<- as.numeric(df_char$Vol_Peek)
      TempPeek_<- as.numeric(df_char$Temp_Peek)
      dfpeek_c<-cbind(HeightPeek_,TimePeek_,VolPeek_,TempPeek_)
      colnames(dfpeek_c)<-c("HeightPeek","TimePeek","VolPeek","TempPeek")
      dfpeek_c <- as.data.frame(dfpeek_c) %>%
        na.omit()
      #print(dfpeek_c)
      
      dfpeek$dfpeek_chg <- dfpeek_c %>% 
        mutate(TIME_H=TimePeek/3600, #time (h)
               FLUX=VolPeek/1000/TIME_H,
               FLUX_KGS=VolPeek/1000/TimePeek,#flux (kg/s)
               PRESSURE_PA=1000*9.81*HeightPeek, #pressure (Pa)
               PRESSURE_MPA=PRESSURE_PA/1000000, #pressure (MPa)
               K=VolPeek/1000/TimePeek/PRESSURE_MPA, #hydraulic conductance(kg s-1 MPa-1)
               N=VolPeek/18*1000, # N (mmol)
               FLUX_MMOLS=N/TimePeek) # Flux (mmol s-1)
      #print(str(dfpeek$dfpeek_chg))

      shinyjs::reset("myappm1")
      
      observeEvent(input$tablepeek_cell_edit, {
        rv$df[input$tablepeek_cell_edit$row,input$tablepeek_cell_edit$col] <<- (DT::coerceValue(input$tablepeek_cell_edit$value, rv$df[input$tablepeek_cell_edit$row,input$tablepeek_cell_edit$col]))
        rv$df %>%
          na.omit()
        df_char<-rv$df
        print("numeric")
        HeightPeek_<- as.numeric(df_char$Height_Peek)
        TimePeek_<- as.numeric(df_char$Time_Peek)
        VolPeek_<- as.numeric(df_char$Vol_Peek)
        TempPeek_<- as.numeric(df_char$Temp_Peek)
        dfpeek_c<-cbind(HeightPeek_,TimePeek_,VolPeek_,TempPeek_)
        colnames(dfpeek_c)<-c("HeightPeek","TimePeek","VolPeek","TempPeek")
        dfpeek_c <- as.data.frame(dfpeek_c) %>%
          na.omit()

        dfpeek$dfpeek_chg <- dfpeek_c %>% 
          mutate(TIME_H=TimePeek/3600, #time (h)
                 FLUX=VolPeek/1000/TIME_H,
                 FLUX_KGS=VolPeek/1000/TimePeek,#flux (kg/s)
                 PRESSURE_PA=1000*9.81*HeightPeek, #pressure (Pa)
                 PRESSURE_MPA=PRESSURE_PA/1000000, #pressure (MPa)
                 K=VolPeek/1000/TimePeek/PRESSURE_MPA, #hydraulic conductance(kg s-1 MPa-1)
                 N=VolPeek/18*1000, # N (mmol)
                 FLUX_MMOLS=N/TimePeek) # Flux (mmol s-1)
        #print(str(dfpeek$dfpeek_chg))
      })
      

      output$hist1P<-renderPlot({
        # Calcul de la pente de la relation linéaire de l'évolution du flux en mmol.s-1 avec la pression
        model <- lm(FLUX_MMOLS~PRESSURE_MPA, dfpeek$dfpeek_chg)
        slopeP <- model$coefficients[2]
        print(dfpeek$dfpeek_chg)
        # Calcul de la résistance 
        Temperature <- mean(dfpeek$dfpeek_chg$TempPeek, na.rm = TRUE)
        print(Temperature)
        R=1/slopeP
        R_25=0.88862/(10^((1.3272*(20-Temperature)-0.001053*(20-Temperature)^2)/(Temperature+105)))*R
        dfresults$R_25_chg=R_25[[1]]

        #print(summary(model))
        print(summary(model)$adj.r.squared)
        annotations <- data.frame(
          xpos = c(-Inf),
          ypos =  c(Inf),
          annotateText = c(paste0("R2=",round(summary(model)$adj.r.squared,4), "   R_25=", round(R_25,4))),
          hjustvar = c(0) ,
          vjustvar = c(1)) 
        
        color <- "purple"
        if (!is.na(summary(model)$adj.r.squared) & summary(model)$adj.r.squared>=0.99) {
          color="blue"
        }  
        #print(color)
        
        #Vérifier que le R2 > 0.95 pour valider les données
        ggplot(dfpeek$dfpeek_chg, aes(PRESSURE_MPA, FLUX_MMOLS)) + geom_point() +geom_smooth(method=lm) + geom_text(data=annotations,color=color, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))
      }) #renderPlot
      #   HAUTEUR	TEMPS	VOLUME	TEMPERATURE
      #    0.05	2069	0.1	21.75
      #    0.15	713	0.1	21.75
      #    0.25	465	0.11	21.75
      #    0.35	310	0.1	21.75
      #    0.45	244	0.1	21.75  
      
output$hist2P<-renderPlot({
  # Calcul de la conductance hydraulique du peek tube 
  Temperature <- mean(dfpeek$dfpeek_chg$TempPeek, na.rm = TRUE)
  model.2 <- lm(FLUX_KGS~PRESSURE_MPA, dfpeek$dfpeek_chg)
  pente.2 <- model.2$coefficients[2]
  summary(model.2)$adj.r.squared
  
  K_25=pente.2/(0.88862*(10^((1.3272*(20-Temperature)-0.001053*(20-Temperature)^2)/(Temperature+105))))
  dfresults$K_25_chg=K_25[[1]]
  
        annotations2 <- data.frame(
          xpos = c(-Inf),
          ypos =  c(Inf),
          annotateText = c(paste0("K_25=",round(K_25,6))),
          hjustvar = c(0) ,
          vjustvar = c(1)) 
 

        ggplot(dfpeek$dfpeek_chg, aes(PRESSURE_MPA, FLUX_KGS)) + geom_point() +  geom_smooth(method=lm) + geom_text(data=annotations2,color="blue", aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))
        
       }) #renderPlot
      
      
  }) #observeEventAdd
  



 
  output$tablepeek<-DT::renderDT({
    if (!is.null(dim(rv$df))) {
    if (dim(rv$df)[1]>=1) {
      essai<-rv$df %>%
        dplyr::filter(Height_Peek  !='' & Time_Peek !='' & Vol_Peek  !='' & Temp_Peek !='')
      print("essai")
      print(essai)

    DT::datatable(essai, 
                  options = list(dom = 't'),
                  editable = TRUE,
                  escape   = FALSE,
                  caption = htmltools::tags$caption("Aperçu des données", style="color:blue")) 
    }}
  }) #tablepeek
  

  
  print("add2")  
  observeEvent(input$Add2, {

    cat("--Validate4 \n")
    validate(
      need(input$CAMPAGNE_MESURE != "", 'Please choose a state.'),
      need(input$MESUREUR != "", 'Please choose a state.'),
      need(input$AN_MESURE != "", 'Please choose a state.'),
      need(input$ColorID != "", 'Please choose a state.'),
      need(input$ID_DEBI != "", 'Please choose a state.')    
      )
    write.csv(rv$df, paste0("OUTPUTS/PEAK/mesure/",input$CAMPAGNE_MESURE,input$MESUREUR,
                            input$AN_MESURE,input$ColorID,input$ID_DEBI,"_peekmesure.csv"), row.names=FALSE)
    
    print(rv4$df4)
    
    rv4$df4 <- rbind(rv4$df4, data.frame(
      CAMPAGNE_MESURE4 = input$CAMPAGNE_MESURE,
      MESUREUR4 = input$MESUREUR,
      AN_MESURE4 = input$AN_MESURE,
      ColorID4 = input$ColorID,
      ID_DEBI4 = input$ID_DEBI,
      K_25_chg4 =  dfresults$K_25_chg,
      R_25_chg4 = dfresults$R_25_chg
    ))

  })
  

  output$tablepeekR<-renderText({paste0("R_25=",dfresults$R_25_chg)}) #tablepeekR
  
  output$tablepeekK<-renderText({paste0("K_25=",dfresults$K_25_chg)}) #tablepeekK
  
  output$tablepeekid<-DT::renderDT({
    DT::datatable(rv4$df4, 
                  options = list(dom = 't'),
                  editable = FALSE,
                  escape   = FALSE,
                  caption = htmltools::tags$caption("Aperçu des données", style="color:blue")) 
    }) #tablepeekid
  
  
  ####### DOWNLOAD BUTTON downloadDataT1
  observeEvent(input$generateButton, {
    write.csv(rv4$df4, paste0("OUTPUTS/PEAK/id/",input$nomid,"_peekid.csv"), row.names=FALSE)
  })
  
  observeEvent(input$Add5, {
    folderPC5id<-substr(as.character(input$folderPC5id)[1],47,1000000L)
    #print("folderPC5id")
    #print(folderPC5id)
    x2<-unlist(gregexpr(pattern ='_peekid.csv',folderPC5id))
    namefile5id=substr(folderPC5id,1,x2[1]-1)
    folderPC5id<-NULL
    #print("namefile5id")#### 
    #print(namefile5id)#### 
    
    folderPC5<-substr(as.character(input$folderPC5)[1],38,1000000L)
    #print("folderPC5")
    #print(folderPC5)
    x2<-unlist(gregexpr(pattern ='_peek.csv',folderPC5))
    namefile5=substr(folderPC5,1,x2[1]-1)
    folderPC5<-NULL
    #print("namefile5")#### 
    #print(namefile5)#### 
    
    data_all1 <-NULL
    data_all2 <-NULL
    if (file.exists(paste0("OUTPUTS/PEAK/id/",namefile5id,"_peekid.csv"))) {
      data_all1 <- read.csv(paste0("OUTPUTS/PEAK/id/",namefile5id,"_peekid.csv"))
    }
    data_all1$K_25_chg4_<- as.numeric(data_all1$K_25_chg4)
    data_all1$R_25_chg4_<- as.numeric(data_all1$R_25_chg4)
    data_all1$ColorID<-data_all1$ColorID4
    
    if (!is.null(dim(data_all1))) {
      data_mean1 <- data_all1 %>% 
        group_by(ColorID) %>% 
        summarise(R_25_mean=mean(R_25_chg4_), R_25_sd=sd(R_25_chg4_), n=n(), R_25_se=R_25_sd/sqrt(n),
                  K_25_mean=mean(K_25_chg4_), K_25_sd=sd(K_25_chg4_), K_25_se=K_25_sd/sqrt(n))
      #print(data_mean1)
      
      if (file.exists(paste0("www/color/",namefile5,"_peek.csv"))) {
        data_all2 <- read.csv(paste0("www/color/",namefile5,"_peek.csv"))
      }
      datasetInput$data_mean <- as.data.frame(rbind(data_mean1,data_all2))
    }
  })


  output$tablepeektub<-DT::renderDT({
    DT::datatable(datasetInput$data_mean, extensions = 'Buttons', 
                               options = list(ordering = TRUE, dom = 'tB',
                                              buttons = c('csv')),
                  rownames= FALSE,
                  caption = htmltools::tags$caption("Aperçu des données", style="color:blue")) 
  }) #tablepeektub
  
  observeEvent(input$generateButton5, {
    write.csv(datasetInput$data_mean, paste0("www/color/",input$nomid5,"_peek.csv"), row.names=FALSE)
  })
  
  #resultats SingleKmeasurement
  observeEvent(input$resetAllpeek, {
    shinyjs::reset("myappm1")
    rv$df<-NULL
    dfpeek$dfpeek_chg<-NULL
    dfresults$R_25_chg<-NULL
    dfresults$K_25_chg<-NULL
    datasetInput$data_mean<-NULL
    rv4$df4<-NULL
 #   folderPC5id<-NULL
#    folderPC5<-NULL
#    folderPC<-NULL
#    data_all1 <-NULL
#    data_all2 <-NULL
#    data_mean1 <-NULL
#    x2<-NULL
#    namefile5id<-NULL
#    namefile5<-NULL
    #output$tablepeektub <-DT::renderDT({})
    output$hist1P <- renderPlot({})
    output$hist2P <- renderPlot({})
    print("a")
    print(datasetInput$data_mean)
  }) 
  

}


shinyApp(ui = ui, server = server)