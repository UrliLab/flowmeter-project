#packages
library(shiny)
library(shinyTime)
library(readODS)
library(gridExtra)

#fonction pour séparer les hauteurs
extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}

getRemoveButton <- function(n, idS = "", lab = "Pit") {
  if (stringr::str_length(idS) > 0) idS <- paste0(idS, "-")
  ret <- shinyInput(actionButton, n,
                    'button_', label = "Enlever",
                    onclick = sprintf('Shiny.onInputChange(\"%sremove_button_%s\",  this.id)' ,idS, lab))
  return (ret)
}


shinyInput <- function(FUN, n, id, ses, ...) {
  as.character(FUN(paste0(id, n), ...))
}

# Creation des répertoires du projet
if (file.exists("OUTPUTS")) {
} else {
  dir.create("OUTPUTS")
}
if (file.exists("OUTPUTS/CALIBRATION")) {
} else {
  dir.create("OUTPUTS/CALIBRATION")
}
if (file.exists("OUTPUTS/SINGLE")) {
} else {
  dir.create("OUTPUTS/SINGLE")
}

server <- function(input, output, session) {

############################################################
############################################################
#ETAPE 2 PressureSensorCalibration  
  #Initial Dataframe 
  rv <- reactiveValues(
  Testdata =data.frame("Hauteur" = integer(),	"ELTime" = integer(),	"Step"=integer(),	
                         "P1.psi"=integer(),	"P2.psi"=integer(),
                         #"T1_oC"=numeric(),	"T2_oC"=numeric(),	
                       "T3_oC"=numeric(), "id"=character(), "Enlever"=character()
                       ))
  rv2 <- reactiveValues(
    data_mean =data.frame("Hauteur" = integer(),	"n" = integer(),		
                         "P1_mean"=numeric(),	"P1_sd"=numeric(), "P1_cv"=numeric(),
                         "P2_mean"=numeric(),	"P2_sd"=numeric(), "P2_cv"=numeric(),	
                         "Bar"=numeric()),
    pente=data.frame("Variable"=character(),"P1"=numeric(),"P2"=numeric()))

  observeEvent(input$recalc,{
  
      Hauteur <- extract(input$breaks)
      Step <- 1:length(Hauteur)
      iter <- cbind(Step,Hauteur)

    if (input$pasted != '') {
      df_table <- fread(paste(input$pasted, collapse = "\n"))
      df_table <-as.data.frame(df_table)
      colnames(df_table) <- c("ELTime",	"Step",	"P1.psi",	"P2.psi",	#"T1_oC",	"T2_oC",	
                              "T3_oC")
      df_table$Step<-df_table$Step-df_table$Step[1]+1
      df_table<-merge(iter, df_table)
      #df_table <- cbind(input$hauteur,df_table)
      #colnames(df_table) <- c("Hauteur", "ELTime",	"Step",	"P1.psi",	"P2.psi",	"T1_oC",	"T2_oC",	"T3_oC")
    }

    print("Enlever data si deja hauteur")
    #rv$Testdata <- subset(rv$Testdata, !(rv$Testdata[[2]] %in% Hauteur))
    #rv$Testdata <- subset(rv$Testdata, rv$Testdata[[2]] != Hauteur)
    #a [! a %in% remove]
    x <- rv$Testdata[ which(! rv$Testdata[[2]] %in% Hauteur), ]
    #x <- rv$Testdata[ which(rv$Testdata[[2]]!=Hauteur), ]
    rv$Testdata<-x

    # APPEND USER ROW
    print("APPEND")
    #rv$Testdata <- subset(rv$Testdata, !(rv$Testdata[[2]] %in% Hauteur))
    df_table$id <- rownames(df_table)
    df_table$Enlever <- NA

    rv$Testdata <- rbind(rv$Testdata, df_table) 
    rv$Testdata$id <- rownames(rv$Testdata)

    print("nomfile")
    nomfile<-paste0("d_",input$date,"_",strftime(input$heure, "%T"),"_",input$operateur,"_",input$xylem)
    nomfile <- str_replace_all(nomfile, c("-"="",":"="", "é"= "e", "à"="a", "è"= "e", "ô" = "o", "ç"="c", "É"="E", "È"="E", "Î"="i", "Ç"="C"))
    cat(nomfile, "\n")

    print("summarise")
    rv2$data_mean <- rv$Testdata %>% 
      group_by(Hauteur) %>% 
      summarise(n=n(), P1_mean=mean(P1.psi), P1_sd=sd(P1.psi), P1_cv=P1_sd/P1_mean,
                P2_mean=mean(P2.psi), P2_sd=sd(P2.psi), P2_cv=P2_sd/P2_mean)
    rv2$data_mean$Bar=rv2$data_mean$Hauteur/1000 
    
    print("lm")
    model1 <- lm(Bar~P1_mean, rv2$data_mean)
    model2 <- lm(Bar~P2_mean, rv2$data_mean)
    ordorig <- cbind("Intercept",model1$coefficients[[1]],model2$coefficients[[1]])
    pente <- cbind("pente",model1$coefficients[[2]],model2$coefficients[[2]])
    adj.r.squared <- cbind("adj.r.squared",summary(model1)$adj.r.squared,summary(model2)$adj.r.squared)
    rv2$pente <- rbind(adj.r.squared,ordorig,pente)
    colnames(rv2$pente)<-c("Variable","P1","P2")
    print(rv2$pente)
    
    rv$Testdata<-rv$Testdata %>%
      rowwise() %>%
      mutate(Enlever = getRemoveButton(id, idS = "", lab = "Tab1"))
   
    proxyTable <- DT::dataTableProxy("Testdata")
 
    observeEvent(input$remove_button_Tab1, {
      session$sendCustomMessage(type = "resetValue", message = "remove_button_Tab1")
      print("$Delete")
      myTable <- rv$Testdata
      #print(myTable)
      s <- as.numeric(strsplit(input$remove_button_Tab1, "_")[[1]][2])
      myTable <- filter(myTable, id != s)
      replaceData(proxyTable, myTable, resetPaging = FALSE)
      rv$Testdata <- myTable

      rv2$data_mean <- rv$Testdata %>% 
        group_by(Hauteur) %>% 
        summarise(n=n(), P1_mean=mean(P1.psi), P1_sd=sd(P1.psi), P1_cv=P1_sd/P1_mean,
                  P2_mean=mean(P2.psi), P2_sd=sd(P2.psi), P2_cv=P2_sd/P2_mean)
      rv2$data_mean$Bar=rv2$data_mean$Hauteur/1000 
      print(head(rv2$data_mean))
      
      model1 <- lm(Bar~P1_mean, rv2$data_mean)
      model2 <- lm(Bar~P2_mean, rv2$data_mean)
      ordorig <- cbind("Intercept",model1$coefficients[[1]],model2$coefficients[[1]])
      pente <- cbind("pente",model1$coefficients[[2]],model2$coefficients[[2]])
      adj.r.squared <- cbind("adj.r.squared",summary(model1)$adj.r.squared,summary(model2)$adj.r.squared)
      rv2$pente <- rbind(adj.r.squared,ordorig,pente)
      colnames(rv2$pente)<-c("Variable","P1","P2") 
      print(head(rv2$pente))    
      
    })   


    output$arduino <- renderDataTable({
      write.csv(rv$Testdata, paste0("OUTPUTS/CALIBRATION/", nomfile,"_arduino.csv"),  row.names=FALSE)
      DT::datatable(rv$Testdata, #rownames = FALSE, colnames = c("Step",	"Heigh (cm)",		"ELTime (s)", "P1(psi)", "P2(psi)",	"T1(oC)",	"T2(oC)",	 "T3(oC)" ),
                    options = list(searching = FALSE,lengthChange = FALSE),
                    editable = TRUE,
                    escape   = FALSE,
                    caption = htmltools::tags$caption("Aperçu des données Arduino", style="color:blue")) %>%
        formatRound(columns = c(4:6), digits = 4)
    })
                                                              
    output$table <- renderDataTable({  
      write.csv(rv2$data_mean, paste0("OUTPUTS/CALIBRATION/",nomfile,"_moyenne.csv"),  row.names=FALSE)
      DT::datatable(subset(rv2$data_mean, select = -c(Bar) ), colnames = c('Step', 'Heigh (cm)', 'n', 'P1 Average', 'P1 StdDev', 'P1 CV  (<0.05)', 'P2 Average', 'P2 StdDev', 'P2 CV  (<0.05)'), 
                    caption = htmltools::tags$caption("Aperçu des moyennes", style="color:blue"),
                    options = list(dom = 't')) %>% 
        formatRound(columns = c(3:8), digits = 2) %>% 
        formatStyle('P1_cv',
        backgroundColor = styleInterval(c(0.05), c('green', 'red'))) %>% 
        formatStyle('P2_cv',
        backgroundColor = styleInterval(c(0.05), c('green', 'red')))
    })

    rowCallback <- c(
      "function(row, dat, displayNum, index){",
      "  if(index == 0){",
      "    for(var j=1; j<dat.length; j++){",
      "      var x = dat[j];",
      "      var color = x >= 0.999 ? 'green' : 'red';",
      "      $('td:eq('+j+')', row)", 
      "        .css('background-color', color);",
      "    }",
      "  }",
      "}"
    )
    
    output$pente <- renderDataTable({
      write.csv(rv2$pente, paste0("OUTPUTS/CALIBRATION/",nomfile,"_coeff.csv"),  row.names=FALSE)
      pente[1,1] <- "adj.r.squared (R2 > 0.999)"
      
      DT::datatable(rv2$pente, 
                    caption = htmltools::tags$caption("Régression", style="color:blue"),
                    options = 
                      list(rowCallback = JS(rowCallback),
                           dom = 't')) 
    })
   
   
    output$hist1 <- renderPlot({
      ggplot(rv2$data_mean, aes(P1_mean, Bar)) + geom_point() +
        geom_smooth(method=lm) +
        ggtitle("P1") +
        xlab("Presure units (psi)") + ylab("bar")
      }) 
    output$hist2 <- renderPlot({
      ggplot(rv2$data_mean, aes(P2_mean, Bar)) + geom_point() +
        geom_smooth(method=lm)+
        ggtitle("P2") +
        xlab("Presure units (psi)") + ylab("bar")
    })
    
      ####### DOWNLOAD BUTTON 
      output$downloadData <- downloadHandler(
        filename <- function() {
          paste("output", "zip", sep=".")
        },
        
        content <- function(fname) {
          fs <- c("OUTPUTS/CALIBRATION")
          zip::zipr(zipfile=fname, files=fs)
        }
      )

  }) #observeEvent

  observeEvent(input$resetAll, {
    shinyjs::reset("myapp")
    shinyjs::reset("remove_button_Tab1")
    reset(id = "", asis = FALSE)
    rv$Testdata<-NULL
    rv2$data_mean<-NULL
    rv2$pente<-NULL
    proxyTable<-NULL
    myTable <-NULL
    output$table <- renderDataTable({}) 
    output$pente <- renderDataTable({}) 
    output$hist1 <- renderPlot({}) 
    output$hist2 <- renderPlot({}) 
    output$arduino <- renderDataTable({}) 
   })  
  
############################################################
############################################################
#ETAPE 2i PressureSensorCalibration  (lecture)
  observe({
      #volumes = getVolumes() # this makes the directory at the base of your computer.
      shinyFileChoose(input, 'folder', roots=c(wd='.'), defaultPath="OUTPUTS\\CALIBRATION", pattern="_arduino", filetypes=c('csv'))

    }) 
  
  observeEvent(input$recalcL, {
    folder<-substr(as.character(input$folder)[1],48,1000000L)
    x<-unlist(gregexpr(pattern ='_arduino.csv',folder))
    nomfile=paste0("d_",substr(folder,1,x[1]-1))
    print("Nom du fichier de calibration importé")
    print(nomfile)
  #observeEvent(input$recalcL, {
  #nomfile<-paste0("d_",input$dateL,"_",strftime(input$heureL, "%T"),"_",input$operateurL,"_",input$xylemL)
  #nomfile <- str_replace_all(nomfile, c("-"="",":"="", "é"= "e", "à"="a", "è"= "e", "ô" = "o", "ç"="c", "É"="E", "È"="E", "Î"="i", "Ç"="C"))
  #cat(nomfile)
  Testdata<-read.csv(paste0("OUTPUTS/CALIBRATION/", nomfile,"_arduino.csv"))
  data_mean<-read.csv(paste0("OUTPUTS/CALIBRATION/",nomfile,"_moyenne.csv"))
  model1 <- lm(Bar~P1_mean, data_mean)
  model2 <- lm(Bar~P2_mean, data_mean)
  
  ordorig <- cbind("Intercept",model1$coefficients[[1]],model2$coefficients[[1]])
  pente <- cbind("pente",model1$coefficients[[2]],model2$coefficients[[2]])
  adj.r.squared <- cbind("adj.r.squared",summary(model1)$adj.r.squared,summary(model2)$adj.r.squared)
  pente <- rbind(adj.r.squared,ordorig,pente)
  colnames(pente)<-c("Variable","P1","P2")
  pente[1,1] <- "adj.r.squared (R2 > 0.999)"

  output$arduinoL <- renderDataTable({
    DT::datatable(subset(Testdata, select = -c(id,Enlever) ), rownames = FALSE, colnames = c("Step",	"Heigh (cm)",		"ELTime (s)", "P1(psi)", "P2(psi)",	#"T1(oC)",	"T2(oC)",	
                                                                                             "T3(oC)" ),
                  options = list(searching = FALSE,lengthChange = FALSE),
                  caption = htmltools::tags$caption("Aperçu des données Arduino", style="color:blue")  ) %>% 
      formatRound(columns = c(4:6), digits = 4)
  })
 
  output$tableL <- renderDataTable({
    DT::datatable(subset(data_mean, select = -c(Bar) ), colnames = c('Step', 'Heigh (cm)', 'n', 'P1 Average', 'P1 StdDev', 'P1 CV  (<0.05)', 'P2 Average', 'P2 StdDev', 'P2 CV  (<0.05)'), 
                  caption = htmltools::tags$caption("Aperçu des moyennes", style="color:blue"),
                  options = list(dom = 't')) %>% 
      formatRound(columns = c(3:8), digits = 2) %>% 
      formatStyle('P1_cv',
                  backgroundColor = styleInterval(c(0.05), c('green', 'red'))) %>% 
      formatStyle('P2_cv',
                  backgroundColor = styleInterval(c(0.05), c('green', 'red')))
  })
  
  rowCallback <- c(
    "function(row, dat, displayNum, index){",
    "  if(index == 0){",
    "    for(var j=1; j<dat.length; j++){",
    "      var x = dat[j];",
    "      var color = x >= 0.999 ? 'green' : 'red';",
    "      $('td:eq('+j+')', row)", 
    "        .css('background-color', color);",
    "    }",
    "  }",
    "}"
  )
  
  output$penteL <- renderDataTable({
    DT::datatable(pente,  
                  caption = htmltools::tags$caption("Régression", style="color:blue"),
                  options = 
                    list(rowCallback = JS(rowCallback),
                         dom = 't')) 
  })
  
  
  output$hist1L<- renderPlot({
    ggplot(data_mean, aes(P1_mean, Bar)) + geom_point() +
      geom_smooth(method=lm) + 
      ggtitle("P1") +
      xlab("Presure units (psi)") + ylab("bar")
  }) 
  output$hist2L <- renderPlot({
    ggplot(data_mean, aes(P2_mean, Bar)) + geom_point() +
      geom_smooth(method=lm) +
      ggtitle("P2") +
      xlab("Presure units (psi)") + ylab("bar")
  })
  
  })
  
  ############################################################
  ############################################################
  #ETAPE 3 SingleKmeasurement 
  observe({
    #volumes = getVolumes() # this makes the directory at the base of your computer.
    shinyFileChoose(input, 'folderPSm', roots=c(wd='.'), defaultPath="OUTPUTS\\CALIBRATION", pattern="_coeff", filetypes=c('csv'))
  })
  
  observe({
    #volumes = getVolumes() # this makes the directory at the base of your computer.
    shinyFileChoose(input, 'folderPTm', roots=c(wd='.'), defaultPath="OUTPUTS\\CALIBRATION", pattern="_peek", filetypes=c('csv'))
    folderPTm<-substr(as.character(input$folderPTm)[1],48,1000000L)
    x2<-unlist(gregexpr(pattern ='_peek.csv',folderPTm))
    nomfile2=paste0("d_",substr(folderPTm,1,x2[1]-1))
    print(nomfile2)  #### 
    if (file.exists(paste0("OUTPUTS/CALIBRATION/",nomfile2,"_peek.csv"))) {
      peek_coeff <- read.csv(paste0("OUTPUTS/CALIBRATION/",nomfile2,"_peek.csv"))
      #print(peek_coeff)
      choiceList <- peek_coeff[, 1]
      updateSelectInput(session, "peek_sel", label = "Couleur", choices = choiceList)
    }
  })
  
  df1 <- reactiveValues(
    data_meanm =data.frame("Step", "n", "P1_mean", "P1_sd", "P1_cv","P2_mean","P2_sd","P2_cv",                           "P2_mean", "P2_sd", "P2_cv",
    "P1bar_mean", "P1bar_sd", "P1bar_cv",    "P2bar_mean", "P2bar_sd", "P2bar_cv",
    "Diffbar_mean", "Diffbar_sd", "Diffbar_cv",
    "k_rough_mean","k_rough_sd","k_rough_cv",
    "ValidPeekChoice",
    "p1_300_mean","p1_300_sd","p1_300_cv",
    "p2_300_mean","p2_300_sd","p2_300_cv",
    "ValidPeekChoice2",
    "k_300_rough_mean","k_300_rough_sd","k_300_rough_cv",
    "p3_min","t1_mean","t2_mean","t3_mean","ELTime2_max"
    )
  )
  
  
  rvz <- reactiveValues(
    df_tablem =data.frame("ELTime",	"Step",	"P1.psi",	"P2.psi", "T3_oC",	"T1_oC",	"T2_oC"
                          ))

  observeEvent(input$recalcm,{
    dir.create(paste0("OUTPUTS/SINGLE/",input$parm1))
    dir.create(paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm7))
    
     if (input$pastedm != '') {
       rvz$df_tablem <- fread(paste(input$pastedm, collapse = "\n"))
       rvz$df_tablem <-as.data.frame(rvz$df_tablem)
      colnames(rvz$df_tablem) <- c("ELTime",	"Step",	"P1.psi",	"P2.psi",	"T3_oC"#,"T1_oC",	"T2_oC" 
                                   )
      rvz$df_tablem <- subset(rvz$df_tablem, P1.psi!="Inf")
      rvz$df_tablem <- subset(rvz$df_tablem, P2.psi!="Inf")
     }
    
    intrant <- data.frame (first_column  = c("parm1","parm2","parm3","parm4","parm5","parm6","folder Preasure Sersors Calibration","folder PEEKtubeCalibration","couleur","Stemdiameter1","Stemdiameter2","Stemlength","Commentaires","parm7","T1_oC","T2_oC"),
                           second_column  = c(input$parm1,input$parm2,input$parm3,input$parm4,input$parm5,input$parm6,substr(as.character(input$folderPSm)[1],48,1000000L),substr(as.character(input$folderPTm)[1],48,1000000L),input$peek_sel,input$Stemdiameter1,input$Stemdiameter2,input$Stemlength,input$commentairem,input$parm7,input$T1_oC,input$T2_oC))
    write.csv(intrant, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm7,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_INTRANT.csv"),  row.names=FALSE)

    folderPSm<-substr(as.character(input$folderPSm)[1],48,1000000L)
    x<-unlist(gregexpr(pattern ='_coeff.csv',folderPSm))
    nomfile=paste0("d_",substr(folderPSm,1,x[1]-1))
    print("Nom du fichier de calibration importé")
    print(nomfile)
    data_coeff<-read.csv(paste0("OUTPUTS/CALIBRATION/",nomfile,"_COEFF.csv"))
    colnames(data_coeff)<-c("Variable","P1 calibration","P2 calibration")

    write.csv(data_coeff, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm7,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_COEFF.csv"),  row.names=FALSE)

    
      folderPTm<-substr(as.character(input$folderPTm)[1],48,1000000L)
      x2<-unlist(gregexpr(pattern ='_peek.csv',folderPTm))
      nomfile2=paste0("d_",substr(folderPTm,1,x2[1]-1))
      print(nomfile2)
      peek_coeff <- read.csv(paste0("OUTPUTS/CALIBRATION/",nomfile2,"_peek.csv"))
      Res_PEEK<-base::subset(peek_coeff,ColorID==input$peek_sel)[[2]]
      peek <- data.frame (first_column  = c("Color of PEEK tubing (from « PEEKtubeCalibration Tab »)", 
                                            "Resistance of choosen PEEK tubing at 25 oC (MPa mmol-1 s)"),
                          second_column = c(input$peek_sel, Res_PEEK)
      )
      write.csv(peek, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm7,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_PEEK.csv"),  row.names=FALSE)
    
    z<-rvz$df_tablem
    print(head(z))
    z$t1_meanx=as.numeric(input$T1_oC)
    z$t2_meanx=as.numeric(input$T2_oC)

    print(dim(z)[1])
    
    if (dim(z)[1]==1) {
      return()
    }
    
    z <- z %>%
      group_by(Step) %>%
      mutate(ELTimemax =max(ELTime))
    z <- z %>% 
      mutate(P1bar=data_coeff$P1[2]+data_coeff$P1[3]*P1.psi,
             P2bar=data_coeff$P2[2]+data_coeff$P2[3]*P2.psi,
             Diffbar=abs(P1bar-P2bar),
             k_rough=((P1bar-P2bar)/Res_PEEK)/P2bar,
             ELTime2=ELTimemax-ELTime,
             condition_1 = na_if(1*((ELTimemax-ELTime)<300),0),
             k_300_rough=((P1bar*condition_1-P2bar*condition_1)/Res_PEEK)/P2bar*condition_1,
             Pithdiameter=0
             )
    df1$zdf1 <- z
      
print("z")  
#print(head(z))

    df1$data_meanm <- z %>% 
      group_by(Step) %>% 
      summarise(n=n(), P1_mean=mean(P1.psi), P1_sd=sd(P1.psi), P1_cv=P1_sd/P1_mean,
                P2_mean=mean(P2.psi), P2_sd=sd(P2.psi), P2_cv=P2_sd/P2_mean,
                P1bar_mean=mean(P1bar), P1bar_sd=sd(P1bar), P1bar_cv=P1bar_sd/P1bar_mean,
                P2bar_mean=mean(P2bar), P2bar_sd=sd(P2bar), P2bar_cv=P2bar_sd/P2bar_mean,
                Diffbar_mean=mean(Diffbar), Diffbar_sd=sd(Diffbar), Diffbar_cv=Diffbar_sd/Diffbar_mean,
                k_rough_mean=mean(k_rough),k_rough_sd=sd(k_rough), k_rough_cv=k_rough_sd/k_rough_mean,
                ValidPeekChoice=P2bar_mean/P1bar_mean,
                p1_300_mean=mean(P1bar*condition_1,na.rm=TRUE),p1_300_sd=sd(P1bar*condition_1,na.rm=TRUE), p1_300_cv=p1_300_sd/p1_300_mean,
                p2_300_mean=mean(P2bar*condition_1,na.rm=TRUE),p2_300_sd=sd(P2bar*condition_1,na.rm=TRUE), p2_300_cv=p2_300_sd/p2_300_mean,
                ValidPeekChoice2=p2_300_mean/p1_300_mean,
                k_300_rough_mean=mean(k_300_rough,na.rm=TRUE),k_300_rough_sd=sd(k_300_rough,na.rm=TRUE), k_300_rough_cv=k_300_rough_sd/k_300_rough_mean,
                p3_min=min(P2bar),
                t1_mean=mean(t1_meanx),t2_mean=mean(t2_meanx),t3_mean=mean(T3_oC),
                ELTime2_max=max(ELTime2*condition_1,na.rm=TRUE)
      )
    write.csv(rvz$df_tablem, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm7,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_ARDUINO.csv"),  row.names=FALSE)
print("data_meanm")   
write.csv(df1$data_meanm , paste0("OUTPUTS/SINGLE/x.csv"),  row.names=FALSE)

    NOMADD<-data.frame (first_column  = c("parm1","parm2","parm3","parm4","parm5","parm6","T1_oC","T2_oC"),
                        second_column = c(input$parm1,input$parm2,input$parm3,input$parm4,input$parm5,as.character(zoo::as.Date(as.numeric(input$parm6))),input$T1_oC,input$T2_oC),
                        third_column = c("","","","","","","",""),
                        fourth_column = c("","","","","","","","")
    )
    print("NOMADD") 

    observeEvent(input$arduinom_cell_edit, {
      #rvz.df_tablem[input$arduinom_cell_edit$row,input$arduinom_cell_edit$col] <<- input$arduinom_cell_edit$value
      rvz$df_tablem[input$arduinom_cell_edit$row,input$arduinom_cell_edit$col] <<- (DT::coerceValue(input$arduinom_cell_edit$value, rvz$df_tablem[input$arduinom_cell_edit$row,input$arduinom_cell_edit$col]))
      z<- rvz$df_tablem   
      z$t1_meanx=as.numeric(input$T1_oC)
      z$t2_meanx=as.numeric(input$T2_oC)
      z <- z %>%
        group_by(Step) %>%
        mutate(ELTimemax =max(ELTime))
      z <- z %>% 
        mutate(P1bar=data_coeff$P1[2]+data_coeff$P1[3]*P1.psi,
               P2bar=data_coeff$P2[2]+data_coeff$P2[3]*P2.psi,
               Diffbar=abs(P1bar-P2bar),
               k_rough=((P1bar-P2bar)/Res_PEEK)/P2bar,
               ELTime2=ELTimemax-ELTime,
               condition_1 = na_if(1*((ELTimemax-ELTime)<300),0),
               k_300_rough=((P1bar*condition_1-P2bar*condition_1)/Res_PEEK)/P2bar*condition_1,
               Pithdiameter=0
        )
      write.csv(rvz$df_tablem, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm7,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_ARDUINO.csv"),  row.names=FALSE)
      df1$zdf1 <- z
      #print("zdf1") 

      df1$data_meanm <- z %>% 
        group_by(Step) %>% 
        summarise(n=n(), P1_mean=mean(P1.psi), P1_sd=sd(P1.psi), P1_cv=P1_sd/P1_mean,
                  P2_mean=mean(P2.psi), P2_sd=sd(P2.psi), P2_cv=P2_sd/P2_mean,
                  P1bar_mean=mean(P1bar), P1bar_sd=sd(P1bar), P1bar_cv=P1bar_sd/P1bar_mean,
                  P2bar_mean=mean(P2bar), P2bar_sd=sd(P2bar), P2bar_cv=P2bar_sd/P2bar_mean,
                  Diffbar_mean=mean(Diffbar), Diffbar_sd=sd(Diffbar), Diffbar_cv=Diffbar_sd/Diffbar_mean,
                  k_rough_mean=mean(k_rough),k_rough_sd=sd(k_rough), k_rough_cv=k_rough_sd/k_rough_mean,
                  ValidPeekChoice=P2bar_mean/P1bar_mean,
                  p1_300_mean=mean(P1bar*condition_1,na.rm=TRUE),p1_300_sd=sd(P1bar*condition_1,na.rm=TRUE), p1_300_cv=p1_300_sd/p1_300_mean,
                  p2_300_mean=mean(P2bar*condition_1,na.rm=TRUE),p2_300_sd=sd(P2bar*condition_1,na.rm=TRUE), p2_300_cv=p2_300_sd/p2_300_mean,
                  ValidPeekChoice2=p2_300_mean/p1_300_mean,
                  k_300_rough_mean=mean(k_300_rough,na.rm=TRUE),k_300_rough_sd=sd(k_300_rough,na.rm=TRUE), k_300_rough_cv=k_300_rough_sd/k_300_rough_mean,
                  p3_min=min(P2bar),
                  t1_mean=mean(t1_meanx),t2_mean=mean(t2_meanx),t3_mean=mean(T3_oC),
                  ELTime2_max=max(ELTime2*condition_1,na.rm=TRUE)
        )
      
     })

  

  output$arduinom <- renderDataTable({
      DT::datatable(rvz$df_tablem, colnames = c("ELTime (s)",	"Step",	"P1(psi)", "P2(psi)",	#"T1(oC)",	"T2(oC)",	
                                                "T3(oC)" ),
                    caption = htmltools::tags$caption("data Arduino", style="color:blue"),
                    options = list(searching = FALSE,lengthChange = FALSE),
                    editable = TRUE ) %>% 
        formatRound(columns = c(5:5), digits = 2) 
    })
  
  output$tablem <- renderDataTable({
    DT::datatable(subset(df1$data_meanm, select = c(Step,n,P1_mean,P2_mean,Diffbar_mean,Diffbar_sd,Diffbar_cv) ), 
                  caption = htmltools::tags$caption("Aperçu des moyennes", style="color:blue"),
                  options = list(dom = "ft",ordering=F,
                                 pageLength = 10000,
                                 searching = FALSE),rownames= FALSE) #%>% 
     #formatRound(columns = c(3:17), digits = 2) 
  })
  
  output$table1m <- renderDataTable({
    DT::datatable(
    data_coeff,
    caption = htmltools::tags$caption(
      style = 'text-align: left; color:blue',
      'Conversion in bar	(From Preasure Sersors Calibration Tab)'
    ),

    options = list(dom = "ft",ordering=F,
                   pageLength = 10000,
                   searching = FALSE), rownames= FALSE) 
  })
  
  output$table2m <- renderDataTable({
  datatable(
    peek,
    colnames = rep("", ncol(peek)),
    caption = htmltools::tags$caption(
      style = 'text-align: left; color:blue',
      'Constant for PEEK'
    ),
    options = list(dom = "ft",ordering=F,
                   pageLength = 10000,
                   searching = FALSE),rownames= FALSE)
  })  
  
  output$table3m <- renderDataTable({
    result1 <- data.frame (first_column  = c("","","Step 1","","","","Step 2","","","","","","Step 3","","","","","","","","","",""),
                           second_column = c("","ValidPeekChoice",
                                             "Validate presuse sensors output (±?) and recalibrate if needed",
                                             "Presure sensor validation P1 (bar)",
                                             "Presure sensor validation P2 (bar)",
                                             "Average",
                                             "Readings of K« rough » measurements (wait for 5 min of stability) [Output from LabView]",
                                             "P1 (bar)",
                                             "P2 (bar)",
                                             "K rough (mmol s-1 MPa-1)",
                                             "K rough (kg s-1 Mpa-1)",
                                             "CV of K (%)",
                                             "(Close stopcock 4 off in order to measure bassin preasure… And the 3 temperatures)",
                                             "P3 [P2 with stopcock4 off] (bar)",
                                             "T1: Temperature upstream of peek tubing (oC)",
                                             "T2 : Temperature downstream of peek tubing (oC)",
                                             "Average temperature through PEEK tubing (oC)",
                                             "Resistance of PEEK tubing at actual temp (MPa mmol-1 s)",
                                             "","T3 : Temperature of water bassin with sample (oC)",
                                             "","K at measurement temperature and corrected for P3 (kg s-1 Mpa-1)",
                                             "K at 25 oC and corrected for P3 (kg s-1 Mpa-1)"
                           ),
                           third_column = c("Total",df1$data_meanm$ValidPeekChoice[2],"",df1$data_meanm$P1bar_mean[1],df1$data_meanm$P2bar_mean[1],(df1$data_meanm$P1bar_mean[1]+df1$data_meanm$P2bar_mean[1])/2,"",
                                            df1$data_meanm$P1bar_mean[2],df1$data_meanm$P2bar_mean[2],((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/Res_PEEK)/df1$data_meanm$P2bar_mean[2],
                                            ((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/Res_PEEK)/df1$data_meanm$P2bar_mean[2]*18/1000000,
                                            df1$data_meanm$k_rough_cv[2],"",df1$data_meanm$p3_min[3],df1$data_meanm$t1_mean[3],df1$data_meanm$t2_mean[3],(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2,
                                            (1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)),"",
                                            df1$data_meanm$t3_mean[3],"",
                                            (df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)))/(df1$data_meanm$P2bar_mean[2]-df1$data_meanm$p3_min[3])*18/1000000,
                                            ((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)))/(df1$data_meanm$P2bar_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))),
                           
                           fourth_column = c("Last 300 s",df1$data_meanm$ValidPeekChoice2[2],"","","","","",
                                             df1$data_meanm$p1_300_mean[2],df1$data_meanm$p2_300_mean[2],((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/Res_PEEK)/df1$data_meanm$p2_300_mean[2],
                                             ((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/Res_PEEK)/df1$data_meanm$p2_300_mean[2]*18/1000000,
                                             df1$data_meanm$k_300_rough_cv[2],"",df1$data_meanm$p3_min[3],df1$data_meanm$t1_mean[3],df1$data_meanm$t2_mean[3],(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2,
                                             (1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)),"",
                                             df1$data_meanm$t3_mean[3],"",
                                             (df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)))/(df1$data_meanm$p2_300_mean[2]-df1$data_meanm$p3_min[3])*18/1000000,
                                             ((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)))/(df1$data_meanm$p2_300_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105)))))
    )
    

    #result1x<-rbind(result1,NOMADD )
    write.csv(result1, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm7,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_STEP.csv"),  row.names=FALSE)
    
    datatable(
      result1,
      caption = htmltools::tags$caption(
        style = 'text-align: left; color:blue',
        'Steps'
      ),
      colnames = rep("", ncol(result1)),
      options = list(dom = "ft",ordering=F,
                     pageLength = 10000,
                     searching = FALSE),rownames= FALSE)
  }) 
  
  output$table4m <- renderDataTable({
    Pithdiameter=0
    result <- data.frame (first_column  = c("Overview", "Results >>>","","Validation >>>","","",""),
                          second_column  = c(
                            "Global Results",
                            "conductivity [k] (kg s-1 MPa m)",
                            "stem area-specific conductivity [ks] (kg s-1 MPa m-1)",
                            "Validation for optimal peek tube choice (parametered in V4 V5)",
                            "Preasure sensors validation (1/1000 maximum variation)",
                            "Time (min 300 sec.)",
                            "Stability (CV < 0.05)"),
                          third_column  = c("Total", ((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)))/(df1$data_meanm$P2bar_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))*(input$Stemlength/1000),
                                            ((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)))/(df1$data_meanm$P2bar_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))*(input$Stemlength/1000)/(((((input$Stemdiameter1+input$Stemdiameter2)/2/1000)/2)^2*pi-((Pithdiameter/1000)/2)^2*pi)), 
                                            base::ifelse((df1$data_meanm$ValidPeekChoice[2]>0.2 & df1$data_meanm$ValidPeekChoice[2]<0.8), "OK", "?"),
                                            base::ifelse((abs(df1$data_meanm$P1bar_mean[1]-df1$data_meanm$P2bar_mean[1])<0.001), "OK", "RECALIBRATE"),
                                            "",""),
                          fourth_column  = c("Last 300 s", ((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)))/(df1$data_meanm$p2_300_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))*(input$Stemlength/1000),
                                             ((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2)-0.001053*((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2-20)^2)/((df1$data_meanm$t1_mean[3]+df1$data_meanm$t2_mean[3])/2+105)))/(df1$data_meanm$p2_300_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))*(input$Stemlength/1000)/(((((input$Stemdiameter1+input$Stemdiameter2)/2/1000)/2)^2*pi-((Pithdiameter/1000)/2)^2*pi)), 
                                             base::ifelse((df1$data_meanm$ValidPeekChoice2[2]>0.2 & df1$data_meanm$ValidPeekChoice2[2]<0.8), "OK", "?"),
                                             base::ifelse((abs(df1$data_meanm$P1bar_mean[1]-df1$data_meanm$P2bar_mean[1])<0.001), "OK", "RECALIBRATE"),
                                             base::ifelse(df1$data_meanm$ELTime2_max[2]<294,"WAIT","OK"),
                                             base::ifelse(df1$data_meanm$k_300_rough_cv[2]<0.05,"OK","WAIT"))
    )
    #resultx<-rbind(result,NOMADD )
    write.csv(result, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm7,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_RESULTS.csv"),  row.names=FALSE)
    
    datatable(
      result,
      caption = htmltools::tags$caption(
        style = 'text-align: left; color:blue',
        'Results'
      ),
      colnames = rep("", ncol(result)),
       options = list(dom = "ft", ordering=F,
                     pageLength = 10000,
                     searching = FALSE),rownames= FALSE) %>% 
    formatStyle('third_column',
                backgroundColor = styleEqual(c("OK", "RECALIBRATE","?","WAIT",NA), c("green", "red", "red", "red", "red"))) %>% 
    formatStyle('fourth_column',
                  backgroundColor = styleEqual(c("OK", "RECALIBRATE","?","WAIT",NA), c("green", "red", "red", "red", "red")))
      
    
  })
  
  output$scatter <- renderPlot({
    z2=subset(df1$zdf1, Step == 2)
    ggplot() + geom_point(data = z2, aes(ELTime, P1bar), colour = 'blue') +
      geom_point(data = z2, aes(ELTime, P2bar), colour = 'orange') +
      ggtitle("View for Log of Step 2") +
      xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
      theme(plot.title = element_text(color = "blue"))
  }) 
  
  output$scatter2 <- renderPlot({
    z3=subset(df1$zdf1, Step == 3)
    print(head(z3))
    ggplot() + geom_point(data = z3, aes(ELTime, P1bar), colour = 'blue') +
      geom_point(data = z3, aes(ELTime, P2bar), colour = 'orange') +
      ggtitle("View for Log of Step 3") +
      xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
      theme(plot.title = element_text(color = "blue"))
  }) 
  
  output$scatter_step2  <- renderPlot({
    z2=subset(df1$zdf1, Step == 2)
    cv_P1bar <- sd(z2$P1bar) / mean(z2$P1bar) 
    g11<-ggplot() + geom_point(data = z2, aes(ELTime, P1bar), colour = 'blue') +
      ggtitle("View for Log of Step 2") +
      xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
      theme(plot.title = element_text(color = "blue"))
    cv_P2bar <- sd(z2$P2bar) / mean(z2$P2bar) 
    g12<-ggplot() + geom_point(data = z2, aes(ELTime, P2bar), colour = 'orange') +
      ggtitle("View for Log of Step 2") +
      xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
      theme(plot.title = element_text(color = "orange"))
    grid.arrange(g11, g12, ncol=2)
  })
  output$scatter_step3  <- renderPlot({
    z3=subset(df1$zdf1, Step == 3)
    cv_P1bar <- sd(z3$P1bar) / mean(z3$P1bar) 
    g21<-ggplot() + geom_point(data = z3, aes(ELTime, P1bar), colour = 'blue') +
      ggtitle(paste0("View for Log of Step 3 - cv=", round(cv_P1bar,6))) +
      xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
      theme(plot.title = element_text(color = "blue")) 
    cv_P2bar <- sd(z3$P2bar) / mean(z3$P2bar) 
    g22<-ggplot() + geom_point(data = z3, aes(ELTime, P2bar), colour = 'orange') +
      ggtitle(paste0("View for Log of Step 3 - cv=", round(cv_P2bar,6))) +
      xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
      theme(plot.title = element_text(color = "orange"))
    grid.arrange(g21, g22, ncol=2)
  })
  
  ####### DOWNLOAD BUTTON 
  output$downloadDatam <- downloadHandler(
    filename <- function() {
      paste("output", "zip", sep=".")
    },
    
    content <- function(fname) {
      fs <- c("OUTPUTS/SINGLE")
      zip::zipr(zipfile=fname, files=fs)
    }
  )
  
  }) #observeEvent
  
#resultats SingleKmeasurement
  observeEvent(input$resetAllm, {
    shinyjs::reset("myappm")
    rvz$df_tablem<-NULL
    df1$data_meanm<-NULL
    output$arduinom <- renderDataTable({})
    output$table4m <- renderDataTable({})
    output$table1m <- renderDataTable({})
    output$table2m <- renderDataTable({})
    output$table3m <- renderDataTable({})    
    output$scatter <- renderPlot({}) 
  }) 
  
   
  
  
  ############################################################
  ############################################################
  #ETAPE 3i SingleKmeasurement  (lecture)
  observe({
    #volumes = getVolumes() # this makes the directory at the base of your computer.
    shinyFileChoose(input, 'folder3i', roots=c(wd='.'), defaultPath="OUTPUTS\\SINGLE", pattern="_ARDUINO", filetypes=c('csv'))
    
  }) 
    rvw2 <- reactiveValues(
      intrantw =data.frame("first_column" = character(),	"second_column" = character()
      ))  

  observeEvent(input$recalcL3i, {
    folder<-substr(as.character(input$folder3i)[1],41,1000000L)
    print("folder")
    print(folder)
    x<-unlist(gregexpr(pattern ='_ARDUINO.csv',folder))
    print("x")
    print(x)
    nomfile=paste0(substr(folder,1,x[1]-1))
    print("Nom du fichier de mesure importé")
    print(nomfile)
    a<-base::gsub(',', '', nomfile)
    a<- base::strsplit(a, '\" \"')
    print(a)
print(is.na(a[[1]][2]))

    if(is.na(a[[1]][2])){
      nomfile2=paste0("OUTPUTS/SINGLE/")}
    if(!is.na(a[[1]][2])){
      nomfile<-a[[1]][3]
      print(nomfile)
      nomfile2=paste0("OUTPUTS/SINGLE/",a[[1]][1],"/",a[[1]][2],"/")}
    
print(nomfile2)
print(paste0(nomfile2, nomfile,"_ARDUINO.csv"))
    
    arduino<-read.csv(paste0(nomfile2, nomfile,"_ARDUINO.csv"))
    data_coeff<-read.csv(paste0(nomfile2,nomfile,"_COEFF.csv"))
    peek<-read.csv(paste0(nomfile2,nomfile,"_PEEK.csv"))
    result1<-read.csv(paste0(nomfile2,nomfile,"_STEP.csv"))
    result<-read.csv(paste0(nomfile2, nomfile,"_RESULTS.csv"))
    intrant<-read.csv(paste0(nomfile2, nomfile,"_INTRANT.csv"))
    Res_PEEK<-peek$second_column[2]
    #intrant[6,2]<-as.character(zoo::as.Date(as.numeric(intrant[6,2])))

    rvw2$intrantw <- intrant
    rvw1<- reactiveValues(
      resultw =result,
      result1w =result1)
    rvz <- reactiveValues(
      df_tablem =arduino)

    df1 <- reactiveValues(
      data_meanm =data.frame("Step", "n", "P1_mean", "P1_sd", "P1_cv","P2_mean","P2_sd","P2_cv",                           "P2_mean", "P2_sd", "P2_cv",
                             "P1bar_mean", "P1bar_sd", "P1bar_cv",    "P2bar_mean", "P2bar_sd", "P2bar_cv",
                             "Diffbar_mean", "Diffbar_sd", "Diffbar_cv",
                             "k_rough_mean","k_rough_sd","k_rough_cv",
                             "ValidPeekChoice",
                             "p1_300_mean","p1_300_sd","p1_300_cv",
                             "p2_300_mean","p2_300_sd","p2_300_cv",
                             "ValidPeekChoice2",
                             "k_300_rough_mean","k_300_rough_sd","k_300_rough_cv",
                             "p3_min","t1_mean","t2_mean","t3_mean","ELTime2_max"
      )
    )
  
    z<-rvz$df_tablem
    z$t1_meanx=as.numeric(intrant[15,2])
    z$t2_meanx=as.numeric(intrant[16,2])
    z <- z %>%
      group_by(Step) %>%
      mutate(ELTimemax =max(ELTime))
    z <- z %>% 
      mutate(P1bar=data_coeff$P1[2]+data_coeff$P1[3]*P1.psi,
             P2bar=data_coeff$P2[2]+data_coeff$P2[3]*P2.psi,
             Diffbar=abs(P1bar-P2bar),
             k_rough=((P1bar-P2bar)/as.numeric(peek$second_column[2]))/P2bar,
             ELTime2=ELTimemax-ELTime,
             condition_1 = na_if(1*((ELTimemax-ELTime)<300),0),
             k_300_rough=((P1bar*condition_1-P2bar*condition_1)/as.numeric(peek$second_column[2]))/P2bar*condition_1,
             Pithdiameter=0
      )
    df1$zdf1 <- z
    
    df1$data_meanm <- z %>% 
      group_by(Step) %>% 
      summarise(n=n(), P1_mean=mean(P1.psi), P1_sd=sd(P1.psi), P1_cv=P1_sd/P1_mean,
                P2_mean=mean(P2.psi), P2_sd=sd(P2.psi), P2_cv=P2_sd/P2_mean,
                P1bar_mean=mean(P1bar), P1bar_sd=sd(P1bar), P1bar_cv=P1bar_sd/P1bar_mean,
                P2bar_mean=mean(P2bar), P2bar_sd=sd(P2bar), P2bar_cv=P2bar_sd/P2bar_mean,
                Diffbar_mean=mean(Diffbar), Diffbar_sd=sd(Diffbar), Diffbar_cv=Diffbar_sd/Diffbar_mean,
                k_rough_mean=mean(k_rough),k_rough_sd=sd(k_rough), k_rough_cv=k_rough_sd/k_rough_mean,
                ValidPeekChoice=P2bar_mean/P1bar_mean,
                p1_300_mean=mean(P1bar*condition_1,na.rm=TRUE),p1_300_sd=sd(P1bar*condition_1,na.rm=TRUE), p1_300_cv=p1_300_sd/p1_300_mean,
                p2_300_mean=mean(P2bar*condition_1,na.rm=TRUE),p2_300_sd=sd(P2bar*condition_1,na.rm=TRUE), p2_300_cv=p2_300_sd/p2_300_mean,
                ValidPeekChoice2=p2_300_mean/p1_300_mean,
                k_300_rough_mean=mean(k_300_rough,na.rm=TRUE),k_300_rough_sd=sd(k_300_rough,na.rm=TRUE), k_300_rough_cv=k_300_rough_sd/k_300_rough_mean,
                p3_min=min(P2bar),
                t1_mean=mean(t1_meanx),t2_mean=mean(t2_meanx),t3_mean=mean(T3_oC),
                ELTime2_max=max(ELTime2*condition_1,na.rm=TRUE)
      )
   
     observeEvent(input$intrantL1_cell_edit, {
      rvw2$intrantw[input$intrantL1_cell_edit$row,2] <<- (DT::coerceValue(input$intrantL1_cell_edit$value, rvw2$intrantw[input$intrantL1_cell_edit$row,2]))
       Pithdiameter=0
       Res_PEEK =as.numeric(peek$second_column[2])
       Stemlength=as.numeric(rvw2$intrantw[12,2])
       Stemdiameter1=as.numeric(rvw2$intrantw[10,2])
       Stemdiameter2=as.numeric(rvw2$intrantw[11,2])
       t1_mean3=as.numeric(rvw2$intrantw[15,2])
       t2_mean3=as.numeric(rvw2$intrantw[16,2])
       
       result <- data.frame (first_column  = c("Overview", "Results >>>","","Validation >>>","","",""),
                             second_column  = c(
                               "Global Results",
                               "conductivity [k] (kg s-1 MPa m)",
                               "stem area-specific conductivity [ks] (kg s-1 MPa m-1)",
                               "Validation for optimal peek tube choice (parametered in V4 V5)",
                               "Preasure sensors validation (1/1000 maximum variation)",
                               "Time (min 300 sec.)",
                               "Stability (CV < 0.05)"),
                             third_column  = c("Total", ((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)))/(df1$data_meanm$P2bar_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))*(Stemlength/1000),
                                               ((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)))/(df1$data_meanm$P2bar_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))*(Stemlength/1000)/(((((Stemdiameter1+Stemdiameter2)/2/1000)/2)^2*pi-((Pithdiameter/1000)/2)^2*pi)), 
                                               base::ifelse((df1$data_meanm$ValidPeekChoice[2]>0.2 & df1$data_meanm$ValidPeekChoice[2]<0.8), "OK", "?"),
                                               base::ifelse((abs(df1$data_meanm$P1bar_mean[1]-df1$data_meanm$P2bar_mean[1])<0.001), "OK", "RECALIBRATE"),
                                               "",""),
                             fourth_column  = c("Last 300 s", ((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)))/(df1$data_meanm$p2_300_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))*(Stemlength/1000),
                                                ((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)))/(df1$data_meanm$p2_300_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))*(Stemlength/1000)/(((((Stemdiameter1+Stemdiameter2)/2/1000)/2)^2*pi-((Pithdiameter/1000)/2)^2*pi)), 
                                                base::ifelse((df1$data_meanm$ValidPeekChoice2[2]>0.2 & df1$data_meanm$ValidPeekChoice2[2]<0.8), "OK", "?"),
                                                base::ifelse((abs(df1$data_meanm$P1bar_mean[1]-df1$data_meanm$P2bar_mean[1])<0.001), "OK", "RECALIBRATE"),
                                                base::ifelse(df1$data_meanm$ELTime2_max[2]<294,"WAIT","OK"),
                                                base::ifelse(df1$data_meanm$k_300_rough_cv[2]<0.05,"OK","WAIT"))
       )
       
       result1 <- data.frame (first_column  = c("","","Step 1","","","","Step 2","","","","","","Step 3","","","","","","","","","",""),
                              second_column = c("","ValidPeekChoice",
                                                "Validate presuse sensors output (±?) and recalibrate if needed",
                                                "Presure sensor validation P1 (bar)",
                                                "Presure sensor validation P2 (bar)",
                                                "Average",
                                                "Readings of K« rough » measurements (wait for 5 min of stability) [Output from LabView]",
                                                "P1 (bar)",
                                                "P2 (bar)",
                                                "K rough (mmol s-1 MPa-1)",
                                                "K rough (kg s-1 Mpa-1)",
                                                "CV of K (%)",
                                                "(Close stopcock 4 off in order to measure bassin preasure… And the 3 temperatures)",
                                                "P3 [P2 with stopcock4 off] (bar)",
                                                "T1: Temperature upstream of peek tubing (oC)",
                                                "T2 : Temperature downstream of peek tubing (oC)",
                                                "Average temperature through PEEK tubing (oC)",
                                                "Resistance of PEEK tubing at actual temp (MPa mmol-1 s)",
                                                "","T3 : Temperature of water bassin with sample (oC)",
                                                "","K at measurement temperature and corrected for P3 (kg s-1 Mpa-1)",
                                                "K at 25 oC and corrected for P3 (kg s-1 Mpa-1)"
                              ),
                              third_column = c("Total",df1$data_meanm$ValidPeekChoice[2],"",df1$data_meanm$P1bar_mean[1],df1$data_meanm$P2bar_mean[1],(df1$data_meanm$P1bar_mean[1]+df1$data_meanm$P2bar_mean[1])/2,"",
                                               df1$data_meanm$P1bar_mean[2],df1$data_meanm$P2bar_mean[2],((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/Res_PEEK)/df1$data_meanm$P2bar_mean[2],
                                               ((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/Res_PEEK)/df1$data_meanm$P2bar_mean[2]*18/1000000,
                                               df1$data_meanm$k_rough_cv[2],"",df1$data_meanm$p3_min[3],t1_mean3,t2_mean3,(t1_mean3+t2_mean3)/2,
                                               (1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)),"",
                                               df1$data_meanm$t3_mean[3],"",
                                               (df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)))/(df1$data_meanm$P2bar_mean[2]-df1$data_meanm$p3_min[3])*18/1000000,
                                               ((df1$data_meanm$P1bar_mean[2]-df1$data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)))/(df1$data_meanm$P2bar_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105))))),
                              
                              fourth_column = c("Last 300 s",df1$data_meanm$ValidPeekChoice2[2],"","","","","",
                                                df1$data_meanm$p1_300_mean[2],df1$data_meanm$p2_300_mean[2],((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/Res_PEEK)/df1$data_meanm$p2_300_mean[2],
                                                ((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/Res_PEEK)/df1$data_meanm$p2_300_mean[2]*18/1000000,
                                                df1$data_meanm$k_300_rough_cv[2],"",df1$data_meanm$p3_min[3],t1_mean3,t2_mean3,(t1_mean3+t2_mean3)/2,
                                                (1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)),"",
                                                df1$data_meanm$t3_mean[3],"",
                                                (df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)))/(df1$data_meanm$p2_300_mean[2]-df1$data_meanm$p3_min[3])*18/1000000,
                                                ((df1$data_meanm$p1_300_mean[2]-df1$data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(t1_mean3+t2_mean3)/2)-0.001053*((t1_mean3+t2_mean3)/2-20)^2)/((t1_mean3+t2_mean3)/2+105)))/(df1$data_meanm$p2_300_mean[2]-df1$data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-df1$data_meanm$t3_mean[3])-0.001053*(df1$data_meanm$t3_mean[3]-20)^2)/(df1$data_meanm$t3_mean[3]+105)))))
       )
       
       
      
       rvw1$resultw<-result
       rvw1$result1w<-result1
       write.csv(rvw2$intrantw, paste0(nomfile2,nomfile,"_INTRANT.csv"),  row.names=FALSE)
       write.csv(rvw1$resultw, paste0(nomfile2,nomfile,"_RESULTS.csv"),  row.names=FALSE)
       write.csv(rvw1$result1w, paste0(nomfile2,nomfile,"_STEP.csv"),  row.names=FALSE)
     })

     output$intrantL1 <-  renderDataTable({
     #print(rvw2$intrantw)
       DT::datatable(rvw2$intrantw,
                     caption = htmltools::tags$caption( style = 'text-align: left; color:blue',"Intrants"),
                     options = list(dom = "ft",ordering=F,
                                    pageLength = 10000,
                                    searching = FALSE),
                     colnames = rep("", ncol(rvw2$intrantw)),
                     rownames= FALSE,
                     editable = TRUE ) 
     })

    output$arduinomL <- renderDataTable({
      DT::datatable(rvz$df_tablem, colnames = c("ELTime (s)",	"Step",	"P1(psi)", "P2(psi)",	#"T1(oC)",	"T2(oC)",	
                                                "T3(oC)" ),
                    caption = htmltools::tags$caption("data Arduino", style="color:blue"),
                    options = list(searching = FALSE,lengthChange = FALSE)) %>% 
        formatRound(columns = c(5:5), digits = 2) 
    })    
 
    output$table1mL <- renderDataTable({
      DT::datatable(
        data_coeff,
        caption = htmltools::tags$caption(
          style = 'text-align: left; color:blue',
          'Conversion in bar	(From Preasure Sersors Calibration Tab)'
        ),
        options = list(dom = "ft",ordering=F,
                       pageLength = 10000,
                       searching = FALSE), rownames= FALSE) 
    })

    output$table2mL <- renderDataTable({
      datatable(
        peek,
        colnames = rep("", ncol(peek)),
        caption = htmltools::tags$caption(
          style = 'text-align: left; color:blue',
          'Constant for PEEK'
        ),
        options = list(dom = "ft",ordering=F,
                       pageLength = 10000,
                       searching = FALSE),rownames= FALSE)
    })

    output$table3mL <- renderDataTable({
      datatable(
        rvw1$result1w,
        caption = htmltools::tags$caption(
          style = 'text-align: left; color:blue',
          'Steps'
        ),
        #colnames = rep("", ncol(result1)),
        options = list(dom = "ft",ordering=F,
                       pageLength = 10000,
                       searching = FALSE),rownames= FALSE) 
    })

    output$table4mL <- renderDataTable({
      Pithdiameter=0
      #print(rvw1$resultw)
      datatable(
        rvw1$resultw,
        caption = htmltools::tags$caption(
          style = 'text-align: left; color:blue',
          'Results'
        ),
       # colnames = rep("", ncol(rvw1$resultw)),
        options = list(dom = "ft", ordering=F,
                       pageLength = 10000,
                       searching = FALSE),rownames= FALSE) %>% 
        formatStyle('third_column',
                    backgroundColor = styleEqual(c("OK", "RECALIBRATE","?","WAIT",NA), c("green", "red", "red", "red", "red"))) %>% 
        formatStyle('fourth_column',
                    backgroundColor = styleEqual(c("OK", "RECALIBRATE","?","WAIT",NA), c("green", "red", "red", "red", "red")))
      
      
    })    

    output$scatterL <- renderPlot({
      z2=subset(df1$zdf1, Step == 2)
      ggplot() + geom_point(data = z2, aes(ELTime, P1bar), colour = 'blue') +
        geom_point(data = z2, aes(ELTime, P2bar), colour = 'orange') +
        ggtitle("View for Log of Step 2") +
        xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
        theme(plot.title = element_text(color = "blue"))
    })
    
    output$scatter2L <- renderPlot({
      z3=subset(df1$zdf1, Step == 3)
      print(head(z3))
      ggplot() + geom_point(data = z3, aes(ELTime, P1bar), colour = 'blue') +
        geom_point(data = z3, aes(ELTime, P2bar), colour = 'orange') +
        ggtitle("View for Log of Step 3") +
        xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
        theme(plot.title = element_text(color = "blue"))
    }) 
    
    output$scatterL_step2  <- renderPlot({
      z2=subset(df1$zdf1, Step == 2)
      g11<-ggplot() + geom_point(data = z2, aes(ELTime, P1bar), colour = 'blue') +
        ggtitle("View for Log of Step 2") +
        xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
        theme(plot.title = element_text(color = "blue"))
      g12<-ggplot() + geom_point(data = z2, aes(ELTime, P2bar), colour = 'orange') +
        ggtitle("View for Log of Step 2") +
        xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
        theme(plot.title = element_text(color = "orange"))
      grid.arrange(g11, g12, ncol=2)
    })
    output$scatterL_step3  <- renderPlot({
      z3=subset(df1$zdf1, Step == 3)
      cv_P1bar <- sd(z3$P1bar) / mean(z3$P1bar) 
      g21<-ggplot() + geom_point(data = z3, aes(ELTime, P1bar), colour = 'blue') +
        ggtitle(paste0("View for Log of Step 3 - cv=", round(cv_P1bar,6))) +
        xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
        theme(plot.title = element_text(color = "blue"))
      cv_P2bar <- sd(z3$P2bar) / mean(z3$P2bar) 
      g22<-ggplot() + geom_point(data = z3, aes(ELTime, P2bar), colour = 'orange') +
        ggtitle(paste0("View for Log of Step 3 - cv=", round(cv_P2bar,6))) +
        xlab("Total Elapsed Time (sec)") + ylab("Preasure (bar)")+
        theme(plot.title = element_text(color = "orange"))
      grid.arrange(g21, g22, ncol=2)
    })
    
  })  
  
  #("RENOMMER FICHIERS")
  observeEvent(input$renamedata, {
    print(getwd())
    folder<-substr(as.character(input$folder3i)[1],41,1000000L)
    print("Renommer fichier")
    print("folder")
    print(folder)
    x<-unlist(gregexpr(pattern ='_ARDUINO.csv',folder))
    print("x")
    print(x)
    nomfile=paste0(substr(folder,1,x[1]-1))
    print("Nom du fichier de mesure importé")
    print(nomfile)
    a<-base::gsub(',', '', nomfile)
    a<- base::strsplit(a, '\" \"')
    print(a)
    print(is.na(a[[1]][2]))
    
    if(is.na(a[[1]][2])){
      nomfile2=paste0("OUTPUTS/SINGLE/")}
    if(!is.na(a[[1]][2])){
      nomfile<-a[[1]][3]
      print(nomfile)
      nomfile2=paste0("OUTPUTS/SINGLE/",a[[1]][1],"/",a[[1]][2],"/")}
    
    print(nomfile2)
    print(paste0(nomfile2, nomfile,"_ARDUINO.csv"))
    cat(nomfile2, "\n")
    cat(nomfile, "\n")
    cat(paste0(nomfile2, nomfile,"_INTRANT.csv"), "\n")
    intrantx<-read.csv(paste0(nomfile2, nomfile,"_INTRANT.csv"))
    arduinox<-read.csv(paste0(nomfile2, nomfile,"_ARDUINO.csv"))
    data_coeffx<-read.csv(paste0(nomfile2,nomfile,"_COEFF.csv"))
    peekx<-read.csv(paste0(nomfile2,nomfile,"_PEEK.csv"))
    result1x<-read.csv(paste0(nomfile2,nomfile,"_STEP.csv"))
    resultx<-read.csv(paste0(nomfile2, nomfile,"_RESULTS.csv"))

    cat("parms \n")
    parm1=intrantx[1,2]
    parm7=intrantx[14,2]
    parm2=intrantx[2,2]
    parm3=intrantx[3,2]
    parm4=intrantx[4,2]
    parm5=intrantx[5,2]
    parm6=intrantx[6,2]
    cat("write \n")
    dir.create(paste0("OUTPUTS/SINGLE/",parm1))
    dir.create(paste0("OUTPUTS/SINGLE/",parm1,"/",parm7))
    cat(paste0("OUTPUTS/SINGLE/", parm1,"/",parm7,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_INTRANT.csv"), "write \n")
    write.csv(intrantx, paste0("OUTPUTS/SINGLE/", parm1,"/",parm7,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_INTRANT.csv"),  row.names=FALSE)
    write.csv(arduinox, paste0("OUTPUTS/SINGLE/", parm1,"/",parm7,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_ARDUINO.csv"),  row.names=FALSE)
    write.csv(data_coeffx, paste0("OUTPUTS/SINGLE/", parm1,"/",parm7,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_COEFF.csv"),  row.names=FALSE)
    write.csv(peekx, paste0("OUTPUTS/SINGLE/", parm1,"/",parm7,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_PEEK.csv"),  row.names=FALSE)
    write.csv(result1x, paste0("OUTPUTS/SINGLE/", parm1,"/",parm7,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_STEP.csv"),  row.names=FALSE)
    write.csv(resultx, paste0("OUTPUTS/SINGLE/", parm1,"/",parm7,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_RESULTS.csv"),  row.names=FALSE)
    
    dir.create("OUTPUTS/DELETE/")
    file.copy(paste0(nomfile2, nomfile,"_INTRANT.csv"), "OUTPUTS/DELETE")
    unlink(paste0(nomfile2, nomfile,"_INTRANT.csv"))
    file.copy(paste0(nomfile2, nomfile,"_ARDUINO.csv"), "OUTPUTS/DELETE")
    unlink(paste0(nomfile2, nomfile,"_ARDUINO.csv"))
    file.copy(paste0(nomfile2, nomfile,"_COEFF.csv"), "OUTPUTS/DELETE")
    unlink(paste0(nomfile2, nomfile,"_COEFF.csv"))
    file.copy(paste0(nomfile2, nomfile,"_PEEK.csv"), "OUTPUTS/DELETE")
    unlink(paste0(nomfile2, nomfile,"_PEEK.csv"))
    file.copy(paste0(nomfile2, nomfile,"_STEP.csv"), "OUTPUTS/DELETE")
    unlink(paste0(nomfile2, nomfile,"_STEP.csv"))
    file.copy(paste0(nomfile2, nomfile,"_RESULTS.csv"), "OUTPUTS/DELETE")
    unlink(paste0(nomfile2, nomfile,"_RESULTS.csv"))

  })
  
  #resultats SingleKmeasurement
  observeEvent(input$refresh, {
    shinyjs::reset("myapp3i")
    #shinyjs::reset("refresh")
    #shinyjs::js$refresh()
    refresh()
    reset(id = "", asis = FALSE)
    rvw2$intrantw<-NULL
    rvz$df_tablem<-NULL
    df1$zdf1<-NULL
    df1$data_meanm <- NULL
    intrant<-NULL
    result <-NULL
    output$arduinomL <- renderDataTable({})
    output$table4mL <- renderDataTable({})
    output$intrantL1 <- renderDataTable({})
    output$table2mL <- renderDataTable({})
    output$table3mL <- renderDataTable({})  
    output$table1mL <- renderDataTable({}) 
    output$scatterL <- renderPlot({}) 
  }) 
  


  
############################################################
############################################################
#ETAPE 4 ODS
  #Dossier qui contient tout les sous-dossiers avec les feuilles ODS
  #observe({
  #  shinyDirChoose(input, 'folder4', roots=c(wd='.'), defaultPath="OUTPUTS", filetypes=c('', 'csv'))
  #}) 
  
  observeEvent(input$recalc4, {
    #folder<-as.character(input$folder4$path[[2]])
    direct<-getwd()
    print("direct")
    print(getwd())
    setwd(input$DIR)
    print("local")
    print(getwd())
    dir.create("SORTIE", showWarnings = F)
    #Feuille à lire
    #SHEET="SingleKmeasurement"
print(paste0(input$DIR))
    #liste de tous les fichiers ODS
    liste_fichiers<- ( list.files(path=getwd(), recursive = T, pattern="_INTRANT.csv") )
    #liste des fichiers deja lus
    liste_fichiers_deja_lus<-data.frame(no=integer(),
                                        fichierslu=character()) 
    print("BD")
    BD<-data.frame(matrix(ncol = 34, nrow = 0))
    x <- c("XYLEM","ID_ECHANTILLON","ID_MESURE","INTRANT3","POTENTIEL","INFILTRATION",
           "DATE","GAMME_PEEK_TUBE",	"VALID_PEEK_TUBE",	"VALID_PSENSOR",	"TIME",	"STABILITY",	
           "SLOPE_P1","INTERCEPT_P1","SLOPE_P2","INTERCEPT_P2","R_PEEKTUBE_25","P1","P2","K_BRUT_MMOL",
           "K_BRUT_KG","CV","P3","T1","T2","T_AV","R_PEEKTUBE_T","T3","K_T","K_25","D","AS","L","KS")
    colnames(BD) <- x

    if (file.exists("SORTIE/liste_fichiers.csv")){
      liste_fichiers_deja_lus<-read.csv("SORTIE/liste_fichiers.csv")
      BD<-read.csv("SORTIE/BD.csv")
    }
    colnames(liste_fichiers_deja_lus)<-c("no","fichierslu")
    
    #liste des nouveaux fichiers
    already_read <-(liste_fichiers_deja_lus$fichierslu)
    #colnames(already_read)<-c("fichierslu")
    to_read <- setdiff(liste_fichiers,already_read)
    
    
    #boucle sur fichiers à lire
    if (length(to_read)>0) { #condition s'il y en a des nouveaux
      for (i in 1:length(to_read)){ #boucle
        x<-unlist(gregexpr(pattern ='_INTRANT.csv',to_read[i]))
        fichier=substr(to_read[i],1,x[1]-1)
             
        cat("fichier", i, fichier, "\n")
        #pour imprimer dans la shiny
        showModal(modalDialog(
          title = "Lecture du fichier:",
          paste0("fichier", i, fichier, "\n"),
          easyClose = TRUE,
          footer = NULL
        ))
        
        #condition que la feuille existe
        ARDUINO <- tryCatch({
          # The code you want run
          read.csv(paste0(fichier,"_ARDUINO.csv"))
         }, warning = function(war) {
          # Is executed if warning encountered
          NULL
        }, error = function(err) {
          # Is executed if error encountered
          NULL
        })
        COEFF <- tryCatch({
          read.csv(paste0(fichier,"_COEFF.csv"))
        }, warning = function(war) {          NULL        }, error = function(err) {          NULL        })
        INTRANT <- tryCatch({
          read.csv(paste0(fichier,"_INTRANT.csv"))
        }, warning = function(war) {          NULL        }, error = function(err) {          NULL        })
        PEEK <- tryCatch({
          read.csv(paste0(fichier,"_PEEK.csv"))
        }, warning = function(war) {          NULL        }, error = function(err) {          NULL        })
        RESULTS <- tryCatch({
          read.csv(paste0(fichier,"_RESULTS.csv"))
        }, warning = function(war) {          NULL        }, error = function(err) {          NULL        })
        STEP <- tryCatch({
          read.csv(paste0(fichier,"_STEP.csv"))
        }, warning = function(war) {          NULL        }, error = function(err) {          NULL        })

        XYLEM<-	INTRANT[1,2]
        ID_ECHANTILLON<-	INTRANT[2,2]
        ID_MESURE<-	as.character(zoo::as.Date(as.numeric(INTRANT[6,2])))
        INTRANT3<-	INTRANT[3,2]
        POTENTIEL<-	INTRANT[4,2]
        INFILTRATION<-	INTRANT[5,2]
        DATE<-	as.character(zoo::as.Date(as.numeric(INTRANT[6,2])))
        GAMME_PEEK_TUBE<-	STEP[2,4]	
        VALID_PEEK_TUBE<-	RESULTS[4,4]
        VALID_PSENSOR<-	RESULTS[5,4]	
        TIME<-	RESULTS[6,4]	
        STABILITY<-	RESULTS[7,4]
        SLOPE_P1<-	COEFF[3,2]
        INTERCEPT_P1<-	COEFF[2,2]
        SLOPE_P2<-	COEFF[3,3]
        INTERCEPT_P2<-	COEFF[2,3]
        R_PEEKTUBE_25<-	PEEK[2,2]
        P1<-	STEP[8,4]
        P2<-	STEP[9,4]
        K_BRUT_MMOL<-	STEP[10,4]
        K_BRUT_KG<-	STEP[11,4]
        CV<-	STEP[12,4]
        P3<-	STEP[14,4]	
        T1<-	STEP[15,4]  #	
        T2<-	STEP[16,4]
        T_AV<-	STEP[17,4]	#
        R_PEEKTUBE_T<-	STEP[18,4] #
        T3<-	STEP[20,4] #
        K_T<-	STEP[22,4] #
        K_25<-	STEP[23,4] #
        D<-	(as.numeric(INTRANT[10,2])+as.numeric(INTRANT[11,2]))/2
        AS<-(((((as.numeric(INTRANT[10,2])+as.numeric(INTRANT[11,2]))/2/1000)/2)^2*pi-((0/1000)/2)^2*pi))
        L<- INTRANT[12,2]
        KS<- RESULTS[3,4] #
        fichier_res<-as.data.frame(cbind(XYLEM,ID_ECHANTILLON,ID_MESURE,INTRANT3,POTENTIEL,INFILTRATION,
                                     DATE,GAMME_PEEK_TUBE,	VALID_PEEK_TUBE,	VALID_PSENSOR,	TIME,	STABILITY,	
                                     SLOPE_P1,INTERCEPT_P1,SLOPE_P2,INTERCEPT_P2,R_PEEKTUBE_25,P1,P2,K_BRUT_MMOL,
                                     K_BRUT_KG,CV,P3,T1,T2,T_AV,R_PEEKTUBE_T,T3,K_T,K_25,D,AS,L,KS))
        
        BD<-plyr::rbind.fill(BD,fichier_res)
      } #dim(to_read)[1]
      removeModal(session = getDefaultReactiveDomain())
      #Exportation de la BD des fichiers qui sont lus
      write.csv(BD,"SORTIE/BD.csv", row.names=F)
      #Exportation du fichier qui contient la liste de fichiers qui sont lus
      write.csv(liste_fichiers,"SORTIE/liste_fichiers.csv")
      
    } #length(to_read)>0

    output$text <- renderText({ "Terminer" })
    setwd(direct)
  }) ##ETAPE 4 ODS


#fermer l'application  
  observe({
    if (input$close > 0) stopApp()                             # stop shiny
  })
  
}
