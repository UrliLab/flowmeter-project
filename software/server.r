#function for height separation
extract <- function(text) {
text <- gsub(" ", "", text)
split <- strsplit(text, ",", fixed = FALSE)[[1]]
as.numeric(split)
}

# Creation of project directories
if (file.exists("OUTPUTS")) {} else {dir.create("OUTPUTS")}
if (file.exists("OUTPUTS/CALIBRATION")) {} else {dir.create("OUTPUTS/CALIBRATION")}
if (file.exists("OUTPUTS/SINGLE")) {} else {dir.create("OUTPUTS/SINGLE")}
if (file.exists("OUTPUTS/PEEK")) {} else {dir.create("OUTPUTS/PEEK")}
if (file.exists("OUTPUTS/PEEK/id")) {} else {dir.create("OUTPUTS/PEEK/id")}
if (file.exists("OUTPUTS/PEEK/measurement")) {} else {dir.create("OUTPUTS/PEEK/measurement")}


#start of the application
server <- function(input, output, session) {
  
############################################################
############################################################
#STEP 1 PEEK tubing calibration
  print("start")  
  rv <- reactiveValues(
    df = data.frame(
      Height = character(),
      Duration = character(),
      Volume = character(),
      Temperature = character()
    )
  )
  dfPEEK <- reactiveValues(
    dfPEEK_chg =data.frame("HeightPEEK","TimePEEK","VolPEEK","TempPEEK",
                           "TIME_H","FLUX","FLUX_KGS","PRESSURE_PA","PRESSURE_MPA","K","N","FLUX_MMOLS")
  )
  dfresults <- reactiveValues(
    R_25 =data.frame(),
    K_25 =data.frame()
  )
  rv4 <- reactiveValues(
    df4 = data.frame(
      PARAMETER1 = character(),
      OPERATOR = character(),
      PEEK_tubing_ID = character(),
      DEVICE = character(),
      K_25 = character(),
      R_25 = character()
    )
  )

  observe({
    print("CALIBRATION PEEK color")
    shinyFiles::shinyFileChoose(input, 'folderPC', roots=c(wd='.'), defaultPath="OUTPUTS/PEEK/id/", pattern="_PEEKid", filetypes=c('csv'))
    folderPC<-substr(as.character(input$folderPC)[1],47,1000000L)
    print("folderPC")
    print(folderPC)
    x2<-unlist(gregexpr(pattern ='_PEEKid.csv',folderPC))
    namefile2=substr(folderPC,1,x2[1]-1)
    print("namefile2")#### 
    print(namefile2)#### 
    
    if (file.exists(paste0("OUTPUTS/PEEK/id/",namefile2,"_PEEKid.csv"))) {
      rv4$df4 <- read.csv(paste0("OUTPUTS/PEEK/id/",namefile2,"_PEEKid.csv"))
    }
    
  })
  
  values <- reactiveValues(
    upload_5id = NULL,
    upload_5 = NULL
  )
  
  observeEvent(input$folderPC5id, {
    values$upload_5id <- 'upload5id'
  })
  observeEvent(input$folderPC5, {
    values$upload_5 <- 'upload5'
  })
  observeEvent(input$reset5, {
    values$upload_5id <- 'reset5'
    values$upload_5 <- 'reset5'
    output$tablePEEKtub <- DT::renderDT({})
    shinyjs::reset("nomid5")
  })
  
  observe({
    print("4- Average")
    print("PEEK color id")
    shinyFiles::shinyFileChoose(input, 'folderPC5id', roots=c(wd='.'), defaultPath="OUTPUTS/PEEK/id/", pattern="_PEEKid.csv", filetypes=c('csv'))
    folderPC5id_<-substr(as.character(file_input())[1],47,1000000L)
    x2<-unlist(gregexpr(pattern ='_PEEKid.csv',folderPC5id_))
    namefile5id=substr(folderPC5id_,1,x2[1]-1)
    output$rawInputValue41 <- renderText({namefile5id})
  })

  
  observe({
    print("PEEK color")
    shinyFiles::shinyFileChoose(input, 'folderPC5', roots=c(wd='.'), defaultPath="www/color/", pattern="_PEEK.csv", filetypes=c('csv'))
    folderPC5_<-substr(as.character(file_input1())[1],38,1000000L)
    x2<-unlist(gregexpr(pattern ='_PEEK.csv',folderPC5_))
    namefile5=substr(folderPC5_,1,x2[1]-1)
    output$rawInputValue42 <- renderText({namefile5})
  })

  file_input <- reactive({
    if (is.null(values$upload_5id)) {
      return(NULL)
    } else if (values$upload_5id == 'upload5id') {
      return(input$folderPC5id)
    } else if (values$upload_5id == 'reset5') {
      return(NULL)
    }
  })
  file_input1 <- reactive({
    if (is.null(values$upload_5)) {
      return(NULL)
    } else if (values$upload_5 == 'upload5') {
      return(input$folderPC5)
    } else if (values$upload_5 == 'reset5') {
      return(NULL)
    }
  })
  

  print("add")  
  observeEvent(input$Add, {
    cat("--Validate \n")
    validate(
      need(input$HeightPEEK != "", 'Please choose a state.'),
      need(input$TimePEEK != "", 'Please choose a state.'),
      need(input$VolPEEK != "", 'Please choose a state.'),
      need(input$TempPEEK != "", 'Please choose a state.'),
      need(input$IDMEASUREMENT1 != "", 'Please choose a state.')
    )
 
    rv$df <- rbind(rv$df, data.frame(
      Height = input$HeightPEEK, 
      Duration = input$TimePEEK,
      Volume = input$VolPEEK,
      Temperature = input$TempPEEK) %>%
        na.omit())
    print(rv$df)
    
    df_char<-rv$df
    print("numeric")
    HeightPEEK_<- as.numeric(df_char$Height)
    TimePEEK_<- as.numeric(df_char$Duration)
    VolPEEK_<- as.numeric(df_char$Volume)
    TempPEEK_<- as.numeric(df_char$Temperature)
    dfPEEK_c<-cbind(HeightPEEK_,TimePEEK_,VolPEEK_,TempPEEK_)
    colnames(dfPEEK_c)<-c("HeightPEEK","TimePEEK","VolPEEK","TempPEEK")
    dfPEEK_c <- as.data.frame(dfPEEK_c) %>%
      na.omit()

    dfPEEK$dfPEEK_chg <- dfPEEK_c %>% 
      mutate(TIME_H=TimePEEK/3600, #time (h)
             FLUX=VolPEEK/1000/TIME_H,
             FLUX_KGS=VolPEEK/1000/TimePEEK,#flux (kg/s)
             PRESSURE_PA=1000*9.81*HeightPEEK, #pressure (Pa)
             PRESSURE_MPA=PRESSURE_PA/1000000, #pressure (MPa)
             K=VolPEEK/1000/TimePEEK/PRESSURE_MPA, #hydraulic conductance(kg s-1 MPa-1)
             N=VolPEEK/18*1000, # N (mmol)
             FLUX_MMOLS=N/TimePEEK) # Flux (mmol s-1)

    shinyjs::reset("HeightPEEK")
    shinyjs::reset("TimePEEK")
    shinyjs::reset("VolPEEK")
    shinyjs::reset("TempPEEK")
    
    observeEvent(input$tablePEEK_cell_edit, {
      rv$df[input$tablePEEK_cell_edit$row,input$tablePEEK_cell_edit$col] <<- (DT::coerceValue(input$tablePEEK_cell_edit$value, rv$df[input$tablePEEK_cell_edit$row,input$tablePEEK_cell_edit$col]))
      rv$df %>%
        na.omit()
      df_char<-rv$df
      print("numeric")
      HeightPEEK_<- as.numeric(df_char$Height)
      TimePEEK_<- as.numeric(df_char$Duration)
      VolPEEK_<- as.numeric(df_char$Volume)
      TempPEEK_<- as.numeric(df_char$Temperature)
      dfPEEK_c<-cbind(HeightPEEK_,TimePEEK_,VolPEEK_,TempPEEK_)
      colnames(dfPEEK_c)<-c("HeightPEEK","TimePEEK","VolPEEK","TempPEEK")
      dfPEEK_c <- as.data.frame(dfPEEK_c) %>%
        na.omit()
      
      dfPEEK$dfPEEK_chg <- dfPEEK_c %>% 
        mutate(TIME_H=TimePEEK/3600, #time (h)
               FLUX=VolPEEK/1000/TIME_H,
               FLUX_KGS=VolPEEK/1000/TimePEEK,#flux (kg/s)
               PRESSURE_PA=1000*9.81*HeightPEEK, #pressure (Pa)
               PRESSURE_MPA=PRESSURE_PA/1000000, #pressure (MPa)
               K=VolPEEK/1000/TimePEEK/PRESSURE_MPA, #hydraulic conductance(kg s-1 MPa-1)
               N=VolPEEK/18*1000, # N (mmol)
               FLUX_MMOLS=N/TimePEEK) # Flux (mmol s-1)
      
      write.csv(rv$df, paste0("OUTPUTS/PEEK/measurement/",input$IDMEASUREMENT1,"_PEEKmeasurement.csv"), row.names=FALSE)
      
    })

    
    output$txtOutputp1<- renderText({
      paste0("PEEK tubing resistance at 25°C (MPammol<sup>-1</sup> s<sup>-1</sup>), R_25=",dfresults$R_25)
    })
    output$txtOutputp2<- renderText({
      paste0("PEEK tubing conductivity at 25°C (kg s<sup>-1</sup> MPa<sup>-1</sup>), K_25=",dfresults$K_25)
    })
    
    output$hist2P<-renderPlot({
      # Calcul de la pente de la relation linéaire de l'évolution du flux en mmol.s-1 avec la pression
      model <- lm(FLUX_MMOLS~PRESSURE_MPA, dfPEEK$dfPEEK_chg)
      slopeP <- model$coefficients[2]
      print(dfPEEK$dfPEEK_chg)
      # Calcul de la résistance 
      Temperature <- mean(dfPEEK$dfPEEK_chg$TempPEEK, na.rm = TRUE)
      print(Temperature)
      R=1/slopeP
      R_25=0.88862/(10^((1.3272*(20-Temperature)-0.001053*(20-Temperature)^2)/(Temperature+105)))*R
      dfresults$R_25=R_25[[1]]
      
      # Calcul de la conductance hydraulique du PEEK tubing 
      Temperature <- mean(dfPEEK$dfPEEK_chg$TempPEEK, na.rm = TRUE)
      model.2 <- lm(FLUX_KGS~PRESSURE_MPA, dfPEEK$dfPEEK_chg)
      pente.2 <- model.2$coefficients[2]
      summary(model.2)$adj.r.squared
      
      K_25=pente.2/(0.88862*(1/10^((1.3272*(20-Temperature)-0.001053*(20-Temperature)^2)/(Temperature+105))))
      dfresults$K_25=K_25[[1]]
      
      annotations2 <- data.frame(
        xpos = c(-Inf),
        ypos =  c(Inf),
        #annotateText = c(paste0("R2=",round(summary(model.2)$adj.r.squared,4),"\nK_25=",round(K_25,6))),
        annotateText = c(paste0("R2=",round(summary(model.2)$adj.r.squared,4))),
        hjustvar = c(0) ,
        vjustvar = c(1)) 
      
      color <- "red2"
      if (!is.na(summary(model.2)$adj.r.squared) & summary(model.2)$adj.r.squared>=0.99) {
        color="darkgreen"
      }  

      ggplot(dfPEEK$dfPEEK_chg, aes(PRESSURE_MPA, FLUX_KGS)) + geom_point() +  geom_smooth(method=lm) + geom_text(data=annotations2,color=color, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))+
        xlab("Pressure (MPa)") + ylab(expression(paste("Flow (mmol ", s^{-1}, ")"))) + theme(text = element_text(size = 15))
      
    }) #renderPlot
    write.csv(rv$df, paste0("OUTPUTS/PEEK/measurement/",input$IDMEASUREMENT1,"_PEEKmeasurement.csv"), row.names=FALSE)
    

  }) #observeEventAdd
  
  output$tablePEEK<-DT::renderDT({
    if (!is.null(dim(rv$df))) {
      if (dim(rv$df)[1]>=1) {
        essai<-rv$df %>%
          dplyr::filter(Height  !='' & Duration !='' & Volume  !='' & Temperature !='') 

        DT::datatable(essai, 
                      colnames = c('Height (m)', 'Duration', 'Volume (mL)', 'Temperature (°C)'),
                      options = list(dom = 't'),
                      editable = TRUE,
                      escape   = FALSE,
                      caption = htmltools::tags$caption("Data overview", style="color:blue")) 
      }}
  }) #tablePEEK

  print("add2")  
  observeEvent(input$Add2, {
    cat("--Validate4 \n")
    validate(
      need(input$PARAMETER1 != "", 'Please choose a state.'),
      need(input$OPERATOR != "", 'Please choose a state.'),
      need(input$PEEK_tubing_ID != "", 'Please choose a state.'),
      need(input$DEVICE != "", 'Please choose a state.')    
    )

    rv4$df4 <- rbind(rv4$df4, data.frame(
      PARAMETER1 = input$PARAMETER1,
      OPERATOR = input$OPERATOR,
      PEEK_tubing_ID = input$PEEK_tubing_ID,
      DEVICE = input$DEVICE,
      K_25 =  dfresults$K_25,
      R_25 = dfresults$R_25
    ))
    
  })

  output$tablePEEKR<-renderText({paste0("R_25=",dfresults$R_25)}) #tablePEEKR
  output$tablePEEKK<-renderText({paste0("K_25=",dfresults$K_25)}) #tablePEEKK
  output$tablePEEKid<-DT::renderDT({
    DT::datatable(rv4$df4, 
                  colnames = c('Parameter 1', 'Operator', 'PEEK tubing ID', 'Device', 'K_25', 'R_25'),
                  options = list(dom = 't'),
                  editable = FALSE,
                  escape   = FALSE,
                  caption = htmltools::tags$caption("Data overview", style="color:blue")
    ) 
  }) #tablePEEKid
  
  
  ####### DOWNLOAD BUTTON downloadDataT1
  observeEvent(input$generateButton, {
    write.csv(rv4$df4, paste0("OUTPUTS/PEEK/id/",input$nomid,"_PEEKid.csv"), row.names=FALSE)
  })
  
  observeEvent(input$Add5, {
      data_all1 <-NULL
      data_all2 <-NULL
      folderPC5id_<-substr(as.character(file_input())[1],47,1000000L)
      x2<-unlist(gregexpr(pattern ='_PEEKid.csv',folderPC5id_))
      namefile5id=substr(folderPC5id_,1,x2[1]-1)
      
      folderPC5_<-substr(as.character(file_input1())[1],38,1000000L)
      x2<-unlist(gregexpr(pattern ='_PEEK.csv',folderPC5_))
      namefile5=substr(folderPC5_,1,x2[1]-1)
      
      if (file.exists(paste0("OUTPUTS/PEEK/id/",namefile5id,"_PEEKid.csv"))) {
        data_all1 <- read.csv(paste0("OUTPUTS/PEEK/id/",namefile5id,"_PEEKid.csv"))
      }
      data_all1$K_25_<- as.numeric(data_all1$K_25)
      data_all1$R_25_<- as.numeric(data_all1$R_25)
      data_all1$PEEK_tubing_ID<-data_all1$PEEK_tubing_ID
      
      if (!is.null(dim(data_all1))) {
        data_mean1 <- data_all1 %>% 
          group_by(PEEK_tubing_ID) %>% 
          summarise(R_25_mean=mean(R_25_), R_25_sd=sd(R_25_), n=n(), R_25_se=R_25_sd/sqrt(n),
                    K_25_mean=mean(K_25_), K_25_sd=sd(K_25_), K_25_se=K_25_sd/sqrt(n))
        
        if (file.exists(paste0("www/color/",namefile5,"_PEEK.csv"))) {
          data_all2 <- read.csv(paste0("www/color/",namefile5,"_PEEK.csv"))
        }
        data_mean <- as.data.frame(rbind(data_mean1,data_all2))
      }
      output$tablePEEKtub<-DT::renderDT({
        DT::datatable(data_mean, 
                      options = list(dom = 't'),
                      rownames= FALSE,
                      caption = htmltools::tags$caption("Data overview", style="color:blue")) 
      }) #tablePEEKtub      
    
    observeEvent(input$generateButton5, {
      write.csv(data_mean, paste0("www/color/",input$nomid5,"_PEEK.csv"), row.names=FALSE)
    })      
  }) #add5

  #resultats SingleKmeasurement
  observeEvent(input$resetAllPEEK, {
    shinyjs::reset("myappm1")
    output$rawInputValue42 <- DT::renderDataTable({})
    output$rawInputValue41 <- DT::renderDataTable({})
    rv$df<-NULL
    dfPEEK$dfPEEK_chg<-NULL
    dfresults$R_25<-NULL
    dfresults$K_25<-NULL
    rv4$df4<-NULL
    output$hist1P <- renderPlot({})
    output$hist2P <- renderPlot({})
  
  }) 


############################################################
############################################################
#STEP 2 Pressure sensor calibration
#Initial Dataframe 
Testdata =data.frame("Height" = integer(),	"ELTime" = integer(),	"Step"=integer(),	
 "P1.psi"=integer(),	"P2.psi"=integer(), "T3_oC"=numeric())
data_mean =data.frame("Height" = integer(),	"n" = integer(),		
 "P1_mean"=numeric(),	"P1_sd"=numeric(), "P1_CV"=numeric(),
 "P2_mean"=numeric(),	"P2_sd"=numeric(), "P2_CV"=numeric(),	
 "Bar"=numeric())
slope=data.frame("Variable"=character(),"P1"=numeric(),"P2"=numeric())

observeEvent(input$recalc,{
Height <- extract(input$breaks)
Step <- 1:length(Height)
iter <- cbind(Step,Height)

if (input$pasted != '') {
df_table <- fread(paste(input$pasted, collapse = "\n"))
df_table <-as.data.frame(df_table)
colnames(df_table) <- c("ELTime",	"Step",	"P1.psi",	"P2.psi",	"T3_oC")
df_table$Step<-df_table$Step-df_table$Step[1]+1
df_table<-merge(iter, df_table)
}

Testdata <- df_table
col_order <- c("Height", "ELTime", "Step","P1.psi", "P2.psi","T3_oC")
Testdata <- Testdata[, col_order]

print("namefile")
namefile<-paste0("d_",input$date,"_",strftime(input$time, "%T"),"_",input$operator,"_",input$device)
namefile <- str_replace_all(namefile, c("-"="",":"="", "é"= "e", "à"="a", "è"= "e", "ô" = "o", "ç"="c", "É"="E", "È"="E", "Î"="i", "Ç"="C"))
cat(namefile, "\n")

data_mean <- Testdata %>% 
group_by(Height) %>% 
summarise(n=n(), P1_mean=mean(P1.psi), P1_sd=sd(P1.psi), P1_CV=P1_sd/P1_mean,
P2_mean=mean(P2.psi), P2_sd=sd(P2.psi), P2_CV=P2_sd/P2_mean)
data_mean$Bar=data_mean$Height/1000 

print("lm")
model1 <- lm(Bar~P1_mean, data_mean)
model2 <- lm(Bar~P2_mean, data_mean)
intercept <- cbind("Intercept",model1$coefficients[[1]],model2$coefficients[[1]])
slope <- cbind("Slope",model1$coefficients[[2]],model2$coefficients[[2]])
adj.r.squared <- cbind("adj.r.squared",summary(model1)$adj.r.squared,summary(model2)$adj.r.squared)
slope2 <- rbind(adj.r.squared,intercept,slope)
colnames(slope2)<-c("Variable","P1","P2")
print(slope2)

output$arduino <- DT::renderDataTable({
write.csv(Testdata, paste0("OUTPUTS/CALIBRATION/", namefile,"_arduino.csv"),row.names=FALSE)
DT::datatable(Testdata, 
options = list(searching = FALSE,lengthChange = FALSE),
caption = htmltools::tags$caption("Overview of Arduino data", style="color:blue")) %>%
formatRound(columns = c(4:6), digits = 4)
})

output$table <- DT::renderDataTable({
write.csv(data_mean, paste0("OUTPUTS/CALIBRATION/",namefile,"_mean.csv"),row.names=FALSE)
DT::datatable(subset(data_mean, select = -c(Bar) ), colnames = c('Step', 'Heigh (cm)', 'n', 'P1 Average', 'P1 StdDev', 'P1 CV(<0.05)', 'P2 Average', 'P2 StdDev', 'P2 CV(<0.05)'), 
caption = htmltools::tags$caption("Overview of average values", style="color:blue"),
options = list(dom = 't')) %>% 
formatRound(columns = c(3:8), digits = 2) %>% 
formatStyle('P1_CV',
backgroundColor = styleInterval(c(0.05), c('#B7EFA1', '#EFA1A1'))) %>% 
formatStyle('P2_CV',
backgroundColor = styleInterval(c(0.05), c('#B7EFA1', '#EFA1A1')))
})

slope2[1,1] <- "adj.r.squared (R2 > 0.999)"
write.csv(slope2, paste0("OUTPUTS/CALIBRATION/",namefile,"_coeff.csv"),row.names=FALSE)

aa <- as.data.frame(adj.r.squared)

output$slope1 <- DT::renderDataTable({
DT::datatable((aa) ,
options = list(dom = 't',
 headerCallback = JS(
"function(thead, data, start, end, display){",
"$(thead).remove();",
"}")),
rownames= FALSE
 ) %>% 
formatStyle('V2', 
backgroundColor = styleInterval(c(0.999), c('#EFA1A1','#B7EFA1'))) %>% 
formatStyle('V3',
backgroundColor = styleInterval(c(0.999), c('#EFA1A1','#B7EFA1')))
})
output$slope2 <- DT::renderDataTable({
DT::datatable(slope2[2:3,],
caption = htmltools::tags$caption("Regression between pressure (bar) associated with water column height and pressure (psi) measured by P1 and P2 sensors", style="color:blue"),
options = list(dom = 't')) })

output$hist1 <- renderPlot({
ggplot(data_mean, aes(P1_mean, Bar)) + geom_point() +
geom_smooth(method=lm) +
ggtitle("P1") +
xlab("Pressure (psi) measured by pressure sensor") + ylab("Pressure (bar) associated with water column height")
}) 
output$hist2 <- renderPlot({
ggplot(data_mean, aes(P2_mean, Bar)) + geom_point() +
geom_smooth(method=lm)+
ggtitle("P2") +
xlab("Pressure (psi) measured by pressure sensor") + ylab("Pressure (bar) associated with water column height")
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



############################################################
############################################################
#Step 2i Pressure sensor calibration(reading)
observe({
shinyFileChoose(input, 'folder', roots=c(wd='.'), defaultPath="OUTPUTS/CALIBRATION", pattern="_arduino", filetypes=c('csv'))
}) 

observeEvent(input$recalcL, {
folder<-substr(as.character(input$folder)[1],48,1000000L)
x<-unlist(gregexpr(pattern ='_arduino.csv',folder))
namefile=paste0(substr(folder,1,x[1]-1))
print("Name of imported calibration file")
print(namefile)

Testdata<-read.csv(paste0("OUTPUTS/CALIBRATION/", namefile,"_arduino.csv"))
data_mean<-read.csv(paste0("OUTPUTS/CALIBRATION/",namefile,"_mean.csv"))
model1 <- lm(Bar~P1_mean, data_mean)
model2 <- lm(Bar~P2_mean, data_mean)

intercept <- cbind("Intercept",model1$coefficients[[1]],model2$coefficients[[1]])
slope <- cbind("Slope",model1$coefficients[[2]],model2$coefficients[[2]])
adj.r.squared <- cbind("adj.r.squared",summary(model1)$adj.r.squared,summary(model2)$adj.r.squared)
slope2 <- rbind(adj.r.squared,intercept,slope)
colnames(slope2)<-c("Variable","P1","P2")
print(slope2)
slope2[1,1] <- "adj.r.squared (R2 > 0.999)"
aa <- as.data.frame(adj.r.squared)
print((aa))

output$arduinoL <- DT::renderDataTable({
DT::datatable(subset(Testdata ), rownames = FALSE, 
colnames = c("Height (cm)",	"ELTime (s)",	"Step", "P1(psi)", "P2(psi)",	"T3(oC)"),
options = list(searching = FALSE,lengthChange = FALSE),
caption = htmltools::tags$caption("Overview of Arduino data", style="color:blue")) %>% 
formatRound(columns = c(4:6), digits = 4)
})
 
output$tableL <- DT::renderDataTable({
DT::datatable(subset(data_mean, select = -c(Bar) ), colnames = c('Step', 'Height (cm)', 'n', 'P1 Average', 'P1 StdDev', 'P1 CV(<0.05)', 'P2 Average', 'P2 StdDev', 'P2 CV(<0.05)'), 
caption = htmltools::tags$caption("Overview of average values", style="color:blue"),
options = list(dom = 't')) %>% 
formatRound(columns = c(3:8), digits = 2) %>% 
formatStyle('P1_CV',
backgroundColor = styleInterval(c(0.05), c('#B7EFA1', '#EFA1A1'))) %>% 
formatStyle('P2_CV',
backgroundColor = styleInterval(c(0.05), c('#B7EFA1', '#EFA1A1')))
})

output$slope1L <- DT::renderDataTable({
DT::datatable((aa) ,
options = list(dom = 't',
 headerCallback = JS(
 "function(thead, data, start, end, display){",
 "$(thead).remove();",
 "}")),
rownames= FALSE
) %>% 
formatStyle('V2', 
backgroundColor = styleInterval(c(0.999), c('#EFA1A1','#B7EFA1'))) %>% 
formatStyle('V3',
backgroundColor = styleInterval(c(0.999), c('#EFA1A1','#B7EFA1')))
})
output$slope2L <- DT::renderDataTable({
DT::datatable(slope2[2:3,],
caption = htmltools::tags$caption("Regression between pressure (bar) associated with water column height and pressure (psi) measured by P1 and P2 sensors", style="color:blue"),
options = list(dom = 't')) })

output$hist1L<- renderPlot({
ggplot(data_mean, aes(P1_mean, Bar)) + geom_point() +
geom_smooth(method=lm) + 
ggtitle("P1") +
xlab("Pressure (psi) measured by pressure sensor") + ylab("Pressure (bar) associated with water column height")
}) 
output$hist2L <- renderPlot({
ggplot(data_mean, aes(P2_mean, Bar)) + geom_point() +
geom_smooth(method=lm) +
ggtitle("P2") +
xlab("Pressure (psi) measured by pressure sensor") + ylab("Pressure (bar) associated with water column height")
})

})

############################################################
############################################################
#STEP 3 Measurement 

#choice of inputs
observe({
print("CALIBRATION")
shinyFileChoose(input, 'folderPSm', roots=c(wd='.'), defaultPath="OUTPUTS/CALIBRATION", pattern="_coeff", filetypes=c('csv'))
name_file=substr(as.character(input$folderPSm)[1],48,1000000L)
name_file=substr(name_file, 1, gregexpr(pattern ='.csv\"))',name_file)[[1]][1])
print(name_file)
output$rawInputValue2 <- renderText({name_file})
})

observe({
print("CALIBRATION PEEK")
shinyFileChoose(input, 'folderPTm', roots=c(wd='.'), defaultPath="www/color", pattern="_PEEK", filetypes=c('csv'))
folderPTm<-substr(as.character(input$folderPTm)[1],38,1000000L)
print(folderPTm)
x2<-unlist(gregexpr(pattern ='_PEEK.csv',folderPTm))
namefile2=substr(folderPTm,1,x2[1]-1)
print(namefile2)#### 

if (file.exists(paste0("www/color/",namefile2,"_PEEK.csv"))) {
output$rawInputValue1 <- renderText(paste0(namefile2,"_PEEK.csv"))
PEEK_coeff <- read.csv(paste0("www/color/",namefile2,"_PEEK.csv"))
choiceList <- PEEK_coeff[, 1]
updateSelectInput(session, "PEEK_sel", label = "PEEK tubing ID", choices = choiceList, selected = '')
}

parm1_choice <- read.csv(paste0("www/parm1_choice.csv"),header = FALSE)
choiceListparm1 <- parm1_choice[, 1]
updateSelectInput(session, "parm1", label = "Parameter 1:", choices = choiceListparm1, selected = '')
parm2_choice <- read.csv(paste0("www/parm2_choice.csv"),header = FALSE)
choiceListparm2 <- parm2_choice[, 1]
updateSelectInput(session, "parm2", label = "Parameter 2:", choices = choiceListparm2, selected = '')
parm3_choice <- read.csv(paste0("www/parm3_choice.csv"),header = FALSE)
choiceListparm3 <- parm3_choice[, 1]
updateSelectInput(session, "parm3", label = "Parameter 3:", choices = choiceListparm3, selected = '')
parm4_choice <- read.csv(paste0("www/parm4_choice.csv"),header = FALSE)
choiceListparm4 <- parm4_choice[, 1]
updateSelectInput(session, "parm4", label = "Parameter 4:", choices = choiceListparm4, selected = '')
})

df_tablem =data.frame("ELTime",	"Step",	"P1.psi",	"P2.psi", "T3_oC",	"T1_oC",	"T2_oC")
data_meanm =data.frame("Step", "n", "P1_mean", "P1_sd", "P1_CV","P2_mean","P2_sd","P2_CV", "P2_mean", "P2_sd", "P2_CV",
 "P1bar_mean", "P1bar_sd", "P1bar_CV","P2bar_mean", "P2bar_sd", "P2bar_CV",
 "Diffbar_mean", "Diffbar_sd", "Diffbar_CV",
 "k_rough_mean","k_rough_sd","k_rough_CV",
 "ValidPEEKChoice",
 "p1_300_mean","p1_300_sd","p1_300_CV",
 "p2_300_mean","p2_300_sd","p2_300_CV",
 "ValidPEEKChoice2",
 "k_300_rough_mean","k_300_rough_sd","k_300_rough_CV",
 "p3_min","t1_mean","t2_mean","t3_mean","ELTime2_max"
)

observeEvent(input$recalcm,{
cat("--Validate \n")
 validate(
 need(input$PEEK_sel != 'No choices yet', 'Please choose a state.'),
 need(input$parm1 != "No choices yet", 'Please choose a state.'),
 need(input$parm2 != "No choices yet", 'Please choose a state.'),
 need(input$parm3 != "No choices yet", 'Please choose a state.'),
 need(input$parm4 != "No choices yet", 'Please choose a state.'),
 need(input$parm5 != "", 'Please choose a state.'),
 need(input$parm6 != "", 'Please choose a state.'),
 need(input$parm8 != "", 'Please choose a state.'), 
 need(input$pastedm != "", 'Please choose a state.'),
 need(input$T1_oC != "", 'Please choose a state.'),
 )
 
print("start")
dir.create(paste0("OUTPUTS/SINGLE/",input$parm1))
dir.create(paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm8))
 
if (input$pastedm != '') {
 df_tablem <- fread(paste(input$pastedm, collapse = "\n"))
 df_tablem <-as.data.frame(df_tablem)
 print(head(df_tablem)) 
colnames(df_tablem) <- c("ELTime",	"Step",	"P1.psi",	"P2.psi",	"T3_oC")
df_tablem <- subset(df_tablem, P1.psi!="Inf")
df_tablem <- subset(df_tablem, P2.psi!="Inf")
}

INPUT <- data.frame (first_column= c("parm1","parm2","parm3","parm4","parm5","parm6","parm7","folder Preasure Sersors Calibration","folder PEEK tubing Calibration","color","Stemdiameter1","Stemdiameter2","Stemlength","Comments","parm8","T1_oC","T2_oC"),
 second_column= c(input$parm1,input$parm2,input$parm3,input$parm4,input$parm5,input$parm6,as.character(zoo::as.Date(as.numeric(input$parm7))),substr(as.character(input$folderPSm)[1],48,1000000L),substr(as.character(input$folderPTm)[1],38,1000000L),input$PEEK_sel,input$Stemdiameter1,input$Stemdiameter2,input$Stemlength,input$Comments,input$parm8,input$T1_oC,input$T2_oC))
write.csv(INPUT, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm8,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_",input$parm7,"_INPUT.csv"),row.names=FALSE)

folderPSm<-substr(as.character(input$folderPSm)[1],48,1000000L)
x<-unlist(gregexpr(pattern ='_coeff.csv',folderPSm))
namefile=paste0(substr(folderPSm,1,x[1]-1))
print("Name of imported calibration file")
print(namefile)
data_coeff<-read.csv(paste0("OUTPUTS/CALIBRATION/",namefile,"_COEFF.csv"))
colnames(data_coeff)<-c("Variable","P1 calibration","P2 calibration")
write.csv(data_coeff, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm8,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_",input$parm7,"_COEFF.csv"),row.names=FALSE)

folderPTm<-substr(as.character(input$folderPTm)[1],38,1000000L)
x2<-unlist(gregexpr(pattern ='_PEEK.csv',folderPTm))
namefile2=paste0(substr(folderPTm,1,x2[1]-1))
print(namefile2)
PEEK_coeff <- read.csv(paste0("www/color/",namefile2,"_PEEK.csv"))
Res_PEEK<-base::subset(PEEK_coeff,PEEK_tubing_ID==input$PEEK_sel)[[2]]
PEEK <- data.frame (first_column= c("PEEK tubing ID (from PEEK tubing calibration Tab)", 
"Resistance of chosen PEEK tubing at 25 oC (MPa mmol-1 s)"),
second_column = c(input$PEEK_sel, Res_PEEK)
)
write.csv(PEEK, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm8,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_",input$parm7,"_PEEK.csv"),row.names=FALSE)

z<-df_tablem
z$t1_meanx=as.numeric(input$T1_oC)
z$t2_meanx=as.numeric(input$T2_oC)

if (dim(z)[1]==1) {
return()
}

#calculs
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
zdf1 <- z

data_meanm <- z %>% 
group_by(Step) %>% 
summarise(n=n(), P1_mean=mean(P1.psi), P1_sd=sd(P1.psi), P1_CV=P1_sd/P1_mean,
P2_mean=mean(P2.psi), P2_sd=sd(P2.psi), P2_CV=P2_sd/P2_mean,
P1bar_mean=mean(P1bar), P1bar_sd=sd(P1bar), P1bar_CV=P1bar_sd/P1bar_mean,
P2bar_mean=mean(P2bar), P2bar_sd=sd(P2bar), P2bar_CV=P2bar_sd/P2bar_mean,
Diffbar_mean=mean(Diffbar), Diffbar_sd=sd(Diffbar), Diffbar_CV=Diffbar_sd/Diffbar_mean,
k_rough_mean=mean(k_rough),k_rough_sd=sd(k_rough), k_rough_CV=k_rough_sd/k_rough_mean,
ValidPEEKChoice=P2bar_mean/P1bar_mean,
p1_300_mean=mean(P1bar*condition_1,na.rm=TRUE),p1_300_sd=sd(P1bar*condition_1,na.rm=TRUE), p1_300_CV=p1_300_sd/p1_300_mean,
p2_300_mean=mean(P2bar*condition_1,na.rm=TRUE),p2_300_sd=sd(P2bar*condition_1,na.rm=TRUE), p2_300_CV=p2_300_sd/p2_300_mean,
ValidPEEKChoice2=p2_300_mean/p1_300_mean,
k_300_rough_mean=mean(k_300_rough,na.rm=TRUE),k_300_rough_sd=sd(k_300_rough,na.rm=TRUE), k_300_rough_CV=k_300_rough_sd/k_300_rough_mean,
p3_min=min(P2bar),
t1_mean=mean(t1_meanx),t2_mean=mean(t2_meanx),t3_mean=mean(T3_oC),
ELTime2_max=max(ELTime2*condition_1,na.rm=TRUE)
)
write.csv(df_tablem, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm8,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_",input$parm7,"_ARDUINO.csv"),row.names=FALSE)
print("data_meanm") 

NOMADD<-data.frame (first_column= c("parm1","parm2","parm3","parm4","parm5","parm6","parm7","T1_oC","T2_oC"),
second_column = c(input$parm1,input$parm2,input$parm3,input$parm4,input$parm5,input$parm6,as.character(zoo::as.Date(as.numeric(input$parm7))),input$T1_oC,input$T2_oC),
third_column = c("","","","","","","","",""),
fourth_column = c("","","","","","","","","")
)
print("NOMADD") 

output$arduinom <- DT::renderDataTable({
DT::datatable(df_tablem, colnames = c("ELTime (s)",	"Step",	"P1(psi)", "P2(psi)",	#"T1(oC)",	"T2(oC)",	
"T3(oC)" ),
caption = htmltools::tags$caption("data Arduino", style="color:blue"),
options = list(searching = FALSE,lengthChange = FALSE),
editable = TRUE ) %>% 
formatRound(columns = c(5:5), digits = 2) 
})

output$tablem <- DT::renderDataTable({
DT::datatable(subset(data_meanm, select = c(Step,n,P1_mean,P2_mean,Diffbar_mean,Diffbar_sd,Diffbar_CV) ), 
caption = htmltools::tags$caption("Overview of average values", style="color:blue"),
options = list(dom = "ft",ordering=F,
 pageLength = 10000,
 searching = FALSE),rownames= FALSE) 
})

output$table1m <- DT::renderDataTable({
DT::datatable(
data_coeff,
caption = htmltools::tags$caption(
style = 'text-align: left; color:blue',
'Conversion in bar	(From Pressure Sensor Calibration Tab)'
),

options = list(dom = "ft",ordering=F,
 pageLength = 10000,
 searching = FALSE), rownames= FALSE) 
})

output$table2m <- DT::renderDataTable({
datatable(
PEEK,
colnames = rep("", ncol(PEEK)),
caption = htmltools::tags$caption(
style = 'text-align: left; color:blue',
'Constant for PEEK tubing'
),
options = list(dom = "ft",ordering=F,
 pageLength = 10000,
 searching = FALSE),rownames= FALSE)
}) 

result1 <- data.frame (first_column= c("","","Step 1","","","","Step 2","","","","","","Step 3","","","","","","","","","",""),
 second_column = c("",
 "PEEK tubing range",
 "Validate pressure sensors output (±?) and recalibrate if needed",
 "Presure sensor validation P1 (bar)",
 "Presure sensor validation P2 (bar)",
 "Average",
 "Readings of K« rough » measurements (wait for 5 min of stability)",
 "P1 (bar)",
 "P2 (bar)",
 "K rough (mmol s-1 MPa-1)",
 "K rough (kg s-1 Mpa-1)",
 "CV of K (%)",
 "",
 "P3 (bar)",
 "T1: Temperature of the solution before measurement (°C)",
 "T2: Temperature of the solution after measurement (°C)",
 "Average temperature during measurement (°C)",
 "Resistance of PEEK tubing at actual temp (MPa mmol-1 s)",
 "","T3: Temperature of water bassin with sample (°C)",
 "","K at measurement temperature and corrected for P3 (kg s-1 Mpa-1)",
 "K at 25 oC and corrected for P3 (kg s-1 Mpa-1)"
 ),
 third_column = c("Total",
data_meanm$ValidPEEKChoice[2],
"",
data_meanm$P1bar_mean[1],
data_meanm$P2bar_mean[1],
(data_meanm$P1bar_mean[1]+data_meanm$P2bar_mean[1])/2,
"",
data_meanm$P1bar_mean[2],
data_meanm$P2bar_mean[2],
((data_meanm$P1bar_mean[2]-data_meanm$P2bar_mean[2])/Res_PEEK)/data_meanm$P2bar_mean[2],
((data_meanm$P1bar_mean[2]-data_meanm$P2bar_mean[2])/Res_PEEK)/data_meanm$P2bar_mean[2]*18/1000000,
data_meanm$k_rough_CV[2],
"",
data_meanm$p3_min[3],
data_meanm$t1_mean[3],
data_meanm$t2_mean[3],(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2,
(1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)),"",
data_meanm$t3_mean[3],"",
(data_meanm$P1bar_mean[2]-data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)))/(data_meanm$P2bar_mean[2]-data_meanm$p3_min[3])*18/1000000,
((data_meanm$P1bar_mean[2]-data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)))/(data_meanm$P2bar_mean[2]-data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-data_meanm$t3_mean[3])-0.001053*(data_meanm$t3_mean[3]-20)^2)/(data_meanm$t3_mean[3]+105))))),
 
 fourth_column = c("Last 300 s",
 data_meanm$ValidPEEKChoice2[2],"","","","","",
 data_meanm$p1_300_mean[2],data_meanm$p2_300_mean[2],
 ((data_meanm$p1_300_mean[2]-data_meanm$p2_300_mean[2])/Res_PEEK)/data_meanm$p2_300_mean[2],
 ((data_meanm$p1_300_mean[2]-data_meanm$p2_300_mean[2])/Res_PEEK)/data_meanm$p2_300_mean[2]*18/1000000,
 data_meanm$k_300_rough_CV[2],
 "",
 data_meanm$p3_min[3],
 data_meanm$t1_mean[3],
 data_meanm$t2_mean[3],
 (data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2,
 (1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)),"",
 data_meanm$t3_mean[3],
 "",
 (data_meanm$p1_300_mean[2]-data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)))/(data_meanm$p2_300_mean[2]-data_meanm$p3_min[3])*18/1000000,
 ((data_meanm$p1_300_mean[2]-data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)))/(data_meanm$p2_300_mean[2]-data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-data_meanm$t3_mean[3])-0.001053*(data_meanm$t3_mean[3]-20)^2)/(data_meanm$t3_mean[3]+105)))))
)
write.csv(result1, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm8,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_",input$parm7,"_STEP.csv"),row.names=FALSE)


output$table3m <- DT::renderDataTable({
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


Pithdiameter=0
result <- data.frame (first_column= c("Overview", "","","Step 1","Step 2","",""),
second_column= c(
"Global Results",
"Conductivity [K] (kg s-1 MPa m)",
"Stem area-specific conductivity [Ks] (kg s-1 MPa m-1)",
"Pressure sensors validation (1/1000 maximum variation)",
"Validation for optimal PEEK tubing choice",
"Time (min 300 sec.)",
"Stability (CV < 0.05)"),
third_column= c("Total", 
((data_meanm$P1bar_mean[2]-data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)))/(data_meanm$P2bar_mean[2]-data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-data_meanm$t3_mean[3])-0.001053*(data_meanm$t3_mean[3]-20)^2)/(data_meanm$t3_mean[3]+105))))*(input$Stemlength/1000),
((data_meanm$P1bar_mean[2]-data_meanm$P2bar_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)))/(data_meanm$P2bar_mean[2]-data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-data_meanm$t3_mean[3])-0.001053*(data_meanm$t3_mean[3]-20)^2)/(data_meanm$t3_mean[3]+105))))*(input$Stemlength/1000)/(((((input$Stemdiameter1+input$Stemdiameter2)/2/1000)/2)^2*pi-((Pithdiameter/1000)/2)^2*pi)), 
base::ifelse((abs(data_meanm$P1bar_mean[1]-data_meanm$P2bar_mean[1])<0.001), "OK", "RECALIBRATE"),
base::ifelse((data_meanm$ValidPEEKChoice[2]>0.2 & data_meanm$ValidPEEKChoice[2]<0.8), "OK", "?"),
"",
""),
fourth_column= c("Last 300 s", 
 ((data_meanm$p1_300_mean[2]-data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)))/(data_meanm$p2_300_mean[2]-data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-data_meanm$t3_mean[3])-0.001053*(data_meanm$t3_mean[3]-20)^2)/(data_meanm$t3_mean[3]+105))))*(input$Stemlength/1000),
 ((data_meanm$p1_300_mean[2]-data_meanm$p2_300_mean[2])/((1.002/0.8904)*Res_PEEK*10^((1.3272*(20-(data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2)-0.001053*((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2-20)^2)/((data_meanm$t1_mean[3]+data_meanm$t2_mean[3])/2+105)))/(data_meanm$p2_300_mean[2]-data_meanm$p3_min[3])*18/1000000)/(0.88862*(1/10^((1.3272*(20-data_meanm$t3_mean[3])-0.001053*(data_meanm$t3_mean[3]-20)^2)/(data_meanm$t3_mean[3]+105))))*(input$Stemlength/1000)/(((((input$Stemdiameter1+input$Stemdiameter2)/2/1000)/2)^2*pi-((Pithdiameter/1000)/2)^2*pi)), 
 base::ifelse((abs(data_meanm$P1bar_mean[1]-data_meanm$P2bar_mean[1])<0.001), "OK", "RECALIBRATE"),
 base::ifelse((data_meanm$ValidPEEKChoice2[2]>0.2 & data_meanm$ValidPEEKChoice2[2]<0.8), "OK", "?"),
 base::ifelse(data_meanm$ELTime2_max[2]<294,"WAIT","OK"),
 base::ifelse(data_meanm$k_300_rough_CV[2]<0.05,"OK","WAIT"))
)
write.csv(result, paste0("OUTPUTS/SINGLE/",input$parm1,"/",input$parm8,"/",input$parm1,"_",input$parm2,"_",input$parm3,"_",input$parm4,"_",input$parm5,"_",input$parm6,"_",input$parm7,"_RESULTS.csv"),row.names=FALSE)

xxx<-result[1:3,] %>%
mutate(row.color1 = case_when(as.numeric(third_column) > 1 ~ "#FFDE59",
as.numeric(third_column) <= 1~ "#6CE2B1",
TRUE ~ "#DE6C6E"),
 row.color2 = case_when(as.numeric(fourth_column) > 1 ~ "#FFDE59",
 as.numeric(fourth_column) <= 1~ "#6CE2B1",
TRUE ~ "#DE6C6E")) 
xxx[1,5]<-NA
xxx[1,6]<-NA

output$table4m1 <- DT::renderDataTable({
Pithdiameter=0
datatable(
xxx,
caption = htmltools::tags$caption(
style = 'text-align: left; color:blue',
'Overview of results'
),
colnames = rep("", ncol(xxx)),
options = list(dom = "ft", ordering=F,
 pageLength = 10000,
 searching = FALSE,
 columnDefs = list(list(targets = c(4,5), visible = F))), 
rownames= FALSE) %>% 
formatStyle('third_column', "row.color1",
backgroundColor = styleEqual(sort(unique(xxx$row.color1)), sort(unique(xxx$row.color1))))%>% 
formatStyle('fourth_column', "row.color2",
backgroundColor = styleEqual(sort(unique(xxx$row.color2)), sort(unique(xxx$row.color2))))
})

output$table4m2 <- DT::renderDataTable({
datatable(
result[4:7,],
caption = htmltools::tags$caption(
style = 'text-align: left; color:blue',
'Validation'
),
colnames = rep("", ncol(result)),
options = list(dom = "ft", ordering=F,
 pageLength = 10000,
 searching = FALSE),rownames= FALSE
) %>% 
formatStyle('third_column',
backgroundColor = styleEqual(c("OK", "RECALIBRATE","?","WAIT",NA), c("#6CE2B1", "#DE6C6E", "#DE6C6E", "#DE6C6E", "#DE6C6E"))) %>% 
formatStyle('fourth_column',
backgroundColor = styleEqual(c("OK", "RECALIBRATE","?","WAIT",NA), c("#6CE2B1", "#DE6C6E", "#DE6C6E", "#DE6C6E", "#DE6C6E"))) 
})

output$scatter <- renderPlot({
z2=subset(zdf1, Step == 2)
ggplot() + geom_point(data = z2, aes(ELTime, P1bar), colour = 'blue') +
geom_point(data = z2, aes(ELTime, P2bar), colour = 'orange') +
ggtitle("View for Log of Step 2") +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "blue"))
}) 

output$scatter_step3<- renderPlot({
z3=subset(zdf1, Step == 3)
CV_P1bar <- sd(z3$P1bar) / mean(z3$P1bar) 
g21<-ggplot() + geom_point(data = z3, aes(ELTime, P1bar), colour = 'blue') +
ggtitle(paste0("View for Log of Step 3 - CV=", round(CV_P1bar,6))) +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "blue")) 
CV_P2bar <- sd(z3$P2bar) / mean(z3$P2bar) 
g22<-ggplot() + geom_point(data = z3, aes(ELTime, P2bar), colour = 'orange') +
ggtitle(paste0("View for Log of Step 3 - CV=", round(CV_P2bar,6))) +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "orange"))
grid.arrange(g21, g22, ncol=2)
})
output$scatter_step3i<- renderPlot({
z3=subset(zdf1, Step == 3)
dernier=tail(z3$ELTime, n = 1)-300
z3=subset(z3, ELTime > dernier)
CV_P1bar <- sd(z3$P1bar) / mean(z3$P1bar) 
g21<-ggplot() + geom_point(data = z3, aes(ELTime, P1bar), colour = 'blue') +
ggtitle(paste0("View for Log of Step 3 - CV=", round(CV_P1bar,6))) +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "blue")) 
CV_P2bar <- sd(z3$P2bar) / mean(z3$P2bar) 
g22<-ggplot() + geom_point(data = z3, aes(ELTime, P2bar), colour = 'orange') +
ggtitle(paste0("View for Log of Step 3 - CV=", round(CV_P2bar,6))) +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "orange"))
grid.arrange(g21, g22, ncol=2)
})

}) #observeEvent

 
#resultats SingleKmeasurement
observeEvent(input$resetAllm, {
shinyjs::reset("myappm")
output$rawInputValue2 <- DT::renderDataTable({})
output$rawInputValue1 <- DT::renderDataTable({})
output$arduinom <- DT::renderDataTable({})
output$table4m1 <- DT::renderDataTable({})
output$table4m2 <- DT::renderDataTable({})
output$table1m <- DT::renderDataTable({})
output$table2m <- DT::renderDataTable({})
output$table3m <- DT::renderDataTable({})
output$scatter <- renderPlot({})
output$scatter_step3 <- renderPlot({})
output$scatter_step3i <- renderPlot({})
}) 
 


############################################################
############################################################
#STEP 3i Measurement(reading)
rvw2 <- reactiveValues(
  intrantw =data.frame("first_column" = character(),	"second_column" = character()
  )) 

observe({
shinyFileChoose(input, 'folder3i', roots=c(wd='.'), defaultPath="OUTPUTS/SINGLE", pattern="_ARDUINO", filetypes=c('csv'))
folder<-substr(as.character(input$folder3i)[1],43,1000000L)
folder<-substr(as.character(input$folder3i)[1],43,1000000L)
x<-unlist(gregexpr(pattern ='_ARDUINO.csv',folder))
namefile=paste0(substr(folder,1,x[1]-1))
a<-base::gsub(',', '', namefile)
a<- base::strsplit(a, '\" \"')
namefile<-a[[1]][3]
if(is.na(namefile)){namefile<-'Select a measurement file'}
output$rawInputValue11 <- renderText({namefile})
}) 


observeEvent(input$recalcL3i, {
folder<-substr(as.character(input$folder3i)[1],43,1000000L)
print("folder")
print(folder)
x<-unlist(gregexpr(pattern ='_ARDUINO.csv',folder))
print("x")
print(x)
namefile=paste0(substr(folder,1,x[1]-1))
print("Name of imported measurement file")
print(namefile)
a<-base::gsub(',', '', namefile)
a<- base::strsplit(a, '\" \"')
print(a)
print(is.na(a[[1]][2]))

if(is.na(a[[1]][2])){
namefile2=paste0("OUTPUTS/SINGLE/")}
if(!is.na(a[[1]][2])){
namefile<-a[[1]][3]
print(namefile)
namefile2=paste0("OUTPUTS/SINGLE/",a[[1]][1],"/",a[[1]][2],"/")}

print(namefile2)
print(paste0(namefile2, namefile,"_ARDUINO.csv"))

arduino<-read.csv(paste0(namefile2, namefile,"_ARDUINO.csv"))
data_coeff<-read.csv(paste0(namefile2,namefile,"_COEFF.csv"))
PEEK<-read.csv(paste0(namefile2,namefile,"_PEEK.csv"))
result1<-read.csv(paste0(namefile2,namefile,"_STEP.csv"))
result<-read.csv(paste0(namefile2, namefile,"_RESULTS.csv"))
INPUT<-read.csv(paste0(namefile2, namefile,"_INPUT.csv"))
Res_PEEK<-PEEK$second_column[2]

rvw2$intrantw <- INPUT

 z<-arduino
 z$t1_meanx=as.numeric(INPUT[16,2])
 z$t2_meanx=as.numeric(INPUT[17,2])
 
 #calculs
 z <- z %>%
 group_by(Step) %>%
 mutate(ELTimemax =max(ELTime))
 z <- z %>% 
 mutate(P1bar=data_coeff$P1[2]+data_coeff$P1[3]*P1.psi,
P2bar=data_coeff$P2[2]+data_coeff$P2[3]*P2.psi,
Diffbar=abs(P1bar-P2bar),
k_rough=((P1bar-P2bar)/as.numeric(PEEK$second_column[2]))/P2bar,
ELTime2=ELTimemax-ELTime,
condition_1 = na_if(1*((ELTimemax-ELTime)<300),0),
k_300_rough=((P1bar*condition_1-P2bar*condition_1)/as.numeric(PEEK$second_column[2]))/P2bar*condition_1,
Pithdiameter=0
 )
 zdf1 <- z
 
 observeEvent(input$INPUTL1_cell_edit, {
   rvw2$intrantw[c(1,2,3,4,5,6,7,14,15),][input$INPUTL1_cell_edit$row,2] <<- (DT::coerceValue(input$INPUTL1_cell_edit$value, rvw2$intrantw[c(1,2,3,4,5,6,7,14,15),][input$INPUTL1_cell_edit$row,2]))
   #write.csv(rvw2$intrantw, paste0(namefile2,namefile,"_INPUT.csv"),  row.names=FALSE)
 })
 
 output$INPUTL1 <-DT::renderDataTable({
  DT::datatable(rvw2$intrantw[c(1,2,3,4,5,6,7,14,15),],
   caption = htmltools::tags$caption( style = 'text-align: left; color:blue',"INPUTs"),
   options = list(dom = "ft",ordering=F, pageLength = 10000, searching = FALSE),
   colnames = rep("", ncol(rvw2$intrantw[c(1,2,3,4,5,6,7,14,15),])),
   rownames= FALSE,
   editable = list(target = "cell", disable = list(columns = c(0))) )
 })

 output$INPUTL2 <-DT::renderDataTable({
   DT::datatable(rvw2$intrantw[c(8,9,10,11,12,13,16,17),],
                 options = list(dom = "ft",ordering=F,pageLength = 10000, searching = FALSE),
                 colnames = rep("", ncol(rvw2$intrantw[c(8,9,10,11,12,13,16,17),])),
                 rownames= FALSE,
                 editable = FALSE) 
 })

output$arduinomL <- DT::renderDataTable({
DT::datatable(arduino, colnames = c("ELTime (s)",	"Step",	"P1(psi)", "P2(psi)",	"T3(oC)" ),
caption = htmltools::tags$caption("Overview of Arduino data", style="color:blue"),
options = list(searching = FALSE,lengthChange = FALSE)) %>% 
formatRound(columns = c(5:5), digits = 2) 
})

output$table1mL <- DT::renderDataTable({
DT::datatable(
data_coeff,
caption = htmltools::tags$caption(
style = 'text-align: left; color:blue',
'Conversion in bar	(From Pressure Sensor calibration Tab)'
),
options = list(dom = "ft",ordering=F,
 pageLength = 10000,
 searching = FALSE), rownames= FALSE) 
})

output$table2mL <- DT::renderDataTable({
datatable(
PEEK,
colnames = rep("", ncol(PEEK)),
caption = htmltools::tags$caption(
style = 'text-align: left; color:blue',
'Constant for PEEK tubing'
),
options = list(dom = "ft",ordering=F,
 pageLength = 10000,
 searching = FALSE),rownames= FALSE)
})

output$table3mL <- DT::renderDataTable({
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

xxx<-result[1:3,] %>%
mutate(row.color1 = case_when(as.numeric(third_column) > 1 ~ "#FFDE59",
as.numeric(third_column) <= 1~ "#6CE2B1",
TRUE ~ "#DE6C6E"),
 row.color2 = case_when(as.numeric(fourth_column) > 1 ~ "#FFDE59",
as.numeric(fourth_column) <= 1~ "#6CE2B1",
TRUE ~ "#DE6C6E")) 
xxx[1,5]<-NA
xxx[1,6]<-NA

output$table4mL1 <- DT::renderDataTable({
Pithdiameter=0
datatable(
xxx,
caption = htmltools::tags$caption(
style = 'text-align: left; color:blue',
'Overview of results'
),
colnames = rep("", ncol(xxx)),
options = list(dom = "ft", ordering=F,
 pageLength = 10000,
 searching = FALSE,
 columnDefs = list(list(targets = c(4,5), visible = F))), 
 rownames= FALSE) %>% 
formatStyle('third_column', "row.color1",
backgroundColor = styleEqual(sort(unique(xxx$row.color1)), sort(unique(xxx$row.color1))))%>% 
formatStyle('fourth_column', "row.color2",
backgroundColor = styleEqual(sort(unique(xxx$row.color2)), sort(unique(xxx$row.color2))))
})
 
output$table4mL2 <- DT::renderDataTable({
datatable(
result[4:7,],
caption = htmltools::tags$caption(
style = 'text-align: left; color:blue',
'Validation'
),
colnames = rep("", ncol(result)),
options = list(dom = "ft", ordering=F,
 pageLength = 10000,
 searching = FALSE),rownames= FALSE) %>% 
formatStyle('third_column',
backgroundColor = styleEqual(c("OK", "RECALIBRATE","?","WAIT",NA), c("#6CE2B1", "#DE6C6E", "#DE6C6E", "#DE6C6E", "#DE6C6E"))) %>% 
formatStyle('fourth_column',
backgroundColor = styleEqual(c("OK", "RECALIBRATE","?","WAIT",NA), c("#6CE2B1", "#DE6C6E", "#DE6C6E", "#DE6C6E", "#DE6C6E"))) 
})

output$scatterL <- renderPlot({
z2=subset(zdf1, Step == 2)
ggplot() + geom_point(data = z2, aes(ELTime, P1bar), colour = 'blue') +
geom_point(data = z2, aes(ELTime, P2bar), colour = 'orange') +
ggtitle("View for Log of Step 2") +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "blue"))
})

output$scatterL_step3<- renderPlot({
z3=subset(zdf1, Step == 3)
CV_P1bar <- sd(z3$P1bar) / mean(z3$P1bar) 
g21<-ggplot() + geom_point(data = z3, aes(ELTime, P1bar), colour = 'blue') +
ggtitle(paste0("View for Log of Step 3 - CV=", round(CV_P1bar,6))) +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "blue"))
CV_P2bar <- sd(z3$P2bar) / mean(z3$P2bar) 
g22<-ggplot() + geom_point(data = z3, aes(ELTime, P2bar), colour = 'orange') +
ggtitle(paste0("View for Log of Step 3 - CV=", round(CV_P2bar,6))) +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "orange"))
grid.arrange(g21, g22, ncol=2)
})

output$scatterL_step3i<- renderPlot({
z3=subset(zdf1, Step == 3)
dernier=tail(z3$ELTime, n = 1)-300
z3=subset(z3, ELTime > dernier)
CV_P1bar <- sd(z3$P1bar) / mean(z3$P1bar) 
g21<-ggplot() + geom_point(data = z3, aes(ELTime, P1bar), colour = 'blue') +
ggtitle(paste0("View for Log of Step 3 - CV=", round(CV_P1bar,6))) +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "blue")) 
CV_P2bar <- sd(z3$P2bar) / mean(z3$P2bar) 
g22<-ggplot() + geom_point(data = z3, aes(ELTime, P2bar), colour = 'orange') +
ggtitle(paste0("View for Log of Step 3 - CV=", round(CV_P2bar,6))) +
xlab("Total Elapsed Time (sec)") + ylab("Pressure (bar)")+
theme(plot.title = element_text(color = "orange"))
grid.arrange(g21, g22, ncol=2)
})

})#observeEvent

#("Rename Files")
observeEvent(input$renamedata, {
  print(getwd())
  folder<-substr(as.character(input$folder3i)[1],43,1000000L)
  print("folder")
  print(folder)
  x<-unlist(gregexpr(pattern ='_ARDUINO.csv',folder))
  namefile=paste0(substr(folder,1,x[1]-1))
  print("Name of imported measurement file")
  print(namefile)
  a<-base::gsub(',', '', namefile)
  a<- base::strsplit(a, '\" \"')

  if(is.na(a[[1]][2])){
    namefile2=paste0("OUTPUTS/SINGLE/")}
  if(!is.na(a[[1]][2])){
    namefile<-a[[1]][3]
    print(namefile)
    namefile2=paste0("OUTPUTS/SINGLE/",a[[1]][1],"/",a[[1]][2],"/")}
  
  print(paste0(namefile2, namefile,"_ARDUINO.csv"))
  intrantx<-rvw2$intrantw
  arduinox<-read.csv(paste0(namefile2, namefile,"_ARDUINO.csv"))
  data_coeffx<-read.csv(paste0(namefile2,namefile,"_COEFF.csv"))
  PEEKx<-read.csv(paste0(namefile2,namefile,"_PEEK.csv"))
  result1x<-read.csv(paste0(namefile2,namefile,"_STEP.csv"))
  resultx<-read.csv(paste0(namefile2, namefile,"_RESULTS.csv"))
  
  cat("parms \n")
  parm1=intrantx[1,2]
  parm2=intrantx[2,2]
  parm3=intrantx[3,2]
  parm4=intrantx[4,2]
  parm5=intrantx[5,2]
  parm6=intrantx[6,2]
  parm7=intrantx[7,2]
  parm8=intrantx[15,2]
  
  cat("write \n")
  dir.create(paste0("OUTPUTS/SINGLE/",parm1))
  dir.create(paste0("OUTPUTS/SINGLE/",parm1,"/",parm8))
  cat(paste0("OUTPUTS/SINGLE/", parm1,"/",parm8,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_",parm7,"_INPUT.csv"), "write \n")
  write.csv(intrantx, paste0("OUTPUTS/SINGLE/", parm1,"/",parm8,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_",parm7,"_INPUT.csv"),  row.names=FALSE)
  write.csv(arduinox, paste0("OUTPUTS/SINGLE/", parm1,"/",parm8,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_",parm7,"_ARDUINO.csv"),  row.names=FALSE)
  write.csv(data_coeffx, paste0("OUTPUTS/SINGLE/", parm1,"/",parm8,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_",parm7,"_COEFF.csv"),  row.names=FALSE)
  write.csv(PEEKx, paste0("OUTPUTS/SINGLE/", parm1,"/",parm8,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_",parm7,"_PEEK.csv"),  row.names=FALSE)
  write.csv(result1x, paste0("OUTPUTS/SINGLE/", parm1,"/",parm8,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_",parm7,"_STEP.csv"),  row.names=FALSE)
  write.csv(resultx, paste0("OUTPUTS/SINGLE/", parm1,"/",parm8,"/",parm1,"_",parm2,"_",parm3,"_",parm4,"_",parm5,"_",parm6,"_",parm7,"_RESULTS.csv"),  row.names=FALSE)
  
  dir.create("OUTPUTS/DELETE/")
  file.copy(paste0(namefile2, namefile,"_INPUT.csv"), "OUTPUTS/DELETE")
  unlink(paste0(namefile2, namefile,"_INPUT.csv"))
  file.copy(paste0(namefile2, namefile,"_ARDUINO.csv"), "OUTPUTS/DELETE")
  unlink(paste0(namefile2, namefile,"_ARDUINO.csv"))
  file.copy(paste0(namefile2, namefile,"_COEFF.csv"), "OUTPUTS/DELETE")
  unlink(paste0(namefile2, namefile,"_COEFF.csv"))
  file.copy(paste0(namefile2, namefile,"_PEEK.csv"), "OUTPUTS/DELETE")
  unlink(paste0(namefile2, namefile,"_PEEK.csv"))
  file.copy(paste0(namefile2, namefile,"_STEP.csv"), "OUTPUTS/DELETE")
  unlink(paste0(namefile2, namefile,"_STEP.csv"))
  file.copy(paste0(namefile2, namefile,"_RESULTS.csv"), "OUTPUTS/DELETE")
  unlink(paste0(namefile2, namefile,"_RESULTS.csv"))
  
})


############################################################
############################################################
#STEP 4 Data compilation
observeEvent(input$recalc4, {
direct<-getwd()
print("direct")
print(getwd())
setwd(input$DIR)
print("local")
print(getwd())
dir.create("OUTPUT", showWarnings = F)
#list of all csv files
list_files<- ( list.files(path=getwd(), recursive = T, pattern="_INPUT.csv") )
#list of files already read
list_files_already_read<-data.frame(no=integer(),
readfiles=character()) 
print("DB")
DB<-data.frame(matrix(ncol = 38, nrow = 0))
x <- c("file","parm1","parm2","parm3","parm4","parm5","parm6","parm7","parm8",
 "VALIDPEEKCHOICE",	"VALID_PEEK_TUBING",	"VALID_PSENSOR",	"TIME",	"STABILITY",	
 "SLOPE_P1","INTERCEPT_P1","SLOPE_P2","INTERCEPT_P2","PEEK_TUBING_ID","R_PEEK_TUBING_25",
 "P1","P2","P3","K_BRUT_MMOL","K_BRUT_KG","CV_K","T1","T2","T_AVG","R_PEEK_TUBING_T",
 "T3","K_T","K_25","STEMDIAMETER_AVG","AS","STEMLENGTH","KS","COMMENTS")
colnames(DB) <- x

if (file.exists("OUTPUT/list_files.csv")){
list_files_already_read<-read.csv("OUTPUT/list_files.csv")
DB<-read.csv("OUTPUT/DB.csv")
}
colnames(list_files_already_read)<-c("no","readfiles")

#list of new files
already_read <-(list_files_already_read$readfiles)
#colnames(already_read)<-c("readfiles")
to_read <- setdiff(list_files,already_read)


#loop on files to be read
if (length(to_read)>0) { #condition if there are new ones
for (i in 1:length(to_read)){ #loop
x<-unlist(gregexpr(pattern ='_INPUT.csv',to_read[i]))
file=substr(to_read[i],1,x[1]-1)
 
cat("file", i, file, "\n")
#pour imprimer dans la shiny
showModal(modalDialog(
title = "Reading the file:",
paste0("file", i, file, "\n"),
easyClose = TRUE,
footer = NULL
))

#if the sheet exists
ARDUINO <- tryCatch({
# The code you want run
read.csv(paste0(file,"_ARDUINO.csv"))
 }, warning = function(war) {
# Is executed if warning encountered
NULL
}, error = function(err) {
# Is executed if error encountered
NULL
})
COEFF <- tryCatch({
read.csv(paste0(file,"_COEFF.csv"))
}, warning = function(war) {NULL}, error = function(err) {NULL})
INPUT <- tryCatch({
read.csv(paste0(file,"_INPUT.csv"))
}, warning = function(war) {NULL}, error = function(err) {NULL})
PEEK <- tryCatch({
read.csv(paste0(file,"_PEEK.csv"))
}, warning = function(war) {NULL}, error = function(err) {NULL})
RESULTS <- tryCatch({
read.csv(paste0(file,"_RESULTS.csv"))
}, warning = function(war) {NULL}, error = function(err) {NULL})
STEP <- tryCatch({
read.csv(paste0(file,"_STEP.csv"))
}, warning = function(war) {NULL}, error = function(err) {NULL})

parm1<-	INPUT[1,2]
parm2<-	INPUT[2,2]
parm3<-	INPUT[3,2]
parm4<-	INPUT[4,2]
parm5<-	INPUT[5,2]
parm6<-	INPUT[6,2]
if(grepl('-', INPUT[7,2])==T){parm7<-	as.character(INPUT[7,2])}
if(grepl('-', INPUT[7,2])==F){parm7<-	as.character(zoo::as.Date(as.numeric(INPUT[7,2])))}
parm8<-	INPUT[15,2]
VALIDPEEKCHOICE<-	STEP[2,4]	
VALID_PEEK_TUBING<-	RESULTS[4,4]
VALID_PSENSOR<-	RESULTS[5,4]	
TIME<-	RESULTS[6,4]	
STABILITY<-	RESULTS[7,4]
SLOPE_P1<-	COEFF[3,2]
INTERCEPT_P1<-	COEFF[2,2]
SLOPE_P2<-	COEFF[3,3]
INTERCEPT_P2<-	COEFF[2,3]
PEEK_TUBING_ID<-PEEK[1,2]
R_PEEK_TUBING_25<-	PEEK[2,2]
P1<-	STEP[8,4]
P2<-	STEP[9,4]
P3<-	STEP[14,4]	
K_BRUT_MMOL<-	STEP[10,4]
K_BRUT_KG<-	STEP[11,4]
CV_K<-	STEP[12,4]
T1<-	STEP[15,4]#	
T2<-	STEP[16,4]
T_AVG<-	STEP[17,4]	#
R_PEEK_TUBING_T<-	STEP[18,4] #
T3<-	STEP[20,4] #
K_T<-	STEP[22,4] #
K_25<-	STEP[23,4] #
STEMDIAMETER_AVG<-	(as.numeric(INPUT[11,2])+as.numeric(INPUT[12,2]))/2
AS<-(((((as.numeric(INPUT[11,2])+as.numeric(INPUT[12,2]))/2/1000)/2)^2*pi-((0/1000)/2)^2*pi))
STEMLENGTH<- INPUT[13,2]
KS<- RESULTS[3,4] #
COMMENTS<-	INPUT[14,2]

file_res<-as.data.frame(cbind(file,parm1,parm2,parm3,parm4,parm5,parm6,parm7,parm8,VALIDPEEKCHOICE,VALID_PEEK_TUBING,
                              VALID_PSENSOR,TIME,STABILITY,SLOPE_P1,INTERCEPT_P1,SLOPE_P2,INTERCEPT_P2,PEEK_TUBING_ID,
                              R_PEEK_TUBING_25,P1,P2,P3,K_BRUT_MMOL,K_BRUT_KG,CV_K,T1,T2,T_AVG,R_PEEK_TUBING_T,T3,K_T,K_25,
                              STEMDIAMETER_AVG,AS,STEMLENGTH,KS,COMMENTS))

DB<-plyr::rbind.fill(DB,file_res)
print(head(DB))
} #dim(to_read)[1]
removeModal(session = getDefaultReactiveDomain())
#Export DB of files being read
write.csv(DB,"OUTPUT/DB.csv", row.names=F)
#Export the file containing the list of files that have been read
write.csv(list_files,"OUTPUT/list_files.csv")

} #length(to_read)>0

output$text <- renderText({ "Finish" })
setwd(direct)
}) ##STEP 4 Data compilation

#close the application 
observe({
if (input$close > 0) stopApp() # stop shiny
})
 
session$onSessionEnded(stopApp) 
}
