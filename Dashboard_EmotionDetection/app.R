library(shiny)
library(shinyFiles)
library(shinyjs)
library(shinydashboard)
library(reticulate)
library(imager)
library(DT)
library(reader)
library(dplyr)
library(av)
library(plotly)
library(shinyalert)

ui <- dashboardPage(
  #Header Content
  dashboardHeader(title = "Emotion Detection App"),
  #Sidebar Content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Landing Page", tabName = "landing", icon = icon("th")),
      menuItem("Ergebnisse Tabellarisch", tabName = "results", icon = icon("th")),
      menuItem("Visualisierungen", tabName = "visualization", icon = icon("th"))
    )
  ),
  #Body Content
  dashboardBody(
    useShinyalert(),
    useShinyjs(), 
    tabItems(
      tabItem(tabName = "landing",
              h2("Landingpage"),
              box(width = 12,
                  box(width = 4,
                      shinyDirButton("dir", "Dateien wÃ¤hlen", "Upload", width = '100%'),
                      verbatimTextOutput("dir", placeholder = TRUE),
                      #fileInput("file", label = h3("Choose custom path")),
                      #verbatimTextOutput("value")
                      ),
                  box(width = 8,
                      actionButton("loadButton", label = "Daten laden",width = '100%'),
                      actionButton("transformButtonWAV", label = ".WAVs in Spektogramme umwandeln",width = '100%'),
                      sliderInput("MP4slider", "Videosplits in Sekunden:",min = 0, max = 10,value = 1),
                      tags$br(),
                      actionButton("startButton", label = "Emotion Detection Starten", width = '100%')),
                  box(width = 12,
                      box(width = 6,DTOutput("FileTable"),title = "Dateiuebersicht tabellarisch"),
                      box(width = 4,uiOutput('my_audio'))),
                  box(width = 12,
                      box(width = 6,DTOutput("ImageTable"),title = "Bilderuebersicht tabellarisch"),
                      box(width = 6,uiOutput('my_image'))),
              ),
      ),
      tabItem(tabName = "results",
              box(width = 12,
                box(width = 8,DTOutput("VoiceEmotionTable"),title = "Sprachemotionserkennung tabellarisch"),
                box(width = 4,uiOutput('my_image_audio'))),
              box(width = 12,
                box(width = 8,DTOutput("ImageEmotionTable"),title = "Mimikemotionserkennung tabellarisch"),
                box(width = 4,uiOutput('my_image_video'))),
                box(width = 12,plotOutput("Image")
              )
      ),
      tabItem(tabName = "visualization",
              box(width = 12, plotlyOutput("emotionsummarised"),title = "Countplot der Ergebnisse"),
              box(width = 12,actionButton("showemotionovertime", label = "Einzelnergebnisse anzeigen ",width = '100%')),
              box(width = 12,plotlyOutput("emotionovertime_audio"),title = "Ergebnisse Audio"),
              box(width = 12,plotlyOutput("emotionovertime_video"),title = "Ergebnisse Mimik"),

      )
    )
  )
)

#addon Funktionen für display von .wav und .pngs
get_audio_tag <- function(filename) {
  tags$audio(src = filename,
             type = "audio/mp3",
             controls = "controls")
}
get_image_tag <- function(imagename) {
  tags$img(width="100%", height="100%", src = imagename)
}

server <- function(input, output, session) {

  #Erhöhung des verfügbaren Specihers der App für größere Uploads
  memory.limit(size = 250000)
  
  #Implementierung von File System Logik
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = getwd()),
    filetypes = c('', 'jpg', 'jpeg', 'png','wav', 'mp4')
  )
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$dir)
  
  output$dir <- renderText({
    global$datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <- getwd()
                 
                 Normal_path <- paste0(global$datapath,"/Test/Normal")
                 Anomaly_path <- paste0(global$datapath,"/Test/Anomaly")
                 Normal_path <<- gsub("\\", "/", as.character(Normal_path), fixed=TRUE)
                 Anomaly_path <<- gsub("\\", "/", as.character(Anomaly_path), fixed=TRUE)
                 
               })


  
  #helper functions für .wav und .png display
  get_audio_tag<-function(filename){tags$audio(src = filename,
                                               type ="audio/mp3", controls = NA)}
  get_image_tag<-function(imagename){
    tags$img(width="100%", height="100%", src = imagename)}
  

#Logik für importvorgänge von Dateien
  observeEvent(input$loadButton, {

    if(is.list(input$dir)){
      #Unnötige UI-Elemente verbergen
      hide("ImageTable")
      hide("my_audio")
      hide("my_image")
      
      #erzeugung und ausgabe von template df
      df_files <- data.frame("Datei" = as.character(), "Herkunft" = as.character())
      output$FileTable <- renderDataTable(df_files,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
      
      #list & load files
      path <- ""
      for(i in 1:length(paste0(getwd(),"/",input$dir$path))){
        path <- paste0(path,"/",input$dir$path[i])
      }
      path <- gsub("//","/", path)
      files <- list.files(paste0(getwd(),"/",path))
      
      files_list <- list()
      df_files <- data.frame("File" = character())
      for(file in 1:length(files)){
        file_path <- paste0(path,"/",files[file])
        df_files[,1] <- as.character(df_files[,1])
        df_files <- rbind(df_files, files[file])
        names(df_files) <- "Datei"
      }
      df_files$Herkunft <- paste0(getwd(),path)
      #df_files in globale env. exportieren für andere Module
      df_files <<- df_files
      #Ausgabe df_files
      output$FileTable <- renderDataTable(df_files,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
    }
    #exception im falle von ungültiger eingabe
    else{
      shinyalert("Please Choose a folder first!",  type = "warning")
    }

  })

#Logik für Overtime Plots der App
  observeEvent(input$showemotionovertime, {
    #template generierung falls df nicht existieren
    if(!exists("df_audio")){df_audio <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                                                   "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                                   ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(), "id"=as.character())}
    if(!exists("df_video")){df_video <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                                                   "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                               ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(), "id"=as.character())}
    
    #Generierung und Ausgabe plots 
    if(nrow(df_audio) != 0){output$emotionovertime_audio <- renderPlotly({
      df_audio$number <- seq(1,nrow(df_audio))
      fig <- plot_ly(df_audio, x=~number, y = ~neutral  , name = 'neutral', type = 'scatter', mode = 'markers')
      fig <- fig %>% add_trace(y = ~happy    , name = 'happy', mode = 'markers')
      fig <- fig %>% add_trace(y = ~sad  , name = 'sad', mode = 'markers')
      fig <- fig %>% add_trace(y = ~angry   , name = 'angry', mode = 'markers')
      fig <- fig %>% add_trace(y = ~fear , name = 'fear', mode = 'markers')
      fig <- fig %>% add_trace(y = ~disgust , name = 'disgust', mode = 'markers')
      fig <- fig %>% add_trace(y = ~surprise   , name = 'surprise', mode = 'markers')
      fig <- fig %>% layout(hovermode = "x", xaxis = list(title="Objekt"), yaxis=list(title="Ergebnis"))
      fig
      fig
    })
    }
    if(nrow(df_video) != 0){output$emotionovertime_video <- renderPlotly({
      df_video$number <- seq(1,nrow(df_video))
      fig <- plot_ly(df_video, x=~number, y = ~neutral  , name = 'neutral', type = 'scatter', mode = 'markers')
      fig <- fig %>% add_trace(y = ~happy    , name = 'happy', mode = 'markers')
      fig <- fig %>% add_trace(y = ~sad  , name = 'sad', mode = 'markers')
      fig <- fig %>% add_trace(y = ~angry   , name = 'angry', mode = 'markers')
      fig <- fig %>% add_trace(y = ~fear , name = 'fear', mode = 'markers')
      fig <- fig %>% add_trace(y = ~disgust , name = 'disgust', mode = 'markers')
      fig <- fig %>% add_trace(y = ~surprise   , name = 'surprise', mode = 'markers')
      fig <- fig %>% layout(hovermode = "x", xaxis = list(title="Objekt"), yaxis=list(title="Ergebnis"))
      fig
      fig
      
    })
    }
    })

#Logik für Erzeugung der Spektogramme aus .wav inhalten
  observeEvent(input$transformButtonWAV, {
    #Laden von TransformationSkript via reticulate
    try(source_python("skript_wav_png_transform.py"))
    #.wav dateien identifizieren und selektieren
    path_of_file <- strsplit(as.character(df_files$Herkunft), "www/")
    path_of_file <- path_of_file[[1]][2]
    files <- list.files("www/current_files/audio")
    for(file in files){
      unlink(paste0("www/current_files/audio/",file))
    }
    files <- list.files("www/wav_transform/untransformed_wavs")
    for(file in files){
      unlink(paste0("www/wav_transform/untransformed_wavs/",file))
    }
    files <- list.files(paste0("www/",path_of_file))
    for(file in files){
      if(grepl("\\.wav$", file)){
        file.copy(paste0("www/",path_of_file,"/",file), paste0("www/wav_transform/untransformed_wavs/",file))
      }
    }
    #transformationsfunktion ausführen
    precompute_spectrograms()
    
  })

#Logik für Aktionen innerhalb der Ergebnistabelle für Voice Emotion detection
  observeEvent(input$VoiceEmotionTable_rows_selected, {
    #filtern der ausgewählten datei und vorbereitung für weitere Schritte
    file_selected <- df_audio$file[input$VoiceEmotionTable_rows_selected]
    file_selected <- gsub(".png","",file_selected)
    file_selected <- gsub("test50_","",file_selected)
    #suche der aktiven datei in Filesystem
    path_of_file <- find.file(file_selected, dir = "www", dirs = c("www/current_files","www/current_files/audio","www/current_files/video","www/current_files/Temp_Current_Video", 
                                                             "www/data","www/data/audio","www/data/video", 
                                                             "www/testdata_general", "www/testdata_general/audio", "www/testdata_general/video",
                                                             "www/testdata_sequence", 
                                                             "www/wav_transform","www/wav_transform/untransfomred_wavs",
                                                             "www/wavs and videos"))

    file_selected <- gsub("www/","",path_of_file)

    #Aktionen für .wav dateien
    if(grepl("\\wav$", file_selected)){
      #anzeigen der aktiven Datei in UI
      wav_name = file_selected
      show("my_image_audio")
      output$my_image_audio <-renderUI(get_audio_tag(wav_name))
      output$audiotag<-renderUI(get_audio_tag("tempwav.wav")) #starting wave file
      output$audiotag<-renderUI(get_audio_tag(wavname))
    }
  })
  
#Logik für Aktionen innerhalb der Ergebnistabelle für Mimik Emotion detection
  observeEvent(input$ImageEmotionTable_rows_selected, {
    #selektieren von gewählter datei
    path_of_file <- strsplit(as.character(df_files$Herkunft[df_video$file[input$ImageEmotionTable_rows_selected]==df_files$Datei]), "www/")
    #exception für ungültige angabe
    if(length(path_of_file)==0){
      path_of_file <- strsplit(as.character(paste0(df_files$Herkunft[df_files$Datei=="video"],"/video")), "www/")
      }
    file_selected <- paste0(path_of_file[[1]][2],"/",df_video$file[input$ImageEmotionTable_rows_selected])
    #aktionen für png undAusgabe in UI
    if(grepl("\\png$", file_selected)){
      img_name = file_selected
      show("my_image_video")
      output$my_image_video <-renderUI(get_image_tag(img_name))
      output$imagetag<-renderUI(get_image_tag("tempimg.jpg")) #starting jpg file)
    }
  })
 
#Logik für Aktionen innerhalb der Tabelle mit Rohdaten
  observeEvent(input$FileTable_rows_selected, {
    #Erzeugen von leeren df
    names(df_files) <- c("Datei", "Herkunft")
    path_of_file <- strsplit(df_files$Herkunft[input$FileTable_rows_selected], "www/")
    file_selected <- paste0(path_of_file[[1]][2],"/",df_files$Datei[input$FileTable_rows_selected])
    #aktionen für png
    if(grepl("\\png$", df_files$Datei[input$FileTable_rows_selected])){
      #filtern der dateu
      path_of_file <- strsplit(as.character(df_files$Herkunft[input$FileTable_rows_selected]), "www/")
      image_selected <- paste0(path_of_file[[1]][2],"/",df_files$Datei[input$FileTable_rows_selected])
      img_name = image_selected
      show("my_image")
      #ausgabe der datai in UI
      output$my_image <-renderUI(get_image_tag(img_name))
      output$imagetag<-renderUI(get_image_tag("tempimg.jpg")) #starting jpg file
      output$imagetag<-renderUI(get_image_tag(imgname))
    }
    
    #aktionen für wav
    if(grepl("\\.wav$", df_files$Datei[input$FileTable_rows_selected])){
      #filtern der datei
      path_of_file <- strsplit(as.character(df_files$Herkunft[input$FileTable_rows_selected]), "www/")
      file_selected <- paste0(path_of_file[[1]][2],"/",df_files$Datei[input$FileTable_rows_selected])
      wav_name = file_selected
      # 
      show("my_audio")
      #ausgabe der datei in UI
      output$my_audio <-renderUI(get_audio_tag(wav_name))
      output$audiotag<-renderUI(get_audio_tag("tempwav.wav")) #starting wave file
      output$audiotag<-renderUI(get_audio_tag(wavname))
    }
    
    #aktionen für mp4
    if(grepl("\\.mp4$", df_files$Datei[input$FileTable_rows_selected])){
      #filtern der datei
      path_of_file <- strsplit(as.character(df_files$Herkunft), "www/")
      #extract images & wav from mp4
      files <- list.files("www/current_files/video")
      #löschen von vorherigen pfaden/dateien
      for(file in files){
        unlink(paste0("www/current_files/video/",file))
      }
      #Frames aus Video generieren
      av_video_images(paste0("www/",path_of_file[[input$FileTable_rows_selected]][2],"/",df_files$Datei[input$FileTable_rows_selected]), destdir = "www/current_files/video", format = "png")
      
      unlink("www/current_files/Temp_Current_Video/current_video.wav")
      #länge des ausgewählten videos ableiten
      length_video <- av_media_info(paste0("www/",path_of_file[[input$FileTable_rows_selected]][2],"/",df_files$Datei[input$FileTable_rows_selected]))$duration
      
      #löschen von vorherigen pfaden/dateien
      files <- list.files("www/current_files/Temp_Current_Video")
      for(file in files){
        unlink(paste0("www/current_files/Temp_Current_Video/",file))
      }
      #.wav aus ausgewählten video generieren
      for(i in 1:(round(length_video)/input$MP4slider)){
        browser()
        start_time <- i-1
        video_name <- paste0('current_video_segment',i,'.wav')
        unlink(paste0("www/current_files/Temp_Current_Video/",video_name))
        av_audio_convert(paste0("www/",path_of_file[[input$FileTable_rows_selected]][2],"/",df_files$Datei[input$FileTable_rows_selected]), start_time = start_time,total_time=input$MP4slider ,output = video_name, channels = NULL)
        file.copy(video_name, paste0("www/current_files/Temp_Current_Video/",video_name))
        
        }

      #bestehende dateien in current files auslesen
      images <- list.files("www/current_files/video")
      files_list <- list()
      #df erzeugen und bestehende dateien aufnehmen
      df_images <- data.frame("Image" = character())
      for(file in 1:length(images)){
        image_path <- paste0(path,"/",images[file])
        df_images[,1] <- as.character(df_images[,1])
        df_images <- rbind(df_images, images[file])
        names(df_images) <- "Bild"
      }
      #template .wav selektieren und in UI anzeigen
      wav_name = "current_files/Temp_Current_Video/current_video_segment1.wav"
      # output$my_audio <-renderUI(get_audio_tag("questionF.mp3"))
      show("my_audio")
      output$my_audio <-renderUI(get_audio_tag(wav_name))
      output$audiotag<-renderUI(get_audio_tag("tempwav.wav")) #starting wave file
      output$audiotag<-renderUI(get_audio_tag(wavname))
      
      #export everything to global env.
      allglobal <- function() {
        lss <- ls(envir = parent.frame())
        for (i in lss) {
          assign(i, get(i, envir = parent.frame()), envir = .GlobalEnv)
        }
      }
      allglobal()
      show("ImageTable")
      output$ImageTable <- renderDataTable(df_images,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
    }
    
    
  })

#Logik für Aktionen innerhalb der Tabelle mit aus .mp4 erzeugten Bildinhalten
  observeEvent(input$ImageTable_rows_selected, {
    #ausgewählte datei filtern
    image_selected <- paste0("current_files/video/",df_images$Bild[input$ImageTable_rows_selected])
    img_name = image_selected
    #datei in UI anzeigen
    show("my_image")
    output$my_image <-renderUI(get_image_tag(img_name))
    output$imagetag<-renderUI(get_image_tag("tempimg.jpg")) #starting jpg file
    output$imagetag<-renderUI(get_image_tag(imgname))
  })
  
#Logik für Bildanalysen durch Mimik und Voice Emotion detection Modelle
  observeEvent(input$startButton, {
    #nicht benötigte UI-Elemente verbergen
    hide("emotionovertime_audio")
    hide("emotionovertime_video")
    #exception falls df_files nicht existiert
    if(!exists("df_files")){
      shinyalert("Please Choose a folder first!",  type = "warning")
    }

    #dummy_df für df_files erzeugen
    else{
      df_audio <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                             "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                             ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"number"=as.numeric())
      df_video <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                             "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                             ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"number"=as.numeric())
      
      #python skripte für modelle laden
      try(source_python("skript_video_class.py"))
      try(source_python("skript_audio_class.py"))
      #pfade selektieren und UI-Elemente die nicht benötigt werden verbergen
      path_of_file <- strsplit(as.character(df_files$Herkunft), "www/")
      audio_path <- paste0("www/",path_of_file[[1]][2],"/audio")
      video_path <- paste0("www/",path_of_file[[1]][2],"/video")
      if(grepl("audio/audio",audio_path, fixed = T)){
        audio_path <- substr(audio_path, start=0, stop=nchar(audio_path)-6)
        hide("my_image_video")
        show("my_image_audio")
      }
      else if(grepl("video/video",video_path, fixed = T)){
        video_path <- substr(video_path, start=0, stop=nchar(video_path)-6)
        hide("my_image_audio")
        show("my_image_video")
      }
      else{
        audio_path <- gsub("/audio","",audio_path)
        video_path <- gsub("/video","",video_path)
        audio_path <- gsub("/video","",audio_path)
        video_path <- gsub("/audio","",video_path)
        audio_path <- paste0(audio_path,"/audio")
        video_path <- paste0(video_path,"/video")
      }
      #Bildinhalte klassifizieren und Ergebnisse für Visualisierung verarbeiten
      try(df_audio <- check_wav(audio_path))
      try(df_video <- check_img(video_path))
      try(df_video <- df_video[,c(1,2,7,6,8,3,5,4,9)])
      try(df_audio$id <-paste0("img", as.character(seq(1:nrow(df_audio)))))
      try(df_video$id <-paste0("img", as.character(seq(1:nrow(df_video)))))
      try(names(df_video) <- c("file","classification","neutral","happy","sad","angry","fear","disgust","surprise","id"))
      try(names(df_video) <- c("file","classification","neutral","happy","sad","angry","fear","disgust","surprise","id"))
      if(!exists("df_audio")){df_audio <- data.frame("file"=as.character(),"number"=as.numeric(),"classification"=as.character(),"neutral"=as.character(),
                                                           "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                                           ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"number"=as.numeric())}
      if(!exists("df_video")){df_video <- data.frame("file"=as.character(),"number"=as.numeric(),"classification"=as.character(),"neutral"=as.character(),
                                                     "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                                     ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"number"=as.numeric())}
      
      #df in globale env. exportieren
      df_audio <<- df_audio
      df_video <<- df_video
      #ausgabe der df in UI
      output$VoiceEmotionTable <- renderDataTable(df_audio,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
      output$ImageEmotionTable <- renderDataTable(df_video,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))

      #plot für generelle overview der Ergebnisse
      output$emotionsummarised <- renderPlotly({
        #df für zusammenfassung anlegen
        df_audio_table <- data.frame("neutral"=0,"happy"=0,"sad"=0,"angry"=0
                                     ,"fear"=0,"disgust"=0,"surprise"=0)
        df_video_table <- data.frame("neutral"=0,"happy"=0,"sad"=0,"angry"=0
                                     ,"fear"=0,"disgust"=0,"surprise"=0)
        
        df_audio_summary <- table(df_audio$classification)
        df_video_summary <- table(df_video$classification)
        
        
        #Ergebnisse auswerten
        for(i in 1:length(df_audio_table)){
          if(names(df_audio_table)[i] %in% names(df_audio_summary)){
            df_audio_table[1,i] <- as.numeric(df_audio_summary[names(df_audio_table)[i]])
          }
        }
        
        for(i in 1:length(df_video_table)){
          if(names(df_video_table)[i] %in% names(df_video_summary)){
            df_video_table[1,i] <- as.numeric(df_video_summary[names(df_video_table)[i]])
          }
        }
        #Ergebnisse für Visualisierung vorbereiten und in UI anzeigen
        emotions_summary_df <- cbind(df_audio_table, df_video_table)
        Emotionen <- c("neutral","happy","sad","angry","fear","disgust","surprise")
        Emotions_Audio_Summary <- table(df_audio$classification)
        
        Emotions_Video_Summary <- table(df_audio$classification)
        audio_vector <- c(df_audio_table$neutral[1],df_audio_table$happy[1],df_audio_table$sad[1],df_audio_table$angry[1],
                          df_audio_table$fear[1],df_audio_table$disgust[1],df_audio_table$surprise[1])
        video_vector <- c(df_video_table$neutral[1],df_video_table$happy[1],df_video_table$sad[1],df_video_table$angry[1],
                          df_video_table$fear[1],df_video_table$disgust[1],df_video_table$surprise[1])
        overview_plot_data <- data.frame(Emotionen, audio_vector, video_vector)
        
        fig <- plot_ly(overview_plot_data, x = ~Emotionen, y = ~audio_vector, type = 'bar', name = 'Audio')
        fig <- fig %>% add_trace(y = ~video_vector, name = 'Mimik')
        fig <- fig %>% layout(yaxis = list(title = 'Anzahl'), barmode = 'group')
        
        fig
        
        
      })
    }

  })
  
}

shinyApp(ui, server)


