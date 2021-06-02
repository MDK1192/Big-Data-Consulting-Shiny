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
  dashboardHeader(title = "Emotion detection App"),
  #Sidebar Content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Landing Page", tabName = "landing", icon = icon("th")),
      menuItem("Results", tabName = "results", icon = icon("th")),
      menuItem("Visualization", tabName = "visualization", icon = icon("th"))
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
                      shinyDirButton("dir", "Dateien wählen", "Upload", width = '100%'),
                      verbatimTextOutput("dir", placeholder = TRUE),
                      #fileInput("file", label = h3("Choose custom path")),
                      #verbatimTextOutput("value")
                      ),
                  box(width = 8,
                      actionButton("loadButton", label = "Load Data",width = '100%'),
                      actionButton("transformButton", label = "Transform Data",width = '100%'),
                      tags$br(),
                      actionButton("startButton", label = "Start Emotion Detection", width = '100%')),
                  box(width = 12,
                      box(width = 6,DTOutput("FileTable"),title = "Dateiuebersicht tabellarisch"),
                      box(width = 4,uiOutput('my_audio'))),
                  box(width = 12,
                      box(width = 6,DTOutput("ImageTable"),title = "Bilderuebersicht tabellarisch"),
                      box(width = 6,uiOutput('my_image'))),
                  #box(width = 12,tags$img(src="current_files/image_000001.jpg")),
                  #box(width = 12,actionButton("GooglestartButton", label = "GoogleNet CAD starten", width = '100%')),
                  #box(width = 12,actionButton("VGGstartButton", label = "VGG16 CAD starten", width = '100%')),


              ),
      ),
      tabItem(tabName = "results",
              h2("Results"),
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
              h2("Visualizations"),
              box(width = 12, plotlyOutput("emotionsummarised"),title = "Ergebnisse grafisch"),
              box(width = 12,actionButton("showemotionovertime", label = "Show Emotion over Time",width = '100%')),
              box(width = 12,plotlyOutput("emotionovertime_audio"),title = "Ergebnisse Audio"),
              box(width = 12,plotlyOutput("emotionovertime_video"),title = "Ergebnisse Video"),

      )
    )
  )
)


get_audio_tag <- function(filename) {
  tags$audio(src = filename,
             type = "audio/mp3",
             controls = "controls")
}

get_image_tag <- function(imagename) {
  tags$img(width="100%", height="100%", src = imagename)
}

#
server <- function(input, output, session) {

  
  memory.limit(size = 250000)
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


  
  #helper functions
  get_audio_tag<-function(filename){tags$audio(src = filename,
                                               type ="audio/mp3", controls = NA)}
  get_image_tag<-function(imagename){
    tags$img(width="100%", height="100%", src = imagename)}
  
  #set global_env variables and memory limit
  path <- paste0(getwd(),"/www/data")
  file_selected <- NA
  memory.limit(size = 250000)
  
  #file overview
  observeEvent(input$loadButton, {

    if(is.list(input$dir)){
      hide("ImageTable")
      hide("my_audio")
      hide("my_image")
      #output$ImageTable <- renderDataTable(df_images,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
      #output$my_audio <-renderUI(get_audio_tag("temp.mp3"))
      #output$my_image <-renderUI(get_image_tag("temp.png"))
      df_files <- data.frame("Datei" = as.character(), "Herkunft" = as.character())
      output$FileTable <- renderDataTable(df_files,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
      #list & load files
      path <- ""
      for(i in 1:length(paste0(getwd(),"/",input$dir$path))){
        path <- paste0(path,"/",input$dir$path[i])
      }
      path <- gsub("//","/", path)
      files <- list.files(paste0(getwd(),"/",path))
      
      #input$dir$path[length(paste0(getwd(),"/",input$dir$path))]
      files_list <- list()
      df_files <- data.frame("File" = character())
      for(file in 1:length(files)){
        file_path <- paste0(path,"/",files[file])
        df_files[,1] <- as.character(df_files[,1])
        df_files <- rbind(df_files, files[file])
        names(df_files) <- "Datei"
      }
      df_files$Herkunft <- paste0(getwd(),path)
      df_files <<- df_files
      output$FileTable <- renderDataTable(df_files,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
    }
    else{
      shinyalert("Please Choose a folder first!",  type = "warning")
    }

  })
  observeEvent(input$showemotionovertime, {

    
    if(!exists("df_audio")){df_audio <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                                                   "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                                   ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"contempt"=as.character())}
    if(!exists("df_video")){df_video <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                                                   "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                               ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"contempt"=as.character())}
    
    nrow_df_audio <- nrow(df_audio)
    x <- seq(1:nrow_df_audio)
    if(nrow(df_audio) != 0){df_audio$file_number <- x}
    
    nrow_df_video <- nrow(df_video)
    x <- seq(1:nrow_df_video)
    if(nrow(df_video) != 0){df_video$file_number <- x}
    
    if(nrow(df_audio) != 0){output$emotionovertime_audio <- renderPlotly({

      fig <- plot_ly(df_audio, x=~file_number, y = ~neutral  , name = 'neutral', file=as.vector(df_audio$file), type = 'scatter', mode = 'markers', hovertemplate  = {file})
      fig <- fig %>% add_trace(y = ~happy    , name = 'happy', mode = 'markers')
      fig <- fig %>% add_trace(y = ~sad  , name = 'sad', mode = 'markers')
      fig <- fig %>% add_trace(y = ~angry   , name = 'angry', mode = 'markers')
      fig <- fig %>% add_trace(y = ~fear , name = 'fear', mode = 'markers')
      fig <- fig %>% add_trace(y = ~disgust , name = 'disgust', mode = 'markers')
      fig <- fig %>% add_trace(y = ~surprise   , name = 'surprise', mode = 'markers')
      fig <- fig %>% add_trace(y = ~calm, name = 'calm', mode = 'markers')
      fig
    })
    }
    if(nrow(df_video) != 0){output$emotionovertime_video <- renderPlotly({
      fig <- plot_ly(df_video, x=~file_number, y = ~neutral  , name = 'neutral', file=as.vector(df_video$file), type = 'scatter', mode = 'markers', hovertemplate  = {file})
      fig <- fig %>% add_trace(y = ~happy    , name = 'happy', mode = 'markers')
      fig <- fig %>% add_trace(y = ~sad  , name = 'sad', mode = 'markers')
      fig <- fig %>% add_trace(y = ~angry   , name = 'angry', mode = 'markers')
      fig <- fig %>% add_trace(y = ~fear , name = 'fear', mode = 'markers')
      fig <- fig %>% add_trace(y = ~disgust , name = 'disgust', mode = 'markers')
      fig <- fig %>% add_trace(y = ~surprise   , name = 'surprise', mode = 'markers')
      fig <- fig %>% add_trace(y = ~contempt, name = 'contempt', mode = 'markers')
      fig
    })
    }
    })
  observeEvent(input$transformButton, {
    
    try(source_python("skript_wav_png_transform.py"))
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
    precompute_spectrograms()
    
  })
  observeEvent(input$VoiceEmotionTable_rows_selected, {
    
    file_selected <- df_audio$file[input$VoiceEmotionTable_rows_selected]
    file_selected <- gsub(".png","",file_selected)
    file_selected <- gsub("test50_","",file_selected)
    path_of_file <- find.file(file_selected, dir = "www", dirs = c("www/current_files","www/current_files/audio","www/current_files/video","www/current_files/Temp_Current_Video", 
                                                             "www/data","www/data/audio","www/data/video", 
                                                             "www/testdata_general", "www/testdata_general/audio", "www/testdata_general/video",
                                                             "www/testdata_sequence", 
                                                             "www/wav_transform","www/wav_transform/untransfomred_wavs",
                                                             "www/wavs and videos"))
    file_selected <- gsub("www/","",path_of_file)

    #actions for png
    if(grepl("\\wav$", file_selected)){
      wav_name = file_selected
      show("my_image_audio")
      output$my_image_audio <-renderUI(get_audio_tag(wav_name))
      output$audiotag<-renderUI(get_audio_tag("tempwav.wav")) #starting wave file
      output$audiotag<-renderUI(get_audio_tag(wavname))
    }
  })
  observeEvent(input$ImageEmotionTable_rows_selected, {
    
    path_of_file <- strsplit(as.character(df_files$Herkunft[df_video$file[input$ImageEmotionTable_rows_selected]==df_files$Datei]), "www/")
    if(length(path_of_file)==0){
      path_of_file <- strsplit(as.character(paste0(df_files$Herkunft[df_files$Datei=="video"],"/video")), "www/")
      }
    file_selected <- paste0(path_of_file[[1]][2],"/",df_video$file[input$ImageEmotionTable_rows_selected])
    #actions for png
    if(grepl("\\png$", file_selected)){
      img_name = file_selected
      show("my_image_video")
      output$my_image_video <-renderUI(get_image_tag(img_name))
      output$imagetag<-renderUI(get_image_tag("tempimg.jpg")) #starting jpg file)
    }
  })
  #wav & video to app import
  observeEvent(input$FileTable_rows_selected, {
    names(df_files) <- c("Datei", "Herkunft")
    path_of_file <- strsplit(df_files$Herkunft[input$FileTable_rows_selected], "www/")
    file_selected <- paste0(path_of_file[[1]][2],"/",df_files$Datei[input$FileTable_rows_selected])
    #actions for png
    if(grepl("\\png$", df_files$Datei[input$FileTable_rows_selected])){
      path_of_file <- strsplit(as.character(df_files$Herkunft[input$FileTable_rows_selected]), "www/")
      image_selected <- paste0(path_of_file[[1]][2],"/",df_files$Datei[input$FileTable_rows_selected])
      img_name = image_selected
      show("my_image")
      output$my_image <-renderUI(get_image_tag(img_name))
      output$imagetag<-renderUI(get_image_tag("tempimg.jpg")) #starting jpg file
      output$imagetag<-renderUI(get_image_tag(imgname))
    }
    
    #actions for .wav
    if(grepl("\\.wav$", df_files$Datei[input$FileTable_rows_selected])){
      path_of_file <- strsplit(as.character(df_files$Herkunft[input$FileTable_rows_selected]), "www/")
      file_selected <- paste0(path_of_file[[1]][2],"/",df_files$Datei[input$FileTable_rows_selected])
      wav_name = file_selected
      # 
      show("my_audio")
      output$my_audio <-renderUI(get_audio_tag(wav_name))
      output$audiotag<-renderUI(get_audio_tag("tempwav.wav")) #starting wave file
      output$audiotag<-renderUI(get_audio_tag(wavname))
    }
    
    #actions for .mp4
    if(grepl("\\.mp4$", df_files$Datei[input$FileTable_rows_selected])){
      
      path_of_file <- strsplit(as.character(df_files$Herkunft), "www/")
      #extract images & wav from mp4
      files <- list.files("www/current_files/video")
      for(file in files){
        unlink(paste0("www/current_files/video/",file))
      }
      av_video_images(paste0("www/",path_of_file[[input$FileTable_rows_selected]][2],"/",df_files$Datei[input$FileTable_rows_selected]), destdir = "www/current_files/video", format = "png")
      
      #Define the file name that will be deleted
      unlink("www/current_files/Temp_Current_Video/current_video.wav")
      av_audio_convert(paste0("www/",path_of_file[[input$FileTable_rows_selected]][2],"/",df_files$Datei[input$FileTable_rows_selected]), 'current_video.wav', channels = NULL)
      file.copy("current_video.wav", "www/current_files/Temp_Current_Video/current_video.wav")
      images <- list.files("www/current_files/video")
      files_list <- list()
      df_images <- data.frame("Image" = character())
      for(file in 1:length(images)){
        image_path <- paste0(path,"/",images[file])
        df_images[,1] <- as.character(df_images[,1])
        df_images <- rbind(df_images, images[file])
        names(df_images) <- "Bild"
      }
      wav_name = "current_files/Temp_Current_Video/current_video.wav"
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
  #Loading  & showing images improted from video
  observeEvent(input$ImageTable_rows_selected, {
    image_selected <- paste0("current_files/video/",df_images$Bild[input$ImageTable_rows_selected])
    img_name = image_selected
    show("my_image")
    output$my_image <-renderUI(get_image_tag(img_name))
    output$imagetag<-renderUI(get_image_tag("tempimg.jpg")) #starting jpg file
    output$imagetag<-renderUI(get_image_tag(imgname))
  })
  observeEvent(input$loadModels, {
    
    source_python("skript_video_class.py")
    Sys.sleep(3)
    source_python("skript_audio_class.py")
    
  })
  observeEvent(input$startButton, {
    
    if(!exists("df_files")){
      shinyalert("Please Choose a folder first!",  type = "warning")
    }

    else{
      df_audio <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                             "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                             ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"contempt"=as.character())
      df_video <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                             "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                             ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"contempt"=as.character())
      
      try(source_python("skript_video_class.py"))
      try(source_python("skript_audio_class.py"))
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

      try(df_audio <- check_wav(audio_path))
      try(df_video <- check_img(video_path))
      try(df_audio <<- df_audio[,c(1,2,3,5,6,7,8,9,10,4)])
      try(df_video <<- df_video[,c(1,2,8,7,9,3,6,5,10,4)])
      try(names(df_video) <<- c("file","classification","neutral","happy","sad","angry","fear","disgust","surprise","contempt"))
      if(!exists("df_audio")){df_audio <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                                                     "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                                     ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"contempt"=as.character())}
      if(!exists("df_video")){df_video <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                                                     "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                                     ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"contempt"=as.character())}
      output$VoiceEmotionTable <- renderDataTable(df_audio,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
      output$ImageEmotionTable <- renderDataTable(df_video,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))

      output$emotionsummarised <- renderPlotly({
        df_audio_table <- data.frame("neutral"=0,"happy"=0,"sad"=0,"angry"=0
                                     ,"fear"=0,"disgust"=0,"surprise"=0,"contempt"=0)
        df_video_table <- data.frame("neutral"=0,"happy"=0,"sad"=0,"angry"=0
                                     ,"fear"=0,"disgust"=0,"surprise"=0,"contempt"=0)
        
        df_audio_summary <- table(df_audio$classification)
        df_video_summary <- table(df_video$classification)
        
        
        for(i in 1:length(df_audio_table)){
          if(names(df_audio_table)[i] %in% names(df_audio_summary)){
            print(names(df_audio_table)[i])
            print(names(df_audio_summary))
            df_audio_table[1,i] <- as.numeric(df_audio_summary[names(df_audio_table)[i]])
          }
        }
        
        for(i in 1:length(df_video_table)){
          if(names(df_video_table)[i] %in% names(df_video_summary)){
            print(names(df_video_table)[i])
            print(names(df_video_summary))
            df_video_table[1,i] <- as.numeric(df_video_summary[names(df_video_table)[i]])
          }
        }
        
        emotions_summary_df <- cbind(df_audio_table, df_video_table)
        Emotions <- c("neutral","happy","sad","angry","fear","disgust","surprise","contempt")
        Emotions_Audio_Summary <- table(df_audio$classification)
        
        Emotions_Video_Summary <- table(df_audio$classification)
        audio_vector <- c(df_audio_table$neutral[1],df_audio_table$happy[1],df_audio_table$sad[1],df_audio_table$angry[1],
                          df_audio_table$fear[1],df_audio_table$disgust[1],df_audio_table$surprise[1],df_audio_table$contempt[1])
        video_vector <- c(df_video_table$neutral[1],df_video_table$happy[1],df_video_table$sad[1],df_video_table$angry[1],
                          df_video_table$fear[1],df_video_table$disgust[1],df_video_table$surprise[1],df_video_table$contempt[1])
        overview_plot_data <- data.frame(Emotions, audio_vector, video_vector)
        
        fig <- plot_ly(overview_plot_data, x = ~Emotions, y = ~audio_vector, type = 'bar', name = 'Audio')
        fig <- fig %>% add_trace(y = ~video_vector, name = 'Video')
        fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
        
        fig
        
        
      })
    }

  })
  
}

shinyApp(ui, server)


