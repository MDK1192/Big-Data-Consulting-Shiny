library(shiny)
library(shinyFiles)
library(shinydashboard)
library(reticulate)
library(imager)
library(DT)

library(dplyr)
library(av)
library(plotly)

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
                      #actionButton("transformButton", label = "Transform Data",width = '100%'),
                      tags$br(),
                      actionButton("testButton", label = "Load Testdata", width = '100%'),
                      tags$br(),
                      actionButton("startButton", label = "Start Emotion Detection", width = '100%')),
                  box(width = 12,
                      box(width = 6,DTOutput("FileTable"),title = "Dateiuebersicht tabellarisch"),
                      box(width = 4,uiOutput('my_audio'))),
                  box(width = 12,
                      box(width = 6,DTOutput("ImageTable"),title = "Bilderuebersicht tabellarisch"),
                      box(width = 6,uiOutput('my_image'))),
                  #box(width = 12,tags$img(src="image_dir/image_000001.jpg")),
                  #box(width = 12,actionButton("GooglestartButton", label = "GoogleNet CAD starten", width = '100%')),
                  #box(width = 12,actionButton("VGGstartButton", label = "VGG16 CAD starten", width = '100%')),


              ),
      ),
      tabItem(tabName = "results",
              h2("Results"),
              box(width = 12,DTOutput("VoiceEmotion"),title = "Bilderuebersicht tabellarisch"),
              box(width = 12,DTOutput("ImageEmotion"),title = "Bilderuebersicht tabellarisch"),
              box(width = 12,DTOutput("ImageTable2"),title = "Bilderuebersicht tabellarisch"),
              box(width = 12,plotOutput("Image")
              )
      ),
      tabItem(tabName = "visualization",
              h2("Visualizations"),
              box(width = 12, 
                  actionButton("showemotionovertime", label = "Show Emotion over Time",width = '100%'),
                  plotlyOutput("emotionovertime"),title = "Ergebnisse grafisch"),
              box(width = 12, plotlyOutput("emotionsummarised"),title = "Ergebnisse grafisch"),

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

  #sampleplot 1
  output$emotionovertime <- renderPlotly({
    #sampledata for plot
    data_sample <- read.csv2("data_sample.csv", sep=",")
    data_sample <- data_sample[1190:1230,]
    data_sample$Price <- 30
    fig <- plot_ly(data_sample, x=~X, y = ~Price, name = 'Value', type = 'scatter', mode = 'lines')
    
    fig <- fig %>% add_trace(y = ~meanf, name = 'Anger', mode = 'lines')
    fig <- fig %>% add_trace(y = ~naive, name = 'Happy', mode = 'lines')
    fig <- fig %>% add_trace(y = ~snaive, name = 'Disgust', mode = 'lines')
    fig <- fig %>% add_trace(y = ~rwf, name = 'Trust', mode = 'lines')
    fig <- fig %>% add_trace(y = ~ses, name = 'Annoyed', mode = 'lines')
    fig <- fig %>% add_trace(y = ~holt, name = 'Sad', mode = 'lines')
    fig <- fig %>% add_trace(y = ~splinef, name = 'Hungry', mode = 'lines')
    fig <- fig %>% add_trace(y = ~thetaf, name = 'Hangry', mode = 'lines')
    fig
  })
  
  #sampleplot2

  
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
  })
  observeEvent(input$testButton, {
    
    
  })
  observeEvent(input$testButton, {
    path <- paste0(getwd(),"/www/testdata_general/audio")
    #list & load files
    files <- list.files(path)
    files_list <- list()
    df_files_audio <- data.frame("File" = character(), "Origin" = character())
    for(file in 1:length(files)){
      file_path <- paste0(path,"/",files[file])
      df_files_audio[,1] <- as.character(df_files_audio[,1])
      df_files_audio <- rbind(df_files_audio, c(files[file], path))
      names(df_files_audio) <- "Datei"
    }
    path <- paste0(getwd(),"/www/testdata_general/video")
    files <- list.files(path)
    files_list <- list()
    df_files_video <- data.frame("File" = character(), "Origin" = character())
    for(file in 1:length(files)){
      file_path <- paste0(path,"/",files[file])
      df_files_video[,1] <- as.character(df_files_video[,1])
      df_files_video <- rbind(df_files_video, c(files[file],path))
      names(df_files_video) <- "Datei"
    }
    df_files <- rbind(df_files_video, df_files_audio)
    names(df_files) <- c("Datei", "Herkunft")
    df_files <<- df_files
    output$FileTable <- renderDataTable(df_files,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
  })
  
  observeEvent(input$transformButton, {
    try(source_python("skript_wav_png_transform.py"))
    path_of_file <- strsplit(as.character(df_files$Herkunft), "www/")
    path_of_file <- path_of_file[[1]][2]
    files <- list.files("www/wav_transform/audio")
    for(file in files){
      unlink(paste0("www/wav_transform/audio/",file))
    }
    files <- list.files("www/wav_transform/untransformed_wavs")
    for(file in files){
      unlink(paste0("www/wav_transform/untransformed_wavs/",file))
    }
    files <- list.files(paste0("www/",path_of_file))
    for(file in files){
      if(grepl("\\.wav$", file)){
        file.copy(file, paste0("www/wav_transform/untransformed_wavs/",file))
      }

    }

    

    

    precompute_spectrograms()
    
  })
  #wav & video to app import
  observeEvent(input$FileTable_rows_selected, {
    names(df_files) <- c("Datei", "Herkunft")
    file_selected <- paste0("data/",df_files$Datei[input$FileTable_rows_selected])
    #actions for png
    if(grepl("\\png$", df_files$Datei[input$FileTable_rows_selected])){
      path_of_file <- strsplit(as.character(df_files$Herkunft[input$FileTable_rows_selected]), "www/")
      image_selected <- paste0(path_of_file[[1]][2],"/",df_files$Datei[input$FileTable_rows_selected])
      img_name = image_selected
      output$my_image <-renderUI(get_image_tag(img_name))
      output$imagetag<-renderUI(get_image_tag("tempimg.jpg")) #starting jpg file
      output$imagetag<-renderUI(get_image_tag(imgname))
    }
    
    #actions for .wav
    if(grepl("\\.wav$", df_files$Datei[input$FileTable_rows_selected])){

      wav_name = file_selected
      # output$my_audio <-renderUI(get_audio_tag("questionF.mp3"))
      output$my_audio <-renderUI(get_audio_tag(wav_name))
      output$audiotag<-renderUI(get_audio_tag("tempwav.wav")) #starting wave file
      output$audiotag<-renderUI(get_audio_tag(wavname))
      output$my_image <-renderUI(tags$h1("")) #get_image_tag("placeholder.jpg")
      df_placeholder <- data.frame("Bild" = as.character())
      output$ImageTable <- renderDataTable(df_placeholder,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
    }
    
    #actions for .mp4
    browser()
    if(grepl("\\.mp4$", df_files$Datei[input$FileTable_rows_selected])){
      path_of_file <- strsplit(as.character(df_files$Herkunft), "www/")
      #extract images & wav from mp4
      files <- list.files("www/image_dir")
      for(file in files){
        unlink(paste0("www/image_dir/",file))
      }
      av_video_images(paste0("www/",path_of_file[[input$FileTable_rows_selected]][2],"/",df_files$Datei[input$FileTable_rows_selected]), destdir = "www/image_dir", format = "png")
      
      #Define the file name that will be deleted
      unlink("www/data/Temp_Current_Video/current_video.wav")
      av_audio_convert(paste0("www/",path_of_file[[input$FileTable_rows_selected]][2],"/",df_files$Datei[input$FileTable_rows_selected]), 'current_video.wav', channels = NULL)
      file.copy("current_video.wav", "www/data/Temp_Current_Video/current_video.wav")
      images <- list.files("www/image_dir")
      files_list <- list()
      df_images <- data.frame("Image" = character())
      for(file in 1:length(images)){
        image_path <- paste0(path,"/",images[file])
        df_images[,1] <- as.character(df_images[,1])
        df_images <- rbind(df_images, images[file])
        names(df_images) <- "Bild"
      }
      wav_name = "data/Temp_Current_Video/current_video.wav"
      # output$my_audio <-renderUI(get_audio_tag("questionF.mp3"))
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
      output$ImageTable <- renderDataTable(df_images,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
    }
    
    
  })
  
  #Loading  & showing images improted from video
  observeEvent(input$ImageTable_rows_selected, {
    image_selected <- paste0("image_dir/",df_images$Bild[input$ImageTable_rows_selected])
    img_name = image_selected
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
    try(source_python("skript_video_class.py"))
    try(source_python("skript_audio_class.py"))
    path_of_file <- strsplit(as.character(df_files$Herkunft), "www/")
    audio_path <- paste0("www/",path_of_file[[1]][2],"/audio")
    video_path <- paste0("www/",path_of_file[[1]][2],"/video")
    audio_path <- gsub("/audio","",audio_path)
    video_path <- gsub("/video","",video_path)
    audio_path <- gsub("/video","",audio_path)
    video_path <- gsub("/audio","",video_path)
    audio_path <- paste0(audio_path,"/audio")
    video_path <- paste0(video_path,"/video")
    try(df_audio <- check_wav(audio_path))
    try(df_video <- check_img(video_path))
    try(df_audio <- df_audio[,c(1,2,3,5,6,7,8,9,10,4)])
    try(df_video <- df_video[,c(1,2,8,7,9,3,6,5,10,4)])
    try(names(df_video) <- c("file","classification","neutral","happy","sad","angry","fear","disgust","surprise","contempt"))
    if(!exists("df_audio")){df_audio <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                                                     "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                                     ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"contempt"=as.character())}
    if(!exists("df_video")){df_video <- data.frame("file"=as.character(),"classification"=as.character(),"neutral"=as.character(),
                                                     "happy"=as.character(),"sad"=as.character(),"angry"=as.character()
                                                     ,"fear"=as.character(),"disgust"=as.character(),"surprise"=as.character(),"contempt"=as.character())}
    output$VoiceEmotion <- renderDataTable(df_audio,options= list(scrollY = TRUE,pageLength = 5))
    output$ImageEmotion <- renderDataTable(df_video,options= list(scrollY = TRUE,pageLength = 5))
    output$emotionsummarised <- renderPlotly({
      df_audio_table <<- data.frame("neutral"=0,"happy"=0,"sad"=0,"angry"=0
                                   ,"fear"=0,"disgust"=0,"surprise"=0,"contempt"=0)
      df_video_table <<- data.frame("neutral"=0,"happy"=0,"sad"=0,"angry"=0
                                   ,"fear"=0,"disgust"=0,"surprise"=0,"contempt"=0)

      df_audio_summary <<- table(df_audio$classification)
      df_video_summary <<- table(df_video$classification)


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
      
      emotions_summary_df <<- cbind(df_audio_table, df_video_table)
      Emotions <<- c("neutral","happy","sad","angry","fear","disgust","surprise","contempt")
      Emotions_Audio_Summary <<- table(df_audio$classification)
        
      Emotions_Video_Summary <<- table(df_audio$classification)
      audio_vector <- c(df_audio_table$neutral[1],df_audio_table$happy[1],df_audio_table$sad[1],df_audio_table$angry[1],
                        df_audio_table$fear[1],df_audio_table$disgust[1],df_audio_table$surprise[1],df_audio_table$contempt[1])
      video_vector <- c(df_video_table$neutral[1],df_video_table$happy[1],df_video_table$sad[1],df_video_table$angry[1],
                        df_video_table$fear[1],df_video_table$disgust[1],df_video_table$surprise[1],df_video_table$contempt[1])
      overview_plot_data <<- data.frame(Emotions, audio_vector, video_vector)
      
      fig <- plot_ly(overview_plot_data, x = ~Emotions, y = ~audio_vector, type = 'bar', name = 'Audio')
      fig <- fig %>% add_trace(y = ~video_vector, name = 'Video')
      fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
      
      fig
      

    })
  })
  
  
  
  
  
}

shinyApp(ui, server)


