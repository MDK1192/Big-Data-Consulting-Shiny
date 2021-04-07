library(shiny)
library(shinydashboard)
library(imager)
library(DT)
library(reticulate)
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
      menuItem("Results", tabName = "results", icon = icon("th"))
    )
  ),
  #Body Content
  dashboardBody(
    tabItems(
      tabItem(tabName = "landing",
              h2("Landingpage"),
              box(width = 12,
                  box(width = 4,  radioButtons("genderChoice", label = h3("Radio buttons"),
                                                choices = list("Male" = 1, "Female" = 2), 
                                                selected = 1)),
                  box(width = 8,
                      actionButton("loadButton", label = "Load Data", width = '100%'),
                      tags$br(),
                      tags$br(),
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
              box(width = 12, plotlyOutput("emotionovertime"),title = "Ergebnisse grafisch"),
              box(width = 12, plotlyOutput("emotionsummarised"),title = "Ergebnisse grafisch"),
              box(width = 12,DTOutput("ImageTable2"),title = "Bilderuebersicht tabellarisch"),
              box(width = 12,plotOutput("Image")
              )
              
              
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
  output$emotionsummarised <- renderPlotly({
    
    Animals <- c("Anger", "Happy", "Disgust", "Trust", "Annoyed", "Sad", "Hungry","Hangry")
    SF_Zoo <- c(20, 14, 23,14, 16, 23,18, 11)
    LA_Zoo <- c(12, 18, 17,20, 10, 21,21, 12)
    data <- data.frame(Animals, SF_Zoo, LA_Zoo)
    
    fig <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'Audio')
    fig <- fig %>% add_trace(y = ~LA_Zoo, name = 'Video')
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    fig
  
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
    #list & load files
    files <- list.files(path)
    files_list <- list()
    df_files <- data.frame("File" = character())
    for(file in 1:length(files)){
      file_path <- paste0(path,"/",files[file])
      df_files[,1] <- as.character(df_files[,1])
      df_files <- rbind(df_files, files[file])
      names(df_files) <- "Datei"
    }

    df_files <<- df_files
    output$FileTable <- renderDataTable(df_files,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
  })
  
  #wav & video to app import
  observeEvent(input$FileTable_rows_selected, {
    file_selected <- paste0("data/",df_files$Datei[input$FileTable_rows_selected])
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


    #actions for video
    if(grepl("\\.mp4$", df_files$Datei[input$FileTable_rows_selected])){

      #extract images & wav from mp4
      av_video_images(paste0("www/data/",df_files$Datei[input$FileTable_rows_selected]), destdir = "www/image_dir", format = "jpg", fps = 5)
      #Define the file name that will be deleted
      unlink("www/data/Temp_Current_Video")
      dir.create("www/data/Temp_Current_Video")
      if (file.exists("current_video.wav")) {file.remove("current_video.wav")}
      
      av_audio_convert(paste0("www/data/",df_files$Datei[input$FileTable_rows_selected]), 'current_video.wav', channels = NULL)
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
      #export everything to global env.
      allglobal <- function() {
        lss <- ls(envir = parent.frame())
        for (i in lss) {
          assign(i, get(i, envir = parent.frame()), envir = .GlobalEnv)
        }
      }
      allglobal()
      output$ImageTable <- renderDataTable(df_images,selection=list(mode="single"),options= list(scrollY = TRUE,pageLength = 5))
      file_selected <- "data/Temp_Current_Video/current_video.wav"
      wav_name = file_selected
      # output$my_audio <-renderUI(get_audio_tag("questionF.mp3"))
      output$my_audio <-renderUI(get_audio_tag(wav_name))
      output$audiotag<-renderUI(get_audio_tag("tempwav.wav")) #starting wave file    
      output$audiotag<-renderUI(get_audio_tag(wavname))
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
  
  
  
  
  

  
  
  
  
  
  
  
  observeEvent(input$VGGstartButton, {
    source_python("model_skript_vgg.py")
    df_CAD <- check_images("C:/Users/Marc/Documents/GitHub/CAD_Pneumony_Detection")
    df_CAD$classification <- as.numeric(df_CAD$classification)
    df_CAD$classification <- ifelse(df_CAD$classification == 0, "Anomaly", "Normal")
    df_CAD$percentages_0 <- round(as.numeric(df_CAD$percentages_0) * 100, 2)
    df_CAD$percentages_1 <- round(as.numeric(df_CAD$percentages_1) * 100, 2)
    names(df_CAD) <- c("Bild", "Diagnose CAD", "Ergebnis Anomaly", "Ergebnis Normal")
    df_CAD <- left_join(df_images, df_CAD, by="Bild")
    df_CAD$Ergebnis <- ifelse(df_CAD$`Diagnose Mensch`==df_CAD$`Diagnose CAD`, "identisch", "unterschiedlich")
    output$CADTable <- renderDataTable(df_CAD,options= list(scrollY = TRUE,pageLength = 5))
    output$ImageTable2 <- renderDataTable(df_CAD,options= list(scrollY = TRUE,pageLength = 5))
    
  })
  
}

shinyApp(ui, server)
