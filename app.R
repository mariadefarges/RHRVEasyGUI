library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)
library(RHRV)
library(RHRVEasy)
#library(shinythemes)

ui <- fluidPage(navbarPage(
  title = "RHRV Easy Analysis",
  tabPanel("Settings", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               titlePanel("Choose files"),
               fileInput("files", "Seleccionar carpetas", multiple = TRUE, accept = NULL),
               actionButton(inputId = "startButton", label = "Start Analysis")
             ),
             mainPanel(titlePanel("Time domain"),
                       sliderInput(inputId = "sizeId", label = "Window size", min = 100, max = 700, value = 300, step = 50),
                       sliderInput(inputId = "intervalId", label = "Histogram width for triangular interpolation", min = 5, max = 10, value = 7.8125),
                       hr(),

                       titlePanel("Frequency domain"),
                       sliderInput(inputId = "freqhrId", label = "Interpolation frequency", min = 0.05, max = 40, value = 4),
                       sliderInput(inputId = "longId", label = "Normal Resting Heart Rate", min = 30, max = 130, value = 50, step = 10),
                       sliderInput(inputId = "lastId", label = "Heart Rate Variability 10%", min = 1, max = 24, value = 10, step = 1),
                       sliderInput(inputId = "minbmpId", label = "Minimum allowable Heart Rate", min = 20, max = 60, value = 25, step = 5),
                       sliderInput(inputId = "maxbmpId", label = "Maximum allowable Heart Rate", min = 100, max = 220, value = 180, step = 5),
                       hr(),

                       titlePanel("Power Bands"),
                       #indexfreqanalysis
                       sliderInput(inputId = "sizepId", label = "STFT points", min = 1000, max = 3000, value = 2048),
                       sliderInput(inputId = "shiftId", label = "Window shift", min = 10, max = 100, value = 30, step = 10),
                       sliderInput(inputId = "bandtoleranceId", label = "Band Tolerance", min = 0, max = 100, value = 0.01),
                       #selectInput(inputId = "typeId", label = "Calculation tpe", choices = c("Wavelet", "Fourier"), selected = "Fourier"),
                       numericInput(inputId = "ULFminId", label = "Minimum Ultra Low Frequency Band", min = 0, max = 1, value = 0),
                       numericInput(inputId = "ULFmaxId", label = "Maximum Ultra Low Frequency Band", min = 0, max = 1, value = 0.03),
                       numericInput(inputId = "VLFminId", label = "Minimum Very Low Frequency Band", min = 0, max = 1, value = 0.03),
                       numericInput(inputId = "VLFmaxId", label = "Maximum Very Low Frequency Band", min = 0, max = 1, value = 0.05),
                       numericInput(inputId = "LFminId", label = "Minimum Low Frequency Band", min = 0, max = 1, value = 0.05),
                       numericInput(inputId = "LFmaxId", label = "Maximum Low Frequency Band", min = 0, max = 1, value = 0.15),
                       numericInput(inputId = "HFminId", label = "Minimum High Frequency Band", min = 0, max = 1, value = 0.15),
                       numericInput(inputId = "HFmaxId", label = "Maximum High Frequency Band", min = 0, max = 1, value = 0.4),
             )
           )
  ),
  tabPanel(
    "Dataframes",
    fluid = TRUE,
    nav_panel(title = "HRV indices",
              actionButton(inputId = "excelButton", label = "Save to an Excel Spreadsheet"),
              DTOutput(outputId = "data_table1", width = "100%")),
    nav_panel(title = "Statistical Analysis",
              DTOutput(outputId = "data_table2", width = "100%"))
  )
))

server <- function(input, output) {

  #Select files and save their paths in a list
  pathslist <- reactiveVal(list())

  observeEvent(input$files, {
    paths <- file.path(unique(dirname(input$files$datapath)))

    updatedpaths <- c(pathslist(), paths)
    pathslist(updatedpaths)

  })

  observeEvent(input$startButton, {
    DB1 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/normal")
    DB2 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/chf")
    DB3 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/normal_half")
    DB4 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/chf_half")

    easyAnalysis = RHRVEasy(c(DB1, DB2, DB3, DB4), nJobs = -1, size = input$sizeId, interval = input$intervalId, freqhr = input$freqhrId,
                            long = input$longId, last = input$lastId, minbmp = input$minbmpId, maxbmp = input$maxbmpId,
                            sizep = input$sizepId, shift = input$shiftId, bandtolerance = input$bandtoleranceId,
                            ULFmin = input$ULFminId, ULFmax = input$ULFmaxId, VLFmin = input$VLFminId, VLFmax = input$VLFmaxId,
                            LFmin = input$LFminId, LFmax = input$LFmaxId, HFmin = input$HFminId, HFmax = input$HFmaxId)

    #Display the tables
    output$data_table1 = renderDT({
      datatable(easyAnalysis$HRVIndices)
    })

    output$data_table2 = renderDT({
      datatable(easyAnalysis$stats$pairwise[[1]])
    })

    observeEvent(input$excelButton, {
      path <- getwd()
      saveHRVIndices(easyAnalysis,saveHRVIndicesInPath = path)
    })

  })



}

shinyApp(ui, server)
#shinyuieditor::launch_editor(app_loc = "/Users/mariadefarges/Desktop/TFG/RHRVEasyUI/app.R")
