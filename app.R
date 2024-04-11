library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)
library(RHRV)
library(RHRVEasy)
library(shinyFiles)
#library(shinythemes)
library(shinyjs)
library(shinyalert)


ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    title = "RHRV Easy Analysis",
    tabsetPanel(id = "tabs",
                tabPanel("Settings", fluid = TRUE,
                         sidebarLayout(
                           sidebarPanel(
                             actionButton(inputId = "startButton", label = "Start Analysis", class="btn-lg"),
                             hr(),
                             h4("Choose files"),
                             shinyDirButton('dirchoose','Choose...','Choose a folder'),
                             actionButton("addfolderButton", "Add folder", class = "btn btn-primary"), #class = estilo y color
                             h5("Selected directories: "),
                             textOutput("selectedFolders"),
                             actionButton(inputId = "restartButton", label = "Restart folder selection")
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Time domain", fluid = TRUE,
                                        br(),
                                        selectInput(inputId = "typeId", label = "Calculation type", choices = c("Wavelet", "Fourier"), selected = "Fourier"),
                                        numericInput(inputId = "sizeId", label = "Window size", min = 100, max = 700, value = 300),
                                        numericInput(inputId = "intervalId", label = "Histogram width for triangular interpolation", min = 5, max = 10, value = 7.8125),
                                        sliderInput("bmpId", "Allowable Heart Rate range", min = 20, max = 220, value = c(25,180)),
                               ),
                               tabPanel("Frequency domain", fluid = TRUE,
                                        br(),
                                        numericInput(inputId = "freqhrId", label = "Interpolation frequency", min = 0.05, max = 40, value = 4),
                                        numericInput(inputId = "longId", label = "Normal Resting Heart Rate", min = 30, max = 130, value = 50),
                                        numericInput(inputId = "lastId", label = "Heart Rate Variability 10%", min = 1, max = 24, value = 10)
                               ),
                               tabPanel("Power Bands", fluid = TRUE,
                                        br(),
                                        fluidRow(
                                          column(6,
                                                 #indexfreqanalysis
                                                 numericInput(inputId = "sizepId", label = "STFT points", min = 1000, max = 3000, value = 2048),
                                                 numericInput(inputId = "shiftId", label = "Window shift", min = 10, max = 100, value = 30, step = 10),
                                                 numericInput(inputId = "bandtoleranceId", label = "Band Tolerance", min = 0, max = 100, value = 0.01),
                                          ),
                                          column(6,
                                                 sliderInput(inputId = "ULFId", label = "Ultra Low Frequency Band range", min = 0, max = 1, value = c(0,0.03)),
                                                 sliderInput(inputId = "VLFId", label = "Very Low Frequency Band range", min = 0, max = 1, value = c(0.03,0.05)),
                                                 sliderInput(inputId = "LFId", label = "Low Frequency Band range", min = 0, max = 1, value = c(0.05, 0.15)),
                                                 sliderInput(inputId = "HFId", label = "High Frequency Band range", min = 0, max = 1, value = c(0.15,0.4))
                                          )
                                        )
                               )
                             )
                           )
                         ),
                ),
                tabPanel(
                  "Dataframes",
                  fluid = TRUE,
                  hidden(
                    div(id = "hiddendataframes",
                        h4("Save to an Excel Spreadsheet"),
                        shinyDirButton('folderchoose','Select...','Select the folder where the file will be saved'),
                        actionButton(inputId = "saveexcelButton", label = "Save", class = "btn btn-primary"),
                        nav_panel(title = "HRV indices",
                                  DTOutput(outputId = "data_table1", width = "100%")
                        ),
                        hr(),
                        nav_panel(title = "Statistical Analysis",
                                  DTOutput(outputId = "data_table2", width = "100%")
                        )
                    )
                  )
                )
    )
  )
)


server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, 'dirchoose', session=session, root=volumes, filetypes=c('txt'))
  shinyDirChoose(input, 'folderchoose', session=session, root=volumes, filetypes=c('txt'))
  dirlist <- reactiveVal(list())
  observeEvent(input$addfolderButton, {
    directories <- file.path((parseDirPath(volumes, input$dirchoose)))
    updateddir <- c(dirlist(), directories)
    dirlist(updateddir)
  })

  output$selectedFolders <- renderPrint({
    if (input$addfolderButton>0) {
      print(dirlist())
    } else {
      return("Server is ready for calculation.")
    }
  })
  observeEvent(input$restartButton, {
    dirlist(list())
  })

  observeEvent(input$startButton, {
    if(length(dirlist()) < 2){
      shinyalert(title = "Warning message", text = "Please, select two or more folders before starting the analysis.",
                 type = "warning")
    }
    else{
      toggle(id = "hiddendataframes")
      updateTabsetPanel(session, "tabs", selected = "Dataframes")
      if (input$typeId == "Fourier")
        typeanalysis = "fourier"
      if(input$typeId == "Wavelet")
        typeanalysis = "wavelet"

      tryCatch({
        easyAnalysis = RHRVEasy(dirlist(), nJobs = -1, size = input$sizeId, interval = input$intervalId, freqhr = input$freqhrId,
                                long = input$longId, last = input$lastId, minbmp = input$bmpId[1], maxbmp = input$bmpId[2],
                                sizep = input$sizepId, shift = input$shiftId, bandtolerance = input$bandtoleranceId,
                                ULFmin = input$ULFId[1], ULFmax = input$ULFId[2], VLFmin = input$VLFId[1], VLFmax = input$VLFId[2],
                                LFmin = input$LFId[1], LFmax = input$LFId[2], HFmin = input$HFId[1], HFmax = input$HFId[2],
                                typeAnalysis = typeanalysis)
      },
      error = function(e) {
        cat(
          shinyalert(title = "Error message", text = e$message, type = "error")
          )
      })

      #Display the tables  c(DB1, DB2, DB3, DB4)
      output$data_table1 = renderDT({
        datatable(easyAnalysis$HRVIndices)
      })

      output$data_table2 = renderDT({
        if (length(dirlist()) > 2)
          datatable(easyAnalysis$stats$pairwise[[1]])
        else
          datatable(easyAnalysis$stats)
      })

      observeEvent(input$saveexcelButton, {
        path <- file.path((parseDirPath(volumes, input$folderchoose)))
        saveHRVIndices(easyAnalysis,saveHRVIndicesInPath = path)
        shinyalert(title = "Success message", text = "The excel spreadsheet has been saved successfully",
                   type = "success")
      })
    }
  })



}

shinyApp(ui, server)

#DB1 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/normal")
#DB2 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/chf")
#DB3 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/normal_half")
#DB4 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/chf_half")

#shinyuieditor::launch_editor(app_loc = "/Users/mariadefarges/Desktop/TFG/RHRVEasyUI/app.R")
