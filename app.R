#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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
library(DT)


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
                             h4("Select folders"),
                             shinyDirButton('dirchoose','Choose...','Choose a folder from your home unit'),
                             actionButton("addfolderButton", "Add folder", class = "btn btn-primary"), #class = estilo y color
                             hr(),
                             actionButton(inputId = "restartButton", label = "Restart folder selection"),
                             h5("Selected folders: "),
                             uiOutput("selectedFolders"),
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Beat filtering", fluid = TRUE,
                                        br(),
                                        sliderInput("bmpId", "Allowable Heart Rate range", min = 20, max = 220, value = c(25,180)),
                                        numericInput(inputId = "longId", label = "Normal Resting Heart Rate", min = 30, max = 130, value = 50),
                                        numericInput(inputId = "lastId", label = "Maximum % of Rate Variation Change ", min = 1, max = 24, value = 10)
                               ),
                               tabPanel("Time domain", fluid = TRUE,
                                        br(),
                                        numericInput(inputId = "sizeId", label = "Window size", min = 100, max = 700, value = 300),
                                        numericInput(inputId = "intervalId", label = "Histogram width for triangular interpolation", min = 5, max = 10, value = 7.8125),
                               ),

                               tabPanel("Power Bands", fluid = TRUE,
                                        br(),
                                        fluidRow(
                                          column(6,
                                                 #indexfreqanalysis
                                                 selectInput(inputId = "typeId", label = "Calculation type", choices = c("Wavelet", "Fourier"), selected = "Fourier"),
                                                 numericInput(inputId = "bandtoleranceId", label = "Band Tolerance", min = 0, max = 100, value = 0.01),
                                                 numericInput(inputId = "freqhrId", label = "Interpolation frequency", min = 0.05, max = 40, value = 4),
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
                        hr(),
                        tabsetPanel(
                          tabPanel("HRV indices", fluid = TRUE,
                                   nav_panel(title = "HRV indices",
                                      DTOutput(outputId = "data_table1", width = "100%")
                                      ),
                                   ),
                          tabPanel("Statistics", fluid = TRUE,
                                     div(id = "hiddenstatistics",
                                         nav_panel(title = "Statistics",
                                                   div(id = "additionalmessage",
                                                       h5("Click on each of the rows to see more information"),
                                                   ),
                                                   DTOutput(outputId = "data_table2", width = "100%"),
                                         )
                                     ),
                          ),
                        )
                    )
                  )
                )
                )
  ),
  #make modals resizeable
  tags$style(
    type = 'text/css',
    '.modal-dialog.test { width: fit-content !important; }',
    '.modal-content { resize: both; overflow: auto; }'
  ),
)

js <- function(id){
  c(
    "table.on('click', 'td', function(){",
    "  var colIndex = $(this).index();",
    "  if (colIndex === 3) {",
    sprintf("    Shiny.setInputValue('%s', true, {priority: 'event'});", id),
    "  }",
    "});"
  )}


server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, 'dirchoose', session=session, root=volumes, filetypes=c('txt'))
  shinyDirChoose(input, 'folderchoose', session=session, root=volumes, filetypes=c('txt'))
  dirlist <- reactiveVal(list())

  observeEvent(input$typeId, {
    if (input$typeId == "Fourier") {
      shinyjs::disable("bandtoleranceId")
    } else {
      shinyjs::enable("bandtoleranceId")
    }
    })


  observeEvent(input$addfolderButton, {
    directories <- file.path((parseDirPath(volumes, input$dirchoose)))
    updateddir <- c(dirlist(), directories)
    dirlist(updateddir)
  })

  #Visualize the name of the folders selected
  output$selectedFolders <- renderUI({
    if (input$addfolderButton>0) {
      foldersnames <- sapply(dirlist(), basename)
      lapply(foldersnames, function(x) {
        div(
          class = "icon",
          icon("folder"),
          x
        )
      })
    }
    else {
      return("Server is ready for calculation.")
    }
  })
  observeEvent(input$restartButton, {
    dirlist(list())
  })

  observeEvent(input$startButton, {
    runjs('$("#hiddendataframes").hide();')
    runjs('$("#hiddenstatistics").hide();')
    runjs('$("#additionalmessage").hide();')

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

      showModal(
        modalDialog( title = NULL, "Computing...")
      )

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
      removeModal()

      runjs('$("#hiddendataframes").show();')

      #Display the tables
      output$data_table1 = renderDT({
        datatable(easyAnalysis$HRVIndices) %>%
          formatRound(columns=c('SDNN', 'SDANN', 'SDNNIDX', 'pNN50', 'SDSD', 'rMSSD',
                                'IRRR', 'MADRR', 'TINN', 'HRVi', 'ULF', 'VLF', 'LF', 'HF'), digits=3)
      })

      toggle(id = "hiddenstatistics")
      if(length(dirlist())==2){
        output$data_table2 = renderDT({
          datatable(easyAnalysis$stats) %>%
            formatRound(columns=c('p.value', 'adj.p.value'), digits=3)
        })
      }
      else{
        toggle(id = "additionalmessage")
        output[["data_table2"]] <- renderDT({
          datatable(selection = "single",
                    easyAnalysis$stats,
                    #column pairwise is not visible
                    options = list(
                      columnDefs = list(list(visible = FALSE, targets = c(5)))
                      ),
                    callback = JS(js("data_table2"))
                    ) %>%
            formatRound(columns=c('p.value', 'adj.p.value'), digits=3)
        })

        observeEvent(input$data_table2_rows_selected, {
          rowIndex <- input$data_table2_rows_selected
          if(!is.null(easyAnalysis$stats$pairwise[[rowIndex]])){
            showModal(
              modalDialog(
                title = "Post Hoc test",
                size = "xl",
                DTOutput("pairwisetable"),
                easyClose = FALSE,
                footer = tagList(
                  modalButton("Close")
                ),
              )
            )
          }
          else{
            showModal(
              modalDialog(
                title = "Post Hoc test",
                "There is no significant information",
                easyClose = FALSE,
                footer = tagList(
                modalButton("Close")
                )
              )
            )
          }

        })

        output$pairwisetable <- renderDT({
          rowIndex <- input$data_table2_rows_selected
            if(!is.null(easyAnalysis$stats$pairwise[[rowIndex]])){
              datatable(easyAnalysis$stats$pairwise[[rowIndex]]) %>%
                formatRound(columns=c('p.value', 'adj.p.value'), digits=3)
            }
        })

        observeEvent(input[["Close"]], {
          removeModal()
        })
      }

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

