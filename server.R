
library(shiny)
library(RHRVEasy)
library(shinyFiles)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(ggplot2)
library(bslib)
library(gridlayout)
library(RHRV)
library(parallel)
library(shinydashboard)
library(promises)
library(future)
library(DT)
plan(multisession)


source("ui.R")

#Sys.setlocale("LC_NUMERIC", "en_US.UTF-8")

js <- function(id){
  c(
    "table.on('click', 'td', function(){",
    "  var colIndex = $(this).index();",
    "  if (colIndex === 3) {",
    sprintf("    Shiny.setInputValue('%s', true, {priority: 'event'});", id),
    "  }",
    "});"
  )}

easyKill <- function(workers) {
  max_n_attemps <- 5
  for (n_attemp in seq_len(max_n_attemps)) {
    # results is a vector of booleans (one boolean
    # per process). True if kill was succesful
    results <- parallelly::killNode(workers)
    resetWorkers(workers)
    #cat("***\n")
    if (all(results)) {
      # Success!
      break
    }
  }
  return(all(results))
}




server <- function(input, output, session) {

  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, 'dirchoose', session=session, root=volumes, filetypes=c('txt'))
  shinyDirChoose(input, 'folderchoose', session=session, root=volumes, filetypes=c('txt'))

  dirlist <- reactiveVal(list())
  fileCount <- reactiveVal(list())
  easyAnalysis <- reactiveVal()
  workers <- NULL
  dirslength <- reactiveVal(0)
  N <- reactiveVal(0)


  count_files <- function(directory) {
    length(list.files(directory))
  }

  #Add the folders paths to a reactive list
  observeEvent(input$addfolderButton, {
    directories <- file.path((parseDirPath(volumes, input$dirchoose)))
    updateddir <- c(dirlist(), directories)
    dirlist(updateddir)
    counts <- sapply(updateddir, count_files)
    counts = fileCount(counts)
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

  #Restart the folder selection
  observeEvent(input$restartButton, {
    dirlist(list())
  })

  #Disable the band tolerance parameter for Fourier analysis type
  observeEvent(input$nonlinear, {
    if (!input$nonlinear) {
      shinyjs::disable("RQA")
    } else {
      shinyjs::enable("RQA")
    }
  })

  observeEvent(input$typeId, {
    if (input$typeId == "Fourier") {
      shinyjs::disable("bandtoleranceId")
      updateNumericInput(session, "bandtoleranceId", value = 0)
    } else {
      shinyjs::enable("bandtoleranceId")
      updateNumericInput(session, "bandtoleranceId", value = 0.01)
    }
  })

  #Negative values of the numeric inputs in red
  validateNegatives <- function(inputId, min, max) {
    observeEvent(input[[inputId]], {
      if (is.null(input[[inputId]]) || !is.numeric(input[[inputId]])) {
        runjs(sprintf('$("#%s").css("color", "black");', inputId))
      } else {
        if (input[[inputId]] < min || input[[inputId]] > max) {
          runjs(sprintf('$("#%s").css("color", "red");', inputId))
        } else {
          runjs(sprintf('$("#%s").css("color", "black");', inputId))
        }
      }
    })
  }

  validateNegatives("longId", 1, 500)
  validateNegatives("lastId", 0, 100)
  validateNegatives("sizeId", 0, 700)
  validateNegatives("intervalId", 0, 100)
  validateNegatives("bandtoleranceId", 0, 100)
  validateNegatives("freqhrId", 0, 100)
  validateNegatives("significanceId", 0, 1)




  easyAnalysis <- reactiveVal()
  workers <- NULL

  observeEvent(input$startButton,{

    log_file <- "temp.txt"
    if (file.exists(log_file)) {
      file.remove(log_file)
    }


    runjs('$("#hiddendataframes").hide();')
    runjs('$("#additionalmessage").hide();')



    #If a non linear analysis is performed, 3 analysis (time, freq, nonlinear) for each file, if else 2 analysis (time, freq)
    if (input$nonlinear) {
      N(3 * sum(unlist(fileCount())))
    } else {
      N(2 * sum(unlist(fileCount())))
    }


    #At least 2 populations are needed to make the analysis
    if(length(dirlist()) < 2){
      shinyalert(title = "Warning message", text = "Please, select two or more folders before starting the analysis.",
                 type = "warning")
    }
    #Adjusting the select inputs
    else{
      if (input$typeId == "Fourier")
        typeanalysis = "fourier"
      if(input$typeId == "Wavelet")
        typeanalysis = "wavelet"

      if (input$correctionMethodId == "Bonferroni")
        correctionMethod = "bonferroni"
      if (input$correctionMethodId == "Holm")
        correctionMethod = "holm"
      if (input$correctionMethodId == "Hochberg")
        correctionMethod = "hochberg"
      if (input$correctionMethodId == "Hommel")
        correctionMethod = "hommel"
      if (input$correctionMethodId == "Benjamini & Hochberg")
        correctionMethod = "BH"
      if (input$correctionMethodId == "Benjamini & Yekutieli")
        correctionMethod = "BY"
      if (input$correctionMethodId == "False discovery rate")
        correctionMethod = "fdr"
      if (input$correctionMethodId == "none")
        correctionMethod = "none"



      showModal(
        modalDialog(
          title = NULL,
          "Computing...",
          footer = tagList(
            actionButton("cancel", label = "Cancel", class="btn-danger"),
          ),
          tags$style(HTML("
          .modal-content .progress {
          height: 18px; /* changes the bar thickness */
          }
          .modal-content .progress-bar {
          font-size: 14px; /* changes the bar percent font size */
          line-height: 19px; /* changes the vertical line height */
          }
          ")),
          progressBar(id = "pb", value = 0, total = N(), display_pct = TRUE),
          easyClose = FALSE,
        )
      )



      #shiny variables do not work inside a future
      directories <- isolate(dirlist())
      dirslength(length(directories))
      size = input$sizeId
      interval = input$intervalId
      freqhr = input$freqhrId
      long = input$longId
      last = input$lastId
      minbmp = input$bmpId[1]
      maxbmp = input$bmpId[2]
      bandtolerance = input$bandtoleranceId
      ULFmin = input$ULFId[1]
      ULFmax = input$ULFId[2]
      VLFmin = input$VLFId[1]
      VLFmax = input$VLFId[2]
      LFmin = input$LFId[1]
      LFmax = input$LFId[2]
      HFmin = input$HFId[1]
      HFmax = input$HFId[2]
      nonLinear = input$nonlinear
      doRQA = input$RQA
      significance = input$significanceId


      result <- future(
          RHRVEasy(directories, verbose = TRUE, nJobs = -1, size = size, interval = interval, freqhr = freqhr,
                   long = long, last = last, minbmp = minbmp, maxbmp = maxbmp, bandtolerance = bandtolerance,
                   ULFmin= ULFmin, ULFmax = ULFmax, VLFmin = VLFmin, VLFmax = VLFmax, LFmin = LFmin, LFmax = LFmax,
                   HFmin = HFmin, HFmax = HFmax, typeAnalysis = typeanalysis, nonLinear = nonLinear, doRQA = doRQA,
                   correctionMethod = correctionMethod, significance = significance, clusterLogFile = log_file)
        )
      #<simpleError in buildEasyOptions(verbose = verbose, significance = significance,     method = correctionMethod): (significance > 0) && (significance < 1) is not TRUE>

      workers <<- result$workers

      #Read the number of "analysis" lines written in the temporary file
      progress <- reactivePoll(100, session,
                               checkFunc = function() {
                                 if (file.exists(log_file)) {
                                   file.info(log_file)$mtime[1]
                                 } else {
                                   ""
                                 }
                               },
                               valueFunc = function() {
                                 if (!file.exists(log_file)) {
                                   return(0)
                                 }
                                 text <- readLines(log_file)
                                 analysis_lines <- grep("analysis", text)
                                 lines <- length(analysis_lines)
                                 lines
                               }
      )


      #Update the progress bar
      observe({
        req(progress())
        req(N())
        updateProgressBar(session, "pb", value = progress(), total = N())
        if (progress() >= N()) {
          removeModal()
        }
      })

      #Errors captured
      result <- then(result, onFulfilled=easyAnalysis,
                     onRejected = function() {
                       toggle(id = "hiddendataframes")
                       shinyalert(title = "Error message", text = "The analysis could not be performed", type = "error")

                     })

      result <- finally(result,
                        function(){
                          plan(sequential)
                          plan(multisession)
                          removeModal()
                          if (file.exists(log_file)) {
                            file.remove(log_file)
                          }
                          toggle(id = "hiddendataframes")
                          updateTabsetPanel(session, "tabs", selected = "Data tables")
                          workers <<- NULL
                        })



      #Cancel the long duration operation
      observeEvent(input$cancel,{
        if (!is.null(workers)) {
          easyKill(workers)
          workers <<- NULL
          removeModal()
        } else {
          showNotification("No jobs, there is nothing to cancel")
        }
        NULL
      })
      NULL
    }

  })


  output$data_table1 = renderDataTable({
        req(easyAnalysis())
        datatable(easyAnalysis()[]$HRVIndices) %>%
          formatRound(columns=c('SDNN', 'SDANN', 'SDNNIDX', 'pNN50', 'SDSD', 'rMSSD',
                                'IRRR', 'MADRR', 'TINN', 'HRVi', 'ULF', 'VLF', 'LF', 'HF'), digits=3)
      })


  observe({
    req(dirslength())
    #For the case of 2 populations, post hoc tests are not performed
    if(dirslength() == 2){
      output$data_table2 = renderDataTable({
        req(easyAnalysis())
        datatable(easyAnalysis()[]$stats) %>%
          formatRound(columns=c('p.value', 'adj.p.value'), digits=3)
      })
    }

    else{
      toggle(id = "additionalmessage")
      output[["data_table2"]] <- renderDataTable({
        req(easyAnalysis())
        datatable(selection = "single",
                  easyAnalysis()[]$stats,
                  options = list(
                    columnDefs = list(list(visible = FALSE, targets = c(5)))
                  ),
                  callback = JS(js("data_table2"))
        ) %>%
          formatRound(columns=c('p.value', 'adj.p.value'), digits=3)
      })

      #Rows can be clicked to see the post hoc tests
      observeEvent(input$data_table2_rows_selected, {
        rowIndex <- input$data_table2_rows_selected
        if(!is.null(easyAnalysis()[]$stats$pairwise[[rowIndex]])){
          showModal(
            modalDialog(
              title = "Post Hoc test",
              dataTableOutput("pairwisetable"),
              size = "l",
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
              "Post hoc test not performed because no significant differences were found in the omnibus test",
              footer = tagList(
                modalButton("Close")
              ),
              easyClose = FALSE,
            )
          )
        }

      })

      output$pairwisetable <- renderDataTable({
        rowIndex <- input$data_table2_rows_selected
        if(!is.null(easyAnalysis()[]$stats$pairwise[[rowIndex]])){
          datatable(easyAnalysis()[]$stats$pairwise[[rowIndex]]) %>%
            formatRound(columns=c('p.value', 'adj.p.value'), digits=3)
        }
      })

      observeEvent(input[["Close"]], {
        removeModal()
      })
    }
  })



      observeEvent(input$saveexcelButton, {
        tryCatch({
          path <- file.path((parseDirPath(volumes, input$folderchoose)))
          saveHRVIndices(easyAnalysis(), saveHRVIndicesInPath = path)
          shinyalert(title = "Success message", text = "The excel spreadsheet has been saved successfully",
                     type = "success")
        }, error = function(e) {
          shinyalert(title = "Error", text = paste("Failed to save the file:", conditionMessage(e)), type = "error")
        })
      })

  }
