library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)
library(RHRV)
library(RHRVEasy)
library(shinyFiles)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(DT)
library(parallel)
library(shinydashboard)
library(promises)
library(future)
plan(multisession)


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
    cat("***\n")
    results <- parallelly::killNode(workers)
    cat("***\n")
    if (all(results)) {
      # Success!
      break
    }
  }
  return(all(results))
}


server <- function(input, output, session) {
  #Choose folders functions
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, 'dirchoose', session=session, root=volumes, filetypes=c('txt'))
  shinyDirChoose(input, 'folderchoose', session=session, root=volumes, filetypes=c('txt'))
  dirlist <- reactiveVal(list())


  easyAnalysis <- reactiveVal()
  workers <- NULL


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
  observeEvent(input$longId, {
    if (input$longId == "" || !is.numeric(input$longId)) {
      runjs('$("#longId").css("color", "black");')
    } else {
      if (as.numeric(input$longId) < 1 || as.numeric(input$longId) > 500) {
        runjs('$("#longId").css("color", "red");')
      } else {
        runjs('$("#longId").css("color", "black");')
      }
    }
  })
  observeEvent(input$lastId, {
    if (input$lastId == "" || !is.numeric(input$lastId)) {
      runjs('$("#lastId").css("color", "black");')
    } else {
      if (as.numeric(input$lastId) < 0 || as.numeric(input$lastId) > 100) {
        runjs('$("#lastId").css("color", "red");')
      } else {
        runjs('$("#lastId").css("color", "black");')
      }
    }
  })
  observeEvent(input$sizeId, {
    if (input$sizeId == "" || !is.numeric(input$sizeId)) {
      runjs('$("#sizeId").css("color", "black");')
    } else {
      if (as.numeric(input$sizeId) < 0 || as.numeric(input$sizeId) > 700) {
        runjs('$("#sizeId").css("color", "red");')
      } else {
        runjs('$("#sizeId").css("color", "black");')
      }
    }
  })
  observeEvent(input$intervalId, {
    if (input$intervalId == "" || !is.numeric(input$intervalId)) {
      runjs('$("#intervalId").css("color", "black");')
    } else {
      if (as.numeric(input$intervalId) < 0 || as.numeric(input$intervalId) > 100) {
        runjs('$("#intervalId").css("color", "red");')
      } else {
        runjs('$("#intervalId").css("color", "black");')
      }
    }
  })
  observeEvent(input$bandtoleranceId, {
    if (input$bandtoleranceId == "" || !is.numeric(input$bandtoleranceId)) {
      runjs('$("#bandtoleranceId").css("color", "black");')
    } else {
      if (as.numeric(input$bandtoleranceId) < 0 || as.numeric(input$bandtoleranceId) > 100) {
        runjs('$("#bandtoleranceId").css("color", "red");')
      } else {
        runjs('$("#bandtoleranceId").css("color", "black");')
      }
    }
  })
  observeEvent(input$freqhrId, {
    if (input$freqhrId == "" || !is.numeric(input$freqhrId)) {
      runjs('$("#freqhrId").css("color", "black");')
    } else {
      if (as.numeric(input$freqhrId) < 0 || as.numeric(input$freqhrId) > 100) {
        runjs('$("#freqhrId").css("color", "red");')
      } else {
        runjs('$("#freqhrId").css("color", "black");')
      }
    }
  })
  observeEvent(input$significanceId, {
    if (input$significanceId == "" || !is.numeric(input$significanceId)) {
      runjs('$("#significanceId").css("color", "black");')
    } else {
      if (as.numeric(input$significanceId) < 0 || as.numeric(input$significanceId) > 1) {
        runjs('$("#significanceId").css("color", "red");')
      } else {
        runjs('$("#significanceId").css("color", "black");')
      }
    }
  })


  easyAnalysis <- reactiveVal()
  workers <- NULL

  observeEvent(input$startButton,{
    runjs('$("#hiddendataframes").hide();')
    runjs('$("#additionalmessage").hide();')

    if(length(dirlist()) < 2){
      shinyalert(title = "Warning message", text = "Please, select two or more folders before starting the analysis.",
                 type = "warning")
    }
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
          actionButton("cancel", label = "Cancel", class="btn-danger"),
        )
      )

      directories <- isolate(dirlist())
      dirslength <<- length(directories)

      result <- future(RHRVEasy(directories, verbose = TRUE, nJobs = -1))
      workers <<- result$workers
      result <- then(result, onFulfilled=easyAnalysis, onRejected = function(reason) {
        # TODO: we should assert that the reason is that it was cancelled by the user.
        # If not, the exception should be handled
        print(reason)
      })

      result <- finally(result,
                        function(){
                          plan(sequential)
                          plan(multisession)
                          removeModal()
                          workers <<- NULL
                          toggle(id= 'hiddendataframes')
                          updateTabsetPanel(session, "tabs", selected = "Dataframes")
                        })

      output$data_table1 = renderDT({
        req(easyAnalysis())
        datatable(easyAnalysis()[]$HRVIndices) %>%
          formatRound(columns=c('SDNN', 'SDANN', 'SDNNIDX', 'pNN50', 'SDSD', 'rMSSD',
                                'IRRR', 'MADRR', 'TINN', 'HRVi', 'ULF', 'VLF', 'LF', 'HF'), digits=3)
      })
      if(dirslength == 2){
        output$data_table2 = renderDT({
          req(easyAnalysis())
          datatable(easyAnalysis()[]$stats) %>%
            formatRound(columns=c('p.value', 'adj.p.value'), digits=3)
        })
      }
      else{
        toggle(id = "additionalmessage")
        output[["data_table2"]] <- renderDT({
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

        observeEvent(input$data_table2_rows_selected, {
          rowIndex <- input$data_table2_rows_selected
          if(!is.null(easyAnalysis()[]$stats$pairwise[[rowIndex]])){
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
                "Post hoc test not performed because no significant differences were found in the omnibus test",
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
          if(!is.null(easyAnalysis()[]$stats$pairwise[[rowIndex]])){
            datatable(easyAnalysis()[]$stats$pairwise[[rowIndex]]) %>%
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

      # return null so that shiny remains responsive
      NULL
    }
  })

  # Register user interrupt
  observeEvent(input$cancel,{
    if (!is.null(workers)) {
      print("Trying to cancel")
      print(workers)
      easyKill(workers)
      print("cancelled")
    } else {
      showNotification("No jobs, there is nothing to cancel")
    }
    NULL
  })

  }
