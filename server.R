
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
  #Choose folders functions
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, 'dirchoose', session=session, root=volumes, filetypes=c('txt'))
  shinyDirChoose(input, 'folderchoose', session=session, root=volumes, filetypes=c('txt'))
  dirlist <- reactiveVal(list())

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

  #Negative values of the numeric inputs in red
  observeEvent(input$typeId, {
    if (input$typeId == "Fourier") {
      shinyjs::disable("bandtoleranceId")
      updateNumericInput(session, "bandtoleranceId", value = 0)
    } else {
      shinyjs::enable("bandtoleranceId")
      updateNumericInput(session, "bandtoleranceId", value = 0.01)
    }
  })

  observeEvent(input$longId, {
    if (input$longId == "" || !is.numeric(input$longId)) {
      runjs('$("#longId").css("color", "black");')
    } else {
      if (as.numeric(input$longId) < 0) {
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
      if (as.numeric(input$lastId) < 0) {
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
      if (as.numeric(input$sizeId) < 0) {
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
      if (as.numeric(input$intervalId) < 0) {
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
      if (as.numeric(input$bandtoleranceId) < 0) {
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
      if (as.numeric(input$freqhrId) < 0) {
        runjs('$("#freqhrId").css("color", "red");')
      } else {
        runjs('$("#freqhrId").css("color", "black");')
      }
    }
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


      observeEvent(input$startButton, {

        showModal(
          modalDialog(
            title = NULL,
            "Computing...",
            actionButton("stop", label = "Stop", class="btn-danger"),
          )
        )

        tryCatch({
          easyAnalysis = RHRVEasy(dirlist(), nJobs = -1, size = input$sizeId, interval = input$intervalId, freqhr = input$freqhrId,
                                  long = input$longId, last = input$lastId, minbmp = input$bmpId[1], maxbmp = input$bmpId[2],
                                  sizep = input$sizepId, shift = input$shiftId, bandtolerance = input$bandtoleranceId,
                                  ULFmin = input$ULFId[1], ULFmax = input$ULFId[2], VLFmin = input$VLFId[1], VLFmax = input$VLFId[2],
                                  LFmin = input$LFId[1], LFmax = input$LFId[2], HFmin = input$HFId[1], HFmax = input$HFId[2],
                                  typeAnalysis = typeanalysis, verbose = TRUE, nonLinear = input$nonlinear, doRQA = input$RQA,
                                  correctionMethod = correctionMethod, significance = input$significanceId)

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
          #})


          #print(output)
          #output$consoletext <- renderText({
          #  return(output)
          #})
        },
        error = function(e) {
          cat(
            removeModal(),
            shinyalert(title = "Error message", text = e$message, type = "error")
          )
        })
      })



      observeEvent(input$stop, {
        reactivevals$easyAnalysis <- list()
        if (!is.null(reactivevals$process)) {
          tools::pskill(reactivevals$process$pid)
          reactivevals$msg <- sprintf("%1$s killed", reactivevals$process$pid)
          reactivevals$process <- NULL

          if (!is.null(reactivevals$obs)) {
            reactivevals$obs$destroy()
          }
          removeModal()
        }
      })
    } }) }
