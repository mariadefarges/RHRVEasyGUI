

ui <- fluidPage(theme = shinytheme("flatly"),
                useShinyjs(),
                tags$head(
                  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
                  tags$style(HTML("
                  .btn.btn-default {
                  padding: 6px 9px;
                  }
                "))
                ),
                navbarPage(
                  title = "HRV Analysis",
                  tabsetPanel(id = "tabs",
                              tabPanel("Settings", fluid = TRUE,
                                       sidebarLayout(
                                         sidebarPanel(
                                           width = 6,
                                           column(width = 12, align = "center",
                                                  actionButton(inputId = "startButton", label = "START ANALYSIS", class="btn-info"),
                                                  tags$hr(style = "border: none; background-color: transparent; margin-top: 10px; margin-bottom: 10px;"),
                                           ),

                                           fluidRow(
                                             column(6,

                                                    selectInput(inputId = "correctionMethodId", label = "Correction method", choices = c("Bonferroni", "Holm", "Hochberg", "Hommel",
                                                                                                                                         "Benjamini & Hochberg", "False discovery rate",
                                                                                                                                         "Benjamini & Yekutieli", "none"), selected = "Bonferroni"),
                                                    numericInput(inputId = "significanceId", label = "Significance level", min = 0, max = 1, value = 0.05),
                                             ),

                                             column(6,
                                                    h6("WARNING: Please note that this process will take several hours to complete"),
                                                    checkboxInput("nonlinear", tags$span("Non Linear analysis", style = "font-size: 14px;"), value = FALSE),
                                                    checkboxInput("RQA", tags$span("RQA", style = "font-size: 15px;"), value = FALSE),
                                             ),
                                           ),

                                           h5("Select folders:"),
                                           shinyDirButton('dirchoose','Choose...','Choose a folder from your home unit'),
                                           actionButton("addfolderButton", "Add folder", class = "btn btn-primary"), #class = estilo y color
                                           actionButton(inputId = "restartButton", label = "", icon = icon("redo"), style = "font-size: 13px;"),
                                           tags$hr(style = "border: none; background-color: transparent; margin-top: 10px; margin-bottom: 10px;"),
                                           uiOutput("selectedFolders", style = "font-size: 15px;"),

                                         ),

                                         mainPanel(
                                           width = 6,
                                           fluidRow(
                                             tabsetPanel(
                                               tabPanel("Beat filtering", fluid = TRUE,
                                                        br(),
                                                        sliderInput("bmpId", "Allowable Heart Rate range", min = 20, max = 220, value = c(25,180)),
                                                        numericInput(inputId = "longId", label = "Normal Resting Heart Rate", min = 30, max = 130, value = 50),
                                                        numericInput(inputId = "lastId", label = "Maximum % of Heart Rate change ", min = 1, max = 24, value = 10)
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
                                                                 numericInput(inputId = "bandtoleranceId", label = "Band Tolerance", min = 0, max = 100, value = 0.01, step = 0.01),
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
                                             ),
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
                                                                   h5("Click on each of the rows to see the Post Hoc tests"),
                                                               ),
                                                               tags$head(
                                                                 tags$style(HTML("
                                                      .dataTable tbody tr:hover {
                                                      cursor: pointer;
                                                      }
                                                      "))
                                                               ),
                                                               DTOutput(outputId = "data_table2", width = "100%")

                                                     )
                                                 ),
                                        ),
                                      )
                                  )
                                )
                              )
                  )
                ),
)
