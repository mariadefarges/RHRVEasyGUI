library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)
library(RHRV)
library(RHRVEasy)


ui <- page_navbar(
  title = "RHRV Easy Analysis",
  selected = "HRV indices",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "Settings",
    textInput(
      inputId = "dataPath1Id",
      label = "Database Path 1",
      value = ""
    ),
    textInput(
      inputId = "dataPath2Id",
      label = "Database Path 2",
      value = ""
    ),
    actionButton(
      inputId = "startButton",
      label = "Start Analysis"
    )
  ),
  nav_panel(
    title = "HRV indices",
    DTOutput(outputId = "data_table1", width = "100%")
  ),
  nav_panel(
    title = "Statistical Analysis",
    DTOutput(outputId = "data_table2", width = "100%")
  )
)


server <- function(input, output) {

  # Function to save the text when button is clicked
  observeEvent(input$startButton, {

    DB1 = file.path(input$dataPath1Id)
    DB2 = file.path(input$dataPath1Id)

    #DB1 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/normal")
    #DB2 = file.path("/Users/mariadefarges/Desktop/TFG/RHRVEasy-master/RRData/chf")

    #the database paths are saved, then RHRVEasy analysis is perfomed
    easyAnalysis = RHRVEasy(folders = c(DB1, DB2), nJobs = -1)

    #display the tables:
    output$data_table1 = renderDT({
      datatable(easyAnalysis$HRVIndices)
    })

    output$data_table2 = renderDT({
      datatable(easyAnalysis$stats)
    })
  })

}

shinyApp(ui, server)


