#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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



#Sys.setlocale("LC_NUMERIC", "en_US.UTF-8")


source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)





#tabPanel(
#  "Heart beat analysis", fluid = TRUE,
# fluidRow(
#   column(6,
#          h4("Heart beat recordings HRV analysis"),
#          fileInput("filechoose","Choose a file"),
#           selectInput(inputId = "fileformat", label = "File format", choices = c("ASCII", "Polar", "EDF", "Suunto", "WFDB"), selected = "ASCII"),
#           hr(),
#           checkboxInput("interpolchoose", "Interpolation"),
#          actionButton(inputId = "HBplotButton", label = "Build HR Plot"),
#         ),
#    column(6,
#           plotOutput("HBplot"),

#          ),
#     )
#  ),

#make modals resizeable
#tags$style(
#   type = 'text/css',
#  '.modal-dialog.test { width: fit-content !important; }',
#   '.modal-content { resize: both; overflow: auto; }'
#  ),
#)

# observeEvent(input$HBplotButton, {
#  #setwd("/Users/mariadefarges/Desktop/TFG/AppPrueba")
#   hrv.data = CreateHRVData()
#   hrv.data = SetVerbose(hrv.data, TRUE)
#   fileselected <- input$filechoose
#   if (is.null(fileselected))
#    return(NULL)
#  if (input$fileformat == "ASCII")
#    hrv.data = LoadBeatAscii(hrv.data, RecordName = fileselected$name, RecordPath = "/Users/mariadefarges/Desktop/TFG/AppPrueba")
#   if (input$fileformat == "Polar")
#    hrv.data = LoadBeatPolar(hrv.data, "example.beats", RecordPath = ".")
#   if (input$interpolchoose) {}
#   else{
#    hrv.data = BuildNIHR(hrv.data)
# output$HBplot <- renderPlot({
#      PlotNIHR(hrv.data)
#   })
#    hrv.data = FilterNIHR(hrv.data)
#   }
#  })


