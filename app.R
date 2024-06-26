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


source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)



