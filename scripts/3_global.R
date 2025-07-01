library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(readr)

# — Helper para leer datos procesados desde output/
read_data <- function(filename) {
  read_csv(file.path("output", filename))
}