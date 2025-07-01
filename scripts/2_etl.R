# — Paquetes comunes
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(readr)
library(here)

# — Helper para leer datos procesados
read_data <- function(filename) {
  read_csv(here("data", "processed", filename))
}