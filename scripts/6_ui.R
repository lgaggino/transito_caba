# scripts/6_ui.R

source("scripts/3_global.R")
source("scripts/7_maps.R")

ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/styles.css")),  # ruta relativa desde raíz
  titlePanel(""),
  navbarPage(NULL,
             tabPanel("Introducción",
                      fluidRow(
                        column(12,
                               tags$div(
                                 class = "paragraph",
                                 "Bienvenido al dashboard inmobiliario de Ciudad Autónoma de Buenos Aires. 
                                 Aquí encontrarás un panorama general del mercado de compraventa y alquiler, 
                                 con mapas interactivos que muestran la distribución de precios por barrio 
                                 y superficie. ¡Explorá haciendo clic en las pestañas!"
                               )
                        )
                      )
             ),
             tabPanel("Dashboard", mapUI("Dashboard"))
  )
)
