# scripts/5_server.R

source("scripts/3_global.R")
source("scripts/7_maps.R")

server <- function(input, output, session) {
  datos_precio_venta    <- reactive(read_data("precio_venta.csv"))
  datos_precio_alquiler <- reactive(read_data("precio_alquiler.csv"))
  
  mapServer("compra",   datos_precio_venta)
  mapServer("alquiler", datos_precio_alquiler)
}