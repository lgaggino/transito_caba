# UI del mapa
mapUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("mapa"), height = 600)
}

# Server del mapa
mapServer <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    output$mapa <- renderLeaflet({
      df <- datos()
      leaflet(df) %>% addTiles() %>%
        addCircleMarkers(~lon, ~lat,
                         radius = ~sqrt(precio)/10,
                         color  = custom_palette["principal"],
                         label  = ~paste0(barrio, ": $", precio)
        )
    })
  })
}