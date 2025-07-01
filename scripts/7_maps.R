# UI del módulo Contexto
contextoUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("desplegable_contexto"), "Seleccione una opción:",
                choices = c("Factores demográficos", "Factores socioeconómicos")),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Factores demográficos'", ns("desplegable_contexto")),
      selectInput(ns("desplegable_demograficos"), "Indicador demográfico:",
                  choices = c("Pirámide demográfica", "Población Estimada", "Población >65")),
      plotlyOutput(ns("grafico_demograficos")),
      dataTableOutput(ns("tabla_piramide"))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Factores socioeconómicos'", ns("desplegable_contexto")),
      dataTableOutput(ns("tabla_socioeconomicos"))
    )
  )
}

# Server del módulo Contexto
contextoServer <- function(id, datos_demog, datos_soc) {
  moduleServer(id, function(input, output, session) {
    # Gráfico demográficos
    output$grafico_demograficos <- renderPlotly({
      req(input$desplegable_contexto == "Factores demográficos")
      df <- datos_demog()
      if (input$desplegable_demograficos == "Pirámide demográfica") {
        # aquí tu código de pirámide…
      } else if (input$desplegable_demograficos == "Población Estimada") {
        # tu código de población estimada…
      } else {
        # …
      }
    })
    # Tablas…
    output$tabla_piramide <- renderDataTable({ req(input$desplegable_demograficos); df <- datos_demog(); datatable(df) })
    output$tabla_socioeconomicos <- renderDataTable({ req(input$desplegable_contexto=="Factores socioeconómicos"); datatable(datos_soc()) })
  })
}