# UI de gráficas genéricas (compra/alquiler)
plotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("xvar"), "Eje X:", choices = NULL),
    selectInput(ns("yvar"), "Eje Y:", choices = NULL),
    plotlyOutput(ns("grafico"))
  )
}

# Server de gráficas genéricas
plotsServer <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    # Actualiza selectInput con nombres de columnas
    observe({
      cols <- names(datos())
      updateSelectInput(session, "xvar", choices = cols)
      updateSelectInput(session, "yvar", choices = cols)
    })
    # Dibuja el scatter
    output$grafico <- renderPlotly({
      req(input$xvar, input$yvar)
      df <- datos()
      p <- ggplot(df, aes_string(x=input$xvar, y=input$yvar)) +
        geom_point(color=custom_palette["principal"]) + theme_minimal()
      ggplotly(p)
    })
  })
}