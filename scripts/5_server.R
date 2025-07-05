source("scripts/4_global.R")

server <- function(input, output, session) {
  
  # ────────────────────────────────────────────────────────────────────────────
  # 0) Mapa base y burbujas iniciales ------------------------------------------
  # ────────────────────────────────────────────────────────────────────────────
  output$mapa_pico <- renderLeaflet({
    df0 <- hora_pico_sentido_df %>%
      mutate(hora = as.integer(hora)) %>%
      filter(
        hora    == isolate(input$filtro_hora),
        sentido %in% isolate(input$filtro_sentido)
      ) %>%
      group_by(estacion, latitud, longitud) %>%
      summarise(conteo = mean(conteo), .groups = "drop")
    
    pal0    <- colorNumeric(
      palette = c("#C9F2E1", "#8FCFBA", "#66B598", "#5A6F4E"),
      domain  = df0$conteo
    )
    colores0<- pal0(df0$conteo)
    
    # calculo máximo local para escalar radios
    max_mean0 <- max(df0$conteo, na.rm = TRUE)
    
    leaflet(df0) %>%
      addTiles() %>%
      setView(lng = -58.40, lat = -34.62, zoom = 11.4) %>%
      addCircleMarkers(
        lng         = ~longitud,
        lat         = ~latitud,
        radius      = ~sqrt(conteo / max_mean0) * 55,
        fillColor   = colores0,
        fillOpacity = 0.55,
        stroke      = FALSE,
        group       = "markers",
        label       = ~paste0(estacion, ": ", round(conteo,1))
      ) %>%
      addLegend(
        "bottomright",
        pal     = pal0,
        values  = ~conteo,
        title   = "Promedio vehículos",
        opacity = 0.7
      )
  })
  
  # 0.1 actualiza SOLO las burbujas al cambiar filtros
  observeEvent(
    list(input$filtro_hora, input$filtro_sentido),
    {
      df <- hora_pico_sentido_df %>%
        mutate(hora = as.integer(hora)) %>%
        filter(
          hora    == input$filtro_hora,
          sentido %in% input$filtro_sentido
        ) %>%
        group_by(estacion, latitud, longitud) %>%
        summarise(conteo = mean(conteo), .groups = "drop")
      
      pal    <- colorNumeric(
        palette = c("#C9F2E1", "#8FCFBA", "#66B598", "#5A6F4E"),
        domain  = df$conteo
      )
      colores<- pal(df$conteo)
      
      max_mean <- max(df$conteo, na.rm=TRUE)
      
      leafletProxy("mapa_pico", data = df) %>%
        clearGroup("markers") %>%
        addCircleMarkers(
          lng         = ~longitud,
          lat         = ~latitud,
          radius      = ~sqrt(conteo / max_mean) * 55,
          fillColor   = colores,
          fillOpacity = 0.55,
          stroke      = FALSE,
          group       = "markers",
          label       = ~paste0(estacion, ": ", round(conteo,1))
        ) %>%
        clearControls() %>%
        addLegend(
          "bottomright",
          pal     = pal,
          values  = ~conteo,
          title   = "Promedio vehículos",
          opacity = 0.7
        )
    },
    ignoreInit = TRUE
  )
  
  # ────────────────────────────────────────────────────────────────────────────
  # 1) Gráfico de categorías por día -------------------------------------------
  # ────────────────────────────────────────────────────────────────────────────
  # 1.1 filtra por mes y siempre devuelve df
  filtered_cat <- reactive({
    req(input$filtro_mes)
    df <- categoria_por_dia_df
    if (input$filtro_mes != "Todos") {
      df <- df %>% filter(mes == input$filtro_mes)
    }
    df <- df %>% filter(categoria != "Total")
    df
  })
  
    output$grafico_cat_dia <- renderPlotly({
    req(input$filtro_mes)
    
    # 1.2 Resumen y filtro
    df <- categoria_por_dia_df %>%
      filter(input$filtro_mes == "Todos" | mes == input$filtro_mes) %>%
      filter(categoria != "Total") %>%
      group_by(categoria) %>%
      summarise(total = sum(conteo), .groups = "drop")
    
    # 1.3 Packing SIN escalar los radios
    packing <- circleProgressiveLayout(df$total, sizetype='area')
    factor <- 23
    packing <- packing %>%
      mutate(
        x      = x * factor,
        y      = y * factor,
        radius = radius * factor
      )
    df_pack  <- bind_cols(df, packing)
    vertices <- circleLayoutVertices(packing, npoints = 100)
    xran <- range(vertices$x)
    yran <- range(vertices$y)
    
    # 1.4 Paleta rotada
    cats    <- df_pack$categoria
    pal_all <- custom_palette
    start   <- which(names(pal_all) == "secundario1")
    pal_rot <- pal_all[c(start:length(pal_all), 1:(start-1))]
    pal_vals<- pal_rot[ seq_along(cats) ]
    names(pal_vals) <- cats
    
    # 1.5 quita ejes
    gg <- ggplot() +
      geom_polygon(
        data = vertices,
        aes(x, y, group=id, fill=df_pack$categoria[id]),
        colour = NA, alpha = 0.8
      ) +
      scale_fill_manual(values = pal_vals, guide="none") +
      coord_equal(
        xlim = xran,
        ylim = yran,
        expand = FALSE
      ) +
      theme_void() +
      theme(
        panel.background = element_rect(fill = custom_palette["secundario7"], color = NA),
        plot.background  = element_rect(fill = custom_palette["secundario7"], color = NA)
      )
    
    # 1.6 a plotly
    p <- ggplotly(gg, tooltip = c("fill")) %>%
      config(displayModeBar = FALSE) %>%
      layout_plotly_transit()
    
    p <- p %>%
      layout(
        margin = list(t=65, b=0, l=20, r=20),
        xaxis = list(
          visible = FALSE,
          range   = xran
        ),
        yaxis = list(
          visible = FALSE,
          range   = yran
        )
      )

    p$x$layout$margin <- list(t=10, b=10, l=10, r=10)
    p$x$layout$xaxis  <- list(visible = FALSE)
    p$x$layout$yaxis  <- list(visible = FALSE)
    
    p
  })
    
  # ────────────────────────────────────────────────────────────────────────────
  # 2) Gráfico de tránsito por estación ----------------------------------------
  # ────────────────────────────────────────────────────────────────────────────
  filtered_trans <- reactive({
    if (input$filtro_est == "Todas") transito_por_estacion_df
    else filter(transito_por_estacion_df, estacion == input$filtro_est)
  })
  output$grafico_trans_est <- renderPlotly({
    df <- transito_por_estacion_df %>%
      filter(año != "Todos") %>%
      { if (input$filtro_est != "Todas") filter(., estacion == input$filtro_est) else . } %>%
      mutate(
        año_chr   = as.character(año),
        año_label = ifelse(
          nchar(año_chr) == 1, paste0("200", año_chr),
          paste0("20",   año_chr)
        ),
        año_num   = as.integer(año_label)
      ) 
    # define los niveles ordenedos por año_num
    niveles_label <- df %>%
      distinct(año_label, año_num) %>%
      arrange(año_num) %>%
      pull(año_label)
    
    df <- df %>%
      mutate(año_label = factor(año_label, levels = niveles_label))
    
    cols_est <- custom_palette[ seq_len(n_distinct(df$estacion)) ]
    names(cols_est) <- unique(df$estacion)
    
    gg <- ggplot(df, aes(x = año_label, y = conteo, fill = estacion)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = cols_est) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_transit(base_size = 13)
    
    ggplotly(gg, tooltip = c("x","y","fill")) %>%
    layout_plotly_transit()
  })
  
  # ────────────────────────────────────────────────────────────────────────────
  # 3) Gráfico de evolución de pago --------------------------------------------
  # ────────────────────────────────────────────────────────────────────────────
  filtered_evo <- reactive({
    if (input$filtro_pago == "Todas") evolucion_pago_df
    else filter(evolucion_pago_df, pago == input$filtro_pago)
  })
  
  output$grafico_evo_pago <- renderPlotly({
    df_base <- evolucion_pago_df %>%
      filter(pago != "Total") %>%
      mutate(
        año_chr   = as.character(año),
        año_label = ifelse(
          nchar(año_chr)==1,
          paste0("200", año_chr),
          paste0("20",   año_chr)
        )
      ) %>%
      # agrupa por pago y por año_label
      group_by(pago, año_label) %>%
      summarise(conteo = sum(conteo), .groups = "drop") %>%
      arrange(año_label)
    
    # streamgraph proporcional
    if (input$filtro_pago == "Todas") {
      niveles_pago    <- sort(unique(df_base$pago))
      pal_stream      <- c(
        "#C9F2E1","#8FCFBA","#66B598","#5A6F4E","#FFAAA5",
        "#F6EB98","#B5EAEA","#CBAACB","#3A3A3A","#FEFDF5"
      )[seq_along(niveles_pago)]
      
      p <- plot_ly()
      for(i in seq_along(niveles_pago)) {
        cat_i <- niveles_pago[i]
        df_i  <- df_base[df_base$pago == cat_i, ]
        p <- p %>%
          add_trace(
            data       = df_i,
            x          = ~año_label,
            y          = ~conteo,
            name       = cat_i,
            type       = 'scatter',
            mode       = 'none',
            stackgroup = 'one',
            groupnorm  = 'percent',
            fillcolor  = pal_stream[i]
          )
      }
      p <- p %>%
        layout(
          xaxis = list(tickangle=45, tickmode="array",
                       tickvals=unique(df_base$año_label),
                       ticktext=unique(df_base$año_label)),
          yaxis = list(tickformat="%", showgrid=FALSE)
        )
      
      # 3.1 Caso “una sola forma de pago”: gráfico de líneas
    } else {
      df_i <- df_base %>% filter(pago == input$filtro_pago)
      p <- plot_ly(
        df_i,
        x    = ~año_label,
        y    = ~conteo,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = custom_palette["principal_dark"], width = 2),
        marker = list(color = custom_palette["principal"], size = 6),
        name = input$filtro_pago
      ) %>%
        layout(
          xaxis = list(tickangle=45, tickmode="array",
                       tickvals = df_i$año_label,
                       ticktext = df_i$año_label),
          yaxis = list(title="Conteo", tickformat=",", showgrid=TRUE)
        )
    }

    p %>%
      layout(
        plot_bgcolor  = custom_palette["secundario7"],
        paper_bgcolor = custom_palette["secundario7"]
      ) %>%
      config(displayModeBar = FALSE) %>%
      layout_plotly_transit()
  })
}

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
