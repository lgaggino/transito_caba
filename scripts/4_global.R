# ──────────────────────────────────────────────────────────────────────────────
# 0) Carga de librerías --------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
library(readr)
library(shiny)
library(shinyjs)
library(shinydashboard) 
library(dplyr)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(packcircles)
library(ggstream)

# ──────────────────────────────────────────────────────────────────────────────
# 1) Lectura de datos pre-agrupados --------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
hora_pico_sentido_df    <- read_csv("input/hora_pico_sentido.csv",    show_col_types = FALSE)
categoria_por_dia_df    <- read_csv("input/categoria_por_dia.csv",    show_col_types = FALSE)
transito_por_estacion_df<- read_csv("input/transito_por_estacion.csv",show_col_types = FALSE)
evolucion_pago_df       <- read_csv("input/evolucion_pago.csv",       show_col_types = FALSE)
estaciones_df           <- read_csv("input/estaciones.csv",           show_col_types = FALSE)

# ──────────────────────────────────────────────────────────────────────────────
# 2) Variables globales y configuración ----------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
custom_palette <- c(
  principal        = "#8FCFBA",
  principal_light  = "#C9F2E1",
  principal_dark   = "#66B598",
  secundario1      = "#5A6F4E",
  secundario2      = "#FFAAA5",
  secundario3      = "#F6EB98",
  secundario4      = "#B5EAEA",
  secundario5      = "#CBAACB",
  secundario6      = "#3A3A3A",
  secundario7      = "#FEFDF5",
  secundario8      = "#F4F3D1"
)

# Maximo absoluto de conteos (para escalar el heatmap)
global_max_conteo <- max(hora_pico_sentido_df$conteo, na.rm = TRUE)

# Ordena los meses en el dataframe de categorías por día
categoria_por_dia_df <- read_csv("input/categoria_por_dia.csv", show_col_types = FALSE) %>%
  mutate(
    mes = factor(
      mes,
      levels = c(
        "Enero","Febrero","Marzo","Abril","Mayo","Junio",
        "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"
      )
    )
  )

# ──────────────────────────────────────────────────────────────────────────────
# 3) Funciones auxiliares para gráficos ----------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# crea un tema de ggplot2 personalizado
theme_transit <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background  = element_rect(fill = custom_palette["secundario7"], color = NA),
      plot.background   = element_rect(fill = custom_palette["secundario7"], color = NA),
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.title      = element_blank(),
      legend.key        = element_rect(fill = custom_palette["secundario7"], color = NA),
      legend.spacing.x  = unit(0.1, "cm"),
      legend.margin     = margin(t = 2, b = 2),
      axis.text.x       = element_text(angle = 45, hjust = 1),
      panel.grid.minor  = element_blank()
    )
}

# Escala manual de relleno
scale_fill_transit <- function(pal_vals) {
  scale_fill_manual(
    values   = pal_vals,
    na.value = custom_palette["secundario7"]
  )
}

# Layout común para Plotly
layout_plotly_transit <- function(p) {
  p %>%
    config(displayModeBar = FALSE) %>%
    layout(
      margin = list(
        t = 30,    
        b = 80,
        l = 40, 
        r = 40  
      ),
      legend = list(
        orientation   = "h",     
        x             = 0.5,      
        xanchor       = "center",
        y             = -0.5,      
        yanchor       = "top",
        font          = list(size = 10),
        itemwidth     = 20,        
        tracegroupgap = 1         
      ),
      xaxis = list(
        title          = "",
        tickangle      = -45       
      ),
      yaxis = list(
        title           = "",
        showticklabels  = FALSE,
        showgrid        = FALSE
      )
    )
}

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────