source("scripts/4_global.R")

# ──────────────────────────────────────────────────────────────────────────────
# 0) Definición de la interfaz de usuario --------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/styles.css"),
            tags$link(rel = "icon", type = "image/png", href = "www/favicon.png")),
  title = "Tránsito CABA",
  navbarPage(NULL,
             tabPanel("Introducción",
                      fluidRow(
                        column(12,
                               tags$div(class = "paragraph",
                                        tags$h2("Bienvenido al dashboard de Tránsito en Peajes de CABA"),
                                        tags$p("Vas a poder explorar:"),
                                        tags$ul(
                                          tags$li(strong("Flujo vehicular por hora:"), 
                                                  "Mapa interactivo para filtrar por hora y sentido de circulación."),
                                          tags$li(strong("Categorías vehiculares:"), 
                                                  "Distribución diaria de tipos de vehículos por mes."),
                                          tags$li(strong("Tránsito por estación de peaje:"), 
                                                  "Evolución anual de conteos por estación."),
                                          tags$li(strong("Modalidad de pago:"), 
                                                  "Streamgraph para ver la participación de cada forma de pago.")
                                        ),
                                        tags$p("Usá los controles para filtrar y descubrir insights")
                               )
                        )
                      )
             ),
             tabPanel("Dashboard",
                      fluidRow(
                        box(
                          title = "Flujo vehicular por hora", width = 6, status = "primary", solidHeader = TRUE,
                          div(class = "controles-inline",
                              sliderInput(
                                inputId   = "filtro_hora", label = NULL,
                                min       = 0, max = 23, value = 9, step = 1,
                                dragRange = FALSE, ticks = FALSE, sep = "", post= ":00 hs", width = "160px"
                              ),
                              checkboxGroupInput(
                                inputId  = "filtro_sentido", label = NULL,
                                choices  = c("Centro", "Provincia"),
                                selected = "Centro", inline = TRUE
                              )
                          ),
                          leafletOutput("mapa_pico", height = "300px"),
                        ),
                        box(
                          title = "Tránsito por Estación de Peaje",
                          width = 6, status = "primary", solidHeader = TRUE,
                          selectInput("filtro_est", "",
                                      choices = c("Todas", sort(unique(transito_por_estacion_df$estacion)))),
                          plotlyOutput("grafico_trans_est", height = 300)
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Evolución de la Modalidad de Pago",
                          width = 6, status = "primary", solidHeader = TRUE,
                          selectInput("filtro_pago", "",
                                      choices = c("Todas", sort(unique(evolucion_pago_df$pago)))),
                          plotlyOutput("grafico_evo_pago", height = 300)
                        ),
                        box(
                          title = "Categorías vehiculares por día",
                          width = 6, status = "primary", solidHeader = TRUE,
                          fluidRow(
                            column(6,
                                   selectInput("filtro_mes", "",
                                               choices = c("Todos", levels(categoria_por_dia_df$mes)))
                            ),
                          ),
                          plotlyOutput("grafico_cat_dia", height = 300)
                        )
                      )
             )
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────