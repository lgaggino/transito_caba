################################################################################
# Procesamiento y Limpieza del “consolidado.csv” de tránsito en Peajes de la   #
# Ciudad de Buenos Aires                                                       #
#                                                                              #
# Este script detecta la ruta del script, lee el archivo consolidado.csv,      #
# aplica saneamiento de texto, filtra y limpia filas con NA y totales,         #
# unifica alias de estaciones, formas de pago y categorías,                    #
# añade georreferencias y variables de fecha, y genera salidas para Shiny.     #
###############################################################################

# ──────────────────────────────────────────────────────────────────────────────
# 0) Carga de librerías --------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(readr)

# ──────────────────────────────────────────────────────────────────────────────
# 1) Configuración inicial -----------------------------------------------------   
# ──────────────────────────────────────────────────────────────────────────────
args     <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grep("^--file=", args)]
if (length(file_arg) == 0) {
  script_dir <- normalizePath(getwd())
} else {
  script_path <- sub("^--file=", "", file_arg)
  script_dir  <- dirname(normalizePath(script_path))
}

project_root <- normalizePath(file.path(script_dir, ".."))

in_file <- file.path(project_root, "input", "consolidado.csv")
if (!file.exists(in_file)) {
  stop("No encontrado ", in_file)
}
df <- read_csv(in_file, locale = locale(encoding = "Latin1"), show_col_types = FALSE)

# ──────────────────────────────────────────────────────────────────────────────
# 2) Saneamiento y filtrado de filas --------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# 2.1 Sanea columnas por tipo de caracter
cat("Saneando texto…\n")
char_cols <- which(vapply(df, is.character, logical(1)))
df[char_cols] <- lapply(df[char_cols], function(x) {
  iconv(x, from = "latin1", to = "UTF-8", sub = "")
})
cat("Texto saneado en", length(char_cols), "columnas.\n")

# 2.2 Filtra filas con NA
before <- nrow(df)
df_proc <- df %>%
  filter(
    !is.na(estacion),
    !is.na(pago),
    !is.na(categoria),
    !is.na(fecha),
    !is.na(hora)
  )
after_na <- nrow(df_proc)
cat(sprintf("Filas antes de filtrar NAs: %d  →  después: %d  (eliminadas: %d)\n",
            before, after_na, before - after_na))

# 2.3 Elimina filas que tengan “Total”
to_drop <- df_proc %>%
  filter(
    str_detect(tolower(estacion),  "^total$") |
      str_detect(tolower(pago),      "^total$") |
      str_detect(tolower(categoria), "^total$") |
      str_detect(tolower(sentido),   "^total$")
  )
cat("Filas con 'Total' detectadas:", nrow(to_drop), "\n")

df_proc <- df_proc %>%
  filter(
    !str_detect(tolower(estacion),  "^total$"),
    !str_detect(tolower(pago),      "^total$"),
    !str_detect(tolower(categoria), "^total$"),
    !str_detect(tolower(sentido),   "^total$")
  )
after_tot <- nrow(df_proc)
cat(sprintf("Filas tras eliminar 'Total': %d  (eliminadas: %d)\n",
            after_tot, after_na - after_tot))

# ──────────────────────────────────────────────────────────────────────────────
# 3) Unificación de alias ------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# 3.1 Estaciones
est_map <- c(
  # Alberti
  "ALB" = "Alberti",        "Alberdi"     = "Alberti",
  "ALBERDI" = "Alberti",    "Alberti"     = "Alberti",
  # Parque Avellaneda
  "AVE" = "Parque Avellaneda",  "Avellaneda" = "Parque Avellaneda",
  "AVELLANEDA" = "Parque Avellaneda",
  # Dellepiane
  "DEL" = "Dellepiane",           "Dellepiane"             = "Dellepiane",
  "Dellepiane Centro" = "Dellepiane","DEC"                   = "Dellepiane",
  "Dellepiane Liniers" = "Dellepiane","DELLEPIANE LINIERS"    = "Dellepiane", 
  "DELLEPIANE LINIERSLEPIANE CENTRO" = "Dellepiane",
  # Illia
  "Illia" = "Illia",       "ILL"     = "Illia",
  "ILLIA" = "Illia",       "ILP"     = "Illia",
  "KDT"   = "Illia",
  "Retiro" = "Illia",     "RET"      = "Illia",
  "RETIRO" = "Illia",
  "Salguero" = "Illia", "SAL"       = "Illia",
  "SALGUERO" = "Illia",
  "Sarmiento" = "Illia","SAR"       = "Illia",
  "SARMIENTO" = "Illia",
  # Paseo del Bajo
  "PDB"  = "Paseo del Bajo","PB1" = "Paseo del Bajo",
  "PB2"  = "Paseo del Bajo","PB3" = "Paseo del Bajo",
  "PB4"  = "Paseo del Bajo","PB5" = "Paseo del Bajo",
  "PB6"  = "Paseo del Bajo"
)

df_proc <- df_proc %>%
  mutate(
    estacion = recode(estacion, !!!est_map, .default = estacion)
  )

cat("Unificados alias de 'estacion'.\n")

# 3.2 Pago
pago_map <- c(
  # Efectivo
  "Efectivo"  = "Efectivo",  "EFECTIVO" = "Efectivo",
  # Exento
  "Exento"    = "Exento",    "EXENTO"   = "Exento",
  # Infracción
  "INFRACCION"          = "Infracción",
  "Violacion"           = "Infracción", 
  "Violación"           = "Infracción",
  "ViolaciÃ³n"          = "Infracción",
  "ViolaciÃ\u0083Â³n"   = "Infracción",
  "ViolaciÃÂÃÂ³n" = "Infracción",
  # Billeteras virtuales
  "Mercado Pago" = "Billeteras virtuales",
  "Modo"         = "Billeteras virtuales",
  # Telepase
  "AUPASS"       = "Telepase",
  "TELEPASE"     = "Telepase",
  "Tag"          = "Telepase",
  # Discapacidad
  "T. DISCAPACIDAD"        = "Discapacidad",
  "TARJETA DISCAPACIDAD"   = "Discapacidad",
  # No cobrado
  "NO COBRADO" = "No Cobrado",
  "No cobrado" = "No Cobrado",
  # Otros
  "Tarjeta MagÃ©tica"   = "Otros",
  "Tarjeta MagnÃ©tica" = "Otros",
  "Tarjeta MagnÃ\u0083Â©tica" = "Otros",
  "Tarjeta MagnÃÂÃÂ©tica" = "Otros",
  "Tarjeta Magnética"   = "Otros",
  "CPP"                 = "Otros",
  "M"                   = "Otros",
  "MONEDERO"            = "Otros",
  "N/D"                 = "Otros",
  "Otros"               = "Otros",
  "Rec.Deuda"           = "Otros",
  "Rec Deuda"           = "Otros"
)

df_proc <- df_proc %>%
  mutate(
    pago = recode(pago, !!!pago_map, .default = pago)
  )

cat("Unificados alias de 'pago'.\n")

# 3.3 Categoría
df_proc <- df_proc %>%
  mutate(
    categoria = case_when(
      categoria %in% c("Moto", "0", "Pago doble Moto") 
      ~ "Moto",
      categoria %in% c("Auto", "Liviano", "LIVIANO", "1", "Pago Doble Auto") 
      ~ "Auto",
      categoria %in% c("Auto con trailer", "2") 
      ~ "Auto con trailer",
      categoria %in% c(as.character(3:23),
                       "Pesado", "PESADO",
                       "Pesados 2 Ejes", "Pago Doble Pesado 2 Ejes",
                       "Pesados 3 Ejes", "Pesados 4 Ejes",
                       "Pesados 5 Ejes", "Pesados 6 Ejes")
      ~ "Pesado",
      categoria %in% c("N/D") 
      ~ "Otros",
      TRUE 
      ~ NA_character_
    ),
    categoria = coalesce(categoria, "Otros")
  )
cat("Unificados alias de categoria.\n")

# ──────────────────────────────────────────────────────────────────────────────
# 4) Georreferencias y variables de fecha --------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# 4.1 Agrega georrefrencias
geo_file <- file.path(project_root, "input", "georreferencias.csv")
if (!file.exists(geo_file)) {
  stop("No encontré el archivo '", geo_file, "'. Asegurate de que el CSV esté en raw/")
}
geof <- read_csv(geo_file, show_col_types = FALSE)

df_proc <- df_proc %>%
  left_join(geof, by = "estacion")
cat("Columnas 'longitud' y 'latitud' agregadas correctamente.\n")

# 4.2 Agrega día de la semana, mes y año
df_proc <- df_proc %>%
  mutate(
    fecha_date = as.Date(fecha, format = "%d/%m/%Y"),
    dia  = weekdays(fecha_date, abbreviate = FALSE) %>% stringr::str_to_title(),
    mes  = lubridate::month(fecha_date, label = TRUE, abbr = FALSE) %>% as.character() %>% stringr::str_to_title(),
    año  = lubridate::year(fecha_date)
  ) %>%
  select(-fecha_date)

cat("Columnas 'dia', 'mes' y 'año' agregadas correctamente.\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 5) Guardar consolidado procesado ---------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
  out_file <- file.path(project_root, "input", "consolidado_processed.csv")
  write_excel_csv(df_proc, out_file)
  cat("✔ Consolidado limpio escrito en:", out_file, "\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 6) Generar salidas para Shiny ------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
out_file <- file.path(project_root, "input", "consolidado_processed.csv")
write_excel_csv(df_proc, out_file)
cat("✔ Consolidado limpio escrito en:", out_file, "\n\n")

df_shiny <- read_csv(out_file, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(año = as.character(año))

geof <- df_shiny %>% distinct(estacion, latitud, longitud)

# 6.1) Hora pico por sentido
peak_hora_sentido <- df_shiny %>%
  group_by(estacion, hora, sentido, dia, año) %>%
  summarise(conteo = n(), .groups = "drop") %>%
  bind_rows(
    df_shiny %>%
      group_by(estacion, hora, sentido, dia) %>%
      summarise(conteo = n(), .groups = "drop") %>%
      mutate(año = "Todos")
  ) %>%
  left_join(geof, by = "estacion")
write_excel_csv(peak_hora_sentido,
                file.path(project_root, "input", "hora_pico_sentido.csv"))
cat("✔ Generado: hora_pico_sentido.csv\n")

# 6.2) Categoría por día y mes
categoria_por_dia <- df_shiny %>%
  count(mes, dia, categoria, name = "conteo") %>%
  bind_rows(
    df_shiny %>%
      count(mes, dia, name = "conteo") %>%
      mutate(categoria = "Total")
  )
write_excel_csv(categoria_por_dia,
                file.path(project_root, "input", "categoria_por_dia.csv"))
cat("✔ Generado: categoria_por_dia.csv\n")

# 6.3) Tránsito por estación y año
transito_por_estacion <- df_shiny %>%
  count(estacion, año, name = "conteo") %>%
  bind_rows(
    df_shiny %>%
      count(estacion, name = "conteo") %>%
      mutate(año = "Todos")
  )
write_excel_csv(transito_por_estacion,
                file.path(project_root, "input", "transito_por_estacion.csv"))
cat("✔ Generado: transito_por_estacion.csv\n")

# 6.4) Evolución de formas de pago
evolucion_pago <- df_shiny %>%
  count(año, estacion, pago, name = "conteo") %>%
  arrange(año, estacion, pago)
write_excel_csv(evolucion_pago,
                file.path(project_root, "input", "evolucion_pago.csv"))
cat("✔ Generado: evolucion_pago.csv\n")

# 6.5) Base de estaciones
estaciones <- geof
write_excel_csv(estaciones,
                file.path(project_root, "input", "estaciones.csv"))
cat("✔ Generado: estaciones.csv\n")

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────