###############################################################################
# Análisis Exploratorio de Datos (EDA) de archivos de tránsito de la Ciudad   #
# de Buenos Aires                                                             #
#                                                                             #
# Este script recorre archivos CSV brutos,                                    #
# registra un log detallado de la estructura de datos (columnas, tipos,       #
# filas, missing), identifica inconsistencias entre archivos y calcula        #
# estadísticos básicos de calidad (valores faltantes).                        #
#                                                                             #
###############################################################################

# ──────────────────────────────────────────────────────────────────────────────
# 0) Carga de librerías --------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
library(data.table)
library(tidyverse)
library(janitor)
library(lubridate)
library(glue)
library(stringr)

# ──────────────────────────────────────────────────────────────────────────────
# 1) Definición de rutas -------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# Determina automáticamente el directorio raíz según la ubicación
wd <- normalizePath(getwd())
if (basename(wd) == "scripts") {
  root_dir <- dirname(wd)
} else if (basename(wd) == "transito_caba") {
  root_dir <- wd
} else {
  root_dir <- normalizePath(file.path(wd, "transito_caba"))
}

raw_dir    <- file.path(root_dir, "raw")                   # Carpeta con datos originales
report_dir <- file.path(root_dir, "output", "reports")     # Carpeta para salidas

# Crea la carpeta de reportes si no existe
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)

# ──────────────────────────────────────────────────────────────────────────────
# 2) Preparar archivo de log ---------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# Abre un archivo de texto para registrar el EDA
log_file <- file.path(report_dir, "eda_raw.txt")
log_con  <- file(log_file, open = "wt", encoding = "UTF-8")

# Función auxiliar para imprimir en consola y en el log
msg <- function(...) {
  line <- glue::glue(..., .envir = parent.frame())
  cat(line, "\n")                     # consola
  cat(line, "\n", file = log_con, append = TRUE)  # log
}

# Encabezado del log
msg("============================================================")
msg("EDA - {Sys.time()}")
msg("Root dir : {root_dir}")
msg("Raw dir  : {raw_dir}")
msg("Rep dir  : {report_dir}")
msg("============================================================\n")

# ──────────────────────────────────────────────────────────────────────────────
# 3) Inventario de archivos ----------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# Función para listar archivos .csv y clasificar por tipo y año
inventariar_archivos <- function(dir_path) {
  files <- list.files(dir_path, "\\.csv$", full.names = TRUE)
  tibble(file = files) %>%
    mutate(
      file_name = basename(file),
      tipo = case_when(
        str_detect(file_name, "^flujo-vehicular-\\d{4}")      ~ "anual",
        str_detect(file_name, "^Transito [A-Za-z]+ \\d{4}")   ~ "mensual",
        str_detect(file_name, "^Transito Enero-Febrero 2022") ~ "bimestre",
        TRUE                                                  ~ "otro"
      ),
      anio   = as.integer(str_extract(file_name, "\\d{4}")),
      tam_mb = round(file.size(file) / 1024^2, 2)
    )
}

# Ejecuta inventario y muestra resumen
file_index <- inventariar_archivos(raw_dir)
msg("### Inventario ###")
msg("Total archivos : {nrow(file_index)} — {round(sum(file_index$tam_mb),2)} MB")

# Archivos por tipo
file_index %>% 
  count(tipo) %>% 
  purrr::pwalk(~ msg("{..2}: {..1}"))

# Archivos por año
msg("\n### Archivos por año ###")
file_index %>% 
  count(anio, sort = TRUE) %>% 
  purrr::pwalk(~ msg("{..1}: {..2} archivos"))
msg("")

# ──────────────────────────────────────────────────────────────────────────────
# 4) Columnas únicas por año ---------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
msg("### Columnas únicas y años de aparición ###")

# Determina el separador del CSV
get_sep <- function(path) {
  con <- file(path, open = "rt", encoding = "Latin1")
  header_raw  <- readLines(con, n = 1); close(con)
  header_utf8 <- iconv(header_raw, from = "Latin1", to = "UTF-8", sub = "")
  tabs <- str_count(header_utf8, "\t")
  sc   <- str_count(header_utf8, ";")
  com  <- str_count(header_utf8, ",")
  if (tabs >= sc && tabs >= com) "\t"
  else if (sc >= com) ";"
  else ","
}

# Extrae y limpia nombres de columnas del header
get_header_cols <- function(path) {
  con <- file(path, open = "rt", encoding = "Latin1")
  header_raw  <- readLines(con, n = 1); close(con)
  header_utf8 <- iconv(header_raw, from = "Latin1", to = "UTF-8", sub = "")
  sep         <- get_sep(path)
  cols        <- strsplit(header_utf8, sep, fixed = TRUE)[[1]]
  janitor::make_clean_names(cols)
}

# Compila lista de columnas y años
col_years <- file_index %>%
  mutate(cols = map(file, get_header_cols)) %>%
  select(anio, cols) %>%
  unnest(cols) %>%
  distinct()

# Genera resumen de aparición por columna
col_summary <- col_years %>%
  group_by(cols) %>%
  summarise(anios = paste(sort(unique(anio)), collapse = ", "), .groups = "drop")

# Imprime resumen
col_summary %>%
  arrange(cols) %>%
  purrr::pwalk(~ msg("{..1}: {..2}"))
msg("")

# ──────────────────────────────────────────────────────────────────────────────
# 5) Estadísticas por archivo --------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
msg("### Estadísticas por archivo ###")

# Listas para acumular resultados de tipos y missing
all_types   <- list()
all_missing <- list()

for (path in file_index$file) {
  name <- basename(path)
  sep  <- get_sep(path)
  
  # Leer con data.table
  dt <- tryCatch(
    data.table::fread(path, sep = sep, encoding = "Latin-1", showProgress = FALSE),
    error = function(e) {
      msg("Archivo: {name}")
      msg("  ERROR al leer: {e$message}\n")
      return(NULL)
    }
  )
  if (is.null(dt)) next
  
  msg("Archivo: {name}")
  msg("  Filas    : {nrow(dt)}")
  msg("  Columnas : {ncol(dt)}")
  
  # Tipos de datos por columna
  clases <- sapply(dt, function(x) class(x)[1])
  tipos_str <- paste0(names(clases), "(", clases, ")", collapse = ", ")
  msg("  Tipos    : {tipos_str}")
  
  # Guardar tipos para comprobaciones posteriores
  all_types[[name]] <- tibble(
    file   = name,
    column = names(clases),
    class  = unname(clases)
  )
  
  # Cálculo de valores faltantes por columna
  miss_vec <- sapply(dt, function(x) sum(is.na(x)))
  all_missing[[name]] <- tibble(
    file      = name,
    column    = names(miss_vec),
    missing   = miss_vec,
    nrows     = nrow(dt)
  )
  
  # Resumen de missing en el log
  miss_str <- if (all(miss_vec == 0)) "ninguno" else {
    paste0(names(miss_vec)[miss_vec > 0], "=", miss_vec[miss_vec > 0], collapse = ", ")
  }
  msg("  Faltantes: {miss_str}\n")
}

# ──────────────────────────────────────────────────────────────────────────────
# 6) Tipos inconsistentes entre archivos ---------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
msg("### Tipos inconsistentes entre archivos ###")

type_df   <- bind_rows(all_types)
inconsist <- type_df %>%
  group_by(column) %>%
  summarise(
    clases  = paste(sort(unique(class)), collapse = "/"),
    n_types = n_distinct(class),
    .groups = "drop"
  ) %>%
  filter(n_types > 1)

if (nrow(inconsist) == 0) {
  msg("No se detectaron inconsistencias de tipo entre archivos.\n")
} else {
  inconsist %>%
    purrr::pwalk(~ msg("  {..1}: clases {..2}"))
  msg("")
}

# ──────────────────────────────────────────────────────────────────────────────
# 7) Resumen global de valores faltantes----------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
msg("### Resumen global de valores faltantes (en %) ###")

missing_df <- bind_rows(all_missing)

missing_summary <- missing_df %>%
  group_by(column) %>%
  summarise(
    total_missing      = sum(missing),
    total_cells        = sum(nrows),
    files_with_missing = sum(missing > 0),
    total_files        = n(),
    pct_missing        = round(100 * total_missing / total_cells, 2),
    pct_files_missing  = round(100 * files_with_missing / total_files, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_missing))

# Imprime resumen final de NA%
missing_summary %>%
  purrr::pwalk(~ msg("  {..1}: {..6}% NA (sobre {..2} valores), {..7}% archivos con NA"))
msg("")

# ──────────────────────────────────────────────────────────────────────────────
# 8) Cierre del reporte --------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
msg("=== Fin del reporte EDA – {Sys.time()} ===")
close(log_con)

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────