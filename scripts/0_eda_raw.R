###############################################################################
# 0_eda_raw_info.R – Reporte de inventario, columnas, estadísticas y chequeos
# Proyecto : transito_caba
# Uso      :
#   1. Sitúate en la carpeta raíz del proyecto (transito_caba/)
#   2. Ejecuta: source("0_eda_raw_info.R")
# Salida   :
#   • data/output/reports/eda_raw.txt – Log con inventario, columnas, estadísticas,
#     chequeo de tipos inconsistentes y resumen de valores faltantes en %
###############################################################################

# ───────────────────── 0) Paquetes ──────────────────────────────────────────
required_pkgs <- c(
  "data.table", "tidyverse", "janitor", "lubridate",
  "glue", "stringr"
)
new_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(new_pkgs)) install.packages(new_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ───────────────────── 1) Rutas ─────────────────────────────────────────────
wd <- normalizePath(getwd())
if (basename(wd) == "scripts") {
  root_dir <- dirname(wd)
} else if (basename(wd) == "transito_caba") {
  root_dir <- wd
} else {
  root_dir <- normalizePath(file.path(wd, "transito_caba"))
}

raw_dir    <- file.path(root_dir, "raw")
report_dir <- file.path(root_dir, "output", "reports")

dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)


# ───────────────────── 2) Preparar log ──────────────────────────────────────
log_file <- file.path(report_dir, "eda_raw.txt")
log_con  <- file(log_file, open = "wt", encoding = "UTF-8")

msg <- function(...) {
  line <- glue::glue(..., .envir = parent.frame())
  cat(line, "\n")
  cat(line, "\n", file = log_con, append = TRUE)
}

msg("============================================================")
msg("EDA - {Sys.time()}")
msg("Root dir : {root_dir}")
msg("Raw dir  : {raw_dir}")
msg("Rep dir  : {report_dir}")
msg("============================================================\n")

# ───────────────────── 3) Inventario de archivos ───────────────────────────
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

file_index <- inventariar_archivos(raw_dir)

msg("### Inventario ###")
msg("Total archivos : {nrow(file_index)} — {round(sum(file_index$tam_mb),2)} MB")
file_index %>% 
  count(tipo) %>% 
  purrr::pwalk(~ msg("{..2}: {..1}"))

msg("\n### Archivos por año ###")
file_index %>% 
  count(anio, sort = TRUE) %>% 
  purrr::pwalk(~ msg("{..1}: {..2} archivos"))
msg("")

# ───────────────────── 4) Columnas únicas por año ───────────────────────────
msg("### Columnas únicas y años de aparición ###")

get_sep <- function(path) {
  con <- file(path, open = "rt", encoding = "Latin1")
  header_raw  <- readLines(con, n = 1)
  close(con)
  header_utf8 <- iconv(header_raw, from = "Latin1", to = "UTF-8", sub = "")
  tabs <- str_count(header_utf8, "\t")
  sc   <- str_count(header_utf8, ";")
  com  <- str_count(header_utf8, ",")
  if (tabs >= sc && tabs >= com) "\t"
  else if (sc >= com) ";"
  else ","
}

get_header_cols <- function(path) {
  con <- file(path, open = "rt", encoding = "Latin1")
  header_raw  <- readLines(con, n = 1)
  close(con)
  header_utf8 <- iconv(header_raw, from = "Latin1", to = "UTF-8", sub = "")
  sep         <- get_sep(path)
  cols        <- strsplit(header_utf8, sep, fixed = TRUE)[[1]]
  janitor::make_clean_names(cols)
}

col_years <- file_index %>%
  mutate(cols = map(file, get_header_cols)) %>%
  select(anio, cols) %>%
  unnest(cols) %>%
  distinct()

col_summary <- col_years %>%
  group_by(cols) %>%
  summarise(anios = paste(sort(unique(anio)), collapse = ", "), .groups = "drop")

col_summary %>%
  arrange(cols) %>%
  purrr::pwalk(~ msg("{..1}: {..2}"))
msg("")

# ───────────────────── 5) Estadísticas por archivo ─────────────────────────
msg("### Estadísticas por archivo ###")

all_types    <- list()
all_missing  <- list()

for (path in file_index$file) {
  name <- basename(path)
  sep  <- get_sep(path)
  
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
  
  clases <- sapply(dt, function(x) class(x)[1])
  tipos_str <- paste0(names(clases), "(", clases, ")", collapse = ", ")
  msg("  Tipos    : {tipos_str}")
  
  # Acumular tipos para paso 6
  all_types[[name]] <- tibble(
    file   = name,
    column = names(clases),
    class  = unname(clases)
  )
  
  # Acumular missing y número de filas
  miss_vec <- sapply(dt, function(x) sum(is.na(x)))
  all_missing[[name]] <- tibble(
    file      = name,
    column    = names(miss_vec),
    missing   = miss_vec,
    nrows     = nrow(dt)
  )
  
  miss_str <- if (all(miss_vec == 0)) "ninguno" else {
    paste0(names(miss_vec)[miss_vec > 0], "=", miss_vec[miss_vec > 0], collapse = ", ")
  }
  msg("  Faltantes: {miss_str}\n")
}

# ───────────────── 6) Tipos inconsistentes entre archivos ───────────────────
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

# ────────────────── 7) Resumen global de valores faltantes (en %) ──────────
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

missing_summary %>%
  purrr::pwalk(~ msg("  {..1}: {..6}% NA (sobre {..2} valores), {..7}% archivos con NA"))

msg("")

# ───────────────────── 8) Fin del reporte ────────────────────────────────────
msg("=== Fin del reporte EDA – {Sys.time()} ===")
close(log_con)