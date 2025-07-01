#!/usr/bin/env Rscript
# 1_consolidate.R – Consolida “estacion”, “pago”, “categoria” y “fecha” + reporte processed.txt

# ==========================================================
# INICIO: CARGA DE PAQUETES Y SETEO DE DIRECTORIOS
# ==========================================================
cat(">>> Arrancando 1_consolidate.R…\n")

suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(glue)
  library(janitor)
})

wd <- getwd()
if (basename(wd) == "scripts") {
  base_dir <- dirname(wd)
} else {
  base_dir <- wd
}

raw_dir    <- file.path(base_dir, "raw")
proc_dir   <- file.path(base_dir, "input")
report_dir <- file.path(base_dir, "output", "reports")
dir.create(proc_dir,   showWarnings = FALSE, recursive = TRUE)
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)

cat(" Carpeta raw    :", raw_dir, "\n")
cat(" Carpeta proc   :", proc_dir, "\n")
cat(" Carpeta reports:", report_dir, "\n\n")

files   <- list.files(raw_dir, "\\.csv$", full.names = TRUE)
n_files <- length(files)
cat("→ Encuentro", n_files, "archivos CSV.\n\n")
if (n_files == 0) stop("¡No encontré ningún CSV en ", raw_dir, "!")

# ==========================================================
# FUNCIONES AUXILIARES PARA LECTURA FLEXIBLE DE CSV
# ==========================================================
get_sep <- function(path){
  hdr <- readLines(path, 1, encoding = "Latin1") %>% iconv("Latin1", "UTF-8", "")
  t <- str_count(hdr, "\t"); s <- str_count(hdr, ";"); c <- str_count(hdr, ",")
  if (t >= s && t >= c) "\t" else if (s >= c) ";" else ","
}

read_dt <- function(path){
  sep <- get_sep(path)
  dt1 <- tryCatch({
    fread(path, sep = sep, encoding = "Latin-1", showProgress = FALSE)
  }, error = function(e) NULL)
  if (!is.null(dt1) && all(!str_detect(names(dt1), "Ã|â|\\?"))) {
    return(dt1)
  }
  dt2 <- tryCatch({
    fread(path, sep = sep, encoding = "UTF-8", showProgress = FALSE)
  }, error = function(e) NULL)
  if (!is.null(dt2)) return(dt2)
  stop("No se pudo leer el archivo con encodings estándar.")
}

# ==========================================================
# INICIALIZACIÓN DE LISTAS Y VARIABLES
# ==========================================================
sum_raw_rows <- 0L
list_est   <- vector("list", n_files)
list_pago  <- vector("list", n_files)
list_cat   <- vector("list", n_files)
list_fecha <- vector("list", n_files)
list_sentido <- vector("list", n_files)
na_cols_report <- list()

# ==========================================================
# PROCESAMIENTO PRINCIPAL DE ARCHIVOS
# ==========================================================
for (i in seq_along(files)) {
  path <- files[i]
  nm   <- basename(path)
  yr   <- str_extract(nm, "\\d{4}") %>% as.integer()
  cat("→ Procesando", i, "/", n_files, ":", nm, "(año:", yr, ")\n")
  
  dt <- tryCatch({
    read_dt(path) %>% as_tibble()
  }, error = function(e){
    cat("  !! ERROR de lectura:", e$message, "\n\n"); return(NULL)
  })
  if (is.null(dt)) next
  
  # Unificar nombres a minúsculas/underscore (corrige headers con ñ/acentos)
  names(dt) <- janitor::make_clean_names(names(dt))
  
  n <- nrow(dt)
  sum_raw_rows <- sum_raw_rows + n
  cat("   → leído, dim:", n, ncol(dt), "\n")
  
  # Completar columnas si faltan
  if (!"estacion"      %in% names(dt)) dt$estacion      <- NA_character_
  if (!"id_peaje"      %in% names(dt)) dt$id_peaje      <- NA_character_
  if (!"forma_pago"    %in% names(dt)) dt$forma_pago    <- NA_character_
  if (!"tipo_cobro"    %in% names(dt)) dt$tipo_cobro    <- NA_character_
  if (!"tipo_vehiculo" %in% names(dt)) dt$tipo_vehiculo <- NA_character_
  if (!"cat_cobrada"   %in% names(dt)) dt$cat_cobrada   <- NA_character_
  if (!"fecha"         %in% names(dt)) dt$fecha         <- NA_character_
  
  # Unificar columnas
  vec_est   <- coalesce(dt$id_peaje, dt$estacion)
  vec_pago  <- if (yr <= 2020) dt$forma_pago else dt$tipo_cobro
  vec_cat   <- if (yr <= 2020) dt$tipo_vehiculo else dt$cat_cobrada
  
  # ==========================================================
  # BLOQUE: CÁLCULO DE vec_fecha PARA TODOS LOS CASOS
  # ==========================================================
  vec_fecha <- rep(NA_character_, n)
  meses_dict <- c(
    enero="01", febrero="02", marzo="03", abril="04", mayo="05", junio="06",
    julio="07", agosto="08", septiembre="09", octubre="10", noviembre="11", diciembre="12"
  )
  
  # 1) 2008–2020: formatos mixtos
  if (!is.na(yr) && yr %in% 2008:2020) {
    f0 <- as.character(dt$fecha)
    d0 <- suppressWarnings(as.Date(f0, "%d/%m/%Y"))
    nas <- which(is.na(d0))
    if (length(nas)>0) {
      iso <- suppressWarnings(as.Date(f0[nas], "%Y-%m-%d"))
      d0[nas[!is.na(iso)]] <- iso[!is.na(iso)]
    }
    nas <- which(is.na(d0))
    if (length(nas)>0) {
      usd <- suppressWarnings(as.Date(f0[nas], "%m/%d/%Y"))
      d0[nas[!is.na(usd)]] <- usd[!is.na(usd)]
    }
    vec_fecha <- ifelse(is.na(d0), NA_character_, format(d0, "%d/%m/%y"))
    cat("[2008-20] →", head(vec_fecha,5), "\n")
  }
  
  # 2) 2021: a partir de 3 columnas limpias
  else if (!is.na(yr) && yr == 2021 &&
           all(c("ano_de_fecha_operativa","mes_de_fecha_operativa","dia_de_fecha_operativa") %in% names(dt))) {
    d     <- as.integer(dt$dia_de_fecha_operativa)
    m_txt <- tolower(dt$mes_de_fecha_operativa)
    y_txt <- as.character(dt$ano_de_fecha_operativa)
    vec_fecha <- paste0(
      str_pad(d,2,pad="0"), "/", meses_dict[m_txt], "/", substr(y_txt,3,4)
    )
    cat("[2021] →", head(vec_fecha,5), "\n")
  }
  
  # 3) 2022 Ene-Feb: fecha nativa en Date/IDate
  else if (!is.na(yr) && yr == 2022 &&
           "ano_de_fecha_operativa" %in% names(dt) &&
           inherits(dt$ano_de_fecha_operativa, c("Date","IDate"))) {
    vec_fecha <- format(dt$ano_de_fecha_operativa, "%d/%m/%y")
    cat("[2022 Ene-Feb] →", head(vec_fecha,5), "\n")
  }
  
  # 4) 2022 archivos con 'ano_de_fecha_operativa' texto "d/m/aaaa" (caso especial)
  else if (!is.na(yr) && yr == 2022 &&
           "ano_de_fecha_operativa" %in% names(dt) &&
           any(str_detect(dt$ano_de_fecha_operativa, "^\\d{1,2}/\\d{1,2}/\\d{4}$"))) {
    f0 <- as.character(dt$ano_de_fecha_operativa)
    valid_idx <- str_detect(f0, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
    d0 <- rep(as.Date(NA), length(f0))
    d0[valid_idx] <- suppressWarnings(as.Date(f0[valid_idx], "%d/%m/%Y"))
    vec_fecha <- ifelse(is.na(d0), NA_character_, format(d0, "%d/%m/%y"))
    cat("[2022 d/m/aaaa caso especial] →", head(vec_fecha,5), "\n")
  }
  
  # 5) 2022 Jul/Sep – texto largo “X de <mes> de YYYY” + fallback ISO
  else if (!is.na(yr) && yr == 2022 && "ano_de_fecha_operativa" %in% names(dt)) {
    f0 <- as.character(dt$ano_de_fecha_operativa)
    d0 <- rep(as.Date(NA), length(f0))
    m_ex  <- str_match(f0, "^(\\d{1,2}) de ([A-Za-z]+) de (\\d{4})$")
    valid <- which(!is.na(m_ex[,1]))
    if (length(valid)>0) {
      dd  <- str_pad(m_ex[valid,2],2,pad="0")
      mm  <- meses_dict[tolower(m_ex[valid,3])]
      yy4 <- m_ex[valid,4]
      d0[valid] <- as.Date(paste0(dd,"/",mm,"/",yy4), "%d/%m/%Y")
    }
    nas <- which(is.na(d0))
    if (length(nas)>0) {
      iso <- suppressWarnings(as.Date(f0[nas], "%Y-%m-%d"))
      d0[nas[!is.na(iso)]] <- iso[!is.na(iso)]
    }
    vec_fecha <- ifelse(is.na(d0), NA_character_, format(d0, "%d/%m/%y"))
    cat("[2022 Jul/Sep] →", head(vec_fecha,5), "\n")
  }
  
  # 6) 2022-2023: columna 'v1' en formato "d/m/aaaa" (caso especial)
  else if (!is.na(yr) && yr %in% c(2022, 2023) &&
           "v1" %in% names(dt) &&
           any(str_detect(dt$v1, "^\\d{1,2}/\\d{1,2}/\\d{4}$"))) {
    f0 <- as.character(dt$v1)
    valid_idx <- str_detect(f0, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
    d0 <- rep(as.Date(NA), length(f0))
    d0[valid_idx] <- suppressWarnings(as.Date(f0[valid_idx], "%d/%m/%Y"))
    vec_fecha <- ifelse(is.na(d0), NA_character_, format(d0, "%d/%m/%y"))
    cat("[v1 d/m/aaaa caso especial] →", head(vec_fecha,5), "\n")
  }
  
  # 7) 2022-2023: columna 'v1' en formato "d de mes de yyyy" (caso especial)
  else if (!is.na(yr) && yr %in% c(2022, 2023) &&
           "v1" %in% names(dt) &&
           any(str_detect(dt$v1, "^(\\d{1,2}) de [A-Za-z]+ de (\\d{4})$"))) {
    f0 <- as.character(dt$v1)
    d0 <- rep(as.Date(NA), length(f0))
    m_ex  <- str_match(f0, "^(\\d{1,2}) de ([A-Za-z]+) de (\\d{4})$")
    valid <- which(!is.na(m_ex[,1]))
    if (length(valid)>0) {
      dd  <- str_pad(m_ex[valid,2],2,pad="0")
      mm  <- meses_dict[tolower(m_ex[valid,3])]
      yy4 <- m_ex[valid,4]
      d0[valid] <- as.Date(paste0(dd,"/",mm,"/",yy4), "%d/%m/%Y")
    }
    vec_fecha <- ifelse(is.na(d0), NA_character_, format(d0, "%d/%m/%y"))
    cat("[v1 'd de mes de yyyy' caso especial] →", head(vec_fecha,5), "\n")
  }
  
  # 8) 2023: columna 'mes_dia_ano_de_fecha_operativa' en d/m/aaaa (caso especial)
  else if (!is.na(yr) && yr == 2023 &&
           "mes_dia_ano_de_fecha_operativa" %in% names(dt) &&
           any(str_detect(dt$mes_dia_ano_de_fecha_operativa, "^\\d{1,2}/\\d{1,2}/\\d{4}$"))) {
    f0 <- as.character(dt$mes_dia_ano_de_fecha_operativa)
    valid_idx <- str_detect(f0, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
    d0 <- rep(as.Date(NA), length(f0))
    d0[valid_idx] <- suppressWarnings(as.Date(f0[valid_idx], "%d/%m/%Y"))
    vec_fecha <- ifelse(is.na(d0), NA_character_, format(d0, "%d/%m/%y"))
    cat("[2023 d/m/aaaa caso especial] →", head(vec_fecha,5), "\n")
  }
  
  # 9) 2023 Junio – columna “mes_dia_ano_de_fecha_operativa”
  else if (!is.na(yr) && yr == 2023 && "mes_dia_ano_de_fecha_operativa" %in% names(dt)) {
    f0 <- as.character(dt$mes_dia_ano_de_fecha_operativa)
    d0 <- rep(as.Date(NA), length(f0))
    m_ex  <- str_match(f0, "^(\\d{1,2}) de ([A-Za-z]+) de (\\d{4})$")
    valid <- which(!is.na(m_ex[,1]))
    if (length(valid)>0) {
      dd  <- str_pad(m_ex[valid,2],2,pad="0")
      mm  <- meses_dict[tolower(m_ex[valid,3])]
      yy4 <- m_ex[valid,4]
      d0[valid] <- as.Date(paste0(dd,"/",mm,"/",yy4), "%d/%m/%Y")
    }
    vec_fecha <- ifelse(is.na(d0), NA_character_, format(d0, "%d/%m/%y"))
    cat("[2023 Junio] →", head(vec_fecha,5), "\n")
  }
  
  # 10) 2022–2024 general: rangos “Mes1-Mes2 YYYY” y “Mes YYYY”
  else if (!is.na(yr) && yr %in% 2022:2024) {
    colf <- intersect(
      c("ano_de_fecha_operativa",
        "mes_dia_ano_de_fecha_operativa",
        "fecha_operativa",
        "fecha"),
      names(dt)
    )[1]
    f0 <- if (!is.na(colf)) as.character(dt[[colf]]) else rep(NA_character_, n)
    idx <- which(!is.na(f0) & str_detect(f0, "^[A-Za-z]+-[A-Za-z]+ \\d{4}$"))
    if (length(idx)>0) {
      f0[idx] <- str_replace(
        f0[idx],
        "^([A-Za-z]+)-[A-Za-z]+ (\\d{4})$",
        "1 de \\1 de \\2"
      )
    }
    idx <- which(!is.na(f0) & str_detect(f0, "^[A-Za-z]+ \\d{4}$"))
    if (length(idx)>0) {
      f0[idx] <- str_replace(
        f0[idx],
        "^([A-Za-z]+) (\\d{4})$",
        "1 de \\1 de \\2"
      )
    }
    d0 <- suppressWarnings(as.Date(f0, "%d/%m/%Y"))
    nas <- which(is.na(d0))
    if (length(nas)>0) {
      iso <- suppressWarnings(as.Date(f0[nas], "%Y-%m-%d"))
      d0[nas[!is.na(iso)]] <- iso[!is.na(iso)]
    }
    nas <- which(is.na(d0))
    if (length(nas)>0) {
      usd <- suppressWarnings(as.Date(f0[nas], "%m/%d/%Y"))
      d0[nas[!is.na(usd)]] <- usd[!is.na(usd)]
    }
    nas <- which(is.na(d0) & !is.na(f0))
    if (length(nas)>0) {
      m_ex <- str_match(f0[nas], "^(\\d{1,2}) de ([A-Za-z]+) de (\\d{4})$")
      ok   <- which(!is.na(m_ex[,1]))
      if (length(ok)>0) {
        dd  <- str_pad(m_ex[ok,2],2,pad="0")
        mm  <- meses_dict[tolower(m_ex[ok,3])]
        yy4 <- m_ex[ok,4]
        d0[nas[ok]] <- as.Date(paste0(dd,"/",mm,"/",yy4), "%d/%m/%Y")
      }
    }
    vec_fecha <- ifelse(is.na(d0), NA_character_, format(d0, "%d/%m/%y"))
    cat(sprintf("[%d Gen] → %s\n", yr, paste(head(vec_fecha,5), collapse=", ")))
  }
  # ==========================================================
  # FIN BLOQUE vec_fecha
  # ==========================================================
  
  # Detectar año
  yr <- str_extract(nm, "\\d{4}") %>% as.integer()
  
  # Agregá esta lógica:
  if (yr >= 2016 && "sentido" %in% names(dt)) {
    vec_sentido <- dt$sentido
  } else if (yr >= 2016 && !"sentido" %in% names(dt)) {
    # Está en año que debería tenerlo, pero no está presente, lo completás NA
    vec_sentido <- rep(NA_character_, n)
  } else {
    # Antes de 2016, lo llenás con "s/d"
    vec_sentido <- rep("s/d", n)
  }
  
  list_est[[i]]   <- tibble(file=nm, nrows=n, estacion=vec_est)
  list_pago[[i]]  <- tibble(file=nm, nrows=n, pago=vec_pago)
  list_cat[[i]]   <- tibble(file=nm, nrows=n, categoria=vec_cat)
  list_fecha[[i]] <- tibble(file=nm, nrows=n, fecha=vec_fecha)
  list_sentido[[i]] <- tibble(file = nm, nrows = n, sentido = vec_sentido)
  
  cat("   → columnas unificadas OK\n\n")
  
  # Calcula % NA solo en las columnas consolidadas (vector, no data.frame)
  na_pct_cons <- c(
    estacion  = mean(is.na(vec_est)),
    pago      = mean(is.na(vec_pago)),
    categoria = mean(is.na(vec_cat)),
    fecha     = mean(is.na(vec_fecha)),
    sentido   = mean(is.na(vec_sentido))
  )
  na_cols_report[[length(na_cols_report) + 1]] <- tibble(
    archivo = nm,
    columna = names(na_pct_cons),
    pct_na  = round(na_pct_cons * 100, 2)
  )
}

# ==========================================================
# CONSOLIDADO FINAL Y SALIDA A CSV
# ==========================================================
df_est   <- bind_rows(list_est)
df_pago  <- bind_rows(list_pago)
df_cat   <- bind_rows(list_cat)
df_fecha <- bind_rows(list_fecha)
df_sentido <- bind_rows(list_sentido)

consol <- tibble(
  estacion  = df_est$estacion,
  pago      = df_pago$pago,
  categoria = df_cat$categoria,
  fecha     = df_fecha$fecha,
  sentido   = df_sentido$sentido
)

out_csv <- file.path(proc_dir, "consolidado.csv")
write_csv(consol, out_csv)
cat("→ CSV consolidado en:", out_csv, "\n\n")

# ==========================================================
# GENERACIÓN DE REPORTE PROCESADO
# ==========================================================
report_path <- file.path(report_dir, "processed.txt")
con <- file(report_path, open = "wt", encoding = "UTF-8")
logf <- function(...) writeLines(glue(...), con)

logf("============================================================")
logf("CONSOLIDACIÓN – {format(Sys.time())}")
logf("Input dir         : {raw_dir}")
logf("Archivos procesados: {n_files}")
logf("Suma filas raw    : {sum_raw_rows}")
logf("Output CSV        : {out_csv}")
logf("============================================================\n")

total_cons <- nrow(consol)
na_est     <- sum(is.na(consol$estacion))
na_pago    <- sum(is.na(consol$pago))
na_cat     <- sum(is.na(consol$categoria))
na_fecha   <- sum(is.na(consol$fecha))
na_sent    <- sum(is.na(consol$sentido))
pct_est    <- round(100 * na_est / total_cons, 2)
pct_pago   <- round(100 * na_pago / total_cons, 2)
pct_cat    <- round(100 * na_cat / total_cons, 2)
pct_fecha  <- round(100 * na_fecha / total_cons, 2)
pct_sent   <- round(100 * na_sent / total_cons, 2)

logf("Total filas consolidadas    : {total_cons}")
logf("Total NA en estacion        : {na_est}  ({pct_est} %)")
logf("Total NA en pago            : {na_pago}  ({pct_pago} %)")
logf("Total NA en categoria       : {na_cat}  ({pct_cat} %)")
logf("Total NA en fecha           : {na_fecha}  ({pct_fecha} %)\n")
logf("Total NA en sentido          : {na_sent}  ({pct_sent} %)")

# Resumen meses y años cargados (solo fechas válidas, formato dd/mm/aa)
if (any(!is.na(consol$fecha))) {
  fechas_validas <- na.omit(consol$fecha[grepl("^\\d{2}/\\d{2}/\\d{2}$", consol$fecha)])
  meses <- unique(substr(fechas_validas, 4, 5))
  anios <- unique(substr(fechas_validas, 7, 8))
  logf("Meses presentes en columna fecha   : {paste(sort(meses), collapse = ', ')}")
  logf("Años presentes en columna fecha    : {paste(sort(anios), collapse = ', ')}")
}

# ==========================================================
# REPORTE: %NA POR COLUMNA EN CADA ARCHIVO (al final del for)
# ==========================================================
# %NA por columna en cada archivo
logf("\n--- Porcentaje de NA en columnas consolidadas ---")
na_cols_total <- bind_rows(na_cols_report)
for (arch in unique(na_cols_total$archivo)) {
  logf("\nArchivo: {arch}")
  sub <- na_cols_total %>% filter(archivo == arch)
  # Por cada fila, imprime bien
  for (j in seq_len(nrow(sub))) {
    logf(sprintf("  %-12s %6.2f %%", sub$columna[j], sub$pct_na[j]))
  }
}

logf("=== Fin CONSOLIDACIÓN – {format(Sys.time())} ===")
close(con)
cat("→ Reporte processed en:", report_path, "\n")
cat(">>> Fin 1_consolidate.R\n")
