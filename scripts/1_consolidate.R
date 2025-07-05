################################################################################
# Consolida archivos de tránsito en Peajes de la Ciudad de Buenos Aires        #
# - Unifica columnas: estacion, pago, categoría, fecha, sentido, hora          #
# - Genera archivo procesado y reporte detallado de consolidación              #
################################################################################

# ──────────────────────────────────────────────────────────────────────────────
# 0) Carga de librerías --------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
  library(data.table)
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(glue)
  library(janitor)

# ──────────────────────────────────────────────────────────────────────────────
# 1) Definición de rutas -------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
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
if (n_files == 0) stop("No encontré ningún CSV en ", raw_dir)

# ──────────────────────────────────────────────────────────────────────────────
# 2) Funciones auxiliares para lectura flexible de CSV -------------------------
# ──────────────────────────────────────────────────────────────────────────────
# Estas funciones detectan el separador del archivo y prueban distintos encodings
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

# ──────────────────────────────────────────────────────────────────────────────
# 3) Inicialización de listas y variables --------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# Prepara estructuras para almacenar los resultados de cada archivo
sum_raw_rows <- 0L
list_est   <- vector("list", n_files)
list_pago  <- vector("list", n_files)
list_cat   <- vector("list", n_files)
list_fecha <- vector("list", n_files)
list_sentido <- vector("list", n_files)
list_hora    <- vector("list", n_files)
na_cols_report <- list()

bad_headers <- c(
  "Transito Enero 2023.csv",
  "Transito Marzo 2023.csv",
  "Transito Noviembre 2022.csv",
  "Transito Octubre 2022.csv"
)

# ──────────────────────────────────────────────────────────────────────────────
# 4) Procesamiento principal de archivos ---------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# Itera sobre cada CSV detectado, lo lee, unifica columnas y acumula 
# los resultados.
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
  
  # Normaliza nombres y encoding
  names(dt) <- janitor::make_clean_names(names(dt))
  
  dt <- dt %>% mutate(across(where(is.character),
                             ~ iconv(.x, from="latin1", to="UTF-8", sub="")))
  
  n <- nrow(dt)
  sum_raw_rows <- sum_raw_rows + n
  cat("   → leído, dim:", n, ncol(dt), "\n")
  
  # Completa columnas faltantes
  if (!"estacion"      %in% names(dt)) dt$estacion      <- NA_character_
  if (!"id_peaje"      %in% names(dt)) dt$id_peaje      <- NA_character_
  if (!"forma_pago"    %in% names(dt)) dt$forma_pago    <- NA_character_
  if (!"tipo_cobro"    %in% names(dt)) dt$tipo_cobro    <- NA_character_
  if (!"tipo_vehiculo" %in% names(dt)) dt$tipo_vehiculo <- NA_character_
  if (!"cat_cobrada"   %in% names(dt)) dt$cat_cobrada   <- NA_character_
  if (!"fecha"         %in% names(dt)) dt$fecha         <- NA_character_
  
  # Rutas de unificación básica
  vec_est   <- coalesce(dt$id_peaje, dt$estacion)
  vec_cat   <- if (yr <= 2020) dt$tipo_vehiculo else dt$cat_cobrada
  
  # ────────────────────────────────────────────────────────────────────────────
  # 4.1) Cabeceras rotas: manejo especial --------------------------------------
  # ────────────────────────────────────────────────────────────────────────────
  if (nm %in% bad_headers) {
    cat("   → CABECERA ROTA: usando posiciones 2=hora, 3=categoria, 6=sentido, 7=pago\n")
    vec_hora    <- str_pad(as.character(dt[[2]]), 2, pad="0")
    vec_cat     <- as.character(dt[[3]])
    vec_sentido <- as.character(dt[[6]])
    vec_pago    <- as.character(dt[[7]])
    vec_est     <- coalesce(dt$id_peaje, dt$estacion, as.character(dt[[2]]))
    vec_fecha <- rep(NA_character_, n)
    # ──────────────────────────────────────────────────────────────────────────
    # 4.2) Unificación de columnas ---------------------------------------------
    # ──────────────────────────────────────────────────────────────────────────
  } else {
    # 4.2.1) Unifica estación
    id   <- if ("id_peaje" %in% names(dt))    as.character(dt$id_peaje)    else rep(NA_character_, n)
    est  <- if ("estacion" %in% names(dt))    as.character(dt$estacion)   else rep(NA_character_, n)
    col2 <- as.character(dt[[2]])
    vec_est <- coalesce(id, est, col2)
    cat("   → estacion extraída con: coalesce(id_peaje, estacion, columna #2)\n")

    # 4.2.2) Unifica pago
    fp <- if ("forma_pago" %in% names(dt)) as.character(dt$forma_pago) else rep(NA_character_, n)
    tc <- if ("tipo_cobro" %in% names(dt)) as.character(dt$tipo_cobro) else rep(NA_character_, n)
    vec_pago <- coalesce(fp, tc)
    cat("   → pago extraído con: coalesce(forma_pago, tipo_cobro)\n")
    
    # 4.2.3) Unifica categoría
    tv <- if ("tipo_vehiculo" %in% names(dt)) as.character(dt$tipo_vehiculo) else rep(NA_character_, n)
    cc <- if ("cat_cobrada"   %in% names(dt)) as.character(dt$cat_cobrada)   else rep(NA_character_, n)
    vec_cat <- coalesce(tv, cc)
    cat("   → categoria extraída con: coalesce(tipo_vehiculo, cat_cobrada)\n")
    
    # 4.2.4) Unifica sentido
    if (!is.na(yr) && yr >= 2016) {
      vec_sentido <- if ("sentido" %in% names(dt)) as.character(dt$sentido) else rep(NA_character_, n)
    } else {
      vec_sentido <- rep("s/d", n)
    }
    cat("   → sentido extraído con: ",
        if (yr>=2016) "'sentido' ó NA" else "\"s/d\"", "\n")
    
    # 4.2.5) Unificar hora
    h1 <- intersect(c("hora", "hora_inicio", "hora_hh"), names(dt))
    if (length(h1)==1) {
      vec_hora <- dt[[h1]] %>% as.character()
      cat("   → hora extraída de:", h1, "\n")
    } else {
      vec_hora <- rep(NA_character_, n)
      cat("   → hora extraída: NINGUNA (falta columna)\n")
    }
    # Homogeneiza a formato HH
    vec_hora <- str_pad(str_extract(vec_hora, "^\\d{1,2}"), 2, pad="0")
    
    # ──────────────────────────────────────────────────────────────────────────
    # 5) Cálculo de vec_fecha para todos los casos -----------------------------
    # ──────────────────────────────────────────────────────────────────────────
    # Calcula y formatea la variable vec_fecha según múltiples formatos de fecha,
    # adaptándose al año del archivo y a las columnas disponibles en cada caso.
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
    
    # 4) 2022 casos especiales de texto "d/m/aaaa"
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
    
    # 5) 2022 Jul/Sep – texto “X de <mes> de YYYY” + fallback ISO
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
    
    # 6) 2022-2023: columna 'v1' en "d/m/aaaa"
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
    
    # 7) 2022-2023: columna 'v1' en "d de mes de yyyy"
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
    
    # 8) 2023: 'mes_dia_ano_de_fecha_operativa' en d/m/aaaa
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
    
    # 9) 2023 Junio – “mes_dia_ano_de_fecha_operativa”
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
    
# ──────────────────────────────────────────────────────────────────────────────
# 6) Determinar sentido y guardar resultados por archivo -----------------------
# ──────────────────────────────────────────────────────────────────────────────
# Según el año y la presencia de la columna 'sentido', asignamos vec_sentido.
# Luego armamos tibbles por archivo para estacion, pago, categoría, fecha, sentido y hora.
  # Detectaa año
  yr <- str_extract(nm, "\\d{4}") %>% as.integer()
    
  # Asigna vec_sentido según año y existencia de la columna
  if (yr >= 2016 && "sentido" %in% names(dt)) {
    vec_sentido <- dt$sentido
  } else if (yr >= 2016 && !"sentido" %in% names(dt)) {
    # Año >=2016 que debería tener 'sentido' pero no existe
    vec_sentido <- rep(NA_character_, n)
  } else {
    # Antes de 2016, no hay sentido
    vec_sentido <- rep("s/d", n)
  }
    # Guarda cada vector en su lista correspondiente
    list_est[[i]]   <- tibble(file=nm, nrows=n, estacion=vec_est)
    list_pago[[i]]  <- tibble(file=nm, nrows=n, pago=vec_pago)
    list_cat[[i]]   <- tibble(file=nm, nrows=n, categoria=vec_cat)
    list_fecha[[i]] <- tibble(file=nm, nrows=n, fecha=vec_fecha)
    list_sentido[[i]] <- tibble(file = nm, nrows = n, sentido = vec_sentido)
    list_hora[[i]] <- tibble(file = nm, nrows = n, hora = vec_hora)
    
    cat("   → columnas unificadas OK\n\n")
    
    # Calcular porcentaje de NA en las columnas consolidadas
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
}

# ──────────────────────────────────────────────────────────────────────────────
# 7) Consolidado final y exportación -------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# Combina todas las listas en data.frames, arma el tibble final y lo escribe a CSV.
df_est   <- bind_rows(list_est)
df_pago  <- bind_rows(list_pago)
df_cat   <- bind_rows(list_cat)
df_fecha <- bind_rows(list_fecha)
df_sentido <- bind_rows(list_sentido)
df_hora    <- bind_rows(list_hora)

consol <- tibble(
  estacion  = df_est$estacion,
  pago      = df_pago$pago,
  categoria = df_cat$categoria,
  fecha     = df_fecha$fecha,
  sentido   = df_sentido$sentido,
  hora      = df_hora$hora 
)

out_csv <- file.path(proc_dir, "consolidado.csv")
write_csv(consol, out_csv)
cat("→ CSV consolidado en:", out_csv, "\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 7) Generación de reporte procesado -------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# En este bloque se crea y escribe un log resumen con:
# - estadísticas de consolidación (filas totales y NAs por columna)
# - listado de meses y años presentes en las fechas válidas
# - porcentaje de NA por columna en cada archivo procesado
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
na_hora    <- sum(is.na(consol$hora))
pct_est    <- round(100 * na_est / total_cons, 2)
pct_pago   <- round(100 * na_pago / total_cons, 2)
pct_cat    <- round(100 * na_cat / total_cons, 2)
pct_fecha  <- round(100 * na_fecha / total_cons, 2)
pct_sent   <- round(100 * na_sent / total_cons, 2)
pct_hora <- round(100 * na_hora  / total_cons, 2)

logf("Total filas consolidadas    : {total_cons}")
logf("Total NA en estacion        : {na_est}  ({pct_est} %)")
logf("Total NA en pago            : {na_pago}  ({pct_pago} %)")
logf("Total NA en categoria       : {na_cat}  ({pct_cat} %)")
logf("Total NA en fecha           : {na_fecha}  ({pct_fecha} %)\n")
logf("Total NA en sentido          : {na_sent}  ({pct_sent} %)")
logf("Total NA en hora            : {na_hora}  ({pct_hora} %)\n")

# Resumen de meses y años cargados (solo fechas válidas en formato dd/mm/aa)
if (any(!is.na(consol$fecha))) {
  fechas_validas <- na.omit(consol$fecha[grepl("^\\d{2}/\\d{2}/\\d{2}$", consol$fecha)])
  meses <- unique(substr(fechas_validas, 4, 5))
  anios <- unique(substr(fechas_validas, 7, 8))
  logf("Meses presentes en columna fecha   : {paste(sort(meses), collapse = ', ')}")
  logf("Años presentes en columna fecha    : {paste(sort(anios), collapse = ', ')}")
}

# REPORTE: %NA POR COLUMNA EN CADA ARCHIVO (al final del for)
logf("\n--- Porcentaje de NA en columnas consolidadas ---")
na_cols_total <- bind_rows(na_cols_report)
# Agrega también NA de la columna hora
na_hora_total <- bind_rows(
  lapply(seq_along(list_hora), function(i) {
    tibble(
      archivo = list_hora[[i]]$file[1],
      columna = "hora",
      pct_na  = round(100 * mean(is.na(list_hora[[i]]$hora)), 2)
    )
  })
)
na_cols_total <- bind_rows(na_cols_total, na_hora_total)

for (arch in unique(na_cols_total$archivo)) {
  logf("\nArchivo: {arch}")
  sub <- na_cols_total %>% filter(archivo == arch)
  for (j in seq_len(nrow(sub))) {
    logf(sprintf("  %-12s %6.2f %%", sub$columna[j], sub$pct_na[j]))
  }
}

logf("=== Fin CONSOLIDACIÓN – {format(Sys.time())} ===")
close(con)
cat("→ Reporte processed en:", report_path, "\n")
cat(">>> Fin 1_consolidate.R\n")

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────