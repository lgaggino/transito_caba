library(tidyverse)

# ──────────────────────────────────────────────────────────────────────────────
# 1) Setup básico --------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
args     <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grep("^--file=", args)]
script_dir <- if (length(file_arg)==0) normalizePath(getwd()) else dirname(sub("^--file=", "", file_arg))
project_root <- normalizePath(file.path(script_dir, ".."))
input_dir    <- file.path(project_root, "input")

# ──────────────────────────────────────────────────────────────────────────────
# 2) Lee y analiza consolidado_processed.csv ---------------------------------
# ──────────────────────────────────────────────────────────────────────────────
consol_path <- file.path(input_dir, "consolidado_processed.csv")
if (!file.exists(consol_path)) stop("No encontré ", consol_path)

df <- read_csv(consol_path, show_col_types = FALSE)
cat("✔ Dimensiones del consolidado:", nrow(df),
    "filas x", ncol(df), "columnas\n\n")

# 2.1 Comprobación georreferencias
required_geo <- c("latitud","longitud")
missing_geo  <- setdiff(required_geo, names(df))
if (length(missing_geo)) stop("Faltan en consolidado:", paste(missing_geo, collapse=", "))
na_geo <- sapply(df[required_geo], function(x) sum(is.na(x)))
cat("=== NA en georreferencias ===\n"); print(na_geo)
if (any(na_geo>0)) {
  cat("⚠ Estas estaciones no tuvieron geo:\n")
  print(df %>% filter(is.na(latitud)|is.na(longitud)) %>% distinct(estacion))
} else cat("✔ Todas las estaciones tienen lat/long.\n\n")

# 2.2 Chequeo de 'dia'
if (!"dia" %in% names(df)) stop("No encontré la columna 'dia'")
cat("=== NA en 'dia':", sum(is.na(df$dia)), "\n")
cat("=== Frecuencia por día ===\n")
print(df %>% count(dia) %>% 
        arrange(match(dia,
                      c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))))

# 2.3 NA y %NA por columna
na_por_col <- sapply(df, function(x) sum(is.na(x)))
cat("\n=== NA por columna ===\n");    print(na_por_col)
cat("\n=== %NA por columna ===\n"); print(round(100 * na_por_col / nrow(df),2))

# 2.4 Valores repetidos
cat("\n=== Valores repetidos (>1) por columna ===\n")
for(col in names(df)){
  tab <- table(df[[col]], useNA="ifany")
  repes <- tab[tab>1]
  if (length(repes)>0) {
    cat(sprintf("\nColumna %s (reps: %d)\n", col, length(repes)))
    print(repes)
  } else {
    cat(sprintf("\nColumna %s: sin repetidos\n", col))
  }
}
cat("\n✔ Análisis completo de consolidado terminado.\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 3) Función genérica para cada CSV extra --------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
chequear_csv <- function(path, req_cols=character()){
  cat("→ Leyendo:", basename(path), "\n")
  if (!file.exists(path)) stop("   ✗ No encontré ", path)
  d <- read_csv(path, show_col_types = FALSE)
  cat("   dim:", nrow(d),"x",ncol(d),"\n")
  na_c <- sapply(d, function(x) sum(is.na(x)))
  cat("   NA por col:\n");    print(na_c)
  cat("   %NA por col:\n");  print(round(100*na_c/nrow(d),2))
  if (length(req_cols)){
    falt <- setdiff(req_cols, names(d))
    if (length(falt)) {
      cat("   ⚠ Faltan cols:", paste(falt,collapse=", "), "\n")
    } else {
      cat("   ✔ Tiene cols:", paste(req_cols,collapse=", "), "\n")
    }
  }
  cat("\n")
}

# ──────────────────────────────────────────────────────────────────────────────
# 4) Recorre CSVs pre-agrupados ------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
archivos <- list(
  hora_pico_sentido  = c("estacion","hora","sentido","año","conteo","latitud","longitud"),
  categoria_por_dia  = c("mes","dia","categoria","conteo"),
  transito_por_estacion = c("estacion","año","conteo"),
  evolucion_pago     = c("año","pago","estacion","conteo"),
  estaciones         = c("estacion","latitud","longitud")
)

for (nm in names(archivos)) {
  path <- file.path(input_dir, paste0(nm, ".csv"))
  cat("=== ", nm, "===\n")
  chequear_csv(path, archivos[[nm]])
}

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────