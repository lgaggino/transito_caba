# Tránsito CABA

#### Dashboard Shiny para visualizar el flujo vehicular en los peajes de la Ciudad Autónoma de Buenos Aires (CABA).  
🔗 https://lgaggino.shinyapps.io/transito_caba/

---

## Pasos para la ejecución local del procesamiento y visualización de los datos

1. **Descargar el proyecto.**
2. **Descomprimir los archivos `.rar`** en la carpeta `/raw` para obtener todas las bases crudas.
3. **Ejecutar `0_eda_raw.R`**  
   Genera un reporte `.txt` con la exploración de los datos crudos en la carpeta `/output`.
4. **Ejecutar `1_consolidate.R`**  
   Produce el archivo `consolidado.csv` en la carpeta `/input`.
5. **Ejecutar `2_etl.R`**  
   Genera los archivos intermedios:
   - `consolidado_processed.csv`
   - `categoria_por_dia.csv`
   - `estaciones.csv`
   - `evolucion_pago.csv`
   - `hora_pico_sentido.csv`
   - `transito_por_estacion.csv`  
   Todos se guardan en la carpeta `/input`.
6. **(Opcional)** Ejecutar `2_etl_eda.R` para visualizar en consola la calidad del consolidado.
7. **Ejecutar `3_app.R`**  
   Lanza la aplicación Shiny localmente para explorar los datos.

---

## 1. EDA de datos crudos (`raw/`)

**Script:** `scripts/0_eda_raw.R`  
Descarga los datos desde [aquí]([https://data.buenosaires.gob.ar/dataset/flujo-vehicular-por-unidades-de-peaje-au](https://data.buenosaires.gob.ar/dataset/flujo-vehicular-por-unidades-de-peaje-ausa)) y realiza:

- Inventario de archivos por año (anuales, mensuales, bimestrales), conteo y tamaño total.
- Listado de columnas únicas y años de aparición.
- Verificación de lectura correcta de cada CSV.
- Estadísticas básicas (filas, columnas).
- Detección de inconsistencias de tipo.
- Conteo de valores faltantes.
- Generación de `eda_raw.txt` en `/output` con el resumen completo.

---

## 2. Consolidación inicial (`input/`)

**Script:** `scripts/1_consolidate.R`  
Genera `consolidado.csv`, un archivo más ligero con las columnas:

- `estacion`: IDs y nombres unificados (2008–2020: `estacion`; 2021–2024: `id_peaje`).
- `pago`: formas de pago normalizadas (2008–2020: `forma_pago`; 2021–2024: `tipo_cobro`).
- `categoria`: tipologías vehiculares (2008–2020: `tipo_vehiculo`; 2021–2024: `cat_cobrada`).
- `fecha`: fecha corregida según año y formato original:
  - 2008–2020: columna `fecha`.
  - 2021: combinación de `mes_de_*`, `da_a_de_*`, `aa_o_de_*`.
  - 2022–2024: a partir de `mes_da_a_aa_o_de_fecha_operativa`.
- `sentido`: registros de 2016–2024; rellena con `s/d` para años anteriores.
- `hora`: diversas columnas según año (`HORA`, `hora`, `hora_inicio`, `Hora.HH`).

---

## 3. ETL y preprocesamiento (`input/`)

**Script:** `scripts/2_etl.R`  
A partir de `consolidado.csv` realiza:

- Saneamiento de caracteres (`Latin1 → UTF-8`).
- Filtrado de filas con `NA` y eliminación de líneas “Total”.
- Unificación de nombres (`estacion`, `pago`, `categoria`).
- Enriquecimiento con georreferencias (`raw/georreferencias.csv`).
- Cálculo de `dia`, `mes` y `año` a partir de `fecha`.
- Generación de `consolidado_processed.csv` con columnas limpias.
- División en archivos pre-agregados para Shiny:
  - `hora_pico_sentido.csv`
  - `categoria_por_dia.csv`
  - `evolucion_pago.csv`
  - `transito_por_estacion.csv`
  - `estaciones.csv`

**Script:** `scripts/2_etl_eda.R`  
Valida el ETL:

- Existencia y esquema de archivos generados.
- Conteo y porcentaje de `NA` por columna.
- Consistencia de totales agregados.

---

## 4. Aplicación Shiny

**Scripts involucrados:**

- `scripts/3_app.R`: inicializa `shinyApp(ui, server)`
- `scripts/4_global.R`: carga CSV pre-agregados, define paletas y funciones auxiliares.
- `scripts/5_server.R`: lógica de renderizado de mapas y gráficos.
- `scripts/6_ui.R`: estructura de la interfaz (tabs, controles, outputs).

**La aplicación ofrece:**

- **Flujo vehicular por hora:** mapa interactivo con burbujas escaladas.
- **Categorías vehiculares por día:** gráfico de burbujas (circle packing).
- **Tránsito por estación:** barras anuales filtrables.
- **Modalidad de pago:** streamgraph o línea según filtro.
