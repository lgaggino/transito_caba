Tránsito CABA

Dashboard Shiny para visualizar el flujo vehicular en los peajes de la Ciudad Autónoma de Buenos Aires (CABA).

Estructura de directorios
transito-caba/
├── raw/                    # Archivos CSV originales descargados de open data (flujo vehicular)
├── www/                    # Recursos estáticos (CSS, favicon)
│   └── styles.css          # Estilos personalizados de la aplicación
├── input/                  # Archivos intermedios generados (consolidado, procesados, pre-agregados)
├── output/                 # Salidas de reportes y logs (EDA, etc.)
├── scripts/                # Scripts de procesamiento, EDA y aplicación
│   ├── 0_eda_raw.R         # EDA de los datos crudos (inventario, columnas, missing)
│   ├── 1_consolidate.R     # Generación de `consolidado.csv` unificando años/formatos
│   ├── 2_etl.R             # ETL: saneamiento, limpieza y enriquecimiento con geolocalización
│   ├── 2_etl_eda.R         # EDA sobre el consolidado procesado (validación de esquema y datos)
│   ├── 3_app.R             # Inicializa ShinyApp (ui + server)
│   ├── 4_global.R          # Carga de datos pre-agregados y funciones auxiliares
│   ├── 5_server.R          # Lógica `server` de la aplicación
│   └── 6_ui.R              # Definición de la interfaz `ui`
└── README.md               # Documentación del proyecto

1. EDA de datos crudos (raw/)
Script: scripts/0_eda_raw.R
Descargando los datos desde https://data.buenosaires.gob.ar/dataset/flujo-vehicular-por-unidades-de-peaje-ausa,
el script realiza:
Inventario de archivos por año (anuales, mensuales, bimestrales), conteo y tamaño total.
Listado de columnas únicas y años de aparición.
Verificación de lectura correcta de cada CSV.
Estadísticas básicas (filas, columnas).
Detección de inconsistencias de tipo.
Conteo de valores faltantes.
Generación de eda_raw.txt en output/ con el resumen completo.

3. Consolidación inicial (input/)
Script: scripts/1_consolidate.R
Genera consolidado.csv, un archivo más ligero con las columnas:
estacion: IDs y nombres unificados (2008–2020: estacion; 2021–2024: id_peaje).
pago: formas de pago normalizadas (2008–2020: forma_pago; 2021–2024: tipo_cobro).
categoria: tipologías vehiculares (2008–2020: tipo_vehiculo; 2021–2024: cat_cobrada).
fecha: fecha corregida según formato año:
2008–2020: columna fecha.
2021: combinación de mes_de_*, da_a_de_*, aa_o_de_*.
2022–2024: a partir de mes_da_a_aa_o_de_fecha_operativa.
sentido: registros de 2016–2024; rellena con s/d para años anteriores.
hora: diversas columnas según año (HORA, hora, hora_inicio, Hora.HH).

4. ETL y preprocesamiento (input/)
Script: scripts/2_etl.R
A partir de consolidado.csv realiza:
Saneamiento de caracteres (Latin1 → UTF-8).
Filtrado de filas con NA y eliminación de líneas “Total”.
Unificación de nombres (estacion, pago, categoria).
Enriquecimiento con georreferencias (raw/georreferencias.csv).
Cálculo de dia, mes y año a partir de fecha.
Generación de consolidado_processed.csv con columnas:estacion, pago, categoria, fecha, sentido, hora, latitud, longitud, dia, año.
División en archivos pre-agregados para Shiny (hora_pico_sentido.csv, categoria_por_dia.csv, etc.).

Script: scripts/2_etl_eda.RValida el ETL:
Existencia y esquema de archivos generados.
Conteo y porcentaje de NA por columna.
Consistencia de totales agregados.

4. Aplicación Shiny
Scripts:
scripts/3_app.R: inicialización de shinyApp(ui, server).
scripts/4_global.R: carga de CSV pre-agregados y definición de paletas y funciones.
scripts/5_server.R: lógica de renderizado de mapas y gráficos.
scripts/6_ui.R: estructura de la interfaz (tab panels, controles, outputs).
La aplicación ofrece:
Flujo vehicular por hora: mapa interactivo con burbujas escaladas.
Categorías vehiculares por día: gráfico de burbujas (circle packing).
Tránsito por estación: barras anuales filtrables.
Modalidad de pago: streamgraph o línea según filtro.

