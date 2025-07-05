# ──────────────────────────────────────────────────────────────────────────────
# 0) Inicializar y lanzar la aplicación Shiny ---------------------------------
# ──────────────────────────────────────────────────────────────────────────────
source("scripts/4_global.R")
source("scripts/5_server.R")
source("scripts/6_ui.R")

shinyApp(ui, server)

# ──────────────────────────────────────────────────────────────────────────────
# 1) Deployment ---------------------------------
# ──────────────────────────────────────────────────────────────────────────────

#options(timeout = 1000)

#files_input <- list.files("C:/Users/Lionel/Desktop/Desarrollos/R/transito_caba/input",
                          #pattern = "\\.csv$", full.names = FALSE)
#files_input <- file.path("input", files_input)

#rsconnect::deployApp(
  #appDir        = "C:/Users/Lionel/Desktop/Desarrollos/R/transito_caba",
  #appName       = "transito_caba",
  #account       = "lgaggino",
  #appPrimaryDoc = "scripts/3_app.R",
  #appFiles      = c(
    #"scripts/3_app.R",
    #"scripts/4_global.R",
    #"scripts/5_server.R",
    #"scripts/6_ui.R",
    #"www/styles.css",
    #files_input
  #)
#)

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────


