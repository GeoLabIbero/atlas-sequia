# El Niño y el campo mexicano
# GEOLab, IBERO Ciudad de México
#
# Esta carpeta (app/) es la que se publica: app.R, datos_app.rds y www/.
# Los datos los genera preparar_datos.R. Para correrla localmente:
#   shiny::runApp("app")

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sf)
library(leaflet)

# ---------------------------------------------------------------------------
# Datos: ya calculados por preparar_datos.R
# ---------------------------------------------------------------------------

list2env(readRDS("datos_app.rds"), environment())
d <- st_drop_geometry(mun)

# Años de inicio de episodios El Niño: MEI.v2 >= +0.5 durante al menos 5 bimestres (NOAA PSL)
anios_nino <- c(2002, 2006, 2009, 2015, 2023)

# ---------------------------------------------------------------------------
# Formatos para el lector
# ---------------------------------------------------------------------------

redondo <- function(x) {
  if (length(x) == 0 || is.na(x)) return("sin dato")
  if (x >= 1e6) return(paste(format(round(x / 1e6, 1), nsmall = 1), "millones"))
  if (x >= 1e4) return(paste(round(x / 1e3), "mil"))
  format(round(x), big.mark = ",")
}
pct    <- function(x) if (length(x) == 0 || is.na(x)) "sin dato" else sprintf("%.0f%%", x)
wmean  <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
fila   <- function(a, b) tags$tr(tags$td(a), tags$td(tags$b(b)))
en_cat <- function(v, cortes, etiq) {
  if (length(v) == 0 || is.na(v)) "sin dato" else as.character(cut(v, cortes, labels = etiq))
}

# Un corte solo filtra si lo moviste de su mínimo
cumple <- \(x, umbral, minimo) if (umbral <= minimo) TRUE else !is.na(x) & x >= umbral

# Rango de los cortes de la FAO, en años de cada 100
fao_max_agri <- ceiling(max(d$fao_agri, na.rm = TRUE) * 100)
fao_max_pec  <- ceiling(max(d$fao_pec,  na.rm = TRUE) * 100)

# Números fijos del relato
n <- list(
  episodios = nrow(episodios),
  mei_2026  = max(mei$mei[floor(mei$fecha) == 2026], na.rm = TRUE)
)

# ---------------------------------------------------------------------------
# Capas del mapa: categorías con etiquetas que se leen solas
# ---------------------------------------------------------------------------

# Un color por significado, igual en todas las pestañas:
#   satélite FAO = naranjas · registro de México = rojos · pobreza = magentas
#   personas = azules · producción = morados · temporal = verdes · riego = turquesas · pastizal = cafés
verdes_col  <- c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#006d2c")
turq_col    <- c("#f6eff7", "#bdc9e1", "#67a9cf", "#1c9099", "#016c59")
cafes_col   <- c("#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404")
morados_col <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#756bb1", "#54278f")

z_cortes   <- c(-Inf, -1, -0.5, -0.1, 0.1, 0.5, 1, Inf)
z_etiq     <- c("Mucho más seco", "Más seco", "Algo más seco", "Casi igual",
                "Algo más húmedo", "Más húmedo", "Mucho más húmedo")
z_col      <- c("#8c510a", "#d8b365", "#f6e8c3", "#eeeeee", "#c7eae5", "#5ab4ac", "#01665e")

fao_cortes <- c(-Inf, 0.2001, 0.25, 0.30, 0.35, Inf)
fao_etiq   <- c("20 o menos", "Entre 20 y 25", "Entre 25 y 30", "Entre 30 y 35", "Más de 35")
fao_col    <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000")

mon_cortes <- c(-Inf, 0.1, 0.2, 0.3, 0.4, 0.5, Inf)
mon_etiq   <- c("Menos de 10%", "10 a 20%", "20 a 30%", "30 a 40%", "40 a 50%", "Más de 50%")
mon_col    <- c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")

pob_cortes <- c(-Inf, 20, 40, 60, 80, Inf)
pob_etiq   <- c("Menos de 20%", "20 a 40%", "40 a 60%", "60 a 80%", "Más de 80%")
pob_col    <- c("#feebe2", "#fbb4b9", "#f768a1", "#c51b8a", "#7a0177")

uso_cortes <- c(-Inf, 5, 10, 20, 40, Inf)
uso_etiq   <- c("Menos de 5%", "5 a 10%", "10 a 20%", "20 a 40%", "Más de 40%")

agr_cortes <- c(-Inf, 100, 500, 1000, 2500, 5000, Inf)
agr_etiq   <- c("Menos de 100", "100 a 500", "500 a 1,000", "1,000 a 2,500", "2,500 a 5,000", "Más de 5,000")
agr_col    <- c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")

gan_cortes <- c(-Inf, 25, 50, 100, 250, 500, Inf)
gan_etiq   <- c("Menos de 25", "25 a 50", "50 a 100", "100 a 250", "250 a 500", "Más de 500")
gan_col    <- agr_col

capas <- list(
  pobreza      = list(cortes = pob_cortes, etiq = pob_etiq, col = pob_col,
                      titulo = "Población en pobreza (2020, % de habitantes)", frase = "de la población en pobreza"),
  agricultura  = list(cortes = agr_cortes, etiq = agr_etiq, col = agr_col,
                      titulo = "Personas que trabajan en la agricultura", frase = "personas trabajan en la agricultura"),
  ganaderia    = list(cortes = gan_cortes, etiq = gan_etiq, col = gan_col,
                      titulo = "Personas que trabajan con ganado", frase = "personas trabajan con ganado"),
  pct_temp     = list(cortes = uso_cortes, etiq = uso_etiq, col = verdes_col,
                      titulo = "Agricultura de temporal (% del territorio)", frase = "del territorio es agricultura de temporal"),
  pct_riego    = list(cortes = uso_cortes, etiq = uso_etiq, col = turq_col,
                      titulo = "Agricultura de riego (% del territorio)", frase = "del territorio es agricultura de riego"),
  pct_pasto    = list(cortes = uso_cortes, etiq = uso_etiq, col = cafes_col,
                      titulo = "Pastizal (% del territorio)", frase = "del territorio es pastizal"),
  fao_agri     = list(cortes = fao_cortes, etiq = fao_etiq, col = fao_col,
                      titulo = "Años con sequía severa en los cultivos, de cada 100 (satélite FAO)",
                      frase = "de cada 100 años con sequía severa en sus cultivos (satélite FAO)"),
  fao_pec      = list(cortes = fao_cortes, etiq = fao_etiq, col = fao_col,
                      titulo = "Años con sequía severa en los pastizales, de cada 100 (satélite FAO)",
                      frase = "de cada 100 años con sequía severa en sus pastizales (satélite FAO)"),
  monitor_agri = list(cortes = mon_cortes, etiq = mon_etiq, col = mon_col,
                      titulo = "Tiempo en sequía en la temporada de siembra (registro de México)",
                      frase = "del tiempo en sequía en la temporada de siembra (registro de México)"),
  monitor_pec  = list(cortes = mon_cortes, etiq = mon_etiq, col = mon_col,
                      titulo = "Tiempo en sequía durante el año (registro de México)",
                      frase = "del tiempo en sequía durante el año (registro de México)")
)

quintil_etiq <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta")

# Cinco grupos del mismo tamaño, para producción y valor
quintiles <- function(v, colores) {
  q <- unique(quantile(v, probs = seq(0, 1, 0.2), na.rm = TRUE))
  if (length(q) < 2) q <- c(q[1] - 1, q[1])
  k <- length(q) - 1
  list(cat = cut(v, q, labels = quintil_etiq[seq_len(k)], include.lowest = TRUE),
       col = colores[seq_len(k)])
}

clasifica <- function(v, var) {
  if (var %in% c("prod", "valor")) {
    q <- quintiles(v, morados_col)
    return(list(cat = q$cat, col = q$col,
                titulo = titulo_var(var),
                frase  = paste("producción", tolower(as.character(q$cat)))))
  }
  k   <- capas[[var]]
  cat <- cut(v, k$cortes, labels = k$etiq)
  list(cat = cat, col = k$col, titulo = k$titulo, frase = paste(as.character(cat), k$frase))
}

# El nombre de cada capa, para leyendas y encabezados
titulo_var <- function(var) {
  if (var == "prod")  return("Producción del cultivo en 2025 (toneladas)")
  if (var == "valor") return("Valor de lo que produce en 2025")
  capas[[var]]$titulo
}

# Los ciclos con un nombre que se entienda fuera del sector
etiqueta_ciclo <- function(ci) setNames(ci, ifelse(ci == "Perennes", "Perennes (cultivos de todo el año)", ci))

# Variables del mapa, agrupadas por la pregunta que responden
vars_agri <- list(
  "¿Quién vive ahí?"     = c("Población en pobreza" = "pobreza",
                             "Personas que trabajan en la agricultura" = "agricultura"),
  "¿Qué se siembra?"     = c("Agricultura de temporal" = "pct_temp",
                             "Agricultura de riego" = "pct_riego",
                             "Producción del cultivo en 2025" = "prod"),
  "¿Qué amenaza?"        = c("Años con sequía severa (satélite FAO)" = "fao_agri",
                             "Tiempo en sequía (registro de México)" = "monitor_agri")
)

vars_gan <- list(
  "¿Quién vive ahí?"     = c("Población en pobreza" = "pobreza",
                             "Personas que trabajan con ganado" = "ganaderia"),
  "¿Qué se produce?"     = c("Pastizal" = "pct_pasto",
                             "Valor de lo que produce en 2025" = "valor"),
  "¿Qué amenaza?"        = c("Años con sequía severa en pastizales (satélite FAO)" = "fao_pec",
                             "Tiempo en sequía durante el año (registro de México)" = "monitor_pec")
)

especies <- c("Vacas, aves y cerdos" = "todas", "Vacas" = "bovino", "Aves" = "ave", "Cerdos" = "porcino")

# ---------------------------------------------------------------------------
# Piezas comunes de los mapas
# ---------------------------------------------------------------------------

resalte  <- highlightOptions(weight = 2.5, color = "white", opacity = 1, bringToFront = TRUE)
sin_clic <- pathOptions(pointerEvents = "none")

mapa_base <- function() {
  leaflet() |>
    addProviderTiles("Esri.WorldImagery",    group = "Satélite") |>
    addProviderTiles("Esri.WorldGrayCanvas", group = "Mapa claro") |>
    addLayersControl(baseGroups = c("Satélite", "Mapa claro"), position = "topleft",
                     options = layersControlOptions(collapsed = TRUE)) |>
    setView(lng = -102, lat = 23.5, zoom = 5)
}

# Pinta los municipios con su categoría, los límites estatales y la leyenda
dibuja <- function(proxy, m, cl, leyenda) {
  ok  <- !is.na(cl$cat)
  m   <- m[ok, ]
  pal <- colorFactor(cl$col, levels = levels(cl$cat))
  proxy |>
    addPolygons(data = m, layerId = m$CVEGEO, fillColor = pal(cl$cat[ok]), fillOpacity = 0.75,
                stroke = TRUE, weight = 0, color = "white", highlightOptions = resalte,
                label = paste0(m$municipio, ", ", m$entidad_federativa, ": ", cl$frase[ok])) |>
    addPolylines(data = estados, color = "white", weight = 1.2, opacity = 0.9, options = sin_clic) |>
    addLegend(layerId = leyenda, pal = pal, values = factor(levels(cl$cat), levels = levels(cl$cat)),
              title = cl$titulo, position = "bottomright", opacity = 0.9)
}

# ---------------------------------------------------------------------------
# Perfil del municipio: todo en porcentaje, agrupado por pregunta
# ---------------------------------------------------------------------------

perfil_dic <- tibble(
  etiqueta = c("Población en pobreza (% de habitantes)",
               "Agricultura de temporal (% del territorio)", "Agricultura de riego (% del territorio)",
               "Pastizal (% del territorio)",
               "Cultivos con sequía severa (% de los años)", "Pastizales con sequía severa (% de los años)",
               "Tiempo en sequía, temporada de siembra (%)", "Tiempo en sequía, todo el año (%)"),
  tema     = c("Pobreza", rep("Uso de suelo (INEGI)", 3),
               rep("Satélite (FAO)", 2), rep("Registro de México", 2)))

grafica_perfil <- function(cv) {
  x <- d[d$CVEGEO == cv, ][1, ]
  perfil_dic |>
    mutate(valor    = c(x$pobreza, x$pct_temp, x$pct_riego, x$pct_pasto, x$fao_agri * 100,
                        x$fao_pec * 100, x$monitor_agri * 100, x$monitor_pec * 100),
           etiqueta = factor(etiqueta, levels = rev(perfil_dic$etiqueta)),
           texto    = if_else(is.na(valor), "sin dato", sprintf("%.0f%%", valor))) |>
    ggplot(aes(valor, etiqueta, fill = tema)) +
    geom_col(width = 0.7, na.rm = TRUE) +
    geom_text(aes(x = coalesce(valor, 0), label = texto), hjust = -0.1, size = 4.2) +
    scale_x_continuous(limits = c(0, 115), breaks = seq(0, 100, 25)) +
    scale_fill_manual(values = c("Pobreza" = "#c51b8a", "Uso de suelo (INEGI)" = "#27ae60",
                                 "Satélite (FAO)" = "#e67e22", "Registro de México" = "#de2d26"),
                      breaks = c("Pobreza", "Uso de suelo (INEGI)", "Satélite (FAO)", "Registro de México")) +
    labs(title    = x$municipio,
         subtitle = sprintf("%s · Población 2025: %s\n%s trabajan en la agricultura y %s con ganado",
                            x$entidad_federativa, redondo(x$poblacion), redondo(x$agricultura),
                            redondo(x$ganaderia)),
         x = NULL, y = NULL, fill = NULL,
         caption = "Satélite FAO: % de los años con sequía severa. Registro de México: % del tiempo en sequía, 2016-2025.") +
    theme_minimal(base_size = 14) +
    guides(fill = guide_legend(nrow = 2)) +
    theme(legend.position    = "top",
          plot.title         = element_text(face = "bold"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank())
}

intro <- function(titulo, ...) {
  card(card_body(h4(titulo, style = "font-weight: 700;"),
                 div(style = "font-size: 1.05rem; line-height: 1.6;", ...)))
}

# ---------------------------------------------------------------------------
# Interfaz
# ---------------------------------------------------------------------------

ui <- page_navbar(
  id       = "pestana",
  fillable = FALSE,
  title    = tags$span(tags$img(src = "logo_geolab.png", height = "40px", style = "margin-right: 10px;"),
                       "El Niño y el campo mexicano"),
  window_title = "El Niño y el campo mexicano · GEOLab IBERO",
  theme = bs_theme(bootswatch = "flatly"),
  header = tags$head(tags$style(HTML("
    .navbar-brand { margin-right: 40px; }
    .navbar-nav .nav-link { padding-left: 16px !important; padding-right: 16px !important; }
  "))),

  # --- El Niño ----------------------------------------------------------------
  nav_panel("El Niño", value = "nino",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("estacion", "¿Qué temporada ver?",
                     c("Verano (junio a agosto)" = "verano", "Invierno (diciembre a febrero)" = "invierno")),
        p(class = "small text-muted",
          "El mapa compara la lluvia de los años Niño con la de un año normal, municipio por municipio.",
          "Fuente: CHIRPS, años Niño desde 1982.")
      ),
      intro("¿Qué es El Niño y qué le hace a la lluvia?",
        p("Cada pocos años, el agua del océano Pacífico frente a Sudamérica se calienta más de lo normal.",
          "A eso se le llama El Niño. Aunque ocurre lejos, cambia la lluvia en México: en invierno suele",
          "traer más lluvia al norte, y en verano tiende a secar el centro y el sur, justo cuando se",
          "siembra el maíz de temporal."),
        p(sprintf("Desde 1982 ha habido %d episodios completos de El Niño. En 2026 se desarrolla uno nuevo, de los más intensos registrados para esta época del año.",
                  n$episodios))),
      layout_columns(
        value_box(title = "Episodios completos de El Niño desde 1982", value = n$episodios),
        value_box(title = "Intensidad máxima de El Niño en 2026, hasta ahora", value = sprintf("+%.1f", n$mei_2026),
                  p("En la escala de la NOAA. A partir de +0.5 ya se considera El Niño."))
      ),
      card(full_screen = TRUE, card_header(textOutput("t_mapa0", inline = TRUE)),
           leafletOutput("mapa0", height = "540px")),
      card(full_screen = TRUE, card_header("El Pacífico, año con año: cuándo hubo El Niño"),
           plotOutput("mei", height = "320px"))
    )
  ),

  # --- Agricultura ------------------------------------------------------------
  nav_panel("Agricultura", value = "municipios",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("cultivo", "Cultivo", choices = sort(cultivos16), selected = "Maíz grano"),
        p(class = "small text-muted",
          "Son los 16 cultivos que más se produjeron en 2025, el último año cerrado del SIAP: los 15 con",
          "más toneladas, sin contar forrajes, más el frijol por su importancia en la alimentación."),
        selectInput("ciclo", "Ciclo productivo", choices = etiqueta_ciclo(sort(unique(donde$ciclo))),
                    selected = "Primavera-Verano"),
        selectInput("var", "¿Qué ver en el mapa?", choices = vars_agri),
        accordion(open = TRUE,
          accordion_panel("Filtros",
            sliderInput("u_pob", "Pobreza: al menos este % de la población", min = 0, max = 100, value = 0, step = 1),
            sliderInput("u_fao", "Años con sequía severa en los cultivos, de cada 100 (satélite FAO): al menos",
                        min = 20, max = fao_max_agri, value = 20, step = 1))),
        p(class = "small text-muted",
          "El mapa pinta los municipios que siembran el cultivo en ese ciclo y pasan los filtros.",
          "Cada filtro actúa solo si lo mueves. Haz clic en un municipio para ver su perfil.")
      ),
      intro("¿Quién vive donde se seca el campo?",
        p("Escoge un cultivo y mueve los filtros: el mapa y los números te dicen cuántos municipios los pasan,",
          "cuánta gente vive ahí y cuántas personas trabajan en la agricultura."),
        p("La sequía se ve de dos formas. El satélite de la FAO cuenta en cuántos de cada 100 años la",
          "vegetación sufrió una sequía severa. El registro de México dice qué parte del tiempo ha pasado",
          "cada municipio en sequía desde 2016.")),
      layout_columns(
        value_box(title = "Municipios que pasan los filtros", value = textOutput("n_mun")),
        value_box(title = "Población 2025", value = textOutput("n_pob")),
        value_box(title = "Personas que trabajan en la agricultura", value = textOutput("n_agri"),
                  p("En estos municipios, en cualquier cultivo"))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE, card_header(textOutput("t_mapa", inline = TRUE)),
             leafletOutput("mapa", height = "520px")),
        card(card_header("Perfil del municipio"), plotOutput("perfil", height = "520px"))
      ),
      card(full_screen = TRUE, card_header("Rendimiento nacional del cultivo"),
           plotOutput("rend", height = "380px"),
           p(class = "small text-muted", style = "margin: 6px 12px 10px;",
             tags$b("¿Por qué aquí casi no se nota El Niño?"),
             "El rendimiento mide cuántas toneladas da cada hectárea que sí se cosechó. Cuando hay sequía,",
             "el daño se ve sobre todo en la superficie: se siembra menos y se pierde más, y las hectáreas",
             "perdidas no entran en este cálculo. Además, el promedio nacional mezcla regiones donde El Niño",
             "seca con otras donde casi no afecta, y el rendimiento sube cada año por mejoras técnicas, lo que",
             "tapa las variaciones."))
    )
  ),

  # --- Ganadería --------------------------------------------------------------
  nav_panel("Ganadería", value = "ganado",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("especie", "Ganado", choices = especies),
        selectInput("var_g", "¿Qué ver en el mapa?", choices = vars_gan),
        accordion(open = TRUE,
          accordion_panel("Filtros",
            sliderInput("u_pob_g", "Pobreza: al menos este % de la población", min = 0, max = 100, value = 0, step = 1),
            sliderInput("u_fao_g", "Años con sequía severa en los pastizales, de cada 100 (satélite FAO): al menos",
                        min = 20, max = fao_max_pec, value = 20, step = 1))),
        p(class = "small text-muted",
          "El mapa pinta los municipios que producen ese ganado y pasan los filtros. El satélite de la",
          "FAO casi no tiene información sobre los pastizales de México, así que su filtro deja muy pocos",
          "municipios; el registro de México sí cubre a todos. Haz clic en un municipio para ver su perfil.")
      ),
      intro("¿Y el ganado?",
        p("Vacas, aves y cerdos son casi toda la ganadería del país: 98 de cada 100 pesos que produce.",
          "Las vacas son las que más dependen del pasto. Las aves y los cerdos se crían casi siempre en",
          "granjas; a ellos la sequía les afecta por el precio del alimento, el agua y el calor.")),
      layout_columns(
        value_box(title = "Municipios que pasan los filtros", value = textOutput("n_mun_g")),
        value_box(title = "Población 2025", value = textOutput("n_pob_g")),
        value_box(title = "Personas que trabajan con ganado", value = textOutput("n_gan_g"),
                  p("En estos municipios, con cualquier ganado")),
        value_box(title = "Parte del valor nacional que producen estos municipios", value = textOutput("n_val_g"))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE, card_header(textOutput("t_mapa_g", inline = TRUE)),
             leafletOutput("mapa_g", height = "520px")),
        card(card_header("Perfil del municipio"), plotOutput("perfil_g", height = "520px"))
      ),
      card(full_screen = TRUE, card_header("De cada 100 pesos que producen vacas, aves y cerdos"),
           plotOutput("especies", height = "260px"))
    )
  ),

  # --- Hectáreas en riesgo ----------------------------------------------------
  nav_panel("Hectáreas en riesgo", value = "riesgo",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("cultivo2", "Cultivo",
                    choices = sort(intersect(cultivos16, unique(ha_anio$cultivo))), selected = "Maíz grano"),
        p(class = "small text-muted",
          "Son los 16 cultivos que más se produjeron en 2025, el último año cerrado del SIAP: los 15 con",
          "más toneladas, sin contar forrajes, más el frijol por su importancia en la alimentación."),
        selectInput("ciclo2", "Ciclo productivo", choices = etiqueta_ciclo(sort(unique(ha_anio$ciclo))),
                    selected = "Primavera-Verano"),
        radioButtons("medida", "Cómo contar la sequía",
                     choices = c("Cualquier sequía cuenta igual"   = "pres",
                                 "Las sequías más fuertes pesan más" = "int"),
                     selected = "pres"),
        radioButtons("vista2", "Qué ver en el mapa",
                     choices = c("Hectáreas en riesgo"                    = "hr",
                                 "Tiempo en sequía (registro de México)"  = "p"),
                     selected = "hr"),
        accordion(open = TRUE,
          accordion_panel("Filtros: pobreza y satélite",
            sliderInput("u_pob2", "Pobreza: al menos este % de la población", min = 0, max = 100, value = 0, step = 1),
            sliderInput("u_fao2", "Años con sequía severa en los cultivos, de cada 100 (satélite FAO): al menos",
                        min = 20, max = fao_max_agri, value = 20, step = 1),
            sliderInput("u_faop2", "Años con sequía severa en los pastizales, de cada 100 (satélite FAO): al menos",
                        min = 20, max = fao_max_pec, value = 20, step = 1),
            p(class = "small text-muted",
              "Escogen los municipios por su pobreza y por la sequía que ve el satélite; las hectáreas",
              "en riesgo se calculan con el registro de sequía de México."))),
        p(class = "small text-muted",
          sprintf("Hectáreas en riesgo: superficie sembrada en %s por la parte de la temporada que ", ANIO_HA),
          sprintf("cada municipio ha pasado en sequía, promedio %s-%s. Detalle en ¿Cómo se calculó?",
                  ANIO_INI_MSM, ANIO_HA))
      ),
      intro("¿Cuánto está en riesgo?",
        p("Con el registro de sequía de México calculamos qué parte de cada temporada ha pasado cada",
          "municipio en sequía desde 2016. Multiplicado por lo que se siembra, da las hectáreas que en un",
          "año típico estarían bajo sequía. Es nuestra aportación: una segunda forma de mirar la sequía,",
          "en paralelo con la del satélite.")),
      value_box(title = "Hectáreas en riesgo en un año típico (promedio 2016-2025)",
                value = textOutput("h_hist"),
                p(textOutput("h_hist_desc", inline = TRUE))),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE, card_header(textOutput("t_mapa2", inline = TRUE)),
             leafletOutput("mapa2", height = "520px")),
        card(full_screen = TRUE, card_header("Hectáreas en riesgo de sequía, por temporada"),
             plotOutput("barras", height = "520px"))
      ),
      card(full_screen = TRUE, card_header("¿Coinciden la pobreza y la sequía?"),
           plotOutput("disp", height = "480px", hover = hoverOpts("disp_hover", delay = 100)),
           textOutput("disp_info"))
    )
  ),

  # --- ¿Cómo se calculó? --------------------------------------------------------
  nav_panel("¿Cómo se calculó?", value = "metodologia",
    card(
      withMathJax(HTML(r"---(
<div style="max-width: 920px; font-size: 1.02rem; line-height: 1.6">

<h4>La estructura del análisis</h4>
<div style="display:flex; flex-wrap:wrap; gap:10px; align-items:stretch; margin-bottom:18px">
  <div style="flex:1; min-width:170px; background:#f4f6f7; border-radius:8px; padding:10px">
    <b>1. Fuentes</b><br>NOAA, CHIRPS, FAO, INEGI, CONEVAL, SIAP y CONAGUA</div>
  <div style="align-self:center; font-size:22px">&rarr;</div>
  <div style="flex:1; min-width:170px; background:#f4f6f7; border-radius:8px; padding:10px">
    <b>2. Cruce territorial</b><br>La FAO leída solo sobre la tierra agrícola y de pastizal del INEGI; todo llevado a municipio</div>
  <div style="align-self:center; font-size:22px">&rarr;</div>
  <div style="flex:1; min-width:170px; background:#f4f6f7; border-radius:8px; padding:10px">
    <b>3. Indicadores</b><br>Años con sequía severa (satélite) y tiempo en sequía (registro); personas, pobreza, producción y hectáreas en riesgo</div>
  <div style="align-self:center; font-size:22px">&rarr;</div>
  <div style="flex:1; min-width:170px; background:#f4f6f7; border-radius:8px; padding:10px">
    <b>4. Pestañas</b><br>El Niño, agricultura, ganadería y hectáreas en riesgo</div>
</div>

<h4>Fuentes</h4>
<table class="table table-sm">
<thead><tr><th>Tema</th><th>Fuente</th><th>Qué se usó</th></tr></thead>
<tbody>
<tr><td>El Niño</td><td>NOAA PSL, índice MEI.v2</td><td>Episodios: MEI de +0.5 o más durante al menos cinco bimestres seguidos, desde 1979</td></tr>
<tr><td>Lluvia en años Niño</td><td>CHIRPS, Climate Hazards Center</td><td>Anomalía estandarizada de la lluvia de verano (junio a agosto) e invierno (diciembre a febrero) en los años Niño, por municipio, procesada en Google Earth Engine</td></tr>
<tr><td>Sequía observada por satélite</td><td>FAO, Agricultural Stress Index System (ASIS)</td><td>Probabilidad de sequía en tierras de cultivo y en pastizales (capas <i>ASIS-DROUGHT-PROBABILITY CROPLAND</i> y <i>PASTURELAND</i>); en la app se presenta como años con sequía severa, de cada 100</td></tr>
<tr><td>Uso de suelo</td><td>INEGI, Uso de Suelo y Vegetación Serie VII, escala 1:250,000</td><td>Agricultura de temporal y de riego; pastizal cultivado e inducido</td></tr>
<tr><td>Población y empleo</td><td>INEGI, Encuesta Intercensal 2025</td><td>Población total del conjunto de datos; personas ocupadas en agricultura (SCIAN 111) y cría de animales (SCIAN 112), de los microdatos con su factor de expansión</td></tr>
<tr><td>Pobreza</td><td>CONEVAL, medición de pobreza municipal 2020</td><td>Porcentaje de la población en pobreza</td></tr>
<tr><td>Agricultura</td><td>SIAP, cierre agrícola municipal 2003-2025</td><td>Superficie sembrada y rendimiento por cultivo y ciclo; toneladas de los 16 cultivos principales en 2025</td></tr>
<tr><td>Ganadería</td><td>SIAP, cierre pecuario municipal 2025</td><td>Valor de la producción de bovinos, aves y porcinos por municipio</td></tr>
<tr><td>Sequía registrada en México</td><td>CONAGUA, Monitor de Sequía de México</td><td>Categorías quincenales por municipio, de 2016 a 2025</td></tr>
</tbody></table>

<h4>Cómo se calculó cada cosa</h4>
<ul>
<li><b>Años con sequía severa según el satélite (FAO).</b> Para cada municipio, la probabilidad de la FAO se promedió solo sobre la superficie que la Serie VII clasifica como agrícola o como pastizal, ponderando cada píxel por la superficie de esa clase que contiene.</li>
<li><b>Tiempo en sequía según el registro de México.</b> En cada fecha del Monitor, un municipio cuenta como en sequía si está en categoría D1 a D4. Para cada temporada se calcula qué parte de las fechas estuvo en sequía, y se promedian las temporadas de 2016 a 2025. Para la agricultura se usa la temporada de abril a septiembre; para el ganado, todo el año.</li>
<li><b>Dónde el campo se seca más.</b> La tercera parte de los municipios con más años de sequía severa (satélite) o más tiempo en sequía (registro), calculada por separado para cada fuente, entre los municipios con agricultura según el INEGI que tienen dato de esa fuente.</li>
<li><b>Cultivos principales.</b> Los 15 con más toneladas producidas en 2025, sin contar forrajes, más el frijol por su importancia en la alimentación.</li>
<li><b>Ganadería.</b> Bovinos, aves y porcinos suman el 97.6% del valor de la producción pecuaria de 2025. Para no contar dos veces, se excluye el ganado en pie, que ya está contenido en la carne.</li>
</ul>

<h4>Hectáreas en riesgo de sequía</h4>
<p>Adapta el método del SIAP: en lugar de la sequía de una sola quincena, usa qué tan seguido ha estado cada municipio en sequía durante la temporada del cultivo.</p>

<p><strong>Variables.</strong> \(m\): municipio; \(k\): cultivo; \(c\): ciclo; \(y\): año; \(t\): corte del Monitor.
\(T_{c,y}\): cortes de la temporada \(y\) del ciclo \(c\). \(Y\): temporadas de 2016 al último año cerrado
del SIAP, \(y^*\). \(H_{m,k,c}\): hectáreas sembradas del cultivo en el municipio en \(y^*\).
\(M_f\): municipios que siembran el cultivo y pasan los filtros elegidos.</p>

$$D_{m,t} = \begin{cases} 1 & \text{si el municipio está en D1, D2, D3 o D4} \\ 0 & \text{si está en D0 o sin sequía} \end{cases}$$

<p>Fracción de la temporada en sequía, cada año:</p>
$$f_{m,c,y} = \frac{1}{|T_{c,y}|} \sum_{t \in T_{c,y}} D_{m,t}$$

<p>Probabilidad histórica de sequía del municipio, que en la app llamamos <b>tiempo en sequía</b>:</p>
$$P_{m,c} = \frac{1}{|Y|} \sum_{y \in Y} f_{m,c,y}$$

<p>Hectáreas en riesgo y su porcentaje del total nacional del cultivo:</p>
$$HR_{k,c} = \sum_{m \in M_f} H_{m,k,c} \, P_{m,c} \qquad \%HR_{k,c} = 100 \, \frac{HR_{k,c}}{\sum_{m} H_{m,k,c}}$$

<p>Cada barra de la gráfica por temporada usa la superficie sembrada ese mismo año y la parte de esa temporada que cada municipio pasó en sequía:</p>
$$HR_{k,c,y} = \sum_{m} H_{m,k,c,y} \, f_{m,c,y}$$

<h4>Temporadas y periodo</h4>
<p><strong>Primavera-Verano</strong>: cortes del Monitor de abril a septiembre del año \(y\).
<strong>Otoño-Invierno</strong>: cortes de octubre a diciembre del año \(y-1\) y de enero a marzo del año \(y\),
porque el SIAP registra ese ciclo en el año agrícola en que termina. Los perennes y el ganado usan el año completo.
Solo se usan las temporadas desde 2016: hasta 2015 el Monitor asignaba la categoría al municipio si cubría al
menos el 40% de su superficie, y desde 2016 asigna la de mayor intensidad observada en él.</p>

<h4>Presencia o intensidad</h4>
<p>Por defecto, cualquier categoría de D1 a D4 cuenta igual. Con la opción de intensidad, cada categoría pesa según su gravedad:</p>
$$w_{m,t} = \begin{cases} 0 & \text{sin sequía o D0} \\ 1/4 & \text{D1} \\ 2/4 & \text{D2} \\ 3/4 & \text{D3} \\ 1 & \text{D4} \end{cases}$$
<p>Los pesos son una convención: no significa que D4 cause cuatro veces el daño de D1.</p>

<h4>Ejemplo paso a paso</h4>
<p>Dos municipios hipotéticos que siembran maíz de Primavera-Verano, con solo tres temporadas.</p>
<table class="table table-sm" style="max-width: 560px">
<thead><tr><th>Temporada</th><th>Municipio A</th><th>Municipio B</th></tr></thead>
<tbody>
<tr><td>2022</td><td>3 de 12 fechas en sequía = 0.25</td><td>0 de 12 = 0.00</td></tr>
<tr><td>2023</td><td>6 de 12 = 0.50</td><td>3 de 12 = 0.25</td></tr>
<tr><td>2024</td><td>0 de 12 = 0.00</td><td>0 de 12 = 0.00</td></tr>
</tbody></table>
<p>Tiempo en sequía: \(P_A = (0.25 + 0.50 + 0.00) / 3 = 0.25\) y \(P_B = (0.00 + 0.25 + 0.00) / 3 \approx 0.083\).
Si A siembra 1,000 hectáreas y B 600, las hectáreas en riesgo son \(1{,}000 \times 0.25 + 600 \times 0.083 = 300\),
el \(18.8\%\) de las 1,600 sembradas.</p>

<h4>Lo que hay que saber al leer los datos</h4>
<ul>
<li>Todo mide <b>exposición</b>, no pérdidas: vivir donde la sequía es más frecuente no significa haber perdido la cosecha.</li>
<li>La capa de la FAO solo tiene dato sobre el 6% de la superficie agrícola y el 0.5% de los pastizales que registra el INEGI. En el 42% de los municipios agrícolas su valor está en el mínimo de la capa, 20 de cada 100 años.</li>
<li>La FAO y el Monitor miden cosas distintas —la respuesta de la vegetación y la falta de lluvia— y ordenan a los municipios de forma diferente. Se probaron otros periodos y umbrales de severidad para el Monitor, y el resultado de fondo no cambia.</li>
<li>La Encuesta Intercensal es una muestra. INEGI marca siete municipios con información limitada. La población ocupada que se calculó desde los microdatos coincide con la que publica INEGI.</li>
<li>La pobreza es de 2020 y la población de 2025. La Serie VII tiene escala 1:250,000, así que no registra manchas pequeñas de uso de suelo.</li>
</ul>

<h4>Cómo citar las fuentes principales</h4>
<p class="small">© FAO – Agricultural Stress Index System (ASIS), http://www.fao.org/giews/earthobservation/ ·
CONAGUA, Monitor de Sequía de México · INEGI, Encuesta Intercensal 2025 y Serie VII de Uso de Suelo y Vegetación ·
CONEVAL, Medición de la pobreza municipal 2020 · SIAP, cierres de la producción agrícola y pecuaria ·
NOAA Physical Sciences Laboratory, Multivariate ENSO Index (MEI.v2) · Climate Hazards Center, CHIRPS ·
DGSIAP, <i>Indicadores de Producción Agrícola Bajo Sequía</i> · Banco de México, Reporte sobre las Economías Regionales, octubre-diciembre 2022.</p>
</div>
)---"))
    )
  )
)

# ---------------------------------------------------------------------------
# Servidor
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # ============================ El Niño ========================================

  output$t_mapa0 <- renderText(
    if (input$estacion == "verano") "Lluvia de verano (junio a agosto) en años Niño, comparada con un año normal"
    else "Lluvia de invierno (diciembre a febrero) en años Niño, comparada con un año normal")

  output$mapa0 <- renderLeaflet({
    z  <- if (input$estacion == "verano") mun$z_verano else mun$z_invierno
    x  <- mutate(mun, cat = cut(z, z_cortes, labels = z_etiq)) |> filter(!is.na(cat))
    et <- paste0(x$municipio, ", ", x$entidad_federativa, ": ", x$cat)
    if (input$estacion == "verano")
      et <- paste0(et, ifelse(is.na(x$acuerdo_verano), "",
                              sprintf(" · más seco en %d de cada 10 Niños", round(x$acuerdo_verano * 10))))
    pal <- colorFactor(z_col, levels = z_etiq)
    mapa_base() |>
      addPolygons(data = x, fillColor = pal(x$cat), fillOpacity = 0.8, stroke = TRUE, weight = 0,
                  color = "white", highlightOptions = resalte, label = et) |>
      addPolylines(data = estados, color = "white", weight = 1.2, opacity = 0.9, options = sin_clic) |>
      addLegend(pal = pal, values = factor(z_etiq, levels = z_etiq), position = "bottomright",
                title = "Comparada con un año normal, la lluvia fue…", opacity = 0.9)
  })

  output$mei <- renderPlot({
    ggplot(mei, aes(fecha, mei, fill = mei >= 0.5)) +
      geom_col(width = 1 / 12) +
      geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40") +
      scale_fill_manual(values = c(`FALSE` = "grey75", `TRUE` = "#c0392b"),
                        labels = c("Debajo del umbral", "Arriba del umbral de El Niño (+0.5)"), name = NULL) +
      scale_x_continuous(breaks = seq(1980, 2025, 5)) +
      labs(x = NULL, y = "Pacífico más caliente  ↑\nPacífico más frío  ↓",
           caption = "Línea punteada: a partir de ahí se considera El Niño. Fuente: NOAA, índice MEI.v2.") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
  })

  # ============================ Agricultura ====================================

  # El menú de ciclo solo ofrece los ciclos en que se siembra el cultivo elegido
  observeEvent(input$cultivo, {
    ciclos <- sort(unique(donde$ciclo[donde$cultivo == input$cultivo]))
    updateSelectInput(session, "ciclo", choices = etiqueta_ciclo(ciclos),
                      selected = if (isolate(input$ciclo) %in% ciclos) isolate(input$ciclo) else ciclos[1])
  })

  # Municipios que siembran el cultivo en ese ciclo y cumplen los cortes, con su producción 2025
  filtrados <- reactive({
    t <- filter(cultivos_mun, cultivo == input$cultivo) |> select(CVEGEO, prod = toneladas)
    mun |>
      filter(CVEGEO %in% donde$CVEGEO[donde$cultivo == input$cultivo & donde$ciclo == input$ciclo]) |>
      left_join(t, by = "CVEGEO") |>
      filter(cumple(pobreza,        input$u_pob, 0),
             cumple(fao_agri * 100, input$u_fao, 20))
  })

  output$n_mun  <- renderText(scales::comma(nrow(filtrados())))

  output$t_mapa <- renderText(sprintf("%s · %s, %s · %s municipios", titulo_var(input$var),
                                      input$cultivo, input$ciclo, scales::comma(nrow(filtrados()))))
  output$n_pob  <- renderText(redondo(sum(filtrados()$poblacion,   na.rm = TRUE)))
  output$n_agri <- renderText(redondo(sum(filtrados()$agricultura, na.rm = TRUE)))

  output$mapa <- renderLeaflet(mapa_base())

  # El mapa avisa cuando ya existe en el navegador (manda su zoom); hasta entonces no se pinta
  listo1 <- reactiveVal(FALSE)
  observeEvent(input$mapa_zoom, listo1(TRUE), once = TRUE)

  observe({
    req(listo1())
    m <- filtrados()
    if (input$var == "prod") m <- filter(m, prod > 0)
    proxy <- leafletProxy("mapa") |> clearShapes() |> removeControl("leyenda")
    if (nrow(m) == 0) {
      showNotification("Ningún municipio cumple esa combinación.", type = "warning")
      return()
    }
    proxy |>
      dibuja(m, clasifica(m[[input$var]], input$var), "leyenda") |>
      marcar("mapa", isolate(sel()))
  })

  # Municipio seleccionado con el clic, y su contorno
  sel <- reactiveVal(NULL)
  observeEvent(input$mapa_shape_click, {
    id <- input$mapa_shape_click$id
    if (!is.null(id)) sel(id)
  })

  marcar <- function(proxy, id_mapa, cv) {
    proxy <- clearGroup(proxy, "seleccion")
    if (is.null(cv)) return(proxy)
    addPolylines(proxy, data = st_boundary(filter(mun, CVEGEO == cv)), group = "seleccion",
                 color = "#00e5ff", weight = 3, opacity = 1, options = sin_clic)
  }
  observeEvent(sel(), leafletProxy("mapa") |> marcar("mapa", sel()))

  output$perfil <- renderPlot({
    validate(need(sel(), "Haz clic en un municipio del mapa."))
    grafica_perfil(sel())
  })

  # Rendimiento nacional del cultivo y ciclo elegidos, con los años Niño
  output$rend <- renderPlot({
    r <- filter(rend_nac, cultivo == input$cultivo, ciclo == input$ciclo)
    validate(need(nrow(r) > 0, "Sin registro de rendimiento para ese cultivo en ese ciclo."))
    nino <- anios_nino[between(anios_nino, min(r$Anio), max(r$Anio))]

    ggplot(r, aes(Anio, rendimiento)) +
      annotate("rect", xmin = nino - 0.5, xmax = nino + 0.5,
               ymin = -Inf, ymax = Inf, fill = "#f6c9a8", alpha = 0.6) +
      geom_line(colour = "#2c3e50", linewidth = 0.8) +
      geom_point(colour = "#2c3e50", size = 2.5) +
      scale_x_continuous(breaks = seq(2003, 2025, 2)) +
      labs(title = str_c(input$cultivo, " · ", input$ciclo), x = NULL, y = "Toneladas por hectárea cosechada",
           caption = "Franjas: años Niño (MEI.v2, NOAA PSL). Fuente: SIAP.") +
      theme_minimal(base_size = 13) +
      theme(plot.title         = element_text(face = "bold"),
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank())
  })

  # ============================ Ganadería ======================================

  valor_especie <- reactive({
    switch(input$especie,
           todas   = mun$valor_bovino + mun$valor_ave + mun$valor_porcino,
           bovino  = mun$valor_bovino,
           ave     = mun$valor_ave,
           porcino = mun$valor_porcino)
  })

  # Municipios que producen ese ganado y cumplen los cortes
  filtrados_g <- reactive({
    mutate(mun, valor = valor_especie()) |>
      filter(valor > 0) |>
      filter(cumple(pobreza,       input$u_pob_g, 0),
             cumple(fao_pec * 100, input$u_fao_g, 20))
  })

  output$n_mun_g <- renderText(scales::comma(nrow(filtrados_g())))

  output$t_mapa_g <- renderText(sprintf("%s · %s · %s municipios", titulo_var(input$var_g),
                                        names(especies)[especies == input$especie],
                                        scales::comma(nrow(filtrados_g()))))
  output$n_pob_g <- renderText(redondo(sum(filtrados_g()$poblacion, na.rm = TRUE)))
  output$n_gan_g <- renderText(redondo(sum(filtrados_g()$ganaderia, na.rm = TRUE)))
  output$n_val_g <- renderText(pct(sum(filtrados_g()$valor) / sum(valor_especie(), na.rm = TRUE) * 100))

  output$mapa_g <- renderLeaflet(mapa_base())

  listo_g <- reactiveVal(FALSE)
  observeEvent(input$mapa_g_zoom, listo_g(TRUE), once = TRUE)

  observe({
    req(listo_g())
    m <- filtrados_g()
    proxy <- leafletProxy("mapa_g") |> clearShapes() |> removeControl("leyenda_g")
    if (nrow(m) == 0) {
      showNotification("Ningún municipio cumple esa combinación.", type = "warning")
      return()
    }
    proxy |>
      dibuja(m, clasifica(m[[input$var_g]], input$var_g), "leyenda_g") |>
      marcar("mapa_g", isolate(sel_g()))
  })

  sel_g <- reactiveVal(NULL)
  observeEvent(input$mapa_g_shape_click, {
    id <- input$mapa_g_shape_click$id
    if (!is.null(id)) sel_g(id)
  })
  observeEvent(sel_g(), leafletProxy("mapa_g") |> marcar("mapa_g", sel_g()))

  output$perfil_g <- renderPlot({
    validate(need(sel_g(), "Haz clic en un municipio del mapa."))
    grafica_perfil(sel_g())
  })

  # Vacas, aves y cerdos, con el elegido resaltado
  output$especies <- renderPlot({
    ganado |>
      mutate(nombre  = case_when(especie == "Bovino"  ~ "Vacas (carne y leche)",
                                 especie == "Ave"     ~ "Aves (pollo y huevo)",
                                 especie == "Porcino" ~ "Cerdos (carne)",
                                 TRUE ~ especie),
             elegido = input$especie == "todas" | tolower(especie) == input$especie) |>
      ggplot(aes(pct, reorder(nombre, pct), fill = elegido)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = sprintf("%.0f pesos", pct)), hjust = -0.1, size = 5, colour = "grey25") +
      scale_fill_manual(values = c(`TRUE` = "#756bb1", `FALSE` = "#d4d1e8"), guide = "none") +
      scale_x_continuous(limits = c(0, 65), labels = NULL) +
      labs(x = NULL, y = NULL, caption = "Valor de la producción de 2025. Fuente: SIAP.") +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank())
  })

  # ============================ Hectáreas en riesgo ============================

  observeEvent(input$cultivo2, {
    ciclos <- sort(unique(ha_anio$ciclo[ha_anio$cultivo == input$cultivo2]))
    updateSelectInput(session, "ciclo2", choices = etiqueta_ciclo(ciclos),
                      selected = if (isolate(input$ciclo2) %in% ciclos) isolate(input$ciclo2) else ciclos[1])
  })

  # Superficie del último año cerrado, probabilidad histórica y hectáreas en riesgo por municipio
  riesgo_mun <- reactive({
    ha_riesgo |>
      filter(cultivo == input$cultivo2, ciclo == input$ciclo2, ha > 0) |>
      left_join(d |> select(CVEGEO, municipio, entidad_federativa, pobreza, fao_agri, fao_pec),
                by = "CVEGEO") |>
      mutate(p_hist = if (input$medida == "int") p_int else p_hist,
             hr     = ha * p_hist,
             prob   = p_hist * 100)
  })

  # Los que cumplen los cortes (un corte solo filtra si lo moviste de su mínimo)
  sel2 <- reactive({
    riesgo_mun() |>
      filter(cumple(pobreza,        input$u_pob2,  0),
             cumple(fao_agri * 100, input$u_fao2,  20),
             cumple(fao_pec * 100,  input$u_faop2, 20))
  })

  # Hectáreas en riesgo de los que cumplen, contra toda la superficie del cultivo en el país
  tipico <- reactive({
    list(riesgo = sum(sel2()$hr, na.rm = TRUE),
         total  = sum(riesgo_mun()$ha),
         n      = nrow(sel2()),
         n_tot  = nrow(riesgo_mun()))
  })

  output$h_hist <- renderText({
    t <- tipico()
    validate(need(t$total > 0, sprintf("sin siembra en %s", ANIO_HA)))
    p <- t$riesgo / t$total * 100
    sprintf("%s ha (%s)", scales::comma(round(t$riesgo)),
            if (p > 0 & p < 0.1) "<0.1%" else sprintf("%.1f%%", p))
  })

  output$h_hist_desc <- renderText({
    t <- tipico()
    sprintf(str_c("Suma de los %s municipios que pasan los filtros, de los %s que siembran el cultivo. ",
                  "El porcentaje es respecto a toda la superficie sembrada del cultivo en el país. ",
                  "Es la línea punteada de la gráfica."),
            scales::comma(t$n), scales::comma(t$n_tot))
  })

  output$t_mapa2 <- renderText(sprintf("%s · %s, %s",
    if (input$vista2 == "hr") "Hectáreas en riesgo"
    else if (input$medida == "int") "Intensidad de la sequía en la temporada (registro de México)"
    else "Tiempo en sequía en la temporada (registro de México)",
    input$cultivo2, input$ciclo2))

  output$mapa2 <- renderLeaflet(mapa_base())

  listo2 <- reactiveVal(FALSE)
  observeEvent(input$mapa2_zoom, listo2(TRUE), once = TRUE)

  observe({
    req(listo2())

    m <- inner_join(mun, select(sel2(), CVEGEO, ha, p_hist, hr), by = "CVEGEO") |>
      filter(!is.na(p_hist))
    proxy <- leafletProxy("mapa2") |> clearShapes() |> removeControl("leyenda2")
    if (nrow(m) == 0) {
      showNotification("Ningún municipio cumple esos cortes.", type = "warning")
      return()
    }

    if (input$vista2 == "p") {
      x      <- m$p_hist * 100
      pal    <- colorBin("Reds", domain = c(0, max(x, na.rm = TRUE)), bins = 6, pretty = TRUE,
                         na.color = "#d9d9d9")
      titulo <- if (input$medida == "int") "Intensidad de la sequía (0-100)"
                else "Tiempo en sequía en la temporada (%)"
    } else {
      x      <- m$hr
      cortes <- unique(quantile(x, probs = seq(0, 1, length.out = 7), na.rm = TRUE))
      if (length(cortes) < 3) cortes <- pretty(range(x, na.rm = TRUE), 3)
      pal    <- colorBin("Reds", domain = x, bins = cortes, na.color = "#d9d9d9")
      titulo <- "Hectáreas en riesgo"
    }

    m$txt_p <- if (input$medida == "int") sprintf("intensidad de la sequía %.0f de 100", m$p_hist * 100)
               else sprintf("%.0f%% del tiempo en sequía", m$p_hist * 100)

    proxy |>
      addPolygons(data = m, fillColor = pal(x), fillOpacity = 0.75,
                  stroke = TRUE, weight = 0, color = "white", highlightOptions = resalte,
                  label = ~sprintf("%s, %s: %s · %s ha sembradas · %s ha en riesgo",
                                   municipio, entidad_federativa, txt_p, scales::comma(round(ha)),
                                   scales::comma(round(hr)))) |>
      addPolylines(data = estados, color = "white", weight = 1.2, opacity = 0.9, options = sin_clic) |>
      addLegend(layerId = "leyenda2", pal = pal, values = x, position = "bottomright", title = titulo,
                labFormat = labelFormat(big.mark = ",", digits = 0))
  })

  # Pobreza y probabilidad de sequía de cada municipio que siembra el cultivo
  disp_datos <- reactive({
    riesgo_mun() |>
      filter(!is.na(p_hist), !is.na(pobreza)) |>
      mutate(cumple = if_else(CVEGEO %in% sel2()$CVEGEO, "Pasa los filtros", "No los pasa"))
  })

  output$disp <- renderPlot({
    dd <- disp_datos()
    validate(need(nrow(dd) > 2, "Sin datos suficientes para ese cultivo y ciclo."))

    r     <- cor(dd$prob, dd$pobreza, method = "spearman")
    m_x   <- median(dd$prob)
    m_y   <- median(dd$pobreza)
    ambos <- filter(dd, prob > m_x, pobreza > m_y)

    ggplot(dd, aes(prob, pobreza, size = hr, colour = cumple)) +
      geom_vline(xintercept = m_x, linetype = "dashed", colour = "grey60") +
      geom_hline(yintercept = m_y, linetype = "dashed", colour = "grey60") +
      { if (input$u_pob2 > 0) geom_hline(yintercept = input$u_pob2, colour = "#2c3e50") } +
      geom_point(alpha = 0.4) +
      scale_colour_manual(values = c("Pasa los filtros" = "#1f4e79", "No los pasa" = "grey70"), name = NULL) +
      scale_size_area(max_size = 9, labels = scales::label_comma(), name = "Hectáreas en riesgo") +
      labs(title    = str_c(input$cultivo2, " · ", input$ciclo2),
           subtitle = sprintf(str_c("Qué tanto van juntas la pobreza y la sequía: %.2f (de -1 a 1; cerca de 0, no van juntas) · ",
                                    "%s municipios con más pobreza y más sequía que la mayoría, ",
                                    "con %s ha en riesgo (%.1f%% del total en riesgo)"),
                              r, scales::comma(nrow(ambos)), scales::comma(round(sum(ambos$hr))),
                              sum(ambos$hr) / sum(dd$hr) * 100),
           x = if (input$medida == "int") "Intensidad de la sequía en la temporada (0-100, registro de México)"
               else "Tiempo en sequía en la temporada (%, registro de México)",
           y = "Pobreza (%)",
           caption = str_c("Cada punto es un municipio que siembra el cultivo. Líneas punteadas: el municipio de en medio ",
                           "en cada eje. En azul, los que pasan los filtros. Línea continua: la pobreza mínima elegida.")) +
      theme_minimal(base_size = 13) +
      theme(plot.title      = element_text(face = "bold"),
            plot.subtitle   = element_text(colour = "grey25"),
            legend.position = "right",
            panel.grid.minor = element_blank())
  })

  output$disp_info <- renderText({
    if (is.null(input$disp_hover)) return("Pasa el cursor sobre un punto para ver el municipio.")
    p <- nearPoints(disp_datos(), input$disp_hover, xvar = "prob", yvar = "pobreza",
                    maxpoints = 1, threshold = 10)
    if (nrow(p) == 0) return("Pasa el cursor sobre un punto para ver el municipio.")
    txt_p <- if (input$medida == "int") sprintf("intensidad de la sequía %.0f de 100", p$prob)
             else sprintf("%.0f%% del tiempo en sequía", p$prob)
    sprintf("%s, %s · pobreza %.1f%% · %s · %s ha sembradas · %s ha en riesgo",
            p$municipio, p$entidad_federativa, p$pobreza, txt_p,
            scales::comma(round(p$ha)), scales::comma(round(p$hr)))
  })

  # Hectáreas en riesgo cada temporada: superficie sembrada ese año × parte de la temporada en sequía
  output$barras <- renderPlot({
    b <- ha_anio |>
      filter(cultivo == input$cultivo2, ciclo == input$ciclo2, CVEGEO %in% sel2()$CVEGEO) |>
      inner_join(f_temp, by = c("CVEGEO", "Anio", "ciclo")) |>
      group_by(Anio) |>
      mutate(f = if (input$medida == "int") f_int else f) |>
      summarise(riesgo = sum(ha * f), total = sum(ha), .groups = "drop") |>
      mutate(nino = Anio %in% anios_nino)

    validate(need(nrow(b) > 0, "Sin superficie sembrada para ese cultivo en ese ciclo."))

    t    <- tipico()
    nino <- b$Anio[b$nino]

    ggplot(b, aes(Anio, riesgo / 1000)) +
      annotate("rect", xmin = nino - 0.5, xmax = nino + 0.5,
               ymin = -Inf, ymax = Inf, fill = "#f6c9a8", alpha = 0.6) +
      geom_col(width = 0.75, fill = "#de2d26") +
      geom_hline(yintercept = t$riesgo / 1000, linetype = "dashed", colour = "#2c3e50") +
      annotate("text", x = min(b$Anio) - 0.4, y = t$riesgo / 1000,
               label = sprintf("Año típico con la superficie de %s: %s ha",
                               ANIO_HA, scales::comma(round(t$riesgo))),
               hjust = 0, vjust = -0.6, size = 3.8, colour = "#2c3e50") +
      scale_x_continuous(breaks = unique(b$Anio)) +
      labs(title = str_c(input$cultivo2, " · ", input$ciclo2),
           x = NULL, y = "Miles de hectáreas en riesgo", fill = NULL,
           caption = str_c("Cada barra: cómo fue esa temporada, con las hectáreas sembradas ese año. ",
                           "Línea horizontal: un año típico. Franjas: años Niño. Temporadas desde 2016.")) +
      theme_minimal(base_size = 13) +
      theme(legend.position    = "top",
            plot.title         = element_text(face = "bold"),
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x        = element_text(angle = 90, vjust = 0.5))
  })
}

shinyApp(ui, server)
