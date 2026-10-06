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

# Los municipios con agricultura según el INEGI, con lo que usan los tres pasos
agri <- filter(mun, universo_agricola %in% TRUE)
agri$pct_agri  <- agri$pct_temp + agri$pct_riego
agri$ha_agri   <- agri$pct_agri / 100 * as.numeric(st_area(agri)) / 1e4
agri$sens_fao  <- agri$pct_agri * agri$fao_agri
n_agri_fao     <- sum(!is.na(agri$fao_agri))

# Sensibilidad agrícola (satélite FAO) para todos los municipios: la usa el filtro de la tercera pestaña
mun$sens_fao <- (mun$pct_temp + mun$pct_riego) * mun$fao_agri
sens_max     <- ceiling(max(mun$sens_fao, na.rm = TRUE))

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

# Rango de los filtros de la FAO, en probabilidad (%)
fao_max_agri <- ceiling(max(d$fao_agri, na.rm = TRUE) * 100)
fao_max_pec  <- ceiling(max(d$fao_pec,  na.rm = TRUE) * 100)

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

fao_cortes <- c(-Inf, 0.2001, 0.225, 0.25, 0.30, 0.35, Inf)
fao_etiq   <- c("20% o menos", "20 a 22.5%", "22.5 a 25%", "25 a 30%", "30 a 35%", "Más de 35%")
fao_col    <- c("#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#e34a33", "#b30000")

mon_cortes <- c(-Inf, 0.1, 0.2, 0.3, 0.4, 0.5, Inf)
mon_etiq   <- c("Menos de 10%", "10 a 20%", "20 a 30%", "30 a 40%", "40 a 50%", "Más de 50%")
mon_col    <- c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")

pob_cortes <- c(-Inf, 20, 40, 60, 80, Inf)
pob_etiq   <- c("Menos de 20%", "20 a 40%", "40 a 60%", "60 a 80%", "Más de 80%")
pob_col    <- c("#feebe2", "#fbb4b9", "#f768a1", "#c51b8a", "#7a0177")

uso_cortes <- c(-Inf, 5, 10, 20, 40, Inf)
uso_etiq   <- c("Menos de 5%", "5 a 10%", "10 a 20%", "20 a 40%", "Más de 40%")

ha_cortes  <- c(-Inf, 1000, 5000, 20000, 50000, Inf)
ha_etiq    <- c("Menos de 1,000 ha", "1,000 a 5,000 ha", "5,000 a 20,000 ha",
                "20,000 a 50,000 ha", "Más de 50,000 ha")

sens_cortes <- c(-Inf, 1, 2.5, 5, 10, 20, Inf)
sens_etiq   <- c("Menos de 1%", "1 a 2.5%", "2.5 a 5%", "5 a 10%", "10 a 20%", "Más de 20%")
sens_col    <- c("#ffffcc", "#fed976", "#feb24c", "#fd8d3c", "#e31a1c", "#800026")

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
  pct_agri     = list(cortes = uso_cortes, etiq = uso_etiq, col = verdes_col,
                      titulo = "Tierra de cultivo (% del territorio)",
                      frase = "del territorio es tierra de cultivo"),
  ha_agri      = list(cortes = ha_cortes, etiq = ha_etiq, col = verdes_col,
                      titulo = "Tierra de cultivo (hectáreas)",
                      frase = "de tierra de cultivo"),
  sens_fao     = list(cortes = sens_cortes, etiq = sens_etiq, col = sens_col,
                      titulo = "Sensibilidad agrícola (% del territorio, satélite FAO)",
                      frase = "de sensibilidad agrícola"),
  pct_pasto    = list(cortes = uso_cortes, etiq = uso_etiq, col = cafes_col,
                      titulo = "Pastizal (% del territorio)", frase = "del territorio es pastizal"),
  fao_agri     = list(cortes = fao_cortes, etiq = fao_etiq, col = fao_col,
                      titulo = "Probabilidad de sequía severa en los cultivos (%, satélite FAO)",
                      frase = "de probabilidad de sequía severa en sus cultivos (satélite FAO)"),
  fao_pec      = list(cortes = fao_cortes, etiq = fao_etiq, col = fao_col,
                      titulo = "Probabilidad de sequía severa en los pastizales (%, satélite FAO)",
                      frase = "de probabilidad de sequía severa en sus pastizales (satélite FAO)"),
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

# Toneladas: cinco clases según cómo se reparte la producción del cultivo en el país,
# con cifras redondeadas en la leyenda
cortes_ton <- function(v) {
  q <- unique(signif(quantile(v, c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE), 2))
  c(0, q, Inf)
}

etiq_ton <- function(cortes) {
  f <- \(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  k <- length(cortes) - 1
  c(sprintf("Menos de %s t", f(cortes[2])),
    if (k > 2) sprintf("%s a %s t", f(cortes[2:(k - 1)]), f(cortes[3:k])),
    sprintf("%s t o más", f(cortes[k])))
}

cl_ton <- function(v, cortes, cultivo) {
  et <- etiq_ton(cortes)
  list(cat    = cut(v, cortes, labels = et, right = FALSE),
       col    = morados_col[seq_along(et)],
       titulo = sprintf("%s en 2025 (toneladas)", cultivo),
       frase  = sprintf("%s toneladas de %s en 2025", format(round(v), big.mark = ","), tolower(cultivo)))
}

# De dónde salen los 16 cultivos de los selectores
nota_16 <- function() {
  p(class = "small text-muted",
    "Son los 16 cultivos que más se produjeron en 2025, el último año cerrado del SIAP: los 15 con",
    "más toneladas, sin contar forrajes, más el frijol por su importancia en la alimentación.")
}

# El nombre de cada capa, para leyendas y encabezados
titulo_var <- function(var) {
  if (var == "prod")  return("Producción del cultivo en 2025 (toneladas)")
  if (var == "valor") return("Valor de lo que produce en 2025")
  capas[[var]]$titulo
}

# Los ciclos con un nombre que se entienda fuera del sector
etiqueta_ciclo <- function(ci) setNames(ci, ifelse(ci == "Perennes", "Perennes (cultivos de todo el año)", ci))

# Variables del mapa de Ganadería, agrupadas por la pregunta que responden
vars_gan <- list(
  "¿Quién vive ahí?"     = c("Población en pobreza" = "pobreza",
                             "Personas que trabajan con ganado" = "ganaderia"),
  "¿Qué se produce?"     = c("Pastizal" = "pct_pasto",
                             "Valor de lo que produce en 2025" = "valor"),
  "¿Qué amenaza?"        = c("Probabilidad de sequía severa en pastizales (satélite FAO)" = "fao_pec",
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
    setView(lng = -102, lat = 23.5, zoom = 5) |>
    # Al mover el mapa o salirse de él se cierran las etiquetas que hayan quedado abiertas
    htmlwidgets::onRender("function(el, x) {
      var m = this, cerrar = function() {
        m.eachLayer(function(l) { if (l.closeTooltip) l.closeTooltip(); });
      };
      m.on('movestart zoomstart', cerrar);
      el.addEventListener('mouseleave', cerrar);
    }")
}

# Etiquetas con el dato exacto de cada municipio
num <- \(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")

et_suelo <- \(x) sprintf("%s, %s: %s%% del territorio es tierra de cultivo (%s ha)",
                         x$municipio, x$entidad_federativa, num(x$pct_agri), num(x$ha_agri, 0))
et_fao   <- \(x) sprintf("%s, %s: %s%% de probabilidad de sequía severa (satélite FAO)",
                         x$municipio, x$entidad_federativa, num(x$fao_agri * 100))
et_sens  <- \(x) sprintf("%s, %s: sensibilidad %s%% = %s%% de tierra de cultivo × %s%% de probabilidad de sequía severa",
                         x$municipio, x$entidad_federativa, num(x$sens_fao), num(x$pct_agri),
                         num(x$fao_agri * 100))
et_ton   <- \(x, cultivo) sprintf("%s, %s: %s toneladas de %s en 2025 · %s personas trabajan en la agricultura",
                                  x$municipio, x$entidad_federativa,
                                  ifelse(x$toneladas < 10, num(x$toneladas, 1), num(x$toneladas, 0)),
                                  tolower(cultivo),
                                  num(x$agricultura, 0))

# El satélite de la FAO: probabilidad de sequía severa, en seis clases
mapa_fao <- function() {
  x <- agri[!is.na(agri$fao_agri), ]
  dibuja(mapa_base(), x, clasifica(x$fao_agri, "fao_agri"), "leyenda", et_fao(x))
}

# Pinta los municipios con su categoría, los límites estatales y la leyenda
dibuja <- function(proxy, m, cl, leyenda, etiquetas = NULL) {
  ok  <- !is.na(cl$cat)
  et  <- if (is.null(etiquetas)) paste0(m$municipio, ", ", m$entidad_federativa, ": ", cl$frase) else etiquetas
  m   <- m[ok, ]
  pal <- colorFactor(cl$col, levels = levels(cl$cat))
  proxy |>
    addPolygons(data = m, layerId = m$CVEGEO, fillColor = pal(cl$cat[ok]), fillOpacity = 0.75,
                stroke = TRUE, weight = 0, color = "white", highlightOptions = resalte,
                label = et[ok]) |>
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
               "Probabilidad de sequía severa en cultivos (%)", "Probabilidad de sequía severa en pastizales (%)",
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
         caption = "Satélite FAO: probabilidad de sequía severa. Registro de México: % del tiempo en sequía, 2016-2025.") +
    theme_minimal(base_size = 14) +
    guides(fill = guide_legend(nrow = 2)) +
    theme(legend.position    = "top",
          plot.title         = element_text(face = "bold"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank())
}

# Cada paso de Agricultura: una tarjeta con su mapa, uno debajo del otro
paso <- function(num, id, titulo, ..., control = NULL, pie = NULL) {
  card(full_screen = TRUE,
       card_header(sprintf("Paso %d · %s", num, titulo)),
       p(class = "small text-muted", style = "margin: 6px 12px 0;", ...),
       if (!is.null(control)) div(style = "margin: 0 12px;", control),
       leafletOutput(id, height = "520px"),
       if (!is.null(pie)) div(style = "margin: 10px 12px 12px;", pie))
}

intro <- function(titulo, ...) {
  card(card_body(h4(titulo, style = "font-weight: 700;"),
                 div(style = "font-size: 1.05rem; line-height: 1.6;", ...)))
}

# ---------------------------------------------------------------------------
# Pestañas en pausa: para volver a mostrarlas, agrégalas dentro de page_navbar()
# ---------------------------------------------------------------------------

pestana_ganaderia <- nav_panel("Ganadería", value = "ganado",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("especie", "Ganado", choices = especies),
        selectInput("var_g", "¿Qué ver en el mapa?", choices = vars_gan),
        accordion(open = TRUE,
          accordion_panel("Filtros",
            sliderInput("u_pob_g", "Pobreza: al menos este % de la población", min = 0, max = 100, value = 0, step = 1),
            sliderInput("u_fao_g", "Probabilidad de sequía severa en los pastizales (%, satélite FAO): al menos",
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
  )

pestana_riesgo <- nav_panel("Hectáreas en riesgo", value = "riesgo",
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
            sliderInput("u_fao2", "Probabilidad de sequía severa en los cultivos (%, satélite FAO): al menos",
                        min = 20, max = fao_max_agri, value = 20, step = 1),
            sliderInput("u_faop2", "Probabilidad de sequía severa en los pastizales (%, satélite FAO): al menos",
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
  )

pestana_metodologia <- nav_panel("¿Cómo se calculó?", value = "metodologia",
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
    <b>3. Indicadores</b><br>Probabilidad de sequía severa (satélite) y tiempo en sequía (registro); sensibilidad agrícola; personas, pobreza, producción y hectáreas en riesgo</div>
  <div style="align-self:center; font-size:22px">&rarr;</div>
  <div style="flex:1; min-width:170px; background:#f4f6f7; border-radius:8px; padding:10px">
    <b>4. Pestañas</b><br>Agricultura, ganadería y hectáreas en riesgo</div>
</div>

<h4>Fuentes</h4>
<table class="table table-sm">
<thead><tr><th>Tema</th><th>Fuente</th><th>Qué se usó</th></tr></thead>
<tbody>
<tr><td>El Niño</td><td>NOAA PSL, índice MEI.v2</td><td>Episodios: MEI de +0.5 o más durante al menos cinco bimestres seguidos, desde 1979</td></tr>
<tr><td>Lluvia en años Niño</td><td>CHIRPS, Climate Hazards Center</td><td>Anomalía estandarizada de la lluvia de verano (junio a agosto) e invierno (diciembre a febrero) en los años Niño, por municipio, procesada en Google Earth Engine</td></tr>
<tr><td>Sequía observada por satélite</td><td>FAO, Agricultural Stress Index System (ASIS)</td><td>Probabilidad de sequía en tierras de cultivo y en pastizales (capas <i>ASIS-DROUGHT-PROBABILITY CROPLAND</i> y <i>PASTURELAND</i>), en porcentaje</td></tr>
<tr><td>Uso de suelo</td><td>INEGI, Uso de Suelo y Vegetación Serie VII, escala 1:250,000</td><td>Agricultura de temporal y de riego; pastizal cultivado e inducido</td></tr>
<tr><td>Población y empleo</td><td>INEGI, Encuesta Intercensal 2025</td><td>Población total del conjunto de datos; personas ocupadas en agricultura (SCIAN 111) y cría de animales (SCIAN 112), de los microdatos con su factor de expansión</td></tr>
<tr><td>Pobreza</td><td>CONEVAL, medición de pobreza municipal 2020</td><td>Porcentaje de la población en pobreza</td></tr>
<tr><td>Agricultura</td><td>SIAP, cierre agrícola municipal 2003-2025</td><td>Superficie sembrada y rendimiento por cultivo y ciclo; toneladas de los 16 cultivos principales en 2025</td></tr>
<tr><td>Ganadería</td><td>SIAP, cierre pecuario municipal 2025</td><td>Valor de la producción de bovinos, aves y porcinos por municipio</td></tr>
<tr><td>Sequía registrada en México</td><td>CONAGUA, Monitor de Sequía de México</td><td>Categorías quincenales por municipio, de 2016 a 2025</td></tr>
</tbody></table>

<h4>Cómo se calculó cada cosa</h4>
<ul>
<li><b>Probabilidad de sequía severa según el satélite (FAO), en los cultivos.</b> Promedio de los píxeles de la capa de cultivo de la FAO que caen en el municipio. La capa ya viene recortada a tierras de cultivo, así que no se vuelve a recortar con el INEGI: hacerlo dejaba sin dato a los municipios donde la Serie VII, por su escala, no dibuja la parcela.</li>
<li><b>Probabilidad de sequía severa según el satélite (FAO), en los pastizales.</b> Promedio sobre la superficie que la Serie VII clasifica como pastizal, ponderando cada píxel por la superficie de esa clase que contiene.</li>
<li><b>Tiempo en sequía según el registro de México.</b> En cada fecha del Monitor, un municipio cuenta como en sequía si está en categoría D1 a D4. Para cada temporada se calcula qué parte de las fechas estuvo en sequía, y se promedian las temporadas de 2016 a 2025. Para la agricultura se usa la temporada de abril a septiembre; para el ganado, todo el año.</li>
<li><b>Dónde el campo se seca más.</b> La tercera parte de los municipios con más probabilidad de sequía severa (satélite) o más tiempo en sequía (registro), calculada por separado para cada fuente, entre los municipios con agricultura según el INEGI que tienen dato de esa fuente.</li>
<li><b>Sensibilidad agrícola.</b> Por municipio, el porcentaje del territorio que la Serie VII clasifica como agricultura de temporal o de riego, multiplicado por la probabilidad de sequía severa de la FAO. Se lee como la parte del territorio que se espera como tierra de cultivo con sequía severa en un año cualquiera.</li>
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
<li>La capa de cultivo de la FAO tiene dato en una parte de los municipios con agricultura; el registro de México los cubre a todos. Donde la FAO no tiene dato no es que haya poca sequía: es que su malla, de alrededor de un kilómetro, no alcanza a ver la parcela.</li>
<li>Los valores de la FAO están apretados contra el mínimo de la capa, 20%: por eso el mapa del paso 2 separa con más detalle la parte baja de la escala, y por eso la sensibilidad calculada con el satélite se parece mucho al mapa de tierra de cultivo.</li>
<li>Para los pastizales la FAO solo tiene dato sobre el 0.5% de la superficie que registra el INEGI, así que el capítulo de ganadería se apoya en el registro de México.</li>
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

  # --- Agricultura ------------------------------------------------------------
  nav_panel("Agricultura", value = "municipios",
    layout_sidebar(
      sidebar = sidebar(
        p(class = "small text-muted",
          "Los mapas pintan los municipios con agricultura según el INEGI, sin separar por cultivo",
          "y sin filtros.")
      ),
      intro("De la tierra de cultivo a la sensibilidad agrícola",
        p("Tres pasos, uno debajo del otro: cuánta tierra de cultivo tiene cada municipio, qué",
          "probabilidad de sequía severa ve el satélite en esa tierra, y la sensibilidad agrícola, que",
          "combina las dos cosas."),
        p("Es la vista general de todo el campo del país. Baja con el scroll.")),
      layout_columns(
        value_box(title = "Municipios con agricultura (INEGI)", value = scales::comma(nrow(agri))),
        value_box(title = "Población 2025", value = redondo(sum(agri$poblacion, na.rm = TRUE))),
        value_box(title = "Personas que trabajan en la agricultura",
                  value = redondo(sum(agri$agricultura, na.rm = TRUE)),
                  p("En estos municipios, en cualquier cultivo"))
      ),
      paso(1, "a_suelo", "¿Cuánta tierra de cultivo hay?",
           "Serie VII del INEGI: agricultura de temporal más agricultura de riego.",
           control = radioButtons("unidad", NULL, inline = TRUE,
                                  c("% del territorio" = "pct", "Hectáreas" = "ha"))),
      paso(2, "a_fao", "¿Qué tan seguido se seca?",
           "El satélite de la FAO (ASIS), en las tierras de cultivo de cada municipio: la probabilidad",
           "de que haya sequía severa en un año cualquiera.",
           sprintf("Tienen dato %s de los %s municipios; los demás quedan sin color aquí y en el paso 3.",
                   scales::comma(n_agri_fao), scales::comma(nrow(agri)))),
      paso(3, "a_sens", "Sensibilidad agrícola",
           "La tierra de cultivo del paso 1 multiplicada por la probabilidad de sequía severa del paso 2:",
           "la parte del territorio que se espera como tierra de cultivo con sequía severa en un año",
           "cualquiera. Un municipio con 30% de tierra de cultivo y 40% de probabilidad da 12%.",
           pie = withMathJax(tagList(
             p(tags$b("Fórmula, para cada municipio \\(m\\):")),
             p("$$S_m = \\frac{A_m}{T_m} \\times P_m \\times 100$$"),
             p(class = "small",
               "\\(S_m\\): sensibilidad agrícola, en porcentaje del territorio.",
               "\\(A_m\\): hectáreas de agricultura de temporal y de riego (INEGI, Serie VII).",
               "\\(T_m\\): hectáreas totales del municipio.",
               "\\(P_m\\): probabilidad de sequía severa del satélite de la FAO, de 0 a 1.",
               "Como \\(A_m / T_m\\) y \\(P_m\\) son fracciones menores que 1, la sensibilidad siempre es",
               "menor que la tierra de cultivo y menor que la probabilidad: un municipio con mucha",
               "probabilidad pero poca tierra de cultivo queda con sensibilidad baja."))))
    )
  ),

  # --- Producción por cultivo -------------------------------------------------
  nav_panel("Producción por cultivo", value = "produccion",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("cultivo_p", "Cultivo", choices = sort(cultivos16), selected = "Maíz grano"),
        nota_16()
      ),
      intro("¿Dónde se produce cada cultivo?",
        p("El mapa muestra cuántas toneladas produjo cada municipio en 2025 del cultivo que elijas,",
          "según el cierre agrícola del SIAP. Solo se pintan los municipios que lo producen.")),
      layout_columns(
        value_box(title = "Municipios que lo producen", value = textOutput("p_mun")),
        value_box(title = "Toneladas producidas en 2025", value = textOutput("p_ton")),
        value_box(title = "Personas que trabajan en la agricultura", value = textOutput("p_agri"),
                  p("En estos municipios, en cualquier cultivo"))
      ),
      card(full_screen = TRUE, card_header(textOutput("t_prod", inline = TRUE)),
           leafletOutput("m_prod", height = "560px"))
    )
  ),

  # --- Pobreza y sensibilidad -------------------------------------------------
  nav_panel("Pobreza y sensibilidad", value = "filtros",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("cultivo_f", "Cultivo", choices = sort(cultivos16), selected = "Maíz grano"),
        nota_16(),
        accordion(open = TRUE,
          accordion_panel("Filtros",
            sliderInput("u_pob_f", "Pobreza: al menos este % de la población",
                        min = 0, max = 100, value = 0, step = 1),
            sliderInput("u_sens_f", "Sensibilidad agrícola (satélite FAO): al menos este % del territorio",
                        min = 0, max = sens_max, value = 0, step = 0.5))),
        p(class = "small text-muted",
          "Cada filtro actúa solo si lo mueves. Con el filtro de sensibilidad quedan fuera los",
          "municipios donde el satélite no tiene dato.")
      ),
      intro("¿Cuánto se produce donde hay pobreza y sequía?",
        p("Elige un cultivo y mueve los filtros: el mapa deja solo los municipios que lo producen y",
          "pasan los filtros, y los números dicen cuánto producen, cuánta gente vive ahí y cuántas",
          "personas trabajan en la agricultura."),
        p("La sensibilidad agrícola es la del paso 3 de la primera pestaña: la parte del territorio",
          "que se espera como tierra de cultivo con sequía severa en un año cualquiera.")),
      layout_columns(
        value_box(title = "Municipios que pasan los filtros", value = textOutput("f_mun")),
        value_box(title = "Toneladas del cultivo en 2025", value = textOutput("f_ton"),
                  p("Y su parte del total del país")),
        value_box(title = "Población 2025", value = textOutput("f_pob")),
        value_box(title = "Personas que trabajan en la agricultura", value = textOutput("f_agri"),
                  p("En estos municipios, en cualquier cultivo"))
      ),
      card(full_screen = TRUE, card_header(textOutput("t_filt", inline = TRUE)),
           leafletOutput("m_filt", height = "560px"))
    )
  )
)

# ---------------------------------------------------------------------------
# Servidor
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # ============================ Agricultura ====================================

  # Contorno del municipio seleccionado con el clic (lo usa Ganadería)
  marcar <- function(proxy, id_mapa, cv) {
    proxy <- clearGroup(proxy, "seleccion")
    if (is.null(cv)) return(proxy)
    addPolylines(proxy, data = st_boundary(filter(mun, CVEGEO == cv)), group = "seleccion",
                 color = "#00e5ff", weight = 3, opacity = 1, options = sin_clic)
  }

  # Paso 1: cambia entre % y hectáreas, así que se repinta sin perder el zoom
  output$a_suelo <- renderLeaflet(mapa_base())

  # El mapa avisa cuando ya existe en el navegador (manda su zoom); hasta entonces no se pinta
  listo <- reactiveVal(FALSE)
  observeEvent(input$a_suelo_zoom, listo(TRUE), once = TRUE)

  observe({
    req(listo())
    v <- if (input$unidad == "ha") "ha_agri" else "pct_agri"
    leafletProxy("a_suelo") |>
      clearShapes() |> removeControl("leyenda") |>
      dibuja(agri, clasifica(agri[[v]], v), "leyenda", et_suelo(agri))
  })

  # Pasos 2 y 3: no cambian con nada, se dibujan una sola vez
  output$a_fao  <- renderLeaflet(mapa_fao())
  output$a_sens <- renderLeaflet(
    dibuja(mapa_base(), agri, clasifica(agri$sens_fao, "sens_fao"), "leyenda", et_sens(agri)))

  # ============================ Producción por cultivo =========================

  prod_cultivo <- reactive({
    t <- filter(cultivos_mun, cultivo == input$cultivo_p) |> select(CVEGEO, toneladas)
    inner_join(mun, t, by = "CVEGEO")
  })

  output$p_mun  <- renderText(scales::comma(nrow(prod_cultivo())))
  output$p_ton  <- renderText(redondo(sum(prod_cultivo()$toneladas, na.rm = TRUE)))
  output$p_agri <- renderText(redondo(sum(prod_cultivo()$agricultura, na.rm = TRUE)))
  output$t_prod <- renderText(sprintf("%s · toneladas producidas en 2025 · %s municipios",
                                      input$cultivo_p, scales::comma(nrow(prod_cultivo()))))

  output$m_prod <- renderLeaflet(mapa_base())

  listo_p <- reactiveVal(FALSE)
  observeEvent(input$m_prod_zoom, listo_p(TRUE), once = TRUE)

  observe({
    req(listo_p())
    m <- prod_cultivo()
    leafletProxy("m_prod") |>
      clearShapes() |> removeControl("leyenda_p") |>
      dibuja(m, cl_ton(m$toneladas, cortes_ton(m$toneladas), input$cultivo_p), "leyenda_p",
             et_ton(m, input$cultivo_p))
  })

  # ============================ Pobreza y sensibilidad =========================

  # Todos los municipios que producen el cultivo: de aquí salen las clases del mapa y el total del país
  base_f <- reactive({
    t <- filter(cultivos_mun, cultivo == input$cultivo_f) |> select(CVEGEO, toneladas)
    inner_join(mun, t, by = "CVEGEO")
  })

  # Los que además pasan los filtros
  filtrados_f <- reactive({
    base_f() |>
      filter(cumple(pobreza,  input$u_pob_f,  0),
             cumple(sens_fao, input$u_sens_f, 0))
  })

  output$f_mun  <- renderText(scales::comma(nrow(filtrados_f())))
  output$f_ton  <- renderText({
    t <- sum(filtrados_f()$toneladas, na.rm = TRUE)
    sprintf("%s t (%.0f%%)", redondo(t), t / sum(base_f()$toneladas, na.rm = TRUE) * 100)
  })
  output$f_pob  <- renderText(redondo(sum(filtrados_f()$poblacion,   na.rm = TRUE)))
  output$f_agri <- renderText(redondo(sum(filtrados_f()$agricultura, na.rm = TRUE)))
  output$t_filt <- renderText(sprintf("%s · toneladas producidas en 2025 · %s municipios pasan los filtros",
                                      input$cultivo_f, scales::comma(nrow(filtrados_f()))))

  output$m_filt <- renderLeaflet(mapa_base())

  listo_f <- reactiveVal(FALSE)
  observeEvent(input$m_filt_zoom, listo_f(TRUE), once = TRUE)

  observe({
    req(listo_f())
    m <- filtrados_f()
    proxy <- leafletProxy("m_filt") |> clearShapes() |> removeControl("leyenda_f")
    if (nrow(m) == 0) {
      showNotification("Ningún municipio pasa esos filtros.", type = "warning")
      return()
    }
    # Las clases salen de todos los productores, para que los colores no cambien al filtrar
    proxy |>
      dibuja(m, cl_ton(m$toneladas, cortes_ton(base_f()$toneladas), input$cultivo_f), "leyenda_f",
             et_ton(m, input$cultivo_f))
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
