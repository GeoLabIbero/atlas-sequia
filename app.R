# Pobreza, uso de suelo, sequía y rendimiento por municipio
# GEOLab, IBERO Ciudad de México
#
# Esta carpeta (app/) es la que se publica: app.R + datos_app.rds.
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

caja <- st_bbox(mun)

# Años de inicio de episodios El Niño: MEI.v2 >= +0.5 durante al menos 5 bimestres (NOAA PSL)
anios_nino <- c(2002, 2006, 2009, 2015, 2023)

# Las seis variables, agrupadas por la pregunta que responden
variables <- list(
  "¿Quién?"          = c("Pobreza (%)"                 = "pobreza"),
  "¿Qué se siembra?" = c("Agricultura de temporal (%)" = "pct_temp",
                         "Agricultura de riego (%)"    = "pct_riego",
                         "Pastoril (%)"                = "pct_pasto"),
  "¿Qué amenaza?"    = c("Prob. de sequía en cultivo"  = "seq_crop",
                         "Prob. de sequía en pastizal" = "seq_past")
)

dic <- tibble(tema     = rep(names(variables), lengths(variables)),
              etiqueta = names(unlist(unname(variables))),
              var      = unname(unlist(variables)))

# Una paleta por tema, la misma familia de color que en el perfil del municipio
pal_tema <- c("¿Quién?" = "Reds", "¿Qué se siembra?" = "Greens", "¿Qué amenaza?" = "Oranges")

# Valores de cada municipio en formato largo, para el perfil
perfil <- st_drop_geometry(mun) |>
  select(CVEGEO, municipio, entidad_federativa, poblacion, all_of(dic$var)) |>
  pivot_longer(all_of(dic$var), names_to = "var", values_to = "valor") |>
  left_join(dic, by = "var")

# Rango de los deslizadores de la FAO
min_crop <- floor(min(mun$seq_crop,   na.rm = TRUE) * 100) / 100
max_crop <- ceiling(max(mun$seq_crop, na.rm = TRUE) * 100) / 100
min_past <- floor(min(mun$seq_past,   na.rm = TRUE) * 100) / 100
max_past <- ceiling(max(mun$seq_past, na.rm = TRUE) * 100) / 100

# Un umbral solo filtra si lo moviste de su mínimo
cumple <- \(x, umbral, minimo) if (umbral <= minimo) TRUE else !is.na(x) & x >= umbral

mapa_base <- function() {
  leaflet() |>
    addProviderTiles("Esri.WorldImagery",    group = "Satélite") |>
    addProviderTiles("Esri.WorldGrayCanvas", group = "Mapa claro") |>
    addLayersControl(baseGroups = c("Satélite", "Mapa claro"), position = "topleft",
                     options = layersControlOptions(collapsed = TRUE)) |>
    setView(lng = -102, lat = 23.5, zoom = 5)
}

# Resalta el municipio bajo el cursor sin que las líneas estorben al clic
resalte <- highlightOptions(weight = 2.5, color = "white", opacity = 1, bringToFront = TRUE)
sin_clic <- pathOptions(pointerEvents = "none")

# ---------------------------------------------------------------------------
# Interfaz
# ---------------------------------------------------------------------------

ui <- page_navbar(
  id    = "pestana",
  title = tags$span(
    tags$img(src = "logo_geolab.png", height = "56px", style = "margin-right: 10px;"),
    "Atlas de sequía y vulnerabilidad"
  ),
  window_title = "Atlas El Niño · GEOLab IBERO",
  theme = bs_theme(bootswatch = "flatly"),
  
  # --- Pestaña 1 --------------------------------------------------------------
  nav_panel("¿Quién es vulnerable?", value = "municipios",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("cultivo", "Cultivo", choices = sort(unique(donde$cultivo)),
                            selected = "Maíz grano"),
                selectInput("ciclo", "Ciclo productivo", choices = sort(unique(donde$ciclo)),
                            selected = "Primavera-Verano"),
                selectInput("var", "Variable del mapa", choices = variables),
                accordion(open = FALSE,
                          accordion_panel("Umbrales",
                                          sliderInput("u_pob",  "Pobreza mínima (%)", min = 0, max = 100, value = 0, step = 1),
                                          sliderInput("u_crop", "Prob. mínima de sequía en cultivo (FAO)",
                                                      min = min_crop, max = max_crop, value = min_crop, step = 0.01),
                                          sliderInput("u_past", "Prob. mínima de sequía en pastizal (FAO)",
                                                      min = min_past, max = max_past, value = min_past, step = 0.01))),
                p(class = "small text-muted",
                  "El mapa pinta los municipios que siembran el cultivo en ese ciclo y cumplen los ",
                  "umbrales. Haz clic en uno para ver su perfil. Detalle en ¿Cómo se calculó?")
              ),
              layout_columns(
                value_box(title = "Municipios que cumplen", value = textOutput("n_mun")),
                value_box(title = "Población 2020",         value = textOutput("n_pob")),
                value_box(title = "Hectáreas en riesgo de sequía (% del cultivo en el país)",
                          value = textOutput("n_ha"))
              ),
              layout_columns(
                col_widths = c(7, 5),
                card(full_screen = TRUE, card_header("Mapa"),
                     leafletOutput("mapa", height = "520px")),
                card(card_header("Perfil del municipio"),
                     plotOutput("perfil", height = "520px"))
              ),
              card(full_screen = TRUE, card_header("Rendimiento nacional del cultivo"),
                   plotOutput("rend", height = "320px"))
            )
  ),
  
  # --- Pestaña 2 --------------------------------------------------------------
  nav_panel("¿Cuánto está en riesgo?", value = "riesgo",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("cultivo2", "Cultivo", choices = sort(unique(ha_anio$cultivo)),
                            selected = "Maíz grano"),
                selectInput("ciclo2", "Ciclo productivo", choices = sort(unique(ha_anio$ciclo)),
                            selected = "Primavera-Verano"),
                radioButtons("medida", "Cómo contar la sequía",
                             choices = c("Presencia: D1 a D4 pesan igual"    = "pres",
                                         "Intensidad: D1 = 1, ..., D4 = 4"   = "int"),
                             selected = "pres"),
                radioButtons("vista2", "Qué ver en el mapa",
                             choices = c("Hectáreas en riesgo"               = "hr",
                                         "Probabilidad histórica de sequía"  = "p"),
                             selected = "hr"),
                accordion(open = FALSE,
                          accordion_panel("Cortes de la FAO (ASIS)",
                                          sliderInput("u_crop2", "Prob. mínima de sequía en cultivo",
                                                      min = min_crop, max = max_crop, value = min_crop, step = 0.01),
                                          sliderInput("u_past2", "Prob. mínima de sequía en pastizal",
                                                      min = min_past, max = max_past, value = min_past, step = 0.01),
                                          p(class = "small text-muted",
                                            "Escogen los municipios con la probabilidad de sequía de la FAO; las hectáreas en ",
                                            "riesgo se siguen calculando con la historia del Monitor."))),
                p(class = "small text-muted",
                  sprintf("Hectáreas en riesgo: superficie sembrada en %s por la parte de la temporada que ", ANIO_HA),
                  sprintf("cada municipio ha pasado en sequía, promedio %s-%s. Detalle en ¿Cómo se calculó?",
                          ANIO_INI_MSM, ANIO_HA))
              ),
              value_box(title = "Hectáreas en riesgo en un año típico",
                        value = textOutput("h_hist"),
                        p(textOutput("h_hist_desc", inline = TRUE))),
              layout_columns(
                col_widths = c(6, 6),
                card(full_screen = TRUE, card_header("El riesgo en el territorio"),
                     leafletOutput("mapa2", height = "520px")),
                card(full_screen = TRUE, card_header("Hectáreas en riesgo de sequía, por temporada"),
                     plotOutput("barras", height = "520px"))
              ),
              card(full_screen = TRUE, card_header("¿Coinciden la pobreza y la sequía?"),
                   plotOutput("disp", height = "480px", hover = hoverOpts("disp_hover", delay = 100)),
                   textOutput("disp_info"))
            )
  ),
  
  # --- Pestaña 3: nota metodológica -----------------------------------------------
  nav_panel("¿Cómo se calculó?", value = "metodologia",
            card(
              withMathJax(HTML(r"---(
<div style="max-width: 900px">

<h4>Fuentes</h4>
<p>SIAP, cierre agrícola municipal 2003&ndash;2025. CONAGUA, Monitor de Sequía de México por municipio
(se usan las temporadas desde 2016). Medición de pobreza municipal 2020. FAO, ASIS.
INEGI, Uso de suelo y vegetación Serie VII.</p>

<h4>Hectáreas en riesgo de sequía</h4>
<p>Pestaña <em>¿Quién es vulnerable?</em>. Adapta el método del SIAP: en lugar de la sequía de una sola quincena,
usa qué tan seguido ha estado cada municipio en sequía durante la temporada del cultivo.</p>

<p><strong>Variables.</strong> \(m\): municipio; \(k\): cultivo; \(c\): ciclo; \(y\): año; \(t\): corte del Monitor.
\(T_{c,y}\): cortes de la temporada \(y\) del ciclo \(c\). \(Y\): temporadas de 2016 al último año cerrado
del SIAP, \(y^*\).
\(H_{m,k,c}\): hectáreas sembradas del cultivo en el municipio en \(y^*\).
\(M_f\): municipios que siembran el cultivo y cumplen los umbrales elegidos.</p>

$$D_{m,t} = \begin{cases} 1 & \text{si el municipio está en D1, D2, D3 o D4} \\ 0 & \text{si está en D0 o sin sequía} \end{cases}$$

<p>Fracción de la temporada en sequía, cada año:</p>
$$f_{m,c,y} = \frac{1}{|T_{c,y}|} \sum_{t \in T_{c,y}} D_{m,t}$$

<p>Probabilidad histórica de sequía del municipio:</p>
$$P_{m,c} = \frac{1}{|Y|} \sum_{y \in Y} f_{m,c,y}$$

<p>Hectáreas en riesgo y su porcentaje del total nacional del cultivo:</p>
$$HR_{k,c} = \sum_{m \in M_f} H_{m,k,c} \, P_{m,c} \qquad \%HR_{k,c} = 100 \, \frac{HR_{k,c}}{\sum_{m} H_{m,k,c}}$$

<p>Se lee como la proporción de la superficie del cultivo que se espera en sequía en un momento
cualquiera de la temporada, según lo observado desde 2016.</p>

<h4>Temporadas</h4>
<p><strong>Primavera-Verano</strong>: cortes del Monitor de abril a septiembre del año \(y\).
<strong>Otoño-Invierno</strong>: cortes de octubre a diciembre del año \(y-1\) y de enero a marzo del año \(y\).
Se asignan así porque el SIAP registra ese ciclo en el año agrícola en que termina: en términos de año
calendario, el otoño-invierno inicia con los tres últimos meses del año anterior. Los perennes usan el
año calendario completo.</p>

<h4>Periodo</h4>
<p>Solo se usan las temporadas desde 2016. Hasta 2015 el Monitor asignaba la categoría al municipio si cubría
al menos el 40% de su superficie; desde 2016 asigna la de mayor intensidad observada en él. Mezclar los dos
criterios haría parecer más secos los años recientes. Desde 2016, además, todos los cortes son quincenales.</p>

<h4>Hectáreas en riesgo por temporada</h4>
<p>Pestaña <em>¿Cuánto está en riesgo?</em>. Cada barra de la gráfica usa la superficie sembrada ese mismo año,
\(H_{m,k,c,y}\), y la parte de esa temporada que cada municipio pasó en sequía:</p>
$$HR_{k,c,y} = \sum_{m} H_{m,k,c,y} \, f_{m,c,y}$$
<p>El contador y el mapa usan la fórmula del año típico, \(HR_{k,c}\), con la superficie del último año cerrado.
Sigue la lógica del indicador de producción bajo sequía del SIAP, con metodología de Banxico, pero en
hectáreas sembradas y con la probabilidad histórica en lugar de la sequía de una sola quincena.</p>

<h4>Presencia o intensidad</h4>
<p>Por defecto, cualquier categoría de D1 a D4 cuenta igual (\(D_{m,t} = 1\)). Con la opción de intensidad,
cada categoría pesa según su gravedad, llevada a una escala de 0 a 1:</p>
$$w_{m,t} = \begin{cases} 0 & \text{sin sequía o D0} \\ 1/4 & \text{D1} \\ 2/4 & \text{D2} \\ 3/4 & \text{D3} \\ 1 & \text{D4} \end{cases}$$
<p>y en las fórmulas \(D_{m,t}\) se sustituye por \(w_{m,t}\). El resultado ya no es una probabilidad sino una
intensidad histórica: 0 si el municipio nunca ha estado en sequía y 1 si siempre ha estado en sequía excepcional.
Los pesos 1 a 4 son una convención: no significa que D4 cause cuatro veces el daño de D1.</p>

<h4>Ejemplo paso a paso</h4>
<p>Dos municipios hipotéticos que siembran maíz de Primavera-Verano. Para simplificar se usan solo tres
temporadas; la app usa todas desde 2016.</p>

<p><strong>Paso 1. Cada fecha del Monitor: sequía sí o no.</strong> En la temporada 2023 hubo 12 fechas, de abril
a septiembre. El municipio A estuvo en sequía D1 o peor en 6 de ellas: vale 1 en esas 6 y 0 en las otras 6.
</p>

<p><strong>Paso 2. Cada temporada: qué parte pasó en sequía.</strong></p>
<table class="table table-sm" style="max-width: 560px">
<thead><tr><th>Temporada</th><th>Municipio A</th><th>Municipio B</th></tr></thead>
<tbody>
<tr><td>2022</td><td>3 de 12 = 0.25</td><td>0 de 12 = 0.00</td></tr>
<tr><td>2023</td><td>6 de 12 = 0.50</td><td>3 de 12 = 0.25</td></tr>
<tr><td>2024</td><td>0 de 12 = 0.00</td><td>0 de 12 = 0.00</td></tr>
</tbody></table>

<p><strong>Paso 3. Probabilidad histórica: el promedio de las temporadas.</strong></p>
<p>\(P_A = (0.25 + 0.50 + 0.00) / 3 = 0.25\) &nbsp;&nbsp; y &nbsp;&nbsp; \(P_B = (0.00 + 0.25 + 0.00) / 3 \approx 0.083\)</p>

<p><strong>Paso 4. Hectáreas en riesgo: superficie del último año cerrado por la probabilidad.</strong></p>
<table class="table table-sm" style="max-width: 560px">
<thead><tr><th>Municipio</th><th>Hectáreas sembradas</th><th>Probabilidad</th><th>Hectáreas en riesgo</th></tr></thead>
<tbody>
<tr><td>A</td><td>1,000</td><td>0.25</td><td>250</td></tr>
<tr><td>B</td><td>600</td><td>0.083</td><td>50</td></tr>
<tr><td><strong>Total</strong></td><td><strong>1,600</strong></td><td></td><td><strong>300</strong></td></tr>
</tbody></table>

<p><strong>Paso 5. Porcentaje:</strong> \(300 / 1{,}600 \approx 18.8\%\). Se lee: en un año típico, se espera que
casi una quinta parte de la superficie sembrada esté en sequía.</p>

<p><strong>La barra de una temporada</strong> usa las hectáreas de ese año y su fracción en sequía. Si en 2023 A sembró
900 hectáreas y B 500, la barra de 2023 es \(900 \times 0.50 + 500 \times 0.25 = 575\) hectáreas.</p>

<h4>Consideraciones</h4>
<ul>
<li>Ambos indicadores miden exposición, no pérdidas: un municipio en sequía moderada puede cosechar casi normal.</li>
<li>D0, anormalmente seco, no se cuenta como sequía.</li>
<li>Los municipios creados recientemente pueden no tener historia en el Monitor ni en el SIAP.</li>
</ul>

<h4>Referencias</h4>
<ul>
<li>DGSIAP. <em>Indicadores de Producción Agrícola Bajo Sequía</em>.
<a href="https://www.gob.mx/cms/uploads/attachment/file/1024087/Metodolog_a_Valor_de_Producci_n_Agr_cola_Bajo_Sequ_a.pdf" target="_blank">Metodología</a>.</li>
<li>Banco de México. Nota técnica del Recuadro 1, Reporte sobre las Economías Regionales, octubre-diciembre 2022.</li>
<li>CONAGUA, SMN. <a href="https://smn.conagua.gob.mx/es/climatologia/monitor-de-sequia/monitor-de-sequia-en-mexico" target="_blank">Monitor de Sequía en México</a>.</li>
</ul>

</div>
)---"))
            )
  )
)

# ---------------------------------------------------------------------------
# Servidor
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # ============================ Pestaña 1 =====================================
  
  # Municipios que siembran el cultivo en ese ciclo y cumplen los umbrales
  filtrados <- reactive({
    mun |>
      filter(CVEGEO %in% donde$CVEGEO[donde$cultivo == input$cultivo &
                                        donde$ciclo   == input$ciclo]) |>
      filter(cumple(pobreza,  input$u_pob,  0),
             cumple(seq_crop, input$u_crop, min_crop),
             cumple(seq_past, input$u_past, min_past))
  })
  
  output$n_mun <- renderText(scales::comma(nrow(filtrados())))
  output$n_pob <- renderText(scales::comma(sum(filtrados()$poblacion, na.rm = TRUE)))
  
  # Hectáreas en riesgo = hectáreas sembradas × probabilidad histórica de sequía (Monitor),
  # en los municipios que cumplen; y su % del total nacional del cultivo
  output$n_ha <- renderText({
    h      <- filter(ha_riesgo, cultivo == input$cultivo, ciclo == input$ciclo)
    total  <- sum(h$ha)
    riesgo <- sum((h$ha * h$p_hist)[h$CVEGEO %in% filtrados()$CVEGEO], na.rm = TRUE)
    if (total == 0) return(sprintf("sin siembra en %s", ANIO_HA))
    sprintf("%s ha (%.1f%%)", scales::comma(round(riesgo)), riesgo / total * 100)
  })
  
  output$mapa <- renderLeaflet(mapa_base())
  
  # Repinta los municipios filtrados, en 6 intervalos iguales
  observe({
    m <- filtrados()
    m <- m[!is.na(m[[input$var]]), ]          # sin dato: no se pinta
    proxy <- leafletProxy("mapa") |> clearShapes() |> removeControl("leyenda")
    
    if (nrow(m) == 0) {
      showNotification("Ningún municipio cumple esa combinación.", type = "warning")
      return()
    }
    
    x     <- m[[input$var]]
    rango <- range(x, na.rm = TRUE)
    if (diff(rango) == 0) rango <- rango + c(-0.01, 0.01)
    tema  <- dic$tema[dic$var == input$var]
    pal   <- colorBin(pal_tema[[tema]], domain = rango, bins = 6, pretty = TRUE, na.color = "#d9d9d9")
    
    proxy |>
      addPolygons(data = m, layerId = ~CVEGEO, fillColor = pal(x), fillOpacity = 0.7,
                  stroke = TRUE, weight = 0, color = "white", highlightOptions = resalte,
                  label = ~municipio) |>
      addPolylines(data = estados, color = "white", weight = 1.2, opacity = 0.9,
                   options = sin_clic) |>
      marcar(isolate(sel())) |>
      addLegend(layerId = "leyenda", pal = pal, values = x, position = "bottomright",
                title = dic$etiqueta[dic$var == input$var],
                labFormat = labelFormat(digits = if (str_starts(input$var, "seq")) 2 else 0))
  })
  
  # Municipio seleccionado con el clic
  sel <- reactiveVal(NULL)
  observeEvent(input$mapa_shape_click, {
    id <- input$mapa_shape_click$id
    if (!is.null(id)) sel(id)
  })
  
  # Contorno del municipio seleccionado
  marcar <- function(proxy, cv) {
    proxy <- clearGroup(proxy, "seleccion")
    if (is.null(cv)) return(proxy)
    addPolylines(proxy, data = st_boundary(filter(mun, CVEGEO == cv)), group = "seleccion",
                 color = "#00e5ff", weight = 3, opacity = 1, options = sin_clic)
  }
  observeEvent(sel(), leafletProxy("mapa") |> marcar(sel()))
  
  # Perfil: las seis variables del municipio, en porcentaje, y su población
  output$perfil <- renderPlot({
    validate(need(sel(), "Haz clic en un municipio del mapa."))
    
    d <- filter(perfil, CVEGEO == sel()) |>
      mutate(valor    = if_else(str_starts(var, "seq"), valor * 100, valor),
             etiqueta = factor(etiqueta, levels = rev(dic$etiqueta)),
             texto    = if_else(is.na(valor), "sin dato", sprintf("%.1f%%", valor)))
    
    ggplot(d, aes(valor, etiqueta, fill = tema)) +
      geom_col(width = 0.7, na.rm = TRUE) +
      geom_text(aes(x = coalesce(valor, 0), label = texto),
                hjust = -0.1, size = 4.2) +
      scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) +
      scale_fill_manual(values = c("¿Quién?"          = "#c0392b",
                                   "¿Qué se siembra?" = "#27ae60",
                                   "¿Qué amenaza?"    = "#e67e22")) +
      labs(title    = d$municipio[1],
           subtitle = sprintf("%s · Población 2020: %s",
                              d$entidad_federativa[1], scales::comma(d$poblacion[1])),
           x = "Valor (%)", y = NULL, fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position    = "top",
            plot.title         = element_text(face = "bold"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank())
  })
  
  # Rendimiento nacional del cultivo y ciclo elegidos, con los años Niño
  output$rend <- renderPlot({
    d <- filter(rend_nac, cultivo == input$cultivo, ciclo == input$ciclo)
    validate(need(nrow(d) > 0, "Sin registro de rendimiento para ese cultivo en ese ciclo."))
    nino <- anios_nino[between(anios_nino, min(d$Anio), max(d$Anio))]
    
    ggplot(d, aes(Anio, rendimiento)) +
      annotate("rect", xmin = nino - 0.5, xmax = nino + 0.5,
               ymin = -Inf, ymax = Inf, fill = "#f6c9a8", alpha = 0.6) +
      geom_line(colour = "#2c3e50", linewidth = 0.8) +
      geom_point(colour = "#2c3e50", size = 2.5) +
      scale_x_continuous(breaks = unique(d$Anio)) +
      labs(title = str_c(input$cultivo, " · ", input$ciclo), x = NULL, y = "t/ha cosechada",
           caption = "Franjas: años Niño (MEI.v2, NOAA PSL).") +
      theme_minimal(base_size = 13) +
      theme(plot.title         = element_text(face = "bold"),
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank())
  })
  
  # ============================ Pestaña 2 =====================================
  
  # Superficie del último año cerrado, probabilidad histórica y hectáreas en riesgo por municipio
  riesgo_mun <- reactive({
    ha_riesgo |>
      filter(cultivo == input$cultivo2, ciclo == input$ciclo2, ha > 0) |>
      left_join(st_drop_geometry(mun) |>
                  select(CVEGEO, municipio, entidad_federativa, pobreza, seq_crop, seq_past),
                by = "CVEGEO") |>
      mutate(p_hist = if (input$medida == "int") p_int else p_hist,
             hr     = ha * p_hist,
             prob   = p_hist * 100)
  })
  
  # Los municipios que cumplen los cortes de la FAO (un corte solo filtra si lo moviste de su mínimo)
  sel2 <- reactive({
    riesgo_mun() |>
      filter(cumple(seq_crop, input$u_crop2, min_crop),
             cumple(seq_past, input$u_past2, min_past))
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
    pct <- t$riesgo / t$total * 100
    sprintf("%s ha (%s)", scales::comma(round(t$riesgo)),
            if (pct > 0 & pct < 0.1) "<0.1%" else sprintf("%.1f%%", pct))
  })
  
  output$h_hist_desc <- renderText({
    t <- tipico()
    sprintf(str_c("En %s de los %s municipios que siembran el cultivo y cumplen los cortes de la FAO. ",
                  "El porcentaje es respecto a toda la superficie sembrada del cultivo en el país. ",
                  "Es la línea punteada de la gráfica."),
            scales::comma(t$n), scales::comma(t$n_tot))
  })
  
  output$mapa2 <- renderLeaflet(mapa_base())
  
  observe({
    req(input$pestana == "riesgo")
    
    m <- inner_join(mun, select(sel2(), CVEGEO, ha, p_hist, hr), by = "CVEGEO") |>
      filter(!is.na(p_hist))                  # sin historia en el Monitor: no se pinta
    proxy <- leafletProxy("mapa2") |> clearShapes() |> removeControl("leyenda2")
    if (nrow(m) == 0) {
      showNotification("Ningún municipio cumple esos umbrales.", type = "warning")
      return()
    }
    
    if (input$vista2 == "p") {
      # Probabilidad histórica, en porcentaje y en intervalos iguales
      x      <- m$p_hist * 100
      pal    <- colorBin("Oranges", domain = c(0, max(x, na.rm = TRUE)), bins = 6, pretty = TRUE,
                         na.color = "#d9d9d9")
      titulo <- if (input$medida == "int") "Intensidad histórica de sequía (0-100)"
      else "Probabilidad histórica de sequía (%)"
    } else {
      # Hectáreas en riesgo, en cuantiles porque unas pocas concentran mucho
      x      <- m$hr
      cortes <- unique(quantile(x, probs = seq(0, 1, length.out = 7), na.rm = TRUE))
      if (length(cortes) < 3) cortes <- pretty(range(x, na.rm = TRUE), 3)
      pal    <- colorBin("YlOrRd", domain = x, bins = cortes, na.color = "#d9d9d9")
      titulo <- "Hectáreas en riesgo"
    }
    
    m$txt_p <- if (input$medida == "int") sprintf("intensidad %.0f de 100", m$p_hist * 100)
    else sprintf("probabilidad %.0f%%", m$p_hist * 100)
    
    proxy |>
      addPolygons(data = m, fillColor = pal(x), fillOpacity = 0.75,
                  stroke = TRUE, weight = 0, color = "white", highlightOptions = resalte,
                  label = ~sprintf("%s: %s · %s ha sembradas · %s ha en riesgo",
                                   municipio, txt_p, scales::comma(round(ha)),
                                   scales::comma(round(hr)))) |>
      addPolylines(data = estados, color = "white", weight = 1.2, opacity = 0.9,
                   options = sin_clic) |>
      addLegend(layerId = "leyenda2", pal = pal, values = x, position = "bottomright", title = titulo,
                labFormat = labelFormat(big.mark = ",", digits = 0))
  })
  
  # Pobreza y probabilidad de sequía de cada municipio que siembra el cultivo
  disp_datos <- reactive({
    riesgo_mun() |>
      filter(!is.na(p_hist), !is.na(pobreza)) |>
      mutate(cumple = if_else(CVEGEO %in% sel2()$CVEGEO, "Cumple los cortes de la FAO", "No cumple"))
  })
  
  output$disp <- renderPlot({
    d <- disp_datos()
    validate(need(nrow(d) > 2, "Sin datos suficientes para ese cultivo y ciclo."))
    
    r     <- cor(d$prob, d$pobreza, method = "spearman")
    m_x   <- median(d$prob)
    m_y   <- median(d$pobreza)
    ambos <- filter(d, prob > m_x, pobreza > m_y)
    
    ggplot(d, aes(prob, pobreza, size = hr, colour = cumple)) +
      geom_vline(xintercept = m_x, linetype = "dashed", colour = "grey60") +
      geom_hline(yintercept = m_y, linetype = "dashed", colour = "grey60") +
      geom_point(alpha = 0.4) +
      scale_colour_manual(values = c("Cumple los cortes de la FAO" = "#c0392b", "No cumple" = "grey70"),
                          name = NULL) +
      scale_size_area(max_size = 9, labels = scales::label_comma(), name = "Hectáreas en riesgo") +
      labs(title    = str_c(input$cultivo2, " · ", input$ciclo2),
           subtitle = sprintf(str_c("Correlación de Spearman: %.2f · %s municipios arriba de ambas medianas, ",
                                    "con %s ha en riesgo (%.1f%% del total en riesgo)"),
                              r, scales::comma(nrow(ambos)), scales::comma(round(sum(ambos$hr))),
                              sum(ambos$hr) / sum(d$hr) * 100),
           x = if (input$medida == "int") "Intensidad histórica de sequía (0-100)"
           else "Probabilidad histórica de sequía (%)",
           y = "Pobreza (%)",
           caption = str_c("Cada punto es un municipio que siembra el cultivo. Líneas punteadas: medianas. ",
                           "En rojo, los que cumplen los cortes de la FAO.")) +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(colour = "grey25"),
            legend.position = "right",
            panel.grid.minor = element_blank())
  })
  
  # El municipio bajo el cursor
  output$disp_info <- renderText({
    if (is.null(input$disp_hover)) return("Pasa el cursor sobre un punto para ver el municipio.")
    p <- nearPoints(disp_datos(), input$disp_hover, xvar = "prob", yvar = "pobreza",
                    maxpoints = 1, threshold = 10)
    if (nrow(p) == 0) return("Pasa el cursor sobre un punto para ver el municipio.")
    txt_p <- if (input$medida == "int") sprintf("intensidad %.0f de 100", p$prob)
    else sprintf("probabilidad %.0f%%", p$prob)
    sprintf("%s, %s · pobreza %.1f%% · %s · %s ha sembradas · %s ha en riesgo",
            p$municipio, p$entidad_federativa, p$pobreza, txt_p,
            scales::comma(round(p$ha)), scales::comma(round(p$hr)))
  })
  
  # Hectáreas en riesgo cada temporada: superficie sembrada ese año × parte de la temporada en sequía
  output$barras <- renderPlot({
    d <- ha_anio |>
      filter(cultivo == input$cultivo2, ciclo == input$ciclo2, CVEGEO %in% sel2()$CVEGEO) |>
      inner_join(f_temp, by = c("CVEGEO", "Anio", "ciclo")) |>
      group_by(Anio) |>
      mutate(f = if (input$medida == "int") f_int else f) |>
      summarise(riesgo = sum(ha * f), total = sum(ha), .groups = "drop") |>
      mutate(nino = if_else(Anio %in% anios_nino, "Año Niño", "Otros años"))
    
    validate(need(nrow(d) > 0, "Sin superficie sembrada para ese cultivo en ese ciclo."))
    
    t <- tipico()
    
    ggplot(d, aes(Anio, riesgo / 1000, fill = nino)) +
      geom_col(width = 0.75) +
      geom_hline(yintercept = t$riesgo / 1000, linetype = "dashed", colour = "#2c3e50") +
      annotate("text", x = min(d$Anio) - 0.4, y = t$riesgo / 1000,
               label = sprintf("Año típico con la superficie de %s: %s ha",
                               ANIO_HA, scales::comma(round(t$riesgo))),
               hjust = 0, vjust = -0.6, size = 3.8, colour = "#2c3e50") +
      scale_fill_manual(values = c("Año Niño" = "#c0392b", "Otros años" = "grey70")) +
      scale_x_continuous(breaks = unique(d$Anio)) +
      labs(title = str_c(input$cultivo2, " · ", input$ciclo2),
           x = NULL, y = "Miles de hectáreas en riesgo", fill = NULL,
           caption = str_c("Cada barra: cómo fue esa temporada, con las hectáreas sembradas ese año. ",
                           "Línea horizontal: un año típico. Temporadas desde 2016.")) +
      theme_minimal(base_size = 13) +
      theme(legend.position    = "top",
            plot.title         = element_text(face = "bold"),
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x        = element_text(angle = 90, vjust = 0.5))
  })
}

shinyApp(ui, server)