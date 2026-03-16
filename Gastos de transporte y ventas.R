# ====================================================
# MAPA DE CORREGIMIENTOS DE DAGUA
# Ventas y Gasto de Transporte — datos reales del Excel
# ====================================================

# 1. Librerías --------------------------------------------------------
library(sf)
library(leaflet)
library(dplyr)
library(readxl)
library(readr)
library(htmlwidgets)
library(htmltools)

# Función: formato en millones (ej. $30.6M) o miles (ej. $425K)
fmt_cop <- function(x) {
  dplyr::case_when(
    x == 0          ~ "$0",
    x >= 1e6        ~ paste0("$", formatC(x / 1e6, format = "f", digits = 1), "M"),
    x >= 1e3        ~ paste0("$", formatC(x / 1e3, format = "f", digits = 0), "K"),
    TRUE            ~ paste0("$", formatC(x, format = "f", digits = 0))
  )
}

# 2. Capa geográfica --------------------------------------------------
corregimientos <- st_read("Corregimientos.gpkg", quiet = TRUE) %>%
  mutate(Nombre = case_when(
    Nombre == "Zelandio"      ~ "Zelandia",
    Nombre == "Jiguiales"     ~ "Jiguales",
    Nombre == "San Bernando"  ~ "San Bernardo",
    Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
    TRUE                       ~ Nombre
  ))

# 3. Leer Base de datos del Excel -------------------------------------
db <- read_excel("ventas.xlsx", sheet = "Base de datos")
names(db) <- make.names(names(db))

# 4. Limpiar valores y corregir nombres de corregimiento --------------
db_clean <- db %>%
  mutate(
    # "Villahermosa" en el Excel → "Villa Hermosa" en el gpkg
    Corregimiento = case_when(
      Corregimiento == "Villahermosa" ~ "Villa Hermosa",
      TRUE ~ Corregimiento
    ),
    transporte = parse_number(as.character(VLR..TRANSPORTE),
                              locale = locale(decimal_mark = ",", grouping_mark = ".")),
    ventas     = parse_number(as.character(VLR..PRODUCTOS),
                              locale = locale(decimal_mark = ",", grouping_mark = "."))
  ) %>%
  filter(!is.na(Corregimiento), Corregimiento != "NA")

# 5. Totales por corregimiento ----------------------------------------
ventas_por_correg <- db_clean %>%
  group_by(Corregimiento) %>%
  summarise(
    Ventas          = sum(ventas,     na.rm = TRUE),
    GastoTransporte = sum(transporte, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Unir datos espaciales con datos de ventas ------------------------
corr_datos <- corregimientos %>%
  left_join(ventas_por_correg, by = c("Nombre" = "Corregimiento")) %>%
  mutate(
    Ventas          = ifelse(is.na(Ventas),          0, Ventas),
    GastoTransporte = ifelse(is.na(GastoTransporte), 0, GastoTransporte)
  ) %>%
  filter(Ventas > 0)

if (nrow(corr_datos) == 0) stop("Sin corregimientos con ventas > 0. Revisa los datos.")

# 7. Transformar a WGS84 y reparar geometrías -------------------------
corr_datos <- st_transform(corr_datos, 4326) %>% st_make_valid()

# 8. Puntos internos para etiquetas -----------------------------------
pts    <- st_point_on_surface(corr_datos)
coords <- st_coordinates(pts)
corr_datos <- corr_datos %>%
  mutate(lng = coords[, 1], lat = coords[, 2])

# 9. Paletas de color -------------------------------------------------
pal_v <- colorNumeric("YlOrRd", domain = corr_datos$Ventas,          na.color = "#808080")
pal_t <- colorNumeric("Blues",  domain = corr_datos$GastoTransporte, na.color = "#808080")

# 10. Popups detallados (clic) ----------------------------------------
popup_v <- paste0(
  "<div style='font-family:Segoe UI,Arial;font-size:14px;padding:10px 14px;min-width:190px;'>",
  "<div style='font-size:16px;font-weight:700;color:#1f4e79;border-bottom:2px solid #e0e0e0;",
  "padding-bottom:5px;margin-bottom:8px;'>", corr_datos$Nombre, "</div>",
  "<div style='color:#27ae60;font-size:15px;'><b>Ventas: ",
  format(corr_datos$Ventas, big.mark = ".", decimal.mark = ",", scientific = FALSE),
  "</b></div>",
  "</div>"
)

popup_t <- paste0(
  "<div style='font-family:Segoe UI,Arial;font-size:14px;padding:10px 14px;min-width:190px;'>",
  "<div style='font-size:16px;font-weight:700;color:#1f4e79;border-bottom:2px solid #e0e0e0;",
  "padding-bottom:5px;margin-bottom:8px;'>", corr_datos$Nombre, "</div>",
  "<div style='color:#2980b9;font-size:15px;'><b>Gasto transporte: ",
  format(corr_datos$GastoTransporte, big.mark = ".", decimal.mark = ",", scientific = FALSE),
  "</b></div>",
  "</div>"
)

# 11. Etiquetas permanentes por corregimiento -------------------------
etiquetas <- lapply(seq_len(nrow(corr_datos)), function(i) {
  nom <- corr_datos$Nombre[i]
  v   <- fmt_cop(corr_datos$Ventas[i])
  t   <- fmt_cop(corr_datos$GastoTransporte[i])

  HTML(paste0(
    "<div style='",
      "font-family: Segoe UI, Arial, sans-serif;",
      "text-align: center;",
      "background: rgba(255,255,255,0.93);",
      "border: 1px solid #bbb;",
      "border-radius: 6px;",
      "padding: 4px 8px;",
      "box-shadow: 0 2px 6px rgba(0,0,0,0.18);",
      "line-height: 1.5;",
      "white-space: nowrap;",
    "'>",
    "<div style='font-size:12px;font-weight:700;color:#1f4e79;'>", nom, "</div>",
    "<div style='font-size:11px;'>",
      "<span style='color:#27ae60;font-weight:600;'>&#x25B2; ", v, "</span>",
      "&nbsp;&nbsp;",
      "<span style='color:#2980b9;font-weight:600;'>&#x1F69A; ", t, "</span>",
    "</div>",
    "</div>"
  ))
})

# 12. Construir mapa --------------------------------------------------
mapa <- leaflet() %>%

  addProviderTiles(providers$CartoDB.Positron,   group = "Claro") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Oscuro") %>%

  # — Capa Ventas
  addPolygons(
    data        = corr_datos,
    fillColor   = ~pal_v(Ventas),
    fillOpacity = 0.72,
    color = "#FFFFFF", weight = 1.5, opacity = 1, smoothFactor = 0.3,
    highlightOptions = highlightOptions(
      weight = 3, color = "#FFD700", fillOpacity = 0.9, bringToFront = TRUE
    ),
    label = ~paste0(Nombre, "  |  Ventas: ", fmt_cop(Ventas)),
    popup = popup_v,
    group = "Ventas"
  ) %>%

  # — Capa Gasto Transporte
  addPolygons(
    data        = corr_datos,
    fillColor   = ~pal_t(GastoTransporte),
    fillOpacity = 0.72,
    color = "#FFFFFF", weight = 1.5, opacity = 1, smoothFactor = 0.3,
    highlightOptions = highlightOptions(
      weight = 3, color = "#FFD700", fillOpacity = 0.9, bringToFront = TRUE
    ),
    label = ~paste0(Nombre, "  |  Transporte: ", fmt_cop(GastoTransporte)),
    popup = popup_t,
    group = "Gasto Transporte"
  ) %>%

  # — Etiquetas permanentes (una por corregimiento)
  addLabelOnlyMarkers(
    data = corr_datos,
    lng  = ~lng,
    lat  = ~lat,
    label        = etiquetas,
    labelOptions = labelOptions(
      noHide    = TRUE,
      direction = "center",
      textOnly  = TRUE,
      offset    = c(0, 0)
    ),
    group = "Etiquetas"
  ) %>%

  # — Control de capas
  addLayersControl(
    baseGroups    = c("Claro", "Oscuro"),
    overlayGroups = c("Ventas", "Gasto Transporte", "Etiquetas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # — Leyenda Ventas
  addLegend(
    position = "bottomright",
    pal      = pal_v,
    values   = corr_datos$Ventas,
    title    = HTML("<b style='color:#27ae60;'>&#x25B2; Ventas ($)</b>"),
    labFormat = labelFormat(
      prefix   = "$",
      transform = function(x) formatC(x, format = "f", big.mark = ".", digits = 0)
    ),
    opacity = 0.9
  ) %>%

  # — Leyenda Transporte
  addLegend(
    position = "bottomleft",
    pal      = pal_t,
    values   = corr_datos$GastoTransporte,
    title    = HTML("<b style='color:#2980b9;'>&#x1F69A; Gasto transporte ($)</b>"),
    labFormat = labelFormat(
      prefix   = "$",
      transform = function(x) formatC(x, format = "f", big.mark = ".", digits = 0)
    ),
    opacity = 0.9
  ) %>%

  fitBounds(
    lng1 = st_bbox(corr_datos)[1], lat1 = st_bbox(corr_datos)[2],
    lng2 = st_bbox(corr_datos)[3], lat2 = st_bbox(corr_datos)[4]
  ) %>%

  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))

# 13. Mostrar ---------------------------------------------------------
mapa
