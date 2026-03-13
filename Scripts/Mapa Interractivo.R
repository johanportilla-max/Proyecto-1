# ====================================================
# CORREGIMIENTOS DE DAGUA con ubicaciones de domicilios
# ====================================================

# 1. Librerías ------------------------------------------------------------
library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)
library(htmltools)

# 2. Cargar capas geográficas ---------------------------------------------
corregimientos <- st_read("Corregimientos.gpkg")
domicilios     <- st_read("Domicilios.shp")

# 3. Corregir nombres de corregimientos mal escritos ----------------------
corregimientos <- corregimientos %>%
  mutate(Nombre = case_when(
    Nombre == "Zelandio"     ~ "Zelandia",
    Nombre == "Jiguiales"    ~ "Jiguales",
    Nombre == "San Bernando" ~ "San Bernardo",
    Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
    TRUE                      ~ Nombre
  ))

# 4. Crear una paleta de colores para los corregimientos (cualitativa) ----
paleta_correg <- colorFactor(
  palette = "Set3",
  domain = corregimientos$Nombre
)

# 5. Preparar popups y etiquetas para los polígonos -----------------------
popup_poligonos <- paste0(
  "<div style='font-family: Segoe UI, Arial; font-size: 14px; padding: 8px;'>",
  "<b style='color: #1f4e79;'>", corregimientos$Nombre, "</b><br>",
  "</div>"
)

label_poligonos <- lapply(seq_len(nrow(corregimientos)), function(i) {
  HTML(paste0(
    "<div style='font-family: Segoe UI, Arial;'>",
    "<strong>", corregimientos$Nombre[i], "</strong>",
    "</div>"
  ))
})

# 6. Detectar la columna de nombre en domicilios --------------------------
# Busca la columna que contiene el nombre de la ubicación (insensible a mayúsculas)
pos_nombre <- which(tolower(names(domicilios)) == "nombre")

if (length(pos_nombre) == 0) {
  warning("No se encontró una columna llamada 'nombre' en domicilios. Se usará un texto genérico.")
  col_nombre <- NULL
} else {
  col_nombre <- names(domicilios)[pos_nombre[1]]
  message("✅ Columna de nombre detectada: ", col_nombre)
}

# 7. Preparar popups y etiquetas para los puntos de domicilio -------------
if (!is.null(col_nombre)) {
  # Popup: muestra el nombre de la ubicación
  popup_puntos <- paste0(
    "<div style='font-family: Segoe UI, Arial; font-size: 13px; padding: 5px;'>",
    "<b style='color:#d35400;'>🏠 Domicilio</b><br>",
    "<b>Nombre:</b> ", domicilios[[col_nombre]], "<br>",
    "</div>"
  )
  
  # Etiqueta hover: generamos una lista de HTML igual que en los polígonos
  label_puntos <- lapply(seq_len(nrow(domicilios)), function(i) {
    HTML(paste0(
      "<div style='font-family: Segoe UI, Arial;'>",
      "<strong>", domicilios[[col_nombre]][i], "</strong>",
      "</div>"
    ))
  })
} else {
  popup_puntos <- "Domicilio de la empresa"
  label_puntos <- "Domicilio"
}

# 8. Construir el mapa ----------------------------------------------------
mapa <- leaflet() %>%
  
  # Mapas base
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Oscuro") %>%
  
  # Capa de polígonos
  addPolygons(
    data = corregimientos,
    fillColor = ~paleta_correg(Nombre),
    fillOpacity = 0.7,
    color = "#FFFFFF",
    weight = 1.2,
    opacity = 1,
    smoothFactor = 0.3,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#FFD700",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = label_poligonos,
    labelOptions = labelOptions(
      style = list(
        "font-family" = "Segoe UI, Arial",
        "font-size" = "12px",
        "padding" = "6px 12px",
        "border-radius" = "6px",
        "background-color" = "rgba(255,255,255,0.95)",
        "box-shadow" = "0 2px 8px rgba(0,0,0,0.15)"
      ),
      textsize = "12px",
      direction = "auto"
    ),
    popup = popup_poligonos,
    group = "Corregimientos"
  ) %>%
  
  # Capa de puntos
  addCircleMarkers(
    data = domicilios,
    radius = 6,
    color = "#FF5733",
    fillColor = "#FFC300",
    fillOpacity = 0.9,
    weight = 1.5,
    opacity = 1,
    popup = popup_puntos,
    label = label_puntos,
    group = "Domicilios",
    clusterOptions = markerClusterOptions()  # Opcional, quita si no quieres agrupamiento
  ) %>%
  
  # Control de capas
  addLayersControl(
    baseGroups = c("Claro", "Oscuro"),
    overlayGroups = c("Corregimientos", "Domicilios"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  
  # Leyenda de corregimientos
  addLegend(
    position = "bottomright",
    pal = paleta_correg,
    values = corregimientos$Nombre,
    title = htmltools::HTML("<span style='font-weight:600;'>Corregimientos</span>"),
    opacity = 0.9,
    group = "Corregimientos"
  ) %>%
  
  # Ajustar vista
  fitBounds(
    lng1 = min(c(st_bbox(corregimientos)[1], st_bbox(domicilios)[1]), na.rm = TRUE),
    lat1 = min(c(st_bbox(corregimientos)[2], st_bbox(domicilios)[2]), na.rm = TRUE),
    lng2 = max(c(st_bbox(corregimientos)[3], st_bbox(domicilios)[3]), na.rm = TRUE),
    lat2 = max(c(st_bbox(corregimientos)[4], st_bbox(domicilios)[4]), na.rm = TRUE)
  ) %>%
  
  # Escala
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))

# 9. Visualizar el mapa
mapa