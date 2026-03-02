# ====================================================
# CORREGIMIENTOS DE DAGUA con domicilios
# 
# ====================================================

# 1. Librerías ------------------------------------------------------------
library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)
library(htmltools)

# 2. Cargar capas geográficas ---------------------------------------------
corregimientos <- st_read("Corregimientos.gpkg")
domicilios     <- st_read("Domicilios.shx")
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
# Cada corregimiento tendrá un color diferente según su nombre.
paleta_correg <- colorFactor(
  palette = "Set3",      # Paleta con colores variados (apta para categorías)
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

# 6. Preparar popups para los puntos de domicilio -------------------------
# Ajusta los nombres de columna según tu shapefile (ej: direccion, titular, etc.)
# Si no existen esos campos, se usará un texto genérico.
# Para conocer los nombres reales: names(domicilios)
if (all(c("direccion", "titular", "telefono") %in% names(domicilios))) {
  popup_puntos <- paste0(
    "<div style='font-family: Segoe UI, Arial; font-size: 13px; padding: 5px;'>",
    "<b style='color:#d35400;'>🏠 Domicilio</b><br>",
    "<b>Dirección:</b> ", domicilios$Nombre, "<br>",
    "<b>Titular:</b> ", domicilios$titular, "<br>",
    "<b>Teléfono:</b> ", domicilios$telefono,
    "</div>"
  )
} else {
  # Si no coinciden los campos, usa solo un texto simple
  popup_puntos <- "Domicilio de la empresa"
}

# 7. Construir el mapa ----------------------------------------------------
mapa <- leaflet() %>%
  
  # Mapas base (estilo claro/oscuro)
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Oscuro") %>%
  
  # Capa de polígonos (corregimientos coloreados por nombre)
  addPolygons(
    data = corregimientos,
    fillColor = ~paleta_correg(Nombre),
    fillOpacity = 0.7,
    color = "#FFFFFF",        # borde blanco
    weight = 1.2,
    opacity = 1,
    smoothFactor = 0.3,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#FFD700",       # dorado al resaltar
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
  
  # Capa de puntos (domicilios) - círculos de color fijo
  addCircleMarkers(
    data = domicilios,
    radius = 6,
    color = "#FF5733",        # borde naranja intenso
    fillColor = "#FFC300",     # relleno amarillo
    fillOpacity = 0.9,
    weight = 1.5,
    opacity = 1,
    popup = popup_puntos,
    label = ~paste("Domicilio:", ifelse("direccion" %in% names(domicilios), Nombre, "sin dirección")),
    group = "Domicilios",
    clusterOptions = markerClusterOptions()  # opcional: agrupa puntos si hay muchos
  ) %>%
  
  # Control de capas (permite activar/desactivar cada elemento)
  addLayersControl(
    baseGroups = c("Claro", "Oscuro"),
    overlayGroups = c("Corregimientos", "Domicilios"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  
  # Leyenda de corregimientos (muestra los colores asignados)
  addLegend(
    position = "bottomright",
    pal = paleta_correg,
    values = corregimientos$Nombre,
    title = htmltools::HTML("<span style='font-weight:600;'>Corregimientos</span>"),
    opacity = 0.9,
    group = "Corregimientos"
  ) %>%
  
  # Ajustar vista para abarcar ambas capas
  fitBounds(
    lng1 = min(c(st_bbox(corregimientos)[1], st_bbox(domicilios)[1]), na.rm = TRUE),
    lat1 = min(c(st_bbox(corregimientos)[2], st_bbox(domicilios)[2]), na.rm = TRUE),
    lng2 = max(c(st_bbox(corregimientos)[3], st_bbox(domicilios)[3]), na.rm = TRUE),
    lat2 = max(c(st_bbox(corregimientos)[4], st_bbox(domicilios)[4]), na.rm = TRUE)
  ) %>%
  
  # Escala gráfica
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))

# 8. Visualizar el mapa
mapa
