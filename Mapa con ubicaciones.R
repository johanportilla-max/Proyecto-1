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
# Asegúrate de que la ruta y extensión sean correctas (para domicilios, debe ser .shp)
corregimientos <- st_read("Corregimientos.gpkg")
domicilios     <- st_read("Domicilios.shp")   # Cambia a .shp si era .shx

# 3. Asegurar que ambas capas estén en WGS84 (EPSG:4326) para leaflet -----
if (st_crs(corregimientos) != st_crs(4326)) {
  corregimientos <- st_transform(corregimientos, 4326)
}
if (st_crs(domicilios) != st_crs(4326)) {
  domicilios <- st_transform(domicilios, 4326)
}

# 4. Reparar geometrías inválidas en corregimientos -----------------------
# Esto evita errores de renderizado (como "Loop 0 is not valid")
corregimientos <- st_make_valid(corregimientos)

# 5. Corregir nombres de corregimientos mal escritos ----------------------
corregimientos <- corregimientos %>%
  mutate(Nombre = case_when(
    Nombre == "Zelandio"     ~ "Zelandia",
    Nombre == "Jiguiales"    ~ "Jiguales",
    Nombre == "San Bernando" ~ "San Bernardo",
    Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
    TRUE                      ~ Nombre
  ))

# 6. Identificar columna de nombre en domicilios --------------------------
# Busca columnas comunes que puedan contener el nombre del lugar
posibles_nombres <- c("Nombre", "nombre", "NOMBRE", "lugar", "sitio", "direccion", "titular")
nombre_col_dom <- intersect(posibles_nombres, names(domicilios))[1]
if (is.na(nombre_col_dom)) {
  nombre_col_dom <- names(domicilios)[1]
  warning("No se encontró una columna de nombre clara. Se usará: ", nombre_col_dom)
}

# 7. Paleta de colores para corregimientos --------------------------------
paleta_correg <- colorFactor(palette = "Set3", domain = corregimientos$Nombre)

# 8. Popups y etiquetas (hover) para polígonos ----------------------------
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

# 9. Popups para puntos de domicilio (al hacer clic) ----------------------
campos_extras <- intersect(c("direccion", "titular", "telefono"), names(domicilios))
if (length(campos_extras) > 0) {
  texto_base <- paste0(
    "<div style='font-family: Segoe UI, Arial; font-size: 13px; padding: 5px;'>",
    "<b style='color:#d35400;'>🏠 ", domicilios[[nombre_col_dom]], "</b><br>"
  )
  for (campo in campos_extras) {
    texto_base <- paste0(texto_base, "<b>", campo, ":</b> ", domicilios[[campo]], "<br>")
  }
  popup_puntos <- paste0(texto_base, "</div>")
} else {
  popup_puntos <- paste0(
    "<div style='font-family: Segoe UI, Arial; font-size: 13px; padding: 5px;'>",
    "<b style='color:#d35400;'>🏠 ", domicilios[[nombre_col_dom]], "</b><br>",
    "</div>"
  )
}

# 10. Etiquetas permanentes para domicilios (visibles siempre) ------------
etiquetas_permanentes <- labelOptions(
  noHide = TRUE,
  direction = "top",
  textOnly = TRUE,
  style = list(
    "font-family" = "Segoe UI, Arial",
    "font-size" = "11px",
    "font-weight" = "bold",
    "color" = "#2c3e50",
    "background-color" = "rgba(255, 255, 255, 0.8)",
    "padding" = "2px 6px",
    "border-radius" = "4px",
    "border" = "1px solid #ccc",
    "box-shadow" = "0 2px 4px rgba(0,0,0,0.2)"
  )
)

# 11. Construir el mapa ---------------------------------------------------
mapa <- leaflet() %>%
  
  # Mapas base
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Oscuro") %>%
  
  # Capa de polígonos (corregimientos) - sin etiquetas permanentes
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
    label = label_poligonos,           # Nombre al pasar el mouse
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
    popup = popup_poligonos,            # Nombre al hacer clic
    group = "Corregimientos"
  ) %>%
  
  # Capa de puntos (domicilios) - círculos
  addCircleMarkers(
    data = domicilios,
    radius = 6,
    color = "#FF5733",
    fillColor = "#FFC300",
    fillOpacity = 0.9,
    weight = 1.5,
    opacity = 1,
    popup = popup_puntos,
    label = ~domicilios[[nombre_col_dom]],   # Etiqueta hover (opcional, pero se puede dejar)
    group = "Domicilios",
    clusterOptions = markerClusterOptions()  # Quitar si no se desea agrupar
  ) %>%
  
  # Etiquetas permanentes para domicilios (visibles siempre)
  addLabelOnlyMarkers(
    data = domicilios,
    lng = ~st_coordinates(domicilios)[,1],
    lat = ~st_coordinates(domicilios)[,2],
    label = ~domicilios[[nombre_col_dom]],
    labelOptions = etiquetas_permanentes,
    group = "Nombres domicilios"
  ) %>%
  
  # Control de capas (activa/desactiva capas)
  addLayersControl(
    baseGroups = c("Claro", "Oscuro"),
    overlayGroups = c("Corregimientos", "Domicilios", "Nombres domicilios"),
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
  
  # Ajustar vista para abarcar ambas capas
  fitBounds(
    lng1 = min(c(st_bbox(corregimientos)[1], st_bbox(domicilios)[1]), na.rm = TRUE),
    lat1 = min(c(st_bbox(corregimientos)[2], st_bbox(domicilios)[2]), na.rm = TRUE),
    lng2 = max(c(st_bbox(corregimientos)[3], st_bbox(domicilios)[3]), na.rm = TRUE),
    lat2 = max(c(st_bbox(corregimientos)[4], st_bbox(domicilios)[4]), na.rm = TRUE)
  ) %>%
  
  # Escala gráfica
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))

# 12. Visualizar el mapa
mapa