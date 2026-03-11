# ====================================================
# MAPA DE CORREGIMIENTOS DE DAGUA: VENTAS Y GASTO DE TRANSPORTE
# ====================================================
# ====================================================
# MAPA DE CORREGIMIENTOS CON VENTAS > 0
# Etiquetas permanentes con Ventas y Gasto de Transporte
# ====================================================

# 1. Librerías ------------------------------------------------------------
library(sf)
library(leaflet)
library(dplyr)
library(readxl)
library(readr)
library(htmlwidgets)
library(htmltools)

# 2. Cargar capa geográfica ------------------------------------------------
corregimientos <- st_read("Corregimientos.gpkg")

# 3. Leer hojas del Excel --------------------------------------------------
listas <- read_excel("ventas.xlsx", sheet = "Listas")
db     <- read_excel("ventas.xlsx", sheet = "Base de datos")

# 4. Limpiar nombres de columnas -------------------------------------------
names(listas) <- make.names(names(listas))
names(db)     <- make.names(names(db))

# 5. Limpiar y convertir valores numéricos en db ---------------------------
db_clean <- db %>%
  mutate(
    transporte = parse_number(as.character(VLR..TRANSPORTE), 
                              locale = locale(decimal_mark = ",", grouping_mark = ".")),
    total_fact = parse_number(as.character(VLR..TOTAL.FACT.), 
                              locale = locale(decimal_mark = ",", grouping_mark = ".")),
    productos  = parse_number(as.character(VLR..PRODUCTOS), 
                              locale = locale(decimal_mark = ",", grouping_mark = ".")),
    ID = as.integer(ID)
  ) %>%
  filter(!is.na(ID))

# 6. Unir con listas para obtener el nombre del corregimiento -------------
listas_clean <- listas %>%
  mutate(
    ID = as.integer(ID),
    Corregimiento = case_when(
      Corregimiento == "Zelandio"     ~ "Zelandia",
      Corregimiento == "Jiguiales"    ~ "Jiguales",
      Corregimiento == "San Bernando" ~ "San Bernardo",
      Corregimiento == "Borreo Ayerbe" ~ "Borrero Ayerbe",
      TRUE ~ Corregimiento
    )
  )


# 7. Agrupar por corregimiento para obtener totales ------------------------
ventas_por_correg <- db_clean%>%
  group_by(Corregimiento) %>%
  summarise(
    Ventas          = sum(productos, na.rm = TRUE),
    GastoTransporte = sum(transporte, na.rm = TRUE),
    .groups = "drop"
  )

# 8. Unir con la capa de corregimientos ------------------------------------
corregimientos <- corregimientos %>%
  mutate(Nombre = case_when(
    Nombre == "Zelandio"     ~ "Zelandia",
    Nombre == "Jiguiales"    ~ "Jiguales",
    Nombre == "San Bernando" ~ "San Bernardo",
    Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
    TRUE                      ~ Nombre
  ))

corregimientos_con_datos <- corregimientos %>%
  left_join(ventas_por_correg, by = c("Nombre" = "Corregimiento")) %>%
  mutate(
    Ventas          = ifelse(is.na(Ventas), 0, Ventas),
    GastoTransporte = ifelse(is.na(GastoTransporte), 0, GastoTransporte)
  )

# 9. FILTRAR: conservar solo corregimientos con Ventas > 0 -----------------
corregimientos_con_datos <- corregimientos_con_datos %>%
  filter(Ventas > 0)

# Si después del filtro no queda ningún registro, mostrar advertencia
if (nrow(corregimientos_con_datos) == 0) {
  stop("No hay corregimientos con ventas mayores a cero. Revisa tus datos.")
}

# 10. Transformar a WGS84 y reparar geometrías -----------------------------
if (st_crs(corregimientos_con_datos) != st_crs(4326)) {
  corregimientos_con_datos <- st_transform(corregimientos_con_datos, 4326)
}
corregimientos_con_datos <- st_make_valid(corregimientos_con_datos)

# Verificar si aún hay geometrías inválidas
if (any(!st_is_valid(corregimientos_con_datos))) {
  warning("Aún hay geometrías inválidas. Se aplicará buffer 0.")
  corregimientos_con_datos <- st_buffer(corregimientos_con_datos, dist = 0)
}

# 11. Calcular puntos sobre la superficie ----------------------------------
puntos_superficie <- st_point_on_surface(corregimientos_con_datos)
coords <- st_coordinates(puntos_superficie)
puntos_superficie <- puntos_superficie %>%
  mutate(lng = coords[,1], lat = coords[,2])

# 12. Crear paletas de colores continuas -----------------------------------
# (ya no es necesario el caso de todos 0 porque filtramos Ventas > 0, 
#  pero por si acaso lo mantenemos)
if (all(corregimientos_con_datos$Ventas == 0)) {
  pal_ventas <- colorNumeric(palette = "YlOrRd", domain = c(0,1), na.color = "#808080")
} else {
  pal_ventas <- colorNumeric(palette = "YlOrRd", domain = corregimientos_con_datos$Ventas, na.color = "#808080")
}

if (all(corregimientos_con_datos$GastoTransporte == 0)) {
  pal_gasto <- colorNumeric(palette = "Blues", domain = c(0,1), na.color = "#808080")
} else {
  pal_gasto <- colorNumeric(palette = "Blues", domain = corregimientos_con_datos$GastoTransporte, na.color = "#808080")
}

# 13. Preparar popups ------------------------------------------------------
popup_ventas <- paste0(
  "<div style='font-family: Segoe UI, Arial; font-size: 14px; padding: 8px;'>",
  "<b style='color: #1f4e79;'>", corregimientos_con_datos$Nombre, "</b><br>",
  "<b>Ventas (productos):</b> $", format(corregimientos_con_datos$Ventas, big.mark = ",", scientific = FALSE), "<br>",
  "</div>"
)

popup_gasto <- paste0(
  "<div style='font-family: Segoe UI, Arial; font-size: 14px; padding: 8px;'>",
  "<b style='color: #1f4e79;'>", corregimientos_con_datos$Nombre, "</b><br>",
  "<b>Gasto transporte:</b> $", format(corregimientos_con_datos$GastoTransporte, big.mark = ",", scientific = FALSE), "<br>",
  "</div>"
)

# 14. Etiquetas permanentes ------------------------------------------------
etiquetas_texto <- paste0(
  "<div style='font-family: Segoe UI, Arial; font-size: 11px; font-weight: bold; ",
  "background-color: rgba(255,255,255,0.8); padding: 2px 6px; border-radius: 4px; ",
  "border: 1px solid #333; box-shadow: 0 2px 4px rgba(0,0,0,0.3);'>",
  puntos_superficie$Nombre, "<br>",
  "V: $", format(puntos_superficie$Ventas, big.mark = ",", scientific = FALSE), "<br>",
  "T: $", format(puntos_superficie$GastoTransporte, big.mark = ",", scientific = FALSE),
  "</div>"
)

etiquetas_opciones <- labelOptions(
  noHide = TRUE,
  direction = "center",
  textOnly = FALSE,
  style = NULL,
  offset = c(0, 0)
)

# 15. Construir el mapa ----------------------------------------------------
mapa <- leaflet() %>%
  
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Oscuro") %>%
  
  # Capa de Ventas
  addPolygons(
    data = corregimientos_con_datos,
    fillColor = ~pal_ventas(Ventas),
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
    label = ~paste(Nombre, "- Ventas: $", format(Ventas, big.mark = ",")),
    popup = popup_ventas,
    group = "Ventas"
  ) %>%
  
  # Capa de Gasto Transporte
  addPolygons(
    data = corregimientos_con_datos,
    fillColor = ~pal_gasto(GastoTransporte),
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
    label = ~paste(Nombre, "- Gasto: $", format(GastoTransporte, big.mark = ",")),
    popup = popup_gasto,
    group = "Gasto Transporte"
  ) %>%
  
  # Etiquetas permanentes
  addLabelOnlyMarkers(
    data = puntos_superficie,
    lng = ~lng,
    lat = ~lat,
    label = lapply(etiquetas_texto, HTML),
    labelOptions = etiquetas_opciones,
    group = "Etiquetas"
  ) %>%
  
  # Control de capas
  addLayersControl(
    baseGroups = c("Claro", "Oscuro"),
    overlayGroups = c("Ventas", "Gasto Transporte", "Etiquetas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Leyendas
  addLegend(
    position = "bottomright",
    pal = pal_ventas,
    values = corregimientos_con_datos$Ventas,
    title = htmltools::HTML("<span style='font-weight:600;'>Ventas ($)</span>"),
    opacity = 0.9,
    group = "Ventas"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_gasto,
    values = corregimientos_con_datos$GastoTransporte,
    title = htmltools::HTML("<span style='font-weight:600;'>Gasto transporte ($)</span>"),
    opacity = 0.9,
    group = "Gasto Transporte"
  ) %>%
  
  # Ajustar vista a los datos filtrados
  fitBounds(
    lng1 = min(st_bbox(corregimientos_con_datos)[1], na.rm = TRUE),
    lat1 = min(st_bbox(corregimientos_con_datos)[2], na.rm = TRUE),
    lng2 = max(st_bbox(corregimientos_con_datos)[3], na.rm = TRUE),
    lat2 = max(st_bbox(corregimientos_con_datos)[4], na.rm = TRUE)
  ) %>%
  
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))

# 16. Visualizar
mapa
