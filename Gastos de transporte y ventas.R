# ====================================================
# MAPA DE CORREGIMIENTOS DE DAGUA: VENTAS Y GASTO DE TRANSPORTE
# ====================================================
# ====================================================
# MAPA DE CORREGIMIENTOS: VENTAS Y GASTO DE TRANSPORTE
# Basado en archivo ventas.xlsx (hojas "Listas" y "Base de datos")
# ====================================================
# ====================================================
# MAPA DE CORREGIMIENTOS: VENTAS Y GASTO DE TRANSPORTE
# Versión corregida y robusta
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

# 4. Limpiar nombres de columnas (evita espacios y caracteres especiales) --
names(listas) <- make.names(names(listas))
names(db)     <- make.names(names(db))

# Verifica que los nombres sean los esperados (opcional, puedes descomentar)
# print(names(db))

# 5. Limpiar y convertir valores numéricos en db ---------------------------
db_clean <- db %>%
  mutate(
    # Convertir a número usando locale con coma decimal y punto de miles
    transporte = parse_number(as.character(VLR..TRANSPORTE), 
                              locale = locale(decimal_mark = ",", grouping_mark = ".")),
    total_fact = parse_number(as.character(VLR..TOTAL.FACT.), 
                              locale = locale(decimal_mark = ",", grouping_mark = ".")),
    productos  = parse_number(as.character(VLR..PRODUCTOS), 
                              locale = locale(decimal_mark = ",", grouping_mark = ".")),
    ID = as.integer(ID)
  ) %>%
  filter(!is.na(ID))  # eliminar filas sin ID

# 6. Preparar listas con correcciones de nombres ---------------------------
listas_clean <- listas %>%
  mutate(
    ID = as.integer(ID),
    # Aplicar las mismas correcciones que en la capa geográfica
    Corregimiento = case_when(
      Corregimiento == "Zelandio"     ~ "Zelandia",
      Corregimiento == "Jiguiales"    ~ "Jiguales",
      Corregimiento == "San Bernando" ~ "San Bernardo",
      Corregimiento == "Borreo Ayerbe" ~ "Borrero Ayerbe",
      TRUE ~ Corregimiento
    )
  )


# 8. Agrupar por corregimiento para obtener totales ------------------------
ventas_por_correg <- db_clean%>%
  group_by(Corregimiento) %>%
  summarise(
    Ventas          = sum(productos, na.rm = TRUE),
    GastoTransporte = sum(transporte, na.rm = TRUE),
    .groups = "drop"
  )

# Mostrar un resumen para verificar que hay datos
print("Totales por corregimiento:")
print(ventas_por_correg)

# 9. Unir con la capa de corregimientos ------------------------------------
# Asegurar que la capa tenga los nombres corregidos
corregimientos <- corregimientos %>%
  mutate(Nombre = case_when(
    Nombre == "Zelandio"     ~ "Zelandia",
    Nombre == "Jiguiales"    ~ "Jiguales",
    Nombre == "San Bernando" ~ "San Bernardo",
    Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
    TRUE                      ~ Nombre
  ))

# Unir (left join) para mantener todos los corregimientos
corregimientos_con_datos <- corregimientos %>%
  left_join(ventas_por_correg, by = c("Nombre" = "Corregimiento"))

# Reemplazar NA por 0 (para que los que no tienen datos aparezcan en un color)
corregimientos_con_datos <- corregimientos_con_datos %>%
  mutate(
    Ventas          = ifelse(is.na(Ventas), 0, Ventas),
    GastoTransporte = ifelse(is.na(GastoTransporte), 0, GastoTransporte)
  )

# 10. Transformar a WGS84 y reparar geometrías -----------------------------
if (st_crs(corregimientos_con_datos) != st_crs(4326)) {
  corregimientos_con_datos <- st_transform(corregimientos_con_datos, 4326)
}
corregimientos_con_datos <- st_make_valid(corregimientos_con_datos)

# 11. Crear paletas de colores continuas -----------------------------------
# Si todos los valores son 0, se usa un dominio artificial (0,1) para evitar errores
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

# 12. Preparar popups ------------------------------------------------------
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

# 13. Construir el mapa ----------------------------------------------------
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
    labelOptions = labelOptions(
      style = list(
        "font-family" = "Segoe UI, Arial",
        "font-size" = "12px",
        "padding" = "6px 12px",
        "border-radius" = "6px",
        "background-color" = "rgba(255,255,255,0.95)"
      ),
      direction = "auto"
    ),
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
    labelOptions = labelOptions(
      style = list(
        "font-family" = "Segoe UI, Arial",
        "font-size" = "12px",
        "padding" = "6px 12px",
        "border-radius" = "6px",
        "background-color" = "rgba(255,255,255,0.95)"
      ),
      direction = "auto"
    ),
    popup = popup_gasto,
    group = "Gasto Transporte"
  ) %>%
  
  # Control de capas
  addLayersControl(
    baseGroups = c("Claro", "Oscuro"),
    overlayGroups = c("Ventas", "Gasto Transporte"),
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
  
  # Ajustar vista (con manejo de errores)
  fitBounds(
    lng1 = min(st_bbox(corregimientos_con_datos)[1], na.rm = TRUE),
    lat1 = min(st_bbox(corregimientos_con_datos)[2], na.rm = TRUE),
    lng2 = max(st_bbox(corregimientos_con_datos)[3], na.rm = TRUE),
    lat2 = max(st_bbox(corregimientos_con_datos)[4], na.rm = TRUE)
  ) %>%
  
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))

# 14. Visualizar el mapa
mapa
