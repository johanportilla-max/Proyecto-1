# ====================================================
# CORREGIMIENTOS DE DAGUA
# 
# ====================================================

# 1. librerías -----------------------------------------------------
library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)
library(htmltools)

# 2. capa de corregimientos (poligonos) ----
corregimientos <- st_read("Corregimientos.gpkg")

# 3. Corregir nombres mal escritos ---------------------------------------
corregimientos <- mutate(corregimientos,
                         Nombre = case_when(
                           Nombre == "Zelandio"     ~ "Zelandia",
                           Nombre == "Jiguiales"    ~ "Jiguales",
                           Nombre == "San Bernando" ~ "San Bernardo",
                           Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
                           TRUE                      ~ Nombre
                         )
)

# 4. Generar tabla de ventas aleatorias ----------
set.seed(010505)  

# Extraer nombres únicos de la capa (ya corregidos)
nombres_correg <- unique(corregimientos$Nombre)

ventas <- data.frame(
  Nombre = nombres_correg,
  ventas_millones = round(runif(length(nombres_correg), min = 500, max = 10000), 0)  
)

# 5. Unir ventas a la capa espacial --------------------------------------
corregimientos_con_ventas <- corregimientos %>%
  left_join(ventas, by = "Nombre")

# 6. Transformar a WGS84  -----------------------
corregimientos_con_ventas <- st_transform(corregimientos_con_ventas, 4326)

# 7. Definir paleta de colores  -------
paleta_color <- colorNumeric(
  palette = c("#2ecc71", "#3498db", "#1f4e79"),  # verde, azul medio, azul oscuro
  domain = corregimientos_con_ventas$ventas_millones,
  na.color = "#cccccc"
)

# 8. Preparar contenido emergente (popup) -------------------------------
popup_content <- paste0(
  "<div style='font-family: \"Segoe UI\", Arial, sans-serif; font-size: 14px; padding: 8px; line-height: 1.4;'>",
  "<b style='font-size: 16px; color: #1f4e79;'>", corregimientos_con_ventas$Nombre, "</b><br>",
  "<span style='color: #2c3e50;'>Ventas: </span>",
  "<b style='color: #27ae60;'>$ ", 
  format(corregimientos_con_ventas$ventas_millones, big.mark = ",", scientific = FALSE), 
  " millones</b>",
  "</div>"
)

# 9. Etiquetas al pasar el mouse (hover) --------------------------------
label_content <- lapply(seq_len(nrow(corregimientos_con_ventas)), function(i) {
  HTML(paste0(
    "<div style='font-family: \"Segoe UI\", Arial, sans-serif;'>",
    "<strong>", corregimientos_con_ventas$Nombre[i], "</strong><br>",
    "Ventas: $", format(corregimientos_con_ventas$ventas_millones[i], big.mark = ",", scientific = FALSE), " M",
    "</div>"
  ))
})

# 10. Construir el mapa interactivo --------------------------------------
mapa_interactivo <- leaflet(data = corregimientos_con_ventas) %>%
  
  # Mapas base elegantes (estilo claro y oscuro)
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Oscuro") %>%
  
  # Capa de polígonos
  addPolygons(
    fillColor = ~paleta_color(ventas_millones),
    fillOpacity = 0.8,
    color = "#FFFFFF",           # borde blanco
    weight = 1.2,
    opacity = 1,
    smoothFactor = 0.3,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#FFD700",          # dorado al resaltar
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = label_content,
    labelOptions = labelOptions(
      style = list(
        "font-family" = "Segoe UI, Arial, sans-serif",
        "font-size" = "12px",
        "padding" = "6px 12px",
        "border-radius" = "6px",
        "background-color" = "rgba(255,255,255,0.95)",
        "box-shadow" = "0 2px 8px rgba(0,0,0,0.15)",
        "border" = "none"
      ),
      textsize = "12px",
      direction = "auto"
    ),
    popup = popup_content,
    group = "Corregimientos"
  ) %>%
  
  # Control de capas (base)
  addLayersControl(
    baseGroups = c("Claro", "Oscuro"),
    overlayGroups = "Corregimientos",
    options = layersControlOptions(collapsed = TRUE)  # colapsado para limpieza
  ) %>%
  
  # Leyenda
  addLegend(
    position = "bottomright",
    pal = paleta_color,
    values = ~ventas_millones,
    title = htmltools::HTML("<span style='font-weight:600;'>Ventas</span><br><span style='font-size:11px;'>millones $</span>"),
    labFormat = labelFormat(prefix = "$ ", suffix = " M", big.mark = ","),
    opacity = 0.9,
    na.label = "Sin datos"
  ) %>%
  
  # Ajustar vista a los datos
  fitBounds(
    lng1 = min(st_bbox(corregimientos_con_ventas)[c(1,3)]),
    lat1 = min(st_bbox(corregimientos_con_ventas)[c(2,4)]),
    lng2 = max(st_bbox(corregimientos_con_ventas)[c(1,3)]),
    lat2 = max(st_bbox(corregimientos_con_ventas)[c(2,4)])
  ) %>%
  
  # Escala
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))

# 11. Mostrar el mapa ----------------------------------------------------
mapa_interactivo
