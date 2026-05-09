# =============================================================================
# MAPAS INDIVIDUALES POR CORREGIMIENTO (solo el polígono + datos)
# =============================================================================
# Cada mapa muestra únicamente el corregimiento seleccionado, con una etiqueta
# que indica las ventas totales y el gasto en transporte.
# =============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)

sf_use_s2(FALSE)

# -----------------------------------------------------------------------------
# 1. Función para formatear montos en millones/miles
# -----------------------------------------------------------------------------
fmt_cop <- function(x) {
  case_when(
    x >= 1e6 ~ paste0("$", formatC(x / 1e6, format = "f", digits = 1), "M"),
    x >= 1e3 ~ paste0("$", formatC(x / 1e3, format = "f", digits = 0), "K"),
    TRUE     ~ paste0("$", formatC(x, format = "f", digits = 0))
  )
}

# -----------------------------------------------------------------------------
# 2. Cargar capa de corregimientos y corregir nombres
# -----------------------------------------------------------------------------
corregimientos <- st_read("Corregimientos.gpkg", quiet = TRUE) %>%
  mutate(Nombre = case_when(
    Nombre == "Zelandio"      ~ "Zelandia",
    Nombre == "Jiguiales"     ~ "Jiguales",
    Nombre == "San Bernando"  ~ "San Bernardo",
    Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
    TRUE                      ~ Nombre
  )) %>%
  st_make_valid() %>%
  st_transform(crs = 4326)  # Mantener coordenadas geográficas (grados)

# -----------------------------------------------------------------------------
# 3. Leer base de datos de ventas (Excel)
# -----------------------------------------------------------------------------
db <- read_excel("Base final.xlsx", sheet = "Base de datos")
names(db) <- make.names(names(db))

db_clean <- db %>%
  mutate(
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

# Totales por corregimiento
ventas_correg <- db_clean %>%
  group_by(Corregimiento) %>%
  summarise(
    Ventas          = sum(ventas, na.rm = TRUE),
    GastoTransporte = sum(transporte, na.rm = TRUE),
    .groups = "drop"
  )

# Unir datos a la capa espacial
corr_datos <- corregimientos %>%
  left_join(ventas_correg, by = c("Nombre" = "Corregimiento")) %>%
  mutate(
    Ventas          = replace(Ventas, is.na(Ventas), 0),
    GastoTransporte = replace(GastoTransporte, is.na(GastoTransporte), 0)
  )

# -----------------------------------------------------------------------------
# 4. Función para generar mapa individual (solo el corregimiento)
# -----------------------------------------------------------------------------
mapa_correg <- function(nombre) {
  
  # Extraer el polígono del corregimiento focal
  focal <- corr_datos %>% filter(Nombre == nombre)
  
  # Calcular un pequeño margen alrededor del polígono (5% de extensión)
  bbox_focal <- st_bbox(focal)
  x_range <- bbox_focal["xmax"] - bbox_focal["xmin"]
  y_range <- bbox_focal["ymax"] - bbox_focal["ymin"]
  xlim <- c(bbox_focal["xmin"] - 0.05 * x_range,
            bbox_focal["xmax"] + 0.05 * x_range)
  ylim <- c(bbox_focal["ymin"] - 0.05 * y_range,
            bbox_focal["ymax"] + 0.05 * y_range)
  
  # Valores formateados
  v_label <- fmt_cop(focal$Ventas)
  t_label <- fmt_cop(focal$GastoTransporte)
  
  # Centroide para colocar la etiqueta interna
  centro <- st_centroid(focal)
  cx <- st_coordinates(centro)[1]
  cy <- st_coordinates(centro)[2]
  
  # Construir mapa
  ggplot() +
    geom_sf(data = focal,
            fill = "#1B3A5C",       # Color azul corporativo
            color = "white",
            linewidth = 0.8) +
    # Etiqueta con los datos
    annotate("label",
             x = cx, y = cy,
             label = paste0(v_label, "\n", t_label),
             size = 4,
             fontface = "bold",
             fill = "white",
             color = "#1B3A5C",
             label.size = 0.5,
             label.padding = unit(0.3, "lines")) +
    # Fijar límites al polígono con margen
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    # Título
    labs(title = nombre) +
    theme_void(base_family = "serif") +
    theme(
      plot.title = element_text(size = 14, face = "bold",
                                color = "#1B3A5C",
                                hjust = 0.5,
                                margin = margin(b = 5)),
      plot.background = element_rect(fill = "#F0F4FA", color = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# -----------------------------------------------------------------------------
# 5. Generar mapas para todos los corregimientos con ventas > 0
# -----------------------------------------------------------------------------
nombres_con_datos <- corr_datos %>% filter(Ventas > 0) %>% pull(Nombre)

# Crear carpeta de salida
dir.create("mapas_individuales_simple", showWarnings = FALSE)

for (nom in nombres_con_datos) {
  cat("Generando mapa para:", nom, "\n")
  p <- mapa_correg(nom)
  
  archivo <- paste0("mapas_individuales_simple/", gsub(" ", "_", nom), ".png")
  ggsave(filename = archivo,
         plot = p,
         width = 6,
         height = 5,
         dpi = 200,
         bg = "#F0F4FA")
}

message("Proceso completado. Los mapas están en la carpeta 'mapas_individuales_simple'.")