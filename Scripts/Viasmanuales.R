# ====================================================
# Vias del Corregimiento de Dagua y ubicaciones de envio
# 
# ====================================================

# 1. librerías -----------------------------------------------------
library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)
library(htmltools)
library(sf)
library(ggplot2)
library(leaflet)
library(leaflet.extras2) 
library(viridis)      
library(ggspatial)
library(tmap)

# Bases de datos

FCOM <- st_read("Ferreterias competencia.shp")
VIAS <- st_read("Manuales.shp")
UBICACIONES <- st_read("Domicilios.shp")

asignar_crs_si_na <- function(sf_obj, default_crs = 4326) {
  if (is.na(st_crs(sf_obj))) {
    message("Asignando CRS por defecto (EPSG:", default_crs, ") a una capa")
    st_crs(sf_obj) <- default_crs
  }
  return(sf_obj)
}

# Asegurar que todas las capas tengan CRS (asumimos WGS84 si no lo tienen)
FCOM <- asignar_crs_si_na(FCOM)
VIAS <- asignar_crs_si_na(VIAS)
UBICACIONES <- asignar_crs_si_na(UBICACIONES)


all_geom <- c(st_geometry(FCOM), st_geometry(VIAS), st_geometry(UBICACIONES))
centro <- st_centroid(st_union(all_geom))
coords_centro <- st_coordinates(centro)

# Calcular zona UTM a partir de la longitud media
long_media <- coords_centro[1, "X"]
utm_zone <- floor((long_media + 180) / 6) + 1
lat_media <- coords_centro[1, "Y"]
hemisferio <- ifelse(lat_media >= 0, "north", "south")
crs_utm <- paste0("EPSG:", ifelse(hemisferio == "north", 32600, 32700) + utm_zone)
message("Usando CRS proyectado: ", crs_utm)

# Transformar todas las capas al CRS UTM
FCOM_proj <- st_transform(FCOM, crs = crs_utm)
VIAS_proj <- st_transform(VIAS, crs = crs_utm)
UBICACIONES_proj <- st_transform(UBICACIONES, crs = crs_utm)

# Calcular el bounding box combinado (para enfocar la vista)
bbox_proj <- st_bbox(c(st_bbox(FCOM_proj), st_bbox(VIAS_proj), st_bbox(UBICACIONES_proj)))

# Crear el mapa con estilo profesional
mapa <- ggplot() +
  # Capa de vías (líneas) - gris suave
  geom_sf(data = VIAS_proj, color = "gray40", size = 0.5, alpha = 0.7) +
  
  # Capa de domicilios (puntos) - azul semitransparente
  geom_sf(data = UBICACIONES_proj, color = "steelblue", size = 2, alpha = 0.6) +
  
  # Capa de ferreterías competencia (puntos) - rojo destacado (triángulos)
  geom_sf(data = FCOM_proj, color = "firebrick", size = 3, shape = 17, alpha = 0.8) +
  
  # Coordenadas delimitadas al área de interés
  coord_sf(
    xlim = c(bbox_proj["xmin"], bbox_proj["xmax"]),
    ylim = c(bbox_proj["ymin"], bbox_proj["ymax"]),
    expand = FALSE,
    crs = st_crs(crs_utm)  # explícitamente el CRS proyectado
  ) +
  
  # Etiquetas
  labs(
    title = "Mapa de ubicaciones y competencia",
    subtitle = "Vías, domicilios y ferreterías de la competencia",
    caption = "Fuente: datos propios",
    x = "Easting (m)", y = "Northing (m)"
  ) +
  
  # Tema minimalista con fondo blanco y detalles elegantes
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 11),
    plot.caption = element_text(color = "gray50", size = 8),
    legend.position = "none",  # si no usas aes(color), no aparece leyenda
    axis.text = element_text(color = "gray50"),
    axis.ticks = element_line(color = "gray70"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  
  # Barra de escala (esquina inferior izquierda)
  annotation_scale(location = "bl", width_hint = 0.2, 
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.3, "cm")) +
  
  # Flecha norte (esquina superior derecha)
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering(),
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"))

# Mostrar el mapa
print(mapa)
