# Mapa estático profesional - Municipio de Dagua: Corregimientos
# Requiere: install.packages(c("sf", "ggplot2", "dplyr", "ggspatial"))

library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)

sf_use_s2(FALSE)  # desactiva motor S2 para evitar error con geometrías inválidas

# ── 1. Cargar y corregir datos ───────────────────────────────────────────────
corregimientos <- st_read("Corregimientos.gpkg", quiet = TRUE) |>
  mutate(Nombre = case_when(
    Nombre == "Zelandio"      ~ "Zelandia",
    Nombre == "Jiguiales"     ~ "Jiguales",
    Nombre == "San Bernando"  ~ "San Bernardo",
    Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
    TRUE                      ~ Nombre
  )) |>
  st_transform(crs = 4326)

# ── 2. Proyección UTM automática ─────────────────────────────────────────────
bbox_total  <- st_bbox(corregimientos)
lon_media   <- mean(c(bbox_total["xmin"], bbox_total["xmax"]))
lat_media   <- mean(c(bbox_total["ymin"], bbox_total["ymax"]))
utm_zona    <- floor((lon_media + 180) / 6) + 1
hemisferio  <- ifelse(lat_media >= 0, "north", "south")
epsg_utm    <- ifelse(hemisferio == "north", 32600, 32700) + utm_zona
crs_utm     <- paste0("EPSG:", epsg_utm)

corr_utm    <- st_transform(corregimientos, crs = crs_utm)

# ── 3. Paleta de azules (asignada directamente, sin leyenda) ─────────────────
n            <- nrow(corr_utm)
paleta       <- colorRampPalette(c("#D6E8F7", "#4A90C4", "#1B3A5C"))(n)
corr_utm$color <- paleta[rank(corr_utm$Nombre, ties.method = "first")]

# ── 5. Mapa ───────────────────────────────────────────────────────────────────
mapa <- ggplot() +
  geom_sf(
    data      = corr_utm,
    aes(fill  = color),
    color     = "white",
    linewidth = 0.5
  ) +
  scale_fill_identity() +
  annotation_scale(
    location   = "bl",
    width_hint = 0.25,
    text_col   = "#1B3A5C",
    line_col   = "#1B3A5C",
    text_cex   = 0.75
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style    = north_arrow_fancy_orienteering(
      fill      = c("white", "#1B3A5C"),
      line_col  = "#1B3A5C",
      text_col  = "#1B3A5C"
    ),
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm")
  ) +
  labs(
    title    = "Municipio de Dagua",
    subtitle = "División por Corregimientos — Valle del Cauca, Colombia"
  ) +
  theme_void(base_family = "serif") +
  theme(
    legend.position   = "none",
    plot.title        = element_text(
      size   = 16, face = "bold",
      color  = "#1B3A5C", hjust = 0.5,
      margin = margin(b = 4)
    ),
    plot.subtitle     = element_text(
      size   = 10, color = "#4A6FA5",
      hjust  = 0.5,
      margin = margin(b = 10)
    ),
    plot.background   = element_rect(fill = "#F0F4FA", color = NA),
    plot.margin       = margin(16, 16, 16, 16)
  )

# ── 6. Mostrar y exportar ────────────────────────────────────────────────────
print(mapa)

ggsave(
  filename = "Mapa_Dagua_Corregimientos.png",
  plot     = mapa,
  width    = 10,
  height   = 12,
  dpi      = 300,
  bg       = "#F0F4FA"
)

message("Mapa generado: Mapa_Dagua_Corregimientos.png")