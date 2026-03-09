# Mapa estático - Dagua: Corregimientos + Domicilios
# Requiere: install.packages(c("sf", "ggplot2", "dplyr", "ggspatial"))

library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)

sf_use_s2(FALSE)

# ── 1. Cargar y corregir datos ───────────────────────────────────────────────
corregimientos <- st_read("Corregimientos.gpkg", quiet = TRUE) |>
  mutate(Nombre = case_when(
    Nombre == "Zelandio"      ~ "Zelandia",
    Nombre == "Jiguiales"     ~ "Jiguales",
    Nombre == "San Bernando"  ~ "San Bernardo",
    Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
    TRUE                      ~ Nombre
  )) |>
  st_make_valid() |>
  st_transform(crs = 4326)

domicilios <- st_read("Domicilios.shp", quiet = TRUE) |>
  st_make_valid() |>
  st_transform(crs = 4326)

# ── 2. Proyección UTM automática ─────────────────────────────────────────────
bbox_total <- st_bbox(corregimientos)
lon_media  <- mean(c(bbox_total["xmin"], bbox_total["xmax"]))
lat_media  <- mean(c(bbox_total["ymin"], bbox_total["ymax"]))
utm_zona   <- floor((lon_media + 180) / 6) + 1
epsg_utm   <- ifelse(lat_media >= 0, 32600, 32700) + utm_zona
crs_utm    <- paste0("EPSG:", epsg_utm)

corr_utm <- st_transform(corregimientos, crs = crs_utm)
dom_utm  <- st_transform(domicilios,     crs = crs_utm)

# ── 3. Contar domicilios por corregimiento ───────────────────────────────────
# st_within evita conflictos de nombres y trabaja en CRS métrico (UTM)
corr_utm$n_domicilios <- lengths(st_contains(corr_utm, dom_utm))

# ── 4. Centroides para etiquetas (solo top 5) ────────────────────────────────
top5 <- c("Borrero Ayerbe", "El Carmen", "El Palmar", "El Limonar", "San Bernardo")

centroides_top5 <- corr_utm |>
  filter(Nombre %in% top5) |>
  st_centroid() |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry()

# ── 5. Mapa ───────────────────────────────────────────────────────────────────
mapa <- ggplot() +

  # Corregimientos coloreados por frecuencia de domicilios
  geom_sf(
    data      = corr_utm,
    aes(fill  = n_domicilios),
    color     = "white",
    linewidth = 0.5
  ) +
  scale_fill_gradient(
    low    = "#D6E8F7",
    high   = "#1B3A5C",
    name   = "N° domicilios",
    breaks = scales::pretty_breaks(n = 5),
    guide  = guide_colorbar(
      title.position = "top",
      barwidth       = unit(0.4, "cm"),
      barheight      = unit(3,   "cm"),
      ticks.colour   = "white"
    )
  ) +

  # Puntos de domicilios
  geom_sf(
    data  = dom_utm,
    color = "#C0392B",
    fill  = "#E74C3C",
    shape = 21,
    size  = 1.8,
    stroke = 0.4,
    alpha = 0.85
  ) +

  # Etiquetas para los 5 corregimientos más frecuentes
  geom_label(
    data  = centroides_top5,
    aes(x = x, y = y, label = Nombre),
    size            = 2.6,
    family          = "serif",
    fontface        = "bold",
    color           = "#1B3A5C",
    fill            = "white",
    label.size      = 0.3,
    label.r         = unit(0.15, "lines"),
    label.padding   = unit(0.18, "lines"),
    alpha           = 0.88
  ) +

  # Escala gráfica
  annotation_scale(
    location   = "bl",
    width_hint = 0.22,
    text_col   = "#1B3A5C",
    line_col   = "#1B3A5C",
    text_cex   = 0.75
  ) +

  # Rosa de los vientos
  annotation_north_arrow(
    location    = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill     = c("white", "#1B3A5C"),
      line_col = "#1B3A5C",
      text_col = "#1B3A5C"
    ),
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm")
  ) +

  labs(
    title    = "Municipio de Dagua — Domicilios por Corregimiento",
    subtitle = "Valle del Cauca, Colombia  ·  Puntos rojos: ubicaciones registradas",
    caption  = "Corregimientos con mayor concentración etiquetados"
  ) +

  theme_void(base_family = "serif") +
  theme(
    legend.position   = c(0.88, 0.35),
    legend.background = element_rect(fill = "white", color = "#B0C4DE", linewidth = 0.4),
    legend.margin     = margin(6, 8, 6, 8),
    legend.title      = element_text(size = 8, color = "#1B3A5C", face = "bold"),
    legend.text       = element_text(size = 7.5, color = "#1B3A5C"),
    plot.title        = element_text(
      size = 15, face = "bold", color = "#1B3A5C",
      hjust = 0.5, margin = margin(b = 4)
    ),
    plot.subtitle     = element_text(
      size = 9, color = "#4A6FA5",
      hjust = 0.5, margin = margin(b = 8)
    ),
    plot.caption      = element_text(
      size = 7.5, color = "#7F8C8D",
      hjust = 0.5, margin = margin(t = 6)
    ),
    plot.background   = element_rect(fill = "#F0F4FA", color = NA),
    plot.margin       = margin(16, 16, 12, 16)
  )

# ── 6. Mostrar y exportar ────────────────────────────────────────────────────
print(mapa)

ggsave(
  filename = "Mapa_Dagua_Con_Ubicaciones.png",
  plot     = mapa,
  width    = 10,
  height   = 12,
  dpi      = 300,
  bg       = "#F0F4FA"
)

message("Mapa generado: Mapa_Dagua_Con_Ubicaciones.png")
