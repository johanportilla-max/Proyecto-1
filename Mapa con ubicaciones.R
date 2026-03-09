# Mapa estático - Dagua: Corregimientos seleccionados + Ubicaciones de envío
# Requiere: install.packages(c("sf", "ggplot2", "dplyr", "ggspatial", "ggrepel"))

library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(ggrepel)
library(ggnewscale)

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
  st_transform(crs = 4326) %>% filter(Nombre != c("Patrones"))

# ── 2. Proyección UTM automática ─────────────────────────────────────────────
bbox_total <- st_bbox(corregimientos)
lon_media  <- mean(c(bbox_total["xmin"], bbox_total["xmax"]))
lat_media  <- mean(c(bbox_total["ymin"], bbox_total["ymax"]))
utm_zona   <- floor((lon_media + 180) / 6) + 1
epsg_utm   <- ifelse(lat_media >= 0, 32600, 32700) + utm_zona
crs_utm    <- paste0("EPSG:", epsg_utm)

corr_utm <- st_transform(corregimientos, crs = crs_utm)
dom_utm  <- st_transform(domicilios,     crs = crs_utm)

# ── 3. Filtrar solo los 4 corregimientos pedidos ─────────────────────────────
seleccion <- c("Borrero Ayerbe", "El Carmen", "El Limonar", "El Palmar", "San Bernardo")

corr_sel  <- corr_utm |> filter(Nombre %in% seleccion)

# ── 4. Detectar columna de nombre en domicilios ──────────────────────────────
posibles <- c("Nombre", "nombre", "NOMBRE", "lugar", "sitio", "direccion", "titular")
col_nom   <- intersect(posibles, names(dom_utm))[1]
if (is.na(col_nom)) col_nom <- names(dom_utm)[1]

# Spatial join: asignar a cada domicilio el corregimiento donde cae
corr_para_join <- corr_sel |> select(corregimiento = Nombre)
dom_utm <- st_join(dom_utm, corr_para_join, left = TRUE)

# Coordenadas para ggrepel
dom_coords <- dom_utm |>
  mutate(
    x             = st_coordinates(geometry)[, 1],
    y             = st_coordinates(geometry)[, 2],
    etiq          = .data[[col_nom]],
    corregimiento = if_else(is.na(corregimiento), "Otro", corregimiento)
  ) |>
  st_drop_geometry()

# ── 5. Paleta de 4 azules para corregimientos seleccionados ──────────────────
paleta_sel <- c(
  "Borrero Ayerbe" = "#0D3349",   # azul marino profundo
  "El Carmen"      = "#1A6B72",   # teal oscuro
  "El Limonar"     = "#2E9EA8",   # cian medio
  "El Palmar"      = "#5B7FA6",   # azul grisáceo
  "San Bernardo"   = "#7B5EA7"    # violeta frío
)

# Paleta para el fondo de las etiquetas de domicilios
paleta_etiq <- c(
  "Borrero Ayerbe" = "#0D3349",
  "El Carmen"      = "#1A6B72",
  "El Limonar"     = "#2E9EA8",
  "El Palmar"      = "#5B7FA6",
  "San Bernardo"   = "#7B5EA7",
  "Otro"           = "#7A8FA6"
)

# ── 6. Mapa ───────────────────────────────────────────────────────────────────
mapa <- ggplot() +
  
  # Corregimientos seleccionados — paleta de azules
  geom_sf(
    data      = corr_sel,
    aes(fill  = Nombre),
    color     = "white",
    linewidth = 0.7
  ) +
  scale_fill_manual(
    values = paleta_sel,
    name   = "Corregimiento",
    guide  = guide_legend(
      title.position = "top",
      keywidth       = unit(0.7, "cm"),
      keyheight      = unit(0.55, "cm"),
      override.aes   = list(color = "white", linewidth = 0.4)
    )
  ) +
  
  # Etiquetas de los corregimientos seleccionados
  geom_sf_text(
    data     = corr_sel,
    aes(label = Nombre),
    size     = 3,
    family   = "serif",
    fontface = "bold.italic",
    color    = "white"
  ) +
  
  # Segunda escala de fill — para etiquetas de domicilios
  new_scale_fill() +
  
  # Puntos de envío
  geom_point(
    data  = dom_coords,
    aes(x = x, y = y),
    shape  = 21,
    size   = 3,
    color  = "white",
    fill   = "#F0C040",
    stroke = 0.7,
    alpha  = 0.95
  ) +
  
  # Etiquetas coloreadas según el corregimiento donde cae cada punto
  geom_label_repel(
    data             = dom_coords,
    aes(x = x, y = y, label = etiq, fill = corregimiento),
    size             = 2.5,
    family           = "serif",
    fontface         = "bold",
    color            = "white",
    label.size       = 0.3,
    label.r          = unit(0.2, "lines"),
    label.padding    = unit(0.22, "lines"),
    box.padding      = unit(0.4, "lines"),
    point.padding    = unit(0.3, "lines"),
    segment.color    = "white",
    segment.size     = 0.4,
    segment.alpha    = 0.8,
    max.overlaps     = 20,
    seed             = 42
  ) +
  scale_fill_manual(
    values = paleta_etiq,
    name   = "Ubicación en",
    guide  = guide_legend(
      title.position = "top",
      keywidth       = unit(0.7, "cm"),
      keyheight      = unit(0.45, "cm")
    )
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
    title    = "Municipio de Dagua — Ubicaciones de Envío",
    subtitle = "Corregimientos: Borrero Ayerbe · El Carmen · El Limonar · El Palmar · San Bernardo",
    caption  = "Valle del Cauca, Colombia  ·  Puntos amarillos: lugares de envío registrados"
  ) +
  
  theme_void(base_family = "serif") +
  theme(
    legend.position   = c(0.15, 0.2),
    legend.background = element_rect(fill = "white", color = "#B0C4DE", linewidth = 0.4),
    legend.margin     = margin(6, 10, 6, 10),
    legend.title      = element_text(size = 8.5, color = "#1B3A5C", face = "bold"),
    legend.text       = element_text(size = 8, color = "#1B3A5C"),
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

# ── 7. Mostrar y exportar ────────────────────────────────────────────────────
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