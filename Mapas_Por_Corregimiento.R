# Mapas individuales por corregimiento — Dagua
# Genera un PNG por cada corregimiento con sus ubicaciones de envío
# Requiere: install.packages(c("sf", "ggplot2", "dplyr", "ggspatial", "ggrepel"))

library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(ggrepel)

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

# ── 3. Corregimientos a graficar ─────────────────────────────────────────────
seleccion <- c("Borrero Ayerbe", "El Carmen", "El Limonar", "El Palmar", "San Bernardo")

corr_sel <- corr_utm |> filter(Nombre %in% seleccion)

# ── 4. Detectar columna de nombre en domicilios ──────────────────────────────
posibles <- c("Nombre", "nombre", "NOMBRE", "lugar", "sitio", "direccion", "titular")
col_nom  <- intersect(posibles, names(dom_utm))[1]
if (is.na(col_nom)) col_nom <- names(dom_utm)[1]

# Spatial join: asignar a cada domicilio el corregimiento donde cae
corr_para_join <- corr_sel |> select(corregimiento = Nombre)
dom_utm        <- st_join(dom_utm, corr_para_join, left = TRUE)

dom_coords <- dom_utm |>
  mutate(
    x             = st_coordinates(geometry)[, 1],
    y             = st_coordinates(geometry)[, 2],
    etiq          = .data[[col_nom]],
    corregimiento = if_else(is.na(corregimiento), "Otro", corregimiento)
  ) |>
  st_drop_geometry()

# ── 5. Paleta de colores ─────────────────────────────────────────────────────
paleta <- c(
  "Borrero Ayerbe" = "#0D3349",
  "El Carmen"      = "#1A6B72",
  "El Limonar"     = "#2E9EA8",
  "El Palmar"      = "#5B7FA6",
  "San Bernardo"   = "#7B5EA7"
)

# ── 6. Función para generar un mapa por corregimiento ────────────────────────
mapa_corregimiento <- function(nombre_corr) {

  color_corr <- paleta[[nombre_corr]]

  # Polígono del corregimiento de interés
  corr_focus <- corr_sel |> filter(Nombre == nombre_corr)

  # Domicilios dentro de ese corregimiento
  dom_focus <- dom_coords |> filter(corregimiento == nombre_corr)

  if (nrow(dom_focus) == 0) {
    message("Sin domicilios en: ", nombre_corr)
    return(invisible(NULL))
  }

  p <- ggplot() +

    # Polígono del corregimiento
    geom_sf(
      data      = corr_focus,
      fill      = color_corr,
      color     = "white",
      linewidth = 0.8,
      alpha     = 0.85
    ) +

    # Puntos de envío
    geom_point(
      data   = dom_focus,
      aes(x  = x, y = y),
      shape  = 21,
      size   = 3.5,
      color  = "white",
      fill   = "#F0C040",
      stroke = 0.8,
      alpha  = 0.95
    ) +

    # Etiquetas de cada domicilio
    geom_label_repel(
      data          = dom_focus,
      aes(x = x, y = y, label = etiq),
      size          = 2.8,
      family        = "serif",
      fontface      = "bold",
      color         = "white",
      fill          = color_corr,
      label.size    = 0.3,
      label.r       = unit(0.2, "lines"),
      label.padding = unit(0.2, "lines"),
      box.padding   = unit(0.3, "lines"),
      point.padding = unit(0.2, "lines"),
      segment.color = "white",
      segment.size  = 0.45,
      segment.alpha = 0.85,
      max.overlaps  = Inf,
      force         = 4,
      force_pull    = 0.5,
      seed          = 42
    ) +

    # Escala gráfica
    annotation_scale(
      location   = "bl",
      width_hint = 0.25,
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
      title    = paste0("Corregimiento: ", nombre_corr),
      subtitle = paste0("Municipio de Dagua — ", nrow(dom_focus), " ubicaciones de envío"),
      caption  = "Valle del Cauca, Colombia  ·  Puntos amarillos: lugares de envío registrados"
    ) +

    theme_void(base_family = "serif") +
    theme(
      plot.title    = element_text(
        size = 15, face = "bold", color = "#1B3A5C",
        hjust = 0.5, margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 9, color = "#4A6FA5",
        hjust = 0.5, margin = margin(b = 8)
      ),
      plot.caption  = element_text(
        size = 7.5, color = "#7F8C8D",
        hjust = 0.5, margin = margin(t = 6)
      ),
      plot.background = element_rect(fill = "#F0F4FA", color = NA),
      plot.margin     = margin(16, 16, 12, 16)
    )

  # Guardar PNG
  archivo <- paste0("Mapa_", gsub(" ", "_", nombre_corr), ".png")
  ggsave(
    filename = archivo,
    plot     = p,
    width    = 10,
    height   = 10,
    dpi      = 300,
    bg       = "#F0F4FA"
  )
  message("Generado: ", archivo, "  (", nrow(dom_focus), " puntos)")
  p
}

# ── 7. Generar un mapa por cada corregimiento ─────────────────────────────────
for (corr in seleccion) {
  mapa_corregimiento(corr)
}

message("Listo. Se generaron ", length(seleccion), " mapas PNG.")
