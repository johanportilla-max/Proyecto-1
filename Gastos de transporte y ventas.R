# ====================================================
# MAPA DE CORREGIMIENTOS DE DAGUA — ggplot2
# Un panel por corregimiento con ventas y gasto de transporte
# ====================================================

# 1. Librerías --------------------------------------------------------
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(patchwork)

# Formato en millones / miles
fmt_cop <- function(x) {
  dplyr::case_when(
    x >= 1e6 ~ paste0("$", formatC(x / 1e6, format = "f", digits = 1), "M"),
    x >= 1e3 ~ paste0("$", formatC(x / 1e3, format = "f", digits = 0), "K"),
    TRUE      ~ paste0("$", formatC(x, format = "f", digits = 0))
  )
}

# 2. Capa geográfica --------------------------------------------------
todos <- st_read("Corregimientos.gpkg", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  mutate(Nombre = case_when(
    Nombre == "Zelandio"      ~ "Zelandia",
    Nombre == "Jiguiales"     ~ "Jiguales",
    Nombre == "San Bernando"  ~ "San Bernardo",
    Nombre == "Borreo Ayerbe" ~ "Borrero Ayerbe",
    TRUE                       ~ Nombre
  ))

# 3. Leer y limpiar Base de datos del Excel ---------------------------
db <- read_excel("ventas.xlsx", sheet = "Base de datos")
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

# 4. Totales por corregimiento ----------------------------------------
ventas_correg <- db_clean %>%
  group_by(Corregimiento) %>%
  summarise(
    Ventas          = sum(ventas,     na.rm = TRUE),
    GastoTransporte = sum(transporte, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Unir espacial + datos --------------------------------------------
corr_datos <- todos %>%
  left_join(ventas_correg, by = c("Nombre" = "Corregimiento")) %>%
  mutate(
    Ventas          = replace(Ventas,          is.na(Ventas),          0),
    GastoTransporte = replace(GastoTransporte, is.na(GastoTransporte), 0)
  )

# Sólo los que tienen ventas (para iterar)
con_datos <- corr_datos %>% filter(Ventas > 0)

# Límites globales del municipio (para que todos los paneles tengan el mismo zoom)
bbox_global <- st_bbox(todos)

# 6. Función: un mapa por corregimiento --------------------------------
mapa_correg <- function(nombre) {

  focal   <- corr_datos %>% filter(Nombre == nombre)
  fondo   <- corr_datos %>% filter(Nombre != nombre)
  v_label <- fmt_cop(focal$Ventas)
  t_label <- fmt_cop(focal$GastoTransporte)

  # Centroide del polígono para la etiqueta interna
  centro  <- suppressWarnings(st_point_on_surface(focal))
  cx      <- st_coordinates(centro)[1]
  cy      <- st_coordinates(centro)[2]

  ggplot() +
    # Todos los demás corregimientos en gris
    geom_sf(data = fondo,
            fill = "#DCDCDC", color = "white", linewidth = 0.35) +
    # Corregimiento destacado
    geom_sf(data = focal,
            fill = "#C0392B", color = "white", linewidth = 0.7) +
    # Etiqueta dentro del polígono
    annotate("label",
             x = cx, y = cy,
             label = paste0(v_label, "\n", t_label),
             size = 2.4, fontface = "bold",
             fill = "white", color = "#222222",
             label.size = 0.25, label.padding = unit(0.18, "lines"),
             alpha = 0.9) +
    # Fijar zoom al municipio completo en todos los paneles
    coord_sf(
      xlim = c(bbox_global["xmin"], bbox_global["xmax"]),
      ylim = c(bbox_global["ymin"], bbox_global["ymax"]),
      expand = FALSE
    ) +
    labs(
      title    = nombre,
      subtitle = paste0("\u25b2 Ventas: ", v_label,
                        "   \u1f69a Transp: ", t_label)
    ) +
    theme_void(base_size = 8) +
    theme(
      plot.title    = element_text(face = "bold", size = 8.5,
                                   color = "#1f4e79", hjust = 0.5,
                                   margin = margin(b = 1)),
      plot.subtitle = element_text(size = 6.8, color = "#555555",
                                   hjust = 0.5,
                                   margin = margin(b = 3)),
      plot.background = element_rect(fill = "white",
                                     color = "#CCCCCC", linewidth = 0.4),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# 7. Generar un plot por corregimiento --------------------------------
nombres_con_datos <- con_datos$Nombre

lista_mapas <- lapply(nombres_con_datos, mapa_correg)
names(lista_mapas) <- nombres_con_datos

# 8. Combinar con patchwork en cuadrícula -----------------------------
n      <- length(lista_mapas)
n_cols <- 3L
n_rows <- ceiling(n / n_cols)

mapa_combinado <- wrap_plots(lista_mapas, ncol = n_cols) +
  plot_annotation(
    title    = "Dagua — Ventas y Gasto de Transporte por Corregimiento",
    subtitle = paste0("Corregimientos con ventas registradas (n = ", n, ")  |  ",
                      "\u25b2 = Ventas totales de productos   ",
                      "\u1f69a = Gasto en transporte"),
    caption  = "Fuente: Base de datos ventas.xlsx",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14,
                                   color = "#1f4e79", hjust = 0.5),
      plot.subtitle = element_text(size = 9, color = "#555555", hjust = 0.5),
      plot.caption  = element_text(size = 7.5, color = "#888888", hjust = 1),
      plot.background = element_rect(fill = "#F7F7F7", color = NA)
    )
  )

# 9. Mostrar ----------------------------------------------------------
print(mapa_combinado)

# Guardar (opcional — descomenta para exportar)
# ggsave("mapa_corregimientos_dagua.png",
#        plot   = mapa_combinado,
#        width  = n_cols * 4,
#        height = n_rows * 4,
#        dpi    = 180,
#        bg     = "#F7F7F7")
