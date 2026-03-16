# ============================================================
# TABLA – PUNTOS DE ENTREGA: NOMBRE / ENVÍOS / DISTANCIA
# Requiere: install.packages(c("gt", "dplyr"))
# ============================================================

library(gt)
library(dplyr)

# ── 1. Datos ─────────────────────────────────────────────────────────────────
puntos <- data.frame(
  tipo = c(rep("data", 63), "promedio"),
  Nombre = c(
    "El Coscorron", "Altos del Carmen", "Pirineos", "KM 34",
    "San Jose del Salado", "San Miguel", "Tortolas", "Casa verde",
    "El Carmelo", "El diviso", "Los bomberos", "Villa del toro",
    "Carmen", "El parque", "La Zulia 2", "Las villas del carmen",
    "Monteredondo", "Palo alto", "Parcelacion la Cristalina",
    "Parcelaon las palmas", "Tocota", "Salida al carmen", "La florida",
    "Agua Sucia", "Altos de las Tortolas", "El limonar", "El Queremal",
    "KM 26", "KM 28", "La clorinda", "La Zulia 3",
    "Parcelaci\u00f3n El bosque",
    "Bahondo", "Chipre", "El palmar", "KM 24", "KM 27",
    "La bomba", "La Zulia 1", "San Fernando", "Tierras blancas",
    "Villa saman", "Saliendo del carmen",
    "Azul y verde", "Cositas ricas", "KM 25", "KM 40",
    "La recta", "Mirador Bellavista", "Parcelaci\u00f3n Monterico",
    "El chilcal", "Panamericano", "Villahermosa",
    "KM 37", "Pa la chingada", "Parcelaci\u00f3n Ambichinte",
    "La caba\u00f1a", "Villa emilia", "El chiringuito", "Las playas",
    "Campamento Salomon", "Finca la cristalina", "MK 26",
    NA   # fila promedio
  ),
  Num_envios = c(
    11, 9, 9, 8,
    7, 7, 7, 7,
    6, 6, 6, 6,
    5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5,
    4, 4, 4, 4, 4, 4, 4, 4, 4,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    1, 1, 1, 1, 1, 1, 1,
    0, 0, 0,
    NA   # fila promedio
  ),
  Distancia = c(
    2.65,       5.464,     2.66665,   4.316,
    13.07,      2.269,     5.757,     4.626,
    6.124,      8.471,     2.092,     2.279762,
    4.201,      0.836,     1.248,     3.507,
    0.428,      4.476,     2.71,      2.641,     9.438,  2.329,  1.466927,
    1.343,      6.724,     4.713,    18.542,     5.378,  3.943014, 5.264, 1.087, 4.800039,
    5.6,        4.781,     4.93,      6.973,     4.583943, 1.321207, 0.944, 7.603318, 0.449, 2.059, 4.734,
    2.31,       0.723,     6.46658,   9.607613,  0.935,  1.201,  9.826848, 14.314, 1.127, 6.788,
    6.606969,   1.31,      3.684,     2.091,     3.449,  1.544,  2.129,
    3.467,      6.071,     5.378,
    4.41060111  # promedio
  ),
  stringsAsFactors = FALSE
)

idx_data <- which(puntos$tipo == "data")
idx_prom <- which(puntos$tipo == "promedio")

# ── 2. Tabla gt ───────────────────────────────────────────────────────────────
tabla <- puntos |>
  select(-tipo) |>
  gt() |>

  cols_label(
    Nombre     = md("**Nombre**"),
    Num_envios = md("**Numero de envios**"),
    Distancia  = md("**Distancia**")
  ) |>

  # Enteros para envíos (sólo filas data)
  fmt_integer(
    columns  = Num_envios,
    rows     = idx_data,
    sep_mark = "."
  ) |>

  # Distancia: decimales variables sin trailing zeros
  fmt_number(
    columns            = Distancia,
    decimals           = 7,
    drop_trailing_zeros = TRUE,
    dec_mark           = ",",
    sep_mark           = "."
  ) |>

  # Celdas vacías en la fila de promedio
  sub_missing(
    columns      = c(Nombre, Num_envios),
    rows         = idx_prom,
    missing_text = ""
  ) |>

  # ── Alineación ──────────────────────────────────────────────────────────────
  cols_align(align = "left",   columns = Nombre) |>
  cols_align(align = "center", columns = Num_envios) |>
  cols_align(align = "right",  columns = Distancia) |>

  # ── Anchos ──────────────────────────────────────────────────────────────────
  cols_width(
    Nombre     ~ px(250),
    Num_envios ~ px(145),
    Distancia  ~ px(115)
  ) |>

  # ── Encabezados de columna ──────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(13),
                font = "Times New Roman")
    ),
    locations = cells_column_labels()
  ) |>

  # ── Filas data: alternadas ───────────────────────────────────────────────────
  tab_style(
    style = cell_fill(color = "#D6E8F7"),
    locations = cells_body(rows = idx_data[seq(1, length(idx_data), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_data[seq(2, length(idx_data), by = 2)])
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C", font = "Times New Roman", size = px(12)),
    locations = cells_body(rows = idx_data)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Num_envios, rows = idx_data)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#B8D4E8", weight = px(1)),
    locations = cells_body(rows = idx_data)
  ) |>

  # ── Fila promedio ────────────────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF"),
      cell_text(color = "#1B3A5C", weight = "bold", size = px(12),
                font = "Times New Roman")
    ),
    locations = cells_body(rows = idx_prom)
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#1B3A5C", weight = px(2)),
    locations = cells_body(rows = idx_prom)
  ) |>

  # ── Opciones generales ──────────────────────────────────────────────────────
  tab_options(
    table.width                       = px(520),
    table.border.top.color            = "#1B3A5C",
    table.border.top.width            = px(3),
    table.border.bottom.color         = "#1B3A5C",
    table.border.bottom.width         = px(2),
    column_labels.border.top.color    = "#1B3A5C",
    column_labels.border.top.width    = px(2),
    column_labels.border.bottom.color = "#7EC8E3",
    column_labels.border.bottom.width = px(2),
    data_row.padding                  = px(6)
  )

# ── 3. Mostrar y exportar ─────────────────────────────────────────────────────
tabla

gtsave(tabla, "tabla_puntos_entrega.html")
# gtsave(tabla, "tabla_puntos_entrega.png")   # requiere webshot2

message("Tabla generada: tabla_puntos_entrega.html")
