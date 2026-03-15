# ============================================================
# TABLA – UBICACIONES Y DISTANCIAS (FERRETERÍA / DOMICILIOS)
# Requiere: install.packages(c("gt", "dplyr"))
# ============================================================

library(gt)
library(dplyr)

# ── 1. Vectores base (se construyen primero, tipo se deriva automáticamente) ──

nombres_lugares <- c(
  "Agua Sucia",              "Altos de las T\u00f3rtolas", "Altos del Carmen",
  "Azul y verde",            "Bahondo",                    "Campamento Salom\u00f3n",
  "Carmen",                  "Chipre",                     "Cositas ricas",
  "El Carmelo",              "El Coscorr\u00f3n",          "El diviso",
  "El limonar",              "El palmar",                  "El parque",
  "El Queremal",             "Finca la cristalina",        "KM 24",
  "KM 25",                   "KM 26",                      "KM 27",
  "KM 28",                   "KM 34",                      "KM 37",
  "KM 40",                   "La bomba",                   "La clorinda",
  "La recta",                "La Zulia 1",                 "La Zulia 2",
  "La Zulia 3",              "Las villas del carmen",      "Los bomberos",
  "Mirador Bellavista",      "MK 26",                      "Monteredondo",
  "Pa la chingada",          "Palo alto",                  "Parcelaci\u00f3n El bosque",
  "Parcelacion la Cristalina","Parcelaci\u00f3n Monterico","Parcelaon las palmas",
  "Pirineos",                "San Fernando",               "San Jose del Salado",
  "San Miguel",              "Tierras blancas",            "Tocota",
  "T\u00f3rtolas",           "Salida al carmen",           "Parcelaci\u00f3n Ambichinte",
  "Villa del toro",          "La florida",                 "Casa verde",
  "La caba\u00f1a",          "Villa emilia",               "El chiringuito",
  "Las playas",              "El chilcal",                 "Panamericano",
  "Villa saman",             "Saliendo del carmen",        "Villahermosa"
)

distancias_lugares <- c(
  1.343,     6.724,     5.464,
  2.31,      5.6,       3.467,
  4.201,     4.781,     0.723,
  6.124,     2.65,      8.471,
  4.713,     4.93,      0.836,
  18.542,    6.071,     6.973,
  6.46658,   5.378,     4.583943,
  3.943014,  4.316,     6.606969,
  9.607613,  1.321207,  5.264,
  0.935,     0.944,     1.248,
  1.087,     3.507,     2.092,
  1.201,     5.378,     0.428,
  1.31,      4.476,     4.800039,
  2.71,      9.826848,  2.641,
  2.66665,   7.603318,  13.07,
  2.269,     0.449,     9.438,
  5.757,     2.329,     3.684,
  2.279762,  1.466927,  4.626,
  2.091,     3.449,     1.544,
  2.129,     14.314,    1.127,
  2.059,     4.734,     6.788
)

# Verificación automática
stopifnot(
  "Nombres y distancias no coinciden" = length(nombres_lugares) == length(distancias_lugares)
)

n_lugares <- length(nombres_lugares)   # 63

# ── 2. Data frame final ───────────────────────────────────────────────────────
ubicaciones <- data.frame(
  tipo      = c(rep("lugar", n_lugares), "promedio"),
  Nombre    = c(nombres_lugares, "Promedio"),
  Distancia = c(distancias_lugares, 4.41060111),
  stringsAsFactors = FALSE
)

idx_lugar    <- which(ubicaciones$tipo == "lugar")
idx_promedio <- which(ubicaciones$tipo == "promedio")

# ── 3. Construir tabla gt ─────────────────────────────────────────────────────
tabla <- ubicaciones |>
  select(-tipo) |>
  gt() |>

  tab_header(
    title    = md("**Ubicaciones y Distancias**"),
    subtitle = md("*Distancia promedio por destino de entrega (km)*")
  ) |>

  cols_label(
    Nombre    = md("**Nombre**"),
    Distancia = md("**Distancia (km)**")
  ) |>

  fmt_number(
    columns  = Distancia,
    rows     = idx_lugar,
    decimals = 3,
    dec_mark = ",",
    sep_mark = "."
  ) |>
  fmt_number(
    columns  = Distancia,
    rows     = idx_promedio,
    decimals = 5,
    dec_mark = ",",
    sep_mark = "."
  ) |>

  cols_align(align = "left",  columns = Nombre) |>
  cols_align(align = "right", columns = Distancia) |>

  cols_width(
    Nombre    ~ px(280),
    Distancia ~ px(160)
  ) |>

  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(14))
    ),
    locations = cells_column_labels()
  ) |>

  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = idx_lugar[seq(1, n_lugares, by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_lugar[seq(2, n_lugares, by = 2)])
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C", weight = "bold"),
    locations = cells_body(columns = Nombre, rows = idx_lugar)
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C"),
    locations = cells_body(columns = Distancia, rows = idx_lugar)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#B0C4DE", weight = px(1)),
    locations = cells_body(rows = idx_lugar)
  ) |>

  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(15))
    ),
    locations = cells_body(rows = idx_promedio)
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#7EC8E3", weight = px(2)),
    locations = cells_body(rows = idx_promedio)
  ) |>

  tab_style(
    style = cell_text(size = px(13), font = "Times New Roman"),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(font = "Times New Roman"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(font = "Times New Roman"),
    locations = cells_title()
  ) |>

  tab_source_note(
    source_note = md(
      "*Fuente: Base de datos de entregas \u2014 Distancias calculadas desde el punto de despacho*"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(1)),
    locations = cells_source_notes()
  ) |>

  tab_options(
    table.width                    = px(460),
    table.border.top.color         = "#1B3A5C",
    table.border.top.width         = px(3),
    table.border.bottom.color      = "#1B3A5C",
    table.border.bottom.width      = px(2),
    heading.background.color       = "#F0F4FA",
    heading.border.bottom.color    = "#1B3A5C",
    heading.border.bottom.width    = px(2),
    column_labels.border.top.color = "#1B3A5C",
    column_labels.border.top.width = px(2),
    data_row.padding               = px(7),
    source_notes.font.size         = px(12)
  )

# ── 4. Mostrar y exportar ────────────────────────────────────────────────────
tabla

gtsave(tabla, "tabla_ubicaciones_distancias.html")
# gtsave(tabla, "tabla_ubicaciones_distancias.png")   # requiere webshot2

message("Tabla generada: tabla_ubicaciones_distancias.html  |  n_lugares = ", n_lugares)
