# Tabla profesional - Estado de Resultados
# Requiere: install.packages(c("gt", "dplyr"))

library(gt)
library(dplyr)

# ── Datos ─────────────────────────────────────────────────────────────────────
estado_resultados <- data.frame(
  Concepto = c(
    "Ingresos operacionales",
    "(-) Costos de ventas: MP MOD CIF",
    "Utilidad Bruta",
    "(-) Depreciación y amortización",
    "(-) Gastos administrativos",
    "(-) Gastos de venta",
    "Utilidad operacional (EBIT)",
    "(+) Ingresos no operacionales",
    "(-) Gastos financieros",
    "(-) Gastos no declarados",
    "Utilidad antes de impuestos (EBT)",
    "(-) Impuesto de renta",
    "Utilidad Neta"
  ),
  Valor = c(
    "$ 650,000,000.00",
    "$ 540,864,903.00",
    "$ 109,135,097.00",
    "",
    "$  24,303,000.00",
    "$  64,872,000.00",
    "$  19,960,097.00",
    "",
    "$   5,920,000.00",
    "$   3,593,000.00",
    "$  17,633,097.00",
    "$   6,171,583.95",
    "$  11,461,513.05"
  ),
  stringsAsFactors = FALSE
)

# Índices de filas especiales
filas_subtotal <- c(3, 7, 11, 13)   # Utilidades
fila_neta      <- 13                 # Utilidad Neta (borde superior)
filas_normales <- setdiff(1:13, filas_subtotal)

# ── Tabla con gt ──────────────────────────────────────────────────────────────
tabla <- estado_resultados |>
  gt() |>

  # Encabezado
  tab_header(
    title    = md("**Estado de Resultados**"),
    subtitle = md("*Período contable*")
  ) |>

  # Etiquetas de columnas
  cols_label(
    Concepto = md("**Concepto**"),
    Valor    = md("**Valor**")
  ) |>

  # Alineación
  cols_align(align = "left",  columns = Concepto) |>
  cols_align(align = "right", columns = Valor) |>

  # Anchos de columna
  cols_width(
    Concepto ~ px(400),
    Valor    ~ px(200)
  ) |>

  # ── Estilo del encabezado de columnas ──
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(15))
    ),
    locations = cells_column_labels()
  ) |>

  # ── Filas normales: alternado azul claro / blanco ──
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = filas_normales[seq(1, length(filas_normales), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = filas_normales[seq(2, length(filas_normales), by = 2)])
  ) |>

  # ── Filas de subtotales: fondo azul oscuro, texto blanco negrita ──
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(15))
    ),
    locations = cells_body(rows = filas_subtotal)
  ) |>

  # ── Borde superior en Utilidad Neta ──
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(2)),
    locations = cells_body(rows = fila_neta)
  ) |>

  # ── Texto general del cuerpo ──
  tab_style(
    style = cell_text(size = px(14), font = "Times New Roman"),
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

  # ── Bordes horizontales entre filas normales ──
  tab_style(
    style = cell_borders(sides = "bottom", color = "#B0C4DE", weight = px(1)),
    locations = cells_body(rows = filas_normales)
  ) |>

  # ── Nota al pie ──
  tab_source_note(
    source_note = md("*Fuente: Sistema contable interno — Estado de Resultados*")
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(1)),
    locations = cells_source_notes()
  ) |>

  # ── Opciones generales ──
  tab_options(
    table.width                    = px(630),
    table.border.top.color         = "#1B3A5C",
    table.border.top.width         = px(3),
    table.border.bottom.color      = "#1B3A5C",
    table.border.bottom.width      = px(2),
    heading.background.color       = "#F0F4FA",
    heading.border.bottom.color    = "#1B3A5C",
    heading.border.bottom.width    = px(2),
    column_labels.border.top.color = "#1B3A5C",
    column_labels.border.top.width = px(2),
    data_row.padding               = px(8),
    source_notes.font.size         = px(12)
  )

# Mostrar en el Viewer de RStudio
tabla

# Exportar a HTML
gtsave(tabla, "tabla_estado_resultados.html")

message("Tabla generada. Archivo: tabla_estado_resultados.html")
