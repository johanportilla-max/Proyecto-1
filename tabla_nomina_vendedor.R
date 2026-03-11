# ============================================================
# NÓMINA – VENDEDOR
# Requiere: install.packages(c("gt", "dplyr"))
# ============================================================

library(gt)
library(dplyr)

# ── 1. Datos ─────────────────────────────────────────────────────────────────
# tipos: "devengado" | "subtotal_dev" | "prestacion" | "total" | "hora"
nomina <- data.frame(
  tipo     = c(
    "devengado",    # Salario base
    "devengado",    # Horas extra diurna
    "subtotal_dev", # Subtotal devengado
    "prestacion",   # Auxilio de transporte
    "prestacion",   # Pensión
    "prestacion",   # ARL
    "prestacion",   # Caja de compensación
    "prestacion",   # Vacaciones
    "prestacion",   # Cesantías
    "prestacion",   # Prima
    "prestacion",   # Intereses de cesantías
    "total",        # Total
    "hora"          # Valor de la hora
  ),
  Concepto = c(
    "Salario base",
    "Horas extra diurna (4)",
    "Subtotal devengado",
    "Auxilio de transporte",
    "Pensi\u00f3n",
    "ARL",
    "Caja de compensaci\u00f3n familiar",
    "Vacaciones",
    "Cesant\u00edas",
    "Prima",
    "Intereses de las cesant\u00edas",
    "Total",
    "Valor de la hora (Total / 180)"
  ),
  Valor    = c(
    1423500.00,
    8088.00,
    1455852.00,
    200000.00,
    174702.24,
    63330.00,
    58234.08,
    59360.00,
    137932.00,
    137932.00,
    9948.00,
    2297290.32,
    12762.72
  ),
  stringsAsFactors = FALSE
)

# Índices por tipo
idx_dev       <- which(nomina$tipo == "devengado")
idx_sub_dev   <- which(nomina$tipo == "subtotal_dev")
idx_prest     <- which(nomina$tipo == "prestacion")
idx_total     <- which(nomina$tipo == "total")
idx_hora      <- which(nomina$tipo == "hora")

# ── 2. Construir tabla gt ─────────────────────────────────────────────────────
tabla <- nomina |>
  select(-tipo) |>
  gt() |>

  # Título
  tab_header(
    title    = md("**N\u00f3mina**"),
    subtitle = md("*Vendedor*")
  ) |>

  # Etiquetas de columnas
  cols_label(
    Concepto = md("**Concepto**"),
    Valor    = md("**Valor**")
  ) |>

  # Formato de moneda (con símbolo $, separador de miles, 2 decimales)
  fmt_currency(
    columns  = Valor,
    currency = "COP",
    decimals = 2,
    sep_mark = ",",
    dec_mark = "."
  ) |>

  # Alineación
  cols_align(align = "left",  columns = Concepto) |>
  cols_align(align = "right", columns = Valor) |>

  # Anchos
  cols_width(
    Concepto ~ px(320),
    Valor    ~ px(200)
  ) |>

  # ── Encabezado de columnas ─────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(14))
    ),
    locations = cells_column_labels()
  ) |>

  # ── Filas DEVENGADO (alternadas azul claro / blanco) ──────────────────────
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = idx_dev[seq(1, length(idx_dev), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_dev[seq(2, length(idx_dev), by = 2)])
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C", weight = "bold"),
    locations = cells_body(columns = Concepto, rows = idx_dev)
  ) |>

  # ── Fila SUBTOTAL DEVENGADO ────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#2E6DA4"),
      cell_text(color = "white", weight = "bold", size = px(13))
    ),
    locations = cells_body(rows = idx_sub_dev)
  ) |>
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "#7EC8E3", weight = px(2)),
    locations = cells_body(rows = idx_sub_dev)
  ) |>

  # ── Filas PRESTACIONES (alternadas verde muy claro / blanco) ───────────────
  tab_style(
    style = cell_fill(color = "#EBF5EB"),
    locations = cells_body(rows = idx_prest[seq(1, length(idx_prest), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_prest[seq(2, length(idx_prest), by = 2)])
  ) |>
  tab_style(
    style = cell_text(color = "#1A5C38", weight = "bold"),
    locations = cells_body(columns = Concepto, rows = idx_prest)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#B0C4DE", weight = px(1)),
    locations = cells_body(rows = idx_prest)
  ) |>

  # ── Fila TOTAL ─────────────────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(15))
    ),
    locations = cells_body(rows = idx_total)
  ) |>
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "#7EC8E3", weight = px(2)),
    locations = cells_body(rows = idx_total)
  ) |>

  # ── Fila VALOR DE LA HORA ─────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#0D2B45"),
      cell_text(color = "white", weight = "bold", size = px(14))
    ),
    locations = cells_body(rows = idx_hora)
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#7EC8E3", weight = px(1)),
    locations = cells_body(rows = idx_hora)
  ) |>

  # ── Separador visual entre devengado y prestaciones ───────────────────────
  tab_style(
    style = cell_borders(sides = "top", color = "#1B3A5C", weight = px(2)),
    locations = cells_body(rows = idx_prest[1])
  ) |>

  # ── Fuente general ─────────────────────────────────────────────────────────
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

  # ── Nota al pie ────────────────────────────────────────────────────────────
  tab_source_note(
    source_note = md(
      "*Fuente: Liquidaci\u00f3n n\u00f3mina interna \u2014 Vendedor \u00b7 Salario m\u00ednimo legal vigente + prestaciones sociales*"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(1)),
    locations = cells_source_notes()
  ) |>

  # ── Opciones generales ─────────────────────────────────────────────────────
  tab_options(
    table.width                    = px(540),
    table.border.top.color         = "#1B3A5C",
    table.border.top.width         = px(3),
    table.border.bottom.color      = "#1B3A5C",
    table.border.bottom.width      = px(2),
    heading.background.color       = "#F0F4FA",
    heading.border.bottom.color    = "#1B3A5C",
    heading.border.bottom.width    = px(2),
    column_labels.border.top.color = "#1B3A5C",
    column_labels.border.top.width = px(2),
    data_row.padding               = px(9),
    source_notes.font.size         = px(12)
  )

# ── 3. Mostrar y exportar ────────────────────────────────────────────────────
tabla

gtsave(tabla, "tabla_nomina_vendedor.html")
# gtsave(tabla, "tabla_nomina_vendedor.png")   # requiere webshot2

message("Tabla generada: tabla_nomina_vendedor.html")
