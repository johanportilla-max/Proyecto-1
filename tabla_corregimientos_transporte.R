# ============================================================
# TABLA – CORREGIMIENTOS: RESUMEN DE TRANSPORTES (AMPLIADO)
# Requiere: install.packages(c("gt", "dplyr"))
# ============================================================

library(gt)
library(dplyr)

# ── 1. Datos ─────────────────────────────────────────────────────────────────
# tipo: "data" | "total"
corr_data <- data.frame(
  tipo               = c(rep("data", 8), "total"),
  Corregimiento      = c(
    "Borrero Ayerbe", "El Carmen",    "El Limonar",
    "San Bernardo",   "El Queremal",  "El Palmar",
    "El Salado",      "VillaHermosa", "Total"
  ),
  Total_transportes  = c(91, 85, 17, 12, 13, 12, 8, 2, 240),
  Valor_total        = c(4155000, 4775000, 870000, 725000, 841000,
                         700000,  560000,  250000, NA),
  Distancia_promedio = c(2.451,  4.490,  5.747,  6.290,
                         10.398, 3.436, 11.976,  6.788, NA),
  Valor_promedio     = c(34945.05, 51235.29, 45882.35, 57916.67,
                         61615.38, 45833.33, 57500.00, 75000.00, NA),
  Costo_ABC          = c(47447.98, 52871.94, 56619.87, 57853.73,
                         67185.94, 49751.92, 73324.34, 59209.39, NA),
  # Valores como fracción decimal para fmt_percent()
  Perdida_margen     = c(-0.1814, -0.0431, -0.3317,  0.0011,
                          0.0684, -0.0251, -0.1877,  0.1534, NA),
  stringsAsFactors   = FALSE
)

idx_data        <- which(corr_data$tipo == "data")
idx_total       <- which(corr_data$tipo == "total")
idx_perd_neg    <- which(corr_data$Perdida_margen < 0)   # pérdida real
idx_perd_pos    <- which(corr_data$Perdida_margen > 0)   # ganancia real

# ── 2. Construir tabla gt ─────────────────────────────────────────────────────
tabla <- corr_data |>
  select(-tipo) |>
  gt() |>

  tab_header(
    title    = md("**Corregimientos \u2013 Resumen de Transportes**"),
    subtitle = md("*Comparativo cobrado vs costo ABC por corregimiento*")
  ) |>

  cols_label(
    Corregimiento      = md("**Corregimiento**"),
    Total_transportes  = md("**Total de<br>transportes**"),
    Valor_total        = md("**Valor total de<br>transporte**"),
    Distancia_promedio = md("**Distancia<br>promedio (km)**"),
    Valor_promedio     = md("**Valor promedio<br>cobrado**"),
    Costo_ABC          = md("**Costo promedio<br>ABC**"),
    Perdida_margen     = md("**P\u00e9rdida<br>promedio**")
  ) |>

  # ── Formato de números ──────────────────────────────────────────────────────
  fmt_integer(
    columns  = Total_transportes,
    sep_mark = "."
  ) |>
  fmt_currency(
    columns  = c(Valor_total, Valor_promedio, Costo_ABC),
    rows     = idx_data,
    currency = "COP",
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  fmt_number(
    columns  = Distancia_promedio,
    rows     = idx_data,
    decimals = 3,
    dec_mark = ",",
    sep_mark = "."
  ) |>
  fmt_percent(
    columns  = Perdida_margen,
    rows     = idx_data,
    decimals = 2,
    dec_mark = ",",
    sep_mark = "."
  ) |>
  sub_missing(
    columns      = everything(),
    rows         = idx_total,
    missing_text = ""
  ) |>

  # ── Alineación ──────────────────────────────────────────────────────────────
  cols_align(align = "left",  columns = Corregimiento) |>
  cols_align(align = "right", columns = c(Total_transportes, Valor_total,
                                           Distancia_promedio, Valor_promedio,
                                           Costo_ABC, Perdida_margen)) |>

  # ── Anchos ──────────────────────────────────────────────────────────────────
  cols_width(
    Corregimiento      ~ px(140),
    Total_transportes  ~ px(90),
    Valor_total        ~ px(155),
    Distancia_promedio ~ px(115),
    Valor_promedio     ~ px(165),
    Costo_ABC          ~ px(165),
    Perdida_margen     ~ px(110)
  ) |>

  # ── Encabezado de columnas ──────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(13))
    ),
    locations = cells_column_labels()
  ) |>

  # ── Filas de datos (alternadas) ─────────────────────────────────────────────
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = idx_data[seq(1, length(idx_data), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_data[seq(2, length(idx_data), by = 2)])
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C", weight = "bold"),
    locations = cells_body(columns = Corregimiento, rows = idx_data)
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C"),
    locations = cells_body(columns = c(Total_transportes, Valor_total,
                                        Distancia_promedio, Valor_promedio,
                                        Costo_ABC),
                           rows = idx_data)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#B0C4DE", weight = px(1)),
    locations = cells_body(rows = idx_data)
  ) |>

  # ── Pérdida: negativa en rojo, positiva en verde ────────────────────────────
  tab_style(
    style = cell_text(color = "#C0392B", weight = "bold"),
    locations = cells_body(columns = Perdida_margen, rows = idx_perd_neg)
  ) |>
  tab_style(
    style = cell_text(color = "#1A7A3C", weight = "bold"),
    locations = cells_body(columns = Perdida_margen, rows = idx_perd_pos)
  ) |>

  # ── Fila TOTAL ──────────────────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(15))
    ),
    locations = cells_body(rows = idx_total)
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#7EC8E3", weight = px(2)),
    locations = cells_body(rows = idx_total)
  ) |>

  # ── Fuente general ─────────────────────────────────────────────────────────
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

  # ── Nota al pie ────────────────────────────────────────────────────────────
  tab_source_note(
    source_note = md(
      "*Fuente: Base de datos de entregas \u2014 Comparativo cobrado vs costo ABC por corregimiento*"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(1)),
    locations = cells_source_notes()
  ) |>

  # ── Opciones generales ─────────────────────────────────────────────────────
  tab_options(
    table.width                    = px(945),
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

gtsave(tabla, "tabla_corregimientos_transporte.html")
# gtsave(tabla, "tabla_corregimientos_transporte.png")   # requiere webshot2

message("Tabla generada: tabla_corregimientos_transporte.html")
