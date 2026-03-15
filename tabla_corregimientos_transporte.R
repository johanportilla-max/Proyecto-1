# ============================================================
# TABLA – CORREGIMIENTOS: RESUMEN DE TRANSPORTES
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
  Total_transportes  = c(91, 85, 16, 13, 13, 12, 8, 2, 240),
  Valor_total        = c(4155000, 4775000, 800000, 795000, 841000,
                         700000,  560000,  265000, NA),
  Valor_promedio     = c(34945.05, 51235.29, 44375.00, 58846.15,
                         61615.38, 45833.33, 57500.00, 72500.00, NA),
  Distancia_promedio = c(2.451, 4.490, 5.631, 6.391,
                         10.398, 3.436, 11.976, 6.788, NA),
  stringsAsFactors   = FALSE
)

idx_data  <- which(corr_data$tipo == "data")
idx_total <- which(corr_data$tipo == "total")

# ── 2. Construir tabla gt ─────────────────────────────────────────────────────
tabla <- corr_data |>
  select(-tipo) |>
  gt() |>

  tab_header(
    title    = md("**Corregimientos \u2013 Resumen de Transportes**"),
    subtitle = md("*Totales y promedios por corregimiento de entrega*")
  ) |>

  cols_label(
    Corregimiento      = md("**Corregimiento**"),
    Total_transportes  = md("**Total de<br>transportes**"),
    Valor_total        = md("**Valor total de<br>transporte**"),
    Valor_promedio     = md("**Valor promedio de<br>transporte**"),
    Distancia_promedio = md("**Distancia promedio<br>(km)**")
  ) |>

  # Formato entero para conteo
  fmt_integer(
    columns  = Total_transportes,
    sep_mark = "."
  ) |>
  # Formato moneda COP (. miles, , decimal)
  fmt_currency(
    columns  = c(Valor_total, Valor_promedio),
    rows     = idx_data,
    currency = "COP",
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  # Formato distancia (3 decimales, , decimal)
  fmt_number(
    columns  = Distancia_promedio,
    rows     = idx_data,
    decimals = 3,
    dec_mark = ",",
    sep_mark = "."
  ) |>
  # NA en fila total → celda vacía
  sub_missing(
    columns      = everything(),
    rows         = idx_total,
    missing_text = ""
  ) |>

  cols_align(align = "left",  columns = Corregimiento) |>
  cols_align(align = "right", columns = c(Total_transportes, Valor_total,
                                           Valor_promedio, Distancia_promedio)) |>

  cols_width(
    Corregimiento      ~ px(155),
    Total_transportes  ~ px(105),
    Valor_total        ~ px(175),
    Valor_promedio     ~ px(185),
    Distancia_promedio ~ px(135)
  ) |>

  # ── Encabezado de columnas ──────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(13))
    ),
    locations = cells_column_labels()
  ) |>

  # ── Filas de datos (alternadas azul claro / blanco) ─────────────────────────
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
                                        Valor_promedio, Distancia_promedio),
                           rows = idx_data)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#B0C4DE", weight = px(1)),
    locations = cells_body(rows = idx_data)
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
      "*Fuente: Base de datos de entregas \u2014 Resumen por corregimiento de entrega*"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(1)),
    locations = cells_source_notes()
  ) |>

  # ── Opciones generales ─────────────────────────────────────────────────────
  tab_options(
    table.width                    = px(760),
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
