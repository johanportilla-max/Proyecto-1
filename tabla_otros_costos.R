# ============================================================
# TABLA – OTROS COSTOS (editables) + DESGLOSE DE SOBRECOSTOS
# Requiere: install.packages(c("gt", "dplyr"))
# ============================================================

library(gt)
library(dplyr)

# ════════════════════════════════════════════════════════════
# TABLA 1 – OTROS COSTOS (editables)
# ════════════════════════════════════════════════════════════

otros_data <- data.frame(
  tipo    = c("cop", "cop", "int", "cop", "int"),
  Concepto = c(
    "Gasto mensual papelar\u00eda e impresi\u00f3n talonarios (COP)",
    "Arriendo mensual del local \u2013 Ferretar\u00eda Borrero Ayerbe (COP)",
    "\u00c1rea total del local (m\u00b2)",
    "Costo real por m\u00b2/mes (Arriendo \u00f7 \u00c1rea total)",
    "m\u00b2 ocupados por archivo de facturas f\u00edsicas"
  ),
  Valor = c(350000.00, 13768000.00, 687, 20040.76, 7),
  stringsAsFactors = FALSE
)

idx_cop_oc <- which(otros_data$tipo == "cop")
idx_int_oc <- which(otros_data$tipo == "int")

tabla1 <- otros_data |>
  select(-tipo) |>
  gt() |>

  tab_header(title = md("**OTROS COSTOS (editables)**")) |>

  fmt_currency(
    columns  = Valor,
    rows     = idx_cop_oc,
    currency = "USD",
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  fmt_integer(
    columns  = Valor,
    rows     = idx_int_oc,
    sep_mark = "."
  ) |>

  cols_align(align = "left",  columns = Concepto) |>
  cols_align(align = "right", columns = Valor) |>

  cols_width(
    Concepto ~ px(360),
    Valor    ~ px(175)
  ) |>

  # Cabecera azul oscuro
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(13),
                align = "center", font = "Times New Roman")
    ),
    locations = cells_title()
  ) |>

  # Texto cuerpo
  tab_style(
    style = cell_text(color = "#1B3A5C", font = "Times New Roman", size = px(13)),
    locations = cells_body()
  ) |>

  # Filas alternadas
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = c(1, 3, 5))
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = c(2, 4))
  ) |>

  # Separador inferior por fila
  tab_style(
    style = cell_borders(sides = "bottom", color = "#D0D9E8", weight = px(1)),
    locations = cells_body()
  ) |>

  tab_options(
    table.width                 = px(545),
    table.border.top.color      = "#1B3A5C",
    table.border.top.width      = px(3),
    table.border.bottom.color   = "#1B3A5C",
    table.border.bottom.width   = px(2),
    column_labels.hidden        = TRUE,
    heading.background.color    = "#1B3A5C",
    heading.border.bottom.color = "#1B3A5C",
    heading.border.bottom.width = px(0),
    data_row.padding            = px(8)
  )


# ════════════════════════════════════════════════════════════
# TABLA 2 – DESGLOSE DE SOBRECOSTOS MENSUALES
# ════════════════════════════════════════════════════════════

desglose_data <- data.frame(
  tipo = c("data", "data", "data", "total"),
  Categoria = c(
    "1. Tiempo improductivo vendedores (digitando)",
    "2. Papelar\u00eda, talonarios e insumos de impresi\u00f3n",
    "3. Almacenamiento en bodega (espacio archivo f\u00edsico)",
    "TOTAL SOBRECOSTO MENSUAL"
  ),
  Mensual    = c(549682.85,   350000.00,   140285.30,  1039968.15),
  Anual      = c(6596194.17, 4200000.00, 1683423.58, 12479617.75),
  Porcentaje = c(0.5286,       0.3365,       0.1349,     1.0000),
  stringsAsFactors = FALSE
)

idx_data_d  <- which(desglose_data$tipo == "data")
idx_total_d <- which(desglose_data$tipo == "total")

tabla2 <- desglose_data |>
  select(-tipo) |>
  gt() |>

  tab_header(title = md("**DESGLOSE DE SOBRECOSTOS MENSUALES**")) |>

  cols_label(
    Categoria  = md("**Categor\u00eda de Sobrecosto**"),
    Mensual    = md("**Mensual (COP)**"),
    Anual      = md("**Anual (COP)**"),
    Porcentaje = md("**% del Total**")
  ) |>

  # Datos: moneda COP y porcentaje
  fmt_currency(
    columns  = c(Mensual, Anual),
    rows     = idx_data_d,
    currency = "USD",
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  # Total: entero sin símbolo
  fmt_integer(
    columns  = c(Mensual, Anual),
    rows     = idx_total_d,
    sep_mark = "."
  ) |>
  fmt_percent(
    columns  = Porcentaje,
    decimals = 2,
    dec_mark = ",",
    sep_mark = "."
  ) |>

  cols_align(align = "left",  columns = Categoria) |>
  cols_align(align = "right", columns = c(Mensual, Anual, Porcentaje)) |>

  cols_width(
    Categoria  ~ px(295),
    Mensual    ~ px(135),
    Anual      ~ px(140),
    Porcentaje ~ px(95)
  ) |>

  # Cabecera azul oscuro
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(13),
                align = "center", font = "Times New Roman")
    ),
    locations = cells_title()
  ) |>

  # Etiquetas de columna
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(13),
                font = "Times New Roman")
    ),
    locations = cells_column_labels()
  ) |>

  # Filas de datos alternadas
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = idx_data_d[seq(1, length(idx_data_d), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_data_d[seq(2, length(idx_data_d), by = 2)])
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C", font = "Times New Roman", size = px(13)),
    locations = cells_body(rows = idx_data_d)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#D0D9E8", weight = px(1)),
    locations = cells_body(rows = idx_data_d)
  ) |>

  # Fila TOTAL (amarillo)
  tab_style(
    style = list(
      cell_fill(color = "#F5A623"),
      cell_text(color = "#1B3A5C", weight = "bold", size = px(13),
                font = "Times New Roman")
    ),
    locations = cells_body(rows = idx_total_d)
  ) |>
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "#C47D00", weight = px(2)),
    locations = cells_body(rows = idx_total_d)
  ) |>

  tab_options(
    table.width                       = px(670),
    table.border.top.color            = "#1B3A5C",
    table.border.top.width            = px(3),
    table.border.bottom.color         = "#1B3A5C",
    table.border.bottom.width         = px(2),
    heading.background.color          = "#1B3A5C",
    heading.border.bottom.color       = "#1B3A5C",
    heading.border.bottom.width       = px(0),
    column_labels.border.top.color    = "#1B3A5C",
    column_labels.border.top.width    = px(0),
    column_labels.border.bottom.color = "#7EC8E3",
    column_labels.border.bottom.width = px(2),
    data_row.padding                  = px(8)
  )


# ── Mostrar y exportar ────────────────────────────────────────────────────────
tabla1
tabla2

gtsave(tabla1, "tabla_otros_costos.html")
gtsave(tabla2, "tabla_desglose_sobrecostos.html")
# gtsave(tabla1, "tabla_otros_costos.png")        # requiere webshot2
# gtsave(tabla2, "tabla_desglose_sobrecostos.png") # requiere webshot2

message("Tablas generadas: tabla_otros_costos.html  |  tabla_desglose_sobrecostos.html")
