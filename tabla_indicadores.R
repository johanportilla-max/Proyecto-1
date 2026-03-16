# ============================================================
# TABLA – INDICADORES
# Requiere: install.packages(c("gt", "dplyr"))
# ============================================================

library(gt)
library(dplyr)

# ── 1. Datos ─────────────────────────────────────────────────────────────────
indicadores <- data.frame(
  Indicador = c(
    "Sobrecosto mensual como % de la n\u00f3mina",
    "Sobrecosto mensual como % de las utilidades"
  ),
  Valor = c(0.0278, 0.0907),
  stringsAsFactors = FALSE
)

# ── 2. Tabla gt ───────────────────────────────────────────────────────────────
tabla <- indicadores |>
  gt() |>

  tab_header(title = md("**Indicadores**")) |>

  fmt_percent(
    columns  = Valor,
    decimals = 2,
    dec_mark = ",",
    sep_mark = "."
  ) |>

  cols_align(align = "left",  columns = Indicador) |>
  cols_align(align = "right", columns = Valor) |>

  cols_width(
    Indicador ~ px(340),
    Valor     ~ px(120)
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

  # Texto filas
  tab_style(
    style = cell_text(color = "#1B3A5C", font = "Times New Roman", size = px(13)),
    locations = cells_body()
  ) |>

  # Filas alternadas
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = 1)
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = 2)
  ) |>

  # Separador inferior
  tab_style(
    style = cell_borders(sides = "bottom", color = "#D0D9E8", weight = px(1)),
    locations = cells_body()
  ) |>

  tab_options(
    table.width                 = px(470),
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

# ── 3. Mostrar y exportar ─────────────────────────────────────────────────────
tabla

gtsave(tabla, "tabla_indicadores.html")
# gtsave(tabla, "tabla_indicadores.png")   # requiere webshot2

message("Tabla generada: tabla_indicadores.html")
