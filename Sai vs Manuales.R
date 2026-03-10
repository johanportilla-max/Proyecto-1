# Tabla profesional - Facturas SAI vs Manuales por mes
# Requiere: install.packages(c("gt", "dplyr"))

library(gt)
library(dplyr)

# ── Datos ─────────────────────────────────────────────────────────────────────
facturas <- data.frame(
  Meses             = c(as.character(1:12), ""),
  Cantidad_SAI      = c(2420, 2399, 2654, 2691, 2719, 2482, 2956, 2643, 2487, 2753, 2536, 2124, 30864),
  Cantidad_Manuales = c(1465, 1461, 1622, 1693, 1700, 1709, 1874, 1654, 1744, 1931, 2088, 1909, 20850),
  Total_facturas    = c(3885, 3860, 4276, 4384, 4419, 4191, 4830, 4297, 4231, 4684, 4624, 4033, 51714),
  Porcentaje        = c("37.71%", "37.85%", "37.93%", "38.62%", "38.47%",
                        "40.78%", "38.80%", "38.49%", "41.22%", "41.23%",
                        "45.16%", "47.33%", "40.32%"),
  Cambios           = c("0", "0.37%", "0.22%", "1.77%", "-0.38%",
                        "5.66%", "-5.10%", "-0.80%", "6.62%", "0.01%",
                        "8.70%", "4.60%", ""),
  stringsAsFactors  = FALSE
)

fila_total  <- nrow(facturas)          # fila 13 — totales
filas_datos <- 1:(fila_total - 1)      # filas 1-12
# Filas con cambio distinto de cero (se pinta rosa)
filas_rosa  <- which(facturas$Cambios != "0" & facturas$Cambios != "")

# ── Tabla ─────────────────────────────────────────────────────────────────────
tabla <- facturas |>
  gt() |>
  
  # Encabezado
  tab_header(
    title    = md("**Facturas SAI vs. Manuales**"),
    subtitle = md("*Análisis mensual — porcentaje de facturas fuera del sistema*")
  ) |>
  
  # Etiquetas de columnas
  cols_label(
    Meses             = md("**Meses**"),
    Cantidad_SAI      = md("**Cantidad SAI**"),
    Cantidad_Manuales = md("**Cantidad Manuales**"),
    Total_facturas    = md("**Total de facturas**"),
    Porcentaje        = md("**% Porcentaje de facturas fuera del sistema**"),
    Cambios           = md("**Cambios en los meses**")
  ) |>
  
  # Alineación
  cols_align(align = "center", columns = everything()) |>
  
  # Anchos
  cols_width(
    Meses             ~ px(80),
    Cantidad_SAI      ~ px(130),
    Cantidad_Manuales ~ px(150),
    Total_facturas    ~ px(150),
    Porcentaje        ~ px(180),
    Cambios           ~ px(160)
  ) |>
  
  # ── Estilos de cabecera ──────────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(14))
    ),
    locations = cells_column_labels()
  ) |>
  
  # ── Filas alternas (solo datos, excluye total) ───────────────────────────────
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = seq(1, fila_total - 1, by = 2))
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = seq(2, fila_total - 1, by = 2))
  ) |>
  
  # ── Fila total ───────────────────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(15))
    ),
    locations = cells_body(rows = fila_total)
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#1B3A5C", weight = px(2)),
    locations = cells_body(rows = fila_total)
  ) |>
  
  # ── Columna Meses y SAI en azul negrita (filas de datos) ────────────────────
  tab_style(
    style = cell_text(color = "#1B3A5C", weight = "bold"),
    locations = cells_body(columns = c(Meses, Cantidad_SAI), rows = filas_datos)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Total_facturas, rows = filas_datos)
  ) |>
  
  # ── Texto general ────────────────────────────────────────────────────────────
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
  
  # ── Bordes horizontales entre filas de datos ─────────────────────────────────
  tab_style(
    style = cell_borders(sides = "bottom", color = "#B0C4DE", weight = px(1)),
    locations = cells_body(rows = filas_datos)
  ) |>
  
  # ── Nota al pie ──────────────────────────────────────────────────────────────
  tab_source_note(
    source_note = md("*Fuente: Sistema contable interno — Comparativo SAI vs. Facturas Manuales*")
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(1)),
    locations = cells_source_notes()
  ) |>
  
  # ── Opciones generales ───────────────────────────────────────────────────────
  tab_options(
    table.width                    = px(870),
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

# ── Exportar ──────────────────────────────────────────────────────────────────
tabla

gtsave(tabla, "tabla_facturas_manuales.png")

message("Tabla generada: tabla_facturas_manuales.html")