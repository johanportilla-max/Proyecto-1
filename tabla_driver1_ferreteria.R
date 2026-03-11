# ============================================================
# DRIVER 1 – COSTOS POR ACTIVIDAD DE VIAJE (FERRETERÍA)
# Requiere: install.packages(c("gt", "dplyr"))
# ============================================================

library(gt)
library(dplyr)

# ── 1. Datos ─────────────────────────────────────────────────────────────────
# Tipos de fila: "costo" | "subtotal" | "division" | "resultado"
driver1 <- data.frame(
  tipo           = c("costo",    "costo",    "costo",
                     "subtotal",
                     "division",
                     "resultado"),
  Rubro          = c(
    "Conductor \u2013 salario+prest. (2)",
    "Ayudante \u2013 salario+prest. (2)",
    "Bocato",
    "TOTAL COSTOS FIJOS (Driver 1)",
    "\u00f7 Viajes estimados al mes",
    "CF POR VIAJE (costo fijo de salir una vez)"
  ),
  Monto          = c(
    "$5,124,456",
    "$4,594,580",
    "$58,667",
    "$9,777,703",
    "200",
    "$48,889"
  ),
  Clasificacion  = c(
    "Costo Fijo",
    "Costo Fijo",
    "Costo Fijo",
    "67.13% del total",
    "",
    ""
  ),
  Justificacion  = c(
    "Personal debe pagarse cada mes",
    "Personal debe pagarse cada mes",
    "Minicargador",
    "",
    "Env\u00edos en el mes de noviembre",
    ""
  ),
  stringsAsFactors = FALSE
)

# Índices por tipo de fila
idx_costo     <- which(driver1$tipo == "costo")
idx_subtotal  <- which(driver1$tipo == "subtotal")
idx_division  <- which(driver1$tipo == "division")
idx_resultado <- which(driver1$tipo == "resultado")

# ── 2. Construir tabla gt ─────────────────────────────────────────────────────
tabla <- driver1 |>
  select(-tipo) |>          # quitar columna auxiliar antes de gt()
  gt() |>

  # Título
  tab_header(
    title    = md("**DRIVER 1 \u2013 COSTOS POR ACTIVIDAD DE VIAJE**"),
    subtitle = md("*Ferreter\u00eda \u2013 Costo fijo por viaje de entrega*")
  ) |>

  # Etiquetas de columnas
  cols_label(
    Rubro         = md("**Rubro**"),
    Monto         = md("**Monto Mensual**"),
    Clasificacion = md("**Clasificaci\u00f3n**"),
    Justificacion = md("**Justificaci\u00f3n**")
  ) |>

  # Alineación
  cols_align(align = "left",   columns = c(Rubro, Justificacion)) |>
  cols_align(align = "right",  columns = Monto) |>
  cols_align(align = "center", columns = Clasificacion) |>

  # Anchos de columna
  cols_width(
    Rubro         ~ px(280),
    Monto         ~ px(150),
    Clasificacion ~ px(160),
    Justificacion ~ px(280)
  ) |>

  # ── Estilo encabezado de columnas ──────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(14))
    ),
    locations = cells_column_labels()
  ) |>

  # ── Filas de COSTO (alternadas) ────────────────────────────────────────────
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = idx_costo[seq(1, length(idx_costo), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_costo[seq(2, length(idx_costo), by = 2)])
  ) |>

  # Nombre del rubro en azul oscuro/negrita para filas de costo
  tab_style(
    style = cell_text(color = "#1B3A5C", weight = "bold"),
    locations = cells_body(columns = Rubro, rows = idx_costo)
  ) |>
  # Clasificación en negrita para filas de costo
  tab_style(
    style = cell_text(color = "#1B3A5C", weight = "bold"),
    locations = cells_body(columns = Clasificacion, rows = idx_costo)
  ) |>

  # ── Fila SUBTOTAL (Total Costos Fijos) ─────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(14))
    ),
    locations = cells_body(rows = idx_subtotal)
  ) |>
  # Borde superior resaltado
  tab_style(
    style = cell_borders(sides = "top", color = "#7EC8E3", weight = px(2)),
    locations = cells_body(rows = idx_subtotal)
  ) |>

  # ── Fila DIVISION (÷ Viajes) ───────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#D6E8F7"),
      cell_text(color = "#1B3A5C", weight = "bold", size = px(14))
    ),
    locations = cells_body(rows = idx_division)
  ) |>
  # Monto en azul intenso para destacar el número de viajes
  tab_style(
    style = cell_text(color = "#1A6BAE", weight = "bold", size = px(15)),
    locations = cells_body(columns = Monto, rows = idx_division)
  ) |>

  # ── Fila RESULTADO (CF por Viaje) ──────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#0D2B45"),
      cell_text(color = "white", weight = "bold", size = px(15))
    ),
    locations = cells_body(rows = idx_resultado)
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#7EC8E3", weight = px(2)),
    locations = cells_body(rows = idx_resultado)
  ) |>

  # ── Bordes inferiores en filas de costo ────────────────────────────────────
  tab_style(
    style = cell_borders(sides = "bottom", color = "#B0C4DE", weight = px(1)),
    locations = cells_body(rows = idx_costo)
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
      "*Fuente: Estructura de costos internos \u2014 Ferretera \u00b7 Driver 1: Costo Fijo por actividad de viaje*"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(1)),
    locations = cells_source_notes()
  ) |>

  # ── Opciones generales ─────────────────────────────────────────────────────
  tab_options(
    table.width                    = px(900),
    table.border.top.color         = "#1B3A5C",
    table.border.top.width         = px(3),
    table.border.bottom.color      = "#1B3A5C",
    table.border.bottom.width      = px(2),
    heading.background.color       = "#F0F4FA",
    heading.border.bottom.color    = "#1B3A5C",
    heading.border.bottom.width    = px(2),
    column_labels.border.top.color = "#1B3A5C",
    column_labels.border.top.width = px(2),
    data_row.padding               = px(10),
    source_notes.font.size         = px(12)
  )

# ── 3. Mostrar y exportar ────────────────────────────────────────────────────
tabla

gtsave(tabla, "tabla_driver1_ferreteria.html")
# gtsave(tabla, "tabla_driver1_ferreteria.png")   # requiere webshot2

message("Tabla generada: tabla_driver1_ferreteria.html")
