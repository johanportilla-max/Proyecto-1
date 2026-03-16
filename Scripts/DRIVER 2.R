# ============================================================
# DRIVER 2 – COSTOS POR KM RECORRIDO (FERRETERÍA)
# Requiere: install.packages(c("gt", "dplyr"))
# ============================================================

library(gt)
library(dplyr)

# ── 1. Datos ─────────────────────────────────────────────────────────────────
# tipos: "costo" | "subtotal" | "km_edit" | "factor" | "km_total" | "resultado"
driver2 <- data.frame(
  tipo          = c("costo",    "costo",
                    "subtotal",
                    "km_edit",
                    "factor",
                    "km_total",
                    "resultado"),
  Rubro         = c(
    "ACPM / Diesel (2 veh\u00edculos)",
    "Mantenimiento (2 veh\u00edculos)",
    "TOTAL COSTOS VARIABLES (Driver 2)",
    "\u00d7 Km promedio por viaje (solo ida)",
    "\u00d7 Factor 2 (veh\u00edculo regresa vac\u00edo desde destino)",
    "Km totales recorridos al mes",
    "CV POR KM (costo variable por cada km adicional)"
  ),
  Monto         = c(
    "$1,893,969",
    "$1,000,000",
    "$2,893,969",
    "4.4 km",
    "2x",
    "2.127 km/mes",
    "$1,360/km"
  ),
  Clasificacion = c(
    "Costo Variable",
    "Semi-Variable",
    "32.87% del total",
    "",
    "",
    "",
    ""
  ),
  Justificacion = c(
    "A mayor km, mayor consumo",
    "Se intensifica con el uso del veh\u00edculo",
    "32.87% son costos VARIABLES \u2014 crecen con los km recorridos",
    "Promedio calculado de la base de noviembre",
    "",
    "",
    "Por cada km extra de distancia, este es el costo adicional"
  ),
  stringsAsFactors = FALSE
)

# Índices por tipo
idx_costo     <- which(driver2$tipo == "costo")
idx_subtotal  <- which(driver2$tipo == "subtotal")
idx_km_edit   <- which(driver2$tipo == "km_edit")
idx_factor    <- which(driver2$tipo == "factor")
idx_km_total  <- which(driver2$tipo == "km_total")
idx_resultado <- which(driver2$tipo == "resultado")

# ── 2. Construir tabla gt ─────────────────────────────────────────────────────
tabla <- driver2 |>
  select(-tipo) |>
  gt() |>
  
  # Título
  tab_header(
    title    = md("**DRIVER 2 \u2013 COSTOS POR KM RECORRIDO**"),
    subtitle = md("*Ferreter\u00eda \u2013 Costos variables con la distancia*")
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
    Rubro         ~ px(300),
    Monto         ~ px(150),
    Clasificacion ~ px(160),
    Justificacion ~ px(280)
  ) |>
  
  # ── Encabezado de columnas ─────────────────────────────────────────────────
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
  # Rubro en azul/negrita
  tab_style(
    style = cell_text(color = "#1B3A5C", weight = "bold"),
    locations = cells_body(columns = Rubro, rows = idx_costo)
  ) |>
  # "Costo Variable" en verde oscuro; "Semi-Variable" en naranja oscuro
  tab_style(
    style = cell_text(color = "#1A5C38", weight = "bold"),
    locations = cells_body(columns = Clasificacion, rows = idx_costo[1])
  ) |>
  tab_style(
    style = cell_text(color = "#7B4F00", weight = "bold"),
    locations = cells_body(columns = Clasificacion, rows = idx_costo[2])
  ) |>
  
  # ── Fila SUBTOTAL ──────────────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(14))
    ),
    locations = cells_body(rows = idx_subtotal)
  ) |>
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "#7EC8E3", weight = px(2)),
    locations = cells_body(rows = idx_subtotal)
  ) |>
  
  # ── Fila KM EDITABLE ──────────────────────────────────────────────────────
  tab_style(
    style = cell_fill(color = "#D6E8F7"),
    locations = cells_body(rows = idx_km_edit)
  ) |>
  # "4.3 km" en azul intenso/negrita
  tab_style(
    style = cell_text(color = "#1A6BAE", weight = "bold", size = px(15)),
    locations = cells_body(columns = Monto, rows = idx_km_edit)
  ) |>
  # "← editable" en azul itálica
  tab_style(
    style = cell_text(color = "#1A6BAE", style = "italic", weight = "bold"),
    locations = cells_body(columns = Clasificacion, rows = idx_km_edit)
  ) |>
  
  # ── Fila FACTOR 2 ──────────────────────────────────────────────────────────
  tab_style(
    style = cell_fill(color = "#F2F2F2"),
    locations = cells_body(rows = idx_factor)
  ) |>
  tab_style(
    style = cell_text(color = "#444444"),
    locations = cells_body(rows = idx_factor)
  ) |>
  
  # ── Fila KM TOTALES ────────────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#E3EDF8"),
      cell_text(color = "#1B3A5C", weight = "bold")
    ),
    locations = cells_body(rows = idx_km_total)
  ) |>
  
  # ── Fila RESULTADO (CV por KM) ─────────────────────────────────────────────
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
  # Celda de Justificación del resultado con fondo distinto para destacar
  tab_style(
    style = list(
      cell_fill(color = "#1A5C38"),
      cell_text(color = "white", weight = "bold", size = px(13))
    ),
    locations = cells_body(columns = Justificacion, rows = idx_resultado)
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
      "*Fuente: Estructura de costos internos \u2014 Ferreter\u00eda Triple AAA \u00b7 Driver 2: Costo Variable por km recorrido*"
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(1)),
    locations = cells_source_notes()
  ) |>
  
  # ── Opciones generales ─────────────────────────────────────────────────────
  tab_options(
    table.width                    = px(910),
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

gtsave(tabla, "tabla_driver2_ferreteria.png")
# gtsave(tabla, "tabla_driver2_ferreteria.png")   # requiere webshot2

message("Tabla generada: tabla_driver2_ferreteria.html")