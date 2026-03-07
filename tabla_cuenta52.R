# Tabla profesional - Cuenta 52: Gastos de Ventas
# Requiere: install.packages(c("gt", "dplyr"))

library(gt)
library(dplyr)

# Datos
cuenta52 <- data.frame(
  Subcuenta = c("5205", "5210", "5215", "5220", "5235", "5245", "5260", "5265", "5280", "5295", ""),
  Nombre = c(
    "Gastos de personal",
    "Honorarios",
    "Impuestos",
    "Arrendamientos",
    "Servicios",
    "Mantenimiento y reparaciones",
    "Depreciaciones",
    "Amortizaciones",
    "Transporte y fletes",
    "Diversos",
    "Total"
  ),
  Descripcion = c(
    "Sueldos, prestaciones sociales y aportes del personal del área de ventas y comercial",
    "Pagos a externos por asesorías comerciales, agentes de ventas independientes",
    "Tributos directamente relacionados con la actividad de ventas",
    "Alquiler de locales comerciales, puntos de venta o equipos de ventas",
    "Energía, teléfono, internet y servicios públicos de puntos de venta",
    "De equipos y bienes usados en el área de ventas",
    "De activos fijos usados en la actividad de ventas",
    "De intangibles relacionados con la actividad comercial",
    "Envío y distribución de productos al cliente",
    "Gastos comerciales que no encajan en las subcuentas anteriores",
    "$ 64,872,000.00"
  ),
  stringsAsFactors = FALSE
)

# Índice de la fila Total
fila_total <- nrow(cuenta52)

# Tabla con gt
tabla <- cuenta52 |>
  gt() |>

  # Encabezado principal
  tab_header(
    title    = md("**Cuenta 52**"),
    subtitle = md("*Gastos de Ventas*")
  ) |>

  # Etiquetas de columnas
  cols_label(
    Subcuenta   = md("**Subcuenta**"),
    Nombre      = md("**Nombre**"),
    Descripcion = md("**Descripción**")
  ) |>

  # Alineación
  cols_align(align = "center", columns = Subcuenta) |>
  cols_align(align = "left",   columns = c(Nombre, Descripcion)) |>

  # Anchos de columna
  cols_width(
    Subcuenta   ~ px(90),
    Nombre      ~ px(220),
    Descripcion ~ px(520)
  ) |>

  # Estilo del encabezado
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(15))
    ),
    locations = cells_column_labels()
  ) |>

  # Filas de datos (pares / impares)
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = seq(1, fila_total - 1, by = 2))
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = seq(2, fila_total - 1, by = 2))
  ) |>

  # Fila Total: fondo oscuro, texto blanco y negrita
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(15))
    ),
    locations = cells_body(rows = fila_total)
  ) |>

  # Nombres en la columna Nombre en azul oscuro (excepto Total)
  tab_style(
    style = cell_text(color = "#1B3A5C", weight = "bold"),
    locations = cells_body(columns = Nombre, rows = 1:(fila_total - 1))
  ) |>

  # Estilo general del texto en cuerpo
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

  # Bordes
  tab_style(
    style = cell_borders(sides = "bottom", color = "#B0C4DE", weight = px(1)),
    locations = cells_body(rows = 1:(fila_total - 1))
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#1B3A5C", weight = px(2)),
    locations = cells_body(rows = fila_total)
  ) |>

  # Nota al pie
  tab_source_note(
    source_note = md("*Fuente: Sistema contable interno — Cuenta 52: Gastos de Ventas*")
  ) |>

  # Borde superior de la nota al pie
  tab_style(
    style = cell_borders(sides = "top", color = "#B0C4DE", weight = px(1)),
    locations = cells_source_notes()
  ) |>

  # Opciones generales
  tab_options(
    table.width                    = px(850),
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
gtsave(tabla, "tabla_cuenta52.html")

# Exportar a PNG (requiere webshot2: install.packages("webshot2"))
# gtsave(tabla, "tabla_cuenta52.png")

message("Tabla generada. Archivos: tabla_cuenta52.html")
