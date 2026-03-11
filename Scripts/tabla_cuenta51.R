# Tabla profesional - Cuenta 51: Gastos de Administración
# Requiere: install.packages(c("gt", "dplyr"))

library(gt)
library(dplyr)

# Datos
cuenta51 <- data.frame(
  Subcuenta = c("5105", "5110", "5115", "5120", "5145", "5160", "5165", "5195", ""),
  Nombre = c(
    "Gastos de personal",
    "Honorarios",
    "Impuestos",
    "Arrendamientos",
    "Mantenimiento y reparaciones",
    "Depreciaciones",
    "Amortizaciones",
    "Diversos",
    "Total"
  ),
  Descripcion = c(
    "Sueldos, prestaciones sociales, aportes a seguridad social y parafiscales del personal administrativo",
    "Pagos a profesionales externos",
    "Industria y comercio, predial, vehículos y otros tributos administrativos",
    "Alquiler de oficinas, bodegas o equipos de uso administrativo",
    "De equipos y bienes de uso administrativo",
    "De activos fijos usados en administración",
    "De intangibles administrativos",
    "Gastos administrativos que no encajan en las anteriores",
    "$ 24,303,000.00"
  ),
  stringsAsFactors = FALSE
)

# Índice de la fila Total
fila_total <- nrow(cuenta51)

# Tabla con gt
tabla <- cuenta51 |>
  gt() |>

  # Encabezado principal
  tab_header(
    title    = md("**Cuenta 51**"),
    subtitle = md("*Gastos de Administración*")
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
    source_note = md("*Fuente: Sistema contable interno — Cuenta 51: Gastos de Administración*")
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
gtsave(tabla, "tabla_cuenta51.html")

library("webshot2")
gtsave(tabla, "tabla_cuenta51.png")

message("Tabla generada. Archivos: tabla_cuenta51.html")
