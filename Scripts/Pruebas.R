# ======================================================
# TABLA DE GASTOS DE ADMINISTRACIÓN (CUENTA 51)
# ESTILO ACADÉMICO PROFESIONAL CON LIBRERÍA gt
# ======================================================

# Cargar librerías
library(gt)
library(dplyr)

# Fijar semilla para reproducibilidad
set.seed(2025)

# ---- 1. Crear el data frame base con las subcuentas ----
datos <- data.frame(
  Subcuenta = c(5105, 5110, 5115, 5120, 5145, 5160, 5165, 5195),
  Nombre = c("Gastos de personal", "Honorarios", "Impuestos", "Arrendamientos",
             "Mantenimiento y reparaciones", "Depreciaciones", "Amortizaciones", "Diversos"),
  Descripcion = c("Sueldos, prestaciones sociales, aportes a seguridad social y parafiscales del personal administrativo",
                  "Pagos a profesionales externos",
                  "Industria y comercio, predial, vehículos y otros tributos administrativos",
                  "Alquiler de oficinas, bodegas o equipos de uso administrativo",
                  "De equipos y bienes de uso administrativo",
                  "De activos fijos usados en administración",
                  "De intangibles administrativos",
                  "Gastos administrativos que no encajan en las anteriores"),
  stringsAsFactors = FALSE
)

# ---- 2. Generar montos aleatorios que sumen el total deseado ----
total_deseado <- 24303000.00

# Crear valores aleatorios y escalarlos para que sumen el total
n <- nrow(datos)
montos_aleatorios <- runif(n, min = 500000, max = 5000000)   # rangos típicos
montos_aleatorios <- montos_aleatorios / sum(montos_aleatorios) * total_deseado
datos$Monto <- round(montos_aleatorios, 2)

# ---- 3. Añadir fila de total ----
total_row <- data.frame(
  Subcuenta = NA,
  Nombre = "Total",
  Descripcion = "",
  Monto = total_deseado,
  stringsAsFactors = FALSE
)

tabla_completa <- bind_rows(datos, total_row)

# ---- 4. Construir tabla profesional con gt ----
tabla_gt <- tabla_completa %>%
  gt() %>%
  # Título y subtítulo
  tab_header(
    title = md("**Cuenta 51 - Gastos de Administración**"),
    subtitle = md("Detalle de subcuentas y montos correspondientes a noviembre de 2025")
  ) %>%
  # Formato de moneda (estilo US: $ y comas para miles, punto decimal)
  fmt_currency(
    columns = Monto,
    currency = "USD",
    decimals = 2
  ) %>%
  # Etiquetas de columnas
  cols_label(
    Subcuenta = "Subcuenta",
    Nombre = "Nombre",
    Descripcion = "Descripción",
    Monto = "Monto (USD)"
  ) %>%
  # Alineación de columnas
  cols_align(align = "center", columns = Subcuenta) %>%
  cols_align(align = "left", columns = c(Nombre, Descripcion)) %>%
  cols_align(align = "right", columns = Monto) %>%
  # Formato especial para la fila del total (negrita + fondo gris)
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_body(rows = Nombre == "Total")
  ) %>%
  # Ocultar NA en las celdas vacías (corrección: usar c() para seleccionar columnas)
  fmt_missing(
    columns = c(Subcuenta, Descripcion),
    missing_text = ""
  ) %>%
  # Ajustes de ancho de columna
  cols_width(
    Subcuenta ~ px(80),
    Nombre ~ px(150),
    Descripcion ~ px(300),
    Monto ~ px(130)
  ) %>%
  # Tipografía y espaciado general
  tab_options(
    table.font.size = "small",
    heading.title.font.size = "medium",
    heading.subtitle.font.size = "small",
    column_labels.font.weight = "bold"
  )

# Mostrar la tabla
tabla_gt

# Para guardarla: gtsave(tabla_gt, "tabla_gastos_administracion.html")