# ============================================================
# TABLA – NÓMINA VENDEDOR
# Requiere: install.packages(c("gt", "dplyr"))
# ============================================================

library(gt)
library(dplyr)

# ── 1. Datos ─────────────────────────────────────────────────────────────────
nomina <- data.frame(
  seccion = c(rep("nomina", 13), rep("datos", 3)),
  tipo    = c(
    "devengado",    # Salario base
    "devengado",    # Horas extra diurna
    "subtotal_dev", # Subtotal devengado (base cálculo)
    "prestacion",   # Auxilio de transporte
    "prestacion",   # Pensión
    "prestacion",   # ARL
    "prestacion",   # Caja de compensación familiar
    "prestacion",   # Vacaciones
    "prestacion",   # Cesantías
    "prestacion",   # Prima
    "prestacion",   # Intereses de las cesantías
    "total",        # TOTAL COSTO MENSUAL POR PERSONA
    "hora",         # Valor de la hora (Total / 180)
    "datos_cop",    # Total nómina mensual empresa
    "datos_int",    # Número de vendedores
    "datos_int"     # Horas mensuales por vendedor en facturación manual
  ),
  Concepto = c(
    "Salario base",
    "Horas extra diurna (4h \u00d7 $7.736/h)",
    "Subtotal devengado (base c\u00e1lculo)",
    "Auxilio de transporte",
    "Pensi\u00f3n",
    "ARL",
    "Caja de compensaci\u00f3n familiar",
    "Vacaciones",
    "Cesant\u00edas",
    "Prima",
    "Intereses de las cesant\u00edas",
    "TOTAL COSTO MENSUAL POR PERSONA",
    "Valor de la hora (Total / 180)",
    "Total n\u00f3mina mensual empresa",
    "N\u00famero de vendedores",
    "Horas mensuales por vendedor en facturaci\u00f3n manual"
  ),
  Valor = c(
    1423500.00,
    38680.00,
    1578220.00,
    200000.00,
    189386.40,
    68653.04,
    63128.80,
    64349.36,
    149525.53,
    149525.53,
    10784.15,
    2473572.81,
    13742.07,
    109936.57,
    8,
    5
  ),
  stringsAsFactors = FALSE
)

# Índices por tipo de fila
idx_dev       <- which(nomina$tipo == "devengado")
idx_sub_dev   <- which(nomina$tipo == "subtotal_dev")
idx_prest     <- which(nomina$tipo == "prestacion")
idx_total     <- which(nomina$tipo == "total")
idx_hora      <- which(nomina$tipo == "hora")
idx_datos_cop <- which(nomina$tipo == "datos_cop")
idx_int       <- which(nomina$tipo == "datos_int")
idx_nomina    <- which(nomina$seccion == "nomina")
idx_datos     <- which(nomina$seccion == "datos")
idx_cop       <- c(idx_dev, idx_sub_dev, idx_prest, idx_total, idx_hora, idx_datos_cop)

# ── 2. Construir tabla gt ─────────────────────────────────────────────────────
tabla <- nomina |>
  select(-seccion, -tipo) |>
  gt() |>

  # ── Grupos de filas / secciones ─────────────────────────────────────────────
  tab_row_group(label = "N\u00d3MINA VENDEDOR", rows = idx_nomina) |>
  tab_row_group(label = "DATOS GENERALES",  rows = idx_datos)  |>
  row_group_order(groups = c("N\u00d3MINA VENDEDOR", "DATOS GENERALES")) |>

  # ── Formato numérico ─────────────────────────────────────────────────────────
  fmt_currency(
    columns  = Valor,
    rows     = idx_cop,
    currency = "USD",   # símbolo $
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  fmt_integer(
    columns  = Valor,
    rows     = idx_int,
    sep_mark = "."
  ) |>

  # ── Alineación ──────────────────────────────────────────────────────────────
  cols_align(align = "left",  columns = Concepto) |>
  cols_align(align = "right", columns = Valor) |>

  # ── Anchos ──────────────────────────────────────────────────────────────────
  cols_width(
    Concepto ~ px(360),
    Valor    ~ px(175)
  ) |>

  # ── Estilo: cabeceras de grupo ───────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#1B3A5C"),
      cell_text(color = "white", weight = "bold", size = px(14),
                align = "center", font = "Times New Roman")
    ),
    locations = cells_row_groups()
  ) |>

  # ── Filas DEVENGADO (alternadas azul claro / blanco) ──────────────────────
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = idx_dev[seq(1, length(idx_dev), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_dev[seq(2, length(idx_dev), by = 2)])
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C", font = "Times New Roman", size = px(13)),
    locations = cells_body(rows = idx_dev)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#D0D9E8", weight = px(1)),
    locations = cells_body(rows = idx_dev)
  ) |>

  # ── Fila SUBTOTAL DEVENGADO ────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#2E6DA4"),
      cell_text(color = "white", weight = "bold", size = px(13),
                font = "Times New Roman", style = "italic")
    ),
    locations = cells_body(rows = idx_sub_dev)
  ) |>
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "#7EC8E3", weight = px(2)),
    locations = cells_body(rows = idx_sub_dev)
  ) |>

  # ── Filas PRESTACIONES (alternadas verde claro / blanco) ───────────────────
  tab_style(
    style = cell_fill(color = "#EBF5EB"),
    locations = cells_body(rows = idx_prest[seq(1, length(idx_prest), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_prest[seq(2, length(idx_prest), by = 2)])
  ) |>
  tab_style(
    style = cell_text(color = "#1A5C38", font = "Times New Roman", size = px(13)),
    locations = cells_body(rows = idx_prest)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#C8DFC8", weight = px(1)),
    locations = cells_body(rows = idx_prest)
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "#1B3A5C", weight = px(2)),
    locations = cells_body(rows = idx_prest[1])
  ) |>

  # ── Fila TOTAL COSTO MENSUAL (fondo amarillo) ──────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#F5A623"),
      cell_text(color = "#1B3A5C", weight = "bold", size = px(13),
                font = "Times New Roman")
    ),
    locations = cells_body(rows = idx_total)
  ) |>
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "#C47D00", weight = px(2)),
    locations = cells_body(rows = idx_total)
  ) |>

  # ── Fila VALOR DE LA HORA ─────────────────────────────────────────────────
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF"),
      cell_text(color = "#1B3A5C", size = px(13), font = "Times New Roman")
    ),
    locations = cells_body(rows = idx_hora)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#1B3A5C", weight = px(2)),
    locations = cells_body(rows = idx_hora)
  ) |>

  # ── Sección DATOS GENERALES (alternadas + valores en azul) ────────────────
  tab_style(
    style = cell_fill(color = "#EAF1FB"),
    locations = cells_body(rows = idx_datos[seq(1, length(idx_datos), by = 2)])
  ) |>
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = idx_datos[seq(2, length(idx_datos), by = 2)])
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C", font = "Times New Roman", size = px(13)),
    locations = cells_body(columns = Concepto, rows = idx_datos)
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C", font = "Times New Roman", size = px(13)),
    locations = cells_body(columns = Valor, rows = idx_datos_cop)
  ) |>
  tab_style(
    style = cell_text(color = "#1B3A5C", weight = "bold", font = "Times New Roman", size = px(13)),
    locations = cells_body(columns = Valor, rows = idx_int)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#D0D9E8", weight = px(1)),
    locations = cells_body(rows = idx_datos)
  ) |>

  # ── Opciones generales ─────────────────────────────────────────────────────
  tab_options(
    table.width                    = px(545),
    table.border.top.color         = "#1B3A5C",
    table.border.top.width         = px(3),
    table.border.bottom.color      = "#1B3A5C",
    table.border.bottom.width      = px(2),
    column_labels.hidden           = TRUE,
    row_group.border.top.color     = "#1B3A5C",
    row_group.border.top.width     = px(2),
    row_group.border.bottom.color  = "#1B3A5C",
    row_group.border.bottom.width  = px(1),
    row_group.padding              = px(6),
    data_row.padding               = px(8)
  )

# ── 3. Mostrar y exportar ─────────────────────────────────────────────────────
tabla

gtsave(tabla, "tabla_nomina_vendedor.html")
# gtsave(tabla, "tabla_nomina_vendedor.png")   # requiere webshot2

message("Tabla generada: tabla_nomina_vendedor.html")
