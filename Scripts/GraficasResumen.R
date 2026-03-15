# ══════════════════════════════════════════════════════════════════════
#  CONTEXTO DE LA EMPRESA — Agrovariedades Triple A S.A.S
#  Proyecto en Ingeniería I · 2026-I · Universidad del Valle
#  Fuente: SAI Open — Ventas noviembre 2025 (8.526 registros)
# ══════════════════════════════════════════════════════════════════════

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(forcats)
library(gridExtra)
library(stringr)

# ── PALETA Y TEMA ──────────────────────────────────────────────────────
azul_1  <- "#0D2B55"
azul_2  <- "#1B5E8A"
azul_3  <- "#4A90C4"
azul_4  <- "#7BB8D8"
azul_5  <- "#BDE0F0"
gris_f  <- "#F4F7FA"
gris_t  <- "#2C3E50"
gris_l  <- "#8FA0AE"
linea_p <- "#555555"

tema_aaa <- theme_minimal(base_family = "sans") +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = gris_f, colour = NA),
    panel.grid.major.y = element_line(colour = "#D0DCE8", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(size = 14, face = "bold", colour = gris_t,
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 10, colour = "#5C6E7A",
                                      margin = margin(b = 12)),
    plot.caption       = element_text(size = 8, colour = gris_l,
                                      hjust = 0, margin = margin(t = 10)),
    axis.text          = element_text(size = 9, colour = gris_t),
    axis.title         = element_text(size = 9, colour = gris_t),
    axis.title.x       = element_text(margin = margin(t = 8)),
    axis.title.y       = element_text(margin = margin(r = 8)),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9, colour = gris_t),
    legend.title       = element_blank(),
    legend.key.size    = unit(0.9, "lines"),
    strip.text         = element_text(size = 10, face = "bold", colour = gris_t),
    plot.margin        = margin(16, 20, 12, 16)
  )

# ── CARGA DE DATOS ─────────────────────────────────────────────────────
df <- read_excel("Ventas del mes de noviembre Virtual.xlsx",
                 sheet = "QPrincipal")

# Limpiar y calcular margen
df <- df %>%
  mutate(
    MARGEN_UNIT = EXTEND - COST * QTYSHIP,
    PCT_MARGEN  = MARGEN_UNIT / EXTEND * 100,
    DESCLINEA   = trimws(DESCLINEA),
    # Etiqueta corta por línea
    LINEA_CORTA = case_when(
      DESCLINEA == "FERRETERIA Y MISCELANEOS"   ~ "Ferretería y\nMisceláneos",
      DESCLINEA == "INSUMOS AGROPECUARIOS"       ~ "Insumos\nAgropecuarios",
      DESCLINEA == "CONCENTRADOS"                ~ "Concentrados",
      DESCLINEA == "COMBUSTIBLES Y LUBRICANTES"  ~ "Combustibles\ny Lubricantes",
      DESCLINEA == "CALZADO"                     ~ "Calzado",
      DESCLINEA == "VETERINARIOS"                ~ "Veterinarios",
      TRUE ~ DESCLINEA
    )
  )

# ── DECODIFICACIÓN DE SUBCATEGORÍAS ────────────────────────────────────
# Ferretería: subcategorías por CLASS
subcat_ferr <- function(cls) {
  case_when(
    cls == 101010 ~ "Construcción y materiales",
    cls == 105010 ~ "Pinturas y recubrimientos",
    cls == 100501 ~ "Transporte y limpieza hogar",
    cls == 101020 ~ "Seguridad y elementos personales",
    cls == 1010   ~ "Detergentes y adhesivos",
    cls == 106010 ~ "Desinfectantes y piscina",
    cls == 104010 ~ "Herramientas de limpieza",
    cls == 102010 ~ "Materiales eléctricos",
    cls == 103010 ~ "Herramientas agrícolas",
    TRUE          ~ "Otros"
  )
}

# Insumos: subcategorías por CLASS
subcat_ins <- function(cls) {
  case_when(
    cls == 10202020 ~ "Plaguicidas y herbicidas",
    cls == 101010   ~ "Fertilizantes",
    cls == 104010   ~ "Cal agrícola y fungicidas",
    cls == 103010   ~ "Nutrición foliar y reguladores",
    cls == 106010   ~ "Insecticidas",
    cls == 105010   ~ "Herbicidas sistémicos",
    cls == 102010   ~ "Minerales",
    cls == 107010   ~ "Adherentes",
    cls == 108010   ~ "Insumos varios",
    TRUE            ~ "Otros"
  )
}

df <- df %>%
  mutate(
    SUBCAT = case_when(
      DESCLINEA == "FERRETERIA Y MISCELANEOS" ~ subcat_ferr(CLASS),
      DESCLINEA == "INSUMOS AGROPECUARIOS"    ~ subcat_ins(CLASS),
      TRUE ~ NA_character_
    )
  )

# ══════════════════════════════════════════════════════════════════════
#  GRÁFICO 1 — Participación en ventas por línea de producto
#  "¿Cuál es el peso de cada área de negocio?"
# ══════════════════════════════════════════════════════════════════════
por_linea <- df %>%
  group_by(LINEA_CORTA) %>%
  summarise(
    ventas       = sum(EXTEND, na.rm = TRUE),
    costo        = sum(COST * QTYSHIP, na.rm = TRUE),
    items_unicos = n_distinct(ITEM),
    registros    = n(),
    .groups = "drop"
  ) %>%
  mutate(
    margen     = ventas - costo,
    pct_margen = margen / ventas * 100,
    pct_ventas = ventas / sum(ventas) * 100,
    LINEA_CORTA = fct_reorder(LINEA_CORTA, ventas)
  )

paleta_lineas <- c(
  "Ferretería y\nMisceláneos"   = azul_1,
  "Insumos\nAgropecuarios"      = azul_2,
  "Concentrados"                 = azul_3,
  "Combustibles\ny Lubricantes"  = azul_4,
  "Calzado"                      = azul_5,
  "Veterinarios"                 = gris_l
)

g1 <- ggplot(por_linea,
             aes(x = LINEA_CORTA, y = pct_ventas, fill = LINEA_CORTA)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct_ventas, 1), "%  ·  ", items_unicos, " ref.")),
            hjust = -0.08, size = 3.4, colour = gris_t, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = paleta_lineas) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 105),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Participación en Ventas por Línea de Producto — Noviembre 2025",
    subtitle = "Ingresos totales SAI: $474.8M · 1.792 referencias activas",
    x        = NULL,
    y        = "Participación en ventas (%)",
    caption  = "Fuente: SAI Open — Agrovariedades Triple A S.A.S | Etiquetas: participación % y número de referencias del catálogo"
  ) +
  tema_aaa +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#D0DCE8", linewidth = 0.4))


# ══════════════════════════════════════════════════════════════════════
#  GRÁFICO 3 — Subcategorías dentro de Ferretería y Misceláneos
#  "¿Qué vende Ferretería realmente?"
# ══════════════════════════════════════════════════════════════════════
ferr_sub <- df %>%
  filter(DESCLINEA == "FERRETERIA Y MISCELANEOS", !is.na(SUBCAT)) %>%
  group_by(SUBCAT) %>%
  summarise(
    ventas       = sum(EXTEND, na.rm = TRUE),
    items_unicos = n_distinct(ITEM),
    .groups = "drop"
  ) %>%
  mutate(
    pct        = ventas / sum(ventas) * 100,
    SUBCAT     = fct_reorder(SUBCAT, ventas),
    etiqueta   = paste0("$", round(ventas / 1e6, 1), "M  (", round(pct, 1), "%)")
  )

paleta_ferr <- colorRampPalette(c(azul_5, azul_3, azul_1))(nrow(ferr_sub))

g3 <- ggplot(ferr_sub,
             aes(x = SUBCAT, y = ventas / 1e6, fill = SUBCAT)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = etiqueta),
            hjust = -0.06, size = 2.9, colour = gris_t) +
  geom_text(aes(label = paste0(items_unicos, " ref.")),
            hjust = -0.06, vjust = 2.5, size = 2.5, colour = gris_l) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = setNames(paleta_ferr, levels(ferr_sub$SUBCAT))) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M", scale = 1/1e6,
                          big.mark = "."),
    limits = c(0, 430),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Ferretería y Misceláneos — Subcategorías",
    subtitle = "Total línea: $368.3M · 1.424 referencias · 6.361 ítems facturados",
    x        = NULL,
    y        = "Ventas (millones COP)",
    caption  = "Fuente: SAI Open · Subcategorías derivadas del código de clasificación (CLASS)"
  ) +
  tema_aaa +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#D0DCE8", linewidth = 0.4))

# ══════════════════════════════════════════════════════════════════════
#  GRÁFICO 4 — Subcategorías dentro de Insumos Agropecuarios
#  "¿Qué tipo de insumos mueve el área agro?"
# ══════════════════════════════════════════════════════════════════════
ins_sub <- df %>%
  filter(DESCLINEA == "INSUMOS AGROPECUARIOS", !is.na(SUBCAT)) %>%
  group_by(SUBCAT) %>%
  summarise(
    ventas       = sum(EXTEND, na.rm = TRUE),
    items_unicos = n_distinct(ITEM),
    .groups = "drop"
  ) %>%
  mutate(
    pct      = ventas / sum(ventas) * 100,
    SUBCAT   = fct_reorder(SUBCAT, ventas),
    etiqueta = paste0("$", round(ventas / 1e6, 1), "M  (", round(pct, 1), "%)")
  )

paleta_ins <- colorRampPalette(c(azul_5, azul_3, azul_1))(nrow(ins_sub))

g4 <- ggplot(ins_sub,
             aes(x = SUBCAT, y = ventas / 1e6, fill = SUBCAT)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = etiqueta),
            hjust = -0.06, size = 2.9, colour = gris_t) +
  geom_text(aes(label = paste0(items_unicos, " ref.")),
            hjust = -0.06, vjust = 2.5, size = 2.5, colour = gris_l) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = setNames(paleta_ins, levels(ins_sub$SUBCAT))) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M", scale = 1/1e6,
                          big.mark = "."),
    limits = c(0, 42),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Insumos Agropecuarios — Subcategorías",
    subtitle = "Total línea: $94.7M · 288 referencias · 1.912 ítems facturados",
    x        = NULL,
    y        = "Ventas (millones COP)",
    caption  = "Fuente: SAI Open · Subcategorías derivadas del código de clasificación (CLASS)"
  ) +
  tema_aaa +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#D0DCE8", linewidth = 0.4))

# ══════════════════════════════════════════════════════════════════════
#  GRÁFICO 5 — Top 15 productos más vendidos (por ingresos)
#  "¿Cuáles son los productos estrella?"
# ══════════════════════════════════════════════════════════════════════
top15 <- df %>%
  group_by(DESCRIPCION, DESCLINEA) %>%
  summarise(
    ventas    = sum(EXTEND, na.rm = TRUE),
    unidades  = sum(QTYSHIP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(ventas, n = 15) %>%
  mutate(
    DESCRIPCION = str_wrap(DESCRIPCION, 30),
    DESCRIPCION = fct_reorder(DESCRIPCION, ventas),
    linea_corta = case_when(
      DESCLINEA == "FERRETERIA Y MISCELANEOS" ~ "Ferretería",
      DESCLINEA == "INSUMOS AGROPECUARIOS"    ~ "Insumos Agro",
      TRUE ~ DESCLINEA
    )
  )

g5 <- ggplot(top15,
             aes(x = DESCRIPCION, y = ventas / 1e6,
                 fill = linea_corta)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0("$", round(ventas / 1e6, 1), "M")),
            hjust = -0.1, size = 2.9, colour = gris_t, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c("Ferretería" = azul_2, "Insumos Agro" = azul_4)
  ) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M", scale = 1/1e6,
                          big.mark = "."),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Top 15 Productos por Ventas — Noviembre 2025",
    subtitle = "El cemento gris lidera con $23.4M · 2° lugar: Transporte ($8.2M)",
    x        = NULL,
    y        = "Ventas (millones COP)",
    caption  = "Fuente: SAI Open · El servicio de Transporte aparece registrado como ítem CLASS 100501 en Ferretería"
  ) +
  tema_aaa +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#D0DCE8", linewidth = 0.4))

# ══════════════════════════════════════════════════════════════════════
#  GRÁFICO 7 — Distribución diaria de facturas emitidas (actividad)
#  "¿Cómo es el ritmo de operación del negocio?"
# ══════════════════════════════════════════════════════════════════════
por_dia <- df %>%
  mutate(DIA = as.integer(format(as.Date(FECHA), "%d"))) %>%
  group_by(DIA) %>%
  summarise(
    facturas = n_distinct(NUMBER),
    ventas   = sum(EXTEND, na.rm = TRUE),
    .groups  = "drop"
  )


# ══════════════════════════════════════════════════════════════════════
#  EXPORTAR todos los gráficos
# ══════════════════════════════════════════════════════════════════════

ggsave("g1_ventas_por_linea.png",      g1, width = 10, height = 5.5, dpi = 160)
ggsave("g3_subcategorias_ferr.png",    g3, width = 11, height = 5.5, dpi = 160)
ggsave("g4_subcategorias_ins.png",     g4, width = 11, height = 5.5, dpi = 160)
ggsave("g5_top15_productos.png",       g5, width = 11, height = 7,   dpi = 160)