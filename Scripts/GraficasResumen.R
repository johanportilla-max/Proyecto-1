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
library(patchwork)

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
    panel.grid.major.y = element_line(colour = "#D0DCE8", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(size = 18, face = "bold", colour = gris_t,
                                      margin = margin(b = 5)),
    plot.subtitle      = element_text(size = 13, colour = "#5C6E7A",
                                      margin = margin(b = 14)),
    plot.caption       = element_text(size = 10, colour = gris_l,
                                      hjust = 0, margin = margin(t = 12)),
    axis.text          = element_text(size = 12, colour = gris_t),
    axis.title         = element_text(size = 12, colour = gris_t),
    axis.title.x       = element_text(margin = margin(t = 10)),
    axis.title.y       = element_text(margin = margin(r = 10)),
    legend.position    = "bottom",
    legend.text        = element_text(size = 11, colour = gris_t),
    legend.title       = element_blank(),
    legend.key.size    = unit(1.1, "lines"),
    strip.text         = element_text(size = 13, face = "bold", colour = gris_t),
    plot.margin        = margin(20, 24, 14, 20)
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
            hjust = -0.08, size = 5.5, colour = gris_t, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = paleta_lineas) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 108),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Participación en Ventas por Línea de Producto — Noviembre 2025",
    subtitle = "1.792 referencias activas · Fuente: SAI Open — Agrovariedades Triple A S.A.S",
    x        = NULL,
    y        = "Participación en ventas (%)"
  ) +
  tema_aaa +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#D0DCE8", linewidth = 0.4),
    plot.caption       = element_text(size = 13, colour = gris_l,
                                      hjust = 0, margin = margin(t = 14))
  )

ggsave("g1_ventas_por_linea.png",   g1, width = 13, height = 7,   dpi = 200)


# ── DECODIFICACIÓN FERRETERÍA (con DESCRIPCION para separar Transporte) ─
subcat_ferr <- function(cls, desc) {
  case_when(
    str_detect(desc, "TRANSPORTE")     ~ "Transporte",
    cls == 101010                       ~ "Construcción y materiales",
    cls == 105010                       ~ "Pinturas y recubrimientos",
    cls == 100501                       ~ "Limpieza del hogar",
    cls == 101020                       ~ "Seguridad y elementos personales",
    cls == 1010                         ~ "Detergentes y adhesivos",
    cls == 106010                       ~ "Desinfectantes y piscina",
    cls == 104010                       ~ "Herramientas de limpieza",
    cls == 102010                       ~ "Materiales eléctricos",
    cls == 103010                       ~ "Herramientas agrícolas",
    TRUE                                ~ "Otros"
  )
}

df <- df %>%
  mutate(
    SUBCAT = case_when(
      DESCLINEA == "FERRETERIA Y MISCELANEOS" ~ subcat_ferr(CLASS, DESCRIPCION),
      DESCLINEA == "INSUMOS AGROPECUARIOS"    ~ subcat_ins(CLASS),
      TRUE ~ NA_character_
    )
  )

# ── DATOS — top 6 ─────────────────────────────────────────────────────
ferr_sub <- df %>%
  filter(DESCLINEA == "FERRETERIA Y MISCELANEOS", !is.na(SUBCAT)) %>%
  group_by(SUBCAT) %>%
  summarise(ventas       = sum(EXTEND, na.rm = TRUE),
            items_unicos = n_distinct(ITEM), .groups = "drop") %>%
  mutate(pct = ventas / sum(ventas) * 100) %>%
  slice_max(ventas, n = 6) %>%
  mutate(SUBCAT = fct_reorder(SUBCAT, ventas))

ins_sub <- df %>%
  filter(DESCLINEA == "INSUMOS AGROPECUARIOS", !is.na(SUBCAT)) %>%
  group_by(SUBCAT) %>%
  summarise(ventas       = sum(EXTEND, na.rm = TRUE),
            items_unicos = n_distinct(ITEM), .groups = "drop") %>%
  mutate(pct = ventas / sum(ventas) * 100) %>%
  slice_max(ventas, n = 6) %>%
  mutate(SUBCAT = fct_reorder(SUBCAT, ventas))

paleta_ferr <- colorRampPalette(c(azul_4, azul_1))(6)
paleta_ins  <- colorRampPalette(c(azul_4, azul_1))(6)

# tema reducido para panel combinado (títulos más pequeños, más margen)
tema_panel <- tema_aaa +
  theme(
    plot.title         = element_text(size = 15, face = "bold", colour = gris_t,
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 11, colour = "#5C6E7A",
                                      margin = margin(b = 10)),
    plot.caption       = element_text(size = 10, colour = gris_l,
                                      hjust = 0, margin = margin(t = 10)),
    axis.text          = element_text(size = 11, colour = gris_t),
    axis.title         = element_text(size = 11, colour = gris_t),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#D0DCE8", linewidth = 0.4),
    plot.margin        = margin(16, 28, 12, 16)
  )

g3A <- ggplot(ferr_sub, aes(x = SUBCAT, y = pct, fill = SUBCAT)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%  ·  ", items_unicos, " ref.")),
            hjust = -0.08, size = 4.8, colour = gris_t, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = setNames(paleta_ferr, levels(ferr_sub$SUBCAT))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 130), expand = c(0, 0)) +
  labs(title    = "Ferretería y Misceláneos",
       subtitle = "Top 6 subcategorías · 1.424 referencias",
       x = NULL, y = "Participación dentro de la línea (%)") +
  tema_panel

g4A <- ggplot(ins_sub, aes(x = SUBCAT, y = pct, fill = SUBCAT)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%  ·  ", items_unicos, " ref.")),
            hjust = -0.08, size = 4.8, colour = gris_t, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = setNames(paleta_ins, levels(ins_sub$SUBCAT))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 52), expand = c(0, 0)) +
  labs(title    = "Insumos Agropecuarios",
       subtitle = "Top 6 subcategorías · 288 referencias",
       x = NULL, y = "Participación dentro de la línea (%)") +
  tema_panel

# combinar y exportar como UNA sola imagen
panel_subcategorias <- g3A + g4A +
  plot_annotation(
    title   = "Subcategorías por Línea de Producto — Noviembre 2025",
    caption = "Fuente: SAI Open — Agrovariedades Triple A S.A.S · Subcategorías derivadas del código CLASS",
    theme   = theme(
      plot.title   = element_text(size = 18, face = "bold", colour = gris_t,
                                  margin = margin(b = 6)),
      plot.caption = element_text(size = 15, colour = gris_l, hjust = 0)
    )
  )

ggsave("g34_subcategorias_panel.png",
       panel_subcategorias,
       width = 22, height = 8, dpi = 200)

# ── EXPORTAR ───────────────────────────────────────────────────────────
ggsave("g3A_subcategorias_ferr.png", g3A, width = 12, height = 7.5, dpi = 200)
ggsave("g4A_subcategorias_ins.png",  g4A, width = 12, height = 7.5, dpi = 200)


# ── DATOS — top 10 productos ───────────────────────────────────────────
top10 <- df %>%
  group_by(DESCRIPCION, DESCLINEA) %>%
  summarise(
    ventas   = sum(EXTEND, na.rm = TRUE),
    unidades = sum(QTYSHIP, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  slice_max(ventas, n = 10) %>%
  mutate(
    pct         = ventas / sum(df$EXTEND, na.rm = TRUE) * 100,
    DESCRIPCION = str_to_title(str_wrap(DESCRIPCION, 28)),
    DESCRIPCION = fct_reorder(DESCRIPCION, ventas),
    linea_corta = case_when(
      DESCLINEA == "FERRETERIA Y MISCELANEOS" ~ "Ferretería",
      DESCLINEA == "INSUMOS AGROPECUARIOS"    ~ "Insumos Agro",
      TRUE ~ DESCLINEA
    ),
    etiqueta = paste0(round(pct, 1), "%  ·  ",
                      format(round(unidades), big.mark = ".", decimal.mark = ","),
                      " uds.")
  )

# ── G5 — Top 10 ────────────────────────────────────────────────────────
g5 <- ggplot(top10,
             aes(x = DESCRIPCION, y = pct, fill = linea_corta)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = etiqueta),
            hjust = -0.08, size = 5.0, colour = gris_t, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c("Ferretería" = azul_2, "Insumos Agro" = azul_4)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 10),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Top 10 Productos más Vendidos — Noviembre 2025",
    subtitle = "Participación sobre el total de ventas SAI · color por línea de producto",
    x        = NULL,
    y        = "Participación en ventas totales (%)",
    caption  = "Fuente: SAI Open — Agrovariedades Triple A S.A.S · Etiquetas: % participación y unidades vendidas"
  ) +
  tema_aaa +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#D0DCE8", linewidth = 0.4))

# ── EXPORTAR ───────────────────────────────────────────────────────────
ggsave("g5_top10_productos.png", g5, width = 13, height = 8, dpi = 200)



# ─────────────────────────────────────────────────────────────






