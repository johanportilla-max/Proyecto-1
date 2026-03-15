# ══════════════════════════════════════════════════════════════════════
#  GRÁFICOS PRESENTACIÓN — Agrovariedades Triple A S.A.S
#  Fuente: Cuadres de caja diarios 2025
#  Proyecto en Ingeniería I · 2026-I · Universidad del Valle
# ══════════════════════════════════════════════════════════════════════

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(forcats)
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

tema_aaa <- theme_minimal(base_family = "sans") +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = gris_f, colour = NA),
    panel.grid.major.y = element_line(colour = "#D0DCE8", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(size = 18, face = "bold", colour = gris_t,
                                      margin = margin(b = 5)),
    plot.subtitle      = element_text(size = 1, colour = "#5C6E7A",
                                      margin = margin(b = 14)),
    plot.caption       = element_text(size = 15, colour = gris_l,
                                      hjust = 0, margin = margin(t = 12)),
    axis.text          = element_text(size = 15, colour = gris_t),
    axis.title         = element_text(size = 15, colour = gris_t),
    axis.title.x       = element_text(margin = margin(t = 10)),
    axis.title.y       = element_text(margin = margin(r = 10)),
    legend.position    = "bottom",
    legend.text        = element_text(size = 11, colour = gris_t),
    legend.title       = element_blank(),
    legend.key.size    = unit(1.1, "lines"),
    plot.margin        = margin(20, 24, 14, 20)
  )

# ── CARGA Y LIMPIEZA ───────────────────────────────────────────────────
raw <- read_excel("CUADRE CAJA 2025 RESUMEN .xlsx", sheet = "Hoja1")

colnames(raw) <- c("FECHA","DIA","AGRO_SAI","FACT_AGRO","TARJETA","FACT_TARJETA",
                   "TRANSFERENCIA","FACT_TRANSF","FERR_SAI","FACT_FERR",
                   "FERR_OTROS","FACT_OTROS","GASTOS","VENTAS_SAI","FACT_SAI_TOT",
                   "VENTAS_REALES","FACT_REALES","PCT_FUERA_FACT","PCT_FUERA_MONTO","DROP")

df <- raw %>%
  select(-DROP) %>%
  mutate(across(-FECHA, as.numeric),
         FECHA = as.Date(FECHA)) %>%
  filter(VENTAS_REALES > 0)

meses_etiq <- c("Ene","Feb","Mar","Abr","May","Jun",
                "Jul","Ago","Sep","Oct","Nov","Dic")

# ── DATOS MENSUALES ────────────────────────────────────────────────────
mensual <- df %>%
  mutate(MES = as.integer(format(FECHA, "%m"))) %>%
  group_by(MES) %>%
  summarise(
    ventas_sai    = sum(VENTAS_SAI,     na.rm = TRUE),
    ventas_reales = sum(VENTAS_REALES,  na.rm = TRUE),
    ferr_otros    = sum(FERR_OTROS,     na.rm = TRUE),
    agro_sai      = sum(AGRO_SAI,       na.rm = TRUE),
    ferr_sai      = sum(FERR_SAI,       na.rm = TRUE),
    tarjeta       = sum(TARJETA,        na.rm = TRUE),
    transferencia = sum(TRANSFERENCIA,  na.rm = TRUE),
    gastos        = sum(GASTOS,         na.rm = TRUE),
    fact_sai      = sum(FACT_SAI_TOT,   na.rm = TRUE),
    fact_reales   = sum(FACT_REALES,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # ferr_otros es efectivo tardío → se suma al efectivo real
    efectivo  = ventas_sai - tarjeta - transferencia + ferr_otros,
    pct_fuera = ferr_otros / ventas_reales * 100,
    mes_etiq  = factor(meses_etiq[MES], levels = meses_etiq)
  )

# ── DATOS NOVIEMBRE ────────────────────────────────────────────────────
nov <- df %>%
  filter(format(FECHA, "%m") == "11") %>%
  mutate(
    DIA           = as.integer(format(FECHA, "%d")),
    efectivo      = VENTAS_SAI - TARJETA - TRANSFERENCIA,
    pct_fuera_dia = FERR_OTROS / VENTAS_REALES * 100
  )


# ══════════════════════════════════════════════════════════════════════
#  A1 — Ventas mensuales: SAI vs. Fuera del sistema + total en la cima
#  (versión mejorada de la que ya tienes — con el total arriba de la barra)
# ══════════════════════════════════════════════════════════════════════
a1_long <- mensual %>%
  select(mes_etiq, ventas_sai, ferr_otros, ventas_reales) %>%
  pivot_longer(c(ventas_sai, ferr_otros),
               names_to  = "tipo",
               values_to = "valor") %>%
  mutate(tipo = recode(tipo,
                       ventas_sai  = "Registradas en el momento (SAI)",
                       ferr_otros  = "Digitalizadas al final del día"))

a1 <- ggplot(a1_long, aes(x = mes_etiq, y = valor / 1e6, fill = tipo)) +
  geom_col(width = 0.72) +
  # total encima de cada barra
  geom_text(
    data = mensual,
    aes(x = mes_etiq, y = ventas_reales / 1e6,
        label = paste0("$", round(ventas_reales / 1e6, 0), "M")),
    inherit.aes = FALSE,
    vjust = -0.5, size = 3.8, colour = gris_t, fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Registradas en el momento (SAI)" = azul_1,
    "Digitalizadas al final del día"  = azul_4
  )) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M", scale = 1/1e6,
                          big.mark = ".", decimal.mark = ","),
    limits = c(0, 820), expand = c(0, 0)
  ) +
  labs(
    title    = "Ventas Mensuales 2025 — Registro inmediato vs. Digitalización tardía",
    subtitle = "La zona azul claro representa ventas digitadas al cierre del día, no en el momento de la venta",
    x        = NULL,
    y        = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa

ggsave("A1_ventas_mensual_sai_vs_real.png", a1, width = 14, height = 7, dpi = 200)

# ══════════════════════════════════════════════════════════════════════
#  A2 — % mensual fuera del sistema (monto)
# ══════════════════════════════════════════════════════════════════════
prom_anual <- mean(mensual$pct_fuera)

a2 <- ggplot(mensual, aes(x = mes_etiq, y = pct_fuera)) +
  geom_col(fill = azul_3, width = 0.68) +
  geom_hline(yintercept = prom_anual, linetype = "dashed",
             colour = gris_t, linewidth = 0.8) +
  annotate("text", x = 1.4, y = prom_anual + 0.8,
           label = paste0("Promedio anual: ", round(prom_anual, 1), "%"),
           size = 4.2, colour = gris_t, fontface = "bold", hjust = 0) +
  geom_text(aes(label = paste0(round(pct_fuera, 1), "%")),
            vjust = -0.5, size = 4.0, colour = gris_t, fontface = "bold") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 32), expand = c(0, 0)
  ) +
  labs(
    title    = "% Mensual de Ventas Fuera del Sistema — 2025",
    subtitle = "Proporción del monto total del mes que no se registra en SAI en el momento de la venta",
    x        = NULL,
    y        = "% del total de ventas del mes",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA\nFórmula: FERR. OTROS / Ventas reales totales del mes"
  ) +
  tema_aaa

ggsave("A2_pct_fuera_mensual.png", a2, width = 14, height = 7, dpi = 200)

# ══════════════════════════════════════════════════════════════════════
#  A3 — Ventas mensuales Agro vs. Ferretería SAI
# ══════════════════════════════════════════════════════════════════════
a3_long <- mensual %>%
  select(mes_etiq, agro_sai, ferr_sai) %>%
  pivot_longer(c(agro_sai, ferr_sai),
               names_to  = "canal",
               values_to = "valor") %>%
  mutate(canal = recode(canal,
                        agro_sai = "Agro (SAI)",
                        ferr_sai = "Ferretería (SAI)"))

a3 <- ggplot(a3_long, aes(x = mes_etiq, y = valor / 1e6, fill = canal)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Agro (SAI)" = azul_1,
                               "Ferretería (SAI)" = azul_4)) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M", scale = 1/1e6,
                          big.mark = ".", decimal.mark = ","),
    limits = c(0, 230), expand = c(0, 0)
  ) +
  labs(
    title    = "Ventas Mensuales por Canal — Agro vs. Ferretería 2025",
    subtitle = "Ventas registradas en SAI (efectivo directo). Ferretería no incluye las digitalizadas al cierre.",
    x        = NULL,
    y        = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa

ggsave("A3_agro_vs_ferreteria_mensual.png", a3, width = 14, height = 7, dpi = 200)

# ══════════════════════════════════════════════════════════════════════
#  A4 — Ventas reales vs. Gastos operativos mensuales
# ══════════════════════════════════════════════════════════════════════
a4_long <- mensual %>%
  select(mes_etiq, ventas_reales, gastos) %>%
  pivot_longer(c(ventas_reales, gastos),
               names_to  = "tipo",
               values_to = "valor") %>%
  mutate(tipo = recode(tipo,
                       ventas_reales = "Ventas reales totales",
                       gastos        = "Gastos operativos"))

a4 <- ggplot(a4_long, aes(x = mes_etiq, y = valor / 1e6,
                          colour = tipo, group = tipo)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3.2) +
  scale_colour_manual(values = c("Ventas reales totales" = azul_1,
                                 "Gastos operativos"     = azul_4)) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M", scale = 1/1e6,
                          big.mark = ".", decimal.mark = ","),
    expand = c(0.05, 0)
  ) +
  labs(
    title    = "Ventas Reales vs. Gastos Operativos por Mes — 2025",
    subtitle = "Comparación entre ingresos reales del negocio y egresos mensuales",
    x        = NULL,
    y        = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa +
  theme(panel.grid.major.x = element_line(colour = "#D0DCE8", linewidth = 0.3))

ggsave("A4_ventas_vs_gastos_mensual.png", a4, width = 14, height = 7, dpi = 200)

# ══════════════════════════════════════════════════════════════════════
#  A5 — Ingresos mensuales por medio de pago (stacked + % interno)
# ══════════════════════════════════════════════════════════════════════

# ── A5 ─────────────────────────────────────────────────────────────────
pago_long <- mensual %>%
  mutate(
    pct_ef = efectivo      / ventas_reales * 100,
    pct_tj = tarjeta       / ventas_reales * 100,
    pct_tr = transferencia / ventas_reales * 100
  ) %>%
  select(mes_etiq, efectivo, tarjeta, transferencia,
         pct_ef, pct_tj, pct_tr, ventas_reales) %>%
  pivot_longer(c(efectivo, tarjeta, transferencia),
               names_to  = "medio",
               values_to = "valor") %>%
  mutate(
    medio = factor(recode(medio,
                          efectivo      = "Efectivo",
                          tarjeta       = "Tarjeta",
                          transferencia = "Transferencia"),
                   levels = c("Transferencia","Tarjeta","Efectivo")),
    pct_label = case_when(
      medio == "Efectivo"      ~ paste0(round(pct_ef, 0), "%"),
      medio == "Tarjeta"       ~ paste0(round(pct_tj, 0), "%"),
      medio == "Transferencia" ~ paste0(round(pct_tr, 0), "%")
    )
  )

a5 <- ggplot(pago_long, aes(x = mes_etiq, y = valor / 1e6, fill = medio)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = pct_label),
            position = position_stack(vjust = 0.5),
            size = 5.0, colour = "white", fontface = "bold") +
  geom_text(
    data        = mensual,
    aes(x       = mes_etiq,
        y       = ventas_reales / 1e6,
        label   = paste0("$", round(ventas_reales / 1e6, 0), "M")),
    inherit.aes = FALSE,
    vjust       = -0.45,
    size        = 5.2,
    colour      = gris_t,
    fontface    = "bold"
  ) +
  scale_fill_manual(values = c(
    "Efectivo"      = azul_1,
    "Tarjeta"       = azul_3,
    "Transferencia" = azul_5
  )) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M",
                          big.mark = ".", decimal.mark = ","),
    limits = c(0, 850),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Ingresos Mensuales por Medio de Pago — 2025",
    subtitle = "El porcentaje dentro de cada segmento indica su participación en el total del mes",
    x        = NULL,
    y        = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA · Efectivo incluye ventas digitalizadas al cierre del día"
  ) +
  tema_aaa +
  theme(legend.text     = element_text(size = 15),
        legend.key.size = unit(1.3, "lines"))

ggsave("A5_medios_pago_mensual.png", a5, width = 15, height = 8, dpi = 200)
# ══════════════════════════════════════════════════════════════════════
#  N1 — Noviembre: ventas diarias SAI vs. Fuera del sistema + total
# ══════════════════════════════════════════════════════════════════════
n1_long <- nov %>%
  select(DIA, VENTAS_SAI, FERR_OTROS, VENTAS_REALES) %>%
  pivot_longer(c(VENTAS_SAI, FERR_OTROS),
               names_to  = "tipo",
               values_to = "valor") %>%
  mutate(tipo = recode(tipo,
                       VENTAS_SAI  = "Registradas en el momento (SAI)",
                       FERR_OTROS  = "Digitalizadas al final del día"))

n1 <- ggplot(n1_long, aes(x = DIA, y = valor / 1e6, fill = tipo)) +
  geom_col(width = 0.8) +
  geom_text(
    data = nov,
    aes(x = DIA, y = VENTAS_REALES / 1e6,
        label = paste0("$", round(VENTAS_REALES / 1e6, 1), "M")),
    inherit.aes = FALSE,
    vjust = -0.45, size = 2.8, colour = gris_t, fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Registradas en el momento (SAI)" = azul_1,
    "Digitalizadas al final del día"  = azul_4
  )) +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M", scale = 1/1e6,
                          big.mark = ".", decimal.mark = ","),
    limits = c(0, 52), expand = c(0, 0)
  ) +
  labs(
    title    = "Ventas Diarias — Registro inmediato vs. Digitalización tardía · Noviembre 2025",
    subtitle = "La zona azul claro es el monto diario digitado al cierre del día · Etiquetas: venta real total del día",
    x        = "Día de Noviembre",
    y        = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa

ggsave("N1_nov_ventas_sai_vs_real.png", n1, width = 16, height = 7, dpi = 200)

# ══════════════════════════════════════════════════════════════════════
#  N2 — Noviembre: % diario fuera del sistema con promedio
# ══════════════════════════════════════════════════════════════════════
prom_nov <- mean(nov$pct_fuera_dia, na.rm = TRUE)

n2 <- ggplot(nov, aes(x = DIA, y = pct_fuera_dia)) +
  geom_col(fill = azul_3, width = 0.78) +
  geom_hline(yintercept = prom_nov, linetype = "dashed",
             colour = gris_t, linewidth = 0.8) +
  annotate("text", x = 2, y = prom_nov + 1.5,
           label = paste0("Promedio nov.: ", round(prom_nov, 1), "%"),
           size = 4.2, colour = gris_t, fontface = "bold", hjust = 0) +
  geom_text(aes(label = paste0(round(pct_fuera_dia, 0), "%")),
            vjust = -0.5, size = 3.2, colour = gris_t, fontface = "bold") +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 42), expand = c(0, 0)
  ) +
  labs(
    title    = "% Diario de Ventas Fuera del Sistema — Noviembre 2025",
    subtitle = "Proporción del monto del día que no se registra en SAI en el momento de la venta",
    x        = "Día de Noviembre",
    y        = "% del total de ventas del día",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA\nFórmula: FERR. OTROS / Ventas reales del día"
  ) +
  tema_aaa

ggsave("N2_nov_pct_fuera_diario.png", n2, width = 16, height = 7, dpi = 200)

# ══════════════════════════════════════════════════════════════════════
#  N3 — Noviembre: Facturas diarias SAI vs. tardías (barras apiladas)
# ══════════════════════════════════════════════════════════════════════
nov_fact <- nov %>%
  mutate(
    fact_tarde    = FACT_OTROS,
    fact_sai      = FACT_SAI_TOT
  )

n3_long <- nov_fact %>%
  select(DIA, fact_sai, fact_tarde) %>%
  pivot_longer(c(fact_sai, fact_tarde),
               names_to = "tipo", values_to = "valor") %>%
  mutate(tipo = recode(tipo,
                       fact_sai   = "Registradas en el momento (SAI)",
                       fact_tarde = "Digitalizadas al final del día"))

n3 <- ggplot(n3_long, aes(x = DIA, y = valor, fill = tipo)) +
  geom_col(width = 0.8) +
  geom_text(
    data = nov_fact,
    aes(x = DIA, y = fact_sai + fact_tarde,
        label = fact_tarde),
    inherit.aes = FALSE,
    vjust = -0.4, size = 3.2, colour = azul_2, fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Registradas en el momento (SAI)" = azul_1,
    "Digitalizadas al final del día"  = azul_4
  )) +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(limits = c(0, 230), expand = c(0, 0)) +
  labs(
    title    = "Facturas Diarias — Registro inmediato vs. Digitalización tardía · Noviembre 2025",
    subtitle = "El número sobre cada barra indica las facturas digitalizadas al cierre del día",
    x        = "Día de Noviembre",
    y        = "Cantidad de facturas",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa

ggsave("N3_nov_facturas_apiladas.png", n3, width = 16, height = 7, dpi = 200)

# ══════════════════════════════════════════════════════════════════════
#  N4 — Noviembre: Ingresos diarios por medio de pago
# ══════════════════════════════════════════════════════════════════════
nov_pago <- nov %>%
  mutate(
    efectivo_nov = VENTAS_SAI - TARJETA - TRANSFERENCIA,
    pct_ef       = efectivo_nov / VENTAS_REALES * 100,
    pct_tj       = TARJETA      / VENTAS_REALES * 100,
    pct_tr       = TRANSFERENCIA/ VENTAS_REALES * 100
  ) %>%
  select(DIA, efectivo_nov, TARJETA, TRANSFERENCIA,
         pct_ef, pct_tj, pct_tr, VENTAS_REALES) %>%
  pivot_longer(c(efectivo_nov, TARJETA, TRANSFERENCIA),
               names_to = "medio", values_to = "valor") %>%
  mutate(
    medio = factor(recode(medio,
                          efectivo_nov  = "Efectivo",
                          TARJETA       = "Tarjeta",
                          TRANSFERENCIA = "Transferencia"),
                   levels = c("Efectivo","Tarjeta","Transferencia")),
    pct_label = case_when(
      medio == "Efectivo"      ~ paste0(round(pct_ef, 0), "%"),
      medio == "Tarjeta"       ~ paste0(round(pct_tj, 0), "%"),
      medio == "Transferencia" ~ paste0(round(pct_tr, 0), "%")
    )
  )

nov_totales <- nov %>%
  mutate(efectivo_nov = VENTAS_SAI - TARJETA - TRANSFERENCIA)

n4 <- ggplot(nov_pago, aes(x = DIA, y = valor / 1e6, fill = medio)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = pct_label),
            position = position_stack(vjust = 0.5),
            size = 2.8, colour = "white", fontface = "bold") +
  geom_text(
    data = nov_totales,
    aes(x = DIA, y = VENTAS_REALES / 1e6,
        label = paste0("$", round(VENTAS_REALES / 1e6, 1), "M")),
    inherit.aes = FALSE,
    vjust = -0.45, size = 2.6, colour = gris_t, fontface = "bold"
  ) +
  scale_fill_manual(values = c("Efectivo"      = azul_1,
                               "Tarjeta"       = azul_3,
                               "Transferencia" = azul_5)) +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M", scale = 1/1e6,
                          big.mark = ".", decimal.mark = ","),
    limits = c(0, 52), expand = c(0, 0)
  ) +
  labs(
    title    = "Ingresos Diarios por Medio de Pago — Noviembre 2025",
    subtitle = "El porcentaje dentro de cada segmento indica su participación en el total del día",
    x        = "Día de Noviembre",
    y        = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa

ggsave("N4_nov_medios_pago_diario.png", n4, width = 16, height = 7.5, dpi = 200)
