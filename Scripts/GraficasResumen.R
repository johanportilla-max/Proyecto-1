library(readxl)
library(tidyverse)
library(scales)
library(lubridate)
library(glue)

ARCHIVO        <- "CUADRE CAJA 2025 RESUMEN .xlsx"
CARPETA_SALIDA <- "."
# TEMA 

azul_1  <- "#0D2B55"   # se mantiene
azul_2  <- "#1B5E8A"   # se mantiene
azul_3  <- "#3A7AB0"   # ligeramente más oscuro que el original (#4A90C4)
azul_4  <- "#5A9BC0"   # más oscuro que el original (#7BB8D8)
azul_5  <- "#8FC5E0"   # más oscuro que el original (#BDE0F0)
gris_f  <- "#F4F7FA"   # se mantiene
gris_t  <- "#2C3E50"   # se mantiene
gris_l  <- "#8FA0AE"   # se mantiene
linea_p <- "#555555"   # se mantiene

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

meses_es <- c("Ene","Feb","Mar","Abr","May","Jun",
              "Jul","Ago","Sep","Oct","Nov","Dic")

# Columnas de la base:
#  C  agro_sai        Ventas Agro registradas en SAI (efectivo)
#  D  fact_agro       Número de facturas Agro SAI
#  E  tarjeta         Pagos con tarjeta (todo el negocio)
#  F  fact_tarjeta    Número de transacciones tarjeta
#  G  transferencia   Pagos por transferencia (todo el negocio)
#  H  fact_transf     Número de transacciones transferencia
#  I  ferr_sai        Ventas Ferretería registradas en SAI (efectivo)
#  J  fact_ferr       Número de facturas Ferretería SAI
#  K  ferr_otros      Ventas Ferretería FUERA del sistema ← Digitalizadas al final del día
#  L  fact_fuera      Número de facturas fuera del sistema ← Indicador del problema
#  M  gastos          Total gastos del día
#  N  ventas_sai      Ventas totales SAI (C+E+G+I)
#  O  fact_sai_total  Facturas totales SAI (D+F+H+J)
#  P  ventas_reales   Ventas reales totales (N+K)
#  Q  fact_total_real Facturas totales reales (O+L)  ← denominador correcto
#  R  pct_fuera_fact  % facturas fuera — NOTA: en el Excel = L/O (denominador incorrecto)
#                     Aquí lo recalculamos como L/Q (correcto)
#  S  pct_fuera_monto % monto fuera = K/P

raw <- read_excel(ARCHIVO, sheet = 1, col_names = TRUE) %>%
  rename(
    fecha          = 1,
    dia            = 2,
    agro_sai       = 3,
    fact_agro      = 4,
    tarjeta        = 5,
    fact_tarjeta   = 6,
    transferencia  = 7,
    fact_transf    = 8,
    ferr_sai       = 9,
    fact_ferr      = 10,
    ferr_otros     = 11,   # ← facturas digitalizadas al final del día (monto)
    fact_fuera     = 12,   # ← facturas digitalizadas al final del día (cantidad)
    gastos         = 13,
    ventas_sai     = 14,
    fact_sai_total = 15,
    ventas_reales  = 16,
    fact_total_real = 17,
    pct_fuera_fact_excel = 18,   # guardamos el del Excel para comparar
    pct_fuera_monto      = 19
  ) %>%
  mutate(
    across(c(agro_sai, fact_agro, tarjeta, fact_tarjeta, transferencia, fact_transf,
             ferr_sai, fact_ferr, ferr_otros, fact_fuera, gastos,
             ventas_sai, fact_sai_total, ventas_reales, fact_total_real,
             pct_fuera_monto), as.numeric),
    fecha           = as.Date(fecha),
    mes             = month(fecha),
    mes_label       = factor(meses_es[mes], levels = meses_es),
    # ── % CORRECTO: facturas digitalizadas tarde / total facturas reales ──
    pct_fuera_fact  = ifelse(fact_total_real > 0,
                             fact_fuera / fact_total_real, 0),
    ventas_fuera    = ventas_reales - ventas_sai
  ) %>%
  filter(!is.na(fecha))
View(raw)

# Resumen mensual
mensual <- raw %>%
  group_by(mes, mes_label) %>%
  summarise(
    ventas_sai      = sum(ventas_sai,      na.rm = TRUE),
    ventas_reales   = sum(ventas_reales,   na.rm = TRUE),
    ventas_fuera    = sum(ventas_fuera,    na.rm = TRUE),
    agro_sai        = sum(agro_sai,        na.rm = TRUE),
    ferr_sai        = sum(ferr_sai,        na.rm = TRUE),
    ferr_otros      = sum(ferr_otros,      na.rm = TRUE),
    tarjeta         = sum(tarjeta,         na.rm = TRUE),
    transferencia   = sum(transferencia,   na.rm = TRUE),
    gastos          = sum(gastos,          na.rm = TRUE),
    fact_sai        = sum(fact_sai_total,  na.rm = TRUE),
    fact_fuera      = sum(fact_fuera,      na.rm = TRUE),
    fact_total      = sum(fact_total_real, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_fuera_fact = ifelse(fact_total > 0, fact_fuera / fact_total, 0),
    pct_fuera_monto = ifelse(ventas_reales > 0, ventas_fuera / ventas_reales, 0)
  )

noviembre <- raw %>% filter(mes == 11)

prom_anual <- mean(raw$pct_fuera_fact, na.rm = TRUE)
prom_nov   <- mean(noviembre$pct_fuera_fact, na.rm = TRUE)


# A1 — Ventas mensuales: registradas al momento vs. digitalizadas tarde

g_a1 <- mensual %>%
  select(mes_label, ventas_sai, ventas_fuera) %>%
  pivot_longer(-mes_label, names_to = "tipo", values_to = "valor") %>%
  mutate(tipo = factor(tipo,
                       levels = c("ventas_fuera", "ventas_sai"),
                       labels = c("Digitalizadas al final del día",
                                  "Registradas en el momento (SAI)"))) %>%
  ggplot(aes(x = mes_label, y = valor / 1e6, fill = tipo)) +
  geom_col(position = "stack", width = 0.72) +
  scale_fill_manual(values = c("Registradas en el momento (SAI)"  = azul_1,
                               "Digitalizadas al final del día"    = azul_4)) +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "M",
                                           big.mark = ",", scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Ventas Mensuales 2025 — Registro inmediato vs. Digitalización tardía",
    subtitle = "La zona azul claro representa el monto de ventas que se digita al cierre del día, no en el momento de la venta",
    x = NULL, y = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(file.path(CARPETA_SALIDA, "A1_ventas_inmediato_vs_tarde.png"),
       g_a1, width = 13, height = 6.5, dpi = 180, bg = "white")


# A2 — % facturas digitalizadas tarde por mes 

prom_m <- mean(mensual$pct_fuera_fact)

g_a2 <- mensual %>%
  ggplot(aes(x = mes_label, y = pct_fuera_fact)) +
  geom_col(fill = azul_3, width = 0.68, alpha = 0.92) +
  geom_hline(yintercept = prom_m, linetype = "dashed",
             colour = linea_p, linewidth = 0.9) +
  annotate("text", x = 0.55, y = prom_m + 0.008, hjust = 0,
           label = glue("Promedio anual: {round(prom_m*100,1)}%"),
           colour = linea_p, size = 3.4, fontface = "bold") +
  geom_text(aes(label = glue("{round(pct_fuera_fact*100,0)}%"),
                y = pct_fuera_fact + 0.005),
            colour = azul_1, size = 3.2, fontface = "bold", vjust = 0) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "% Mensual de Facturas Digitalizadas al Final del Día — 2025",
    subtitle = "Proporción de transacciones del mes que no se registran en SAI en el momento de la venta",
    x = NULL, y = "% del total de facturas del mes",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA\nFórmula: facturas digitalizadas tarde ÷ total facturas reales del mes"
  ) +
  tema_aaa +
  theme(legend.position = "none")

ggsave(file.path(CARPETA_SALIDA, "A2_pct_facturas_tarde_mensual.png"),
       g_a2, width = 13, height = 6.5, dpi = 180, bg = "white")
cat("✔  A2 guardada\n")


# A3 — Cantidad absoluta de facturas: SAI vs digitalizadas tarde (por mes)

g_a3 <- mensual %>%
  select(mes_label, fact_sai, fact_fuera) %>%
  pivot_longer(-mes_label, names_to = "tipo", values_to = "facturas") %>%
  mutate(tipo = factor(tipo,
                       levels = c("fact_fuera", "fact_sai"),
                       labels = c("Digitalizadas al final del día",
                                  "Registradas en el momento (SAI)"))) %>%
  ggplot(aes(x = mes_label, y = facturas, fill = tipo)) +
  geom_col(position = "stack", width = 0.72) +
  geom_text(data = mensual,
            aes(x = mes_label,
                y = fact_total + max(mensual$fact_total) * 0.025,
                label = format(fact_fuera, big.mark = ",")),
            inherit.aes = FALSE,
            colour = azul_2, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Registradas en el momento (SAI)"  = azul_1,
                               "Digitalizadas al final del día"    = azul_4)) +
  scale_y_continuous(labels = label_comma(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Número de Facturas Mensuales — Registro inmediato vs. Digitalización tardía",
    subtitle = "El número sobre cada barra indica las facturas que se digitalizan al cierre del día",
    x = NULL, y = "Cantidad de facturas",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(file.path(CARPETA_SALIDA, "A3_facturas_inmediato_vs_tarde.png"),
       g_a3, width = 13, height = 6.5, dpi = 180, bg = "white")


# A4 — Ingresos por canal: Agro vs Ferretería SAI
g_a4 <- mensual %>%
  select(mes_label, agro_sai, ferr_sai) %>%
  pivot_longer(-mes_label, names_to = "canal", values_to = "valor") %>%
  mutate(canal = factor(canal,
                        levels = c("agro_sai", "ferr_sai"),
                        labels = c("Agro", "Ferretería"))) %>%
  ggplot(aes(x = mes_label, y = valor / 1e6, fill = canal)) +
  geom_col(position = "dodge", width = 0.68) +
  scale_fill_manual(values = c("Agro"        = azul_1,
                               "Ferretería"  = azul_4)) +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "M",
                                           big.mark = ",", scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Ventas Mensuales por Canal — Agro vs. Ferretería 2025",
    subtitle = "Ventas registradas en SAI. Ferretería no incluye las digitalizadas al cierre.",
    x = NULL, y = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa

ggsave(file.path(CARPETA_SALIDA, "A4_ingresos_agro_vs_ferreteria.png"),
       g_a4, width = 13, height = 6.5, dpi = 180, bg = "white")

# A5 — Ventas reales vs Gastos por mes
g_a5 <- mensual %>%
  select(mes_label, ventas_reales, gastos) %>%
  pivot_longer(-mes_label, names_to = "tipo", values_to = "valor") %>%
  mutate(tipo = factor(tipo,
                       levels = c("ventas_reales", "gastos"),
                       labels = c("Ventas reales totales", "Gastos operativos"))) %>%
  ggplot(aes(x = mes_label, y = valor / 1e6, colour = tipo, group = tipo)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("Ventas reales totales" = azul_1,
                                 "Gastos operativos"     = azul_4)) +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "M",
                                           big.mark = ",", scale = 1)) +
  labs(
    title    = "Ventas Reales vs. Gastos Operativos por Mes — 2025",
    subtitle = "Comparación entre ingresos reales del negocio y egresos mensuales",
    x = NULL, y = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa

ggsave(file.path(CARPETA_SALIDA, "A5_ventas_vs_gastos.png"),
       g_a5, width = 13, height = 6.5, dpi = 180, bg = "white")

# A6 — Mapa de calor: % digitalización tardía 

heat <- raw %>%
  filter(!is.na(pct_fuera_fact)) %>%
  mutate(semana = pmin(ceiling(day(fecha) / 7), 5)) %>%
  group_by(mes, mes_label, semana) %>%
  summarise(pct = mean(pct_fuera_fact, na.rm = TRUE), .groups = "drop")

g_a6 <- heat %>%
  ggplot(aes(x = factor(semana), y = mes_label, fill = pct)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = glue("{round(pct*100,0)}%")),
            colour = "white", size = 3.2, fontface = "bold") +
  scale_fill_gradient(low = azul_4, high = azul_1,
                      labels = percent_format(accuracy = 1),
                      name = "% facturas\ntardías") +
  scale_x_discrete(labels = c("Semana 1","Semana 2","Semana 3","Semana 4","Semana 5")) +
  scale_y_discrete(limits = rev(levels(heat$mes_label))) +
  labs(
    title    = "Mapa de Calor — % Facturas Digitalizadas Tardíamente",
    subtitle = "Promedio semanal por mes. Azul oscuro = mayor proporción de ventas sin registro inmediato.",
    x = NULL, y = NULL,
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa +
  theme(legend.position = "right",
        panel.grid = element_blank())

ggsave(file.path(CARPETA_SALIDA, "A6_heatmap_digitalizacion_tardia.png"),
       g_a6, width = 12, height = 7, dpi = 180, bg = "white")

# A7 — Ingresos mensuales por medio de pago %
medios_mensual <- mensual %>%
  mutate(efectivo = agro_sai + ferr_sai + ferr_otros) %>%
  select(mes_label, efectivo, tarjeta, transferencia) %>%
  pivot_longer(-mes_label, names_to = "medio", values_to = "valor") %>%
  group_by(mes_label) %>%
  mutate(pct = valor / sum(valor)) %>%
  ungroup() %>%
  mutate(medio = factor(medio,
                        levels = c("transferencia", "tarjeta", "efectivo"),
                        labels = c("Transferencia", "Tarjeta", "Efectivo")))

g_mp1 <- ggplot(medios_mensual,
                aes(x = mes_label, y = valor / 1e6, fill = medio)) +
  geom_col(position = "stack", width = 0.72) +
  geom_text(aes(label = ifelse(pct >= 0.07,
                               paste0(round(pct * 100, 0), "%"), "")),
            position = position_stack(vjust = 0.5),
            colour = "white", size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c("Efectivo"      = azul_1,
                               "Tarjeta"       = azul_3,
                               "Transferencia" = azul_5)) +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "M",
                                           big.mark = ",", scale = 1),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title    = "Ingresos Mensuales por Medio de Pago — 2025",
    subtitle = "El porcentaje dentro de cada segmento indica su participación en el total del mes",
    x = NULL, y = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(file.path(CARPETA_SALIDA, "A7_medio_pago_mensual.png"),
       g_mp1, width = 13, height = 6.5, dpi = 180, bg = "white")

# ─── NOVIEMBRE — análisis diario

# N1 — SAI vs ventas reales por día
g_n1 <- noviembre %>%
  mutate(ventas_fuera = ventas_reales - ventas_sai) %>%
  select(dia, ventas_sai, ventas_fuera) %>%
  pivot_longer(-dia, names_to = "tipo", values_to = "valor") %>%
  mutate(tipo = factor(tipo,
                       levels = c("ventas_fuera", "ventas_sai"),
                       labels = c("Digitalizadas al final del día",
                                  "Registradas en el momento (SAI)"))) %>%
  ggplot(aes(x = dia, y = valor / 1e6, fill = tipo)) +
  geom_col(position = "stack", width = 0.78) +
  geom_line(data = noviembre,
            aes(x = dia, y = (ventas_reales - ventas_sai) / 1e6, group = 1),
            inherit.aes = FALSE,
            colour = azul_2, linetype = "dashed", linewidth = 1.1) +
  geom_point(data = noviembre,
             aes(x = dia, y = (ventas_reales - ventas_sai) / 1e6),
             inherit.aes = FALSE,
             colour = azul_2, size = 2.2) +
  scale_fill_manual(values = c("Registradas en el momento (SAI)"  = azul_1,
                               "Digitalizadas al final del día"    = azul_5)) +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "M",
                                           big.mark = ",", scale = 1)) +
  labs(
    title    = "Ventas Registradas en SAI vs. Ventas Reales — Noviembre 2025",
    subtitle = "La zona azul claro es el monto diario digitado al cierre del día · La línea muestra su evolución",
    x = "Día de Noviembre", y = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(file.path(CARPETA_SALIDA, "N1_noviembre_SAI_vs_real.png"),
       g_n1, width = 14, height = 6.5, dpi = 180, bg = "white")


# N2 — % facturas digitalizadas tarde por día (noviembre)
g_n2 <- noviembre %>%
  ggplot(aes(x = dia, y = pct_fuera_fact)) +
  geom_col(fill = azul_3, width = 0.75, alpha = 0.92) +
  geom_hline(yintercept = prom_nov, linetype = "dashed",
             colour = linea_p, linewidth = 0.9) +
  annotate("text", x = 0.5, y = prom_nov + 0.012, hjust = 0,
           label = glue("Promedio nov.: {round(prom_nov*100,1)}%"),
           colour = linea_p, size = 3.4, fontface = "bold") +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "% Diario de Facturas Digitalizadas al Final del Día — Noviembre 2025",
    subtitle = "Proporción de transacciones del día que no se registran en SAI en el momento de la venta",
    x = "Día de Noviembre", y = "% del total de facturas del día",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA\nFórmula: facturas tardías ÷ total facturas reales del día"
  ) +
  tema_aaa +
  theme(legend.position = "none")

ggsave(file.path(CARPETA_SALIDA, "N2_noviembre_pct_tarde.png"),
       g_n2, width = 14, height = 6.5, dpi = 180, bg = "white")

# N3 — Facturas absolutas por día (noviembre)
g_n3 <- noviembre %>%
  select(dia, fact_sai_total, fact_fuera) %>%
  pivot_longer(-dia, names_to = "tipo", values_to = "n") %>%
  mutate(tipo = factor(tipo,
                       levels = c("fact_fuera", "fact_sai_total"),
                       labels = c("Digitalizadas al final del día",
                                  "Registradas en el momento (SAI)"))) %>%
  ggplot(aes(x = dia, y = n, fill = tipo)) +
  geom_col(position = "stack", width = 0.78) +
  geom_text(data = noviembre,
            aes(x = dia,
                y = fact_total_real + max(noviembre$fact_total_real, na.rm=TRUE) * 0.03,
                label = fact_fuera),
            inherit.aes = FALSE,
            colour = azul_2, size = 2.7, fontface = "bold") +
  scale_fill_manual(values = c("Registradas en el momento (SAI)"  = azul_1,
                               "Digitalizadas al final del día"    = azul_4)) +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Facturas Diarias — Registro inmediato vs. Digitalización tardía — Noviembre 2025",
    subtitle = "El número sobre cada barra = facturas digitadas al cierre del día",
    x = "Día de Noviembre", y = "Cantidad de facturas",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(file.path(CARPETA_SALIDA, "N3_noviembre_facturas_absolutas.png"),
       g_n3, width = 14, height = 6.5, dpi = 180, bg = "white")


# N4 — Agro vs Ferretería por día (noviembre)
g_n4 <- noviembre %>%
  select(dia, agro_sai, ferr_sai) %>%
  pivot_longer(-dia, names_to = "canal", values_to = "valor") %>%
  mutate(canal = factor(canal,
                        levels = c("agro_sai","ferr_sai"),
                        labels = c("Agro (SAI)", "Ferretería (SAI)"))) %>%
  ggplot(aes(x = dia, y = valor / 1e6, colour = canal, group = canal)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = c("Agro (SAI)"       = azul_1,
                                 "Ferretería (SAI)" = azul_4)) +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "M",
                                           big.mark = ",", scale = 1)) +
  labs(
    title    = "Ventas Diarias por Canal — Agro vs. Ferretería — Noviembre 2025",
    subtitle = "Ventas registradas en SAI (efectivo directo)",
    x = "Día de Noviembre", y = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa

ggsave(file.path(CARPETA_SALIDA, "N4_noviembre_agro_vs_ferreteria.png"),
       g_n4, width = 14, height = 6.5, dpi = 180, bg = "white")


# N5 — Gastos diarios noviembre
g_n5 <- noviembre %>%
  ggplot(aes(x = dia, y = gastos / 1e6)) +
  geom_col(fill = azul_3, width = 0.72, alpha = 0.9) +
  geom_hline(yintercept = mean(noviembre$gastos / 1e6, na.rm = TRUE),
             linetype = "dashed", colour = linea_p, linewidth = 0.9) +
  annotate("text", x = 1,
           y = mean(noviembre$gastos / 1e6, na.rm = TRUE) + 0.12, hjust = 0,
           label = glue("Promedio: ${round(mean(noviembre$gastos/1e6, na.rm=TRUE),1)}M"),
           colour = linea_p, size = 3.4, fontface = "bold") +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "M",
                                           big.mark = ",", scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Gastos Operativos Diarios — Noviembre 2025",
    subtitle = "Egresos registrados en cuadres de caja",
    x = "Día de Noviembre", y = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa

ggsave(file.path(CARPETA_SALIDA, "N5_noviembre_gastos.png"),
       g_n5, width = 14, height = 6.5, dpi = 180, bg = "white")

# N6 — Ingresos diarios por medio de pago
medios_nov <- noviembre %>%
  mutate(efectivo = agro_sai + ferr_sai + ferr_otros) %>%
  select(dia, efectivo, tarjeta, transferencia) %>%
  pivot_longer(-dia, names_to = "medio", values_to = "valor") %>%
  group_by(dia) %>%
  mutate(pct = valor / sum(valor)) %>%
  ungroup() %>%
  mutate(medio = factor(medio,
                        levels = c("transferencia", "tarjeta", "efectivo"),
                        labels = c("Transferencia", "Tarjeta", "Efectivo")))
g_mp2 <- ggplot(medios_nov,
                aes(x = dia, y = valor / 1e6, fill = medio)) +
  geom_col(position = "stack", width = 0.78) +
  geom_text(aes(label = ifelse(pct >= 0.08,
                               paste0(round(pct * 100, 0), "%"), "")),
            position = position_stack(vjust = 0.5),
            colour = "white", size = 2.8, fontface = "bold") +
  scale_fill_manual(values = c("Efectivo"      = azul_1,
                               "Tarjeta"       = azul_3,
                               "Transferencia" = azul_5)) +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "M",
                                           big.mark = ",", scale = 1),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title    = "Ingresos Diarios por Medio de Pago — Noviembre 2025",
    subtitle = "El porcentaje dentro de cada segmento indica su participación en el total del día",
    x = "Día de Noviembre", y = "Millones de pesos (COP)",
    caption  = "Fuente: Cuadres de caja diarios · Agrovariedades Triple AAA"
  ) +
  tema_aaa +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(file.path(CARPETA_SALIDA, "N6_medio_pago_noviembre_v3.png"),
       g_mp2, width = 14, height = 6.5, dpi = 180, bg = "white")

total_tarde_fact  <- sum(raw$fact_fuera, na.rm = TRUE)
total_real_fact   <- sum(raw$fact_total_real, na.rm = TRUE)
total_tarde_monto <- sum(raw$ferr_otros, na.rm = TRUE)
total_real_monto  <- sum(raw$ventas_reales, na.rm = TRUE)
