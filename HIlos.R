# ══════════════════════════════════════════════════════════════════════
#  DASHBOARD — Encuesta de Satisfacción · Clínica de Ropa Entre Hilos
#  Proyecto en Ingeniería I · 2026-I · Universidad del Valle
#  Fuente: Google Forms — 91 respuestas (marzo 2026)
# ══════════════════════════════════════════════════════════════════════

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)

# Asegurar codificacion UTF-8 para tildes y caracteres especiales
Sys.setlocale("LC_ALL", "C.UTF-8")

# ── PALETA VERDE Y TEMA ─────────────────────────────────────────────
verde_1 <- "#0B3D2E"
verde_2 <- "#14694E"
verde_3 <- "#1E9E6B"
verde_4 <- "#4EC98B"
verde_5 <- "#A8E6C3"
verde_6 <- "#D4F5E2"
acento   <- "#FF6B35"
gris_f  <- "#F2F7F4"
gris_t  <- "#1C2E26"
gris_l  <- "#7A9488"
blanco  <- "#FFFFFF"

tema_eh <- theme_minimal(base_family = "sans") +
  theme(
    plot.background    = element_rect(fill = blanco, colour = NA),
    panel.background   = element_rect(fill = gris_f, colour = NA),
    panel.grid.major.y = element_line(colour = "#C8DDD2", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(size = 15, face = "bold", colour = verde_1,
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 10, colour = gris_l,
                                      margin = margin(b = 14)),
    plot.caption       = element_text(size = 7.5, colour = gris_l,
                                      hjust = 0, margin = margin(t = 10)),
    axis.text          = element_text(size = 9, colour = gris_t),
    axis.title         = element_text(size = 9, colour = gris_t),
    axis.title.x       = element_text(margin = margin(t = 8)),
    axis.title.y       = element_text(margin = margin(r = 8)),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9, colour = gris_t),
    legend.title       = element_blank(),
    legend.key.size    = unit(0.9, "lines"),
    strip.text         = element_text(size = 10, face = "bold", colour = verde_1),
    plot.margin        = margin(18, 22, 14, 18)
  )

# ── CARGA DE DATOS ───────────────────────────────────────────────────
# Copiar archivo para evitar problemas de codificación en la ruta
archivo_original <- list.files(pattern = "", full.names = TRUE)
archivo_tmp <- 
df <- read_excel("H.xlsx")

names(df) <- c("timestamp", "ultima_vez", "satisfaccion", "puntualidad",
               "entrega_cumplida", "tiempo_retraso", "aspecto_mejorar",
               "recomendaria")

df <- df %>%
  mutate(
    satisfaccion = as.numeric(satisfaccion),
    puntualidad  = as.numeric(puntualidad)
  )

n_total <- nrow(df)
caption_base <- paste0("Fuente: Encuesta de satisfacción · Clínica de Ropa Entre Hilos · n = ", n_total, " respuestas")

# ══════════════════════════════════════════════════════════════════════
#  G1 — Satisfacción con la calidad del servicio (escala 1-5)
# ══════════════════════════════════════════════════════════════════════
sat_df <- df %>%
  count(satisfaccion) %>%
  mutate(
    pct  = n / sum(n) * 100,
    satisfaccion = factor(satisfaccion),
    fill_color = case_when(
      satisfaccion == "5" ~ verde_2,
      satisfaccion == "4" ~ verde_3,
      satisfaccion == "3" ~ verde_4,
      satisfaccion == "2" ~ verde_5,
      satisfaccion == "1" ~ acento
    )
  )

media_sat <- round(mean(df$satisfaccion, na.rm = TRUE), 2)

g1 <- ggplot(sat_df, aes(x = satisfaccion, y = pct, fill = fill_color)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(", n, ")")),
            vjust = -0.4, size = 3.5, colour = gris_t, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 85), expand = c(0, 0)) +
  labs(
    title    = paste0("Satisfacción con la Calidad del Servicio  ·  Media: ", media_sat, " / 5"),
    subtitle = "\"¿Qué tan satisfecho(a) quedó con la calidad del último servicio?\"",
    x        = "Calificación (1 = Muy insatisfecho · 5 = Muy satisfecho)",
    y        = "Porcentaje de respuestas",
    caption  = caption_base
  ) +
  tema_eh

# ══════════════════════════════════════════════════════════════════════
#  G2 — Puntualidad en tiempos de entrega (escala 1-5)
# ══════════════════════════════════════════════════════════════════════
punt_df <- df %>%
  count(puntualidad) %>%
  mutate(
    pct  = n / sum(n) * 100,
    puntualidad = factor(puntualidad),
    fill_color = case_when(
      puntualidad == "5" ~ verde_2,
      puntualidad == "4" ~ verde_3,
      puntualidad == "3" ~ verde_4,
      puntualidad == "2" ~ verde_5,
      puntualidad == "1" ~ acento
    )
  )

media_punt <- round(mean(df$puntualidad, na.rm = TRUE), 2)

g2 <- ggplot(punt_df, aes(x = puntualidad, y = pct, fill = fill_color)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(", n, ")")),
            vjust = -0.4, size = 3.5, colour = gris_t, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 50), expand = c(0, 0)) +
  labs(
    title    = paste0("Puntualidad en Tiempos de Entrega  ·  Media: ", media_punt, " / 5"),
    subtitle = "\"¿Cómo califica la puntualidad en los tiempos de entrega del servicio?\"",
    x        = "Calificación (1 = Muy impuntual · 5 = Muy puntual)",
    y        = "Porcentaje de respuestas",
    caption  = caption_base
  ) +
  tema_eh

# ══════════════════════════════════════════════════════════════════════
#  G3 — Última vez que usó el servicio
# ══════════════════════════════════════════════════════════════════════
orden_uso <- c("Hace menos de 1 semana", "Hace aproximadamente 1 mes",
               "Entre 1 y 3 meses", "Entre 3 y 6 meses", "Más de 6 meses")

uso_df <- df %>%
  count(ultima_vez) %>%
  mutate(
    pct = n / sum(n) * 100,
    ultima_vez = factor(ultima_vez, levels = orden_uso),
    etiqueta_corta = c(
      "Hace menos de 1 semana"       = "< 1 semana",
      "Hace aproximadamente 1 mes"   = "~1 mes",
      "Entre 1 y 3 meses"            = "1-3 meses",
      "Entre 3 y 6 meses"            = "3-6 meses",
      "Más de 6 meses"               = "> 6 meses"
    )[as.character(ultima_vez)]
  ) %>%
  arrange(ultima_vez)

paleta_uso <- c(verde_1, verde_2, verde_3, verde_4, verde_5)

g3 <- ggplot(uso_df, aes(x = etiqueta_corta, y = pct,
                         fill = factor(etiqueta_corta, levels = uso_df$etiqueta_corta))) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(", n, ")")),
            vjust = -0.4, size = 3.5, colour = gris_t, fontface = "bold") +
  scale_fill_manual(values = setNames(paleta_uso, uso_df$etiqueta_corta)) +
  scale_x_discrete(limits = uso_df$etiqueta_corta) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 42), expand = c(0, 0)) +
  labs(
    title    = "Frecuencia de Uso del Servicio",
    subtitle = "\"Sin contar hoy, ¿hace cuánto fue la última vez que utilizó nuestros servicios?\"",
    x        = NULL,
    y        = "Porcentaje de respuestas",
    caption  = caption_base
  ) +
  tema_eh

# ══════════════════════════════════════════════════════════════════════
#  G4 — ¿Pedido entregado a tiempo? (Sí / No)
# ══════════════════════════════════════════════════════════════════════
entrega_df <- df %>%
  count(entrega_cumplida) %>%
  mutate(
    pct = n / sum(n) * 100,
    fill_color = ifelse(grepl("^S", entrega_cumplida), verde_2, acento)
  )

g4 <- ggplot(entrega_df, aes(x = entrega_cumplida, y = pct, fill = fill_color)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(", n, " resp.)")),
            vjust = -0.4, size = 4.2, colour = gris_t, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 72), expand = c(0, 0)) +
  labs(
    title    = "Cumplimiento de Fecha y Hora Prometida",
    subtitle = "\"¿Su pedido siempre ha sido entregado en la fecha y hora prometida?\"",
    x        = NULL,
    y        = "Porcentaje de respuestas",
    caption  = caption_base
  ) +
  tema_eh

# ══════════════════════════════════════════════════════════════════════
#  G5 — Tiempo de retraso reportado
# ══════════════════════════════════════════════════════════════════════
orden_retraso <- c("30 minutos - 1 hora", "1 hora - 2 horas",
                   "2 horas - 4 horas", "4 horas - 8 horas",
                   "1 día", "2-3 días", "4 días o más")

retraso_df <- df %>%
  filter(!is.na(tiempo_retraso)) %>%
  count(tiempo_retraso) %>%
  mutate(
    pct = n / sum(n) * 100,
    tiempo_retraso = factor(tiempo_retraso, levels = orden_retraso)
  ) %>%
  arrange(tiempo_retraso)

paleta_retraso <- colorRampPalette(c(verde_4, verde_2, verde_1))(nrow(retraso_df))

g5 <- ggplot(retraso_df, aes(x = tiempo_retraso, y = pct, fill = tiempo_retraso)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(", n, ")")),
            vjust = -0.4, size = 3.3, colour = gris_t, fontface = "bold") +
  scale_fill_manual(values = setNames(paleta_retraso, levels(retraso_df$tiempo_retraso))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 48), expand = c(0, 0)) +
  labs(
    title    = "Distribución del Tiempo de Retraso Reportado",
    subtitle = paste0("\"Si su pedido ha presentado retrasos, ¿de cuánto tiempo ha sido?\"  ·  n = ",
                      sum(retraso_df$n), " respuestas con retraso"),
    x        = NULL,
    y        = "Porcentaje de respuestas",
    caption  = caption_base
  ) +
  tema_eh +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 8))

# ══════════════════════════════════════════════════════════════════════
#  G6 — Aspectos a mejorar
# ══════════════════════════════════════════════════════════════════════
# Separar respuestas múltiples (separadas por ", ")
aspectos_raw <- df %>%
  filter(!is.na(aspecto_mejorar)) %>%
  pull(aspecto_mejorar)

aspectos_lista <- unlist(strsplit(aspectos_raw, ", "))

aspecto_df <- as.data.frame(table(aspectos_lista), stringsAsFactors = FALSE) %>%
  rename(aspecto = aspectos_lista, n = Freq) %>%
  mutate(
    pct = n / length(aspectos_raw) * 100,
    aspecto = forcats::fct_reorder(aspecto, n)
  )

paleta_asp <- colorRampPalette(c(verde_5, verde_3, verde_1))(nrow(aspecto_df))

g6 <- ggplot(aspecto_df, aes(x = aspecto, y = pct, fill = aspecto)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%  (", n, ")")),
            hjust = -0.08, size = 3.3, colour = gris_t, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = setNames(paleta_asp, levels(aspecto_df$aspecto))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 90), expand = c(0, 0)) +
  labs(
    title    = "Aspectos del Servicio que los Clientes Mejorarían",
    subtitle = "\"Si pudiese mejorar algún aspecto del servicio, ¿qué aspectos serían?\"",
    x        = NULL,
    y        = "Porcentaje de menciones",
    caption  = caption_base
  ) +
  tema_eh +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#C8DDD2", linewidth = 0.35))

# ══════════════════════════════════════════════════════════════════════
#  G7 — ¿Recomendaría Entre Hilos?
# ══════════════════════════════════════════════════════════════════════
rec_df <- df %>%
  filter(!is.na(recomendaria)) %>%
  count(recomendaria) %>%
  mutate(
    pct = n / sum(n) * 100,
    fill_color = ifelse(grepl("^S", recomendaria), verde_2, acento)
  )

g7 <- ggplot(rec_df, aes(x = recomendaria, y = pct, fill = fill_color)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(", n, " resp.)")),
            vjust = -0.4, size = 4.2, colour = gris_t, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 115), expand = c(0, 0)) +
  labs(
    title    = "Nivel de Recomendación del Servicio",
    subtitle = "\"¿Recomendaría la Clínica de Ropa Entre Hilos a sus amigos o familiares?\"",
    x        = NULL,
    y        = "Porcentaje de respuestas",
    caption  = caption_base
  ) +
  tema_eh

# ══════════════════════════════════════════════════════════════════════
#  EXPORTAR gráficas individuales
# ══════════════════════════════════════════════════════════════════════
dir.create("Graficas encuesta", showWarnings = FALSE)

ggsave("Graficas encuesta/g1_satisfaccion_calidad.png",  g1, width = 10, height = 6, dpi = 200, type = "cairo")
ggsave("Graficas encuesta/g2_puntualidad_entrega.png",   g2, width = 10, height = 6, dpi = 200, type = "cairo")
ggsave("Graficas encuesta/g3_frecuencia_uso.png",        g3, width = 10, height = 6, dpi = 200, type = "cairo")
ggsave("Graficas encuesta/g4_cumplimiento_entrega.png",  g4, width = 8,  height = 6, dpi = 200, type = "cairo")
ggsave("Graficas encuesta/g5_tiempo_retraso.png",        g5, width = 11, height = 6, dpi = 200, type = "cairo")
ggsave("Graficas encuesta/g6_aspectos_mejorar.png",      g6, width = 11, height = 6, dpi = 200, type = "cairo")
ggsave("Graficas encuesta/g7_recomendacion.png",         g7, width = 8,  height = 6, dpi = 200, type = "cairo")

# ══════════════════════════════════════════════════════════════════════
#  DASHBOARD COMPUESTO — Composición con patchwork
# ══════════════════════════════════════════════════════════════════════
dashboard <- (g1 | g2) /
  (g3 | g4) /
  (g5 | g6) /
  (plot_spacer() | g7 | plot_spacer()) +
  plot_annotation(
    title    = "DASHBOARD  ·  Encuesta de Satisfacción  ·  Clínica de Ropa Entre Hilos",
    subtitle = paste0("91 respuestas recolectadas  ·  Marzo 2026  ·  Satisfacción promedio: ",
                      media_sat, "/5  ·  Puntualidad promedio: ", media_punt, "/5  ·  ",
                      round(rec_df$pct[rec_df$recomendaria == "Sí"], 1), "% recomendaría el servicio"),
    caption  = "Fuente: Google Forms — Encuesta de satisfacción · Clínica de Ropa Entre Hilos · Proyecto en Ingeniería I · Universidad del Valle · 2026-I",
    theme = theme(
      plot.title    = element_text(size = 20, face = "bold", colour = verde_1,
                                   hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, colour = verde_3,
                                   hjust = 0.5, margin = margin(b = 16)),
      plot.caption  = element_text(size = 8, colour = gris_l,
                                   hjust = 0.5, margin = margin(t = 12)),
      plot.background = element_rect(fill = blanco, colour = verde_5, linewidth = 1.5),
      plot.margin     = margin(20, 24, 16, 24)
    )
  )

ggsave("Graficas encuesta/dashboard_encuesta_completo.png",
       dashboard, width = 22, height = 28, dpi = 200, limitsize = FALSE, type = "cairo")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  Dashboard exportado exitosamente en 'Graficas encuesta/'\n")
cat("  - 7 gráficas individuales (PNG, alta resolución)\n")
cat("  - 1 dashboard compuesto (dashboard_encuesta_completo.png)\n")
cat("══════════════════════════════════════════════════════════════\n")