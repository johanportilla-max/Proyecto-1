# =============================================================================
# MODELO M/M/C — DIMENSIONAMIENTO DE TABLETS
# Agrovariedades Triple AAA — Propuesta de Mejoramiento #1
# Proyecto Integrador de Ingeniería I — Universidad del Valle 2026-I
# =============================================================================

# ── PARÁMETROS ────────────────────────────────────────────────────────────────

lambda   <- 154.1 / 8   # 19.26 fact/h  (cuadres de caja noviembre 2025)
mu       <- 60 / 7.61   #  7.88 fact/h  (survey a operarios: 7.61 min/factura)
t_crit   <- 1           # minuto crítico (umbral antes de volver al papel)
objetivo <- 2           # P(Wq > t_crit) < 2%
n_comp   <- 2           # computadores existentes (servidores base)

cat("=================================================================\n")
cat("PARÁMETROS DEL MODELO M/M/C\n")
cat("=================================================================\n")
cat(sprintf("  lambda   = %.4f fact/h  (154.1 fact/día / 8 h)\n", lambda))
cat(sprintf("  mu       = %.4f fact/h  (60 / 7.61 min por factura)\n", mu))
cat(sprintf("  t critico= %d min       (umbral de reproceso)\n", t_crit))
cat(sprintf("  Objetivo : P(Wq > %d min) < %.0f%%\n", t_crit, objetivo))
cat(sprintf("  Comp. base: %d unidades ya existentes\n\n", n_comp))

# ── FUNCIONES M/M/C (Erlang C) ────────────────────────────────────────────────

#' Calcula todas las métricas M/M/C para c servidores
mmc <- function(lambda, mu, c, t_min = 1) {
  a   <- lambda / mu          # intensidad de tráfico (Erlangs)
  rho <- lambda / (c * mu)   # utilización por servidor
  
  if (rho >= 1) return(NULL)  # sistema inestable
  
  # P(0): probabilidad de sistema vacío
  suma_terms <- sum(sapply(0:(c - 1), function(i) a^i / factorial(i)))
  ult_term   <- (a^c / factorial(c)) / (1 - rho)
  p0         <- 1 / (suma_terms + ult_term)
  
  # P(espera): fórmula de Erlang C = C(c, a)
  p_esp <- ult_term * p0
  
  # Tiempo esperado en cola (horas) y en sistema (horas)
  Wq  <- p_esp / (c * mu - lambda)
  W   <- Wq + 1 / mu
  
  # Número esperado en cola y en sistema (Little)
  Lq  <- lambda * Wq
  L   <- lambda * W
  
  # P(Wq > t_crit)
  t   <- t_min / 60   # convertir minutos a horas
  p_exc <- p_esp * exp(-(c * mu - lambda) * t)
  
  list(
    rho    = rho,
    p0     = p0,
    p_esp  = p_esp,
    Wq_min = Wq * 60,   # en minutos
    W_min  = W  * 60,
    Lq     = Lq,
    L      = L,
    p_exc  = p_exc * 100  # en porcentaje
  )
}

# ── EVALUACIÓN c = 3 a 9 SERVIDORES (1 a 7 tablets) ──────────────────────────

cat("=================================================================\n")
cat("RESULTADOS POR NÚMERO DE TABLETS\n")
cat("=================================================================\n")
cat(sprintf("%-4s %-8s %-7s %-8s %-10s %-10s %-11s %-8s\n",
            "c", "Tablets", "rho", "P0",
            "P(espera)", "Wq (min)", "P(Wq>1min)", "Cumple?"))
cat(paste(rep("-", 72), collapse = ""), "\n")

tabla <- data.frame()

for (n in 1:7) {
  c_tot <- n + n_comp
  r     <- mmc(lambda, mu, c_tot, t_min = t_crit)
  
  if (is.null(r)) {
    cat(sprintf("%-4d %-8d  INESTABLE (rho >= 1)\n", c_tot, n))
    next
  }
  
  cumple <- ifelse(r$p_exc < objetivo, "SI", "NO")
  marca  <- ifelse(r$p_exc < objetivo & nrow(tabla[tabla$Cumple == "SI", ]) == 0,
                   " <-- c*", "")
  
  cat(sprintf("%-4d %-8d %-7.4f %-8.4f %-10.4f %-10.4f %-11.2f %-8s%s\n",
              c_tot, n, r$rho, r$p0, r$p_esp,
              r$Wq_min, r$p_exc, cumple, marca))
  
  tabla <- rbind(tabla, data.frame(
    c_total   = c_tot,
    Tablets   = n,
    rho       = r$rho,
    P0        = r$p0,
    P_espera  = r$p_esp,
    Wq_min    = r$Wq_min,
    W_min     = r$W_min,
    Lq        = r$Lq,
    L         = r$L,
    P_exc_pct = r$p_exc,
    Cumple    = cumple
  ))
}

# ── RESULTADO ÓPTIMO ──────────────────────────────────────────────────────────

cat(paste(rep("-", 72), collapse = ""), "\n\n")

opt <- tabla[tabla$Cumple == "SI", ][1, ]

cat("=================================================================\n")
cat("RESULTADO OPTIMO\n")
cat("=================================================================\n")
cat(sprintf("  c* = %d tablets adicionales\n", opt$Tablets))
cat(sprintf("  Sistema total: %d dispositivos (%d tablets + %d computadores)\n",
            opt$c_total, opt$Tablets, n_comp))
cat(sprintf("  rho = %.1f%% utilizacion por dispositivo\n", opt$rho * 100))
cat(sprintf("  P(Wq > 1 min) = %.2f%% < %.0f%% objetivo\n",
            opt$P_exc_pct, objetivo))
cat(sprintf("  Wq promedio   = %.4f min (cuando hay espera)\n", opt$Wq_min))
cat(sprintf("  Lq promedio   = %.4f facturas en cola\n", opt$Lq))
cat(sprintf("  L  promedio   = %.4f facturas en el sistema\n\n", opt$L))

# ── GRÁFICA ───────────────────────────────────────────────────────────────────

colores <- ifelse(tabla$P_exc_pct < objetivo, "#1A5FA8", "#F09595")

barplot(
  height     = tabla$P_exc_pct,
  names.arg  = paste0(tabla$Tablets, "\ntablets"),
  col        = colores,
  border     = NA,
  main       = "P(Wq > 1 min) por numero de tablets — Modelo M/M/C\nAgrovariedades Triple AAA",
  ylab       = "P(Wq > 1 min) (%)",
  xlab       = "Tablets adicionales a los 2 computadores existentes",
  ylim       = c(0, max(tabla$P_exc_pct) * 1.2),
  cex.main   = 0.9,
  cex.axis   = 0.85,
  cex.lab    = 0.85
)

# Línea de objetivo
abline(h = objetivo, col = "#E07830", lwd = 2, lty = 2)

# Etiqueta de objetivo
mtext(text = "Objetivo 2%", side = 4, at = objetivo,
      col = "#E07830", cex = 0.75, las = 1, line = 0.2)

# Valores sobre las barras
bp <- barplot(tabla$P_exc_pct, plot = FALSE)
text(x      = bp,
     y      = tabla$P_exc_pct + max(tabla$P_exc_pct) * 0.04,
     labels = sprintf("%.1f%%", tabla$P_exc_pct),
     cex    = 0.8, col = "black")

legend("topright",
       legend = c(paste0("Cumple < ", objetivo, "%"), "No cumple"),
       fill   = c("#1A5FA8", "#F09595"),
       border = NA, cex = 0.8, bty = "n")

cat("Grafica generada en el panel Plots de RStudio.\n")
cat("Para guardarla: Export > Save as Image desde el panel Plots.\n\n")

# ── TABLA COMPLETA PARA LA DIAPOSITIVA ────────────────────────────────────────

cat("=================================================================\n")
cat("TABLA PARA DIAPOSITIVA 6\n")
cat("=================================================================\n")
cat(sprintf("%-12s %-8s %-10s %-10s %-14s %-8s\n",
            "c (total)", "Tablets", "rho", "P(espera)",
            "P(Wq>1min)", "Cumple"))
cat(paste(rep("-", 65), collapse = ""), "\n")

for (i in 1:nrow(tabla)) {
  f <- tabla[i, ]
  cat(sprintf("%-12d %-8d %-10s %-10s %-14s %-8s\n",
              f$c_total,
              f$Tablets,
              paste0(round(f$rho * 100, 1), "%"),
              paste0(round(f$P_espera * 100, 1), "%"),
              paste0(round(f$P_exc_pct, 1), "%"),
              f$Cumple))
}
