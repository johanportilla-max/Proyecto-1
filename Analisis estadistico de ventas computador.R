# =====================================================
# ANÁLISIS DESCRIPTIVO DE VENTAS - AGROVARIEDADES TRIPLE A SAS
# =====================================================

# 1. Cargar librerías necesarias ---------------------------------------------
library(readxl)      # lectura de Excel
library(dplyr)       # manipulación de datos
library(lubridate)   # manejo de fechas
library(ggplot2)     # gráficos
library(scales)      # formateo de números en gráficos
library(knitr)       # tablas elegantes
library(kableExtra)  # estilo adicional para tablas

# 2. Importar datos ----------------------------------------------------------
# Asegúrate de que el archivo BD.xlsx esté en tu directorio de trabajo.
df <- read_excel("BD.xlsx", sheet = "QPrincipal")


# 3. Limpieza y preparación --------------------------------------------------
# Convertir columnas numéricas que puedan haber sido leídas como texto
# (usualmente EXTEND, QTYSHIP, PRICE, COST, VLR_IVA vienen como numéricas)
df <- df %>%
  mutate(
    FECHA = as.Date(FECHA),                     # convertir a fecha
    MES = month(FECHA, label = TRUE),            # extraer mes (etiqueta)
    DIA = day(FECHA),                            # extraer día
    VENTA_NETA = as.numeric(EXTEND),             # asegurar numérico
    IVA = as.numeric(VLR_IVA),                   
    COSTO = as.numeric(COST),
    CANTIDAD = as.numeric(QTYSHIP)
  )

# Eliminar filas con valores NA en columnas clave (opcional)
df <- df %>% filter(!is.na(VENTA_NETA), !is.na(FECHA))

# 4. Indicadores globales ----------------------------------------------------
total_ventas <- sum(df$VENTA_NETA, na.rm = TRUE)
total_iva <- sum(df$IVA, na.rm = TRUE)
total_con_iva <- total_ventas + total_iva
n_transacciones <- nrow(df)
total_unidades <- sum(df$CANTIDAD, na.rm = TRUE)
ticket_promedio <- total_ventas / n_transacciones
margen_bruto_promedio <- (sum(df$VENTA_NETA - df$COSTO) / total_ventas) * 100

# Crear tabla resumen
resumen <- data.frame(
  Concepto = c("Total ventas (sin IVA)", "IVA total", "Total ventas con IVA",
               "Número de transacciones", "Total unidades vendidas",
               "Ticket promedio", "Margen bruto promedio (%)"),
  Valor = c(total_ventas, total_iva, total_con_iva,
            n_transacciones, total_unidades,
            ticket_promedio, margen_bruto_promedio)
)

# Formatear números
resumen$Valor <- ifelse(grepl("ventas|IVA|Ticket", resumen$Concepto),
                        dollar(resumen$Valor, prefix = "$ "),
                        ifelse(grepl("Margen", resumen$Concepto),
                               paste0(round(resumen$Valor, 2), "%"),
                               format(round(resumen$Valor), big.mark = ".")))

kable(resumen, caption = "Resumen General de Ventas") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# 5. Ventas por línea de producto --------------------------------------------
ventas_linea <- df %>%
  group_by(DESCLINEA) %>%
  summarise(
    Ventas = sum(VENTA_NETA, na.rm = TRUE),
    Unidades = sum(CANTIDAD, na.rm = TRUE),
    Participacion = Ventas / total_ventas * 100
  ) %>%
  arrange(desc(Ventas))

# Tabla de ventas por línea
kable(ventas_linea, digits = c(0, 2, 0, 2),
      col.names = c("Línea", "Ventas (sin IVA)", "Unidades", "Participación (%)"),
      caption = "Ventas por Línea de Producto") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(2, currency = "$ ")

# Gráfico de barras
ggplot(ventas_linea, aes(x = reorder(DESCLINEA, Ventas), y = Ventas)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$ ")) +
  labs(title = "Ventas por Línea de Producto",
       x = "Línea", y = "Ventas (sin IVA)") +
  theme_minimal()

top_productos <- df %>%
  group_by(ITEM, DESCRIPCION) %>%
  summarise(
    Cantidad = sum(CANTIDAD, na.rm = TRUE),
    Precio_promedio = mean(PRICE, na.rm = TRUE),
    Venta_total = sum(VENTA_NETA, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(Venta_total)) %>%
  slice_head(n = 10)

# Formatear las columnas numéricas como moneda
top_productos_formateado <- top_productos %>%
  mutate(
    Precio_promedio = dollar(Precio_promedio, prefix = "$ "),
    Venta_total = dollar(Venta_total, prefix = "$ ")
  )

# Mostrar tabla con kable
kable(top_productos_formateado, 
      col.names = c("Código", "Descripción", "Cantidad", "Precio prom.", "Venta total"),
      caption = "Top 10 Productos por Ingresos") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Gráfico de barras
ggplot(top_productos, aes(x = reorder(DESCRIPCION, Venta_total), y = Venta_total)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$ ")) +
  labs(title = "Top 10 Productos por Ingresos",
       x = "Producto", y = "Venta total") +
  theme_minimal()

# 7. Clientes principales (Top 10 por compras) -------------------------------
top_clientes <- df %>%
  group_by(NIT, RAZON) %>%
  summarise(Total_comprado = sum(VENTA_NETA, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Total_comprado)) %>%
  slice_head(n = 10)

# Tabla de top clientes
kable(top_clientes, digits = 2,
      col.names = c("NIT", "Razón Social", "Total comprado"),
      caption = "Top 10 Clientes") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(3, currency = "$ ")

# Gráfico de barras
ggplot(top_clientes, aes(x = reorder(RAZON, Total_comprado), y = Total_comprado)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$ ")) +
  labs(title = "Top 10 Clientes por Monto Comprado",
       x = "Cliente", y = "Total comprado") +
  theme_minimal()

# 8. Evolución diaria de las ventas -----------------------------------------
ventas_diarias <- df %>%
  group_by(FECHA) %>%
  summarise(Ventas_dia = sum(VENTA_NETA, na.rm = TRUE)) %>%
  arrange(FECHA)

# Gráfico de líneas
ggplot(ventas_diarias, aes(x = FECHA, y = Ventas_dia)) +
  geom_line(color = "firebrick", size = 1) +
  geom_point(color = "firebrick", size = 2) +
  scale_y_continuous(labels = dollar_format(prefix = "$ ")) +
  scale_x_date(date_breaks = "1 day", date_labels = "%d/%m") +
  labs(title = "Evolución Diaria de las Ventas (sin IVA)",
       x = "Fecha", y = "Ventas diarias") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 9. Rentabilidad por línea ------------------------------------------------
rentabilidad_linea <- df %>%
  group_by(DESCLINEA) %>%
  summarise(
    Ventas = sum(VENTA_NETA, na.rm = TRUE),
    Costo = sum(COSTO * CANTIDAD, na.rm = TRUE),
    Margen_bruto = (Ventas - Costo) / Ventas * 100
  ) %>%
  arrange(desc(Margen_bruto)) %>%
  mutate(
    Ventas = dollar(Ventas, prefix = "$ "),    # formato moneda
    Costo = dollar(Costo, prefix = "$ ")
  )

# Tabla de rentabilidad (sin column_spec)
kable(rentabilidad_linea, digits = c(0, 2, 2, 2),
      col.names = c("Línea", "Ventas", "Costo", "Margen bruto (%)"),
      caption = "Rentabilidad por Línea de Producto") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Gráfico de dispersión ventas vs margen
ggplot(rentabilidad_linea, aes(x = Ventas, y = Margen_bruto)) +
  geom_point(aes(color = DESCLINEA), size = 3) +
  geom_text(aes(label = DESCLINEA), hjust = -0.1, vjust = 0.5, size = 3) +
  scale_x_log10(labels = dollar_format(prefix = "$ ")) +
  labs(title = "Relación Ventas vs Margen Bruto por Línea",
       x = "Ventas (escala log10)", y = "Margen bruto (%)") +
  theme_minimal()

# 10. Guardar resultados (opcional) -----------------------------------------
# Si deseas guardar las tablas y gráficos en un archivo PDF o HTML,
# puedes usar R Markdown o la función ggsave() para cada gráfico.
# Ejemplo:
# ggsave("ventas_por_linea.png", width = 10, height = 6)

# Fin del análisis