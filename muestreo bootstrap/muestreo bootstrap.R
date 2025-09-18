# Muestreo Bootstrap para CSV en R
# Autor: Script de Bootstrap
# Fecha: 2025

# Cargar librerías necesarias
library(readr)    # Para leer CSV
library(dplyr)    # Para manipulación de datos
library(boot)     # Para bootstrap (opcional)

# =============================================================================
# 1. CARGAR LOS DATOS
# =============================================================================

# Leer el archivo CSV
# Reemplaza "tu_archivo.csv" con la ruta real de tu archivo
datos <- read_csv("D:/UNAP/10mo/estadistica espacial/OneDrive_2025-09-10/BBDD_ENA 2014-2024/2024/data_seleccionada.csv")

# Explorar los datos
head(datos)
dim(datos)
summary(datos)

# Verificar las variables de interés
cat("Resumen de P901 (Crédito):\n")
summary(datos$P901)
cat("\nResumen de P905 (Seguro):\n")
summary(datos$P905)

# Ver distribución de estas variables
table(datos$P901, useNA = "ifany")
table(datos$P905, useNA = "ifany")

# =============================================================================
# 2. BOOTSTRAP SIMPLE - Remuestreo de filas
# =============================================================================

# Función para realizar bootstrap simple
bootstrap_simple <- function(data, n_bootstrap = 1000) {
  n <- nrow(data)
  bootstrap_samples <- list()
  
  for (i in 1:n_bootstrap) {
    # Muestreo con reemplazo
    indices <- sample(1:n, n, replace = TRUE)
    bootstrap_samples[[i]] <- data[indices, ]
  }
  
  return(bootstrap_samples)
}

# Realizar 1000 muestras bootstrap
muestras_bootstrap <- bootstrap_simple(datos, n_bootstrap = 1000)

# Ver la primera muestra
head(muestras_bootstrap[[1]])

# =============================================================================
# 3. BOOTSTRAP PARA P901 (CRÉDITO) Y P905 (SEGURO)
# =============================================================================

# Bootstrap para proporciones de P901 (Crédito)
bootstrap_proporcion_credito <- function(data, n_bootstrap = 1000) {
  proporciones <- numeric(n_bootstrap)
  n <- nrow(data)
  
  for (i in 1:n_bootstrap) {
    indices <- sample(1:n, n, replace = TRUE)
    muestra <- data[indices, ]
    # Calcular proporción de personas con crédito (asumiendo que 1 = Sí, 0 = No)
    proporciones[i] <- mean(muestra$P901, na.rm = TRUE)
  }
  
  return(proporciones)
}

# Bootstrap para proporciones de P905 (Seguro)
bootstrap_proporcion_seguro <- function(data, n_bootstrap = 1000) {
  proporciones <- numeric(n_bootstrap)
  n <- nrow(data)
  
  for (i in 1:n_bootstrap) {
    indices <- sample(1:n, n, replace = TRUE)
    muestra <- data[indices, ]
    # Calcular proporción de personas con seguro (asumiendo que 1 = Sí, 0 = No)
    proporciones[i] <- mean(muestra$P905, na.rm = TRUE)
  }
  
  return(proporciones)
}

# Realizar bootstrap para ambas variables
proporciones_credito <- bootstrap_proporcion_credito(datos, 1000)
proporciones_seguro <- bootstrap_proporcion_seguro(datos, 1000)

# Ver primeros resultados
head(proporciones_credito)
head(proporciones_seguro)

# =============================================================================
# 4. BOOTSTRAP USANDO LA LIBRERÍA BOOT PARA P901 Y P905
# =============================================================================

# Función estadística para bootstrap de P901 (Crédito)
estadistico_credito <- function(data, indices) {
  return(mean(data[indices, ]$P901, na.rm = TRUE))
}

# Función estadística para bootstrap de P905 (Seguro)
estadistico_seguro <- function(data, indices) {
  return(mean(data[indices, ]$P905, na.rm = TRUE))
}

# Realizar bootstrap con la librería boot
boot_credito <- boot(datos, estadistico_credito, R = 1000)
boot_seguro <- boot(datos, estadistico_seguro, R = 1000)

# Ver resultados
print("Resultados Bootstrap para P901 (Crédito):")
print(boot_credito)
print("\nResultados Bootstrap para P905 (Seguro):")
print(boot_seguro)

# Intervalos de confianza
ic_credito <- boot.ci(boot_credito, type = c("norm", "basic", "perc", "bca"))
ic_seguro <- boot.ci(boot_seguro, type = c("norm", "basic", "perc", "bca"))

print("\nIntervalos de Confianza para P901 (Crédito):")
print(ic_credito)
print("\nIntervalos de Confianza para P905 (Seguro):")
print(ic_seguro)

# =============================================================================
# 5. BOOTSTRAP ESTRATIFICADO POR DISTRITO O AÑO
# =============================================================================

# Bootstrap estratificado por año
bootstrap_estratificado_anio <- function(data, n_bootstrap = 1000) {
  bootstrap_samples <- list()
  
  for (i in 1:n_bootstrap) {
    muestra_estratificada <- data %>%
      group_by(anio) %>%
      sample_n(n(), replace = TRUE) %>%
      ungroup()
    
    bootstrap_samples[[i]] <- muestra_estratificada
  }
  
  return(bootstrap_samples)
}

# Bootstrap estratificado por distrito (usando NOMBREDI)
bootstrap_estratificado_distrito <- function(data, n_bootstrap = 1000) {
  bootstrap_samples <- list()
  
  for (i in 1:n_bootstrap) {
    muestra_estratificada <- data %>%
      group_by(NOMBREDI) %>%
      sample_n(n(), replace = TRUE) %>%
      ungroup()
    
    bootstrap_samples[[i]] <- muestra_estratificada
  }
  
  return(bootstrap_samples)
}

# Ejemplo de uso - puedes descomentar según necesites
# muestras_por_anio <- bootstrap_estratificado_anio(datos, 500)
# muestras_por_distrito <- bootstrap_estratificado_distrito(datos, 500)

# =============================================================================
# 6. ANÁLISIS ESPECÍFICO DE P901 Y P905
# =============================================================================

# Análisis detallado de los resultados bootstrap
analizar_proporciones <- function(proporciones, variable_name) {
  resultados <- list(
    variable = variable_name,
    media_proporcion = mean(proporciones, na.rm = TRUE),
    mediana_proporcion = median(proporciones, na.rm = TRUE),
    desviacion_estandar = sd(proporciones, na.rm = TRUE),
    error_estandar = sd(proporciones, na.rm = TRUE),
    intervalo_confianza_95 = quantile(proporciones, c(0.025, 0.975), na.rm = TRUE),
    min_valor = min(proporciones, na.rm = TRUE),
    max_valor = max(proporciones, na.rm = TRUE)
  )
  
  return(resultados)
}

# Análisis para ambas variables
resultados_credito <- analizar_proporciones(proporciones_credito, "P901 (Crédito)")
resultados_seguro <- analizar_proporciones(proporciones_seguro, "P905 (Seguro)")

# Mostrar resultados
print("=== RESULTADOS BOOTSTRAP PARA P901 (CRÉDITO) ===")
print(resultados_credito)

print("\n=== RESULTADOS BOOTSTRAP PARA P905 (SEGURO) ===")
print(resultados_seguro)

# Bootstrap conjunto para analizar correlación
bootstrap_correlacion <- function(data, n_bootstrap = 1000) {
  correlaciones <- numeric(n_bootstrap)
  n <- nrow(data)
  
  for (i in 1:n_bootstrap) {
    indices <- sample(1:n, n, replace = TRUE)
    muestra <- data[indices, ]
    correlaciones[i] <- cor(muestra$P901, muestra$P905, use = "complete.obs")
  }
  
  return(correlaciones)
}

# Calcular correlaciones bootstrap
correlaciones <- bootstrap_correlacion(datos, 1000)
resultados_correlacion <- analizar_proporciones(correlaciones, "Correlación P901-P905")

print("\n=== CORRELACIÓN ENTRE P901 Y P905 ===")
print(resultados_correlacion)

# =============================================================================
# 7. VISUALIZACIÓN ESPECÍFICA PARA P901 Y P905
# =============================================================================

library(ggplot2)

# Función para crear histogramas comparativos
visualizar_bootstrap_comparativo <- function(prop_credito, prop_seguro) {
  # Crear dataframe combinado
  df_viz <- data.frame(
    valores = c(prop_credito, prop_seguro),
    variable = rep(c("P901 (Crédito)", "P905 (Seguro)"), 
                   c(length(prop_credito), length(prop_seguro)))
  )
  
  # Crear gráfico
  p1 <- ggplot(df_viz, aes(x = valores, fill = variable)) +
    geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
    facet_wrap(~variable, ncol = 1) +
    geom_vline(data = df_viz %>% group_by(variable) %>% 
                 summarise(media = mean(valores)), 
               aes(xintercept = media), color = "red", linetype = "dashed", size = 1) +
    labs(title = "Distribuciones Bootstrap - P901 vs P905", 
         x = "Proporción", y = "Frecuencia") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p1)
}

# Crear visualización
plot_comparativo <- visualizar_bootstrap_comparativo(proporciones_credito, proporciones_seguro)
print(plot_comparativo)

# Gráfico de dispersión de correlaciones
visualizar_correlacion <- function(correlaciones) {
  df <- data.frame(correlacion = correlaciones)
  
  p2 <- ggplot(df, aes(x = correlacion)) +
    geom_histogram(bins = 50, fill = "lightblue", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean(correlacion)), color = "red", linetype = "dashed", size = 1) +
    labs(title = "Distribución Bootstrap de la Correlación P901-P905", 
         x = "Correlación", y = "Frecuencia") +
    theme_minimal()
  
  return(p2)
}

# Crear gráfico de correlación
plot_correlacion <- visualizar_correlacion(correlaciones)
print(plot_correlacion)

# =============================================================================
# 8. GUARDAR RESULTADOS ESPECÍFICOS
# =============================================================================

# Crear resumen completo de resultados
crear_resumen_completo <- function() {
  # Estadísticas bootstrap
  resumen <- data.frame(
    Variable = c("P901_Credito", "P905_Seguro", "Correlacion_P901_P905"),
    Media = c(resultados_credito$media_proporcion, 
              resultados_seguro$media_proporcion,
              resultados_correlacion$media_proporcion),
    Error_Estandar = c(resultados_credito$error_estandar,
                       resultados_seguro$error_estandar,
                       resultados_correlacion$error_estandar),
    IC_Inferior_95 = c(resultados_credito$intervalo_confianza_95[1],
                       resultados_seguro$intervalo_confianza_95[1],
                       resultados_correlacion$intervalo_confianza_95[1]),
    IC_Superior_95 = c(resultados_credito$intervalo_confianza_95[2],
                       resultados_seguro$intervalo_confianza_95[2],
                       resultados_correlacion$intervalo_confianza_95[2])
  )
  
  return(resumen)
}

# Generar y mostrar resumen
resumen_final <- crear_resumen_completo()
print("\n=== RESUMEN COMPLETO DE RESULTADOS BOOTSTRAP ===")
print(resumen_final)

# Guardar resultados en archivos
# Descomentar las líneas que quieras usar:

# 1. Guardar resumen de estadísticas
# write_csv(resumen_final, "resumen_bootstrap_P901_P905.csv")

# 2. Guardar todas las proporciones bootstrap
# resultados_detallados <- data.frame(
#   muestra = 1:1000,
#   proporcion_credito = proporciones_credito,
#   proporcion_seguro = proporciones_seguro,
#   correlacion = correlaciones
# )
# write_csv(resultados_detallados, "proporciones_bootstrap_detalladas.csv")

# 3. Guardar una muestra bootstrap específica con todas las variables
# muestra_ejemplo <- muestras_bootstrap[[1]]
# write_csv(muestra_ejemplo, "ejemplo_muestra_bootstrap.csv")

# 4. Guardar gráficos
# ggsave("distribucion_bootstrap_P901_P905.png", plot_comparativo, width = 10, height = 8)
# ggsave("correlacion_bootstrap.png", plot_correlacion, width = 8, height = 6)


tinytex::tinytex_root()
tinytex::is_tinytex()

tinytex::is_tinytex()
tinytex::pdflatex("test.tex")

tinytex::tlmgr_install("collection-latexrecommended")
tinytex::tlmgr_install("collection-fontsrecommended")
tinytex::tlmgr_install("collection-latexextra")
