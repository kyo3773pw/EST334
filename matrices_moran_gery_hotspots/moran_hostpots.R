# ============================================================================
# Matrices de Pesos, Moran, Geary y Hotspots
library(sf)
library(spdep)
library(dplyr)
library(ggplot2)
library(viridis)

# ============================================================================
# 1. CARGAR Y PREPARAR DATOS

# Cargar shapefile de distritos (ajusta la ruta)
distritos <- st_read("D:/UNAP/10mo/estadistica espacial/shapefiles/DISTRITOS.shp")

# Filtrar solo Puno (código departamento 21)
distritos_puno <- distritos %>%
  filter(substr(UBIGEO, 1, 2) == "21")

# Tus datos
r <- datos

# Agregar datos por distrito (ejemplo: contar registros por distrito)
datos_agg <- r %>%
  group_by(UBIGEO) %>%
  summarise(
    n_registros = n(),

# Unir datos con shapefile
puno_completo <- distritos_puno %>%
  left_join(datos_agg, by = "UBIGEO") %>%
  mutate(n_registros = ifelse(is.na(n_registros), 0, n_registros))

# ============================================================================
# 2. CREAR MATRICES DE PESOS ESPACIALES
# Crear vecindarios (contigüidad Queen)
vecinos_queen <- poly2nb(puno_completo, queen = TRUE)

# Crear vecindarios (contigüidad Rook)
vecinos_rook <- poly2nb(puno_completo, queen = FALSE)

# Crear vecindarios por distancia (k vecinos más cercanos)
coords <- st_centroid(st_geometry(puno_completo))
vecinos_knn <- knn2nb(knearneigh(coords, k = 4))

# Matriz de pesos espaciales (Queen, estandarizada por filas)
W_queen <- nb2listw(vecinos_queen, style = "W", zero.policy = TRUE)

# Matriz de pesos espaciales (KNN)
W_knn <- nb2listw(vecinos_knn, style = "W", zero.policy = TRUE)

# Visualizar estructura de vecindarios
cat("\n=== RESUMEN DE VECINDARIOS ===\n")
cat("Método Queen:\n")
print(summary(vecinos_queen))

cat("\nMétodo KNN (k=4):\n")
print(summary(vecinos_knn))

# Graficar vecindarios
plot(st_geometry(puno_completo), border = "gray", main = "Vecindarios Queen")
plot(vecinos_queen, coords = st_coordinates(coords), add = TRUE, col = "red")

# ============================================================================
# 3. ÍNDICE I DE MORAN (Autocorrelación Espacial Global)

# Calcular I de Moran
variable <- puno_completo$n_registros
moran_test <- moran.test(variable, W_queen, zero.policy = TRUE)

cat("\n=== ÍNDICE I DE MORAN ===\n")
print(moran_test)

# Interpretación
cat("\nInterpretación:")
cat("\nI de Moran:", round(moran_test$estimate[1], 4))
cat("\nValor esperado:", round(moran_test$estimate[2], 4))
cat("\nVarianza:", round(moran_test$estimate[3], 6))
cat("\np-valor:", format.pval(moran_test$p.value))

if(moran_test$p.value < 0.05) {
  if(moran_test$estimate[1] > 0) {
    cat("\n✓ Existe autocorrelación espacial POSITIVA significativa")
  } else {
    cat("\n✓ Existe autocorrelación espacial NEGATIVA significativa")
  }
} else {
  cat("\n✗ No hay autocorrelación espacial significativa")
}

# Gráfico de dispersión de Moran
moran_plot <- moran.plot(variable, W_queen, 
                         labels = puno_completo$DISTRITO,
                         xlab = "Variable (estandarizada)",
                         ylab = "Rezago espacial",
                         main = "Diagrama de Dispersión de Moran",
                         zero.policy = TRUE)

# ============================================================================
# 4. ÍNDICE C DE GEARY (Autocorrelación Espacial Global)
# Calcular C de Geary
geary_test <- geary.test(variable, W_queen, zero.policy = TRUE)

cat("\n\n=== ÍNDICE C DE GEARY ===\n")
print(geary_test)

# Interpretación
cat("\nInterpretación:")
cat("\nC de Geary:", round(geary_test$estimate[1], 4))
cat("\nValor esperado:", round(geary_test$estimate[2], 4))
cat("\nVarianza:", round(geary_test$estimate[3], 6))
cat("\np-valor:", format.pval(geary_test$p.value))

if(geary_test$p.value < 0.05) {
  if(geary_test$estimate[1] < 1) {
    cat("\n✓ Existe autocorrelación espacial POSITIVA significativa")
  } else {
    cat("\n✓ Existe autocorrelación espacial NEGATIVA significativa")
  }
} else {
  cat("\n✗ No hay autocorrelación espacial significativa")
}

# ============================================================================
# 5. ANÁLISIS DE HOTSPOTS (LISA - Local Indicators of Spatial Association)
# Calcular Moran Local (Ii)
moran_local <- localmoran(variable, W_queen, zero.policy = TRUE)

# Agregar resultados al shapefile
puno_completo$Ii <- moran_local[, 1]  # Estadístico Ii
puno_completo$E_Ii <- moran_local[, 2]  # Valor esperado
puno_completo$Var_Ii <- moran_local[, 3]  # Varianza
puno_completo$Z_Ii <- moran_local[, 4]  # Z-score
puno_completo$p_valor <- moran_local[, 5]  # p-valor

# Crear variable estandarizada y rezago espacial
puno_completo$variable_std <- scale(variable)
puno_completo$lag_std <- lag.listw(W_queen, puno_completo$variable_std, 
                                   zero.policy = TRUE)

# Clasificar tipos de clusters (significancia p < 0.05)
puno_completo <- puno_completo %>%
  mutate(
    cluster_type = case_when(
      p_valor > 0.05 ~ "No significativo",
      variable_std > 0 & lag_std > 0 ~ "Alto-Alto (HH)",
      variable_std < 0 & lag_std < 0 ~ "Bajo-Bajo (LL)",
      variable_std > 0 & lag_std < 0 ~ "Alto-Bajo (HL)",
      variable_std < 0 & lag_std > 0 ~ "Bajo-Alto (LH)",
      TRUE ~ "No clasificado"
    )
  )

# Resumen de hotspots
cat("\n\n=== RESUMEN DE HOTSPOTS (LISA) ===\n")
tabla_clusters <- table(puno_completo$cluster_type)
print(tabla_clusters)
cat("\n")
print(prop.table(tabla_clusters) * 100)

# ============================================================================
# 6. VISUALIZACIONES

# Mapa de la variable original
p1 <- ggplot(puno_completo) +
  geom_sf(aes(fill = n_registros), color = "white", size = 0.2) +
  scale_fill_viridis(option = "plasma", name = "N° Registros") +
  labs(title = "Distribución de Registros por Distrito",
       subtitle = "Departamento de Puno") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# Mapa de valores Z (Moran Local)
p2 <- ggplot(puno_completo) +
  geom_sf(aes(fill = Z_Ii), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, name = "Z-score") +
  labs(title = "Estadístico Z de Moran Local",
       subtitle = "Valores positivos indican similitud espacial") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

# Mapa de significancia
puno_completo$significativo <- ifelse(puno_completo$p_valor < 0.05, 
                                      "Significativo", "No significativo")

p3 <- ggplot(puno_completo) +
  geom_sf(aes(fill = significativo), color = "white", size = 0.2) +
  scale_fill_manual(values = c("Significativo" = "red", 
                               "No significativo" = "lightgray"),
                    name = "Significancia\n(p < 0.05)") +
  labs(title = "Distritos con Autocorrelación Local Significativa") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)

# Mapa de HOTSPOTS (LISA clusters)
colores_cluster <- c(
  "Alto-Alto (HH)" = "#d7191c",      # Rojo
  "Bajo-Bajo (LL)" = "#2b83ba",      # Azul
  "Alto-Bajo (HL)" = "#fdae61",      # Naranja
  "Bajo-Alto (LH)" = "#abd9e9",      # Celeste
  "No significativo" = "#f0f0f0"     # Gris claro
)

p4 <- ggplot(puno_completo) +
  geom_sf(aes(fill = cluster_type), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_cluster, 
                    name = "Tipo de Cluster",
                    drop = FALSE) +
  labs(title = "HOTSPOTS - Análisis LISA",
       subtitle = "Clusters significativos (p < 0.05)") +
  theme_minimal() +
  theme(legend.position = "right")

print(p4)

# ============================================================================
# 7. TABLA DE RESULTADOS

# Ver nombres de columnas disponibles
cat("\n=== COLUMNAS DISPONIBLES ===\n")
print(names(puno_completo))

# Tabla de distritos significativos
distritos_significativos <- puno_completo %>%
  filter(p_valor < 0.05) %>%
  st_drop_geometry() %>%
  select(DISTRITO, UBIGEO, n_registros, Ii, Z_Ii, p_valor, cluster_type) %>%
  arrange(p_valor)

cat("\n=== DISTRITOS CON HOTSPOTS SIGNIFICATIVOS ===\n")
print(distritos_significativos)

cat("\n✓ Análisis completado exitosamente\n")
