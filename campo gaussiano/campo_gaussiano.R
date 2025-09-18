# Kriging Gaussiano Simplificado - Puno
# Versión sin conflictos de librerías

# --- LIBRERÍAS (carga individual para evitar conflictos) ---
library(readr)
library(dplyr)
library(sf)
library(gstat)
library(sp)

# --- CARGA DE DATOS ---
datos_final <- read_csv("D:/UNAP/10mo/estadistica espacial/OneDrive_2025-09-10/BBDD_ENA 2014-2024/2024/gauss_data/datos_gauss_final.csv")

# Verificar estructura
cat("Columnas en datos_final:", paste(names(datos_final), collapse = ", "), "\n")
cat("Dimensiones:", nrow(datos_final), "filas,", ncol(datos_final), "columnas\n")

# --- PREPARACIÓN DE DATOS ---
# Filtrar datos válidos y crear UBIGEO
datos_prep <- datos_final %>%
  dplyr::filter(!is.na(P1002B_TOTAL), P1002B_TOTAL > 0) %>%
  dplyr::mutate(
    CODDEP = "21",  # Puno
    CCPP_pad = stringr::str_pad(CCPP, 2, pad = "0"),
    CCDI_pad = stringr::str_pad(CCDI, 2, pad = "0"),
    UBIGEO = paste0(CODDEP, CCPP_pad, CCDI_pad),
    value = P1002B_TOTAL
  ) %>%
  dplyr::select(UBIGEO, value, CCPP, CCDI, ANIO) %>%
  dplyr::filter(!is.na(value))

cat("Datos preparados:", nrow(datos_prep), "observaciones\n")
cat("UBIGEOs únicos:", length(unique(datos_prep$UBIGEO)), "\n")

# --- CARGA DEL SHAPEFILE ---
cat("Cargando shapefile de distritos...\n")
peru_distritos <- st_read("D:/UNAP/10mo/estadistica espacial/shapefiles/DISTRITOS.shp", quiet = TRUE)

# Filtrar solo Puno
puno_distritos <- peru_distritos %>%
  dplyr::filter(DEPARTAMEN == "PUNO")

cat("Distritos de Puno encontrados:", nrow(puno_distritos), "\n")

# Verificar columnas del shapefile para hacer el join correcto
cat("Columnas en shapefile:", paste(names(puno_distritos), collapse = ", "), "\n")

# --- AGREGAR DATOS POR DISTRITO (promedio si hay múltiples años) ---
datos_agregados <- datos_prep %>%
  dplyr::group_by(UBIGEO) %>%
  dplyr::summarise(
    value = mean(value, na.rm = TRUE),
    n_obs = n(),
    .groups = 'drop'
  ) %>%
  dplyr::filter(value > 0)

cat("Distritos con datos agregados:", nrow(datos_agregados), "\n")

# --- UNIR CON GEOMETRÍA ---
# Intentar join por UBIGEO (verificar que el shapefile tenga esta columna)
datos_espaciales <- datos_agregados %>%
  dplyr::inner_join(puno_distritos, by = "UBIGEO") %>%
  st_as_sf()

cat("Distritos con geometría:", nrow(datos_espaciales), "\n")

if(nrow(datos_espaciales) == 0) {
  cat("ERROR: No se pudo hacer el join. Verificando columnas del shapefile...\n")
  print(head(puno_distritos))
  stop("No hay coincidencias entre UBIGEO de datos y shapefile")
}

# --- OBTENER CENTROIDES ---
centroides <- st_centroid(datos_espaciales)
coords <- st_coordinates(centroides)

datos_espaciales$lon <- coords[,1]
datos_espaciales$lat <- coords[,2]

# --- PROYECCIÓN A UTM ---
# UTM Zone 19S para Puno
crs_utm <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"

datos_utm <- st_transform(datos_espaciales, crs = crs_utm)
coords_utm <- st_coordinates(st_centroid(datos_utm))

# --- CREAR OBJETO SPATIAL POINTS PARA GSTAT ---
datos_sp <- data.frame(
  x = coords_utm[,1],
  y = coords_utm[,2],
  value = datos_utm$value
)

# Convertir a SpatialPointsDataFrame
coordinates(datos_sp) <- ~ x + y
proj4string(datos_sp) <- CRS(crs_utm)

cat("Puntos para kriging:", length(datos_sp), "\n")
cat("Rango de valores:", min(datos_sp$value), "-", max(datos_sp$value), "\n")

# --- ANÁLISIS VARIOGRÁFICO ---
# Calcular distancias para variograma
bbox_utm <- bbox(datos_sp)
max_dist <- sqrt((bbox_utm[1,2] - bbox_utm[1,1])^2 + (bbox_utm[2,2] - bbox_utm[2,1])^2) / 3

cat("Distancia máxima para variograma:", round(max_dist/1000, 2), "km\n")

# Variograma empírico
vg_emp <- variogram(value ~ 1, datos_sp, cutoff = max_dist, width = max_dist/15)

print("Variograma empírico:")
print(vg_emp)

# --- AJUSTE DEL MODELO GAUSSIANO ---
# Parámetros iniciales estimados
nugget_est <- min(vg_emp$gamma) * 0.1
sill_est <- max(vg_emp$gamma)
range_est <- max_dist / 3

# Modelo inicial
vg_model <- vgm(nugget = nugget_est,
                psill = sill_est - nugget_est,
                model = "Gau",
                range = range_est)

cat("Modelo inicial:\n")
print(vg_model)

# Ajustar modelo
vg_fit <- fit.variogram(vg_emp, vg_model)

cat("Modelo ajustado:\n")
print(vg_fit)

# --- VALIDACIÓN CRUZADA ---
cat("Realizando validación cruzada...\n")
cv_result <- krige.cv(value ~ 1, datos_sp, model = vg_fit, nfold = length(datos_sp))

# Métricas
mae <- mean(abs(cv_result$residual), na.rm = TRUE)
rmse <- sqrt(mean(cv_result$residual^2, na.rm = TRUE))
me <- mean(cv_result$residual, na.rm = TRUE)
r2 <- cor(cv_result$observed, cv_result$var1.pred, use = "complete.obs")^2

cat("\n=== MÉTRICAS DE VALIDACIÓN ===\n")
cat("MAE:", round(mae, 4), "\n")
cat("RMSE:", round(rmse, 4), "\n")
cat("ME:", round(me, 4), "\n")
cat("R²:", round(r2, 4), "\n")

# --- CREAR GRILLA DE PREDICCIÓN ---
cat("Creando grilla de predicción...\n")

# Usar el extent de Puno proyectado
puno_utm <- st_transform(puno_distritos, crs = crs_utm)
grid_sf <- st_make_grid(puno_utm, cellsize = 10000, what = "centers") %>%
  st_sf() %>%
  .[puno_utm, ]  # Mantener solo puntos dentro de Puno

grid_coords <- st_coordinates(grid_sf)
grid_sp <- SpatialPoints(grid_coords, proj4string = CRS(crs_utm))

cat("Puntos de grilla:", length(grid_sp), "\n")

# --- INTERPOLACIÓN KRIGING ---
cat("Ejecutando Kriging Ordinario...\n")
krig_result <- krige(value ~ 1, datos_sp, grid_sp, model = vg_fit)

# --- RESULTADOS ---
pred_summary <- summary(krig_result$var1.pred)
var_summary <- summary(krig_result$var1.var)

cat("\n=== RESUMEN DE PREDICCIONES ===\n")
print(pred_summary)
cat("\n=== RESUMEN DE VARIANZA ===\n")
print(var_summary)

# --- VISUALIZACIÓN ---

# 1. Gráfico del variograma
plot(vg_emp, vg_fit, 
     main = "Variograma Gaussiano - P1002B_TOTAL (Puno)",
     xlab = "Distancia (m)",
     ylab = "Semivarianza")

# 2. MAPA DE PUNO CON RESULTADOS KRIGING

# Cargar librerías adicionales para mapas
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(viridis)) install.packages("viridis")
if (!require(scales)) install.packages("scales")

library(ggplot2)
library(viridis)
library(scales)

# Convertir resultados a raster para visualización
library(raster)

# Crear raster de las predicciones
coords_pred <- coordinates(krig_result)
r_pred <- rasterFromXYZ(data.frame(
  x = coords_pred[,1],
  y = coords_pred[,2],
  z = krig_result$var1.pred
), crs = CRS(crs_utm))

# Crear raster de la varianza (incertidumbre)
r_var <- rasterFromXYZ(data.frame(
  x = coords_pred[,1],
  y = coords_pred[,2],
  z = sqrt(krig_result$var1.var)  # Desviación estándar
), crs = CRS(crs_utm))

# Convertir a data.frame para ggplot
pred_df <- as.data.frame(r_pred, xy = TRUE) %>%
  dplyr::rename(prediccion = z) %>%
  dplyr::filter(!is.na(prediccion))

var_df <- as.data.frame(r_var, xy = TRUE) %>%
  dplyr::rename(incertidumbre = z) %>%
  dplyr::filter(!is.na(incertidumbre))

# Preparar datos puntuales para el mapa
puntos_obs <- data.frame(
  x = coordinates(datos_sp)[,1],
  y = coordinates(datos_sp)[,2],
  valor_obs = datos_sp$value
)

# Convertir límites de Puno para ggplot
puno_df <- st_transform(puno_distritos, crs = crs_utm) %>%
  st_simplify(dTolerance = 1000)  # Simplificar para mejor rendimiento

# MAPA 1: Predicciones Kriging
p1 <- ggplot() +
  # Superficie interpolada
  geom_raster(data = pred_df, aes(x = x, y = y, fill = prediccion)) +
  # Límites de distritos
  geom_sf(data = puno_df, fill = NA, color = "white", size = 0.3) +
  # Puntos observados
  geom_point(data = puntos_obs, aes(x = x, y = y, size = valor_obs), 
             color = "red", alpha = 0.7) +
  # Escala de colores
  scale_fill_viridis_c(name = "P1002B_TOTAL\n(Predicción)", option = "plasma") +
  scale_size_continuous(name = "Valor\nObservado", range = c(1, 4)) +
  # Tema y etiquetas
  labs(
    title = "Interpolación Kriging Gaussiano - P1002B_TOTAL",
    subtitle = "Departamento de Puno",
    x = "Coordenada X (UTM)",
    y = "Coordenada Y (UTM)",
    caption = "Puntos rojos: valores observados"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    legend.position = "right",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  coord_sf(crs = crs_utm)

print(p1)

# MAPA 2: Incertidumbre (Desviación Estándar)
p2 <- ggplot() +
  # Superficie de incertidumbre
  geom_raster(data = var_df, aes(x = x, y = y, fill = incertidumbre)) +
  # Límites de distritos
  geom_sf(data = puno_df, fill = NA, color = "white", size = 0.3) +
  # Puntos observados
  geom_point(data = puntos_obs, aes(x = x, y = y), 
             color = "black", size = 2, alpha = 0.8) +
  # Escala de colores (más incertidumbre = más rojo)
  scale_fill_viridis_c(name = "Desviación\nEstándar", option = "inferno", direction = 1) +
  # Tema y etiquetas
  labs(
    title = "Mapa de Incertidumbre - Kriging Gaussiano",
    subtitle = "Desviación estándar de las predicciones",
    x = "Coordenada X (UTM)",
    y = "Coordenada Y (UTM)",
    caption = "Puntos negros: ubicaciones observadas"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    legend.position = "right",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  coord_sf(crs = crs_utm)

print(p2)

# MAPA 3: Comparación Observado vs Predicho (para validación)
# Extraer predicciones en los puntos observados
pred_en_puntos <- krige(value ~ 1, datos_sp, datos_sp, model = vg_fit)

comp_df <- data.frame(
  x = coordinates(datos_sp)[,1],
  y = coordinates(datos_sp)[,2],
  observado = datos_sp$value,
  predicho = pred_en_puntos$var1.pred,
  residual = datos_sp$value - pred_en_puntos$var1.pred
)

p3 <- ggplot() +
  # Fondo con límites de Puno
  geom_sf(data = puno_df, fill = "lightgray", color = "white", size = 0.3) +
  # Puntos coloreados por residual
  geom_point(data = comp_df, aes(x = x, y = y, color = residual, size = observado), 
             alpha = 0.8) +
  # Escala de colores divergente (azul = subestimado, rojo = sobreestimado)
  scale_color_gradient2(name = "Residual\n(Obs-Pred)", 
                        low = "blue", mid = "white", high = "red",
                        midpoint = 0) +
  scale_size_continuous(name = "Valor\nObservado", range = c(2, 6)) +
  # Tema y etiquetas
  labs(
    title = "Validación: Observado vs Predicho",
    subtitle = "Residuales del modelo Kriging",
    x = "Coordenada X (UTM)",
    y = "Coordenada Y (UTM)",
    caption = "Azul: subestimado, Rojo: sobreestimado"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    legend.position = "right",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  coord_sf(crs = crs_utm)

print(p3)

# Guardar los mapas
ggsave("mapa_kriging_predicciones.png", p1, width = 12, height = 8, dpi = 300)
ggsave("mapa_kriging_incertidumbre.png", p2, width = 12, height = 8, dpi = 300)
ggsave("mapa_kriging_validacion.png", p3, width = 12, height = 8, dpi = 300)

cat("Mapas guardados:\n")
cat("- mapa_kriging_predicciones.png\n")
cat("- mapa_kriging_incertidumbre.png\n")
cat("- mapa_kriging_validacion.png\n")

# --- GUARDAR RESULTADOS ---
resultados <- data.frame(
  x = coordinates(krig_result)[,1],
  y = coordinates(krig_result)[,2],
  prediccion = krig_result$var1.pred,
  varianza = krig_result$var1.var,
  sd = sqrt(krig_result$var1.var)
)

write.csv(resultados, "kriging_resultados.csv", row.names = FALSE)
cat("\nResultados guardados en 'kriging_resultados.csv'\n")

cat("\n¡Análisis completado exitosamente!\n")
