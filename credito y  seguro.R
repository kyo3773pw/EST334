library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(stringr)

# --- 1) Cargar tu data unida ---
datos <- read_csv("D:/UNAP/10mo/estadistica espacial/OneDrive_2025-09-10/BBDD_ENA 2014-2024/2024/data_seleccionada.csv")

# Suponiendo que tu data se llama 'datos'
datos <- datos %>%
  mutate(
    CODDEP = "21",  # Puno
    CCPP = str_pad(CCPP, 2, pad = "0"),
    CCDI = str_pad(CCDI, 2, pad = "0"),
    UBIGEO = paste0(CODDEP, CCPP, CCDI)
  )

# --- 2) Resumir totales por distrito (CCDI = UBIGEO) ---
resumen <- datos %>%
  group_by(UBIGEO, NOMBREDI) %>%
  summarise(
    total_p901 = sum(P901, na.rm = TRUE),
    total_p905 = sum(P905, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3) Leer shapefile de todos los distritos ---
peru_distritos <- st_read("D:/UNAP/10mo/estadistica espacial/shapefiles/DISTRITOS.shp")

# --- 4) Filtrar solo Puno ---
puno_distritos <- peru_distritos %>%
  filter(DEPARTAMEN == "PUNO")

# Unir con tus datos usando UBIGEO
puno_datos <- puno_distritos %>%
  left_join(resumen, by = "UBIGEO")

puno_centroides <- st_centroid(puno_datos) %>%
  mutate(
    lng = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

# Crear offset para separar los puntos (desplazamiento en grados)
offset_distance <- 0.01  # Ajusta según el zoom deseado

# Crear datasets separados con posiciones desplazadas
puntos_p901 <- puno_centroides %>%
  mutate(
    lng_offset = lng - offset_distance,  # P901 a la izquierda
    lat_offset = lat,
    popup_info = paste0("<b>", DISTRITO, "</b><br>",
                        "<b>Crédito Solicitado (P901):</b> ", format(total_p901, big.mark = ",")),
    # Escalar radius basado en valores (mín 3, máx 20)
    radius_scaled = pmax(3, pmin(20, sqrt(total_p901) * 0.5))
  )

puntos_p905 <- puno_centroides %>%
  mutate(
    lng_offset = lng + offset_distance,  # P905 a la derecha  
    lat_offset = lat,
    popup_info = paste0("<b>", DISTRITO, "</b><br>",
                        "<b>Seguro Agropecuario (P905):</b> ", format(total_p905, big.mark = ",")),
    # Escalar radius basado en valores (mín 3, máx 20)
    radius_scaled = pmax(3, pmin(20, sqrt(total_p905) * 0.5))
  )

# Mapa con puntos separados y escalados proporcionalmente
mapa_puno <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  
  # Polígonos de distritos
  addPolygons(data = puno_datos, 
              fillColor = "transparent", 
              color = "gray", 
              weight = 1,
              popup = ~paste0("<b>", DISTRITO, "</b>")) %>%
  
  # Puntos P901 (Crédito Solicitado) - Rojos, lado izquierdo
  addCircleMarkers(data = puntos_p901, 
                   lng = ~lng_offset, 
                   lat = ~lat_offset, 
                   radius = ~radius_scaled,
                   color = "#d73027", 
                   fillColor = "#d73027", 
                   fillOpacity = 0.7,
                   stroke = TRUE,
                   weight = 2,
                   popup = ~popup_info, 
                   group = "Crédito Solicitado (P901)") %>%
  
  # Puntos P905 (Seguro Agropecuario) - Azules, lado derecho  
  addCircleMarkers(data = puntos_p905, 
                   lng = ~lng_offset, 
                   lat = ~lat_offset, 
                   radius = ~radius_scaled,
                   color = "#2166ac", 
                   fillColor = "#2166ac", 
                   fillOpacity = 0.7,
                   stroke = TRUE,
                   weight = 2,
                   popup = ~popup_info, 
                   group = "Seguro Agropecuario (P905)") %>%
  
  # Control de capas mejorado
  addLayersControl(
    overlayGroups = c("Crédito Solicitado (P901)", "Seguro Agropecuario (P905)"), 
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Leyenda personalizada
  addLegendCustom(
    colors = c("#d73027", "#2166ac"),
    labels = c("Crédito Solicitado (P901)", "Seguro Agropecuario (P905)"),
    sizes = c(10, 10),
    position = "bottomright",
    title = "Variables Agrarias"
  )

# Función para leyenda personalizada (si no existe)
addLegendCustom <- function(map, colors, labels, sizes, position, title) {
  map %>%
    addLegend(
      position = position,
      colors = colors,
      labels = labels,
      title = title,
      opacity = 0.7
    )
}

mapa_puno

