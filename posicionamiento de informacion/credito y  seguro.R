library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(stringr)
library(htmlwidgets)

# --- 1) Cargar tu data ---
datos <- read_csv("D:/UNAP/10mo/estadistica espacial/OneDrive_2025-09-10/BBDD_ENA 2014-2024/2024/data_seleccionada.csv")

# Crear UBIGEO
datos <- datos %>%
  mutate(
    CODDEP = "21",  # Puno
    CCPP = str_pad(CCPP, 2, pad = "0"),
    CCDI = str_pad(CCDI, 2, pad = "0"),
    UBIGEO = paste0(CODDEP, CCPP, CCDI)
  )

# --- 2) Resumir totales por distrito ---
resumen <- datos %>%
  group_by(UBIGEO, NOMBREDI) %>%
  summarise(
    total_p901 = sum(P901, na.rm = TRUE),
    total_p905 = sum(P905, na.rm = TRUE),
    .groups = "drop"
  )


# --- 3) Leer shapefile y filtrar Puno ---
peru_distritos <- st_read("D:/UNAP/10mo/estadistica espacial/shapefiles/DISTRITOS.shp")

puno_distritos <- peru_distritos %>%
  filter(DEPARTAMEN == "PUNO")

# --- 4) Unir con datos ---
puno_datos <- puno_distritos %>%
  left_join(resumen, by = "UBIGEO")

# --- 5) Centroides ---
puno_centroides <- st_centroid(puno_datos) %>%
  mutate(
    lng = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

offset_distance <- 0.01  # separación de puntos


# --- 6) Puntos para P901 ---
puntos_p901 <- puno_centroides %>%
  mutate(
    lng_offset = lng - offset_distance,
    lat_offset = lat,
    popup_info = paste0("<b>", DISTRITO, "</b><br>",
                        "<b>Crédito Solicitado (P901):</b> ", format(total_p901, big.mark = ",")),
    radius_scaled = pmax(3, pmin(20, sqrt(total_p901) * 0.5))
  )

# --- 7) Puntos para P905 ---
puntos_p905 <- puno_centroides %>%
  mutate(
    lng_offset = lng + offset_distance,
    lat_offset = lat,
    popup_info = paste0("<b>", DISTRITO, "</b><br>",
                        "<b>Seguro Agropecuario (P905):</b> ", format(total_p905, big.mark = ",")),
    radius_scaled = pmax(3, pmin(20, sqrt(total_p905) * 0.5))
  )

# --- 8) Función para leyenda personalizada ---
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

# --- 9) Crear mapa ---
mapa_puno <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(data = puno_datos, 
              fillColor = "transparent", 
              color = "gray", 
              weight = 1,
              popup = ~paste0("<b>", DISTRITO, "</b>")) %>%
  addCircleMarkers(data = puntos_p901, 
                   lng = ~lng_offset, lat = ~lat_offset, 
                   radius = ~radius_scaled,
                   color = "#d73027", fillColor = "#d73027", fillOpacity = 0.7,
                   stroke = TRUE, weight = 2,
                   popup = ~popup_info, group = "Crédito Solicitado (P901)") %>%
  addCircleMarkers(data = puntos_p905, 
                   lng = ~lng_offset, lat = ~lat_offset, 
                   radius = ~radius_scaled,
                   color = "#2166ac", fillColor = "#2166ac", fillOpacity = 0.7,
                   stroke = TRUE, weight = 2,
                   popup = ~popup_info, group = "Seguro Agropecuario (P905)") %>%
  addLayersControl(
    overlayGroups = c("Crédito Solicitado (P901)", "Seguro Agropecuario (P905)"), 
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegendCustom(
    colors = c("#d73027", "#2166ac"),
    labels = c("Crédito Solicitado (P901)", "Seguro Agropecuario (P905)"),
    sizes = c(10, 10),
    position = "bottomright",
    title = "Variables Agrarias"
  )
mapa_puno
# --- 10) Guardar HTML ---
saveWidget(mapa_puno, file = "mapa_credito_puno.html", selfcontained = TRUE)
getwd()
