# =============================================================================
# 2. Geoprocessing and datasets
# Author: Lucas Salamuni
# Date: 2025-12-21
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

## 1.1. Retrieving Packages

pacotes <- c("wdman", "factoextra", "readxl", "ggplot2", "patchwork",
             "corrplot", "plotly", "data.table", "EFAtools", "psych",
             "leaflet", "sf", "sp", "mapview", "webshot", "htmltools",
             "leafem", "viridisLite", "writexl", "rvest", "readr", "tidygeocoder",
             "stringr", "curl", "magrittr", "RSelenium", "netstat", "tidyverse")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

# -----------------------------------------------------------------------------
# 2. Shapefiles
# -----------------------------------------------------------------------------

## 2.1. Loading shapefiles for use in the overall map

#Neighborhoods - ok
sf_loc <- st_read("DIVISA_DE_BAIRROS.shp")
st_crs(sf_loc)
sf_loc <- st_transform(sf_loc, "+proj=longlat +datum=WGS84")

#Parks - ok
sf_parks <- st_read("PARQUES_E_BOSQUES.shp")
st_crs(sf_parks)
sf_parks <- st_transform(sf_parks, "+proj=longlat +datum=WGS84")

#Gardens and squares - ok
sf_squares <- st_read("PRACAS_E_JARDINETES.shp")
st_crs(sf_squares)
sf_squares <- st_transform(sf_squares, "+proj=longlat +datum=WGS84")

#Cicloways - ok
sf_cicloways <- st_read("CICLOVIA_OFICIAL.shp")
st_crs(sf_cicloways)
sf_cicloways <- st_transform(sf_cicloways, "+proj=longlat +datum=WGS84")

#Cicloroutes - ok
sf_cicloroutes <- st_read("CICLORROTA.shp")
st_crs(sf_cicloroutes)
sf_cicloroutes <- st_transform(sf_cicloroutes, "+proj=longlat +datum=WGS84")

#Railroads - ok
sf_railroads <- st_read("RRFSA_FERROVIAS.shp")
st_crs(sf_railroads)
sf_railroads <- st_transform(sf_railroads, "+proj=longlat +datum=WGS84")

#Sport & leisure areas - ok
sf_sport <- st_read("CENTRO_DE_ESPORTE_E_LAZER.shp")
st_crs(sf_sport)
sf_sport <- st_transform(sf_sport, "+proj=longlat +datum=WGS84")

#Cemiteries - ok
sf_cemiteries <- st_read("CEMITERIOS.shp")
st_crs(sf_cemiteries)
sf_cemiteries <- st_transform(sf_cemiteries, "+proj=longlat +datum=WGS84")

#Cidadania - ok
sf_cidadania <- st_read("RUA_DA_CIDADANIA.shp")
st_crs(sf_cidadania)
sf_cidadania <- st_transform(sf_cidadania, "+proj=longlat +datum=WGS84")

#Terminals - ok
sf_terminals <- st_read("TERMINAL_DE_TRANSPORTE.shp")
st_crs(sf_terminals)
sf_terminals <- st_transform(sf_terminals, "+proj=longlat +datum=WGS84")

#Dentist units - ok
sf_dentists <- st_read("CENTRO_DE_ESPECIALIDADES_ODONTOLOGICAS.shp")
st_crs(sf_dentists)
sf_dentists <- st_transform(sf_dentists, "+proj=longlat +datum=WGS84")

#Emergency - ok
sf_emergency <- st_read("UNIDADE_DE_PRONTO_ATENDIMENTO.shp")
st_crs(sf_emergency)
sf_emergency <- st_transform(sf_emergency, "+proj=longlat +datum=WGS84")

#Health units - ok
sf_health_units <- st_read("UNIDADE_DE_SAUDE.shp")
st_crs(sf_health_units)
sf_health_units <- st_transform(sf_health_units, "+proj=longlat +datum=WGS84")

#Hospitals - ok
sf_hospitals <- st_read("HOSPITAL.shp")
st_crs(sf_hospitals)
sf_hospitals <- st_transform(sf_hospitals, "+proj=longlat +datum=WGS84")

#Specialized medical units - ok
sf_med_units <- st_read("CENTRO_DE_ESPECIALIDADES_MEDICAS.shp")
st_crs(sf_med_units)
sf_med_units <- st_transform(sf_med_units, "+proj=longlat +datum=WGS84")

#Irregular settlements - ok
sf_irregular <- st_read("OCUPACAO_IRREGULAR.shp")
st_crs(sf_irregular)
sf_irregular <- st_transform(sf_irregular, "+proj=longlat +datum=WGS84")

#Lakes - ok
sf_lakes <- st_read("HIDRO_LAGOS_LAGOAS_REPRESAS.shp")
st_crs(sf_lakes)
sf_lakes <- st_transform(sf_lakes, "+proj=longlat +datum=WGS84")

#Rivers - ok
sf_rivers <- st_read("HIDRO_RIOS_LN.shp")
st_crs(sf_rivers)
sf_rivers <- st_transform(sf_rivers, "+proj=longlat +datum=WGS84")

#Public schools - ok
sf_schools <- st_read("ESCOLA_MUNICIPAL.shp")
st_crs(sf_schools)
sf_schools <- st_transform(sf_schools, "+proj=longlat +datum=WGS84")

# -----------------------------------------------------------------------------
# 3. Map
# -----------------------------------------------------------------------------

## 3.1. Data wrangling

types <- read_excel(path = "Neighborhood_classification.xlsx",
                    sheet = "Sheet1")

df_cwb <- read_excel(path = "CWB_PCA.xlsx",
                     sheet = "Data")

types$Loc <- toupper(types$Loc)

types <- types %>%
  mutate(NOME = case_when(
    Loc == "AGUA VERDE" ~ "ÁGUA VERDE",
    Loc == "AHU" ~ "AHÚ",
    Loc == "ALTO BOQUEIRAO" ~ "ALTO BOQUEIRÃO",
    Loc == "ALTO DA GLORIA" ~ "ALTO DA GLÓRIA",
    Loc == "BOQUEIRAO" ~ "BOQUEIRÃO",
    Loc == "CAPAO DA IMBUIA" ~ "CAPÃO DA IMBUIA",
    Loc == "CAPAO RASO" ~ "CAPÃO RASO",
    Loc == "CENTRO CIVICO" ~ "CENTRO CÍVICO",
    Loc == "CIC" ~ "CIDADE INDUSTRIAL DE CURITIBA",
    Loc == "GUAIRA" ~ "GUAÍRA",
    Loc == "JARDIM BOTANICO" ~ "JARDIM BOTÂNICO",
    Loc == "JARDIM DAS AMERICAS" ~ "JARDIM DAS AMÉRICAS",
    Loc == "JUVEVE" ~ "JUVEVÊ",
    Loc == "LINDOIA" ~ "LINDÓIA",
    Loc == "MERCES" ~ "MERCÊS",
    Loc == "MOSSUNGUE" ~ "MOSSUNGUÊ",
    Loc == "PORTAO" ~ "PORTÃO",
    Loc == "REBOUCAS" ~ "REBOUÇAS",
    Loc == "SANTA CANDIDA" ~ "SANTA CÂNDIDA",
    Loc == "SANTA QUITERIA" ~ "SANTA QUITÉRIA",
    Loc == "SANTO INACIO" ~ "SANTO INÁCIO",
    Loc == "SAO BRAZ" ~ "SÃO BRAZ",
    Loc == "SAO FRANCISCO" ~ "SÃO FRANCISCO",
    Loc == "SAO JOAO" ~ "SÃO JOÃO",
    Loc == "SAO LOURENCO" ~ "SÃO LOURENÇO",
    Loc == "SAO MIGUEL" ~ "SÃO MIGUEL",
    Loc == "SEMINARIO" ~ "SEMINÁRIO",
    Loc == "SITIO CERCADO" ~ "SÍTIO CERCADO",
    Loc == "TABOAO" ~ "TABOÃO",
    Loc == "TARUMA" ~ "TARUMÃ",
    Loc == "UMBARA" ~ "UMBARÁ",
    TRUE ~ Loc
  )) %>%
  relocate(NOME, .after = "Loc")

sf_loc <- merge(sf_loc, types,
                by = "NOME") %>%
  select(-Loc, NOME) %>%
  relocate(Final_Score, .after = "NOME") %>%
  relocate(Type, .after = "Final_Score")

df_cwb$Loc <- toupper(df_cwb$Loc)

df_cwb <- df_cwb %>%
  mutate(NOME = case_when(
    Loc == "AGUA VERDE" ~ "ÁGUA VERDE",
    Loc == "AHU" ~ "AHÚ",
    Loc == "ALTO BOQUEIRAO" ~ "ALTO BOQUEIRÃO",
    Loc == "ALTO DA GLORIA" ~ "ALTO DA GLÓRIA",
    Loc == "BOQUEIRAO" ~ "BOQUEIRÃO",
    Loc == "CAPAO DA IMBUIA" ~ "CAPÃO DA IMBUIA",
    Loc == "CAPAO RASO" ~ "CAPÃO RASO",
    Loc == "CENTRO CIVICO" ~ "CENTRO CÍVICO",
    Loc == "CIC" ~ "CIDADE INDUSTRIAL DE CURITIBA",
    Loc == "GUAIRA" ~ "GUAÍRA",
    Loc == "JARDIM BOTANICO" ~ "JARDIM BOTÂNICO",
    Loc == "JARDIM DAS AMERICAS" ~ "JARDIM DAS AMÉRICAS",
    Loc == "JUVEVE" ~ "JUVEVÊ",
    Loc == "LINDOIA" ~ "LINDÓIA",
    Loc == "MERCES" ~ "MERCÊS",
    Loc == "MOSSUNGUE" ~ "MOSSUNGUÊ",
    Loc == "PORTAO" ~ "PORTÃO",
    Loc == "REBOUCAS" ~ "REBOUÇAS",
    Loc == "SANTA CANDIDA" ~ "SANTA CÂNDIDA",
    Loc == "SANTA QUITERIA" ~ "SANTA QUITÉRIA",
    Loc == "SANTO INACIO" ~ "SANTO INÁCIO",
    Loc == "SAO BRAZ" ~ "SÃO BRAZ",
    Loc == "SAO FRANCISCO" ~ "SÃO FRANCISCO",
    Loc == "SAO JOAO" ~ "SÃO JOÃO",
    Loc == "SAO LOURENCO" ~ "SÃO LOURENÇO",
    Loc == "SAO MIGUEL" ~ "SÃO MIGUEL",
    Loc == "SEMINARIO" ~ "SEMINÁRIO",
    Loc == "SITIO CERCADO" ~ "SÍTIO CERCADO",
    Loc == "TABOAO" ~ "TABOÃO",
    Loc == "TARUMA" ~ "TARUMÃ",
    Loc == "UMBARA" ~ "UMBARÁ",
    TRUE ~ Loc
  )) %>%
  relocate(NOME, .after = "Loc") %>%
  select(-Loc)

sf_loc <- merge(sf_loc, df_cwb,
                by = "NOME")

sf_loc$Type <- factor(sf_loc$Type,
                      levels = c("Low", "Mid", "High"))

sf_med_units <- sf_med_units %>%
  mutate(NOME_COMPL = iconv(NOME_COMPL, from = "latin1", to = "UTF-8")) %>%
  mutate(NOME = iconv(NOME, from = "latin1", to = "UTF-8")) %>%
  mutate(BAIRRO = iconv(BAIRRO, from = "latin1", to = "UTF-8"))


## 3.2. Palettes

palette <- colorFactor(palette = c("blue", "gold", "red"),
                       domain = sf_loc$Type)

viridis <- viridis(20)

palette_i <- colorNumeric(palette = "YlOrBr",
                          domain = sf_loc$Pop,
                          na.color = "transparent")

palette_ii <- colorNumeric(palette = "Blues",
                           domain = sf_loc$Dens,
                           na.color = "transparent")


## 3.3. Texts

text_loc <- paste(
  "Neighborhood: ", sf_loc$NOME, "<br/>",
  "Population: ", sf_loc$Pop,
  sep = "") %>%
  lapply(htmltools::HTML)

text_parks <- paste(
  "Name: ", sf_parks$TEXTO_MAPA, "<br/>",
  "Type: ", sf_parks$TIPO,
  sep = "") %>%
  lapply(htmltools::HTML)

text_squares <- paste(
  "Name: ", sf_squares$NOME, "<br/>",
  "Type: ", sf_squares$TIPO,
  sep = "") %>%
  lapply(htmltools::HTML)

text_cemiteries <- paste(
  "Name: ", sf_cemiteries$NOME,
  sep = "") %>%
  lapply(htmltools::HTML)

text_lakes <- paste(
  "Type: ", sf_lakes$TIPO,
  sep = "") %>%
  lapply(htmltools::HTML)

text_irregular <- paste(
  "Name: ", sf_irregular$NOME, "<br/>",
  "Type: ", sf_irregular$CATEG_2000,
  sep = "") %>%
  lapply(htmltools::HTML)

text_railroads <- paste(
  "Name: ", sf_railroads$TEXTO, "<br/>",
  "Type: ", sf_railroads$TIPO,
  sep = "") %>%
  lapply(htmltools::HTML)

text_cicloways <- paste(
  "Type: ", sf_cicloways$TIPO,
  sep = "") %>%
  lapply(htmltools::HTML)

text_cicloroutes <- paste(
  "Type: ", sf_cicloroutes$TIPO,
  sep = "") %>%
  lapply(htmltools::HTML)

text_rivers <- paste(
  "Type: ", sf_rivers$TIPO,
  sep = "") %>%
  lapply(htmltools::HTML)

text_density <- paste(
  "Name: ", sf_loc$NOME, "<br/>",
  "Density: ", sf_loc$Dens,
  sep = "") %>%
  lapply(htmltools::HTML)

text_estratification <- paste(
  "Neighborhood: ", sf_loc$NOME, "<br/>",
  "Type: ", sf_loc$Type,
  sep = "") %>%
  lapply(htmltools::HTML)


## 3.4. Icons

icon_hospitals <- awesomeIcons(
  icon = "plus",
  iconColor = "white",
  library = "glyphicon",
  markerColor = "red"
)

icon_cidadania <- awesomeIcons(
  icon = "star",
  iconColor = "white",
  library = "glyphicon",
  markerColor = "pink"
)

icon_sport <- awesomeIcons(
  icon = "futbol-o",
  iconColor = "white",
  library = "fa",
  markerColor = "lightgreen"
)

icon_terminals <- awesomeIcons(
  icon = "bus",
  iconColor = "white",
  library = "fa",
  markerColor = "blue"
)

icon_schools <- awesomeIcons(
  icon = "book",
  iconColor = "white",
  library = "glyphicon",
  markerColor = "orange"
)


## 3.5. OpenStreetMap geocoding
## NOTE: This section is set to NOT RUN by default (time-consuming geocoding process)

# df_housing <- read_excel("CWB_Housing_Wrangled.xlsx")
#
# df_housing <- df_housing %>%
#   filter(Outlier == 0) %>%
#   mutate(full_address = paste(Address, Neighborhood, "Curitiba, Paraná, Brazil", sep = ", ")) %>%
#   relocate(full_address, .after = "Neighborhood")
#
# batch_size <- 50
# n_rows <- nrow(df_housing)
# df_housing$latitude <- NA
# df_housing$longitude <- NA
#
# for (i in seq(1, n_rows, by = batch_size)) {
#   end_idx <- min(i + batch_size - 1, n_rows)
#   cat(sprintf("Geocoding rows %d to %d of %d...\n", i, end_idx, n_rows))
#
#   batch_indices <- i:end_idx
#   batch_df <- df_housing[batch_indices, ] %>%
#     select(full_address) %>%
#     geocode(full_address, method = "osm", lat = lat, long = lon)
#
#   df_housing$latitude[batch_indices] <- batch_df$lat
#   df_housing$longitude[batch_indices] <- batch_df$lon
#
#   write_xlsx(df_housing, "CWB_Housing_Geocoded.xlsx")
#
#   geocoded_so_far <- sum(!is.na(df_housing$latitude[1:end_idx]))
#   cat(sprintf("  Progress saved. %d/%d geocoded successfully.\n", geocoded_so_far, end_idx))
#
#   Sys.sleep(2)
# }
#
# cat(sprintf("\nDone! Geocoded %d of %d addresses.\n",
#             sum(!is.na(df_housing$latitude)), n_rows))
# cat(sprintf("Failed: %d addresses\n", sum(is.na(df_housing$latitude))))
#
# write_xlsx(df_housing, "CWB_Housing_Geocoded.xlsx")


## 3.6. Retry failed geocoding
## NOTE: This section is set to NOT RUN by default (time-consuming geocoding process)

# df_housing <- read_excel("CWB_Housing_Geocoded.xlsx")
#
# failed_indices <- which(is.na(df_housing$latitude))
# cat(sprintf("Found %d addresses that need re-geocoding\n", length(failed_indices)))
#
# if (length(failed_indices) > 0) {
#   batch_size <- 25
#
#   for (i in seq(1, length(failed_indices), by = batch_size)) {
#     end_idx <- min(i + batch_size - 1, length(failed_indices))
#     current_indices <- failed_indices[i:end_idx]
#
#     cat(sprintf("Retry batch %d to %d of %d failed addresses...\n",
#                 i, end_idx, length(failed_indices)))
#
#     batch_df <- df_housing[current_indices, ] %>%
#       select(full_address) %>%
#       geocode(full_address, method = "arcgis", lat = lat, long = lon)
#
#     successful <- !is.na(batch_df$lat)
#     df_housing$latitude[current_indices[successful]] <- batch_df$lat[successful]
#     df_housing$longitude[current_indices[successful]] <- batch_df$lon[successful]
#
#     Sys.sleep(3)
#   }
#
#   write_xlsx(df_housing, "CWB_Housing_Geocoded.xlsx")
# }
#
# still_failed <- sum(is.na(df_housing$latitude))
# cat(sprintf("After retry: %d addresses still failed\n", still_failed))


## 3.7. Retrieve df_housing with geoencoding data

df_housing <- read_excel("CWB_Housing_Geocoded.xlsx")


## 3.8. Map providers

providers <- c("OpenStreetMap", "Esri.WorldImagery")


## 3.9. UTM -> lat/long

utm_proj <- "+proj=utm +zone=22 +datum=WGS84"

sf_hospitals <- st_transform(sf_hospitals, "+proj=longlat +datum=WGS84")

sf_hospitals$Longitude <- st_coordinates(sf_hospitals)[, "X"]
sf_hospitals$Latitude <- st_coordinates(sf_hospitals)[, "Y"]

###

sf_health_units <- st_transform(sf_health_units, "+proj=longlat +datum=WGS84")

sf_health_units$Longitude <- st_coordinates(sf_health_units)[, "X"]
sf_health_units$Latitude <- st_coordinates(sf_health_units)[, "Y"]

###

sf_med_units <- st_transform(sf_med_units, "+proj=longlat +datum=WGS84")

sf_med_units$Longitude <- st_coordinates(sf_med_units)[, "X"]
sf_med_units$Latitude <- st_coordinates(sf_med_units)[, "Y"]

###

sf_emergency <- st_transform(sf_emergency, "+proj=longlat +datum=WGS84")

sf_emergency$Longitude <- st_coordinates(sf_emergency)[, "X"]
sf_emergency$Latitude <- st_coordinates(sf_emergency)[, "Y"]

###

sf_dentists <- st_transform(sf_dentists, "+proj=longlat +datum=WGS84")

sf_dentists$Longitude <- st_coordinates(sf_dentists)[, "X"]
sf_dentists$Latitude <- st_coordinates(sf_dentists)[, "Y"]

###

sf_sport <- st_transform(sf_sport, "+proj=longlat +datum=WGS84")

sf_sport$Longitude <- st_coordinates(sf_sport)[, "X"]
sf_sport$Latitude <- st_coordinates(sf_sport)[, "Y"]

###

sf_cidadania <- st_transform(sf_cidadania, "+proj=longlat +datum=WGS84")

sf_cidadania$Longitude <- st_coordinates(sf_cidadania)[, "X"]
sf_cidadania$Latitude <- st_coordinates(sf_cidadania)[, "Y"]

###

sf_terminals <- st_transform(sf_terminals, "+proj=longlat +datum=WGS84")

sf_terminals$Longitude <- st_coordinates(sf_terminals)[, "X"]
sf_terminals$Latitude <- st_coordinates(sf_terminals)[, "Y"]

###

sf_schools <- st_transform(sf_schools, "+proj=longlat +datum=WGS84")

sf_schools$Longitude <- st_coordinates(sf_schools)[, "X"]
sf_schools$Latitude <- st_coordinates(sf_schools)[, "Y"]


## 3.10. Map

mapa <- leaflet()
for(i in 1:length(providers)){
  mapa <- mapa %>%
    addProviderTiles(providers[i],
                     group = providers[i])
}

mapa <- mapa %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(data = sf_loc,
              group = "Neighborhoods",
              fillColor = "transparent",
              smoothFactor = 0.5,
              opacity = 1,
              stroke = TRUE,
              weight = 1,
              color = "black",
              highlight = highlightOptions(color = "white",
                                           weight = 2,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text_loc,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addPolygons(data = sf_parks,
              group = "Green Area",
              stroke = FALSE,
              fillColor = "green",
              color = "transparent",
              fillOpacity = 0.7,
              highlight = highlightOptions(color = "darkgreen",
                                           weight = 3,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text_parks,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addPolygons(data = sf_squares,
              group = "Green Area",
              stroke = FALSE,
              fillColor = "green",
              color = "transparent",
              fillOpacity = 0.7,
              highlight = highlightOptions(color = "darkgreen",
                                           weight = 3,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text_squares,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addPolygons(data = sf_lakes,
              group = "Hydrography",
              stroke = FALSE,
              fillColor = "blue",
              color = "transparent",
              fillOpacity = 0.7,
              highlight = highlightOptions(color = "darkblue",
                                           weight = 3,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text_lakes,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addPolylines(data = sf_rivers,
               group = "Hydrography",
               color = "blue",
               weight = 1.5,
               opacity = 0.7,
               highlight = highlightOptions(color = "darkblue",
                                            weight = 5,
                                            bringToFront = TRUE),
               label = text_rivers,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "13px",
                 direction = "auto")) %>%
  addPolylines(data = sf_railroads,
               group = "Railroads",
               color = "purple",
               weight = 1.5,
               opacity = 0.7,
               highlight = highlightOptions(color = "white",
                                            weight = 5,
                                            bringToFront = TRUE),
               label = text_railroads,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "13px",
                 direction = "auto")) %>%
  addPolygons(data = sf_irregular,
              group = "Irregular Settlements",
              stroke = FALSE,
              fillColor = "darkorange",
              color = "transparent",
              fillOpacity = 0.7,
              highlight = highlightOptions(color = "white",
                                           weight = 3,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text_irregular,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addPolylines(data = sf_cicloways,
               group = "Cycling",
               color = "red",
               weight = 1.5,
               opacity = 0.7,
               highlight = highlightOptions(color = "white",
                                            weight = 5,
                                            bringToFront = TRUE),
               label = text_cicloways,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "13px",
                 direction = "auto")) %>%
  addPolylines(data = sf_cicloroutes,
               group = "Cycling",
               color = "red",
               weight = 1.5,
               opacity = 0.7,
               highlight = highlightOptions(color = "white",
                                            weight = 5,
                                            bringToFront = TRUE),
               label = text_cicloways,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "13px",
                 direction = "auto")) %>%
  addPolygons(data = sf_cemiteries,
              group = "Cemiteries",
              stroke = FALSE,
              fillColor = "#5A5A5A",
              color = "transparent",
              fillOpacity = 0.7,
              highlight = highlightOptions(color = "white",
                                           weight = 3,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text_cemiteries,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addAwesomeMarkers(group = "Hospitals & Clinics",
                    data = sf_hospitals,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = ~NOME_COMPL,
                    icon = icon_hospitals) %>%
  addAwesomeMarkers(group = "Hospitals & Clinics",
                    data = sf_dentists,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = ~NOME_COMPL,
                    icon = icon_hospitals) %>%
  addAwesomeMarkers(group = "Hospitals & Clinics",
                    data = sf_med_units,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = ~NOME_COMPL,
                    icon = icon_hospitals) %>%
  addAwesomeMarkers(group = "Hospitals & Clinics",
                    data = sf_emergency,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = ~NOME_COMPL,
                    icon = icon_hospitals) %>%
  addAwesomeMarkers(group = "Bus Terminals",
                    data = sf_terminals,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = ~NOME_COMPL,
                    icon = icon_terminals) %>%
  addAwesomeMarkers(group = "Public Schools",
                    data = sf_schools,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = ~NOME_MAPA,
                    icon = icon_schools) %>%
  addAwesomeMarkers(group = "Cultural Centers",
                    data = sf_cidadania,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = ~NOME_MAPA,
                    icon = icon_cidadania) %>%
  addAwesomeMarkers(group = "Sport Centers",
                    data = sf_sport,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = ~NOME_MAPA,
                    icon = icon_sport) %>%
  addCircleMarkers(data = df_housing,
                   group = "Housing Listings",
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = 6,
                   color = "#6C3BAA",
                   fillColor = "#A47DAB",
                   fillOpacity = 1,
                   stroke = TRUE,
                   weight = 1,
                   popup = ~paste0("<b>", Address, "</b><br>",
                                   "Neighborhood: ", Neighborhood, "<br>",
                                   "Price: R$ ", format(Price, big.mark = ","), "<br>",
                                   "Area: ", Total_area_m2, " m²"),
                   clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = sf_loc,
              group = "Population",
              fillColor = ~palette_i(sf_loc$Pop),
              smoothFactor = 0.5,
              opacity = 1,
              stroke = TRUE,
              weight = 1,
              fillOpacity = 0.5,
              color = "black",
              highlight = highlightOptions(color = "white",
                                           weight = 2,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text_loc,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addPolygons(data = sf_loc,
              group = "Density",
              fillColor = ~palette_ii(sf_loc$Dens),
              smoothFactor = 0.5,
              opacity = 1,
              stroke = TRUE,
              weight = 1,
              fillOpacity = 0.5,
              color = "black",
              highlight = highlightOptions(color = "white",
                                           weight = 2,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text_density,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addPolygons(data = sf_loc,
              group = "Neighborhood Types",
              fillColor = ~palette(sf_loc$Type),
              smoothFactor = 0.5,
              opacity = 1,
              stroke = TRUE,
              weight = 1,
              fillOpacity = 0.5,
              color = "black",
              highlight = highlightOptions(color = "white",
                                           weight = 2,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text_estratification,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addLegend(group = "Neighborhood Types",
            data = sf_loc,
            pal = palette,
            values = ~Type,
            opacity = 1,
            title = "Type",
            position = "bottomright") %>%
  addLegend(group = "Population",
            data = sf_loc,
            pal = palette_i,
            values = ~Pop,
            opacity = 1,
            title = "Population",
            position = "bottomright") %>%
  addLegend(group = "Density",
            data = sf_loc,
            pal = palette_ii,
            values = ~Dens,
            opacity = 1,
            title = "Density",
            position = "bottomright") %>%
  addLayersControl(
    baseGroups = providers,
    overlayGroups = c("Neighborhoods",
                      "Green Area",
                      "Hydrography",
                      "Railroads",
                      "Cycling",
                      "Irregular Settlements",
                      "Hospitals & Clinics",
                      "Cemiteries",
                      "Bus Terminals",
                      "Public Schools",
                      "Cultural Centers",
                      "Sport Centers",
                      "Housing Listings",
                      "Population",
                      "Density",
                      "Neighborhood Types"),
    position = "topleft",
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("Green Area",
              "Hydrography",
              "Railroads",
              "Cycling",
              "Irregular Settlements",
              "Hospitals & Clinics",
              "Cemiteries",
              "Bus Terminals",
              "Public Schools",
              "Cultural Centers",
              "Housing Listings",
              "Sport Centers",
              "Population",
              "Density",
              "Neighborhood Types")) %>%
  addMeasure(primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             position = "bottomleft")

mapa

# -----------------------------------------------------------------------------
# 4. Datasets
# -----------------------------------------------------------------------------

## 4.1. Retrieving base data

private_schools <- read_excel("Private_schools.xlsx")

shoppings <- read_excel("Shopping_centers.xlsx")

green_area <- read_excel("Green_area.xlsx")


## 4.2. info_loc

info_loc <- df_cwb %>%
  select(NOME, Pop, Dens) %>%
  rename(NEIGHBORHOOD = NOME,
         POPULATION = Pop,
         DENSITY = Dens)

###

hospitals <- sf_hospitals %>%
  select(NEIGHBORHOOD = BAIRRO) %>%
  st_drop_geometry() %>%
  mutate(NEIGHBORHOOD = toupper(NEIGHBORHOOD)) %>%
  count(NEIGHBORHOOD) %>%
  rename(N_HOSPITALS = n) %>%
  arrange(NEIGHBORHOOD) %>%
  as_tibble()

###

terminals <- sf_terminals %>%
  select(NEIGHBORHOOD = BAIRRO) %>%
  mutate(NEIGHBORHOOD = toupper(NEIGHBORHOOD)) %>%
  st_drop_geometry() %>%
  count(NEIGHBORHOOD) %>%
  rename(N_TERMINALS = n)

###

public_schools <- sf_schools %>%
  select(NEIGHBORHOOD = BAIRRO) %>%
  mutate(NEIGHBORHOOD = toupper(NEIGHBORHOOD)) %>%
  st_drop_geometry() %>%
  count(NEIGHBORHOOD) %>%
  rename(N_PUBLIC_SCHOOLS = n)

private_schools <- private_schools %>%
  rename(NEIGHBORHOOD = BAIRRO) %>%
  mutate(NEIGHBORHOOD = toupper(NEIGHBORHOOD)) %>%
  group_by(NEIGHBORHOOD) %>%
  summarise(N_PRIVATE_SCHOOLS = n()) %>%
  arrange(NEIGHBORHOOD) %>%
  as_tibble()

###

cicloways <- sf_cicloways %>%
  rename(NEIGHBORHOOD = BAIRRO) %>%
  st_drop_geometry() %>%
  group_by(NEIGHBORHOOD) %>%
  summarise(CICLOWAYS = sum(SHAPE_LEN)) %>%
  arrange(NEIGHBORHOOD) %>%
  as_tibble()

###

cidadania <- sf_cidadania %>%
  select(NEIGHBORHOOD = BAIRRO) %>%
  mutate(NEIGHBORHOOD = toupper(NEIGHBORHOOD)) %>%
  st_drop_geometry() %>%
  count(NEIGHBORHOOD) %>%
  rename(N_CULTURE_FACILITIES = n)

###

green_area <- green_area %>%
  rename(NEIGHBORHOOD = Neighborhood) %>%
  mutate(NEIGHBORHOOD = toupper(NEIGHBORHOOD)) %>%
  group_by(NEIGHBORHOOD) %>%
  summarise(N_PARKS = n()) %>%
  arrange(NEIGHBORHOOD) %>%
  as_tibble()

###

shoppings <- shoppings %>%
  rename(NEIGHBORHOOD = BAIRRO) %>%
  mutate(NEIGHBORHOOD = toupper(NEIGHBORHOOD)) %>%
  group_by(NEIGHBORHOOD) %>%
  summarise(N_SHOPPINGS = n()) %>%
  arrange(NEIGHBORHOOD) %>%
  as_tibble()

###

loc <- sf_loc %>%
  select(NEIGHBORHOOD = NOME,
         AREA = SHAPE_AREA) %>%
  st_drop_geometry() %>%
  as_tibble()

###

lista_dfs <- list(hospitals, terminals, public_schools, private_schools,
                  cicloways, cidadania, green_area, shoppings, loc)

info_loc <- reduce(lista_dfs, ~left_join(.x, .y, by = "NEIGHBORHOOD"),
                   .init = info_loc) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

info_loc <- info_loc %>%
  mutate(HOSPITALS = as.integer(N_HOSPITALS > 0),
         TERMINALS = as.integer(N_TERMINALS > 0),
         PRIVATE_SCHOOLS = as.integer(N_PRIVATE_SCHOOLS > 0),
         PUBLIC_SCHOOLS = as.integer(N_PUBLIC_SCHOOLS > 0),
         CULTURE_FACILITIES = as.integer(N_CULTURE_FACILITIES > 0),
         GREEN_AREA = as.integer(N_PARKS > 0),
         CICLOWAYS = as.integer(CICLOWAYS > 0),
         SHOPPINGS = as.integer(N_SHOPPINGS > 0)) %>%
  select(-c(N_HOSPITALS, N_TERMINALS, N_PUBLIC_SCHOOLS, N_PRIVATE_SCHOOLS,
            AREA, N_PARKS, N_CULTURE_FACILITIES, N_SHOPPINGS))

write_xlsx(info_loc,
           path = "Info_Neighborhoods.xlsx")


## 4.3. Merging the information with the main dataset

housing_df <- read_excel("CWB_Housing_Geocoded.xlsx")

housing_df <- housing_df %>%
  mutate(NEIGHBORHOOD = toupper(Neighborhood)) %>%
  relocate(NEIGHBORHOOD, .before = "Category") %>%
  relocate(Price, .after = "NEIGHBORHOOD") %>%
  filter(Outlier == 0) %>%
  select(-c(Address, Neighborhood, Outlier, URL))

housing_df <- left_join(housing_df, info_loc,
                        by = "NEIGHBORHOOD")

types <- read_excel("Neighborhood_classification.xlsx")

types <- types %>%
  rename(NEIGHBORHOOD = Loc) %>%
  mutate(NEIGHBORHOOD = toupper(NEIGHBORHOOD)) %>%
  select(-Final_Score)

housing_df <- left_join(housing_df, types,
                        by = "NEIGHBORHOOD")

housing_df <- housing_df %>%
  relocate(Type, .before = "NEIGHBORHOOD") %>%
  mutate(Type = as.factor(Type),
         Apartment = case_when(Category == "Apartmento" ~ 1,
                              TRUE ~ 0)) %>%
  relocate(Apartment, .after = "Price") %>%
  select(-Category)


## 4.4. Separating the data frames by stratification for later regression

high_tier <- housing_df %>%
  filter(Type == "High") %>%
  select(-Type) %>%
  relocate(full_address, .before = "NEIGHBORHOOD") %>%
  relocate(latitude, .after = "full_address") %>%
  relocate(longitude, .after = "latitude")

write_xlsx(high_tier,
           path = "High_tier.xlsx")

###

medium_tier <- housing_df %>%
  filter(Type == "Mid") %>%
  select(-Type) %>%
  relocate(full_address, .before = "NEIGHBORHOOD") %>%
  relocate(latitude, .after = "full_address") %>%
  relocate(longitude, .after = "latitude")

write_xlsx(medium_tier,
           path = "Medium_tier.xlsx")

###

low_tier <- housing_df %>%
  filter(Type == "Low") %>%
  select(-Type) %>%
  relocate(full_address, .before = "NEIGHBORHOOD") %>%
  relocate(latitude, .after = "full_address") %>%
  relocate(longitude, .after = "latitude")

write_xlsx(low_tier,
           path = "Low_tier.xlsx")