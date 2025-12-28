# =============================================================================
# 1. PCA of Curitiba's Neighborhoods
# Author: Lucas Salamuni
# Date: 2025-12-20
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

## 1.1. Retrieving Packages

pacotes <- c("factoextra", "readxl", "ggplot2", "patchwork", "leaflet",
             "corrplot", "plotly", "data.table", "EFAtools", "psych", "writexl",
             "sf", "sp", "mapview", "webshot", "htmltools", "tidyverse")

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
# 2. Dataset
# -----------------------------------------------------------------------------

## 2.1. Verifying the dataset

df_cwb <- read_excel("CWB_PCA.xlsx",
                     sheet = "Data")

summary(df_cwb)

str(df_cwb)

head(df_cwb)

# -----------------------------------------------------------------------------
# 3. PCA
# -----------------------------------------------------------------------------

## 3.1. Correlation matrix

var <- df_cwb %>%
  select(Inc,
         `Pop_1/2_SM`,
         Lit,
         Grow,
         Dens,
         Pop) %>%
  as.matrix()

head(var)

cor_matrix <- var %>%
  corr.test(method = "pearson")

cor_matrix$r %>%
  round(3) %>%
  head()

cor_matrix$p %>%
  round(3) %>%
  head()

ggplotly(
  var %>%
    cor() %>%
    as.data.frame() %>%
    rownames_to_column(var = "Var1") %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation") %>%
    ggplot() +
    geom_tile(aes(x = Var1,
                  y = Var2,
                  fill = Correlation)) +
    geom_text(aes(x = Var1,
                  y = Var2,
                  label = format(round(Correlation, 3))),
              size = 3) +
    scale_fill_gradient2(low = "blue",
                         mid = "gold",
                         high = "red",
                         midpoint = 0) +
    labs(x = NULL,
         y = NULL) +
    theme_bw(base_size = 9)
)


## 3.2. KMO & Bartlett

EFAtools::KMO(var)

EFAtools::BARTLETT(var)


## 3.3. pca_prcomp()

pca <- var %>%
  prcomp(center = TRUE,
         scale. = TRUE)

loadings <- pca$rotation
scores <- as.data.frame(pca$x)
variance <- (pca$sdev)^2        # Kaiser Criterion: Var > 1 passes (thus, PC1 and PC2 are selected)

scores$Loc <- df_cwb$Loc
scores <- scores %>%
  relocate(Loc, .before = 1)

pca

resumo_pca <- summary(pca)

resumo_pca$importance

variancia <- data.frame(PC1 = resumo_pca$importance[2,1],
                        PC2 = resumo_pca$importance[2,2]) %>%
  t() %>%
  as.data.frame() %>%
  rename("Shared Variance" = V1)

variancia

# Note 1: Shared Variance = multiplication of the squared factor loadings by the corresponding value of the principal component's variance.


## 3.3. Plots and Exploratory Analysis

corrplot(loadings, is.corr = FALSE)

fviz_eig(pca, addlabels = TRUE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Explained Variance for each Principal Component",
       x = "Principal Components",
       y = "Percentage of Explained Variance")

# Note 2: The first plot is a visualization of the rotation matrix (loadings) using the corrplot() function, which is useful for displaying the relationships between variables and principal components in a PCA analysis.
# Note 3: The second plot is used to visualize the proportion of explained variance by each principal component in a PCA. It shows a bar plot that represents the proportion of variance explained by each principal component, which is helpful for assessing the relative importance of each component in explaining the total variability in the data.


## 3.4. Scores and Factor Loadings

scores <- scores %>%
  select(Loc, PC1, PC2)

scores

loadings <- loadings %>%
  as.data.frame() %>%
  select(PC1, PC2)

loadings

fviz_pca_var(pca, col.var = "cos2") +
  scale_color_gradient2(low = "blue",
                        mid = "gold",
                        high = "red",
                        midpoint = 0.6) +
  theme(axis.title.x = element_text(hjust = 1,
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(hjust = 1,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  labs(title = "PCA Plot",
       color = "Relevance")

# Note 4: According to Kaiser's criterion, only eigenvalues greater than 1 should be selected (i.e., PC1 and PC2).
# Note 5: Scores = coordinates of the observations in the new variable space created by the principal components.
# Note 6: Loadings = factor loadings (correlations between the original variables and the principal components).
# Note 7: The plot shows the variables in the PCA space, with colors indicating the quality of the representation of the variables in this space. Variables colored closer to red indicate a more accurate representation in the PCA space, while variables closer to blue indicate a less accurate representation. This helps identify which variables contribute more significantly to the principal components identified in the PCA, with those farther from the center of the plot having a greater influence on the principal components.


## 3.5. Weighted Sum

estratificacao <- data.frame("Loc" = df_cwb$Loc,
                             "Final_Score" = ((scores[,2] * variancia[1,] +
                                                 (scores[,3] * variancia[2,])))) %>%
  mutate(Type = case_when(
    Final_Score < -0.01 ~ "Low",
    Final_Score > 1.00 ~ "High",
    TRUE ~ "Mid"
  ))

print(estratificacao)

write_xlsx(estratificacao,
           path = "Neighborhood_classification.xlsx")

# Stratification performed according to Favero (2005, p. 92).

# -----------------------------------------------------------------------------
# 4. Geoprocessing
# -----------------------------------------------------------------------------

## 4.1. Reading Curitiba's shapefile

sf <- st_read("DIVISA_DE_BAIRROS.shp")

st_crs(sf)


## 4.2. Creating the map with neighborhood boundaries (localities)

sf <- st_transform(sf, "+proj=longlat +datum=WGS84")

mapa_i <- leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(data = sf,
              fillColor = "transparent",
              color = "black",
              weight = 2,
              opacity = 1)

## mapshot(mapa_i, file = "mapa_i.png")

## knitr::include_graphics("mapa_i.png")

mapa_i


## 4.3. Creating the map with stratifications by neighborhood

estratificacao$Loc <- toupper(estratificacao$Loc)

estratificacao <- estratificacao %>%
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

sf <- merge(sf, estratificacao,
            by = "NOME") %>%
  select(-Loc, NOME) %>%
  relocate(Final_Score, .after = "NOME") %>%
  relocate(Type, .after = "Final_Score")

palette <- colorFactor(palette = c("red", "blue", "gold"),
                       domain = sf$type)

text <- paste(
  "Neighborhood: ", sf$NOME, "<br/>",
  "Type: ", sf$Type,"<br/>",
  "Score: ", sf$Final_Score,
  sep = "") %>%
  lapply(htmltools::HTML)

mapa_ii <- leaflet(sf) %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(fillColor = ~palette(sf$Type),
              smoothFactor = 0.5,
              opacity = 1,
              stroke = TRUE,
              weight = 1,
              fillOpacity = 0.45,
              color = "black",
              highlight = highlightOptions(color = "lightgrey",
                                           weight = 2,
                                           bringToFront = TRUE,
                                           stroke = TRUE),
              label = text,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
              )) %>%
  addLegend(pal = palette,
            values = ~Type,
            opacity = 1,
            title = "Category",
            position = "bottomright")

## mapshot(mapa_ii, file = "mapa_ii.png")

## knitr::include_graphics("mapa_ii.png")

## saveWidget(mapa_ii, "temp.html", selfcontained = FALSE)

mapa_ii