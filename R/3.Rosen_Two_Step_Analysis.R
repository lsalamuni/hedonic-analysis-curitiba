# =============================================================================
# 3. Hedonic Regression - Rosen's Two Steps
# Author: Lucas Salamuni
# Date: 2025-12-23
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

## 1.1. Retrieving Packages

pacotes <- c("tidyverse", "factoextra", "readxl", "ggplot2", "writexl", "spdep",
             "cowplot", "car", "lmtest", "psych", "car", "tseries", "MASS",
             "whitestrap", "jtools", "sandwich", "foreign", "plm", "lm.beta",
             "conleyreg", "sf")

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
# 2. Exploratory Analysis
# -----------------------------------------------------------------------------

## 2.1. Accessing the Data

low_tier <- read_excel("C:/Users/Lucas/OneDrive/Lucas Salamuni/Universidades/Universitat zu Koln/Semester 3/RG Econometrics/Proposal/Datasets/Low_tier.xlsx")

mid_tier <- read_excel("C:/Users/Lucas/OneDrive/Lucas Salamuni/Universidades/Universitat zu Koln/Semester 3/RG Econometrics/Proposal/Datasets/Medium_tier.xlsx")

high_tier <- read_excel("C:/Users/Lucas/OneDrive/Lucas Salamuni/Universidades/Universitat zu Koln/Semester 3/RG Econometrics/Proposal/Datasets/High_tier.xlsx")


## 2.2. Dividing into low, mid and high tiers

tibble("Low" = as.numeric(count(low_tier)),
       "Mid" = as.numeric(count(mid_tier)),
       "High" = as.numeric(count(high_tier)),
       "TOTAL" = as.numeric(count(low_tier)) + as.numeric(count(mid_tier)) + as.numeric(count(high_tier)))


## 2.3. Checking price trends using a linear function form

low_tier %>%
  ggplot(aes(x = Total_area_m2,
             y = Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red")

mid_tier %>%
  ggplot(aes(x = Total_area_m2,
             y = Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red")

high_tier %>%
  ggplot(aes(x = Total_area_m2,
             y = Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red")

# NOTE 1 = Linear models assume that Y exhibits constant absolute increases (or decreases) given absolute variations in X. Thus, the marginal change in the expected value of Y is the same for any value of X.


## 2.4. Checking price trends using a semi-logarithmic function form

low_tier %>%
  ggplot(aes(x = Total_area_m2,
             y = Price)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(trans = "log") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red")

mid_tier %>%
  ggplot(aes(x = Total_area_m2,
             y = Price)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(trans = "log") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red")


high_tier %>%
  ggplot(aes(x = Total_area_m2,
             y = Price)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(trans = "log") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red")

# NOTE 2 = Semi-logarithmic (or log-linear) models assume that Y exhibits exponential growth (or decay) in relation to absolute variations in X.


## 2.5. Checking price trends using a double logarithmic function form

low_tier %>%
  ggplot(aes(x = Total_area_m2,
             y = Price)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red")

mid_tier %>%
  ggplot(aes(x = Total_area_m2,
             y = Price)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red")

high_tier %>%
  ggplot(aes(x = Total_area_m2,
             y = Price)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red")

# NOTE 3 = Logarithmic (or double logarithmic) models assume that Y exhibits constant relative variations given relative variations in X. Thus, the beta coefficient would be a constant measure of the elasticity of Y in relation to X, meaning that it considers that relative variations in X are the same for any values of Xi and Yi.
# NOTE 4 = It seems that the functional form that best fits the model, across all tiers, is indeed the logarithmic form (log-log). In other words, the price of properties in its natural logarithmic form (lnY) is a function of the logarithms of the intrinsic characteristics of the property (I) and the urban amenities of its respective locality/neighborhood (A).

# -----------------------------------------------------------------------------
# 3. Box-Cox transformation to select functional-form
# -----------------------------------------------------------------------------

## 3.1. Low tier

model_intrinsic <- lm(data = low_tier,
                      Price ~ Total_area_m2 + Usable_area_m2 + Age_years + Apartment + Off_plan + Party_room +
                      Game_room + Gym + Pool + Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse + Cameras +
                      Balcony + Playground + PARKING1 + PARKING2 + BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                      BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4)


bc <- boxcox(model_intrinsic, lambda = seq(-2,2, by = 0.1))

bc$x[which.max(bc$y)]


## 3.2. Mid tier

model_intrinsic <- lm(data = mid_tier,
                      Price ~ Total_area_m2 + Usable_area_m2 + Age_years + Apartment + Off_plan + Party_room +
                      Game_room + Gym + Pool + Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse + Cameras +
                      Balcony + Playground + PARKING1 + PARKING2 + BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                      BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4)


bc <- boxcox(model_intrinsic, lambda = seq(-2,2, by = 0.1))

bc$x[which.max(bc$y)]


## 3.3. High tier

model_intrinsic <- lm(data = high_tier,
                      Price ~ Total_area_m2 + Usable_area_m2 + Age_years + Apartment + Off_plan + Party_room +
                      Game_room + Gym + Pool + Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse + Cameras +
                      Balcony + Playground + PARKING1 + PARKING2 + BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                      BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4)


bc <- boxcox(model_intrinsic, lambda = seq(-2,2, by = 0.1))

bc$x[which.max(bc$y)]

# -----------------------------------------------------------------------------
# 4. Rosen's First Stage
# -----------------------------------------------------------------------------

## 4.1. Low tier

### 4.1.1. First Regression (First Stage of Rosen - Only Intrinsic Characteristics)

low_tier_ln <- low_tier %>%
  mutate_at(vars(Price, Total_area_m2), log) %>%
  mutate(Age_years = log(Age_years + 1)) %>%
  mutate_at(vars(Price, Total_area_m2),
            ~ifelse(is.infinite(.), 0, .)) %>%
  dplyr::select(-c(NEIGHBORHOOD, Apartment, Usable_area_m2, POPULATION, DENSITY, CICLOWAYS,
                   GREEN_AREA, HOSPITALS, TERMINALS, PRIVATE_SCHOOLS, PUBLIC_SCHOOLS,
                   CULTURE_FACILITIES, SHOPPINGS, latitude, longitude, full_address))

low_lm_ln <- lm(Price ~ .,
                data = low_tier_ln)

## Plots

plot(low_lm_ln)
qplot(low_lm_ln$residuals)

## Normality

jarque.bera.test(low_lm_ln$residuals)

## Heteroskedasticity

white_test(low_lm_ln)

## Spatial autocorrelation

low_tier_sf <- low_tier %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

coords <- st_coordinates(low_tier_sf)
knn <- knearneigh(coords, k = 5)
nb <- knn2nb(knn)
listw <- nb2listw(nb, style = "W")

residuals_low <- residuals(low_lm_ln)

moran.test(residuals_low, listw)

## Multicolinearity

car::vif(low_lm_ln)

## Summary

summary(low_lm_ln)


### 4.1.2. Addressing the spatial correlation and heteroscedasticity through Conley SEs

low_tier_conley <- low_tier %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  mutate(Price = log(Price),
         Total_area_m2 = log(Total_area_m2),
         Age_years = log(Age_years + 1))

low_conley <- conleyreg(Price ~ Total_area_m2 + Age_years +
                          Off_plan + Party_room + Game_room + Gym + Pool +
                          Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                          Cameras + Balcony + Playground +
                          PARKING1 + PARKING2 +
                          BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                          BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4,
                        data = low_tier_conley,
                        lat = "latitude",
                        lon = "longitude",
                        dist_cutoff = 2)

low_conley


## 4.2. Medium tier

### 4.2.1. First Regression (First Stage of Rosen - Only Intrinsic Characteristics)

mid_tier_ln <- mid_tier %>%
  mutate_at(vars(Price, Total_area_m2), log) %>%
  mutate(Age_years = log(Age_years + 1)) %>%
  mutate_at(vars(Price, Total_area_m2),
            ~ifelse(is.infinite(.), 0, .)) %>%
  dplyr::select(-c(NEIGHBORHOOD, Apartment, Usable_area_m2, POPULATION, DENSITY, CICLOWAYS,
                   GREEN_AREA, HOSPITALS, TERMINALS, PRIVATE_SCHOOLS, PUBLIC_SCHOOLS,
                   CULTURE_FACILITIES, SHOPPINGS, latitude, longitude, full_address))

mid_lm_ln <- lm(Price ~ .,
                data = mid_tier_ln)

## Plots

plot(mid_lm_ln)
qplot(mid_lm_ln$residuals)

## Normality

jarque.bera.test(mid_lm_ln$residuals)

## Heteroskedasticity

white_test(mid_lm_ln)

## Spatial autocorrelation

mid_tier_sf <- mid_tier %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

coords <- st_coordinates(mid_tier_sf)
knn <- knearneigh(coords, k = 5)
nb <- knn2nb(knn)
listw <- nb2listw(nb, style = "W")

residuals_mid <- residuals(mid_lm_ln)

moran.test(residuals_mid, listw)

## Multicolinearity

car::vif(mid_lm_ln)

## Summary

summary(mid_lm_ln)


### 4.2.2. Addressing the spatial correlation and heteroscedasticity through Conley SEs

mid_tier_conley <- mid_tier %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  mutate(Price = log(Price),
         Total_area_m2 = log(Total_area_m2),
         Age_years = log(Age_years + 1))

mid_conley <- conleyreg(Price ~ Total_area_m2 + Age_years +
                          Off_plan + Party_room + Game_room + Gym + Pool +
                          Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                          Cameras + Balcony + Playground +
                          PARKING1 + PARKING2 +
                          BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                          BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4,
                        data = mid_tier_conley,
                        lat = "latitude",
                        lon = "longitude",
                        dist_cutoff = 2)

mid_conley

## 4.3. High tier

### 4.3.1. First Regression (First Stage of Rosen - Only Intrinsic Characteristics)

high_tier_ln <- high_tier %>%
  mutate_at(vars(Price, Total_area_m2), log) %>%
  mutate(Age_years = log(Age_years + 1)) %>%
  mutate_at(vars(Price, Total_area_m2),
            ~ifelse(is.infinite(.), 0, .)) %>%
  dplyr::select(-c(NEIGHBORHOOD, Apartment, Usable_area_m2, POPULATION, DENSITY, CICLOWAYS,
                   GREEN_AREA, HOSPITALS, TERMINALS, PRIVATE_SCHOOLS, PUBLIC_SCHOOLS,
                   CULTURE_FACILITIES, SHOPPINGS, latitude, longitude, full_address))

high_lm_ln <- lm(Price ~ .,
                 data = high_tier_ln)

# Plots

plot(high_lm_ln)
qplot(high_lm_ln$residuals)

## Normality

jarque.bera.test(high_lm_ln$residuals)

## Heteroskedasticity

white_test(high_lm_ln) # this time, no heteroskedasticity

## Spatial autocorrelation

high_tier_sf <- high_tier %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

coords <- st_coordinates(high_tier_sf)
knn <- knearneigh(coords, k = 5)
nb <- knn2nb(knn)
listw <- nb2listw(nb, style = "W")

residuals_high <- residuals(high_lm_ln)

moran.test(residuals_high, listw)

## Multicolinearity

car::vif(high_lm_ln)

## Summary

summary(high_lm_ln)


### 4.3.2. Addressing the spatial correlation and heteroscedasticity through Conley SEs

high_tier_conley <- high_tier %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  mutate(Price = log(Price),
         Total_area_m2 = log(Total_area_m2),
         Age_years = log(Age_years + 1))

high_conley <- conleyreg(Price ~ Total_area_m2 + Age_years +
                           Off_plan + Party_room + Game_room + Gym + Pool +
                           Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                           Cameras + Balcony + Playground +
                           PARKING1 + PARKING2 +
                           BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                           BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4,
                         data = high_tier_conley,
                         lat = "latitude",
                         lon = "longitude",
                         dist_cutoff = 2)

high_conley

# -----------------------------------------------------------------------------
# 5. Rosen's Second Stage
# -----------------------------------------------------------------------------

## 5.1. Low tier (Second Stage of Rosen - Both Intrinsic and Extrinsic Characteristics)

# Coefficient from first stage
beta_area_low <- coef(low_lm_ln)["Total_area_m2"]

# Dependent variable for second stage
low_tier$price_m2 <- beta_area_low*(low_tier$Price/low_tier$Total_area_m2)

# Second stage regression
low_supply <- lm(price_m2 ~
                   # Intrinsic (z)
                   log(Total_area_m2) + log(Age_years + 1) +
                   Off_plan + Party_room + Game_room + Gym + Pool +
                   Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                   Cameras + Balcony + Playground +
                   PARKING1 + PARKING2 +
                   BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                   BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4 +
                   # Y2 - Supply shifters (continuous = log)
                   log(DENSITY) + log(POPULATION) +
                   # W - Location variables
                   GREEN_AREA + CICLOWAYS +
                   HOSPITALS + TERMINALS + PRIVATE_SCHOOLS +
                   PUBLIC_SCHOOLS + CULTURE_FACILITIES + SHOPPINGS,
                 data = low_tier)

## Plots

plot(low_supply)
qplot(low_supply$residuals)

## Normality

jarque.bera.test(low_supply$residuals)

## Heteroskedasticiy

white_test(low_supply)

## Multicolinearity

car::vif(low_supply)

## Summary

summary(low_supply)

## Conley SEs for spatial autocorrelation and heteroskedasticity

beta_area_low <- coef(low_conley)["Total_area_m2"]

low_tier_conley$price_m2 <- beta_area_low*(low_tier_conley$Price/low_tier_conley$Total_area_m2)

low_supply_conley <- conleyreg(price_m2 ~
                                 Total_area_m2 + Age_years +
                                 Off_plan + Party_room + Game_room + Gym + Pool +
                                 Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                                 Cameras + Balcony + Playground +
                                 PARKING1 + PARKING2 +
                                 BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                                 BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4 +
                                 # Y2 - Supply shifters
                                 log(POPULATION) + log(DENSITY) +
                                 # W - Location variables
                                 CICLOWAYS + GREEN_AREA +
                                 HOSPITALS + TERMINALS + PRIVATE_SCHOOLS +
                                 PUBLIC_SCHOOLS + CULTURE_FACILITIES + SHOPPINGS,
                               data = low_tier_conley,
                               lat = "latitude",
                               lon = "longitude",
                               dist_cutoff = 2)

low_supply_conley


## 5.2. Mid tier (Second Stage of Rosen - Both Intrinsic and Extrinsic Characteristics)

# Coefficient from first stage
beta_area_mid <- coef(mid_lm_ln)["Total_area_m2"]

# Dependent variable for second stage
mid_tier$price_m2 <- beta_area_mid*(mid_tier$Price/mid_tier$Total_area_m2)

# Second stage regression
mid_supply <- lm(price_m2 ~
                   # Intrinsic (z)
                   log(Total_area_m2) + log(Age_years + 1) +
                   Off_plan + Party_room + Game_room + Gym + Pool +
                   Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                   Cameras + Balcony + Playground +
                   PARKING1 + PARKING2 +
                   BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                   BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4 +
                   # Y2 - Supply shifters (continuous = log)
                   log(DENSITY) + log(POPULATION) +
                   # W - Location variables
                   GREEN_AREA + CICLOWAYS +
                   HOSPITALS + TERMINALS + PRIVATE_SCHOOLS +
                   PUBLIC_SCHOOLS + CULTURE_FACILITIES + SHOPPINGS,
                 data = mid_tier)

## Plots

plot(mid_supply)
qplot(mid_supply$residuals)

## Normality

jarque.bera.test(mid_supply$residuals)

## Heteroskedasticity

white_test(mid_supply)

## Multicolinearity

car::vif(mid_supply)

## Summary

summary(mid_supply)

## Conley SEs for spatial autocorrelation and heteroskedasticity

beta_area_mid <- coef(mid_conley)["Total_area_m2"]

mid_tier_conley$price_m2 <- beta_area_mid*(mid_tier_conley$Price/mid_tier_conley$Total_area_m2)

mid_supply_conley <- conleyreg(price_m2 ~
                                 Total_area_m2 + Age_years +
                                 Off_plan + Party_room + Game_room + Gym + Pool +
                                 Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                                 Cameras + Balcony + Playground +
                                 PARKING1 + PARKING2 +
                                 BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                                 BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4 +
                                 # Y2 - Supply shifters
                                 log(POPULATION) + log(DENSITY) +
                                 # W - Location variables
                                 CICLOWAYS + GREEN_AREA +
                                 HOSPITALS + TERMINALS + PRIVATE_SCHOOLS +
                                 PUBLIC_SCHOOLS + CULTURE_FACILITIES + SHOPPINGS,
                               data = mid_tier_conley,
                               lat = "latitude",
                               lon = "longitude",
                               dist_cutoff = 2)

mid_supply_conley


## 5.3. High tier (Second Stage of Rosen - Both Intrinsic and Extrinsic Characteristics)

# Coefficient from first stage
beta_area_high <- coef(high_lm_ln)["Total_area_m2"]

# Dependent variable for second stage
high_tier$price_m2 <- beta_area_high*(high_tier$Price/high_tier$Total_area_m2)

# Second stage regression
high_supply <- lm(price_m2 ~
                   # Intrinsic (z)
                   log(Total_area_m2) + log(Age_years + 1) +
                   Off_plan + Party_room + Game_room + Gym + Pool +
                   Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                   Cameras + Balcony + Playground +
                   PARKING1 + PARKING2 +
                   BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                   BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4 +
                   # Y2 - Supply shifters (continuous = log)
                   log(DENSITY) + log(POPULATION) +
                   # W - Location variables
                   GREEN_AREA + CICLOWAYS +
                   HOSPITALS + TERMINALS +
                   PUBLIC_SCHOOLS + SHOPPINGS,
                 data = high_tier)

## Plots

plot(high_supply)
qplot(high_supply$residuals)

## Normality

jarque.bera.test(high_supply$residuals)

## Heteroskedasticity

white_test(high_supply)

## Multicolinearity

car::vif(high_supply)

## Summary

summary(high_supply)

## Conley SEs for spatial autocorrelation and heteroskedasticity

beta_area_high <- coef(high_conley)["Total_area_m2"]

high_tier_conley$price_m2 <- beta_area_high*(high_tier_conley$Price/high_tier_conley$Total_area_m2)

high_supply_conley <- conleyreg(price_m2 ~
                                  Total_area_m2 + Age_years +
                                  Off_plan + Party_room + Game_room + Gym + Pool +
                                  Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                                  Cameras + Balcony + Playground +
                                  PARKING1 + PARKING2 +
                                  BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                                  BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4 +
                                  # Y2 - Supply shifters
                                  log(POPULATION) + log(DENSITY) +
                                  # W - Location variables
                                  CICLOWAYS + GREEN_AREA +
                                  HOSPITALS + TERMINALS +
                                  PUBLIC_SCHOOLS + SHOPPINGS,
                                data = high_tier_conley,
                                lat = "latitude",
                                lon = "longitude",
                                dist_cutoff = 2)

high_supply_conley

# Note 1: PRIVATE_SCHOOLS and CULTURAL_FACILITIES removed from the high tier due to multicolinearity.

# -----------------------------------------------------------------------------
# 6. Wald Test for Market Segmentation
# -----------------------------------------------------------------------------

## 6.1. Robust Wald Test with Conley SEs (First Stage)

pooled_data <- bind_rows(low_tier %>% mutate(tier = "Low"),
                         mid_tier %>% mutate(tier = "Mid"),
                         high_tier %>% mutate(tier = "High")) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  mutate(Price = log(Price),
         Total_area_m2 = log(Total_area_m2),
         Age_years = log(Age_years + 1),
         tier = factor(tier, levels = c("Low", "Mid", "High")))

unrestricted_lm <- lm(Price ~ tier * (Total_area_m2 + Age_years) +
                        Off_plan + Party_room + Game_room + Gym + Pool +
                        Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                        Cameras + Balcony + Playground +
                        PARKING1 + PARKING2 +
                        BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                        BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4,
                      data = pooled_data)

unrestricted_conley <- conleyreg(Price ~ tier * (Total_area_m2 + Age_years) +
                                   Off_plan + Party_room + Game_room + Gym + Pool +
                                   Sauna + BBQ + Gourmet_space + Sports_court + Guardhouse +
                                   Cameras + Balcony + Playground +
                                   PARKING1 + PARKING2 +
                                   BEDROOM1 + BEDROOM2 + BEDROOM3 + BEDROOM4 +
                                   BATHROOM1 + BATHROOM2 + BATHROOM3 + BATHROOM4,
                                 data = pooled_data,
                                 lat = "latitude",
                                 lon = "longitude",
                                 dist_cutoff = 2)

unrestricted_conley

## Wald test (H0: tier interactions = 0)

interaction_terms <- names(coef(unrestricted_lm))[grepl("^tier", names(coef(unrestricted_lm)))]

linearHypothesis(unrestricted_lm, interaction_terms, vcov = vcovHC(unrestricted_lm, type = "HC3"))