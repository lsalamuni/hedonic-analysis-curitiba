# Hedonic Pricing Analysis of Curitiba's Real Estate Market

This repository contains the code and data for a hedonic pricing analysis of residential real estate in Curitiba, Brazil, applying Rosen's (1974) two-stage estimation procedure.

## Project Overview

The study estimates implicit prices of housing attributes using a stratified hedonic model. Following Fávero et al. (2008), neighborhoods are classified into three market tiers (low, middle, high) based on Principal Component Analysis of socioeconomic characteristics. The analysis employs Conley (1999) standard errors to account for spatial autocorrelation.

### Key Features
- **Data collection**: Web-scraped property listings from Brazilian real estate platforms
- **Geocoding**: Address-to-coordinates conversion using OpenStreetMap and ArcGIS APIs
- **Market segmentation**: PCA-based neighborhood stratification validated by robust Wald test
- **Spatial econometrics**: Conley HAC standard errors with 2km distance cutoff

## Repository Structure

```
├── Datasets/
│   ├── CWB_Housing.xlsx          # Raw scraped property data
│   ├── CWB_Housing_Wrangled.xlsx # Cleaned property data
│   ├── CWB_PCA.xlsx              # Neighborhood socioeconomic data
│   ├── Green_area.xlsx           # Green areas by neighborhood
│   ├── Private_schools.xlsx      # Private schools by neighborhood
│   └── Shopping_centers.xlsx     # Shopping centers by neighborhood
├── Python/
│   ├── wrangling.py              # Data cleaning and preprocessing
│   └── pyproject.toml            # Python dependencies
├── R/
│   ├── 1.PCA.R                   # Principal Component Analysis
│   ├── 2.Geoprocessing.R         # Geocoding and spatial data processing
│   └── 3.Rosen_Two_Step_Analysis.R # Hedonic regression analysis
└── R/Markdown/
    └── *.html                    # Rendered analysis reports
```

## Replication Instructions

### Prerequisites
- **R** (≥ 4.0) with packages: `tidyverse`, `sf`, `leaflet`, `tidygeocoder`, `writexl`, `sandwich`, `lmtest`
- **Python** (≥ 3.10) with packages listed in `pyproject.toml`

### Step 1: Data Wrangling (Optional)
```bash
cd Python
python wrangling.py
```
*Note: `CWB_Housing_Wrangled.xlsx` is already provided, so this step is optional.*

### Step 2: Principal Component Analysis
Run `R/1.PCA.R` to classify neighborhoods into market tiers.

### Step 3: Geoprocessing
Run `R/2.Geoprocessing.R` to:
- Geocode property addresses
- Merge locational attributes
- Generate tier-stratified datasets

**Important**: This step requires shapefiles (see below).

### Step 4: Hedonic Regression
Run `R/3.Rosen_Two_Step_Analysis.R` to estimate first and second-stage regressions.

## Obtaining Shapefiles

The geoprocessing script requires shapefiles for Curitiba's neighborhoods and urban infrastructure. These files exceed GitHub's size limit and must be downloaded separately.

### Download from IPPUC
Shapefiles are available from the Instituto de Pesquisa e Planejamento Urbano de Curitiba (IPPUC):

**[IPPUC Open Data Portal](https://ippuc.org.br/geodownloads/geo.htm)**

Download the following shapefiles and place them in a `Shapefiles/` folder:

| Folder | File | Description |
|--------|------|-------------|
| `Neighborhoods/` | `DIVISA_DE_BAIRROS.shp` | Neighborhood boundaries |
| `Nature/Parks/` | `PARQUES_E_BOSQUES.shp` | Parks and forests |
| `Nature/Squares/` | `PRACAS_E_JARDINETES.shp` | Squares and gardens |
| `Infrastructure/Terminals/` | `TERMINAL_DE_TRANSPORTE.shp` | Bus terminals |
| `Infrastructure/Cicleways/` | `CICLOVIA_OFICIAL.shp` | Bicycle paths |
| `Health/Hospitals/` | `HOSPITAL.shp` | Hospitals |
| `Schools/` | `ESCOLA_MUNICIPAL.shp` | Public schools |

## References

- Fávero, L. P. L. (2005). *O mercado imobiliário residencial da região metropolitana de São Paulo*. Doctoral dissertation, Universidade de São Paulo.
- Fávero, L. P. L., Belfiore, P., & Lima, G. A. S. F. (2008). Modelos de precificação hedônica de imóveis residenciais na região metropolitana de São Paulo. *Estudos Econômicos*, 38(1), 65-90.
- Rosen, S. (1974). Hedonic prices and implicit markets: Product differentiation in pure competition. *Journal of Political Economy*, 82(1), 34-55.
- Conley, T. G. (1999). GMM estimation with cross sectional dependence. *Journal of Econometrics*, 92(1), 1-45.

## Author

Lucas Salamuni
University of Cologne - MSc Economics
Winter Semester 2024/25