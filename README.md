
# GROUNDWATER-LULC ANALYSIS: GGREATER GIYANI LOCAL MUNICIPALITY(1995-2005)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18474680.svg)](https://doi.org/10.5281/zenodo.18474680)

Code repository for MSc thesis: "The impact of land use and land cover change on groundwater in Greater Giyani Local Municipality, South Africa."

#--Repository Structure--#

├── gee_scripts/          # Google Earth Engine LULC classification
├── r_scripts/            # R statistical analysis pipeline
└── README.md

#--Requirements--#

# GEE Scripts
- Google Earth Engine account
- Access to Landsat 5 Collection 2 Level 2

# R Scripts
**R Version:** 4.0+

**Required Packages:**
   r
install.packages(c("sf", "terra", "dplyr", "tidyr", "readr", 
                   "ggplot2", "gstat", "automap", "raster", 
                   "trend", "mgcv", "ranger", "caret"))

#--Workflow--#

# 1. LULC Classification (GEE)

gee_scripts/LULC_Classification_1995.js → Corrected_LULC_1995.tif
gee_scripts/LULC_Classification_2000.js → Corrected_LULC_2000.tif
gee_scripts/LULC_Classification_2005.js → Corrected_LULC_2005.tif


# 2. Groundwater Analysis (R)\

Execute scripts in order:
   r
source("r_scripts/Groundwater_cleaning.R")                # Section 4.3
source("r_script/Groundwater(GW)_Analysis.R")             # Section 4.4
source("r_scripts/Groundwater_LULC_Preparation.R")        # Section 4.5.A
source("r_scripts/GW_LULC_Interpolation.R")               # Section 4.5.B
source("r_scripts/GW_LULC_Raster_Export.R")               # Section 4.5.C
source("r_scripts/GW_LULC_Zonal_Extraction(Buffers).R")   # Section 4.5.D
source("r_scripts/GW_LULC_Zonal_Extraction(Wards).R")     # Section 4.5.E
source("r_scripts/GW_LULC_Temporal_Deltas.R")             # Section 4.5.F
source("r_scripts/GW_LULC_Summary_Statistics.R")          # Section 4.5.G
source("r_scripts/GW_LULC_Modelling.R")                   # Section 4.6

#Input Data

**Not included in repository** (available in supplementary data deposit):
- WaterLevelsGGLM.csv - Raw groundwater measurements (1995-2005)
- GreaterGiyani.shp - Municipal boundary
- GreaterGiyani_Wards.shp - Ward boundaries
- Buffers_LULC_All.csv - Buffer-level LULC statistics

See: [Zenodo deposit DOI - 10.5281/zenodo.18474680]

# Citation

If you use this code, please cite:
Shipalana, M.S. (2026). Code for: Land use/land cover change and groundwater 
level dynamics in Greater Giyani Local Municipality (1995-2005). 
GitHub repository. https://github.com/mshipalana/GGLM-Groundwater-LULC-Analysis

# License

MIT License - see LICENSE file

# Contact

Matsavu Shipalana
Nelson Mandela University
matsavushipalana@gmail.com
