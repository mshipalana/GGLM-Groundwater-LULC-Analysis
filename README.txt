SUPPLEMENTARY DATA:  The impact of land use and land cover change on groundwater in Greater Giyani Local Municipality, South Africa.

MSc Thesis - Matsavu Shipalana
Nelson Mandela University, 2025

-----------------------------------------------------------------------------
FILE DESCRIPTIONS
-----------------------------------------------------------------------------

S1_WaterLevels_1995_2005_annual.csv
  - Cleaned groundwater level measurements (1995-2005)
  - 380 rows (borehole-year combinations)
  - Columns: Year, Identifier, GeoType, Latitude, Longitude, WaterLevel, 
             DepthCategory
  - Corresponds to Appendix A, Table A.2

S2_Buffer_GW_Deltas.csv
  - Groundwater level changes for all boreholes
  - 1 065 rows (355 boreholes × 3 time periods)
  - Columns: Identifier, gw_mean_y1, gw_mean_y2, delta_abs, delta_pct, year1, year2
  - Contains complete ΔGWL values supporting Section 4.5.5 analysis
  - Removed from main appendix for brevity

S3_ACCURACY ASSESSMENT.csv
  - Complete confusion matrices for LULC classification
  - 4 sheets: Pre/Post classification for 1995, 2000, 2005, and Summary
  - Supports Appendix B accuracy assessment (Tables B.2-B.4)

S4_CHANGE DETECTION (LCM).csv
  - Complete LULC change detection results
  - 5 sheets: 1995-2000, 2000-2005, 1995-2005, Key transitions, Summary
  - Includes: Cross-tabulation matrices, Gains/Losses, Net Change matrices,
    Contributors to Net Change per Class, From-To transition matrices
  - Supports Appendix C change detection analysis (Tables C.1-C.4)

  S5_A_Spearman Correlation_Matrix.csv
  - Complete Spearman correlation results (all 70+ predictors per period)
  - Three response periods: 1995-2000, 2000-2005, 1995-2005
  - Truncated to top 20 in Appendix D (Tables D.2-D.4)
  - Supports Section 4.6 predictor selection

  S6_Buffer_GW_ZonalStats_AllYears.csv
  - Buffer-level (500m) zonal extraction results
  - BGroundwater and LULC metricss around individual boreholes
  - Output from Section 4.5.3 buffer-based extraction

  S7_Ward_GW_Stats_allYears.csv
  - Ward-level municipal zonal extraction results
  - Aggregated groundwater and LULC metrics by administrative ward
  - Output from Section 4.5.4 ward-level aggregation

-----------------------------------------------------------------------------
RELATED RESOURCES
-----------------------------------------------------------------------------

Code Repository: https://github.com/[username]/GGLM-Groundwater-LULC-Analysis
DOI: 10.5281/zenodo.18474680

-----------------------------------------------------------------------------
LICENSE
-----------------------------------------------------------------------------

This data is licensed under Creative Commons Attribution 4.0 (CC BY 4.0)
https://creativecommons.org/licenses/by/4.0/

------------------------------------------------------------------------------
CITATION
------------------------------------------------------------------------------

Shipalana, M.S. (2026). Supplementary Data for:  The impact of land use and land cover change on groundwater in Greater Giyani Local Municipality, South Africa. Zenodo. https://doi.org/10.5281/zenodo.18475606

Cite all versions: https://doi.org/10.5281/zenodo.18475605

------------------------------------------------------------------------------
CONTACT
------------------------------------------------------------------------------

Matsavu Shipalana
Nelson Mandela University
matsavushipalana@gmail.com
