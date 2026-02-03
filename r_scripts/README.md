# CLEANING GROUNDWATER-LEVEL DATA FOR 1995-2005

#---Filtering WaterLevelsGGLM.csv and preparing for analysis---#

#Setting up R environment and clearing workspace
rm(list = ls())

#Loading required packages
library(dplyr)
library(lubridate)
library(tidyr)

#Loading the dataset
data <- read.csv("WaterLevelsGGLM.csv", stringsAsFactors = FALSE)

#Inspecting the dataset (rows preview)
head(data)

#Checking column names and data types
str(data)

#Summary of the dataset (identifying missing values)
summary(data)

#Converting date format
data$Date <- parse_date_time(data$Date, orders = c("d/m/Y H:M", "Y-m-d H:M:S"), tz = "UTC")

#Filtering for 1995-2005
data_filtered <- data %>%
  filter(year(Date) >= 1995 & year(Date) <= 2005)

#Cleaning the data (removing missing dates or water levels)
data_clean <- data_filtered %>%
  filter(!is.na(Date) & !is.na(WaterLevel))

#Removing duplicate rows
data_clean <- data_clean %>%
  distinct(Date, Identifier, WaterLevel, .keep_all = TRUE)

#Verifying spatial consistency
coords_check <- data_clean %>%
  group_by(Identifier) %>%
  summarise(
    lat_var = var(Latitude, na.rm = TRUE),
    lon_var = var(Longitude, na.rm = TRUE)
  ) %>%
  filter(lat_var > 0.0001 | lon_var > 0.0001) #small threshold for coordinate drift

#Printing problematic identifiers
if (nrow(coords_check) > 0) {
  print("Warning: Inconsistent coordinates found for these Identifiers:")
  print(coords_check)
}

#Summarising cleaned data
summary_stats <- data_clean %>%
  group_by(year = year(Date)) %>%
  summarise(
    n_boreholes = n_distinct(Identifier),
    n_measurements = n()
  )

print(summary_stats)

#Saving cleaned dataset
write.csv(data_clean, "WaterLevels_1995_2005_cleaned.csv", row.names = FALSE)

#Saving summary statistics
write.csv(summary_stats, "WaterLevels_1995_2005_summary.csv", row.names = FALSE)

#Calculating the number of unique boreholes
library(dplyr)
data <- read.csv("WaterLevels_1995_2005_cleaned.csv")
unique_boreholes <- data %>% 
  distinct(Identifier, Latitude, Longitude) %>% 
  summarise(n_boreholes = n(), 
            lat_range = max(Latitude) - min(Latitude), 
            lon_range = max(Longitude) - min(Longitude))
print(unique_boreholes)

#Checking measurement frequency
measurement_freq <- data %>%
  mutate(year = lubridate::year(Date)) %>%
  group_by(year, Identifier) %>%
  summarise(n_measurements = n(), .groups = "drop")
print(summary(measurement_freq$n_measurements))

#---Cleaning and refining groundwater water level data for 1995-2005---#

#Aggregating annual medians, classifying depth categories, and preparing for trend analysis

#Setting up R environment and clearing workspace
rm(list = ls())

#Loading required packages
library(dplyr)
library(lubridate)

#Loading the dataset
data <- read.csv("WaterLevels_1995_2005_cleaned.csv", stringsAsFactors = FALSE)

#Inspecting dataset structure
str(data)

#Standardising date format
data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#Verifying no parse errors
if (any(is.na(data$Date))) {
  print("Warning: Some dates could not be parsed")
  print(data[is.na(data$Date), ])
}

#Aggregating annual medians
data_annual <- data %>%
  mutate(year = year(Date)) %>%
  group_by(year, Identifier, GeoType, Latitude, Longitude) %>%
  summarise(WaterLevel = median(WaterLevel, na.rm = TRUE), .groups = "drop") %>%
  filter(year >= 1995 & year <= 2005)

#Classifying depth categories
data_annual <- data_annual %>%
  mutate(DepthCategory = case_when(
    WaterLevel < 15 ~ "Shallow",
    WaterLevel >= 15 & WaterLevel <= 30 ~ "Intermediate",
    WaterLevel > 30 ~ "Deep",
    TRUE ~ NA_character_
  ))

#Checking depth distribution
print(table(data_annual$DepthCategory, data_annual$year))

#Verifying spatial consistency
coords_check <- data_annual %>%
  group_by(Identifier) %>%
  summarise(
    lat_var = var(Latitude, na.rm = TRUE),
    lon_var = var(Longitude, na.rm = TRUE)
  ) %>%
  filter(lat_var > 0.0001 | lon_var > 0.0001)

if (nrow(coords_check) > 0) {
  print("Warning: Inconsistent coordinates found:")
  print(coords_check)
}

#Summarising boreholes and measurements per year
summary_stats <- data_annual %>%
  group_by(year) %>%
  summarise(
    n_boreholes = n_distinct(Identifier),
    n_measurements = n()
  )

print(summary_stats)

#Saving outputs
write.csv(data_annual, "WaterLevels_1995_2005_annual.csv", row.names = FALSE)
write.csv(summary_stats, "WaterLevels_1995_2005_annual_summary.csv", row.names = FALSE)

#Confirming borehole count
rm(list = ls())
library(dplyr)
data <- read.csv("WaterLevels_1995_2005_annual.csv", stringsAsFactors = FALSE)
length(unique(data$Identifier))

# Counting multi-year boreholes
table(data$Identifier) >= 3
sum(table(data$Identifier) >= 3)
table(data$Identifier) >= 2
sum(table(data$Identifier) >= 2)

------------------------------------------------------------------------------------------------------------

# GROUNDWATER LEVELS AND CHANGE ANALYSIS - SECTION 4.4

# Setting up R and clearing workspace
rm(list = ls())

# Installing required packages
pkgs <- c(
  "dplyr", "tidyr", "readr", "ggplot2",
  "sf", "sp", "gstat", "raster", "terra",
  "trend", "lubridate"
)

inst <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if(length(inst) > 0){
  message("Installing missing packages: ", paste(inst, collapse = ", "))
  install.packages(inst)
}

# Loading required packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sf)
library(sp)
library(gstat)
library(raster)
library(terra)
library(trend)
library(lubridate)

#---Input files & parameters---#

input_csv <- "M:/GGLM/GGLM_GROUNDWATER/WaterLevels_1995_2005_annual.csv"
out_dir <- "M:/GGLM/GGLM_GROUNDWATER/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# municipal polygon (used to mask rasters)
gglm_shp <- "M:/GGLM/GGLM_SHAPEFILES/GreaterGiyani.shp"

target_crs <- 32736 # EPSG:32736

years_of_interest <- c(1995, 1997, 2002)
depth_breaks <- c(-Inf, 15, 30, Inf)     # water level (m bgl), positive
depth_labels <- c("Shallow", "Intermediate", "Deep")

# IDW parameters
idw_power <- 2
grid_res <- 30
min_points <- 5
force_small_samples <- TRUE     # set TRUE to force IDW even when n < min_points
idw_nmax <- 12                   # neighbors
write_uncertainty <- TRUE        # save var and sd rasters

#---Loading datasets and basic cleaning---#

gw_raw <- read_csv(input_csv, show_col_types = FALSE)
colnames(gw_raw) <- make.names(colnames(gw_raw))

req_cols <- c("Identifier", "Year", "Latitude", "Longitude", "WaterLevel")
missing_cols <- setdiff(req_cols, colnames(gw_raw))
if(length(missing_cols) > 0) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))

gw_clean <- gw_raw %>%
  filter(!is.na(WaterLevel)) %>%
  mutate(
    Year = as.integer(Year),
    Longitude = as.numeric(Longitude),
    Latitude  = as.numeric(Latitude),
    WaterLevel = as.numeric(WaterLevel),
    Identifier = as.character(Identifier)
  ) %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  distinct(Identifier, Year, .keep_all = TRUE)

# Depth category assignment (if missing) using water-level thresholds (m bgl)
if(!"DepthCategory" %in% colnames(gw_clean)){
  gw_clean <- gw_clean %>%
    mutate(DepthCategory = cut(WaterLevel, breaks = depth_breaks, labels = depth_labels, right = FALSE))
} else {
  # enforce consistent labels
  gw_clean <- gw_clean %>%
    mutate(DepthCategory = case_when(
      WaterLevel < 15 ~ "Shallow",
      WaterLevel >= 15 & WaterLevel <= 30 ~ "Intermediate",
      WaterLevel > 30 ~ "Deep",
      TRUE ~ as.character(DepthCategory)
    )) %>%
    mutate(DepthCategory = factor(DepthCategory, levels = depth_labels))
}

write_csv(gw_clean, file.path(out_dir, "WaterLevels_1995_2005_annual_cleaned.csv"))

#---Aggregated annual summaries---#

gw_annual <- gw_clean %>%
  group_by(Year) %>%
  summarise(
    n_boreholes = n_distinct(Identifier),
    median_level = median(WaterLevel, na.rm = TRUE),
    min_level = min(WaterLevel, na.rm = TRUE),
    max_level = max(WaterLevel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year)
write_csv(gw_annual, file.path(out_dir, "gw_annual_summary.csv"))

gw_depth_year <- gw_clean %>%
  group_by(Year, DepthCategory) %>%
  summarise(
    n_boreholes = n_distinct(Identifier),
    median_level = median(WaterLevel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(DepthCategory, Year)
write_csv(gw_depth_year, file.path(out_dir, "gw_depth_year_summary.csv"))

# Quick QA plots 
p1 <- ggplot(gw_annual, aes(x = Year, y = median_level)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = unique(gw_annual$Year), labels = as.integer(unique(gw_annual$Year))) +
  labs(title = "Annual median groundwater (municipal)", x = "Year", y = "Median water level (m bgl)") +
  theme_minimal()
ggsave(file.path(out_dir, "04_MedianTimeSeries.png"), p1, dpi = 300, width = 8, height = 4)

p2 <- ggplot(gw_depth_year, aes(x = Year, y = median_level, color = DepthCategory)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = unique(gw_depth_year$Year), labels = as.integer(unique(gw_depth_year$Year))) +
  labs(title = "Median groundwater by depth class", x = "Year", y = "Median water level (m bgl)") +
  theme_minimal()
ggsave(file.path(out_dir, "04_DepthTimeSeries.png"), p2, dpi = 300, width = 8, height = 4)

#--- Temporal Trend Analysis - Mann-Kendall Test & Sen's Slope---#

gw_series <- gw_annual %>% filter(!is.na(median_level)) %>% arrange(Year)
if(nrow(gw_series) < 5){
  warning("Fewer than 5 aggregated annual points available; MK and Sen's slope may be unreliable.")
}

# Mann-Kendall Test
mk_result <- mk.test(gw_series$median_level)
mk_result

# Sen's Slope Estimator
sen_result <- sens.slope(gw_series$median_level)
sen_result

# Mann-Kendall summary
mk_table <- tibble(
  Test = "Mann-Kendall (aggregated median)",
  Tau = as.numeric(mk_result$estimate),
  Z = as.numeric(mk_result$statistic),
  P_Value = as.numeric(mk_result$p.value)
)
write_csv(mk_table, file.path(out_dir, "04_MK_result_aggregated.csv"))

# Sen's slope summary
sen_table <- tibble(
  Test = "Sen's Slope (aggregated median)",
  Estimate_m_per_year = sen_result$estimates,
  CI_low = sen_result$conf.int[1],
  CI_high = sen_result$conf.int[2]
)
write_csv(sen_table, file.path(out_dir, "04_SenSlope_result_aggregated.csv"))

# Plot municipal median with Sen's slope line (compute intercept robustly)
if(!is.null(sen_result$estimates)){
  Year0 <- mean(gw_series$Year)
  intercept <- median(gw_series$median_level, na.rm = TRUE) - as.numeric(sen_result$estimates) * Year0
  p_trend <- ggplot(gw_series, aes(x = Year, y = median_level)) +
    geom_point() + geom_line() +
    geom_abline(intercept = intercept, slope = as.numeric(sen_result$estimates), linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = sort(unique(gw_series$Year)), labels = as.integer) +
    labs(title = "Municipal median groundwater with Sen's slope", x = "Year", y = "Median water level (m bgl)") +
    theme_minimal()
  ggsave(file.path(out_dir, "04_Median_with_SenSlope.png"), p_trend, dpi = 300, width = 8, height = 4)
}

#---MK + Sen's Slope by depth class (on depth-class aggregated medians)---#

depth_trend_list <- list()
for(cat in depth_labels){
  df_cat <- gw_depth_year %>% filter(DepthCategory == cat) %>% arrange(Year)
  if(nrow(df_cat) < 5){
    message("Depth category ", cat, " has fewer than 5 values; MK/Sen skipped for reliability.")
    next
  }
  mk_cat <- mk.test(df_cat$median_level)
  sen_cat <- sens.slope(df_cat$median_level)
  depth_trend_list[[cat]] <- list(MK = mk_cat, Sen = sen_cat)
  mk_out <- tibble(DepthCategory = cat, Tau = as.numeric(mk_cat$estimate), Z = as.numeric(mk_cat$statistic), P_Value = as.numeric(mk_cat$p.value))
  write_csv(mk_out, file.path(out_dir, paste0("04_MK_result_", cat, ".csv")))
  sen_out <- tibble(DepthCategory = cat, Estimate_m_per_year = as.numeric(sen_cat$estimates), CI_low = as.numeric(sen_cat$conf.int[1]), CI_high = as.numeric(sen_cat$conf.int[2]))
  write_csv(sen_out, file.path(out_dir, paste0("04_SenSlope_result_", cat, ".csv")))
}
saveRDS(depth_trend_list, file.path(out_dir, "04_DepthCategory_TrendResults.rds"))

# Depth trend plots with Sen's slope line (if available)
trend_plot_dir <- file.path(out_dir, "DepthTrendPlots"); dir.create(trend_plot_dir, showWarnings = FALSE)
for(cat in depth_labels){
  df_cat <- gw_depth_year %>% filter(DepthCategory == cat) %>% arrange(Year)
  if(nrow(df_cat) < 2) next
  if(nrow(df_cat) >= 2){
    sen_cat <- tryCatch(sens.slope(df_cat$median_level), error = function(e) NULL)
    Year0 <- mean(df_cat$Year)
    intercept_cat <- if(!is.null(sen_cat$estimates)) median(df_cat$median_level, na.rm = TRUE) - as.numeric(sen_cat$estimates) * Year0 else NA_real_
    p_cat <- ggplot(df_cat, aes(x = Year, y = median_level)) +
      geom_point(size = 2) + geom_line() +
      scale_x_continuous(breaks = sort(unique(df_cat$Year)), labels = as.integer) +
      labs(title = paste0("Trend in Groundwater Levels (", cat, " Aquifers)"), x = "Year", y = "Median water level (m bgl)") +
      theme_minimal()
    if(!is.na(intercept_cat)) p_cat <- p_cat + geom_abline(intercept = intercept_cat, slope = as.numeric(sen_cat$estimates), linetype = "dashed", colour = "red")
    out_plot <- file.path(trend_plot_dir, paste0("Trend_", cat, ".png"))
    ggsave(out_plot, p_cat, dpi = 300, width = 8, height = 4)
  }
}

#---Spatial preparation: sf points and export---#

# Convert cleaned points to sf and rproject to UTM
gw_sf <- gw_clean %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = target_crs)
write_sf(gw_sf, file.path(out_dir, "GW_AllPoints.shp"), delete_layer = TRUE)

#---IDW interpolation per YEAR and per DEPTH CATEGORY---#

# municipal boundary
gglm_poly <- st_read(gglm_shp, quiet = TRUE) %>% st_transform(crs = target_crs)
gglm_sp <- as(gglm_poly, "Spatial")   # for raster::mask when saving

# create terra template covering municipal boundary at 30 m
bb <- st_bbox(gglm_poly)
gglm_ext <- terra::ext(bb$xmin, bb$xmax, bb$ymin, bb$ymax)
ncol <- ceiling((gglm_ext$xmax - gglm_ext$xmin) / grid_res)
nrow <- ceiling((gglm_ext$ymax - gglm_ext$ymin) / grid_res)
r_template_terra <- rast(xmin = gglm_ext$xmin, xmax = gglm_ext$xmax, ymin = gglm_ext$ymin, ymax = gglm_ext$ymax, ncols = ncol, nrows = nrow, crs = paste0("EPSG:", target_crs))

# convert to raster::RasterLayer for gstat
r_template_raster <- raster::raster(r_template_terra)

# IDW function: returns SpatialPixelsDataFrame (var1.pred & var1.var)
run_idw_spdf <- function(points_sf, value_field, template_raster, idp = 2, nmax = 12, min_pts = 5, force = FALSE){
  npts <- nrow(points_sf)
  if(npts < min_pts && !force){
    warning("Only ", npts, " points available; skipping IDW (set force = TRUE to override).")
    return(NULL)
  }
  pts_sp <- as(points_sf, "Spatial")
  pred_sp <- as(template_raster, "SpatialPixelsDataFrame")
  g <- gstat(formula = as.formula(paste0(value_field, " ~ 1")), data = pts_sp, set = list(idp = idp), nmax = nmax)
  idw_pred <- tryCatch({ predict(g, newdata = pred_sp) }, error = function(e){ warning("gstat predict error: ", conditionMessage(e)); return(NULL) })
  return(idw_pred)
}

# helper to write IDW outputs (prediction + var + sd) and metadata
save_idw_rasters <- function(idw_spdf, out_base, mask_sp = NULL, save_uncertainty = TRUE, n_points = NA){
  if(is.null(idw_spdf)) return(NULL)
  r_pred <- raster::raster(idw_spdf["var1.pred"])
  if(!is.null(mask_sp)) r_pred <- raster::mask(r_pred, mask_sp)
  out_pred <- paste0(out_base, "_pred.tif")
  raster::writeRaster(r_pred, out_pred, overwrite = TRUE)
  message("Saved prediction: ", out_pred)
  out_var <- out_sd <- NULL
  if(save_uncertainty){
    r_var <- raster::raster(idw_spdf["var1.var"])
    if(!is.null(mask_sp)) r_var <- raster::mask(r_var, mask_sp)
    out_var <- paste0(out_base, "_var.tif")
    raster::writeRaster(r_var, out_var, overwrite = TRUE)
    r_sd <- raster::calc(r_var, fun = function(x) sqrt(x))
    out_sd <- paste0(out_base, "_sd.tif")
    raster::writeRaster(r_sd, out_sd, overwrite = TRUE)
    message("Saved variance & sd: ", out_var, " , ", out_sd)
  }
  # metadata CSV
  meta <- tibble(out_base = out_base, pred = out_pred, var = out_var, sd = out_sd, n_points = n_points, created = Sys.time())
  write_csv(meta, paste0(out_base, "_metadata.csv"))
  return(list(pred = out_pred, var = out_var, sd = out_sd, meta = meta))
}

# Run IDW for overall year and per depth category
for (y in years_of_interest){
  message("IDW: processing year ", y)
  pts_y <- gw_sf %>% filter(Year == y)
  if(nrow(pts_y) >= min_points || force_small_samples){
    idw_spdf <- run_idw_spdf(pts_y, "WaterLevel", r_template_raster, idp = idw_power, nmax = idw_nmax, min_pts = min_points, force = force_small_samples)
    if(!is.null(idw_spdf)){
      outbase <- file.path(out_dir, paste0("IDW_WaterLevel_", y))
      save_idw_rasters(idw_spdf, outbase, mask_sp = gglm_sp, save_uncertainty = write_uncertainty, n_points = nrow(pts_y))
    }
  } else message("Skipping overall IDW for ", y, " (n=", nrow(pts_y), ")")
  # per depth
  for(cat in depth_labels){
    pts_y_cat <- gw_sf %>% filter(Year == y & DepthCategory == cat)
    if(nrow(pts_y_cat) >= min_points || force_small_samples){
      idw_spdf_cat <- run_idw_spdf(pts_y_cat, "WaterLevel", r_template_raster, idp = idw_power, nmax = idw_nmax, min_pts = min_points, force = force_small_samples)
      if(!is.null(idw_spdf_cat)){
        outbase_cat <- file.path(out_dir, paste0("IDW_WaterLevel_", y, "_", cat))
        save_idw_rasters(idw_spdf_cat, outbase_cat, mask_sp = gglm_sp, save_uncertainty = write_uncertainty, n_points = nrow(pts_y_cat))
      }
    } else message("Skipping IDW for year ", y, " depth ", cat, " (n=", nrow(pts_y_cat), ")")
  }
}

#---Create depth-category rasters per year (safe handling of missing layers)---#

depth_raster_mapping <- function(year, out_dir, template_terra, depth_labels){
  
  # expected IDW pred files
  file_shallow <- file.path(out_dir, paste0("IDW_WaterLevel_", year, "_Shallow_pred.tif"))
  file_inter   <- file.path(out_dir, paste0("IDW_WaterLevel_", year, "_Intermediate_pred.tif"))
  file_deep    <- file.path(out_dir, paste0("IDW_WaterLevel_", year, "_Deep_pred.tif"))
  file_overall <- file.path(out_dir, paste0("IDW_WaterLevel_", year, "_pred.tif"))
  
  # safe raster read (single layer)
  safe_read <- function(path){
    if(!file.exists(path)) return(NULL)
    r <- tryCatch(terra::rast(path), error = function(e){ warning("Failed to read ", path); return(NULL) })
    if(!is.null(r) && terra::nlyr(r) > 1) r <- r[[1]]
    return(r)
  }
  
  avail <- list(
    shallow = safe_read(file_shallow),
    intermediate = safe_read(file_inter),
    deep = safe_read(file_deep),
    overall = safe_read(file_overall)
  )
  
  if(all(sapply(avail, is.null))){
    warning("No IDW prediction rasters found for year ", year, " -> skipping depth-class raster.")
    return(NULL)
  }
  
  # Initialize raster stack and class index
  ras_stack <- list()
  class_index <- integer(0)
  
  # List of expected classes
  expected_classes <- c("shallow", "intermediate", "deep")
  
  # Fill rasters, replacing missing layers
  for(cls in expected_classes){
    r <- avail[[cls]]
    if(is.null(r)){
      # Fill missing layer
      if(!is.null(avail$overall)){
        r <- terra::resample(avail$overall, template_terra, method = "bilinear")
        message("Year ", year, ": missing '", cls, "' raster -> filled with overall raster.")
      } else if(length(ras_stack) > 0){
        r <- ras_stack[[1]]
        message("Year ", year, ": missing '", cls, "' raster -> filled with first available layer.")
      } else {
        warning("Year ", year, ": no raster available to fill '", cls, "' layer -> assigning NA")
        r <- terra::rast(template_terra)
        values(r) <- NA_real_
      }
    } else {
      r <- terra::resample(r, template_terra, method = "bilinear")
    }
    ras_stack[[length(ras_stack) + 1]] <- r
    class_index <- c(class_index, match(cls, expected_classes))
  }
  
  # Create SpatRaster stack
  st <- do.call(c, ras_stack)
  
  # Function for terra::app (returns single numeric per cell)
  fun_depth <- function(vals){
    if(all(is.na(vals))) return(NA_real_)
    vals_num <- as.numeric(vals)
    vals_num[!is.finite(vals_num)] <- NA_real_
    if(all(is.na(vals_num))) return(NA_real_)
    i_min <- which.min(vals_num)
    if(length(i_min) == 0) return(NA_real_)
    return(as.numeric(class_index[i_min]))
  }
  
  # Apply function and save raster
  outpath <- file.path(out_dir, paste0("DepthClassRaster_", year, ".tif"))
  class_r <- tryCatch({
    terra::app(st, fun_depth)
  }, error = function(e){
    stop("Error applying depth function for year ", year, ": ", conditionMessage(e))
  })
  
  terra::writeRaster(class_r, outpath, overwrite = TRUE)
  message("Saved depth class raster: ", outpath)
  
  return(outpath)
}

# Run for all years
for(y in years_of_interest){
  depth_raster_mapping(y, out_dir, r_template_terra, depth_labels)
}

#---Saving workspace summaries & end---#

saveRDS(list(
  gw_clean = gw_clean, gw_annual = gw_annual, gw_depth_year = gw_depth_year,
  mk_result = mk_result, sen_result = sen_result
), file = file.path(out_dir, "04_GW_workflow_results.rds"))

message("Section 4.4 workflow complete. Outputs saved to: ", out_dir)

--------------------------------------------------------------------------------------------------------------------------

# LULC-GROUNDWATER ANALYSIS (A) - Section 4.5
#--PREPARING ANNUAL GROUNDWAtER SUBSETS--#

#Setting up R and clearing workspace
rm(list = ls())
options(stringsAsFactors = FALSE)

#---Installing / Loading packages---#

pkgs <- c("sf", "dplyr", "readr", "stringr", "tibble")
inst <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if(length(inst) > 0){
  message("Installing: ", paste(inst, collapse = ", "))
  install.packages(inst)
}
library(sf); library(dplyr); library(readr); library(stringr); library(tibble)

#---Paths and Parameters---#

input_csv   <- "M:/GGLM/GGLM_GROUNDWATER/WaterLevels_1995_2005_annual.csv"
out_shp_dir <- "M:/GGLM/GGLM_GROUNDWATER/Annual_Shapefiles"
out_summary <- "M:/GGLM/GGLM_GROUNDWATER/Annual_Borehole_Summary.csv"
out_rds     <- file.path(out_shp_dir, "GW_All_points_UTM36S.rds")
dir.create(out_shp_dir, recursive = TRUE, showWarnings = FALSE)

target_epsg <- 32736 # UTM Zone 36S

# Reading files and Basic validation---#

if(!file.exists(input_csv)) stop("Input CSV not found: ", input_csv)
gw <- read_csv(input_csv, show_col_types = FALSE)

# Standardise column names 
names(gw) <- make.names(names(gw))
names(gw) <- str_replace_all(names(gw), "\\.+", "_")

# Required columns
req_cols <- c("Identifier", "Year", "Latitude", "Longitude", "WaterLevel")
missing_cols <- setdiff(req_cols, names(gw))
if(length(missing_cols) > 0){
  stop("Missing required columns in input CSV: ", paste(missing_cols, collapse = ", "))
}

# Ensure types & basic cleaning
gw <- gw %>%
  mutate(
    Identifier = as.character(Identifier),
    Year = as.integer(Year),
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude),
    WaterLevel = as.numeric(WaterLevel)
  ) %>%
  # drop rows with missing coords or water level
  filter(!is.na(Longitude) & !is.na(Latitude) & !is.na(WaterLevel))

# If DepthCategory exists, clean; if not, create NA (downstream scripts will assign)
if(!"DepthCategory" %in% names(gw)){
  gw$DepthCategory <- NA_character_
} else {
  gw <- gw %>% mutate(DepthCategory = as.character(DepthCategory))
  gw$DepthCategory[gw$DepthCategory == ""] <- NA_character_
}

# Remove exact duplicate borehole-year rows, keeping the first
gw <- gw %>% arrange(Identifier, Year) %>%
  distinct(Identifier, Year, .keep_all = TRUE)
  

#---Convert to sf and project to UTM36S---#

# create sf (WGS84 assumed for input coords)
gw_sf <- st_as_sf(gw, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Validate geometries and make valid where necessary
if(any(!st_is_valid(gw_sf))){
  gw_sf <- st_make_valid(gw_sf)
  message("Some geometries were invalid and have been repaired with st_make_valid().")
}

# Transform to target CRS
gw_sf <- st_transform(gw_sf, crs = target_epsg)

# Clip to municipal boundary if available (safer than forcing)
gglm_shp <- "M:/GGLM/GGLM_SHAPEFILES/GreaterGiyani.shp"
if(file.exists(gglm_shp)){
  gglm <- st_read(gglm_shp, quiet = TRUE)
  if(st_crs(gglm)$epsg != target_epsg) gglm <- st_transform(gglm, target_epsg)
  # intersect to keep only points inside municipal boundary
  gw_sf <- st_intersection(gw_sf, st_make_valid(gglm))
  message("Points clipped to Greater Giyani polygon (", nrow(gw_sf), " points remain).")
} else {
  message("Municipal boundary not found; proceeding without clipping.")
}


#---Creating annual summary and per-year shapefiles---#

years <- sort(unique(gw_sf$Year))
annual_summary <- tibble(Year = integer(), n_boreholes = integer(), note = character())

for(y in years){
  gw_y <- gw_sf %>% filter(Year == y)
  n <- nrow(gw_y)
  note <- if(n == 0) {
    "no data"
  } else if(n < 5) {
    "sparse (point-only recommended)"
  } else if(n < 15) {
    "low density (interpolate with caution)"
  } else {
    "sufficient for interpolation (verify variogram)"
  }
  
  # Save per-year shapefile only if points exist
  out_shp <- file.path(out_shp_dir, paste0("GW_", y, ".shp"))
  if(n > 0){
    # ensure valid
    if(any(!st_is_valid(gw_y))) gw_y <- st_make_valid(gw_y)
    st_write(gw_y, out_shp, delete_layer = TRUE, quiet = TRUE)
    message("Wrote per-year shapefile: ", out_shp, " (n=", n, ")")
  } else {
    message("No points for year ", y, " — no shapefile written.")
  }
  
  annual_summary <- bind_rows(annual_summary, tibble(Year = y, n_boreholes = n, note = note))
}

# Add expected years (defensive)
expected_years <- c(1995:2005)
missing_expected <- setdiff(expected_years, annual_summary$Year)
if(length(missing_expected) > 0){
  add_df <- tibble(Year = missing_expected, n_boreholes = 0L, note = "no data (expected year absent)")
  annual_summary <- bind_rows(annual_summary, add_df)
}

annual_summary <- annual_summary %>% arrange(Year)
write_csv(annual_summary, out_summary)
message("Annual summary written: ", out_summary)

# Save combined sf for downstream steps
saveRDS(gw_sf, file = out_rds)
message("Combined GW sf object saved: ", out_rds)

# End
message("4.4.A complete. Annual point subsets and summary created.")

----------------------------------------------------------------------------------------------------------------------------------

# LULC-GROUNDWATER ANALYSIS (B) - Section 4.5
#--KRIGING/IDW INTERPOLATION--# 

# Setting up R and clearing workspace
rm(list = ls())
options(stringsAsFactors = FALSE)

#---Installing/Loading required packages---#

pkgs <- c("sf","terra","sp","gstat","automap","dplyr","ggplot2","purrr","readr","raster","tools")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

#---Paths and Parameters---#

base_dir <- "M:/GGLM/GGLM_GROUNDWATER"
in_rds <- file.path(base_dir, "Annual_Shapefiles", "GW_All_points_UTM36S.rds")
out_dir <- file.path(base_dir, "Interpolation_Results")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

gglm_boundary_shp <- "M:/GGLM/GGLM_SHAPEFILES/GreaterGiyani.shp"       # municipality boundary
wards_shp         <- "M:/GGLM/GGLM_SHAPEFILES/GreaterGiyani_Wards.shp"  # wards (optional)

grid_res_m <- 30
years_all <- c(1995,1996,1997,1998,1999,2000,2001,2002,2004,2005)
krige_years <- c(1996,1997,1998,2002)     # years to prioritise kriging
hybrid_years <- c(1995,2000,2001)         # try kriging but also produce IDW
idw_only_years <- c(1999,2004,2005)       # IDW only

min_points_for_variogram <- 10   # require this many pts to attempt variogram/kriging
min_points_for_idw <- 3         # require this many pts to attempt IDW (lower because IDW can run on few pts)
force_idw_when_few <- FALSE     # set TRUE to force IDW even when pts < min_points_for_idw (not recommended)

# Diagnostics folders
diag_dir <- file.path(out_dir, "diagnostics_points")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
thumb_dir <- file.path(out_dir, "thumbnails"); dir.create(thumb_dir, showWarnings = FALSE)

#---Read inputs and checks---#

if(!file.exists(in_rds)) stop("GW points RDS not found: ", in_rds)
gw_sf <- readRDS(in_rds)
if(!inherits(gw_sf, "sf")) stop("Input RDS must contain an sf object.")

if(!file.exists(gglm_boundary_shp)) stop("Municipality boundary missing: ", gglm_boundary_shp)
gglm <- st_read(gglm_boundary_shp, quiet = TRUE)
gglm <- st_transform(gglm, 32736)

if(file.exists(wards_shp)){
  wards_sf <- st_read(wards_shp, quiet = TRUE) %>% st_transform(32736)
  wards_vect <- terra::vect(wards_sf)
} else {
  wards_sf <- NULL; wards_vect <- NULL
}

# detect Year and value columns robustly
year_col <- names(gw_sf)[grepl("^year$|^yr$|^year\\b", names(gw_sf), ignore.case = TRUE)][1]
value_col <- names(gw_sf)[grepl("water|level|wl|depth|val|value", names(gw_sf), ignore.case = TRUE)][1]
if(is.na(year_col) || is.null(year_col)) stop("Could not find Year column in GW points.")
if(is.na(value_col) || is.null(value_col)) stop("Could not find WaterLevel/value column in GW points.")

gw_sf <- gw_sf %>% rename(Year = !!sym(year_col), WaterLevel = !!sym(value_col))

# CRSs: ensure points are EPSG:32736
if(is.na(st_crs(gw_sf))) stop("GW points have no CRS - assign EPSG:32736 before running.")
if(is.null(st_crs(gw_sf)$epsg) || st_crs(gw_sf)$epsg != 32736){
  message("Transforming GW points to EPSG:32736")
  gw_sf <- st_transform(gw_sf, 32736)
}

# Convert to SpatialPointsDataFrame for gstat where needed
gw_sp_orig <- as(gw_sf, "Spatial")

# Remove exact duplicate coordinates-year by averaging
gw_df <- gw_sp_orig@data
coords <- as.data.frame(coordinates(gw_sp_orig)); names(coords) <- c("x","y")
gw_clean_df <- cbind(coords, gw_df) %>%
  as_tibble() %>%
  group_by(Year, x, y) %>%
  summarise(WaterLevel = mean(WaterLevel, na.rm = TRUE), .groups = "drop")

# Rebuild SpatialPointsDataFrame (safe)
coordinates(gw_clean_df) <- ~x + y
proj4string(gw_clean_df) <- CRS(sf::st_crs(32736)$proj4string)
gw_sp <- gw_clean_df  # final SpatialPointsDataFrame for interpolation loops

#---Helpers---#

create_prediction_grid <- function(sp_points = NULL, mask_boundary = NULL, res_m = 30){
  # Accept either sp_points (Spatial or sf) OR mask_boundary (sf polygon)
  if(!is.null(mask_boundary)){
    if(inherits(mask_boundary, "sf")) bb <- st_bbox(mask_boundary)
    else if(inherits(mask_boundary, "Spatial")) bb <- as.numeric(unlist(sp::bbox(mask_boundary)))
    else stop("mask_boundary must be sf or Spatial")
    xmin <- bb["xmin"]; xmax <- bb["xmax"]; ymin <- bb["ymin"]; ymax <- bb["ymax"]
  } else if(!is.null(sp_points)){
    if(inherits(sp_points, "sf")) bb <- st_bbox(sp_points)
    else if(inherits(sp_points, "Spatial")) bb <- as.numeric(unlist(sp::bbox(sp_points)))
    else stop("sp_points must be sf or Spatial")
    xmin <- bb["xmin"]; xmax <- bb["xmax"]; ymin <- bb["ymin"]; ymax <- bb["ymax"]
  } else {
    stop("Either sp_points or mask_boundary must be provided to create_prediction_grid()")
  }
  # Expand slightly to avoid edge effects
  pad <- res_m * 2
  x_seq <- seq(xmin - pad, xmax + pad, by = res_m)
  y_seq <- seq(ymin - pad, ymax + pad, by = res_m)
  grd <- expand.grid(x = x_seq, y = y_seq)
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  proj4string(grd) <- proj4string(gw_sp) # use CRS of gw_sp (already set)
  return(grd)
}

#---Safe variogram autofit---#

fit_variogram_auto_safe <- function(spdf, field, min_pts = min_points_for_variogram){
  # spdf must be SpatialPointsDataFrame
  if(is.null(spdf)) return(NULL)
  n <- NA_integer_
  try(n <- nrow(spdf), silent = TRUE)
  if(is.na(n) || n < min_pts){
    message("Variogram: insufficient points (n=", n, ") — skipping (min_pts=", min_pts,")")
    return(NULL)
  }
  res <- tryCatch({
    automap::autofitVariogram(as.formula(paste(field, "~1")), spdf)
  }, error = function(e){
    warning("autofitVariogram failed: ", conditionMessage(e)); return(NULL)
  })
  if(inherits(res, "try-error")) return(NULL)
  return(res)
}

#---Kriging wrapper (safe)---#

krige_and_save_safe <- function(spdf, field, vgm_model, grid_sp, out_prefix){
  # Basic input checks
  if(is.null(spdf) || is.null(vgm_model) || is.null(grid_sp)) return(NULL)
  n <- tryCatch(nrow(spdf), error = function(e) NA_integer_)
  if(is.na(n) || n < min_points_for_variogram){
    warning("krige_and_save_safe: insufficient points (n=", n, ") -> skipping ", out_prefix); return(NULL)
  }
  # ensure data column present
  if(!(field %in% names(spdf@data))){
    warning("Field '", field, "' not found in spdf; skipping ", out_prefix); return(NULL)
  }
  names(spdf@data)[names(spdf@data) == field] <- "zvar"
  kr <- tryCatch({
    gstat::krige(zvar ~ 1, spdf, newdata = grid_sp, model = vgm_model)
  }, error = function(e){
    warning("krige failed for ", out_prefix, ": ", conditionMessage(e)); return(NULL)
  })
  if(is.null(kr) || nrow(kr) == 0) { warning("krige returned no rows for ", out_prefix); return(NULL) }
  pred_r <- tryCatch(raster::raster(kr, layer = "var1.pred"), error = function(e) NULL)
  var_r  <- tryCatch(raster::raster(kr, layer = "var1.var"), error = function(e) NULL)
  if(is.null(pred_r)) { warning("Failed to extract krige pred to raster for ", out_prefix); return(NULL) }
  pred_rst <- terra::rast(pred_r); var_rst <- if(!is.null(var_r)) terra::rast(var_r) else NULL
  pred_file <- file.path(out_dir, paste0(out_prefix, "_krige_pred.tif"))
  var_file  <- file.path(out_dir, paste0(out_prefix, "_krige_var.tif"))
  terra::writeRaster(pred_rst, pred_file, overwrite = TRUE)
  if(!is.null(var_rst)) terra::writeRaster(var_rst, var_file, overwrite = TRUE)
  return(list(pred = pred_file, var = var_file))
}

#---IDW wrapper (safe)---#

idw_and_save_safe <- function(spdf, field, grid_sp, out_prefix, idp = 2, min_pts = min_points_for_idw, force = force_idw_when_few){
  if(is.null(spdf) || is.null(grid_sp)) return(NULL)
  n <- tryCatch(nrow(spdf), error = function(e) NA_integer_)
  if(is.na(n) || (n < min_pts && !force)){
    warning("IDW: insufficient points (n=", n, ") for ", out_prefix, " - need >= ", min_pts, " (or set force=TRUE).")
    return(NULL)
  }
  if(!(field %in% names(spdf@data))){
    warning("Field '", field, "' not found in spdf; skipping IDW ", out_prefix); return(NULL)
  }
  names(spdf@data)[names(spdf@data) == field] <- "zvar"
  idw_res <- tryCatch({
    gstat::idw(zvar ~ 1, spdf, newdata = grid_sp, idp = idp)
  }, error = function(e){
    warning("IDW failed for ", out_prefix, ": ", conditionMessage(e)); return(NULL)
  })
  if(is.null(idw_res) || nrow(idw_res) == 0){ warning("IDW returned no rows for ", out_prefix); return(NULL) }
  pred_r <- tryCatch(raster::raster(idw_res, layer = "var1.pred"), error = function(e) NULL)
  if(is.null(pred_r)) { warning("Failed to convert idw output to raster: ", out_prefix); return(NULL) }
  pred_rst <- terra::rast(pred_r)
  pred_file <- file.path(out_dir, paste0(out_prefix, "_idw_pred.tif"))
  terra::writeRaster(pred_rst, pred_file, overwrite = TRUE)
  return(list(pred = pred_file))
}

#---Cross-validation helper for kriging (safe)---#

kriging_cv <- function(spdf, field, vgm_model){
  n <- tryCatch(nrow(spdf), error = function(e) NA_integer_)
  if(is.na(n) || n < 5) return(NULL)
  form <- as.formula(paste(field, "~1"))
  cv <- tryCatch({
    gstat::krige.cv(form, spdf, model = vgm_model, nfold = min(10, n))
  }, error = function(e) NULL)
  return(cv)
}

#---Main Loop: per-year interpolation---#

meta_list <- list()

for(y in years_all){
  message("\n=== Year: ", y, " ===")
  # subset points for this year (SpatialPointsDataFrame)
  gw_y_sp <- tryCatch(gw_sp[gw_sp$Year == y, ], error = function(e) NULL)
  n <- tryCatch(nrow(gw_y_sp), error = function(e) NA_integer_)
  message("Points found: ", n)
  if(is.na(n) || n == 0){
    meta_list[[as.character(y)]] <- list(year=y, n=0, method="no_data")
    next
  }
  # Save per-year points for diagnostics
  pts_df <- tryCatch(as.data.frame(gw_y_sp), error = function(e) NULL)
  if(!is.null(pts_df)) write_csv(as_tibble(pts_df), file.path(diag_dir, paste0("points_", y, ".csv")))
  # Create prediction grid using municipal boundary (robust)
  grid_sp <- create_prediction_grid(mask_boundary = gglm, res_m = grid_res_m)
  out_prefix <- paste0("GW_", y)
  
  # ensure spdf exists and has correct class
  spdf <- gw_y_sp
  if(is.null(spdf) || !inherits(spdf, "Spatial")) {
    warning("Year ", y, ": spdf is invalid -> skipping")
    meta_list[[as.character(y)]] <- list(year=y, n=n, method="invalid_spdf")
    next
  }
  
  # (A) kriging years
  if(y %in% krige_years){
    fit_obj <- fit_variogram_auto_safe(spdf, "WaterLevel")
    v_model <- if(!is.null(fit_obj)) fit_obj$var_model else NULL
    if(!is.null(v_model)){
      kr_files <- krige_and_save_safe(spdf, "WaterLevel", v_model, grid_sp, out_prefix)
      cv <- kriging_cv(spdf, "WaterLevel", v_model)
      if(!is.null(cv)){
        resids <- cv$observed - cv$var1.pred
        rmse <- sqrt(mean(resids^2, na.rm=TRUE)); mae <- mean(abs(resids), na.rm=TRUE)
      } else { rmse <- NA; mae <- NA }
      meta_list[[as.character(y)]] <- list(year=y, n=n, method="kriging", krige_files=kr_files, rmse=rmse, mae=mae)
    } else {
      warning("Variogram fit failed or insufficient points for year ", y, " — attempting IDW fallback")
      idw_files <- idw_and_save_safe(spdf, "WaterLevel", grid_sp, out_prefix, idp=2)
      meta_list[[as.character(y)]] <- list(year=y, n=n, method=if(!is.null(idw_files)) "idw_fallback" else "kriging_failed", krige_files=NULL, idw_files=idw_files)
    }
    
    # (B) hybrid years: attempt both where possible
  } else if(y %in% hybrid_years){
    fit_obj <- fit_variogram_auto_safe(spdf, "WaterLevel")
    v_model <- if(!is.null(fit_obj)) fit_obj$var_model else NULL
    kr_files <- if(!is.null(v_model)) krige_and_save_safe(spdf, "WaterLevel", v_model, grid_sp, out_prefix) else NULL
    idw_files <- idw_and_save_safe(spdf, "WaterLevel", grid_sp, out_prefix, idp = 2)
    meta_list[[as.character(y)]] <- list(year=y, n=n, method="hybrid", krige_files=kr_files, idw_files=idw_files)
    
    # (C) idw only years
  } else if(y %in% idw_only_years){
    idw_files <- idw_and_save_safe(spdf, "WaterLevel", grid_sp, out_prefix, idp = 2)
    meta_list[[as.character(y)]] <- list(year=y, n=n, method="idw_only", idw_files = idw_files)
    
    # (D) default/hybrid: try both
  } else {
    fit_obj <- fit_variogram_auto_safe(spdf, "WaterLevel")
    v_model <- if(!is.null(fit_obj)) fit_obj$var_model else NULL
    kr_files <- if(!is.null(v_model)) krige_and_save_safe(spdf, "WaterLevel", v_model, grid_sp, out_prefix) else NULL
    idw_files <- idw_and_save_safe(spdf, "WaterLevel", grid_sp, out_prefix, idp = 2)
    meta_list[[as.character(y)]] <- list(year=y, n=n, method="default_hybrid", krige_files=kr_files, idw_files=idw_files)
  }
  
  # optionally make small thumbnails of produced rasters (if any)
  produced <- c(
    if(!is.null(meta_list[[as.character(y)]]$krige_files)) meta_list[[as.character(y)]]$krige_files$pred else NULL,
    if(!is.null(meta_list[[as.character(y)]]$idw_files )) meta_list[[as.character(y)]]$idw_files$pred else NULL
  )
  # ---- Generate PNG thumbnails from rasters ----
  
  for(pf in produced){
    if(!is.null(pf) && file.exists(pf)){
      
      # Load raster
      r <- try(raster(pf), silent=TRUE)
      
      if(!inherits(r,"RasterLayer")) next
      
      png(
        filename = file.path(thumb_dir, paste0("GW_", y, "_thumbnail.png")),
        width = 800, height = 650
      )
      plot(r, main = paste("Groundwater Level -", y))
      plot(gglm, add=TRUE, border="black", lwd=1.2)   # boundary overlay
      dev.off()
    }
  }
  gc()
} # end years loop

#---Save metadata---#

meta_df <- bind_rows(lapply(meta_list, function(m){
  tibble::tibble(
    year = m$year,
    n_boreholes = m$n,
    method = m$method,
    krige_pred = if(!is.null(m$krige_files)) m$krige_files$pred else "",
    krige_var = if(!is.null(m$krige_files)) m$krige_files$var else "",
    idw_pred = if(!is.null(m$idw_files)) m$idw_files$pred else ""
  )
}), .id = NULL)

write_csv(meta_df, file.path(out_dir, "Interpolation_Metadata.csv"))
message("Step 4.4.B complete. Metadata and rasters saved to: ", out_dir)

#---Optional thumbnails---#

for(pf in unique(c(meta_df$krige_pred, meta_df$idw_pred))){
  if(pf != "" && file.exists(pf)){
    r <- terra::rast(pf)
    if(terra::hasValues(r)){
      png(file.path(out_dir, paste0(tools::file_path_sans_ext(basename(pf)), "_thumb.png")), width=800, height=600)
      plot(r, main = basename(pf))
      dev.off()
    } else {
      warning("Skipping thumbnail (no values) for: ", pf)
    }
  }
}

list.files(out_dir, full.names = TRUE)
terra::rast(file.path(out_dir, "GW_1996_krige_pred.tif")) |> terra::hasValues()
r <- terra::rast("M:/GGLM/GGLM_GROUNDWATER/Interpolation_Results/GW_1997_krige_pred.tif")
plot(r)
r <- terra::rast("M:/GGLM/GGLM_GROUNDWATER/Interpolation_Results/GW_2005_idw_pred.tif")
plot(r)
read.csv(file.path(out_dir, "Interpolation_Metadata.csv")) |> head(10)

---------------------------------------------------------------------------------------------------------------------------------------

# LULC-GROUNDWATER ANALYSIS (C) - Section 4.5
#--EXPORTING INTERPOLATED GROUNDWATER RASTERS--#

# Loading required packages
library(terra)
library(readr)

#---Loading Metadata---#

meta_file <- "M:/GGLM/GGLM_Groundwater/Interpolation_Results/Interpolation_Metadata.csv"
boundary_file <- "M:/GGLM/GGLM_SHAPEFILES/GreaterGiyani.shp"
out_dir <- "M:/GGLM/GGLM_GROUNDWATER/Exported_Rasters/"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Diagnostics
diag <- list(success = list(), failed = list())

#---Basic checks---#

if(!file.exists(meta_file)) stop("Metadata CSV missing: ", meta_file)
if(!file.exists(boundary_file)) stop("Boundary missing: ", boundary_file)

meta <- read_csv(meta_file, show_col_types = FALSE)
# Expect columns: year, method, krige_pred, idw_pred (or similar)
meta <- meta %>%
  dplyr::mutate(year = as.character(year),
                krige_pred = ifelse(is.na(krige_pred), "", krige_pred),
                idw_pred   = ifelse(is.na(idw_pred), "", idw_pred))

#---Loading boundary (terra vect) and enforce EPSG:32736---#

gglm_boundary <- tryCatch({
  terra::vect(boundary_file)
}, error = function(e) stop("Failed to read boundary with terra::vect(): ", conditionMessage(e)))

# Set/confirm CRS
target_epsg <- 32736
target_wkt <- sf::st_crs(target_epsg)$wkt
if(is.na(terra::crs(gglm_boundary, proj=TRUE)) || terra::crs(gglm_boundary, proj=TRUE) == ""){
  terra::crs(gglm_boundary) <- target_wkt
} else if(!terra::same.crs(terra::crs(gglm_boundary), target_wkt)){
  gglm_boundary <- terra::project(gglm_boundary, target_wkt)
}

#---Determine raster path to use per row (prefer kriging then idw)---#

meta <- meta %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    chosen_raster = {
      kp <- ifelse(!is.na(krige_pred) && krige_pred != "" && file.exists(krige_pred), krige_pred, NA_character_)
      ip <- ifelse(!is.na(idw_pred) && idw_pred != "" && file.exists(idw_pred), idw_pred, NA_character_)
      if(!is.na(kp)) kp else if(!is.na(ip)) ip else NA_character_
    }
  ) %>%
  dplyr::ungroup()

# Warn if none chosen
if(any(is.na(meta$chosen_raster))){
  warning("Some metadata rows have no valid raster. Check Interpolation_Metadata.csv. Those years will be skipped.")
}


#---Load rasters (clip & reproject) and save clipped copies---#

loaded_rasters <- list()
diag_rows <- list()

# Use the first successfully loaded raster as reference for stacking/resampling
ref_rast <- NULL
ref_year <- NULL

for(i in seq_len(nrow(meta))){
  yr <- as.character(meta$year[i])
  p <- meta$chosen_raster[i]
  if(is.na(p) || p == "" || !file.exists(p)){
    diag_rows[[yr]] <- tibble::tibble(year = yr, chosen = p, status = "no_file")
    next
  }
  
  # Attempt to load with terra
  r_try <- tryCatch(terra::rast(p), error = function(e) {
    diag_rows[[yr]] <- tibble::tibble(year = yr, chosen = p, status = paste0("read_error: ", conditionMessage(e)))
    return(NULL)
  })
  if(is.null(r_try)) next
  
  # If raster has >1 layer, take first
  if(terra::nlyr(r_try) > 1) r_try <- r_try[[1]]
  
  # Ensure CRS: if missing assume EPSG:32736 but record a warning
  r_crs <- tryCatch(terra::crs(r_try, proj = TRUE), error = function(e) NA_character_)
  if(is.na(r_crs) || r_crs == ""){
    warning("Raster ", basename(p), " has no CRS; assigning EPSG:32736. Verify correctness.")
    terra::crs(r_try) <- target_wkt
  }
  
  # Set or reproject to target_wkt
  if(!terra::same.crs(terra::crs(r_try), target_wkt)){
    r_try <- tryCatch(terra::project(r_try, target_wkt, method = "bilinear"), error = function(e){
      diag_rows[[yr]] <- tibble::tibble(year = yr, chosen = p, status = paste0("reproject_error: ", conditionMessage(e)))
      return(NULL)
    })
    if(is.null(r_try)) next
  }
  
  # Crop and mask to municipal boundary (with small buffer safety)
  try({
    ext_bb <- terra::ext(terra::ext(gglm_boundary))
    # add small buffer to avoid cut issues
    ext_bb <- terra::ext(ext_bb$xmin - 100, ext_bb$xmax + 100, ext_bb$ymin - 100, ext_bb$ymax + 100)
    r_clip <- terra::crop(r_try, ext_bb)
    r_clip <- terra::mask(r_clip, gglm_boundary)
  }, silent = TRUE) -> tmp_res
  
  if(inherits(tmp_res, "try-error") || !exists("r_clip")){
    diag_rows[[yr]] <- tibble::tibble(year = yr, chosen = p, status = "crop_mask_failed")
    next
  }
  
  # Check values
  if(!terra::hasValues(r_clip)){
    diag_rows[[yr]] <- tibble::tibble(year = yr, chosen = p, status = "no_values_after_clip")
    next
  }
  
  # Save clipped raster
  out_file <- file.path(out_dir, paste0("GW_", yr, "_Interpolated_Clipped.tif"))
  tryCatch({
    terra::writeRaster(r_clip, out_file, overwrite = TRUE)
    loaded_rasters[[yr]] <- out_file
    diag_rows[[yr]] <- tibble::tibble(year = yr, chosen = p, status = "ok", out_file = out_file)
    # set reference if not yet set
    if(is.null(ref_rast)){
      ref_rast <- r_clip
      ref_year <- yr
    }
    gc()
  }, error = function(e){
    diag_rows[[yr]] <- tibble::tibble(year = yr, chosen = p, status = paste0("write_failed: ", conditionMessage(e)))
  })
}

diag_df <- dplyr::bind_rows(diag_rows)
readr::write_csv(diag_df, file.path(out_dir, "Export_Rasters_Diagnostics.csv"))

if(length(loaded_rasters) == 0) stop("No rasters successfully loaded and clipped. Check diagnostics CSV.")

message("Reference raster chosen from year: ", ref_year)


#---Build multi-year raster stack---#

# Ensure all rasters align to ref_rast (resample/project if necessary), then build stack
stack_list <- list()
years_ok <- sort(names(loaded_rasters))
for(yr in years_ok){
  p <- loaded_rasters[[yr]]
  r <- tryCatch(terra::rast(p), error = function(e) NULL)
  if(is.null(r)) next
  # if geometry/resolution differ, resample to ref
  if(!terra::compareGeom(r, ref_rast, stopOnError = FALSE)){
    r2 <- tryCatch(terra::resample(r, ref_rast, method = "bilinear"), error = function(e) NULL)
    if(is.null(r2)) next
    r <- r2
  }
  names(r) <- paste0("GW_", yr)
  stack_list[[yr]] <- r
}

if(length(stack_list) == 0) stop("No rasters to stack after alignment.")

# Combine and write
stack_r <- tryCatch(terra::rast(stack_list), error = function(e) stop("Failed to build stack: ", conditionMessage(e)))
stack_file <- file.path(out_dir, "GW_1995_2005_Stack_Clipped.tif")
terra::writeRaster(stack_r, stack_file, overwrite = TRUE)
message("Wrote multiyear stack: ", stack_file)

# Final diagnostics
diag_df2 <- diag_df %>% dplyr::mutate(final_written = ifelse(status == "ok", TRUE, FALSE))
readr::write_csv(diag_df2, file.path(out_dir, "Export_Rasters_Diagnostics_final.csv"))

message("Script 4.5.C complete. Export directory: ", out_dir)

------------------------------------------------------------------------------------------------------------------------------

# LULC-GROUNDWATER ANAlYSis (D) - Section 4.5
#--ZONAL EXTRACTION (BUFFER LEVEL): GROUNDWATER x LULC--#

# Setting up R and clearing workspace
rm(list = ls())
options(stringsAsFactors = FALSE)

#---Installing/Loading required packages---#

pkgs <- c("sf", "terra", "dplyr", "readr", "stringr", "tibble", "tools", "ggplot2", "tidyr")
installed <- rownames(installed.packages())
to_install <- setdiff(pkgs, installed)
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

#---Paths---#

base_dir <- "M:/GGLM/GGLM_GROUNDWATER"
interp_meta_csv <- file.path(base_dir, "Interpolation_Results", "Interpolation_Metadata.csv")
gw_points_rds   <- file.path(base_dir, "Annual_Shapefiles", "GW_All_points_UTM36S.rds")
buffers_lulc_csv <- file.path(base_dir, "Buffers_LULC_All.csv")

out_dir <- file.path(base_dir, "Zonal_Extraction_Buffers")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

buffer_radius <- 500
target_crs <- "EPSG:32736"

years_to_process <- c(1995,1996,1997,1998,1999,2000,2001,2002,2004,2005)

#---Loading inputs---#

stopifnot(file.exists(gw_points_rds))
gw_sf <- readRDS(gw_points_rds)

if(is.na(st_crs(gw_sf)) || st_crs(gw_sf)$epsg != 32736){
  gw_sf <- st_transform(gw_sf, target_crs)
}

buffers_lulc <- read_csv(buffers_lulc_csv, show_col_types = FALSE)

meta_df <- read_csv(interp_meta_csv, show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    krige_pred = ifelse(is.na(krige_pred), "", krige_pred),
    idw_pred   = ifelse(is.na(idw_pred), "", idw_pred)
  )

#---Creating 500m Buffers---#

gw_unique <- gw_sf %>%
  arrange(Identifier, Year) %>%
  group_by(Identifier) %>%
  slice(1) %>% 
  ungroup() %>%
  dplyr::select(Identifier, DepthCategory, geometry)

buffers_sf <- st_buffer(gw_unique, dist = buffer_radius) %>% st_make_valid()

gpkg_buffers <- file.path(out_dir, "GW_Buffers_500m.gpkg")
st_write(buffers_sf, gpkg_buffers, delete_dsn = TRUE, quiet = TRUE)

buffers_vect <- vect(buffers_sf)

#---Selecting raster per Year---#

meta_select <- meta_df %>%
  mutate(
    chosen_raster = case_when(
      krige_pred != "" & file.exists(krige_pred) ~ krige_pred,
      idw_pred   != "" & file.exists(idw_pred)  ~ idw_pred,
      TRUE ~ ""
    ),
    method_used = case_when(
      chosen_raster == krige_pred ~ "kriging",
      chosen_raster == idw_pred  ~ "idw",
      TRUE ~ "none"
    )
  ) %>%
  filter(year %in% years_to_process) %>%
  dplyr::select(year, method_used, chosen_raster)


#---Extraction Loop---#

zonal_dir <- file.path(out_dir, "Years")
dir.create(zonal_dir, recursive = TRUE, showWarnings = FALSE)

zonal_list <- list()
diagnostic_list <- list()

for(y in sort(unique(meta_select$year))){
  
  message("\n=== YEAR: ", y, " ===")
  
  row_meta <- meta_select %>% filter(year == y)
  rast_path <- row_meta$chosen_raster
  method_used <- row_meta$method_used
  
  # skip missing rasters
  if(!file.exists(rast_path) || rast_path == ""){
    message(" -> No raster found for ", y)
    diagnostic_list[[as.character(y)]] <- tibble(
      year = y, method = method_used, raster_used = "", note = "NO RASTER"
    )
    next
  }
  
  # load raster
  r <- rast(rast_path)
  if(is.na(crs(r))) crs(r) <- target_crs
  
  if(crs(r) != st_crs(buffers_sf)$wkt){
    message(" -> Reprojecting raster to EPSG:32736")
    r <- project(r, st_crs(buffers_sf)$wkt)
  }
  
  # crop raster
  bb <- st_bbox(buffers_sf)
  ext_r <- ext(bb$xmin - 1000, bb$xmax + 1000,
               bb$ymin - 1000, bb$ymax + 1000)
  r_crop <- try(crop(r, ext_r), silent = TRUE)
  if(inherits(r_crop, "try-error")) r_crop <- r
  
  # extract stats
  ext_df <- try(terra::extract(r_crop, buffers_vect, cells = TRUE, df = TRUE),
                silent = TRUE)
  
  if(inherits(ext_df, "try-error") || nrow(ext_df) == 0){
    stats_df <- tibble(
      Identifier = buffers_sf$Identifier,
      year = y, n_cells = 0,
      gw_mean = NA, gw_median = NA, gw_min = NA, gw_max = NA, gw_sd = NA,
      note = "no overlap"
    )
  } else {
    val_col <- setdiff(names(ext_df), c("ID","cell"))[1]
    
    ext_df <- ext_df %>% rename(gw_val = !!sym(val_col)) %>% filter(is.finite(gw_val))
    
    stats_agg <- ext_df %>%
      group_by(ID) %>%
      summarise(
        n_cells = n(),
        gw_mean = mean(gw_val),
        gw_median = median(gw_val),
        gw_min = min(gw_val),
        gw_max = max(gw_val),
        gw_sd = sd(gw_val),
        .groups = "drop"
      )
    
    stats_df <- tibble(ID = seq_len(nrow(buffers_sf)),
                       Identifier = buffers_sf$Identifier) %>%
      left_join(stats_agg, by = "ID") %>%
      mutate(year = y) %>%
      mutate(across(n_cells:gw_sd, ~tidyr::replace_na(.x, NA))) %>%
      mutate(
        note = case_when(
          n_cells == 0 ~ "no overlap",
          n_cells < 5  ~ "very few (caution)",
          n_cells < 20 ~ "low (caution)",
          TRUE ~ "ok"
        )
      ) %>%
      dplyr::select(Identifier, year, n_cells, gw_mean, gw_median,
             gw_min, gw_max, gw_sd, note)
  }
  
  # export outputs
  out_csv <- file.path(zonal_dir, paste0("Buffers_ZonalStats_", y, ".csv"))
  write_csv(stats_df, out_csv)
  zonal_list[[as.character(y)]] <- stats_df
  
  diagnostic_list[[as.character(y)]] <- tibble(
    year = y, method_used = method_used, raster_used = rast_path, note = "ok"
  )
  
  st_write(
    left_join(buffers_sf, stats_df, by = "Identifier"),
    file.path(zonal_dir, paste0("Buffers_", y, ".gpkg")),
    quiet = TRUE, delete_dsn = TRUE
  )
}

#---Merge all Years---# 

zonal_all <- bind_rows(zonal_list)
write_csv(zonal_all, file.path(out_dir, "Buffers_GW_ZonalStats_AllYears.csv"))

diag_df <- bind_rows(diagnostic_list)
write_csv(diag_df, file.path(out_dir, "Buffer_Zonal_Diagnostics.csv"))

names(buffers_lulc) %>% grep("P_LULC", ., value = TRUE)

#---Merge with LULC using year-wise pairing---#

final <- zonal_all %>%
  left_join(buffers_lulc, by = "Identifier") %>%
  mutate(
    LULC_year = case_when(
      year == 1995 ~ 1995,
      year == 1997 ~ 2000,
      year == 2002 ~ 2000,  # nearest available LULC
      TRUE ~ NA_integer_
    )
  ) %>%
  mutate(
    P_LULC_1 = case_when(
      LULC_year == 1995 ~ P_LULC_1,
      LULC_year == 2000 ~ LULC00_P_LULC_1,
      TRUE ~ NA_real_
    ),
    P_LULC_2 = case_when(
      LULC_year == 1995 ~ P_LULC_2,
      LULC_year == 2000 ~ LULC00_P_LULC_2,
      TRUE ~ NA_real_
    ),
    P_LULC_3 = case_when(
      LULC_year == 1995 ~ P_LULC_3,
      LULC_year == 2000 ~ LULC00_P_LULC_3,
      TRUE ~ NA_real_
    ),
    P_LULC_4 = case_when(
      LULC_year == 1995 ~ P_LULC_4,
      LULC_year == 2000 ~ LULC00_P_LULC_4,
      TRUE ~ NA_real_
    ),
    P_LULC_5 = case_when(
      LULC_year == 1995 ~ P_LULC_5,
      LULC_year == 2000 ~ LULC00_P_LULC_5,
      TRUE ~ NA_real_
    ),
    P_LULC_6 = case_when(
      LULC_year == 1995 ~ P_LULC_6,
      LULC_year == 2000 ~ LULC00_P_LULC_6,
      TRUE ~ NA_real_
    )
  ) %>%
  arrange(Identifier, year)

write_csv(final, file.path(out_dir, "Buffers_LULC_GW_Combined.csv"))


#---Computing per-year, per-LULC class distribution statistics---#

library(dplyr)
library(readr)
library(tidyr)

base_dir <- "M:/GGLM/GGLM_GROUNDWATER"
file_in  <- file.path(base_dir, "Zonal_Extraction_Buffers",
                      "Buffers_LULC_GW_Combined.csv")

buffers <- read_csv(file_in, show_col_types = FALSE)

buffers_filt <- buffers %>%
  filter(year %in% c(1995, 1997, 2002))

lulc_long <- buffers_filt %>%
  select(Identifier, year,
         P_LULC_1, P_LULC_2, P_LULC_3,
         P_LULC_4, P_LULC_5, P_LULC_6) %>%
  pivot_longer(
    cols = starts_with("P_LULC_"),
    names_to = "LULC_class",
    values_to = "proportion"
  )

table_16 <- lulc_long %>%
  group_by(year, LULC_class) %>%
  summarise(
    mean_prop   = mean(proportion, na.rm = TRUE),
    median_prop = median(proportion, na.rm = TRUE),
    IQR_prop    = IQR(proportion, na.rm = TRUE),
    min_prop    = min(proportion, na.rm = TRUE),
    max_prop    = max(proportion, na.rm = TRUE),
    n_buffers   = sum(!is.na(proportion)),
    .groups = "drop"
  )

write_csv(
  table_16,
  file.path(base_dir, "Zonal_Extraction_Buffers", "Table_16_LULC_Buffer_Summary.csv")
)

#---Boxplots for LULC proportions---#

library(ggplot2)

fig_19 <- ggplot(lulc_long,
                  aes(x = factor(year),
                      y = proportion)) +
  geom_boxplot(outlier.size = 0.6) +
  facet_wrap(~ LULC_class, ncol = 3) +
  labs(
    x = "Year",
    y = "LULC proportion within 500 m buffer",
    title = "Distribution of LULC proportions within borehole buffers (1995, 1997, 2002)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(base_dir, "Zonal_Extraction_Buffers", "Figure_19_LULC_Buffer_Boxplots.png"),
  plot = fig_19,
  width = 12,
  height = 8,
  dpi = 300
)

#---Identifying dominant LULC per buffer---#

lulc_cols <- paste0("P_LULC_", 1:6)

final_lulc_dom <- final %>%
  mutate(
    # identify rows with valid LULC information
    lulc_sum = rowSums(across(all_of(lulc_cols)), na.rm = TRUE),
    
    dominant_LULC = case_when(
      lulc_sum == 0 ~ NA_character_,
      TRUE ~ lulc_cols[
        max.col(
          as.matrix(across(all_of(lulc_cols))),
          ties.method = "first"
        )
      ]
    )
  ) %>%
  mutate(
    dominant_LULC = recode(
      dominant_LULC,
      "P_LULC_1" = "Built-up",
      "P_LULC_2" = "Agriculture",
      "P_LULC_3" = "Dense vegetation",
      "P_LULC_4" = "Rangeland",
      "P_LULC_5" = "Bare/Eroded",
      "P_LULC_6" = "Water"
    )
  ) %>%
  select(-lulc_sum)

#---Summary statistics of groundwater depth by dominant LULC---#

table_17 <- final_lulc_dom %>%
  filter(
    !is.na(dominant_LULC),
    !is.na(gw_median)
  ) %>%
  group_by(dominant_LULC) %>%
  summarise(
    n_buffers     = n(),
    median_depth  = median(gw_median, na.rm = TRUE),
    mean_depth    = mean(gw_median, na.rm = TRUE),
    IQR_depth     = IQR(gw_median, na.rm = TRUE),
    min_depth     = min(gw_median, na.rm = TRUE),
    max_depth     = max(gw_median, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(median_depth)

write_csv(
  table_17,
  file.path(out_dir, "Table_17_GW_Depth_by_Dominant_LULC.csv")
)

#---Groundwater depth distribution plots---#

library(ggplot2)

fig_20 <- final_lulc_dom %>%
  filter(
    !is.na(dominant_LULC),
    !is.na(gw_median)
  ) %>%
  ggplot(
    aes(
      x = dominant_LULC,
      y = gw_median
    )
  ) +
  geom_boxplot(outlier.size = 0.6) +
  labs(
    x = "Dominant buffer LULC class",
    y = "Median groundwater depth",
    title = "Groundwater depth distributions stratified by dominant buffer LULC"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

ggsave(
  filename = file.path(out_dir, "Figure_20_GW_Depth_by_Dominant_LULC.png"),
  plot = fig_20,
  width = 10,
  height = 6,
  dpi = 300
)

table(final_lulc_dom$dominant_LULC, useNA = "ifany")
table_17

message("\n✓ COMPLETED: 4.5.D — Buffer-level Zonal Extraction")

-----------------------------------------------------------------------------------------------------------------------------

# LULC-GROUNDWATER ANALYSIS (E) - Section 4.5
#--ZONAL EXTRACTION (MUNICIPAL LEVEL): GROUNDWATER x LULC--#

#Setting up R and clearing workspace
rm(list = ls())
options(stringsAsFactors = FALSE)

#---Installing/Loading packages---#

pkgs <- c("terra","sf","dplyr","tidyr","readr","stringr","tibble","purrr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

#---Paths & Parameters---# 

base_dir <- "M:/GGLM/GGLM_GROUNDWATER"
interp_dir <- file.path(base_dir, "Interpolation_Results")
out_dir    <- file.path(interp_dir, "Final_GW_Rasters")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

meta_csv <- file.path(interp_dir, "Interpolation_Metadata.csv")
if(!file.exists(meta_csv)) stop("Metadata CSV not found: ", meta_csv)

lulc_files <- list(
  "1995" = file.path("M:/GGLM/GGLM_LULC/QGIS_MAPS", "Corrected_LULC_1995.tif"),
  "2000" = file.path("M:/GGLM/GGLM_LULC/QGIS_MAPS", "Corrected_LULC_2000.tif"),
  "2005" = file.path("M:/GGLM/GGLM_LULC/QGIS_MAPS", "Corrected_LULC_2005.tif")
)

wards_shp <- "M:/GGLM/GGLM_SHAPEFILES/GreaterGiyani_Wards.shp"
if(!file.exists(wards_shp)) stop("Wards shapefile not found: ", wards_shp)

# standard target CRS
target_epsg <- 32736
target_crs <- sf::st_crs(target_epsg)
target_wkt <- target_crs$wkt

# delta pairs
delta_pairs <- list(c(1995,2000), c(2000,2005), c(1995,2005))

# flags
use_rasterize_for_wards <- FALSE
do_ward_class <- TRUE      # WARNING: can be slow for many classes & wards

# output folders & logging
out_stats_dir <- file.path(base_dir, "Analysis_Outputs")
dir.create(out_stats_dir, recursive = TRUE, showWarnings = FALSE)
delta_dir <- file.path(out_stats_dir, "Deltas"); dir.create(delta_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(out_stats_dir, "4.5E_process_log.txt")
cat(paste(Sys.time(), "Starting 4.5.E"), file = log_file, sep = "\n")

#---Read metadata + decide final raster per year---#

meta_df <- readr::read_csv(meta_csv, show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    krige_pred = ifelse(is.na(krige_pred), "", krige_pred),
    idw_pred   = ifelse(is.na(idw_pred),   "", idw_pred)
  ) %>%
  mutate(final_raster = dplyr::case_when(
    krige_pred != "" & file.exists(krige_pred) ~ krige_pred,
    idw_pred  != "" & file.exists(idw_pred)  ~ idw_pred,
    TRUE ~ ""
  ))

if(any(meta_df$final_raster == "")){
  warn_msg <- paste(Sys.time(), "- WARNING: some years have no final raster chosen in metadata")
  warning(warn_msg); cat(warn_msg, file = log_file, sep = "\n", append = TRUE)
}

meta_df <- meta_df %>% filter(final_raster != "" & file.exists(final_raster))
if(nrow(meta_df) == 0) stop("No final rasters found in metadata — aborting.")

cat(paste(Sys.time(), "- Years available in metadata:", paste(meta_df$year, collapse = ", ")),
    file = log_file, sep = "\n", append = TRUE)

#---Load final rasters, reproject to target CRS, save standardised copies---#

gw_rasters <- list()
# We'll pick the first valid raster as reference later
for(i in seq_len(nrow(meta_df))){
  yr <- as.character(meta_df$year[i])
  p  <- meta_df$final_raster[i]
  cat(paste(Sys.time(), "- Loading raster:", p), file = log_file, sep = "\n", append = TRUE)
  r_try <- tryCatch(terra::rast(p), error = function(e){
    msg <- paste("Failed to load", p, ":", e$message); cat(msg, file = log_file, sep = "\n", append = TRUE); NULL
  })
  if(is.null(r_try)) next
  if(terra::nlyr(r_try) > 1) r_try <- r_try[[1]]
  # If raster has no CRS, warn and assume it should be target_crs (but DO NOT FORCE if uncertain)
  if(is.na(terra::crs(r_try, proj = TRUE)) || terra::crs(r_try, proj = TRUE) == ""){
    msg <- paste("Raster", basename(p), "has no CRS. Assigning target CRS (EPSG:", target_epsg, ") — please verify.");
    warning(msg); cat(msg, file = log_file, sep = "\n", append = TRUE)
    terra::crs(r_try) <- target_wkt
  }
  # Reproject if needed
  if(!terra::same.crs(r_try, terra::crs(target_wkt))){
    # project to target_wkt
    cat(paste(Sys.time(), "- Reprojecting", basename(p), "to EPSG:", target_epsg), file = log_file, sep = "\n", append = TRUE)
    r_try <- terra::project(r_try, target_wkt)
  }
  # store
  gw_rasters[[yr]] <- r_try
  outp <- file.path(out_dir, paste0("GW_final_", yr, ".tif"))
  terra::writeRaster(r_try, outp, overwrite = TRUE)
  cat(paste(Sys.time(), "- Wrote standardized raster:", outp), file = log_file, sep = "\n", append = TRUE)
  gc()
}

if(length(gw_rasters) == 0) stop("No GW rasters loaded after reprojection — aborting.")
cat(paste(Sys.time(), "- Final GW raster years:", paste(names(gw_rasters), collapse = ", ")),
    file = log_file, sep = "\n", append = TRUE)

# Build and save stack
stack_r <- tryCatch(terra::rast(gw_rasters), error = function(e){ stop("Failed to build stack: ", e$message) })
names(stack_r) <- paste0("GW_", names(gw_rasters))
stack_file <- file.path(out_dir, "GW_Final_Stack.tif")
terra::writeRaster(stack_r, stack_file, overwrite = TRUE)
cat(paste(Sys.time(), "- Stack written:", stack_file), file = log_file, sep = "\n", append = TRUE)

#---Load LULC rasters & Wards; align LULC to reference grid---#

for(s in names(lulc_files)) {
  if(!file.exists(lulc_files[[s]])) stop("Missing LULC snapshot: ", lulc_files[[s]])
}

# choose a reference raster (first in gw_rasters)
ref_rast <- gw_rasters[[1]]
ref_res  <- terra::res(ref_rast)
ref_crs  <- terra::crs(ref_rast, proj = TRUE)

lulc_r_list <- list()
for(snap in names(lulc_files)){
  lr <- terra::rast(lulc_files[[snap]])
  # if LULC missing crs, set to target
  if(is.na(terra::crs(lr, proj = TRUE)) || terra::crs(lr, proj = TRUE) == ""){
    warning(paste("LULC", snap, "has no CRS; assigning target EPSG:", target_epsg))
    terra::crs(lr) <- target_wkt
  }
  # ensure same crs as reference
  if(!terra::same.crs(lr, ref_rast)) lr <- terra::project(lr, ref_rast, method = "near")
  # resample to match grid/alignment
  lr_aligned <- tryCatch({
    terra::resample(lr, ref_rast, method = "near")
  }, error = function(e){
    lr2 <- terra::project(lr, ref_rast, method = "near")
    terra::resample(lr2, ref_rast, method = "near")
  })
  lulc_r_list[[snap]] <- lr_aligned
  cat(paste(Sys.time(), "- Aligned LULC snapshot:", snap), file = log_file, sep = "\n", append = TRUE)
  gc()
}

# read wards and prepare
wards_sf <- sf::st_read(wards_shp, quiet = TRUE) %>% st_transform(target_epsg) %>% dplyr::mutate(ID = dplyr::row_number())
wards_vect <- terra::vect(wards_sf)
cat(paste(Sys.time(), "- Loaded wards; n wards =", nrow(wards_sf)), file = log_file, sep = "\n", append = TRUE)

#---Helpers---#

map_to_lulc <- function(year_int, lulc_snapshot_years){
  snaps <- as.integer(lulc_snapshot_years)
  snaps[which.min(abs(snaps - year_int))]
}

# safe function to check raster has values
rast_has_values <- function(r){
  if(is.null(r)) return(FALSE)
  tryCatch(terra::hasValues(r), error = function(e) FALSE)
}

#---Main processing: municipal LULC stats, Ward stats, Ward x class---#

lulc_class_stats_list <- list()
ward_stats_list <- list()
ward_class_stats_list <- list()

for(yr in names(gw_rasters)){
  cat(paste0("\n", Sys.time(), " - Processing year: ", yr), file = log_file, sep = "\n", append = TRUE)
  yr_i <- as.integer(yr)
  r_gw <- gw_rasters[[yr]]
  if(!rast_has_values(r_gw)){
    warn_msg <- paste(Sys.time(), "- Raster for year", yr, "has no values; skipping year.")
    warning(warn_msg); cat(warn_msg, file = log_file, sep = "\n", append = TRUE)
    next
  }
  nearest_lulc_snap <- map_to_lulc(yr_i, names(lulc_r_list))
  r_lulc <- lulc_r_list[[as.character(nearest_lulc_snap)]]
  cat(paste(Sys.time(), "- Using LULC snapshot:", nearest_lulc_snap, "for GW year", yr), file = log_file, sep = "\n", append = TRUE)
  
  # (A) LULC-class statistics (municipality-wide)
  z_mean  <- terra::zonal(r_gw, r_lulc, fun = "mean", na.rm = TRUE)
  z_count <- terra::zonal(!is.na(r_gw), r_lulc, fun = "sum", na.rm = TRUE)
  z_min   <- terra::zonal(r_gw, r_lulc, fun = "min", na.rm = TRUE)
  z_max   <- terra::zonal(r_gw, r_lulc, fun = "max", na.rm = TRUE)
  z_sd    <- terra::zonal(r_gw, r_lulc, fun = "sd", na.rm = TRUE)
  
  class_stats <- tibble::tibble(
    lulc = z_mean[,1],
    gw_mean = z_mean[,2],
    n_cells = z_count[,2],
    gw_min = z_min[,2],
    gw_max = z_max[,2],
    gw_sd = z_sd[,2]
  ) %>%
    dplyr::mutate(
      area_m2 = n_cells * prod(ref_res),
      area_ha = area_m2 / 10000,
      year = yr_i,
      lulc_snapshot = nearest_lulc_snap
    ) %>%
    dplyr::relocate(year, lulc_snapshot, .before = lulc)
  
  lulc_class_stats_list[[yr]] <- class_stats
  readr::write_csv(class_stats, file.path(out_stats_dir, paste0("LULC_ClassStats_", yr, ".csv")))
  cat(paste(Sys.time(), "- Wrote LULC class stats for", yr), file = log_file, sep = "\n", append = TRUE)
  
  # (B) Ward-level GW stats
  if(!use_rasterize_for_wards){
    ext_df <- terra::extract(r_gw, wards_vect, cells = FALSE, df = TRUE)
    # pick value column (not 'ID')
    val_col <- setdiff(names(ext_df), "ID")[1]
    names(ext_df)[names(ext_df) == val_col] <- "gw_val"
    ext_df2 <- dplyr::as_tibble(ext_df) %>% dplyr::filter(!is.na(gw_val) & is.finite(gw_val))
    
    if(nrow(ext_df2) == 0){
      ward_stats <- tibble::tibble(
        ward_code = wards_sf$GID_4,
        ward_name = wards_sf$NAME_4,
        year = yr_i,
        n_cells = 0L,
        gw_mean = NA_real_
      )
    } else {
      ward_agg <- dplyr::as_tibble(ext_df2) %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(
          n_cells = dplyr::n(),
          gw_mean = mean(gw_val, na.rm = TRUE),
          .groups = "drop"
        )
      ward_stats <- dplyr::left_join(
        tibble::tibble(ID = wards_sf$ID, ward_code = wards_sf$GID_4, ward_name = wards_sf$NAME_4),
        ward_agg, by = "ID"
      ) %>%
        tidyr::replace_na(list(n_cells = 0L, gw_mean = NA_real_)) %>%
        dplyr::mutate(year = yr_i) %>%
        dplyr::select(ward_code, ward_name, year, n_cells, gw_mean)
    }
  } else {
    wards_r <- terra::rasterize(wards_vect, r_gw, field = "ID")
    ward_mean <- terra::zonal(r_gw, wards_r, fun = "mean", na.rm = TRUE)
    ward_count <- terra::zonal(!is.na(r_gw), wards_r, fun = "sum", na.rm = TRUE)
    ward_stats <- tibble::tibble(
      ID = ward_mean[,1],
      gw_mean = ward_mean[,2],
      n_cells = ward_count[,2]
    ) %>%
      dplyr::left_join(tibble::tibble(ID = seq_len(nrow(wards_sf)), ward_name = wards_sf$NAME_4, ward_code = wards_sf$GID_4), by = "ID") %>%
      dplyr::mutate(year = yr_i) %>%
      dplyr::select(ward_code, ward_name, year, n_cells, gw_mean)
  }
  
  ward_stats_list[[yr]] <- ward_stats
  readr::write_csv(ward_stats, file.path(out_stats_dir, paste0("Ward_GW_Stats_", yr, ".csv")))
  cat(paste(Sys.time(), "- Wrote ward stats for", yr), file = log_file, sep = "\n", append = TRUE)
  
  # (C) Ward x LULC-class stats (optional)
  if(do_ward_class){
    cat(paste(Sys.time(), "- Computing ward x class stats for", yr), file = log_file, sep = "\n", append = TRUE)
    lulc_vals <- unique(terra::values(r_lulc))
    lulc_cats <- na.omit(sort(as.integer(lulc_vals)))
    wc_rows <- list()
    for(cat in lulc_cats){
      mask_r <- terra::ifel(r_lulc == cat, r_gw, NA)
      if(!use_rasterize_for_wards){
        ext_cat <- terra::extract(mask_r, wards_vect, cells = FALSE, df = TRUE)
        valcol <- setdiff(names(ext_cat), "ID")[1]
        names(ext_cat)[names(ext_cat) == valcol] <- "gw_val"
        ext_cat2 <- dplyr::as_tibble(ext_cat) %>% dplyr::filter(!is.na(gw_val) & is.finite(gw_val))
        if(nrow(ext_cat2) == 0){
          tmp <- tibble::tibble(ID = wards_sf$ID, n_cells = 0L, gw_mean = NA_real_)
        } else {
          tmp <- ext_cat2 %>%
            dplyr::group_by(ID) %>%
            dplyr::summarise(n_cells = dplyr::n(), gw_mean = mean(gw_val, na.rm = TRUE), .groups = "drop")
          tmp <- dplyr::left_join(tibble::tibble(ID = wards_sf$ID, ward_code = wards_sf$GID_4),
                                  tmp, by = "ID") %>%
            dplyr::mutate(lulc = cat, year = yr_i) %>%
            tidyr::replace_na(list(n_cells = 0L, gw_mean = NA_real_))
        }
      } else {
        wards_r <- terra::rasterize(wards_vect, mask_r, field = "ID")
        ext_cat <- terra::zonal(mask_r, wards_r, fun = "mean", na.rm = TRUE)
        ext_count <- terra::zonal(!is.na(mask_r), wards_r, fun = "sum", na.rm = TRUE)
        tmp <- tibble::tibble(ID = ext_cat[,1], gw_mean = ext_cat[,2], n_cells = ext_count[,2]) %>%
          dplyr::left_join(tibble::tibble(ID = seq_len(nrow(wards_sf)), ward_code = wards_sf$GID_4), by = "ID") %>%
          dplyr::mutate(lulc = cat, year = yr_i) %>%
          tidyr::replace_na(list(n_cells = 0L, gw_mean = NA_real_))
      }
      wc_rows[[as.character(cat)]] <- tmp
      gc()
    }
    ward_class_stats <- dplyr::bind_rows(wc_rows)
    ward_class_stats_list[[yr]] <- ward_class_stats
    readr::write_csv(ward_class_stats, file.path(out_stats_dir, paste0("Ward_LULC_GW_Stats_", yr, ".csv")))
    cat(paste(Sys.time(), "- Wrote ward x class stats for", yr), file = log_file, sep = "\n", append = TRUE)
  }
  
  rm(r_gw, r_lulc); gc()
} # end for years

#---Deltas: pairwise differences and ward extraction---#

for(pair in delta_pairs){
  y1 <- as.character(pair[1]); y2 <- as.character(pair[2])
  cat(paste(Sys.time(), "- Delta pair:", y1, y2), file = log_file, sep = "\n", append = TRUE)
  
  if(!(y1 %in% names(gw_rasters) && y2 %in% names(gw_rasters))){
    warn_msg <- paste(Sys.time(), "- Skipping delta; missing years:", y1, y2)
    warning(warn_msg); cat(warn_msg, file = log_file, sep = "\n", append = TRUE); next
  }
  
  r1 <- gw_rasters[[y1]]; r2 <- gw_rasters[[y2]]
  
  # skip if either raster has no values
  if(!rast_has_values(r1) || !rast_has_values(r2)){
    msg <- paste(Sys.time(), "- One of the rasters has no values; skipping delta for", y1, y2)
    warning(msg); cat(msg, file = log_file, sep = "\n", append = TRUE); next
  }
  
  # ensure same CRS
  if(!terra::same.crs(r1, r2)) r2 <- terra::project(r2, r1)
  
  # ensure same geometry/resolution
  if(!terra::compareGeom(r1, r2, stopOnError = FALSE)){
    r2 <- terra::resample(r2, r1, method = "bilinear")
  }
  
  delta_r <- r2 - r1
  delta_file <- file.path(delta_dir, paste0("GW_delta_", y1, "_", y2, ".tif"))
  terra::writeRaster(delta_r, delta_file, overwrite = TRUE)
  cat(paste(Sys.time(), "- Delta raster written:", delta_file), file = log_file, sep = "\n", append = TRUE)
  
  # extract ward-level delta stats
  if(!use_rasterize_for_wards){
    ext_df <- terra::extract(delta_r, wards_vect, cells = FALSE, df = TRUE)
    val_col <- setdiff(names(ext_df), "ID")[1]
    names(ext_df)[names(ext_df) == val_col] <- "delta_val"
    ext_df2 <- dplyr::as_tibble(ext_df) %>% dplyr::filter(!is.na(delta_val) & is.finite(delta_val))
    
    if(nrow(ext_df2) > 0){
      delta_ward <- ext_df2 %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(n_cells = dplyr::n(), delta_mean = mean(delta_val, na.rm = TRUE),
                         delta_sd = sd(delta_val, na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(tibble::tibble(ID = wards_sf$ID, ward_code = wards_sf$GID_4, ward_name = wards_sf$NAME_4),
                         by = "ID") %>%
        tidyr::replace_na(list(n_cells = 0L, delta_mean = NA_real_, delta_sd = NA_real_))
      readr::write_csv(delta_ward, file.path(delta_dir, paste0("Ward_DeltaStats_", y1, "_", y2, ".csv")))
      cat(paste(Sys.time(), "- Wrote delta ward stats for", y1, y2), file = log_file, sep = "\n", append = TRUE)
    } else {
      warn_msg <- paste(Sys.time(), "- No finite delta values inside wards for pair", y1, y2)
      warning(warn_msg); cat(warn_msg, file = log_file, sep = "\n", append = TRUE)
    }
  } else {
    wards_r <- terra::rasterize(wards_vect, r1, field = "ID")
    ext_mean <- terra::zonal(delta_r, wards_r, fun = "mean", na.rm = TRUE)
    ext_count <- terra::zonal(!is.na(delta_r), wards_r, fun = "sum", na.rm = TRUE)
    if(nrow(ext_mean) > 0){
      delta_ward <- tibble::tibble(ID = ext_mean[,1], delta_mean = ext_mean[,2], n_cells = ext_count[,2]) %>%
        dplyr::left_join(tibble::tibble(ID = wards_sf$ID, ward_code = wards_sf$GID_4, ward_name = wards_sf$NAME_4), by = "ID") %>%
        tidyr::replace_na(list(n_cells = 0L, delta_mean = NA_real_))
      readr::write_csv(delta_ward, file.path(delta_dir, paste0("Ward_DeltaStats_", y1, "_", y2, ".csv")))
      cat(paste(Sys.time(), "- Wrote delta ward stats (rasterized) for", y1, y2), file = log_file, sep = "\n", append = TRUE)
    } else {
      warn_msg <- paste(Sys.time(), "- No finite delta values inside wards for pair", y1, y2)
      warning(warn_msg); cat(warn_msg, file = log_file, sep = "\n", append = TRUE)
    }
  }
  
  gc()
} # end delta loop

#---Final reporting: concatenate and write master tables---#

if(length(lulc_class_stats_list) > 0){
  all_class_stats <- dplyr::bind_rows(lulc_class_stats_list)
  readr::write_csv(all_class_stats, file.path(out_stats_dir, "LULC_ClassStats_allYears.csv"))
  cat(paste(Sys.time(), "- Wrote LULC_ClassStats_allYears.csv"), file = log_file, sep = "\n", append = TRUE)
}

if(length(ward_stats_list) > 0){
  all_ward_stats <- dplyr::bind_rows(ward_stats_list)
  readr::write_csv(all_ward_stats, file.path(out_stats_dir, "Ward_GW_Stats_allYears.csv"))
  cat(paste(Sys.time(), "- Wrote Ward_GW_Stats_allYears.csv"), file = log_file, sep = "\n", append = TRUE)
}

if(length(ward_class_stats_list) > 0){
  readr::write_csv(dplyr::bind_rows(ward_class_stats_list), file.path(out_stats_dir, "Ward_LULC_GW_Stats_allYears.csv"))
  cat(paste(Sys.time(), "- Wrote Ward_LULC_GW_Stats_allYears.csv"), file = log_file, sep = "\n", append = TRUE)
}

#---Ward-level aggregated groundwater and dominant LULC metrics---#

# Loading required outputs
library(dplyr)
library(readr)

# Load ward-level outputs 
ward_gw <- read_csv(
  "M:/GGLM/GGLM_GROUNDWATER/Analysis_Outputs/Ward_GW_Stats_allYears.csv",
  show_col_types = FALSE
)

ward_lulc <- read_csv(
  "M:/GGLM/GGLM_GROUNDWATER/Analysis_Outputs/Ward_LULC_GW_Stats_allYears.csv",
  show_col_types = FALSE
)

# Aggregate groundwater across years 
ward_gw_agg <- ward_gw %>%
  group_by(ward_code, ward_name) %>%
  summarise(
    mean_groundwater_depth_m = mean(gw_mean, na.rm = TRUE),
    total_gw_cells = sum(n_cells, na.rm = TRUE),
    .groups = "drop"
  )

#  Aggregate LULC across years 
ward_lulc_agg <- ward_lulc %>%
  group_by(ward_code, lulc) %>%
  summarise(
    class_cells = sum(n_cells, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ward_code) %>%
  mutate(
    total_cells = sum(class_cells, na.rm = TRUE),
    proportion = class_cells / total_cells
  ) %>%
  arrange(ward_code, desc(proportion)) %>%
  slice(1) %>%   # dominant LULC per ward
  ungroup() %>%
  select(
    ward_code,
    dominant_LULC = lulc,
    dominant_LULC_prop = proportion
  )

# Final Table 18 
table_18 <- ward_gw_agg %>%
  left_join(ward_lulc_agg, by = "ward_code") %>%
  mutate(
    Dominant_LULC_Proportion_pct = round(dominant_LULC_prop * 100, 1),
    Mean_Groundwater_Depth_m = round(mean_groundwater_depth_m, 2)
  ) %>%
  select(
    Ward_Code = ward_code,
    Ward_Name = ward_name,
    Mean_Groundwater_Depth_m,
    Dominant_LULC_Class = dominant_LULC,
    Dominant_LULC_Proportion_pct,
    Groundwater_Cell_Count = total_gw_cells
  )

# Export 
write_csv(
  table_18,
  "M:/GGLM/GGLM_GROUNDWATER/Analysis_Outputs/Table_18_Ward_GW_LULC_Summary.csv"
)

#---Comparative plots of buffer-scale vs ward-level groundwater–LULC associations---#

# Loading required package 
library(tidyverse)

# Read in buffer scale data 
buffers <- read_csv("M:/GGLM/GGLM_GROUNDWATER/Zonal_Extraction_Buffers/Buffers_LULC_GW_Combined.csv")

# Filter to analysis year (1995)
buffers_1995 <- buffers %>%
  filter(year == 1995)

# Derive dominant LULC per buffer
lulc_cols <- c(
  "P_LULC_1",
  "P_LULC_2",
  "P_LULC_3",
  "P_LULC_4",
  "P_LULC_5",
  "P_LULC_6"
)

buffers_1995 <- buffers_1995 %>%
  rowwise() %>%
mutate(
  dominant_LULC_code = which.max(c_across(all_of(lulc_cols)))
) %>%
  ungroup()

# Recode dominant LULC to class names
buffers_1995 <- buffers_1995 %>%
  mutate(
    dominant_LULC = recode(
      dominant_LULC_code,
      `1` = "Built-up",
      `2` = "Agriculture",
      `3` = "Dense vegetation",
      `4` = "Rangeland",
      `5` = "Bare/Eroded",
      `6` = "Water"
    )
  )

buffers_dom <- buffers_1995 %>%
  filter(!is.na(dominant_LULC))

# Read in ward scale data
wards <- read_csv("M:/GGLM/GGLM_GROUNDWATER/Analysis_Outputs/Table_5_5_Ward_GW_LULC_Summary.csv")

# Recode LULC classes
wards <- wards %>%
  mutate(
    dominant_LULC = recode(
      as.character(Dominant_LULC_Class),
      "1" = "Built-up",
      "2" = "Agriculture",
      "3" = "Dense vegetation",
      "4" = "Rangeland",
      "5" = "Bare/Eroded",
      "6" = "Water"
    )
  )


# Panel A : Buffer scale 

p_buffer <- ggplot(buffers_dom, aes(x = dominant_LULC, y = gw_median)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    title = "Buffer-scale groundwater depth by dominant LULC (1995)",
    x = "Dominant LULC class",
    y = "Median groundwater depth (m)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


# Panel B : Ward scale

p_ward <- ggplot(wards, aes(x = dominant_LULC, y = Mean_Groundwater_Depth_m)) +
  geom_point(size = 3) +
  geom_jitter(width = 0.15) +
  labs(
    title = "Ward-level mean groundwater depth by dominant LULC",
    x = "Dominant LULC class",
    y = "Mean groundwater depth (m)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Combine and export figure 
library(patchwork)

figure_22 <- p_buffer / p_ward

figure_22

output_dir <- "M:/GGLM/GGLM_GROUNDWATER/Analysis_Outputs"

ggsave(
  filename = file.path(output_dir, "Figure_22_Buffer_vs_Ward_Scale_Comparison.png"),
  plot = figure_22,
  width = 10,
  height = 12,
  dpi = 300
)

writeLines(capture.output(sessionInfo()), file.path(out_stats_dir, "sessionInfo_4.4E.txt"))
cat(paste(Sys.time(), "Finished 4.5.E"), file = log_file, sep = "\n", append = TRUE)
message("4.5.E complete. Outputs in: ", out_stats_dir)

-----------------------------------------------------------------------------------------------------------------

# LULC-GROUNDWATER ANALYSIS (F) - Section 4.5
#--TREND ANALYSIS & DELTA CALCULATION (BUFFERS, WARDS, WARD x LULC)--#

# Setting up R and clearing workspace
rm(list = ls())
options(stringsAsFactors = FALSE)

#---Installing/Loading packages---#

pkgs <- c("dplyr","tidyr","readr","purrr","stringr","tibble")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

#---Paths and parameters---#

base_dir <- "M:/GGLM/GGLM_GROUNDWATER"
out_dir <- file.path(base_dir, "Analysis_Outputs", "Deltas")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(out_dir, "4.4.F_process_log.txt")
cat(paste(Sys.time(), "Starting 4.4.F — Delta Computation"), file = log_file, sep = "\n")

buffer_stats_file <- file.path(base_dir, "Zonal_Extraction_Buffers", "Buffers_GW_ZonalStats_AllYears.csv")
ward_stats_dir <- file.path(base_dir, "Analysis_Outputs")  # contains Ward_GW_Stats_allYears.csv and Ward_LULC_GW_Stats_*.csv

delta_pairs <- list(c(1995,2000), c(2000,2005), c(1995,2005))

#---Safe CSV read function---#

safe_read_csv <- function(path){
  tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e){warning("Failed to read ", path); NULL})
}

#---Read previous outputs---#

buffer_stats <- safe_read_csv(buffer_stats_file)
if(is.null(buffer_stats)) stop("Buffer stats file missing or unreadable")

ward_stats_file <- file.path(ward_stats_dir, "Ward_GW_Stats_allYears.csv")
ward_stats <- safe_read_csv(ward_stats_file)
if(is.null(ward_stats)) stop("Ward stats file missing or unreadable")

#---Delta computation functions---#

compute_delta_generic <- function(df, year1, year2, id_col = "Identifier", value_col = "gw_mean") {
  df1 <- df %>% filter(year == year1) %>% dplyr::select(all_of(c(id_col, value_col)))
  df2 <- df %>% filter(year == year2) %>% dplyr::select(all_of(c(id_col, value_col)))
  
  df_delta <- df1 %>%
    dplyr::inner_join(df2, by = id_col, suffix = c("_y1", "_y2")) %>%
    dplyr::mutate(
      delta_abs = !!sym(paste0(value_col, "_y2")) - !!sym(paste0(value_col, "_y1")),
      delta_pct = ifelse(!!sym(paste0(value_col, "_y1")) != 0,
                         100 * delta_abs / !!sym(paste0(value_col, "_y1")),
                         NA_real_)
    )
  
  df_delta$year1 <- year1
  df_delta$year2 <- year2
  return(df_delta)
}

compute_delta_lulc <- function(df, year1, year2, id_col = "ward_code", class_col = "lulc", value_col = "gw_mean") {
  df1 <- df %>% filter(year == year1) %>% dplyr::select(all_of(c(id_col, class_col, value_col)))
  df2 <- df %>% filter(year == year2) %>% dplyr::select(all_of(c(id_col, class_col, value_col)))
  
  df_delta <- df1 %>%
    dplyr::inner_join(df2, by = c(id_col, class_col), suffix = c("_y1", "_y2")) %>%
    dplyr::mutate(
      delta_abs = !!sym(paste0(value_col, "_y2")) - !!sym(paste0(value_col, "_y1")),
      delta_pct = ifelse(!!sym(paste0(value_col, "_y1")) != 0,
                         100 * delta_abs / !!sym(paste0(value_col, "_y1")),
                         NA_real_)
    )
  
  df_delta$year1 <- year1
  df_delta$year2 <- year2
  return(df_delta)
}

#---Compute buffer-level deltas---#

buffer_deltas <- purrr::map_dfr(delta_pairs, ~compute_delta_generic(buffer_stats, .x[1], .x[2]))
readr::write_csv(buffer_deltas, file.path(out_dir, "Buffer_GW_Deltas.csv"))
cat(paste(Sys.time(), "- Wrote Buffer_GW_Deltas.csv"), file = log_file, sep = "\n", append = TRUE)

#---Compute ward-level deltas---#

ward_deltas <- purrr::map_dfr(delta_pairs, ~compute_delta_generic(ward_stats, .x[1], .x[2], id_col = "ward_code"))
readr::write_csv(ward_deltas, file.path(out_dir, "Ward_GW_Deltas.csv"))
cat(paste(Sys.time(), "- Wrote Ward_GW_Deltas.csv"), file = log_file, sep = "\n", append = TRUE)

#---Compute Ward × LULC deltas---#

lulc_files <- list.files(ward_stats_dir, pattern = "^Ward_LULC_GW_Stats_.*\\.csv$", full.names = TRUE)
lulc_list <- purrr::map(lulc_files, safe_read_csv)
lulc_list <- lulc_list[!sapply(lulc_list, is.null)]

if(length(lulc_list) > 0){
  ward_lulc_stats <- dplyr::bind_rows(lulc_list)
  lulc_deltas <- purrr::map_dfr(delta_pairs, ~compute_delta_lulc(ward_lulc_stats, .x[1], .x[2]))
  readr::write_csv(lulc_deltas, file.path(out_dir, "Ward_LULC_GW_Deltas.csv"))
  cat(paste(Sys.time(), "- Wrote Ward_LULC_GW_Deltas.csv"), file = log_file, sep = "\n", append = TRUE)
} else {
  warning("No Ward × LULC CSVs found; skipping LULC delta computation")
}

#---Boxplots of ΔGWL by dominant LULC class---#

library(dplyr)
library(readr)
library(ggplot2)

# Paths
delta_file <- "M:/GGLM/GGLM_GROUNDWATER/Analysis_Outputs/Deltas/Ward_LULC_GW_Deltas.csv"
out_dir <- "M:/GGLM/GGLM_GROUNDWATER/Analysis_Outputs/Figures"

# output directory
dir.create(out_dir, showWarnings = FALSE)

# Read data
df <- read_csv(delta_file, show_col_types = FALSE)

# Recode LULC classes
lulc_labels <- c(
  "1" = "Built-up",
  "2" = "Agriculture",
  "3" = "Dense vegetation",
  "4" = "Rangeland",
  "5" = "Bare/Eroded",
  "6" = "Water"
)

# clean data: remove zeros + NA + outliers
df_clean <- df %>%
  filter(!is.na(delta_abs), lulc != 0) %>%  # **REMOVES CLASS 0**
  mutate(
    LULC_Class = recode(as.character(lulc), !!!lulc_labels),
    Interval = paste(year1, year2, sep = "–")
  )


# Plot
p <- ggplot(df_clean, aes(x = LULC_Class, y = delta_abs)) +
  geom_boxplot(outlier.size = 0.6, fill = "grey85") +
  facet_wrap(~ Interval, scales = "free_y") +
  labs(
    x = "Dominant LULC class",
    y = "Groundwater-level change ΔGWL (m)",
    title = "Distribution of groundwater-level change by dominant LULC class"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )

  # FACETS WITH FIXED AXES (CRITICAL FIX)
  facet_wrap(~ Interval, scales = "fixed") +  # **CHANGED FROM "free_y"**
  
# PREVIEW
print(p)

ggsave(
  filename = file.path(out_dir, "Figure_x3_DeltaGWL_by_LULC.png"),
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

#---Finish---#

cat(paste(Sys.time(), "Finished 4.5.F — Delta Computation"), file = log_file, sep = "\n")
message("4.5.F complete. Outputs in: ", out_dir)

----------------------------------------------------------------------------------------------------------------------------

# LULC-GROUNDWATER ANALYSIS (G) - Section 4.5
#--SUMMARY STATISTICS & TREND REPORTING (BUFFERS, WARDS, WARD × LULC)--#

# Setting up R and clearing workspace
rm(list = ls())
options(stringsAsFactors = FALSE)

#---Packages---#

pkgs <- c("dplyr","tidyr","readr","purrr","stringr","tibble")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

#---Paths and parameters---#

base_dir <- "M:/GGLM/GGLM_GROUNDWATER"
delta_dir <- file.path(base_dir, "Analysis_Outputs", "Deltas")
summary_dir <- file.path(base_dir, "Analysis_Outputs", "Summaries")
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(summary_dir, "4.4.G_process_log.txt")
cat(paste(Sys.time(), "Starting 4.4.G — Summary Statistics"), file = log_file, sep = "\n")

#---Safe CSV read function---#

safe_read_csv <- function(path){
  tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e){warning("Failed to read ", path); NULL})
}

#---Helper: compute summary stats---#

compute_summary <- function(df, value_col = "delta_abs", group_cols = c("year1","year2")){
  df %>%
    dplyr::group_by(across(all_of(group_cols))) %>%
    dplyr::summarise(
      mean_val = mean(.data[[value_col]], na.rm = TRUE),
      sd_val = sd(.data[[value_col]], na.rm = TRUE),
      min_val = min(.data[[value_col]], na.rm = TRUE),
      max_val = max(.data[[value_col]], na.rm = TRUE),
      median_val = median(.data[[value_col]], na.rm = TRUE),
      n = sum(!is.na(.data[[value_col]])),
      .groups = "drop"
    )
}

#---Read delta CSVs---#

buffer_deltas_file <- file.path(delta_dir, "Buffer_GW_Deltas.csv")
ward_deltas_file   <- file.path(delta_dir, "Ward_GW_Deltas.csv")
lulc_deltas_file   <- file.path(delta_dir, "Ward_LULC_GW_Deltas.csv")

buffer_deltas <- safe_read_csv(buffer_deltas_file)
ward_deltas   <- safe_read_csv(ward_deltas_file)
lulc_deltas   <- safe_read_csv(lulc_deltas_file)

#---Compute summaries---#

if(!is.null(buffer_deltas)){
  buffer_summary <- compute_summary(buffer_deltas)
  readr::write_csv(buffer_summary, file.path(summary_dir, "Buffer_GW_Delta_Summary.csv"))
  cat(paste(Sys.time(), "- Wrote Buffer_GW_Delta_Summary.csv"), file = log_file, sep = "\n", append = TRUE)
}

if(!is.null(ward_deltas)){
  ward_summary <- compute_summary(ward_deltas, value_col = "delta_abs")
  readr::write_csv(ward_summary, file.path(summary_dir, "Ward_GW_Delta_Summary.csv"))
  cat(paste(Sys.time(), "- Wrote Ward_GW_Delta_Summary.csv"), file = log_file, sep = "\n", append = TRUE)
}

if(!is.null(lulc_deltas)){
  lulc_summary <- compute_summary(lulc_deltas, value_col = "delta_abs", group_cols = c("year1","year2","lulc"))
  readr::write_csv(lulc_summary, file.path(summary_dir, "Ward_LULC_GW_Delta_Summary.csv"))
  cat(paste(Sys.time(), "- Wrote Ward_LULC_GW_Delta_Summary.csv"), file = log_file, sep = "\n", append = TRUE)
}

#---Optional: overall trends---#

# For each spatial unit (buffer or ward), compute cumulative change across all delta pairs
compute_cumulative_trend <- function(df, id_col = "Identifier", delta_col = "delta_abs"){
  df %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      total_change = sum(.data[[delta_col]], na.rm = TRUE),
      mean_change = mean(.data[[delta_col]], na.rm = TRUE),
      n_deltas = sum(!is.na(.data[[delta_col]])),
      .groups = "drop"
    )
}

if(!is.null(buffer_deltas)){
  buffer_trend <- compute_cumulative_trend(buffer_deltas)
  readr::write_csv(buffer_trend, file.path(summary_dir, "Buffer_GW_Cumulative_Trend.csv"))
  cat(paste(Sys.time(), "- Wrote Buffer_GW_Cumulative_Trend.csv"), file = log_file, sep = "\n", append = TRUE)
}

if(!is.null(ward_deltas)){
  ward_trend <- compute_cumulative_trend(ward_deltas, id_col = "ward_code")
  readr::write_csv(ward_trend, file.path(summary_dir, "Ward_GW_Cumulative_Trend.csv"))
  cat(paste(Sys.time(), "- Wrote Ward_GW_Cumulative_Trend.csv"), file = log_file, sep = "\n", append = TRUE)
}

#---Finish---#

cat(paste(Sys.time(), "Finished 4.5.G — Summary Statistics"), file = log_file, sep = "\n")
message("4.5.G complete. Outputs in: ", summary_dir)

---------------------------------------------------------------------------------------------------------------------------

# LULC-GROUNDWATER ANAlYSIS - Section 4.6
#--Modelling pipeline: Spearman -> GAM -> Random Forest--#

# Setting up R and clearing workspace
rm(list = ls())

#---Packages---#

pkgs <- c("readr","dplyr","tidyr","ggplot2","corrplot","Hmisc","mgcv","ranger","caret","pdp","performance","blockCV","sf")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

#---Paths & parameters---#

base_dir <- "M:/GGLM/GGLM_GROUNDWATER"
model_csv <- file.path(base_dir, "Model_Input_4.6_final.csv")   # your merged CSV
out_dir <- file.path(base_dir, "Modelling_Outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Period response names in your CSV
response_vars <- c("gw_95_00_m", "gw_00_05_m", "gw_95_05_m")  # mean delta columns
# Predictor name patterns (adjust if your headers differ)
lulc_pct_pattern <- "^P_LULC_"           # 1995 class percentages
lulc00_pct_pattern <- "^LULC00_P_LULC_"  # 2000 class percentages
trans95_pattern <- "^TRANS95_P_TRANS_"   # 1995-2000 transition percentages
trans00_pattern <- "^TRANS00_P_TRANS_"   # 2000-2005 transition percentages

# Modelling options
min_rows_required <- 30    # warn / avoid modelling if fewer samples
use_spatial_cv <- TRUE     # attempt spatial blocking by ward (fallback to kfold)
n_folds <- 5
set.seed(1234)

#---Utilities---#

safe_read <- function(p){
  if(!file.exists(p)) stop("Model CSV not found: ", p)
  readr::read_csv(p, show_col_types = FALSE)
}

# Function to prune predictors: remove near-zero variance and high pairwise corr
prune_predictors <- function(df_predictors, var_thresh = 0.95, corr_thresh = 0.9){
  # remove near-zero variance
  nzv <- caret::nearZeroVar(df_predictors, saveMetrics = TRUE)
  keep1 <- rownames(nzv)[!nzv$nzv]
  df1 <- df_predictors[, keep1, drop = FALSE]
  # remove highly correlated predictors (caret::findCorrelation)
  cmat <- cor(df1, use = "pairwise.complete.obs")
  drop_idx <- caret::findCorrelation(cmat, cutoff = corr_thresh, verbose = FALSE)
  if(length(drop_idx)>0){
    keep2 <- setdiff(colnames(df1), colnames(df1)[drop_idx])
    df_final <- df1[, keep2, drop = FALSE]
  } else df_final <- df1
  return(list(df = df_final, dropped = setdiff(colnames(df_predictors), colnames(df_final))))
}

# Function to write diagnostic summary
write_diag <- function(msg){
  cat(paste0(Sys.time(), " - ", msg, "\n"))
}

#---Load data---#

df <- safe_read(model_csv)
write_diag(paste("Loaded rows:", nrow(df), "columns:", ncol(df)))

# Identify predictors by patterns
all_preds <- names(df)
preds_pct <- grep(paste(lulc_pct_pattern, lulc00_pct_pattern, trans95_pattern, trans00_pattern, sep="|"), all_preds, value = TRUE)
write_diag(paste("Found predictors:", length(preds_pct)))
if(length(preds_pct) == 0) stop("No predictors found by pattern. Check column names.")

# optionally add other predictors (e.g., DepthCategory encoded)
# create numeric encoding for DepthCategory if present
if("DepthCategory" %in% names(df)){
  df <- df %>% mutate(Depth_cat_num = as.integer(factor(DepthCategory, levels=c("Shallow","Intermediate","Deep"))))
  preds_pct <- c(preds_pct, "Depth_cat_num")
}

#---Loop over response periods---#

for(resp in response_vars){
  write_diag(paste("Starting modelling for response:", resp))
  if(!(resp %in% names(df))){
    write_diag(paste("Response", resp, "not found in CSV — skipping."))
    next
  }
  # prepare modelling table: keep response + predictors + ID + ward if present
  model_df <- df %>% dplyr::select(any_of(c("Identifier")), all_of(resp), any_of(preds_pct)) %>%
    dplyr::rename(response = !!resp)
  
  # drop rows with NA response
  model_df <- model_df %>% filter(!is.na(response))
  write_diag(paste("Available cases (non-missing response):", nrow(model_df)))
  if(nrow(model_df) < min_rows_required){
    write_diag(paste("WARNING: low sample size for", resp, "(", nrow(model_df), ") — results will be exploratory."))
  }
  
  # Predictor matrix
  pred_mat <- model_df %>% dplyr::select(all_of(preds_pct))
  # ensure numeric
  pred_mat[] <- lapply(pred_mat, function(x) as.numeric(as.character(x)))
  
  # prune predictors
  pr <- prune_predictors(pred_mat, corr_thresh = 0.90)
  pred_pruned <- pr$df
  dropped_preds <- pr$dropped
  write_diag(paste("Pruned predictors: kept", ncol(pred_pruned), "dropped", length(dropped_preds)))
  write_diag(paste("Dropped:", paste(dropped_preds, collapse=", ")))
  
  # Build final modelling table
  mod_table <- bind_cols(model_df %>% dplyr::select(any_of(c("Identifier","response"))),
                         pred_pruned)
  
  # Standardise predictors for GAM (center & scale)
  preds_names <- colnames(pred_pruned)
  mod_table_std <- mod_table
  mod_table_std[preds_names] <- scale(mod_table_std[preds_names])
  
  # Prepare training set: remove NAs in predictors
  rf_df <- mod_table_std %>% dplyr::select(-Identifier)
  if("ward_name" %in% names(rf_df)) {
    rf_df <- rf_df %>% dplyr::select(-ward_name)
  }
  # drop rows with any NA in predictors
  rf_df <- rf_df[complete.cases(rf_df), ]
  
  
  #---(A) Exploratory Spearman Correlations---#

  cor_list <- Hmisc::rcorr(as.matrix(mod_table_std[, c("response", preds_names)]), type="spearman")
  cor_r <- cor_list$r
  cor_p <- cor_list$P
  write.csv(as.data.frame(cor_r["response", preds_names]), file = file.path(out_dir, paste0("Spearman_r_", resp, ".csv")), row.names = TRUE)
  write.csv(as.data.frame(cor_p["response", preds_names]), file = file.path(out_dir, paste0("Spearman_p_", resp, ".csv")), row.names = TRUE)
  
  # Simple heatmap for top correlations
  spearmans <- data.frame(variable = preds_names,
                          rho = cor_r["response", preds_names],
                          pval = cor_p["response", preds_names]) %>%
    arrange(desc(abs(rho)))
  write.csv(spearmans, file = file.path(out_dir, paste0("Spearman_ranked_", resp, ".csv")), row.names = FALSE)
  
  # Plot top 20 by absolute correlation
  topk <- min(20, nrow(spearmans))
  topvars <- spearmans$variable[1:topk]
  # save scatterplots for top 6
  for(v in head(topvars, 6)){
    p <- ggplot(mod_table_std, aes_string(x = v, y = "response")) +
      geom_point(alpha=0.6) + geom_smooth(method="loess", se=TRUE) +
      labs(title = paste0("Response vs ", v, " (Spearman rho=", round(cor_r["response", v],3), ")"))
    ggsave(filename = file.path(out_dir, paste0("Scatter_", resp, "_", v, ".png")), plot = p, width = 6, height = 4, dpi = 300)
  }
  
  #---(B) GAM (mgcv) — parsimonious: use top predictors by Spearman (top 6-10)---#

  # choose predictors for GAM
  gam_k <- min(10, ncol(pred_pruned))          # max smooth terms
  gam_vars <- spearmans$variable[1:gam_k]
  gam_formula <- as.formula(paste("response ~", paste0("s(", gam_vars, ", k=5)", collapse = " + ")))
  write_diag(paste("GAM formula:", deparse(gam_formula)))
  
  gam_mod <- tryCatch({
    mgcv::gam(gam_formula, data = mod_table_std, method="REML")
  }, error = function(e){
    mgcv::gam(as.formula(paste("response ~", paste0("s(", gam_vars, ")", collapse = " + "))), data = mod_table_std, method="REML")
  })
  saveRDS(gam_mod, file = file.path(out_dir, paste0("GAM_model_", resp, ".rds")))
  capture.output(summary(gam_mod), file = file.path(out_dir, paste0("GAM_summary_", resp, ".txt")))
  # concurvity check
  capture.output(mgcv::concurvity(gam_mod, full = TRUE), file = file.path(out_dir, paste0("GAM_concurvity_", resp, ".txt")))
  # plot smooths
  png(file.path(out_dir, paste0("GAM_smooths_", resp, ".png")), width = 1200, height = 800)
  plot(gam_mod, pages=1, rug=TRUE, shade=TRUE)
  dev.off()
  
  # predictions & diagnostics
  mod_table_std$gam_pred <- predict(gam_mod, newdata = mod_table_std, type = "response")
  gam_resid <- mod_table_std$response - mod_table_std$gam_pred
  write.csv(mod_table_std %>% dplyr::select(Identifier, response, gam_pred), file = file.path(out_dir, paste0("GAM_predictions_", resp, ".csv")), row.names = FALSE)
  # RMSE
  gam_rmse <- sqrt(mean(gam_resid^2, na.rm=TRUE))
  write_diag(paste("GAM RMSE for", resp, "=", round(gam_rmse,4)))
  write.csv(data.frame(metric="RMSE", value=gam_rmse), file=file.path(out_dir, paste0("GAM_RMSE_", resp, ".csv")), row.names = FALSE)
  
  #---(C) Random Forest (ranger) with spatial CV if possible---#

  # Prepare training set: remove NAs in predictors
  rf_df <- mod_table_std %>% dplyr::select(-Identifier)  # keep ward_code maybe
  # drop rows with any NA in predictors
  rf_df <- rf_df[complete.cases(rf_df), ]
  write_diag(paste("RF: usable rows =", nrow(rf_df)))
  if(nrow(rf_df) < 10){
    write_diag("Too few rows after NA removal for RF; skipping.")
  } else {
    # define formula
    rf_formula <- as.formula(paste("response ~", paste(colnames(pred_pruned), collapse = " + ")))
    # Spatial CV attempt using blockCV if ward_code present
    train_control <- NULL
    if(use_spatial_cv && "ward_code" %in% names(mod_table) && !all(is.na(mod_table$ward_code))){
      write_diag("Using ward_code for fold assignment (spatial-ish CV).")
      # make folds by ward_code (stratify if needed)
      folds <- caret::groupKFold(mod_table$ward_code, k = n_folds)
      # caret's train uses list of indices; but we will do manual loop for ranger
      # Function to compute RMSE for fold
      fold_rmse <- c()
      oob_rmse <- NA
      # tune mtry: try sqrt(p) and p/3
      p <- ncol(pred_pruned)
      try_mtrys <- unique(c(floor(sqrt(p)), floor(p/3), max(1, floor(p/2))))
      rf_best <- NULL; rf_best_mtry <- NULL; rf_best_rmse <- Inf
      for(mtr in try_mtrys){
        fold_preds <- c()
        fold_obs <- c()
        for(i in seq_along(folds)){
          tr_idx <- unlist(folds[-i])
          te_idx <- unlist(folds[i])
          rf_tr <- rf_df[tr_idx, ]
          rf_te <- rf_df[te_idx, ]
          rf_mod <- ranger::ranger(rf_formula, data = rf_tr, num.trees = 1000, mtry = mtr, importance = "permutation", write.forest = TRUE, seed=123)
          pred_te <- predict(rf_mod, data = rf_te)$predictions
          fold_preds <- c(fold_preds, pred_te)
          fold_obs <- c(fold_obs, rf_te$response)
        }
        rmse_m <- sqrt(mean((fold_obs - fold_preds)^2, na.rm = TRUE))
        write_diag(paste("mtry", mtr, "cv-RMSE=", round(rmse_m,4)))
        if(rmse_m < rf_best_rmse){ rf_best_rmse <- rmse_m; rf_best_mtry <- mtr }
      }
      # final model on all data
      final_rf <- ranger::ranger(rf_formula, data = rf_df, num.trees = 2000, mtry = rf_best_mtry, importance = "permutation", seed=123)
      # variable importance
      vi <- sort(importance(final_rf), decreasing = TRUE)
      write.csv(as.data.frame(vi), file = file.path(out_dir, paste0("RF_varimp_", resp, ".csv")), row.names = TRUE)
      saveRDS(final_rf, file = file.path(out_dir, paste0("RF_model_", resp, ".rds")))
      write_diag(paste("Final RF mtry:", rf_best_mtry, "CV-RMSE:", round(rf_best_rmse,4)))
      # Partial dependence for top 6 variables
      topv <- names(vi)[1:min(6, length(vi))]
      for(tv in topv){
        pd <- pdp::partial(final_rf, pred.var = tv, train = rf_df, which.class = NULL, progress = "none")
        p_pd <- autoplot(pd) + ggtitle(paste("PDP:", tv, "for", resp))
        ggsave(file.path(out_dir, paste0("RF_PDP_", resp, "_", tv, ".png")), plot = p_pd, width = 6, height = 4, dpi = 300)
      }
      # OOB RMSE
      if(!is.null(final_rf$prediction.error)) oob_rmse <- sqrt(final_rf$prediction.error)
      write.csv(data.frame(metric="CV_RMSE", value=rf_best_rmse, oob_RMSE=oob_rmse), file=file.path(out_dir, paste0("RF_metrics_", resp, ".csv")), row.names=FALSE)
    } else {
      # non-spatial k-fold via caret
      write_diag("Using non-spatial k-fold CV (caret).")
      train_ctrl <- trainControl(method = "cv", number = n_folds)
      # caret wrapper with ranger
      caret_grid <- expand.grid(
        mtry = floor(c(sqrt(ncol(pred_pruned)), ncol(pred_pruned)/3)),
        splitrule = "variance",      # regression RF
        min.node.size = 5
      )
      caret_mod <- train(rf_formula, data = rf_df, method = "ranger",
                         trControl = train_ctrl, tuneGrid = caret_grid,
                         num.trees = 1000, importance = "permutation", verbose = FALSE)
      saveRDS(caret_mod, file = file.path(out_dir, paste0("RF_caret_model_", resp, ".rds")))
      vi <- varImp(caret_mod)$importance
      write.csv(vi, file = file.path(out_dir, paste0("RF_varimp_caret_", resp, ".csv")), row.names = TRUE)
      write_diag(paste("Caret RF best:", paste(caret_mod$bestTune, collapse=",")))
    }
  } # end rf block
  
  write_diag(paste("Finished modelling for:", resp))
} # end resp loop

write_diag("All modelling finished. Outputs in: ")
print(out_dir)


