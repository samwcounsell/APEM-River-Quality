library(sf)
library(hetoolkit)
library(dplyr)

#### Reading in the data ####

## Wards ##
# Custom area of wards pulled from government website
soton_wards <- read_sf("data/soton_wards.geojson")

## Site Data ##
# Reading in APEM site data and bouding for Southampton area (crs = 27700)
# import_env(save_dwnld = TRUE)

sites <- read.csv("data/apem_sites.csv")
soton_sites <- subset(sites, FULL_EASTING > 439000 & FULL_EASTING < 451000 & FULL_NORTHING > 100000 & FULL_NORTHING < 130000)

## Bio Data ##
# Bulk download of EDE data for all sites in parquet format and save as .rds file for future use:
# import_inv(save_dwnld = TRUE)

# Get list of sites in area (soton_sites) to import data from only those sites
required_sites <- unique(soton_sites$SITE_ID)

# Read in local .rds file and filter on selected sites and dates (up to the present day):
bio_x <- import_inv(source = "data/INV_OPEN_DATA_METRICS_ALL.rds",
                    sites = required_sites,
                    start_date = "1995-01-01",
                    end_date = Sys.Date())

## Rivers ##
# Read in rivers data, source: OS Data Portal UK Rivers
rivers <- st_read("data/WatercourseLink.shp")


#### Transforming the data ####

## Sites and Wards ##

# Converting points and wards to sf_list to then apply the intersect function
sites_sf <- do.call("st_sfc",c(lapply(1:nrow(soton_sites), 
                                      function(i) {st_point(as.numeric(soton_sites[i, c("FULL_EASTING", "FULL_NORTHING")]))}), list("crs" = 27700))) 

# Setting crs = 4326 (lat lon)
sites_sf <- st_transform(sites_sf, crs = 4326)
wards_sf <- st_transform(soton_wards, crs = 4326)

# Mapping sites to wards and adding WD24CD column to sites df
soton_sites$WD24CD <- apply(st_intersects(wards_sf, sites_sf, sparse = FALSE), 2, 
                            function(col) { 
                              if (any(col)) {
                                wards_sf[which(col), ]$WD24CD
                              } else {
                                NA
                              }
                            })

# Count number of instances of each ward code in points$WD22CD, convert to dataframe and rename columns
ward_count <- as.data.frame(table(soton_sites$WD24CD))
colnames(ward_count) <- c("WD24CD", "Count")

# Merge the ward_count with the wards data frame
soton_wards <- merge(soton_wards, ward_count, by = "WD24CD", all.x = TRUE)
# Replace NA values in the Count column with 0
soton_wards$Count[is.na(soton_wards$Count)] <- 0

## Bio Data ##

# Merge the site data with the bio data (i.e. co-ordinate data)
bio_df <- merge(bio_x, soton_sites, by.x = "biol_site_id", by.y = "SITE_ID", all.x = TRUE)


# Converting points and wards to sf_list to then apply the intersect function, and setting coord system
bio_sf <- do.call("st_sfc",c(lapply(1:nrow(bio_df), 
                                   function(i) {st_point(as.numeric(bio_df[i, c("FULL_EASTING", "FULL_NORTHING")]))}), list("crs" = 27700))) 

# Setting crs = 4326 (lat lon)
bio_sf <- st_transform(bio_sf, crs = 4326)

# Adding back as column to the dataframe
bio_df$geometry <- bio_sf

## River Data ##

# Filter for the Itchen River and transform to lat lon
itchen_river <- dplyr::filter(rivers, grepl("Itchen", name1))
itchen_river <- st_transform(itchen_river, crs = 4326)

# There are 2 Itchens in the UK, remove the incorrect one by removing any linestrings that have a lat co-ordinate above 51.5
lat_filter <- function(geometry){
  # Convert geometry column to X (lon), Y (lat) and other co-ords
  coords <- st_coordinates(geometry)
  # Return TRUE  if max lat <= 51.5
  max(coords[, "Y"]) <= 51.5
}

# Apply the function to each row in df and keep only where TRUE returned
itchen_river <- itchen_river %>%
  filter(sapply(geometry, lat_filter))

# Get all rivers in an area instead of just the Itchen
river_coord_filter <- function(geometry) {
  # Convert geometry column to X (lon), Y (lat) and other co-ords
  coords <- st_coordinates(geometry)
  
  # Check if the coordinates meet the criteria
  min_lat_check <- min(coords[, "Y"]) >= 100000
  max_lat_check <- max(coords[, "Y"]) <= 130000
  min_lon_check <- min(coords[, "X"]) >= 439000
  max_lon_check <- max(coords[, "X"]) <= 451000
  
  # Return TRUE if all conditions are met
  min_lat_check && max_lat_check && min_lon_check && max_lon_check
}

soton_rivers <- rivers %>%
  filter(sapply(geometry, river_coord_filter))

soton_rivers <- st_transform(soton_rivers, crs = 4326)


#### Plots ####

## Base Plot ##
base_plot <- ggplot() + 
  geom_sf(data = soton_wards, color = "black", fill = "snow", alpha = 1) + 
  theme_minimal()

## Ward Plots ##

# Sites in Wards Plot
ward_plot <- ggplot() + 
  geom_sf(data = soton_wards, aes(fill = Count), color = "black", alpha = 1) + 
  theme_minimal()

# Plot points on  top of wards and show
ward_plot_2 <- ward_plot + geom_sf(data = sites_sf, color = "white")

## River Plots ##

# Itchen River only
itchen_river_plot <- base_plot + geom_sf(data = itchen_river, color = "steelblue1", lwd = 1)

# All Rivers
river_plot <- base_plot + geom_sf(data = soton_rivers, color = "steelblue1", lwd = 1)

## Bio Data Plots ##

# Bio Data only
bio_plot <- base_plot + geom_sf(data = bio_df, aes(geometry = geometry, color = LIFE_SCORES_TOTAL), size = 3)

bio_plot_2 <- river_plot + geom_sf(data = bio_df, aes(geometry = geometry, color = LIFE_SCORES_TOTAL), size = 3)


## My Personal Favourite ##

best_plot <- bio_plot_2 + labs(
  color = "Total Life Score",
  title = "Map of Southampton Wards, Rivers and Total Life Scores at Sampling Sites",
  subtitle = "Data Sources: OS Data Portal, APEM, Environment Agency, GOV.uk",
  x = "Latitude",
  y = "Longitude"
)

best_plot

## Misc Plots ##

# Density Plots
density_plot <- ggplot(bio_df, aes(LIFE_SCORES_TOTAL, color = WATER_BODY)) + geom_density()

density_plot_2 <- ggplot(bio_df, aes(LIFE_SCORES_TOTAL, color = WD24CD)) + geom_density()

# Box Plots
box_plot <- ggplot(bio_df, aes(LIFE_SCORES_TOTAL, WATER_BODY)) + geom_boxplot()

box_plot_2 <- ggplot(bio_df, aes(LIFE_SCORES_TOTAL, WD24CD)) + geom_boxplot()

