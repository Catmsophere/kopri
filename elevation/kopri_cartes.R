library(readr); library(gstat)
library(dplyr); library(ggplot2)
library(sf); library(terra)
library(tidyterra); library(data.table)
library(stars); library(automap)
library(mapview); library(leaflet)
library(jsonlite)

setwd("C:/Users/mathy/Downloads/Données")

########################################################################################################
###############################       DONNEES DEPARTEMENT
########################################################################################################

departement <- st_read("C:/Users/mathy/Downloads/Données/DEPARTEMENT/DEPARTEMENT.shp")
departement <- st_transform(departement, crs = 4326)


########################################################################################################
###############################       DONNEES TOPO
########################################################################################################
dep <- "001"

get_topo <- function(dep_no, dep_na, departements){
  asc_files <- list.files(paste0("C:/Users/mathy/Downloads/Données/Topographiques/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D",dep_no,"/BDALTIV2/1_DONNEES_LIVRAISON/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D",dep_no,"/"), pattern = "\\.asc$", full.names = TRUE)
  
  
  # Read all .asc files as rasters
  rasters <- lapply(asc_files, function(file) {
    r <- rast(file)
    crs(r) <- "EPSG:2154"  # Set the input CRS (Lambert 93)
    return(r)
  })
  
  merged_raster <- do.call(mosaic, c(rasters, list(fun = mean)))  # Use mean for overlapping pixels
  # Reproject to WGS 84
  reprojected_raster <- project(merged_raster, "EPSG:4326")
  
  # Crop to the bounding box of the polygon (optional but speeds up processing)
  r_crop <- crop(reprojected_raster, departements[departements$NOM_DEPT  == dep_na,])
  # Mask to keep only values inside the polygon
  r_masked <- mask(r_crop, vect(departements[departements$NOM_DEPT  == dep_na,]))
  return(r_masked)
}

ain <- get_topo("001","AIN", departement)

# Convert SpatRaster to SpatVector (polygonizing)
vec <- as.polygons(r, dissolve = FALSE)  # dissolve = TRUE to merge similar values

# Convert SpatVector to sf
sf_obj <- st_as_sf(vec)

# View the sf object
print(sf_obj)
plot(sf_obj)


#ggplot() + geom_spatraster(data = r_masked)  + 
#  scale_fill_gradientn(colors = terrain.colors(20), na.value = NA) + 
#  geom_contour(r_masked, add = T, col = "black") + 
#  geom_sf(data = departement[departement$NOM_DEPT == dep_na,], fill = NA, col = "red") +
#  theme_classic()




############# CATMOSPHERE
#####
format_catmosphere(get_topo("071","SAONE-ET-LOIRE", departement),"topo_71")

format_catmosphere <- function(rast,filename){
  #r <- project(rast, "EPSG:4326") 
  
  target_crs <- "EPSG:32633"
  r <- project(rast, target_crs)
  r <- round(r / 10) * 10
  r <- aggregate(r, fact = c(10, 10), fun = "mean", expand = TRUE)
  r <- round(r / 10) * 10
  
  r <- project(r, "EPSG:4326")
  
  # Get bounding box coordinates
  ext <- ext(r)
  bbox <- matrix(c(ext[1], ext[3], ext[2], ext[4]), nrow = 2, byrow = TRUE)
  
  # Print bounding box for Leaflet
  print(bbox)
  
  # Get the min and max values of the raster
  r_min <- minmax(r)[1]
  r_max <- minmax(r)[2]
  
  # Define a monochrome color palette (for example, light to dark gray)
  pal <- colorRampPalette(c("gray90", "gray1"))
  
  # Create and save the PNG image
  png("topography_overlay.png", bg = "transparent")
  # Remove axes, legends and borders by using a simple plot
  par(mar = c(0,0,0,0), xaxs="i", yaxs="i") 
  plot(r,
       col = pal(100),
       legend = FALSE,
       axes = FALSE,
       box = FALSE)
  dev.off()
  
  altitude_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  write.csv(altitude_df, "aggregated_altitude_data.csv", row.names = FALSE)
  
  json_data <- toJSON(r)
  write(json_data, file = "aggregated_altitude_data.json")
  
  return(0)
}

ain_plot <- format_catmosphere(ain,"AIN")

breaks <- seq(0, 1500, by = 50)
col <- gray.colors(length(breaks), start = 0.1, end = 1)


# Generate one PNG per range: 0–50, 51–100, 101–150, etc.
for (i in seq_along(breaks)[-length(breaks)]) {
  lower <- breaks[i]
  upper <- breaks[i + 1] - 1
  
  # Mask values outside the range
  ain_sub <- ain
  vals <- values(ain)
  vals[vals < lower | vals > upper] <- NA
  values(ain_sub) <- vals
  
  png_name <- paste0("AIN_range_", lower, "_", upper, ".png")
  
  # Get bounding box coordinates
  ext <- ext(ain_sub)
  bounds <- list(
    c(ext[3], ext[1]),  # ymin, xmin
    c(ext[4], ext[2])   # ymax, xmax
  )
  # Print bounding box for Leaflet
  print(lower)
  print("\n")
  print(bbox)
  
  png(png_name, bg = "transparent")
  plot(ain_sub,
       col = col[i],  # White = low, black = high
       legend = FALSE, axes = FALSE, box = FALSE)
  dev.off()
}



#####
# Create a ggplot map
p <- ggplot() +
  geom_sf(data = points_sf, color = "red", size = 3) +
  theme_minimal()

# Save the plot as a PNG file
ggsave("sf_plot.png", plot = p, width = 6, height = 4, dpi = 300)




########################################################################################################
###############################       DONNEES METEO
########################################################################################################

#Lire facilement les données 

decad_data_69 <- fread("C:/Users/mathy/Downloads/Données/Météo/DECADQ_69_latest-2024-2025.csv.gz")
decad_data_01 <- fread("C:/Users/mathy/Downloads/Données/Météo/DECADQ_01_latest-2024-2025.csv.gz")
decad_data_71 <- fread("C:/Users/mathy/Downloads/Données/Météo/DECADQ_71_latest-2024-2025.csv.gz")

decad_data <- rbind(decad_data_01,
                    decad_data_69,
                    decad_data_71)
decad_data$NUM_DEP <- floor(decad_data$NUM_POSTE/1e6)

ggplot(departement[departement$NOM_DEPT %in% c("RHONE","AIN","SAONE-ET-LOIRE"),]) + geom_sf(fill = "grey99") +
  geom_point(data = decad_data[decad_data$AAAAMM == 202401 & decad_data$NUM_DECADE == 1,],aes(x = LON, y = LAT, col = TX)) + 
  theme_classic()


#Interpolation par interpolation linéaire

crs <- st_crs("EPSG:32632")

departement_interp <- st_transform(departement[departement$NOM_DEPT %in% c("RHONE","AIN","SAONE-ET-LOIRE"),], crs = 32632)
st_bbox(departement_interp) |>
  st_as_stars(dx = 3000) |>
  st_crop(departement_interp) -> grd

st_as_sf(decad_data[decad_data$AAAAMM == 202401 & decad_data$NUM_DECADE == 1,], crs = 4326, coords = 
           c("LON", "LAT")) |>
  st_transform(crs) -> dep.sf

i <- idw(TX~1, dep.sf, grd)

ggplot() + geom_stars(data = i, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(departement_interp, "MULTILINESTRING")) + 
  geom_sf(data = dep.sf)


#Ajout de l'altitude - restreindre à l'Ain
departement_interp <- st_transform(departement[departement$NOM_DEPT %in% c("AIN"),], crs = 32632)
st_bbox(departement_interp) |>
  st_as_stars(dx = 3000) |>
  st_crop(departement_interp) -> grd

st_as_sf(decad_data[decad_data$AAAAMM == 202401 & decad_data$NUM_DECADE == 1 & decad_data$NUM_DEP == 1,], crs = 4326, coords = 
           c("LON", "LAT")) |>
  st_transform(crs) -> dep.sf


grid <- st_as_sf(grd)
grid_vect <- vect(grid)
grid$altitude <- terra::extract(project(ain,"EPSG:32632"), grid_vect, fun = mean, na.rm = TRUE)[,2]


# Convert temperature points to SpatVector
temperature_points_vect <- vect(dep.sf)

# Extract altitude values at each temperature point
dep.sf$altitude <- terra::extract(project(ain,"EPSG:32632"), temperature_points_vect)[,2]

v = autofitVariogram(TX ~ altitude, dep.sf)
g = gstat(formula = TX ~ altitude, model = v$var_model, data = dep.sf)

# Predict temperature over the grid
interpolated <- predict(g, grid)

ggplot() +
  geom_sf(data = interpolated, aes(fill = var1.pred), color = NA) +
  scale_fill_viridis_c() +
  ggtitle("Interpolated Temperature with Altitude Consideration") +
  theme_minimal()

########################################################################################################
###############################       DONNEES GEOLOGIQUES
########################################################################################################

setwd("C:/Users/mathy/Downloads/Données/Géologiques/GEO050K_HARM_001")

geol <- st_read("GEO050K_HARM_001_S_FGEOL_2154.shp")
geol <- st_transform(geol, crs = 4326)


ggplot(departement[departement$NOM_DEPT %in% c("AIN"),]) + geom_sf(fill = "grey99") +
  geom_sf(geol[2228,c("NOTATION")], mapping = aes(fill = NOTATION),show.legend = FALSE) + 
  theme(legend.position = "none") +
  theme_classic()

geol_bis <- geol %>%
  st_transform(., 3857) %>%  # Convert to meters-based CRS (Web Mercator)
  mutate(area_m2 = st_area(geometry))

geol_ter <- geol %>%
  st_transform(., 3035) # Convert to meters-based CRS (Web Mercator)

plot(geol_bis)
which.max(geol_bis$area_m2)



geol <- st_read("GEO050K_HARM_001_S_SURCH_2154.shp")
geol <- st_transform(geol, crs = 4326)

unique(geol$DESCR)


ggplot(departement[departement$NOM_DEPT %in% c("AIN"),]) + geom_sf(fill = "grey99") +
  geom_sf(geol[,c("NOTATION")], mapping = aes(fill = NOTATION),show.legend = FALSE) + 
  theme(legend.position = "none") +
  theme_classic()



########################################################################################################
###############################       INFERENCE BAYESIENNE
########################################################################################################

library(INLA); library(spdep); library(exactextractr)


#Geology AIN: geol (geol$NOTATION)
#Topography AIN: ain
#Where fossils are: geol_fossile

departement_interp <- st_transform(departement[departement$NOM_DEPT %in% c("AIN"),], crs = 3035)
st_bbox(departement_interp) |>
  st_as_stars(dx = 1000) |>
  st_crop(departement_interp) -> grd

grid <- st_as_sf(grd)
grid_vect <- vect(grid)
grid_support <- st_make_grid(grid, cellsize = 1000, square = TRUE)  # 1km x 1km squares
grid_sf <- st_transform(st_sf(geometry = grid_support), crs = 3035)

grid_inter <- st_intersection(grid_sf, grid)
plot(grid_inter[,"altitude"])

library(raster)
library(spdep)

# Create a blank raster aligned with the grid (1km resolution)
grid_raster <- raster(extent(grid_inter), res=1000, crs=3035)

# Rasterize the geological polygons (assigning a unique ID per geology type)
geol <- st_transform(geol, crs = 3035)
geol$NOTATION <- as.numeric(as.factor(geol$NOTATION))
geology_raster <- rasterize(geol, grid_raster, field="NOTATION", fun="modal")
grid_sf$geology_type <- exact_extract(geology_raster, grid_sf, "mode")

geology_levels <- levels(as.factor(geol$NOTATION))
grid_sf$geology_type <- factor(geology_levels[grid_sf$geology_type], ordered = F)
grid_sf$grid_id <- 1:nrow(grid_sf)

grid_sf$altitude <- terra::extract(project(ain,"EPSG:3035"), grid_sf, fun = mean, na.rm = TRUE)[,2]

# Load or create spatial grid (assuming 'grid' is an sf object with topography data)
nb <- poly2nb(grid_sf)  # Neighborhood structure
mat <- nb2mat(nb, style="B", zero.policy=TRUE)  # Adjacency matrix

fossils_sf <- st_transform(geol_fossile, 3035)
fossils_sf$grid_id <- st_within(fossils_sf, grid_sf) %>% unlist()
grid_sf$fossil_presence <- as.numeric(grid_sf$grid_id %in% fossils_sf$grid_id)

plot(na.omit(grid_sf))
plot(nb, st_coordinates(st_centroid(grid_sf[,"altitude"])), add = T)

formula <- fossil_presence ~ altitude + f(geology_type, model="iid") + 
  f(grid_id, model="besag", graph=mat)

# Fit the INLA model
result <- inla(formula, data=na.omit(grid_sf), family="binomial", 
               control.compute=list(dic=TRUE, waic=TRUE), verbose=TRUE, debug=TRUE)

result <- inla(fossil_presence ~ 1, data=grid_sf, family="binomial", verbose=TRUE, debug=TRUE)
result
# Check results
summary(result)

grid_clean <- na.omit(grid_sf)

# Get the fitted values (posterior means for the linear predictor)
grid_clean$posterior_mean <- result$summary.fitted.values$mean

# Optional: get the posterior SD or quantiles
grid_clean$posterior_sd <- result$summary.fitted.values$sd
plot(grid_clean[,"posterior_mean"])

posterior_plot <- ggplot(grid_clean) +
  geom_sf(aes(fill = posterior_mean), color = NA, alpha = 0.5) +
  scale_fill_viridis_c(option = "D", name = "Posterior Mean") +
  theme_void() +  # Removes axes, ticks, etc.
  theme(legend.position = "none")  # Hide legend (optional)

ggsave("C:/Users/mathy/Downloads/Données/posterior_map.png",
       plot = posterior_plot,
       width = 6, height = 6, units = "in", dpi = 300, bg = NA)


posterior_plot <- ggplot(grid_clean) +
  geom_sf(aes(fill = fossil_presence), color = NA, alpha = 0.5) +
  scale_fill_viridis_c(option = "D", name = "Posterior Mean") +
  theme_void() +  # Removes axes, ticks, etc.
  theme(legend.position = "none")  # Hide legend (optional)

ggsave("C:/Users/mathy/Downloads/Données/fossile_presence.png",
       plot = posterior_plot,
       width = 6, height = 6, units = "in", dpi = 300, bg = NA)

########################################################################################################
###############################       CARTES FOSSILE
########################################################################################################


setwd("C:/Users/mathy/Downloads/Données/Géologiques/")
i <- "001"
geol_fossile <- NULL
for(i in c("001", "070", "071", "002", "004")){
  geol <- st_read(paste0("GEO050K_HARM_",i,"/GEO050K_HARM_",i,"_P_DIVERS_2154.shp"))
  geol <- st_transform(geol, crs = 4326)
  geol_fossile <- rbind(geol_fossile, geol[grepl("fossil",geol$DESCR),])
}



ggplot(departement[departement$NOM_DEPT %in% c("AIN"),]) + geom_sf(fill = "grey99") +
  geom_sf(geol_fossile[,c("DESCR")], mapping = aes(col = DESCR),show.legend = T) + 
  theme(legend.position = "none") +
  theme_classic()

# Create a ggplot map
p <- ggplot() +
  geom_sf(data = geol_fossile, color = "red", size = 0.8) +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )
p
# Save the plot as a PNG file
ggsave("C:/Users/mathy/Downloads/Données/fossiles_001.png", plot = p, width = 6, height = 4, dpi = 300, bg = "transparent")


generate_leaflet_txt <- function(sf_file, output_txt = "leaflet_script.txt", read_sf = F) {
  # Read the sf file
  if(read_sf == T){
    sf_data <- st_read(sf_file, quiet = TRUE)
  }
  else sf_data <- sf_file
  # Extract coordinates
  coords <- st_coordinates(sf_data)
  sf_data <- sf_data %>% mutate(lon = coords[,1], lat = coords[,2])
  
  # Generate JavaScript markers
  js_markers <- paste0(
    "L.circleMarker([", sf_data$lat, ", ", sf_data$lon, "], {\n",
    "    radius: 8,\n",
    "    color: 'red',\n",
    "    fillColor: 'red',\n",
    "    fillOpacity: 1\n",
    "}).addTo(map);\n"
  )
  
  # Create full JavaScript script
  js_code <- paste0(
    "<script>\n",
    "var map = L.map('map').setView([", mean(sf_data$lat), ", ", mean(sf_data$lon), "], 13);\n",
    "L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {\n",
    "    attribution: '&copy; OpenStreetMap contributors'\n",
    "}).addTo(map);\n\n",
    paste(js_markers, collapse = "\n"),
    "\n</script>"
  )
  
  # Write to a text file
  writeLines(js_code, output_txt)
  message("JavaScript code saved to: ", output_txt)
}

# Example Usage:
generate_leaflet_txt(geol_fossile, "C:/Users/mathy/Downloads/Données/map_script.txt")


