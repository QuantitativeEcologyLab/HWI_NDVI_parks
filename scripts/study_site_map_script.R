# loading packages
library(geodata) # for downloading provinces 
library(terra) #shape file 
library(sf) # spatial data
library(sp) #Spatial Points function
library(ggplot2) # for scatter plot
library(tidyterra) #geomspatraster

# new map ----

# import coordinates of all national parks, coordinates obtained from Google Maps
park_coordinates <- read.csv("data/park_coordinates.csv")

# remove the dropped parks x5
parks_to_drop <- c("Grasslands National Park of Canada", "Bruce Peninsula National Park of Canada", "Prince of Wales Fort National Historic Site of Canada", "Saoy\\xfa-?ehdacho National Historic Site of Canada", "Sable Island National Park Reserve", "Fort Walsh National Historic Site of Canada")

new_park_coordinates <- subset(park_coordinates, !(park %in% parks_to_drop))

# match coordinates name to park ID
new_park_coordinates$park[new_park_coordinates$park == "Banff National Park of Canada"]<- "BANF"
new_park_coordinates$park[new_park_coordinates$park == "Pacific Rim National Park Reserve of Canada"]<- "PRIM"
new_park_coordinates$park[new_park_coordinates$park == "Waterton Lakes National Park of Canada"]<- "WATE"
new_park_coordinates$park[new_park_coordinates$park == "Kejimkujik National Park and National Historic Site of Canada"]<- "KEJI"
new_park_coordinates$park[new_park_coordinates$park == "Jasper National Park of Canada"]<- "JASP"
new_park_coordinates$park[new_park_coordinates$park == "Forillon National Park of Canada"]<- "FORI"
new_park_coordinates$park[new_park_coordinates$park == "Prince Albert National Park of Canada"]<- "PALB"
new_park_coordinates$park[new_park_coordinates$park == "Kootenay National Park of Canada"]<- "KOOT"
new_park_coordinates$park[new_park_coordinates$park == "Glacier National Park of Canada"]<- "GLAC"
new_park_coordinates$park[new_park_coordinates$park == "Wapusk National Park of Canada"]<- "WAPU"
new_park_coordinates$park[new_park_coordinates$park == "Yoho National Park of Canada"]<- "YOHO"
new_park_coordinates$park[new_park_coordinates$park == "Terra Nova National Park of Canada"]<- "NOVA"
new_park_coordinates$park[new_park_coordinates$park == "Mount Revelstoke National Park of Canada"]<- "REVE"
new_park_coordinates$park[new_park_coordinates$park == "Elk Island National Park of Canada"]<- "ELKI"
new_park_coordinates$park[new_park_coordinates$park == "Georgian Bay Islands National Park of Canada"]<- "GBIS"
new_park_coordinates$park[new_park_coordinates$park == "Point Pelee National Park of Canada"]<- "PELE"
new_park_coordinates$park[new_park_coordinates$park == "Thousand Islands National Park of Canada"]<- "THIS"
new_park_coordinates$park[new_park_coordinates$park == "Wood Buffalo National Park of Canada"]<- "WOOD"
new_park_coordinates$park[new_park_coordinates$park == "Prince Edward Island National Park of Canada"]<- "PEIS"
new_park_coordinates$park[new_park_coordinates$park == "Ivvavik National Park of Canada"]<- "IVVA"
new_park_coordinates$park[new_park_coordinates$park == "Kouchibouguac National Park of Canada"]<- "KOUC"
new_park_coordinates$park[new_park_coordinates$park == "Fundy National Park of Canada"]<- "FUND"
new_park_coordinates$park[new_park_coordinates$park == "Nahanni National Park Reserve of Canada"]<- "NAHA"
new_park_coordinates$park[new_park_coordinates$park == "Aulavik National Park of Canada"]<- "AULA"
new_park_coordinates$park[new_park_coordinates$park == "Fathom Five National Marine Park of Canada"]<- "FIVE"


# convert coordinates into spatial data
#park_location <- SpatialPoints(select(park_coordinates, longitude, latitude))
new_park_location <- SpatialPoints(new_park_coordinates[, c("longitude", "latitude")])

parks_sf <- st_as_sf(new_park_coordinates, coords = c("longitude", "latitude"), crs = 4326)

#define the crs
esri_102001 <- st_crs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# reproject parks coordinate
parks_esri <- st_transform(parks_sf, crs = esri_102001)

#convert back to dataframe 
parks_esri_df <- st_drop_geometry

#convert to coordinates
coordinates <- st_coordinates(parks_esri)

# combine reprojected esri coordinates into original coordinates df
park_coordinates_esri <- cbind(coordinates, new_park_coordinates)

saveRDS(park_coordinates_esri,file ="data/park_coordinates_esri.rds")

park_coordinates_esri <- readRDS("data/park_coordinates_esri.rds")

# renaming the columns
names(park_coordinates_esri)[1] <- "esri_long"
names(park_coordinates_esri)[2] <- "esri_lat"

# Canada map ----
#level 0 = country; level 1 = province/state; level 2 = counties
provinces <- gadm(country="Canada", level=1, path = tempdir())

#save as an RDS
saveRDS(provinces,file ="data/shapefiles/CAprovinces_map.rds")

provinces <- readRDS("data/shapefiles/CAprovinces_map.rds")

#plot both shape files, layered
plot(provinces)

# import ndvi file
ndvi_bg <- "C:/Users/grace/Documents/GitHub/HWI_NDVI_parks/data/ndvi/2021ndvi/2021_jun/VIIRS-Land_v001-preliminary_NPP13C1_S-NPP_20210630_c20220419155820.nc"
ndvi_bg <- terra::rast(ndvi_bg) #bg is 2021 jun 30
plot(ndvi_bg$NDVI)

# reproject NDVI to provinces crs
reprojected_bg <- terra::project(ndvi_bg,
                                 provinces,
                                 method = "near")

saveRDS(reprojected_bg,file ="data/shapefiles/reprojected_bg.rds")

reprojected_bg <- readRDS("data/shapefiles/reprojected_bg.rds")

#crop reprojected ndvi bg to Can shape
cropped_provinces_ndvi <- crop(reprojected_bg, provinces, mask = TRUE) 
provinces_bg <- cropped_provinces_ndvi$NDVI
saveRDS(provinces_bg,file ="data/shapefiles/Canmap.rds")
provinces_bg <- readRDS("data/shapefiles/Canmap.rds")

#find the extent of the raster
ext(provinces_bg)

#set the bounding box
bbox <- ext(c(-141.006866, -52.6000041603337, 41.6999988824795, 83.0999985311861))

#crop the ndvi
bg_crop <- crop(provinces_bg, bbox)

#write raster
writeRaster(bg_crop, "figures/old_figures/bg_crop.tif", overwrite = TRUE)

#REPROJECT BG_CROP 
bg_reproject <- terra::project(bg_crop,
                               "ESRI:102001")

plot(bg_reproject)
saveRDS(bg_reproject,file ="figures/old_figures/bg_reproject.rds")
bg_reproject <- readRDS("figures/old_figures/bg_reproject.rds")

#crop the map
Can_crop <- crop(provinces, bbox)
saveRDS(Can_crop,file ="figures/old_figures/Can_crop.rds")
Can_crop <- readRDS("figures/old_figures/Can_crop.rds")

plot(Can_crop)

# Define manual color scale due to adding national parks ----
manual_colors <- c("WATE"= "#560133", "ELKI" = "#790149", "JASP" = "#9F0162", "WOOD" = "#C7007C",
                   "BANF" = "#EF0096", "YOHO" = "#FF5AAF", "KOOT" = "#FF9DCB", "REVE" = "#FFCFF2",
                   "PRIM" = "#450270", "GLAC" = "#65019F", "WAPU" = "#8400CD", "FUND" = "#A700FC",
                   "KOUC" = "#DA00FD", "NOVA" = "#FF3CFE", "KEJI" = "#FF92FD", "AULA" = "#FFCCFE",
                   "NAHA" = "#5A000F", "FIVE" = "#7E0018", "PELE" = "#A40122", "GBIS" = "#CD022D",
                   "THIS" = "#F60239", "PEIS" = "#FF6E3A", "FORI" = "#FFAC3B", "PALB" = "#FFDC3D", "IVVA" = "#FF4C30")


# Plotting the map 
provinces_sf <- st_as_sf(Can_crop)

saveRDS(provinces_sf,file ="data/shapefiles/provinces_sf.rds")
provinces_sf <- readRDS("data/shapefiles/provinces_sf.rds")

#NDVI colour palette
NDVI_cols <- colorRampPalette(rev(c("#0f2902", "#1d3900","#193401","#274009","#2e4511",
                                    "#3d4f21", "#485921","#536321","#69761f","#868924",
                                    "#8d8e37","#aaa263","#b5a975","#c2b58c","#c7b995",
                                    "#cdbf9f","#e3d6c6","#e7dbce")))
#plot map withESRI:102001 projection
#reprojected_new_map <- 
  ggplot() +
  geom_spatraster(data = bg_reproject, alpha = 0.8, maxcell = 5e+08) + #ndvi bg
  scale_fill_gradientn(name = "Normalized Difference Vegetation Index",
                       colours = NDVI_cols(255),
                       na.value = NA,
                       breaks = c(11,  63.75, 127.50, 191.25, 254.00),
                       labels = c(-1.0, -0.5,  0.0,  0.5,  1.0)) +
  geom_sf(data = provinces_sf, fill = "transparent", color = "black", size = 1) + #map
  geom_point(data = park_coordinates_esri, aes(x = esri_long, y = esri_lat, col = park, shape = park), 
             size = 3, alpha = 0.8) +
  guides(col = guide_legend(override.aes = list(alpha=0.8,
                                                shape = rep(17,25))),
         shape = "none", alpha = "none") +
  scale_colour_manual(name="Park",
                      values = manual_colors) +
  scale_shape_manual(values = rep(17,25)) +
  theme(
    panel.background = element_rect(fill="transparent"), #transparent panel bg
    plot.background = element_rect(fill="transparent", color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.x = element_blank(),
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "none",
    legend.justification = "center",
    legend.direction = "vertical",
    #legend.box.background = element_rect(color = "black"),
    plot.margin = unit(c(-1,0,-1,0), "cm"),
    plot.title = element_text(vjust = -8.5, hjust = 0.03,
                              size = 30, family = "sans", face = "bold")) +
  coord_sf() # ensures points don't get jittered around when figure dimensions change

ggsave(reprojected_new_map, filename = "figures/new_map_reprojected.png", width = 6.86, height = 6, units = "in", dpi = 600, background ="transparent")

