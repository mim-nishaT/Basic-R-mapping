

library(sf)
library(terra)
library(ggplot2)
library(tidyverse)
library(dplyr)

#menual map adding from BGD file or local data
U_map <- read_sf("CopyOfBGD_adm/BGD_adm1_prj.shp")

#visualize map
ggplot()+
  geom_sf(data= U_map)

#universal update data package
library(giscoR)

bangladesh <- gisco_get_countries(
  country = "BD",
  resolution = "10"
)


ggplot()+
  geom_sf(data=bangladesh, aes(), fill="red")




#another universal data package geodata
library(geodata)
bangladesh <- gadm(
  country = "BD",
  level = 2,
  path = getwd()
)|>
  st_as_sf()
#country visualize
ggplot()+
  geom_sf(data = bangladesh)

#division visualize
head(bangladesh)

ggplot()+
  geom_sf(data = bangladesh)+
  geom_sf_text(data = bangladesh, aes(label = NAME_1))

#district visualize
ggplot()+
  geom_sf(data = bangladesh)+
  geom_sf_text(data = bangladesh, aes(label = NAME_2))

#or 
ggplot()+
  geom_sf(data = bangladesh)+
  geom_sf_label(data = bangladesh, aes(label = NAME_2))


ctg <- bangladesh |> 
  filter(NAME_1=="Barisal")

ggplot()+
  geom_sf(data=ctg, aes(fill=NAME_2))+
  geom_sf_label(data = bangladesh, aes(label=NAME_1))+
  theme_classic()+
  theme(
    legend.position = "none",

  )


  districts <- c(
    "Barisal", "Bhola", "Brahamanbaria", "Chandpur", "Chuadanga",
    "Comilla", "Dhaka", "Faridpur", "Feni", "Gaibandha", "Gazipur",
    "Gopalganj", "Jamalpur", "Jessore", "Kishoreganj", "Kurigram",
    "Kushtia", "Laksmipur", "Madaripur", "Magura", "Manikganj",
    "Maulvibazar", "Munshiganj", "Mymensingh", "Naogaon", "Narayanganj",
    "Noakhali", "Pabna", "Putuakhali", "Rajbari", "Rajshahi", "Rangpur",
    "Shariatpur", "Satkhira", "Sherpur", "Sirajganj", "Tangail"
  )
  
regions_filtered <- bangladesh %>%
  filter(NAME_2 %in% districts)


modern_colors_38 <- c(
  "#BF1922", "#C99383", "#B17A50", "#FACF39", "#FFC93C",
  "#FCE38A", "#F7E8D3", "#137DC5", "#112D4E", "#00CFC8",
  "#FF6B6B", "#6A994E", "#FFA69E", "#FFB947", "#2E86AB",
  "#5D3FD3", "#D4A5A5", "#8E44AD", "#E59866", "#273746",
  "#5B6D92", "#B5D5E6", "#08A8E4", "#99B6B3", "#70959E",
  "#BFC9D0", "#605E55", "#4C4D4F", "#FED5BC", "#FEAC7A",
  "#FEEE98", "#FEE7D8", "#FE995C", "#A26BCD", "#D8ACE0", 
  "#E33B08", "#0FA27E"
)

ggplot()+
  geom_sf(data=bangladesh, fill="grey90", color="black", size=0.05)+
  geom_sf(data=regions_filtered, aes(fill=NAME_2), color="black", size=0.05)+
  theme_classic()+
  guides(colour = guide_legend(),
size= guide_legend(title.position = "bottom", title.hjust=0.05))+
  theme(
    plot.background = element_rect("#C4E1E6"),
    legend.position = "none",
    panel.background = element_rect("#C4E1E6")
  )+
  scale_fill_manual(values = modern_colors_38)







library(sf)
library(terra)
library(ggplot2)
library(tidyverse)
library(dplyr)

library(tmap)
library(tmaptools)
library(spdep)
library(classInt)
library(ggpubr)
library(scales)
library(raster)



data <- read_excel("C:\\Users\\estie\\OneDrive\\Documents\\class 2\\NOISE DATA OF 64 DISTRICT.xlsx")


bd_shape <- gadm(
  country = "Bd",
  level = 2,
  path= getwd()
  
)%>%
  st_as_sf()


bd_shape <- merge(bd_shape, data, by.x="NAME_2", by.y= "NAME_2", all.x=T)


head(bd_shape)



breaks <- classIntervals(
  bd_shape$`Noise (dBA)`,
  n=5,
  style = "equal"
)$breaks



ggplot()+
  geom_sf(
    data = bd_shape,
    aes(fill=`Noise (dBA)`)
  )

nb <- poly2nb(bd_shape, snap = 0.001)
  
lw <- nb2listw(nb, style = "W", zero.policy = T)

bd_shape$variable_of_interest <- bd_shape$`Noise (dBA)`

noise_data_sf <- st_as_sf( data, coords = c("long", "lat"), crs=4326)


bd_shape_sp <- as(bd_shape, "Spatial")
noise_data_sf <- as(noise_data_sf, "Spatial")
noise_data_sf$variable_of_interest <- noise_data_sf$Noise..dBA.

grd <- as.data.frame(spsample(bd_shape_sp,"regular", n=100000))


names(grd) <- c("x", "y")

coordinates(grd) <- ~x + y

gridded(grd) <- TRUE
fullgrid(grd) <- T



proj4string(grd) <- proj4string(bd_shape_sp)


library(gstat)


idw_output <- idw(variable_of_interest ~1, noise_data_sf, newdata= grd, idp=2)


raster_idw <- raster(idw_output)


plot(raster_idw)

raster_idw_masked <- mask(raster_idw, bd_shape_sp)

plot(raster_idw_masked)


idw_df <- as.data.frame(rasterToPoints(raster_idw_masked))


colnames(idw_df) <- c("long", "lat", "Noise")

breaks <- classIntervals(
  idw_df$Noise,
  n=5,
  style = "equal"
)$brks


library(RColorBrewer)
library(viridis)

colors <- hcl.colors(
  n=length(breaks),
  palette= "Temps",
  rev= F
)


ggplot(data = idw_df)+
  geom_raster(
    aes(
      x=long,
      y=lat,
      fill = Noise
    )
  )+
  geom_point(data = data, x=data$long, y=data$lat, aes(), color="red")+
  geom_contour(
    aes(
      x=long,
      y=lat,
      z= Noise)
  )+
  coord_sf()+
  scale_fill_gradientn(
    name= "noise level (DB)",
    colors= colors,
    breaks=breaks,
    labels = round(breaks, 0),
    limits = c(
      min(idw_df$Noise),
      max(idw_df$Noise)
    )
    
  )+
  guides(
    fill=guide_colorbar(
      direction = "horizontal",
      barheight = unit(8, "mm"),
      title.position = "top"
    ))+
  theme_classic()+
  theme(
    legend.position = "bottom"
  )



nb <- poly2nb(bd_shape, snap = 0.001)

lw <- nb2listw(nb, style = "W", zero.policy = T)

noise_data_sf$variable_of_interest <- noise_data_sf$Noise..dBA.


sum(is.na(bd_shape$variable_of_interest))


moran_local <- localmoran(bd_shape$variable_of_interest, lw, zero.policy = T)


bd_shape$Ii <- moran_local[,1]
bd_shape$Z_Ii <- moran_local[,4]
bd_shape$p_value <- moran_local[,5]


bd_shape$Gi_Bin <- cut(
  bd_shape$p_value,
  breaks = c(0, 0.01, 0.05, 0.1, 1),
  labels= c(
    "Hot spotspot- 99%",
    "Hot spotspot- 95%",
    "Hot spotspot- 90%",
    "Not significant"
  )
)


pallate_name <- c(
  "Hot spotspot- 99%"= "red",
  "Hot spotspot- 95%"= "orange",
  "Hot spotspot- 90%"="yellow",
  "Not significant"="grey"
  
)


tm_shape(bd_shape)+
  tm_polygons(
    "Gi_Bin",
    fill.scale = tm_scale(values=pallate_name),
    fill.legend = tm_legend(show = F)
  )+
  tm_borders()+
  tm_graticules(
    lines=F
  )

"C:\Users\estie\Downloads\bangladesh_dem.tif"


install.packages(c("terra", "ggplot2", "viridis", "sf"))

library(terra)
library(ggplot2)
library(viridis)
library(sf)

# Load DEM
dem <- rast("C:\\Users\\estie\\Downloads\\bangladesh_dem.tif")

library(terra)

# Reduce resolution (factor = 5 → 150 m)
dem_small <- aggregate(dem, fact = 10, fun = mean)

pol <- as.polygons(dem_small, dissolve = TRUE)


dem_small_one <- as(pol, "Spatial")

dem_small_one <- mask(dem, bd_shape_sp)
plot(dem_small_one)
dem_df <- as.data.frame(dem_small_one, xy = TRUE, na.rm = TRUE)

colnames(dem_df) <- c("x", "y", "elevation")



ggplot(dem_df, aes(x = x, y = y, fill = elevation)) +
  geom_raster() +
  scale_fill_viridis(
    option = "terrain",
    name = "Elevation (m)"
  ) +
  coord_equal() +
  theme_minimal()

# Create hillshade
slope <- terrain(dem, "slope", unit = "radians")
aspect <- terrain(dem, "aspect", unit = "radians")
hillshade <- shade(slope, aspect)

plot(hillshade, col = gray.colors(100), main = "Hillshade of Bangladesh")


# 5. RENDER SCENE
#----------------

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

rayshader::plot_gg(
  ggobj = map,
  width = w / 500,
  height = h / 500,
  scale = 50,
  solid = FALSE,
  shadow = TRUE,
  shadowcolor = "white",
  shadowwidth = 0,
  shadow_intensity = 1,
  zoom = .7,
  phi = 87,
  theta = 0,
  windowsize = c(1000,1000)
)

# 6. RENDER OBJECT
#-----------------

url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(url)

hdri_file <- basename("harvest_4k.hdr")


download.file(
  url = url,
  destfile = hdri_file,
  mode = "wb"
)

filename <- "3d-dem-mym_three.png"

rayshader::render_highquality(
  filename = filename,
  preview = TRUE,
  light = TRUE,
  environment_light = hdri_file,
  intensity_env = .6,
  interactive = FALSE,
  width = 800,
  height = 800
)

