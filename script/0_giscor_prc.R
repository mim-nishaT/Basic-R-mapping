library(giscoR)
library(sf)
library(dplyr)

# Download The Netherlands boundaries at different resolutions
nl_all <- lapply(c("60", "20", "10", "03"), function(r) {
  gisco_get_countries(country = "Netherlands", year = 2024, resolution = r) |>
    mutate(res = paste0(r, "M"))
}) |>
  bind_rows()

glimpse(nl_all)

# Plot with ggplot2

library(ggplot2)

ggplot(nl_all) +
  geom_sf(fill = "#AD1D25") +
  facet_wrap(~res) +
  labs(
    title = "The Netherlands boundaries at different resolutions",
    subtitle = "Year: 2024",
    caption = gisco_attributions()
  ) +
  theme_minimal()


#bd
library(giscoR)
library(sf)
library(dplyr)

# Download The Netherlands boundaries at different resolutions
bd_all <- lapply(c("60", "20", "10", "03"), function(r) {
  gisco_get_countries(country = "Bangladesh", year = 2024, resolution = r) |>
    mutate(res = paste0(r, "M"))
}) |>
  bind_rows()

glimpse(bd_all)

# Plot with ggplot2

library(ggplot2)

ggplot(bd_all) +
  geom_sf(fill = "#AD1D25") +
  facet_wrap(~res) +
  labs(
    title = "The Bangladesh boundaries at different resolutions",
    subtitle = "Year: 2024",
    caption = gisco_attributions()
  ) +
  theme_minimal()


library(geodata)
library(terra)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)


# 1. Load your dataset
# Replace 'pop_data.csv' with your actual file name
data <- read.csv("data/bangladesh_pop_density.csv")

colnames(data)
# 2. Calculate Population Density
# Formula: Population / Area
data <- data |> 
  mutate(density = Population_2022/Area_km2)

# 3. Get Bangladesh District Map (Level 2)
bd_map <- gadm(country = "BGD",
               level = 2,
               path = getwd()) |>  
  st_as_sf()

glimpse(bd_map$NAME_2)

# 4. Standardize Names for Joining
# We match your "Name" column with the map's "NAME_2" column
# We use toupper() to avoid case-sensitivity issues
bd_map <- bd_map |>  mutate(join_key = toupper(NAME_2))
data <- data |> mutate(join_key = toupper(Name))

# 5. Merge Data
bd_final <- bd_map |> 
  left_join(data, by = "join_key")

# 6. Set Breaks and Labels (Adjusted for Bangladesh context)
# Bangladesh is dense; we use higher thresholds than Europe
br <- c(0, 500, 800, 1100, 1500, 2500, 5000, 10000, 50000)
labs <- c("0-500", "500-800", "800-1100", "1100-1500", "1500-2500", 
          "2500-5000", "5000-10k", ">10k")

bd_final <- bd_final |> 
  mutate(dist_cut = cut(density, breaks = br, labels = labs))

# 7. Palette and Plot
pal <- hcl.colors(length(labs), "Lajolla")

ggplot(bd_final) +
  geom_sf(aes(fill = dist_cut), color = "white", linewidth = 0.1) +
  # CRS 3106 is the optimized projection for Bangladesh
  coord_sf(crs = st_crs(3106)) +
  scale_fill_manual(
    values = pal,
    na.value = "grey90",
    name = "People / sq. km",
    guide = guide_legend(direction = "vertical", nrow = 1, title.position = "top")
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Bangladesh Population Density 2022",
    subtitle = "District-wise Visualization",
    caption = "Source: 2022 Population Census | Map: GADM"
  )


# ... (your previous code for mapping and joining)

ggplot(bd_final) +
  geom_sf(aes(fill = dist_cut), color = "white", linewidth = 0.1) +
  coord_sf(crs = st_crs(3106)) +
  
  # 1. Update the scale_fill_manual for vertical orientation
  scale_fill_manual(
    values = pal,
    na.value = "grey90",
    name = "People / sq. km",
    guide = guide_legend(
      direction = "vertical",    # Change from horizontal to vertical
      title.position = "top", 
      label.position = "right"   # Labels appear to the right of the color boxes
    )
  ) +
  
  theme_void() +
  
  # 2. Update the theme for right-side placement
  # ... (your ggplot code)
  theme_void() +
  theme(
    # --- ADD THESE LINES TO FIX THE BACKGROUND ---
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # ----------------------------------------------
    
    legend.position = "right",
    legend.justification = "center",
    legend.key.height = unit(1.2, "line"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5))+labs(
      title = "Bangladesh Population Density 2022",
      subtitle = "District-wise Visualization",
      caption = "Source: 2022 Population Census | Map: GADM"
    )

# Then save as usual
ggsave("figure/0_giscore_geomdata_pop_bd.png",
       units = "in", width = 8, height = 10,
       dpi = 600)
