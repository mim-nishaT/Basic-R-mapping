library(sf)
library(terra)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(giscoR)
library(geodata)
library(patchwork)




#need to compare 3 census data with sharp mapswork with patchwork comparison,
#theme_minimal and adding proper legant

# Calculate densities
data <- read.csv("data/bangladesh_pop_density.csv")

data_dens <- data |> 
  mutate(
    dens_1991 = Population_1991 / Area_km2,
    dens_2001 = Population_2001 / Area_km2,
    dens_2011 = Population_2011 / Area_km2,
    dens_2022 = Population_2022 / Area_km2
  )



#district map

district_bd<- gadm(
  country = "BGD",
  level = 2,
  path = getwd())|>
  st_as_sf()
#country visualize
ggplot()+
  geom_sf(data = district_bd)

#check mapdata
glimpse(district_bd)
unique(district_bd$NAME_2)

# Join with your map object (assuming it's named bd_map)
bd_final <- district_bd |> 
  left_join(data_dens, by = c("NAME_2" = "Name"))

# Create a function to make the maps
make_density_map <- function(year_col, title_text) {
  ggplot(bd_final) +
    geom_sf(aes(fill = .data[[year_col]]), color = "white", linewidth = 0.05) +
    scale_fill_viridis_c(option = "rocket", direction = -1, name = "Density") +
    theme_void() +
    labs(subtitle = title_text) +
    theme(legend.position = "none", plot.title = element_text(size = 10))
}


# Generate the 4 plots
p1 <- make_density_map("dens_1991", "1991 Census")
p2 <- make_density_map("dens_2001", "2001 Census")
p3 <- make_density_map("dens_2011", "2011 Census")
p4 <- make_density_map("dens_2022", "2022 Census")

# Combine them into a 2x2 grid
# 'collect' makes sure there is only ONE legend for all 4 maps
library(patchwork)

# Combine the 4 plots using the explicit wrapper
final_comparison <- wrap_plots(p1, p2, p3, p4, ncol = 2) + 
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Population Density Evolution (1991 - 2022)",
    subtitle = "Comparison of four national censuses",
    caption = "Source: BBS Census Data | Map: GADM",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

# Apply white background to every sub-plot as well
final_comparison <- final_comparison & theme(
  plot.background = element_rect(fill = "white", color = NA))

# Show the plot
print(final_comparison)

# Save
ggsave("figure/census_comparison.png",
       final_comparison, units = "in", width = 12, height = 14,
       bg = "white", dpi = 800)

#0r


# 2. Update the Function to match your requested style
make_styled_map <- function(year_col, title_text) {
  ggplot() +
    # Layer 1: Background (All districts in grey)
    geom_sf(data = district_bd, fill = "grey90", color = "black", size = 0.1) +
    geom_sf(data = bd_final, aes(fill = .data[[year_col]]), color = "black",
            size = 0.1) +
    scale_fill_viridis_c(option = "mako", direction = -1, name = "Density") +
    theme_minimal() +
    labs(subtitle = title_text) +
    theme(
      legend.position = "none", # Individual legends hidden for patchwork
      panel.grid = element_blank(),
      axis.text = element_blank(),
      plot.subtitle = element_text(hjust = 0.5, face = "italic")
    )
}

# 4. Combine with Patchwork and add your specific Labels
final_plot <- wrap_plots(p1, p2, p3, p4, ncol = 2) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Study Area Map: Population Density Evolution",
    subtitle = "District-wise Visualization (1991-2022)",
    caption = "Source: 2022 Population Census | Map: GADM",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      plot.caption = element_text(color = "grey50", size = 9, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

# 5. Save with white background
ggsave("figure/2_study_area_comparison.png", final_plot, 
       width = 10, height = 12, dpi = 600, bg = "white")
