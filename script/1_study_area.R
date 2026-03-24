library(sf)
library(terra)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(giscoR)
library(geodata)


#district map

district_bd<- gadm(
  country = "BD",
  level = 2,
  path = getwd()
)|>
  st_as_sf()
#country visualize
ggplot()+
  geom_sf(data = district_bd)

#to show all the district names or maps area names
glimpse(district_bd)
unique(district_bd$NAME_2)

#select study area
study_area <- c("Barisal", "Comilla", "Lakshmipur")

filte_dis <- district_bd |> filter(NAME_2%in%study_area)

#plot
pal <- hcl.colors(length(labs), "Lajolla")

ggplot()+
  geom_sf(data = district_bd, fill = "grey", colours = "black", size = .8)+
  geom_sf(data = filte_dis, aes(fill = NAME_2),colours = "black", size = .1)+
  theme_light() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Study Area Map",
    subtitle = "District-wise Visualization",
    caption = "Map: GADM"
  )

ggsave("figure/1_study_area_theme_light.png",
       units = "in", width = 8, height = 10, dpi = 600)

ggplot()+
  geom_sf(data = district_bd, fill = "grey", colours = "black", size = .8)+
  geom_sf(data = filte_dis, aes(fill = NAME_2),colours = "black", size = .1)+
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Study Area Map",
    subtitle = "District-wise Visualization",
    caption = "Map: GADM"
  )

ggsave("figure/1_study_area_theme_bw.png",
       units = "in", width = 8, height = 10, dpi = 600)  

ggplot()+
  geom_sf(data = district_bd, fill = "grey", colours = "black", size = .8)+
  geom_sf(data = filte_dis, aes(fill = NAME_2),colours = "black", size = .1)+
  theme_classic() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Study Area Map",
    subtitle = "District-wise Visualization",
    caption = "Map: GADM"
  )

ggsave("figure/1_study_area_theme_classic.png",
       units = "in", width = 8, height = 10, dpi = 600)
  

ggplot()+
  geom_sf(data = district_bd, fill = "grey", colours = "black", size = .8)+
  geom_sf(data = filte_dis, aes(fill = NAME_2),colours = "black", size = .1)+
  theme_dark() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Study Area Map",
    subtitle = "District-wise Visualization",
    caption = "Map: GADM"
  )

ggsave("figure/1_study_area_theme_dark.png",
       units = "in", width = 8, height = 10, dpi = 600)

ggplot()+
  geom_sf(data = district_bd, fill = "grey", colours = "black", size = .8)+
  geom_sf(data = filte_dis, aes(fill = NAME_2),colours = "black", size = .1)+
  theme_get() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Study Area Map",
    subtitle = "District-wise Visualization",
    caption = "Map: GADM"
  )

ggsave("figure/1_study_area_theme_get.png",
       units = "in", width = 8, height = 10, dpi = 600)


ggplot()+
  geom_sf(data = district_bd, fill = "grey", colours = "black", size = .8)+
  geom_sf(data = filte_dis, aes(fill = NAME_2),colours = "black", size = .1)+
  theme_gray() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Study Area Map",
    subtitle = "District-wise Visualization",
    caption = "Map: GADM"
  )

ggsave("figure/1_study_area_theme_gray.png",
       units = "in", width = 8, height = 10, dpi = 600)

ggplot()+
  geom_sf(data = district_bd, fill = "grey", colours = "black", size = .8)+
  geom_sf(data = filte_dis, aes(fill = NAME_2),colours = "black", size = .1)+
  theme_grey() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Study Area Map",
    subtitle = "District-wise Visualization",
    caption = "Map: GADM"
  )

ggsave("figure/1_study_area_theme_grey.png",
       units = "in", width = 8, height = 10, dpi = 600)


ggplot()+
  geom_sf(data = district_bd, fill = "grey", colours = "black", size = .8)+
  geom_sf(data = filte_dis, aes(fill = NAME_2),colours = "black", size = .1)+
  theme_linedraw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Study Area Map",
    subtitle = "District-wise Visualization",
    caption = "Map: GADM"
  )

ggsave("figure/1_study_area_theme_linedraw.png",
       units = "in", width = 8, height = 10, dpi = 600)


ggplot()+
  geom_sf(data = district_bd, fill = "grey", colours = "black", size = .8)+
  geom_sf(data = filte_dis, aes(fill = NAME_2),colours = "black", size = .1)+
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(color = "grey50", size = 8, hjust = 0.5)
  ) +
  labs(
    title = "Study Area Map",
    subtitle = "District-wise Visualization",
    caption = "Map: GADM"
  )

ggsave("figure/1_study_area_theme_minimal.png",
       units = "in", width = 8, height = 10, dpi = 600)


