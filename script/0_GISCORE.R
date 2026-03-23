
library(sf)
library(terra)
library(ggplot2)
library(tidyverse)
library(dplyr)
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

#upazila visualize
Upazila <- gadm(
  country = "BD",
  level = 3,
  path = getwd()
)|>
  st_as_sf()

head(Upazila)

ggplot()+
  geom_sf(data = Upazila)+
  geom_sf_label(data = Upazila, aes(label = NAME_3))

