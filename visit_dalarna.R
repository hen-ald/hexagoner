library(sf)
library(readxl)
library(dplyr)
library(tidyverse)
library(mapview)


dalarna_besoksmal <- read_excel('visit_dalarna.xlsx')
#glimpse(dalarna_besoksmal)

dalarna_besoksmal <- dalarna_besoksmal%>% 
  st_as_sf(coords = c("Longitud", "Latitud"),
    na.fail = FALSE, 
    crs=4326) %>% 
  st_transform(crs = 3006)

mapview(dalarna_besoksmal)+
  mapview(kommun)

kommun_fil <- "G:/Samhällsanalys/GIS/projekt/Invest in Dalarna/utdata/till_rmd/kommun.gpkg" #824 kb
kommun <- st_read(kommun_fil, crs = 3006) %>% 
  select("kom_kod" = "kommunkod",
         "kommun" = "kommunnamn")

plot(dalarna_besoksmal)

# Lägger punkter på polygoner med add = TRUE
kommun %>% 
  select(geom) %>% plot()

dalarna_besoksmal %>% 
  select(geometry) %>% 
           plot(col = "red", add=TRUE)

#lägger punkter på polygoner i GGPLOT

library(ggplot2)
ggplot(dalarna_besoksmal)+
  geom_sf(data=kommun)+
  geom_sf()+
  coord_sf()

# Vilka punkter ligger i vilka kommuner
#I vilka kommuner är besöksmålen?
glimpse(dalarna_besoksmal)
besoksmal_kom <- dalarna_besoksmal %>% 
  select(Namn, geometry) %>%
  st_join(
    kommun %>% 
      select(kommun,kom_kod, geom),
    join = st_intersects,
    left = FALSE
  )
besoksmal_kom

n_besoksmal_kom<- besoksmal_kom %>% 
  st_drop_geometry() %>% 
  count(kommun) %>% 
  right_join(kommun, by = c("kommun" = "kommun")) %>% 
  st_as_sf()

ggplot(n_besoksmal_kom,
       aes(fill = n))+
  geom_sf()+
  coord_sf()


# TMAP

library(tmap)
tm_shape(besoksmal_kom)+
  tm_dots()

tm_shape(kommun)+
  tm_polygons()+
  tm_shape(besoksmal_kom)+
  tm_dots()

tm_shape(n_besoksmal_kom)+
  tm_polygons(col = "n")

#Leaflet behöver wgs84

dalarna_besoksmal_wgs <- read_excel('visit_dalarna.xlsx') %>% 
  drop_na(Latitud, Longitud)
#glimpse(dalarna_besoksmal)

dalarna_besoksmal_wgs <- dalarna_besoksmal_wgs%>% 
  st_as_sf(coords = c("Longitud", "Latitud"),
           na.fail = FALSE, 
           crs=4326) #%>% 
  # st_transform(crs = 3006)

kommun_fil_wgs <- "G:/Samhällsanalys/GIS/projekt/Invest in Dalarna/utdata/till_rmd/kommun.gpkg" #824 kb
kommun_wgs <- st_read(kommun_fil_wgs, crs = 3006) %>% 
  select("kom_kod" = "kommunkod",
         "kommun" = "kommunnamn") %>% 
  st_transform(crs = 4326)

plot(dalarna_besoksmal_wgs)

# Lägger punkter på polygoner med add = TRUE
kommun_wgs %>% 
  select(geom) %>% plot()

dalarna_besoksmal_wgs %>% 
  select(geometry) %>% 
  plot(col = "red", add=TRUE)

#lägger punkter på polygoner i GGPLOT

library(ggplot2)
ggplot(dalarna_besoksmal_wgs)+
  geom_sf(data=kommun)+
  geom_sf()+
  coord_sf()

# Vilka punkter ligger i vilka kommuner
#I vilka kommuner är besöksmålen?
glimpse(dalarna_besoksmal_wgs)
besoksmal_kom_wgs <- dalarna_besoksmal_wgs %>% 
  select(Namn, geometry) %>%
  st_join(
    kommun_wgs %>% 
      select(kommun,kom_kod, geom),
    join = st_intersects,
    left = FALSE
  )
besoksmal_kom_wgs

n_besoksmal_kom_wgs<- besoksmal_kom_wgs %>% 
  st_drop_geometry() %>% 
  count(kommun) %>% 
  right_join(kommun, by = c("kommun" = "kommun")) %>% 
  st_as_sf()

n_besoksmal_kom_wgs
library(leaflet)

leaflet(besoksmal_kom_wgs) %>% 
  addTiles() %>% 
  addCircles(data = besoksmal_kom_wgs)

#mapview(besoksmal_kom_wgs)

leaflet(n_besoksmal_wgs) %>% 
  addTiles() %>% 
  addPolygons(data = kommun_wgs, color = "black", weight = 3, opacity = 0.2, popup = ~kommun) %>% 
  addCircles(data = dalarna_besoksmal_wgs, popup = ~Namn)

#leafgl klarar större lager fler punkter även bättre på choropleth
install.packages("leafgl")
library(leafgl)
leaflet(elementId = "kommun") %>% 
  addTiles() %>% 
  addGlPolygons(data = kommun_wgs %>% st_cast("POLYGON"), popup = "Namn", fillColor = "black") %>%
  addGlPoints(data = besoksmal_kom_wgs, popup = "Namn")

#Choropleth funkade ej!!
leaflet(n_besoksmal_kom_wgs) %>% 
  addProviderTiles('CartoDB.Voyager') %>% 
  addGlPolygons(data = n_besoksmal_kom_wgs %>% 
  st_cast("POLYGON"), opacity = 0.5, fillColor = n_besoksmal_kom_wgs$n)

#HEXAGONER!!!!!!!!!!

install.packages("h3jsr")
library(h3jsr) 

besoksmal_wgs_hex <- dalarna_besoksmal_wgs %>% 
  select(Namn) %>% 
  mutate(hex = point_to_cell(geometry, res = 8)) %>% 
  st_drop_geometry() %>% 
  count(hex) %>% 
  cell_to_polygon(simple = TRUE)

#choropleth hex
################################
###Funkar ej!!##################

library(here)

leaflet(besoksmal_wgs_hex) %>% 
  addProviderTiles('CartoDB.Voyager') %>% 
  addGlPolygons(data=besoksmal_wgs_hex, opacity=0.5, fillColor = 'n')
  
