local_options <- options()
install.packages("tidyverse")
install.packages("sf")
install.packages("h3jsr")
install.packages("mapview")

library(sf)
library(dplyr)
library(ggplot2)
library(h3jsr)

library(mapview)
# for R < 4, since H3 addresses are handled as strings
options(stringsAsFactors = FALSE)

# This is the location of the Brisbane Town Hall:
bth <- sf::st_sfc(sf::st_point(c(153.023503, -27.468920)), crs = 4326)
# koordinaterna till Fisknätsgatan 21 59.292096, 18.246489
fiskis <- st_sfc(st_point(c(59.292096, -18.246489)), crs = 4326)

# where is the Brisbane Town Hall at resolution 15?
point_to_cell(bth, res = 15)
#> [1] "8fbe8d12acad2f3"
#> 
# where is fiskis at resolution 15?
point_to_cell(fiskis, res = 15)


nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
nc_pts <- st_centroid(nc)
nc_pts <- st_transform(nc_pts, crs = 4326)
nc_pts <- dplyr::select(nc_pts, CNTY_ID, NAME)

mapview(nc_pts)

# Give me the address for the center of each NC county at every resolution
nc_all_res <- point_to_cell(nc_pts, res = seq(0, 15), simple = FALSE)
head(nc_all_res[, c(1:5)])

# plot a few
ashe_hexes <- unlist(nc_all_res[1, c(6,7,8,9,10)], use.names = FALSE)
ashe_hexes <- cell_to_polygon(ashe_hexes, simple = FALSE)
ggplot(nc[1,]) +
  geom_sf(fill = NA, colour = 'black') +
  geom_sf(data = ashe_hexes, aes(fill = h3_address), alpha = 0.5) +
  scale_fill_viridis_d() +
  ggtitle('H3 hexagons over County Ashe, NC', subtitle = 'Resolutions 6-10') +
  theme_minimal() +
  coord_sf()

mapview(ashe_hexes)
