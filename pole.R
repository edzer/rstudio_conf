library(sf)
library(units)

pts = rbind(c(0,80), c(120,80), c(240,80), c(0,80))
pol = st_sfc(st_polygon(list(pts)), crs = "+proj=longlat")

pol %>% st_area %>% set_units(km^2)
pol %>% st_transform("+proj=eqc") %>% st_area
pol %>% st_set_crs(NA) %>% st_area
