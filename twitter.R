library(sf)
library(maptools)
library(animation)
data(wrld_simpl)
w <- st_as_sf(wrld_simpl)
set.seed(131)
w$f = factor(sample(1:12, nrow(w), replace = TRUE))

# all points long l, lat p for which cos(c) = 0; l0 = lon0, p0 = lat0

# cos(c) = sin(p0)*sin(p) + cos(p0)*cos(p)*cos(l-l0) = 0
# sin(p0)*sin(p) = - cos(p0)*cos(p)*cos(l-l0)
# sin(p)/cos(p) = -cos(p0)*cos(l-l0)/sin(p0)
# tan(p) = -cos(l-l0)/tan(p0)
# p = atan(-cos(l-l0)/tan(p0))

# vary l, compute corresponding p;
# all gc's have all longitudes, except when the poles are part of it;
# in that case, lat0 == p0 = 0 and p is always pi/2

circ = function(l = c(-180:180), lon0 = 0, lat0 = 30) {
	deg2rad = pi / 180
	lat = atan(-cos((l - lon0) * deg2rad)/tan(lat0 * deg2rad)) / deg2rad
	xy = if (lat0 == 0) {
		l1 = lon0 - 90
		l2 = lon0 + 90
		rbind(c(l1,-90), c(l2,-90), c(l2,0), c(l2,90), c(l1,90), c(l1,0), c(l1,-90))
	} else if (lat0 > 0) {
		xy = cbind(lon = l, lat = lat)
		rbind(c(-180,90),xy,c(180,90),c(-180,90))
	} else {
		xy = cbind(lon = l, lat = lat)[length(l):1,]
		rbind(c(180,-90), xy, c(-180,-90),c(180,-90))
	}
	st_sfc(st_polygon(list(xy)), crs = st_crs(4326))
	# TODO: break at dateline, guarantee within -180,180
}

m = st_make_grid()
m = st_segmentize(m, 4e5)

par(mar = rep(0,4))
i = 0
lat=30+(i/2)
lon=-10-(i/2)
print(c(i,lon,lat))
p4s=paste0("+proj=ortho +lat_0=", lat, " +lon_0=", lon)
plot(st_transform(m, st_crs(p4s), check = TRUE), col = 'lightblue', border = 'grey')
crc = circ(lat0 = lat, lon0 = lon)
w0 = suppressWarnings(st_intersection(w, crc))
w0 = st_cast(w0, "MULTIPOLYGON")
plot(st_geometry(st_transform(w0["f"], st_crs(p4s), check = TRUE)), add = TRUE, col = 'white')

lt = 70
pts = rbind(c(0,lt), c(120,lt), c(-120,lt), c(0,lt))
pol = st_sfc(st_polygon(list(pts)), crs = "+proj=longlat")

library(units)
pol_seg = st_segmentize(pol, set_units(100, km))
pol %>% st_transform(p4s) %>% plot(add = TRUE, col = 'red')
pol_seg %>% st_transform(p4s) %>% plot(add = TRUE, col = NA, border = '#00aa00aa', lwd = 2)

pol_eq = st_transform(pol, "+proj=eqc") %>% st_segmentize(set_units(100, km)) %>%
	st_transform(p4s) %>% plot(add = TRUE, col = NA, border = 'blue', lwd = 2)

# Equidistant Cylindrical (Plate Carrée):
# pol %>% st_transform("+proj=eqc") %>% st_area
