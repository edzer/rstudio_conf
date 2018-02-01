library(sf)
png("empty.png", width=600, heigh=200)
opar <- par(mfrow = c(1, 2), mar=c(2,0,0,0))
a <- st_polygon(list(cbind(c(0,0,7.5,7.5,0),c(0,-1,-1,0,0))))
b <- st_polygon(list(cbind(c(0,1,2,3,4,5,6,7,7,0),c(1,0,.5,0,0,0.5,-0.5,-0.5,1,1))))
plot(a - c(0,.7), ylim = c(-2,1))
text(3,-2.5, "POLYGON EMPTY")
plot(b, add = TRUE, border = 'red')
(i <- st_intersection(a,b))
plot(a, ylim = c(-2.5,1))
print(st_intersection(a,b))
text(3,-2, 
"GEOMETRYCOLLECTION (POINT (1 0), \n
      LINESTRING (4 0, 3 0), \n
      POLYGON ((5.5 0, 7 0, 7 -0.5, 6 -0.5, 5.5 0)))")

plot(b, add = TRUE, border = 'red')
plot(i, add = TRUE, col = 'green', lwd = 2)
par(opar)
