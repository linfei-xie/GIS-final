library("GWmodel")
library("ggplot2")
library("rgdal")
library("sf")
library("RColorBrewer")
library("openxlsx")
library("spdep")
library("Cairo")
library("showtext")


# Read  file
gw_maps <- readOGR("barcelona_data.shp", stringsAsFactors = F, use_iconv = T, encoding = "UTF-8")
names(gw_maps)


# range
gw_maps@bbox

# label
list2 <- list("SpatialPolygonsRescale", layout.north.arrow(), 
              offset = c(229200,5051000), scale = 2000) # compass
list3 <- list("SpatialPolygonsRescale", layout.scale.bar(), 
              offset = c(231000,5051800), scale = 4000, fill=c("transparent","black")) # label
s1_text0 <- list("sp.text", c(231000, 5050600 + 2000), "0", cex = 0.8) 
s1_text1 <- list("sp.text", c(231000 + 4000, 5050600 + 2000), "4000m", cex = 0.8) 


# queen
gw_maps$id <- rownames(gw_maps@data)
queen.w <- poly2nb(gw_maps, row.names = gw_maps$id, queen = TRUE)

# listw
queen.wl <- nb2listw(queen.w, style = "W")

# global moran
set.seed(12345)
moran.mc(gw_maps$airbnb, listw = queen.wl, nsim = 999, alternative = 'greater')

# moran
lomoran <- data.frame(localmoran(gw_maps$airbnb, listw = queen.wl, alternative = "greater"))
gw_maps@data[names(lomoran)] <- lomoran


CairoPNG(file="lomoran.png",width=429,height=508)
spplot(gw_maps, "Ii",
       panel = panel.polygonsplot,
       col.regions = rev(brewer.pal(7, "RdYlBu")), cuts = 6,
       colorkey = list(space = "right"),
       main = "local moran",
       sp.layout=list(list2,list3,s1_text0,s1_text1))
dev.off()

CairoPNG(file="global.moranmap.png",width=429,height=508)
moran.plot(gw_maps$airbnb, queen.wl)
dev.off()

# hotcold
localG(gw_maps$airbnb, listw = queen.wl)[1:73]
gw_maps@data$localG <- localG(gw_maps$airbnb, listw = queen.wl)[1:73]


CairoPNG(file="Getis.map.png",width=429,height=508)
spplot(gw_maps, "localG",
       panel = panel.polygonsplot,
       col.regions = rev(brewer.pal(7, "RdYlBu")), cuts = 6,
       colorkey = list(space = "right"),
       main = "Cold and hot spot analysis",
       sp.layout=list(list2,list3,s1_text0,s1_text1))
dev.off()

library(car)

# bandwith
bw.gwr <- bw.gwr(airbnb ~ houseprice + income + parks + markets + pubs + density, data = gw_maps,
                 approach = "AICc", kernel = "bisquare", adaptive = TRUE)

# gwr
gwr.res <- gwr.basic(airbnb ~ houseprice + income + parks + markets + pubs + density, data = gw_maps,
                     bw = bw.gwr, kernel = "gaussian", adaptive = TRUE, F123.test = TRUE)
print(gwr.res)

# houseprice
CairoPNG(file="houseprice.png",width=429,height=508)
spplot(gwr.res$SDF, "houseprice", panel = panel.polygonsplot,
       col.regions = brewer.pal(7, "YlOrRd"), cuts = 6,
       colorkey = list(space = "right"),
       main = "second-hand housing price", 
       sp.layout=list(list2,list3,s1_text0,s1_text1))
dev.off()

# income
CairoPNG(file="income.png",width=429,height=508)
spplot(gwr.res$SDF, "income", panel = panel.polygonsplot,
       col.regions = brewer.pal(7, "YlOrRd"), cuts = 6,
       colorkey = list(space = "right"),
       main = "Household income index", 
       sp.layout=list(list2,list3,s1_text0,s1_text1))
dev.off()

# parks
CairoPNG(file="parks.png",width=429,height=508)
spplot(gwr.res$SDF, "parks", panel = panel.polygonsplot,
       col.regions = brewer.pal(7, "YlOrRd"), cuts = 6,
       colorkey = list(space = "right"),
       main = "Parks", 
       sp.layout=list(list2,list3,s1_text0,s1_text1))
dev.off()

# markets
CairoPNG(file="markets.png",width=429,height=508)
spplot(gwr.res$SDF, "markets", panel = panel.polygonsplot,
       col.regions = brewer.pal(7, "YlOrRd"), cuts = 6,
       colorkey = list(space = "right"),
       main = "markets", 
       sp.layout=list(list2,list3,s1_text0,s1_text1))
dev.off()


# pubs
CairoPNG(file="pubs.png",width=429,height=508)
spplot(gwr.res$SDF, "pubs", panel = panel.polygonsplot,
       col.regions = brewer.pal(7, "YlOrRd"), cuts = 6,
       colorkey = list(space = "right"),
       main = "music and drink's space", 
       sp.layout=list(list2,list3,s1_text0,s1_text1))
dev.off()

# density
CairoPNG(file="density.png",width=429,height=508)
spplot(gwr.res$SDF, "density", panel = panel.polygonsplot,
       col.regions = brewer.pal(7, "YlOrRd"), cuts = 6,
       colorkey = list(space = "right"),
       main = "Population density", 
       sp.layout=list(list2,list3,s1_text0,s1_text1))
dev.off()

