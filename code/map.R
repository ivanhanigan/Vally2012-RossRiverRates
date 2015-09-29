
################################################################
# name:nework
require(swishdbtools)
require(maps)
pwdsu <- getPassword(remote=T)
pwd <- getPassword(remote=T)
su <- connect2postgres('130.56.102.41','delphe','ivan_hanigan', p = pwdsu)
pgListTables(su, "public")
gislib <- connect2postgres('dc-geoserve.anu.edu.au','gislibrary','gislibrary', p = pwd)
if(!file.exists("data/temproads.shp"))
{
roads <-  readOGR2(hostip = 'dc-geoserve.anu.edu.au', user = 'gislibrary', db = 'gislibrary',
                   layer = 'osm.australia_highway', p = pwd)

dbSendQuery(gislib,
"drop table public.bunburyroads;
SELECT name, st_transform(t1.geom,4283) as the_geom
into public.bunburyroads
  FROM osm.australia_highway t1,
  (select * from abs_sd.aussd07 where sdcode07 = '510') t2
  where st_within(st_transform(st_centroid(t1.geom),4283), t2.geom);
alter table public.bunburyroads add column gid serial primary key;
")
# because gislibrary user cant add to the geometry table readOGR wont work
# do as ivan_hanigan
# sql <- fixGeom(su, "public.bunburyroads")
# cat(sql)
# dbSendQuery(su,sql)
dbGetQuery(gislib, "select * from public.geometry_columns where f_table_schema = 'public'")
pgListTables(gislib, "public")
roads <- readOGR2(hostip = 'dc-geoserve.anu.edu.au', user = 'gislibrary', db = 'gislibrary', layer = 'bunburyroads', p = pwd)

oldwd <- getwd();setwd('data')
writeOGR(roads,'temproads','temproads','ESRI Shapefile')
setwd(oldwd)
}
setwd("data")
dir()

#roads <- readOGR(dsn="temproads.shp", layer="temproads")
roads <- readOGR2(hostip = 'dc-geoserve.anu.edu.au', user = 'gislibrary', db = 'gislibrary', layer = 'bunburyroads', p = pwd)
plot(roads)
setwd("..")

wa <- readOGR2(hostip = 'dc-geoserve.anu.edu.au', user = 'gislibrary', db = 'gislibrary', layer = 'abs_sd.aussd07', pwd)
#rivers <- readOGR2('130.56.102.150', 'ivan_hanigan','delphe','public.watercl_wa', pwdsu)  
est <- readOGR2('130.56.102.41', 'ivan_hanigan','delphe','public.leschenault', pwdsu)
oldwd <- getwd()
setwd("ANZJPH/rrv_final/")
rrv <- readOGR('rrv.shp', 'rrv')        
setwd(oldwd)
setwd("ANZJPH")
dir(pattern = ".shp")
estbuf <- readOGR('estbuf.shp', 'estbuf') 
estbuf@data$band <- ifelse(estbuf@data$gid2 %in% c(1,2,5,6,9,10,13,14,17,18),  'white', 'darkgrey')
plot(estbuf, col = estbuf@data$band)
bunbury <- readOGR('bunbury.shp', 'bunbury') 
setwd(oldwd)
if (!require(oz)) install.packages('oz'); require(oz)
# #############################
# jpeg('reports/Figure1.jpg', res = 150, width = 1000, height = 1400)
# m <- matrix(c(2,1,1,1),2,2)
# layout(m, widths=c(1.5, 2),heights=c(1.5, 2))
# # layout.show(2)
# par(cex = 1.2, mar = c(2,1,1,1))
# 
# plot(est, col = 'white', ylim = c(-33.33, -33.2), xlim=c(115.62,115.77))
# plot(bunbury, add=T, col = 'grey', border=F)
# plot(bunburyroads, add=T)
# plot(wa, add=T)
# # with(rrv, points(RESIDENCE_.1, RESIDENCE_, cex=.7,pch =16))
# #with(rrv, points(EXPOSURE_L.1, EXPOSURE_L, cex=.7,pch =16))
# #axis(1)
# map.scale(ratio=F,relwidth=.15,cex=1, x = 115.732, y=-33.34)
# box()
# legend('topright', c('Roads','Urban zone'), lty  =c(1,NA), fill=c(NA,'grey')  ,   border=c(NA,1)   )
# # inset
#         plot(wa, col='lightgrey',border=F)
# # map('world','Australia', ylim=c(-45,-11),xlim=c(110.5,130))
# xdj <- .15; ydj <- .5
# polygon(c(115.5-xdj,115.9+xdj,115.9+xdj,115.5-xdj),c(-33.45+ydj,-33.45+ydj,-33.1-ydj,-33.1-ydj))
# arrows(105,-33.3,115,-33.3, length = 0.1)
# box()
# dev.off()
# # make some final changes in MS Paint
# 
# #dbSendQuery(gislib, 'drop table  public.bunburyroads')
# 

jpeg('reports/Figure2.jpg', res = 150, width = 1000, height = 1400)
m <- matrix(c(2,1,1,1),2,2)
layout(m, widths=c(1, 2),heights=c(1, 2))
# layout.show(2)
par(cex = 1.2, mar = c(2,1,1,1))

plot(estbuf)
plot(estbuf, add=T, col = estbuf@data$band)
plot(est, add=T, col = 'lightgrey')
plot(bunbury, add=T, col = NA, lwd =2)             
# with(rrv, points(RESIDENCE_.1, RESIDENCE_, cex=.7,pch =16))
# with(rrv, points(EXPOSURE_L.1, EXPOSURE_L, cex=.7,pch =16))   
plot(rrv, add=T, pch = 16, cex = 0.55)   
plot(wa, add=T)      
#plot(rivers, add=T)        
#axis(1)
map.scale(ratio=F,relwidth=.15,cex=1, x = 115.79) 
box()
legend('topright',c('RRV case location','500m buffer from','Leschenault Estuary', 'and associated waterways'),pch = c(16,NA,NA,NA), pt.cex= 0.55, cex = .9, border  =c(NA,1,NA,NA), fill = c(NA,'white',NA,NA))
# inset
plot(estbuf[estbuf@data$gid2 == 20,], col = 'white')
#plot(est, add=T, col = 'lightgrey')
plot(bunbury[bunbury@data$gid2 == 20,], add=T, col = NA, lwd =2)          
# map('world','Australia', ylim=c(-45,-11),xlim=c(110.5,153))
# xdj <- .5; ydj <- 1
# polygon(c(115.5-xdj,115.9+xdj,115.9+xdj,115.5-xdj),c(-33.45+ydj,-33.45+ydj,-33.1-ydj,-33.1-ydj), col='black')
# arrows(105,-33.3,115,-33.3, length = 0.15)
box()
dev.off()
