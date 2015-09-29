
################################################################
# name:nework
require(swishdbtools)
pwdsu <- getPassword()
pwd <- getPassword()
su <- connect2postgres('pdb2.anu.edu.au','gislibrary','ivan_hanigan', p = pwdsu)
gislib <- connect2postgres('pdb2.anu.edu.au','gislibrary','gislibrary', p = pwd)
roads <-  readOGR2(hostip = 'pdb2.anu.edu.au', user = 'ivan_hanigan', db = 'gislibrary',
                   layer = 'osm.au_roads_wa', p = pwdsu)

dbSendQuery(gislib,
'SELECT highway, name, geom as the_geom
into public.bunburyroads
  FROM osm.au_roads_wa t1,
  (select * from abs_sd.wasd06 where sd_code = 510) t2
  where st_within(st_centroid(t1.geom), t2.the_geom);
alter table public.bunburyroads add column gid serial primary key;
grant all on public.bunburyroads to ivan_hanigan;
')
# because gislibrary user cant add to the geometry table readOGR wont work
# do as ivan_hanigan
sql <- fixGeom(su, "public.bunburyroads")
cat(sql)
dbSendQuery(su,sql)

bunburyroads <- readOGR2(hostip = 'pdb2.anu.edu.au', user = 'ivan_hanigan', db = 'gislibrary', layer = 'bunburyroads', p = pwdsu)

oldwd <- getwd();setwd('data')
writeOGR(bunburyroads,'bunburyroads','bunburyroads','ESRI Shapefile')
setwd(oldwd)
wa <- readOGR2(hostip = '130.56.102.41', user = 'ivan_hanigan', db = 'delphe', layer = 'abs_sd.wasd06')

#############################
jpeg('reports/Figure1.jpg', res = 150, width = 1000, height = 1400)
m <- matrix(c(2,1,1,1),2,2)
layout(m, widths=c(1.5, 2),heights=c(1.5, 2))
# layout.show(2)
par(cex = 1.2, mar = c(2,1,1,1))

plot(est, col = 'white', ylim = c(-33.33, -33.2), xlim=c(115.62,115.77))
plot(bunbury, add=T, col = 'grey', border=F)
plot(bunburyroads, add=T)
plot(wa, add=T)
# with(rrv, points(RESIDENCE_.1, RESIDENCE_, cex=.7,pch =16))
#with(rrv, points(EXPOSURE_L.1, EXPOSURE_L, cex=.7,pch =16))
#axis(1)
map.scale(ratio=F,relwidth=.15,cex=1, x = 115.732, y=-33.34)
box()
legend('topright', c('Roads','Urban zone'), lty  =c(1,NA), fill=c(NA,'grey')  ,   border=c(NA,1)   )
# inset
        plot(wa, col='lightgrey',border=F)
# map('world','Australia', ylim=c(-45,-11),xlim=c(110.5,130))
xdj <- .15; ydj <- .5
polygon(c(115.5-xdj,115.9+xdj,115.9+xdj,115.5-xdj),c(-33.45+ydj,-33.45+ydj,-33.1-ydj,-33.1-ydj))
arrows(105,-33.3,115,-33.3, length = 0.1)
box()
dev.off()
# make some final changes in MS Paint

dbSendQuery(gislib, 'drop table  public.bunburyroads')
