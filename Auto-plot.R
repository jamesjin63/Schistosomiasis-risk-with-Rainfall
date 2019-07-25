# fucntion ----------------------------------------------------------------
ersum=function(te=20190713){
  library(raster)
  library(sp)
  library(maptools)
  library(lubridate)
  library(dplyr)
  library(dismo)
  library(rgdal)
  library(leaflet)
  library(knitr)
  library(ggplot2)
  #read the raster
  load("D:/partime job/NIPDmete/Flood/CHNshp.RData")
  proj4string(CHNshp) <-  CRS("+proj=longlat +ellps=WGS84")
  
  s=stack()
  for (i in 1:10) {
    a=c("024","048","072","096","120","144","168","192","216","240")
    fname=paste0("D:/partime job/NIPDmete/Flood/",te,"08/",te,"08ER24_",a[i],".GRB2")
    er=raster(fname)
    s=stack(s,er)
  }
  a=sum(s);proj4string(a)=CRS("+proj=longlat +ellps=WGS84")
  r.m=mask(a,CHNshp)
  return(r.m)
  #writeRaster(r.m, filename=paste0(te,"Rainfall.tif"), options="INTERLEAVE=BAND", overwrite=TRUE)
}


#sch=rgdal::readOGR("D:/partime job/NIPDmete/Flood/shp/hm2.shp")

# #rainfall raster data
# rainfall=ersum(20190721)
# snail=raster("D:/partime job/NIPDmete/Flood/shp/sch.tif")
# 
# #snail raster data
# ## raster template
# rst_template <- raster(ncols = 525, nrows = 250, 
#                        crs = projection(rainfall_H), 
#                        ext = extent(rainfall_H))
# 
# ## rasterize
# rst_snail<- rasterize(sch, rst_template)
# snail=setValues(raster(rst_snail),rst_snail[])

#calc the ch and total_rainfall----------------------------------------------------------------

load("D:/partime job/NIPDmete/Flood/snail.RData")
rainfall=ersum(20190722)

#crop to the same shp
rainfall_H=mask(rainfall,sch)
snail_H=mask(snail,sch)


#set into same extend
rasterDF = raster(ext=extent(69.975, 140.025, -0.025, 
                             60.025), res=c(0.05,0.05))
snail_H=raster::resample(snail_H,rasterDF, method='bilinear')

#calc the ch and total_rainfall
totyal= overlay(rainfall_H,snail_H,fun=function(r1, r2){return(r1*r2/10000)})

#ggplot the raster----------------------------------------------------------------

#change the raster into dataframe
df.ranfall=as.data.frame(rainfall,xy=T) %>% na.omit(df.ranfall)
colnames(df.ranfall)=c("x","y","rainfall")


df.snail=as.data.frame(totyal,xy=T)%>% na.omit(df.snail)
colnames(df.snail)=c("x","y","risk")

#plot rainfall
ggplot(df.ranfall) +
  geom_raster(aes(x,y,fill=rainfall))+
  coord_quickmap()

#plot sanil
ggplot(df.snail) +
  geom_raster(aes(x,y,fill=risk))+
  coord_quickmap()

#bind two map togeher
ggplot() +
  geom_raster(data = df.ranfall , 
              aes(x = x, y = y, 
                  fill = rainfall)) + 
  geom_raster(data = df.snail, 
              aes(x = x, y = y, 
                  alpha = risk)) +  
  scale_fill_viridis_c(option = "plasma") +  
  scale_alpha(range = c(0.15, 0.65), guide = "none") +  
  ggtitle("Elevation with hillshade") +
  coord_quickmap()

#two raster
# plot looks fine
ggplot() + 
  geom_tile(data=df.ranfall, aes(x,y,colour=rainfall), fill="transparent") +
  scale_color_gradientn(colours = terrain.colors(7))+
  
  geom_raster(data=df.snail, aes(x,y,fill=risk)) +
  scale_fill_gradient(low="white", high="red", na.value="transparent") + 
  
  ggtitle("Risk with Rainfall") +
  coord_quickmap()

#with color palettes----------------------------------------------------------------
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
#https://ggplot2.tidyverse.org/reference/scale_alpha.html
#http://colorbrewer2.org/#type=sequential&scheme=Reds&n=8
# Load


# set into diffrent grades
####risk
df.snail_level=df.snail %>% 
  mutate(level=cut(risk,breaks=c(-1,0.2,1,5,500)),
         level=factor(level,labels = c("Level 4","Level 3","Level 2","Level 1")))

ggplot() + 
  geom_raster(data=df.snail_level, aes(x,y,fill=level)) +
  scale_fill_manual(values=c(
    "#fee0d2","#FC8D59", "#EF6548", "#D7301F" ))+
  ggtitle("Risk with Rainfall") +
  coord_quickmap()

####Rainfall
df.rainfall_level=df.ranfall %>% 
  mutate(Rcut=cut(rainfall,breaks=c(-1,10,25,50,100,250,900)),
         Rcut=factor(Rcut,labels = c("0-10","10-25","25-50",
                                     "50-100","100-250",">250")))
ggplot() + 
  geom_raster(data=df.rainfall_level, aes(x,y,fill=Rcut)) +
  scale_fill_manual(values=c("#9ECAE1" ,"#6BAED6", "#4292C6" ,"#2171B5" ,"#08519C","#08306B"),
                    na.value="transparent") +
  ggtitle("Risk with Rainfall") +
  coord_quickmap()

#two raster
# plot looks fine
ggplot() + 
  geom_tile(data=df.rainfall_level, aes(x,y,colour=Rcut), fill="transparent") +
  scale_colour_manual(name="Cumulative Rainfall",
                      values=c("#9ECAE1" ,"#6BAED6", "#4292C6" ,"#2171B5" ,"#08519C","#08306B"),
                      na.value="transparent") +
  
  geom_raster(data=df.snail_level, aes(x,y,fill=level)) +
  scale_fill_manual(name="Sch.Risk",
                    values=c("#FDBB84","#FC8D59", "#EF6548", "#D7301F" )) + 
  
  ggtitle("Risk with Rainfall") +
  coord_quickmap()

