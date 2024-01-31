library(sf)
library(tidyverse)
library(leaflet)
library(ggrepel)
library(GWmodel)
library(gstat) 
library(openxlsx)
library(units)

setwd("C:/Users/fredr/Dropbox/B_PROJETOS_PESQUISA_GV/SPMA_SUBCENTERS/SPMA_employment_centralities")

zonas_OD <- read_sf("zonasOD_allperiods.gpkg")
CBD <- read_sf("CBD_1.shp")
st_crs(CBD)
st_crs(zonas_OD)
CBD<-st_transform(CBD, crs = st_crs(31983))
st_crs(CBD)
names(zonas_OD)

zonas_OD$pop_density97 <- zonas_OD$od97_pop97/zonas_OD$Area_ha
zonas_OD$job_density97 <- zonas_OD$od97_emp97/zonas_OD$Area_ha
zonas_OD$job_perctg97 <- zonas_OD$od97_emp97/sum(zonas_OD$od97_emp97)*100
zonas_OD$job_pop_ratio97<- zonas_OD$od97_emp97/zonas_OD$od97_emp97

zonas_OD$pop_density07 <- zonas_OD$pop07/zonas_OD$Area_ha
zonas_OD$job_density07 <- zonas_OD$emp07/zonas_OD$Area_ha
zonas_OD$job_perctg07 <- zonas_OD$emp07/sum(zonas_OD$emp07)*100
zonas_OD$job_pop_ratio07<- zonas_OD$emp07/zonas_OD$pop07

zonas_OD$pop_density17 <- zonas_OD$pop17/zonas_OD$Area_ha
zonas_OD$job_density17 <- zonas_OD$emp17/zonas_OD$Area_ha
zonas_OD$job_perctg17 <- zonas_OD$emp17/sum(zonas_OD$emp17)*100
zonas_OD$job_pop_ratio17<- zonas_OD$emp17/zonas_OD$pop17

max_jobs_97 <- max(zonas_OD$od97_emp97)
max_jobs_07 <- max(zonas_OD$emp07)
max_jobs_17 <- max(zonas_OD$emp17)

#CALCULATING MEANS

mean(zonas_OD$pop_density97)
mean(zonas_OD$job_density97)
mean(zonas_OD$pop_density07)
mean(zonas_OD$job_density07)
mean(zonas_OD$pop_density17)
mean(zonas_OD$job_density17)


sorted_values_emp_97 <- sort(unique(zonas_OD$od97_emp97), decreasing = TRUE)
selected_values_emp_97 <- sorted_values_emp_97[2:5]
sum_selected_values_emp_97 <- sum(selected_values_emp_97)
prim_index_abs_97 <- max_jobs_97/sum_selected_values_emp_97

sorted_values_emp_07 <- sort(unique(zonas_OD$emp07), decreasing = TRUE)
selected_values_emp_07 <- sorted_values_emp_07[2:5]
sum_selected_values_emp_07 <- sum(selected_values_emp_07)
prim_index_abs_07 <- max_jobs_07/sum_selected_values_emp_07

sorted_values_emp_17 <- sort(unique(zonas_OD$emp17), decreasing = TRUE)
selected_values_emp_17 <- sorted_values_emp_17[2:5]
sum_selected_values_emp_17 <- sum(selected_values_emp_17)
prim_index_abs_17 <- max_jobs_17/sum_selected_values_emp_17

value_prim_abs <- c(prim_index_abs_97,prim_index_abs_07,prim_index_abs_17)
years <- c(1997, 2007, 2017)

plot(x = years, y = value_prim_abs, type = "o", col = "blue", pch = 16, main = "Primacy Index Absolute Jobs", xlab = "Year", ylab = "Values",xaxt = "n",ylim = c(0.2, 0.4))+axis(1, at = years)+
  text(years, value_prim_abs, labels = round(value_prim_abs, 2), pos = 1, cex = 1, col = "red")


# scatterplot when saving them...use export and save width 1400 and height 650 to fit nicely in the doc#

ggplot()+geom_point(data=zonas_OD, aes(x=job_perctg97, y=job_density97),size=2)+geom_text_repel(data=subset(zonas_OD, job_perctg97>0.5 | job_density97>550), aes(x=job_perctg97, y=job_density97+100, label=NomeZona97),size=6.5) +ylab("Job Density 1997")+xlab("Share of Metropolitan Jobs 1997")+ggtitle(expression(italic("Employment Density VS Employment Share in 1997"),size=20)) +xlim(0,1.75)+ylim(0,1900) +theme(plot.title = element_text(hjust = 0.1, vjust = -10,color = "gray45",size=20),axis.title = element_text(size = 20),  axis.text = element_text(size = 15),legend.position = "none")   
ggplot()+geom_point(data=zonas_OD, aes(x=job_perctg07, y=job_density07),size=2)+geom_text_repel(data=subset(zonas_OD, job_perctg07>0.5 | job_density07>550), aes(x=job_perctg07, y=job_density07+100, label=NomeZona97),size=6.5) +ylab("Job Density 2007")+xlab("Share of Metropolitan Jobs 2007")+ggtitle(expression(italic("Employment Density VS Employment Share in 2007"),size=20)) +xlim(0,1.75)+ylim(0,1900) +theme(plot.title = element_text(hjust = 0.1, vjust = -10,color = "gray45",size=20),axis.title = element_text(size = 20),  axis.text = element_text(size = 15),legend.position = "none")  
ggplot()+geom_point(data=zonas_OD, aes(x=job_perctg17, y=job_density17),size=2)+geom_text_repel(data=subset(zonas_OD, job_perctg17>0.5 | job_density17>550), aes(x=job_perctg17, y=job_density17+100, label=NomeZona97),size=6.5) +ylab("Job Density 2017")+xlab("Share of Metropolitan Jobs 2017")+ggtitle(expression(italic("Employment Density VS Employment Share in 2017"),size=20)) +xlim(0,1.75)+ylim(0,1900) +theme(plot.title = element_text(hjust = 0.1, vjust = -10,color = "gray45",size=20),axis.title = element_text(size = 20),  axis.text = element_text(size = 15),legend.position = "none")    

# creating a dataset without zeros and the log of job density#

zonas_OD_temp97 <- zonas_OD %>% dplyr::select(Area_ha,job_density97,pop_density97) %>% 
  filter(job_density97>0) %>% 
  filter(pop_density97>0) %>% 
  mutate(pop_density97_log=log(pop_density97),job_density97_log=log(job_density97))

zonas_OD_temp07 <- zonas_OD %>% dplyr::select(Area_ha,job_density07,pop_density07) %>% 
  filter(job_density07>0) %>% 
  filter(pop_density07>0) %>% 
  mutate(pop_density07_log=log(pop_density07),job_density07_log=log(job_density07))

zonas_OD_temp17 <- zonas_OD %>% dplyr::select(Area_ha,job_density17,pop_density17) %>% 
  filter(job_density17>0) %>% 
  filter(pop_density17>0) %>% 
  mutate(pop_density17_log=log(pop_density17),job_density17_log=log(job_density17))

# converting to centroids #
zonas_OD_P_97 <- st_centroid(zonas_OD_temp97)
plot (zonas_OD_P_97)

zonas_OD_P_07 <- st_centroid(zonas_OD_temp07)
plot (zonas_OD_P_07)

zonas_OD_P_17 <- st_centroid(zonas_OD_temp17)
plot (zonas_OD_P_17)

#calculating distance to each centroid and the CBD #

zonas_OD_P_97$distances <- st_distance(zonas_OD_P_97, CBD)
zonas_OD_P_07$distances <- st_distance(zonas_OD_P_07, CBD)
zonas_OD_P_17$distances <- st_distance(zonas_OD_P_17, CBD)

# transforming into a spatial dataframe #
zonas_OD_P_97_sp <- as(zonas_OD_P_97,"Spatial")
zonas_OD_P_07_sp <- as(zonas_OD_P_07,"Spatial")
zonas_OD_P_17_sp <- as(zonas_OD_P_17,"Spatial")

hist(zonas_OD_P_97_sp$distances,col="black",xlab="Distance from the CBD (meters)",main="Histogram with the distances from Zones Centroids and the CBD 1997")
hist(zonas_OD_P_07_sp$distances,col="black",xlab="Distance from the CBD (meters)",main="Histogram with the distances from Zones Centroids and the CBD 2007")
hist(zonas_OD_P_17_sp$distances,col="black",xlab="Distance from the CBD (meters)",main="Histogram with the distances from Zones Centroids and the CBD 2017")

linmod<-lm(zonas_OD_P_97_sp$job_density97_log~zonas_OD_P_97_sp$distances)
plot(zonas_OD_P_97_sp$job_density97_log~zonas_OD_P_97_sp$distances,xlab="Distance from the CBD (meters)",ylab="Job Density (log)")
lines(stats::lowess(zonas_OD_P_97_sp$job_density97_log~zonas_OD_P_97_sp$distances), col = "green", lwd=2)

linmod<-lm(zonas_OD_P_07_sp$job_density07_log~zonas_OD_P_07_sp$distances)
plot(zonas_OD_P_07_sp$job_density07_log~zonas_OD_P_07_sp$distances,xlab="Distance from the CBD (meters)",ylab="Job Density (log)")
lines(stats::lowess(zonas_OD_P_07_sp$job_density07_log~zonas_OD_P_07_sp$distances), col = "red", lwd=2)

linmod<-lm(zonas_OD_P_17_sp$job_density17_log~zonas_OD_P_17_sp$distances)
plot(zonas_OD_P_17_sp$job_density17_log~zonas_OD_P_17_sp$distances,xlab="Distance from the CBD (meters)",ylab="Job Density (log)")
lines(stats::lowess(zonas_OD_P_17_sp$job_density17_log~zonas_OD_P_17_sp$distances), col = "blue", lwd=2)

lines(stats::lowess(zonas_OD_P_97_sp$job_density97_log~zonas_OD_P_97_sp$distances), col = "green", lwd=2)
lines(stats::lowess(zonas_OD_P_07_sp$job_density07_log~zonas_OD_P_07_sp$distances), col = "red", lwd=2)
lines(stats::lowess(zonas_OD_P_17_sp$job_density17_log~zonas_OD_P_17_sp$distances), col = "blue", lwd=2)

# calculating the bandwidth for the GWR #
bw.gwr(job_density97_log~distances+pop_density97_log,data=zonas_OD_P_97_sp, kernel="gaussian",approach = "CV")
bw.gwr(job_density07_log~distances+pop_density07_log,data=zonas_OD_P_07_sp, kernel="gaussian",approach = "CV")
bw.gwr(job_density17_log~distances+pop_density17_log,data=zonas_OD_P_17_sp, kernel="gaussian",approach = "CV")

# calculating GWR model #
gwr_97 <- gwr.basic(job_density97_log~distances+pop_density97_log,data=zonas_OD_P_97_sp,kernel="gaussian",bw=6479.21)
gwr_97_map <- gwr_97$SDF
gwr_97_map <- spTransform(gwr_97_map, CRS("+proj=longlat +datum=WGS84"))
gwr_97_map@data[,c("long","lat")] <- coordinates(gwr_97_map)

subcenters_97 <- gwr_97_map[gwr_97_map$Stud_residual > 1.96, ]
subcenters_97_sf <- st_as_sf(subcenters_97)
subcenters_97_sf <- st_transform(subcenters_97_sf, st_crs(zonas_OD))
subcenters_97_OD <- st_intersection(zonas_OD, subcenters_97_sf)

gwr_07 <- gwr.basic(job_density07_log~distances+pop_density07_log,data=zonas_OD_P_07_sp,kernel="gaussian",bw=6479.21)
gwr_07_map <- gwr_07$SDF
gwr_07_map <- spTransform(gwr_07_map, CRS("+proj=longlat +datum=WGS84"))
gwr_07_map@data[,c("long","lat")] <- coordinates(gwr_07_map)

subcenters_07 <- gwr_07_map[gwr_07_map$Stud_residual > 1.96, ]
subcenters_07_sf <- st_as_sf(subcenters_07)
subcenters_07_sf <- st_transform(subcenters_07_sf, st_crs(zonas_OD))
subcenters_07_OD <- st_intersection(zonas_OD, subcenters_07_sf)

gwr_17 <- gwr.basic(job_density17_log~distances+pop_density17_log,data=zonas_OD_P_17_sp,kernel="gaussian",bw=6479.21)
gwr_17_map <- gwr_17$SDF
gwr_17_map <- spTransform(gwr_17_map, CRS("+proj=longlat +datum=WGS84"))
gwr_17_map@data[,c("long","lat")] <- coordinates(gwr_17_map)

subcenters_17 <- gwr_17_map[gwr_17_map$Stud_residual > 1.96, ]
subcenters_17_sf <- st_as_sf(subcenters_17)
subcenters_17_sf <- st_transform(subcenters_17_sf, st_crs(zonas_OD))
subcenters_17_OD <- st_intersection(zonas_OD, subcenters_17_sf)

class(zonas_OD)
zonas_OD_xy <- st_zm(zonas_OD)
zonas_OD_degrees <- st_transform(zonas_OD_xy, CRS("+proj=longlat +datum=WGS84"))

#Plotting subcenters according to statistical significance - 99.9 = 3.29; 99 = 2.58; 95 = 1.96; 90 = 1.65; 80 = 1.28#
leaflet() %>% addProviderTiles("CartoDB.PositronNoLabels") %>% addCircleMarkers(data=gwr_97_map[gwr_97_map$Stud_residual>1.96,],lat=~lat,lng=~long,radius = 8,color = "darkred",stroke = FALSE, fillOpacity = 1) %>% 
  addPolygons(data=zonas_OD_degrees , color = "gray", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE))%>% 
  addLabelOnlyMarkers(data = subcenters_97_OD, lat = ~lat, lng = ~long, label = ~as.character(NomeZona97), labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, style = list("font-size" = "20px"))) %>% 
  addControl("Subcenters 1997", position = "topright")

leaflet() %>% addProviderTiles("CartoDB.PositronNoLabels") %>% addCircleMarkers(data=gwr_07_map[gwr_07_map$Stud_residual>1.96,],lat=~lat,lng=~long,radius = 8,color = "darkred",stroke = FALSE, fillOpacity = 1) %>% 
  addPolygons(data=zonas_OD_degrees , color = "gray", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE))%>% 
  addLabelOnlyMarkers(data = subcenters_07_OD, lat = ~lat, lng = ~long, label = ~as.character(NomeZona97), labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, style = list("font-size" = "20px"))) %>% 
  addControl("Subcenters 2007", position = "topright")

leaflet() %>% addProviderTiles("CartoDB.PositronNoLabels") %>% addCircleMarkers(data=gwr_17_map[gwr_17_map$Stud_residual>1.96,],lat=~lat,lng=~long,radius = 8,color = "darkred",stroke = FALSE, fillOpacity = 1) %>% 
  addPolygons(data=zonas_OD_degrees , color = "gray", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE))%>% 
  addLabelOnlyMarkers(data = subcenters_17_OD, lat = ~lat, lng = ~long, label = ~as.character(NomeZona97), labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, style = list("font-size" = "20px"))) %>% 
  addControl("Subcenters 2017", position = "topright")

# maps when saving them...use export and save width 1400 and height 650 to fit nicely in the doc#
ggplot() +
  geom_sf(data = zonas_OD_degrees,color="grey40", fill = "white") + geom_sf(data = subcenters_97_OD,size = 4)+ggtitle(expression(italic("Subcenters in 1997"),size=20)) +theme(plot.title = element_text(hjust = 0.1, vjust = -10,color = "gray45",size=20),axis.title = element_text(size = 20),  axis.text = element_text(size = 15),legend.position = "none")

ggplot() +
  geom_sf(data = zonas_OD_degrees,color="grey40", fill = "white") + geom_sf(data = subcenters_07_OD,size = 4)+ggtitle(expression(italic("Subcenters in 2007"),size=20)) +theme(plot.title = element_text(hjust = 0.1, vjust = -10,color = "gray45",size=20),axis.title = element_text(size = 20),  axis.text = element_text(size = 15),legend.position = "none")

ggplot() +
  geom_sf(data = zonas_OD_degrees,color="grey40", fill = "white") + geom_sf(data = subcenters_17_OD,size = 4)+ggtitle(expression(italic("Subcenters in 2017"),size=20)) +theme(plot.title = element_text(hjust = 0.1, vjust = -10,color = "gray45",size=20),axis.title = element_text(size = 20),  axis.text = element_text(size = 15),legend.position = "none")
