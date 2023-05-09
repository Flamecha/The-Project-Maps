library(SpatialEpi)
library(readxl)
library(tidyverse)
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
IMR1=read_excel("~/2018 Reported Infant Death.xlsx",skip = 1)
head(IMR1)
##SMR by location
f=aggregate(x=IMR1$Cases,by=list(Location=IMR1$`Place of recorded death`),FUN=sum)
head(f)
names(f)=c("id","X")
pop=IMR1$`Population (Total Infant Born)`
cases=IMR1$Cases
n.strata=2
Exp=expected(pop,cases,n.strata)
f$Exp=Exp[match(f$id,unique(IMR1$`Place of recorded death`))]
f$SMR=f$X/f$E
f
##SMR by District/Region
d=aggregate(x=IMR1$Cases,by=list(Districts=IMR1$Districts),FUN=sum)
M=aggregate(x=IMR1$`Gender (M)`,by=list(Districts=IMR1$Districts),FUN=sum)
head(d)
names(d)=c("id","Y")
names(M)=c("Districts","Male_sex")
pop1=IMR1$`Population (Total Infant Born)`
cases1=IMR1$Cases
n.strata1=3.25
E=expected(pop1,cases1,n.strata1)
d$E=E[match(d$id,unique(IMR1$Districts))]
d=merge(d,M,by.x="id",by.y="Districts")
d$SMR=d$Y/d$E
head(d)
District = raster::getData("GADM", country = "Botswana", level = 2)
library(sp)
rown=sapply(slot(Districts, "polygons"), function(x) slot(x, "ID"))
rownames(d)=rown
identical(rownames(d), getSpPPolygonsIDSlots(Districts))
map = SpatialPolygonsDataFrame(Districts,d,match.ID = TRUE)
head(map@data)
library(leaflet)
l=leaflet(map)%>%
  addTiles()
pal=colorNumeric(palette="YlOrRd",domain=map$SMR)
l %>% addPolygons(color="grey",weight=1,fillColor=~pal(SMR),
                  fillOpacity=0.5) %>% 
  addLegend(pal=pal,values=~SMR,opacity=0.5,title="SMR",
            position="bottomright")
labels=sprintf("<strong> %s </strong> <br/> Observed: %s <br/> Expected: %s <br/> 
               Male sex proportion: %s <br/> SMR: %s",
               map$id,map$Y,round(map$E,2),map$Male_sex,round(map$SMR,2)) %>%
  lapply(htmltools::HTML)
l %>% addPolygons(color="grey",weight=1,fillColor=~pal(SMR),fillOpacity=0.5,
                  highlightOptions=highlightOptions(weight=4),label=labels,
                  labelOptions=labelOptions(style=list("font-weight"="normal",
                                                       padding="3px 8px"),
                                            textsize="15px",
                                            direction="auto")) %>%
  addLegend(pal=pal,values=~SMR,opacity=0.5,title="SMR",
            position="bottomright")
library(spdep)
library(INLA)
nb=poly2nb(map)
head(nb)
nb2INLA("map.adj",nb)
g=inla.read.graph("map.adj")
map$re_u=1:nrow(map@data)
map$re_v=1:nrow(map@data)
formula=Y~Male_sex+f(re_u,model="besag",graph=g)+f(re_v,model="iid")
res=inla(formula,family="poisson",data=map@data,E=E,
         control.predictor=list(compute=T))
summary(res)
head(res$summary.fitted.values)
map$RR=res$summary.fitted.values[,"mean"]
map$LL=res$summary.fitted.values[,"0.025quant"]
map$UL=res$summary.fitted.values[,"0.975quant"]
pal=colorNumeric(palette = "YlOrRd",domain=map$RR)
labels=sprintf("<strong> %s </strong> <br/> Observed: %s <br/> Expected: %s <br/>
Male sex proportion: %s <br/> SMR: %s <br/> RR: %s (%s, %s)",
               map$id, map$Y, round(map$E, 2), map$Male_sex, round(map$SMR, 2),
               round(map$RR, 2), round(map$LL, 2), round(map$UL, 2)) %>%
  lapply(htmltools::HTML)
leaflet(map) %>% addTiles() %>%
  addPolygons(color = "grey", weight = 1, fillColor = ~pal(RR), fillOpacity = 0.5,
              highlightOptions = highlightOptions(weight = 4), label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~RR, opacity = 0.5, title = "RR",
            position = "bottomright")
pal=colorNumeric(palette = "YlOrRd", domain = map$SMR)
leaflet(map) %>% addTiles() %>%
  addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
              highlightOptions = highlightOptions(weight = 4), label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "15px", direction = "auto")) %>%
  
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR",
            position = "bottomright")
