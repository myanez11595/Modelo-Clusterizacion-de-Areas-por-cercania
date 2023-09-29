library(sf)
library(sp)
library(rgdal)  
library(stats)
library(reshape2)
manzanas=st_read("D:/Entregable/APP completa ejercicios/Aplicacion_AES/Aplicacion_AES/Ejercicio Condado/SECTOR_MANZANA_A.shp") %>% st_as_sf()
ca04=st_read("D:/Entregable/APP completa ejercicios/Aplicacion_AES/Aplicacion_AES/Ejercicio Condado/CA04_A.shp") %>% st_as_sf()
source("D:/MODELO PRIORIZADOS/st_centroid_within_poly.r")
manzanas$priorizado = ifelse(is.na(manzanas$priorizado) == TRUE, 0, manzanas$priorizado)
man_pri=manzanas[manzanas$priorizado=="ALTA",]
CA04_PRI=st_intersection(ca04,man_pri)
CA04_PRI=CA04_PRI[1:200,]
CA04_PRI=CA04_PRI[,c(1:14,28)]
CA04_PRI=CA04_PRI[order(CA04_PRI$n_edif),]
cent_ca04=st_centroid_within_poly(CA04_PRI)
cent_ca04_1=cent_ca04 %>% as_Spatial()
cent_ca04_1$X <-  coordinates(cent_ca04_1)[,1]
cent_ca04_1$Y <-  coordinates(cent_ca04_1)[,2]
cent_ca04_1=cent_ca04_1 %>% as_data_frame()
cent_ca04_1=cent_ca04_1[,c(1,15,16)]
#cent_ca04=merge(cent_ca04,cent_ca04_1,by.x="pk",by.y="pk")
cent_ca04_1=cent_ca04_1[,c(2,3)]
mat=dist(cent_ca04_1)
x=as.matrix(mat)
colnames(x) <- cent_ca04$pk
rownames(x) <- cent_ca04$pk
x=melt(as.matrix(x), varnames = c("row", "col"))
x=x[x$value!=0,]
CA04_PRI_PRE=CA04_PRI
i=nrow(CA04_PRI)
count=0
CA04_PRI_PRE$index=1:nrow(CA04_PRI_PRE)
while (i>0) {
  y=0
  edif=""
  viv=y
  while(viv<14 || viv<=17){
    if(is.null(CA04_PRI_PRE$id_ae)){
      i=i
      subs=x[x$row == CA04_PRI$pk[i],]}else{
        i=CA04_PRI_PRE$index[CA04_PRI_PRE$pk==z]
        subs=x[x$row == z,]}
    z=subs$col[subs$value==min(subs$value)]
    viv=y+CA04_PRI_PRE$n_viv[i]+CA04_PRI_PRE$n_viv[CA04_PRI_PRE$pk==z]
    y=viv-CA04_PRI_PRE$n_viv[CA04_PRI_PRE$pk==z]
    if(y<=17){}else if(y>17){
      int=2
      sal=12
      viv=y
      rep=viv
      while(int<=10  || y>17 || sal>2){
        z=subs$col[subs$value==subs$value[order(subs$value)][int]]
        z=z[!is.na(z)]
        viv=rep+CA04_PRI_PRE$n_viv[CA04_PRI_PRE$pk==z]
        int=int+1
        sal=sal-int 
        if(is.na(CA04_PRI_PRE$id_ae[CA04_PRI_PRE$pk==z])){
        CA04_PRI_PRE$id_ae[CA04_PRI$pk==z]=1+count
        x=x[x$col!=CA04_PRI$pk[CA04_PRI$pk==z],]
        CA04_PRI=CA04_PRI[CA04_PRI$pk!=z,]}
        if(sal==2){
          CA04_PRI_PRE$id_ae[CA04_PRI$n_viv==i]="no_ae"
          y=17
        }
        y=viv
      }
    }
    #edif=c(edif,CA04_PRI$pk[i],z)
    #edif=edif[edif!=""]
    #edif=edif[!is.na(edif)]
    if(is.na(CA04_PRI_PRE$id_ae[CA04_PRI_PRE$pk==z]) || is.null(CA04_PRI_PRE$id_ae)){
      CA04_PRI_PRE$id_ae[i]=1+count
      x=x[x$col!=CA04_PRI_PRE$pk[i],]
      CA04_PRI=CA04_PRI[CA04_PRI$pk!=CA04_PRI_PRE$pk[i],]
    }

    i=i-1
  }
  count=CA04_PRI_PRE$id_ae[i+1]
}

names(CA04_PRI_PRE)
CA04_PRI_PRE=CA04_PRI_PRE[,c(1,18,2:17)]
prueba=x[x$col==430150,]
plot(CA04_PRI_PRE)
