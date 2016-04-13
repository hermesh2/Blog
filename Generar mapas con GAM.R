library("haven")
library("maptools")
library("dplyr")
library("readxl")
library("spacetime")
library("colorRamps")
library("gam")

#Descargo y leo los shapefiles y datos
te=tempdir()
download.file(url = 
                "http://www.ecuadorencifras.gob.ec/documentos/web-inec/Cartografia/2015/Clasificador_Geografico/2012/SHP.zip",
              destfile = paste0(te,"\\","te.zip"),mode = "wb")

download.file(url = 
                "http://www.ecuadorencifras.gob.ec/documentos/datos/investigaciones_sociales/Camas_Egresos_Hospitalarios/Egresos/Egresos%20spss/bdd_egresos_hos_2014_spss.zip",
              destfile = paste0(te,"\\","te2.zip"),mode = "wb")

download.file(url = 
                "http://www.ecuadorencifras.gob.ec/wp-content/descargas/Boletines/Proyecciones_poblacionales_cantonales/proyeccion_cantonal_total_2010-2020.xlsx",
              destfile = paste0(te,"\\","rel.xlsx"),mode = "wb")

unzip(paste0(te,"\\","te.zip"),exdir=te)
unzip(paste0(te,"\\","te2.zip"),exdir=te)

#Leer los Shapesfile
c("nxprovincias","nxcantones","nxparroquias")
ecuador=
  lapply(c("nxprovincias","nxcantones","nxparroquias"),function(x)
    readShapeSpatial(paste0(te,"\\","SHP","\\",x,".shp"))
  )



ecuad=ecuador$provincias
names(ecuador)=c("provincias","cantones","parroquias")

egresos=read_sav(paste0(te,"\\","Base_Egresos_Hospitalarios_2014.sav"))
proyecciones=read_excel(paste0(te,"\\","rel.xlsx"),skip=2)


rm(ecuad,te)

#Extraigo los shapefile del Ecuador Continental
par=ecuador$parroquias[ecuador$parroquias$DPA_PROVIN!=20,]
par$DPA_PARROQ=droplevels(par$DPA_PARROQ) #(es para borrar a Galapago como nivel en el dataframe asociado)

#Intervalos de tiempos usados
tim=as.Date(levels(cut(egresos$fecha_egr,"days")))

#Creo un factor que es la interacci?n de la parroquia con los dias del a?o
par %>% names
egresos$rel=
  interaction(
    par$DPA_PARROQ[
    match(as.character(egresos$parr_res),as.character(par$DPA_PARROQ))
    ],
    tim
    )




#Calculo la tasa por egresado de Dengue por d?a y parroquia
val=tapply(egresos$causa3=="A90",egresos$rel,function(x) sum(x)/length(x))


#La tasas NA hacer valer 0
val[is.na(val)]=0
val=as.numeric(val)

#Crear un objeto de tiempo espacio temporal llamado rel
rel=STFDF(par,tim,data.frame(val=val))

#Crear un data frame con las coordenas de cada parroquia, dia y tasa
exp=expand.grid(sp=1:nrow(rel@sp),tm=1:365)
cc=coordinates(rel@sp)[exp$sp,]
rownames(cc)=1:nrow(cc)
exp=cbind(exp,as.data.frame(cc))
exp$times=rel@time[exp$tm,]
exp$val=val

#Usar modelo GAM con LOESS sobre las coordenadas espaciales y los dias, 
#usar una familia quasi-poisson para la regresi?n (por defecto usa la familia gaussiana)
fit=gam(val~lo(V1)+lo(V2)+lo(tm),data = exp,family ="quasipoisson" )

#Guardar los valores ajustado por el modelo GAM en el data.frame exp
exp$val_smooth=fitted(fit)

#Guardar los valores ajustado por el modelo GAM en el objeto espacio temporal rel
rel@data$val_smooth=exp$val_smooth

#Graficar, Advertencia. puede tardar una hora tener las 365 gr?fica
jpeg("ecuador%03d.jpg",width = 271*4,height = 271*4)
stplot(rel[,,2],
       main="Tasa Suavizada espacio-temporalmente del caso de Dengue\nSuavizaci?n Kernel",
       col=NA,
       col.regions=matlab.like2(10000),
       sp.layout=
         list(
         list("sp.text",coordinates(ecuador$provincias[-25,]),ecuador$provincias$DPA_DESPRO[-25],cex=1.5,col="black"),
         list("sp.polygons",ecuador$provincias,col="black",first = FALSE)),
       animate = 1,
       do.repeat=F,cuts=1000
         )
       
dev.off()


#Para unir las gr?ficas en un video se puede usar varias herramienta, en mi 
#caso use un programa de comando ffmpeg