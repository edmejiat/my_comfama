library("data.table")

empresas=fread(osPathJoin(datosPath,"MicrosPequenas_InfoCompleta_Abr21.csv"))
ventas=fread(osPathJoin(datosPath,"ventas_corregido.csv"))



############ Cambio en los nombres ############ 
nombres=names(empresas)

nombres=gsub("[[:space:]]", "", nombres)
nombres=gsub("[][!#$%()*,.:;<=>@^_`|~.{}?¿]", "",nombres)

nombres=rm_accent(nombres)

names(empresas)=nombres


nombres=names(ventas)

nombres=gsub("[[:space:]]", "", nombres)
nombres=gsub("[][!#$%()*,.:;<=>@^_`/|~.{}?¿]", "",nombres)

nombres=rm_accent(nombres)

names(ventas)=nombres

############  Eleccion de los grupos  ############ 

morosos=empresas[Tienemoraenaportes=="Si"]

comercio_automotores=empresas[Tienemoraenaportes=="No" & Seccioneconomica=="G_Comercio y Rep. Automotores"]

recientes_19.20.21=empresas[Tienemoraenaportes=="No" & substr(Fechadeafiliacion,1,4)%in%c("2019","2020","2021")]

antiguos=empresas[Tienemoraenaportes=="No" & !substr(Fechadeafiliacion,1,4)%in%c("2020","2021")]

############  comercio_automotores  ############

#Es el grupo mas grande, vamos a ver diferencias entre 2019 y 2020

con_ventas=merge(comercio_automotores, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")


ventas_year=con_ventas[,.N,by=c("NITsinDV","Ano")]
ventas_year=dcast(ventas_year, NITsinDV ~ Ano , value.var = "N")
ventas_year[is.na(ventas_year)] <- 0
ventas_year[,variacion:=ifelse(`2019`==0,100,((`2020`-`2019`)/`2019`)*100)]

fwrite(ventas_year,osPathJoin(resultadosPath,"grupo1_ventas_year.csv"))

no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")
no_morosos=no_morosos[,.N,by=c("Seccioneconomica","Ano")]
no_morososF=dcast(no_morosos, Seccioneconomica   ~ Ano , value.var = "N")
fwrite(no_morososF,osPathJoin(resultadosPath,"todos_ventas_year.csv"),sep=";")


construccion=empresas[Seccioneconomica=="F_Construcción" & Tienemoraenaportes=="No"]
construccion=merge(construccion, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

construccion2=construccion[,.N,by=c("NITsinDV","Ano")]
construccion2=dcast(construccion2, NITsinDV   ~ Ano , value.var = "N")
construccion2[is.na(construccion2)] <- 0

construccion3=construccion2[,nuevos:=`2020`-`2019`]
construccion3[,.N,by=nuevos][order(nuevos)]

construccion2$`2019`

names(ventas)

construccion2[,.N,by=`2019`]

############ Por Capacidad  ############

no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

no_morosos2=no_morosos[,.N,by=c("Capacidad","Ano")]
no_morosos3=dcast(no_morosos2, Capacidad   ~ Ano , value.var = "N")
fwrite(no_morosos3,osPathJoin(resultadosPath,"capcidad.csv"),sep=";")

############ Capacidad Salud y Bienestar  ############

no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

no_morosos=no_morosos[Capacidad=="Salud y Bienestar"]
no_morosos=no_morosos[,.N,by=c("Ano","LineaDeProducto")]
no_morososF=dcast(no_morosos, LineaDeProducto  ~ Ano , value.var = "N")
no_morososF[is.na(no_morososF)] <- 0
no_morososF=no_morososF[,nuevos:=`2020`-`2019`]


no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

no_morosos=no_morosos[Capacidad=="Salud y Bienestar"]
no_morosos=no_morosos[,.N,by=c("Ano","MesDeEntrega")]
no_morososF=dcast(no_morosos, MesDeEntrega  ~ Ano , value.var = "N")

############ Que paso en abril  ############

no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

no_morosos=no_morosos[Capacidad=="Salud y Bienestar" & MesDeEntrega==4]
no_morosos=no_morosos[,.N,by=c("Ano","Programa")]
no_morososF=dcast(no_morosos, Programa  ~ Ano , value.var = "N")
no_morososF=no_morososF[,nuevos:=`2020`-`2019`]
no_morososF[,variacion:=ifelse(`2019`==0,100,((`2020`-`2019`)/`2019`)*100)]



############ Que paso en abril  ############

no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")
no_morosos=no_morosos[ substr(Fechadeafiliacion,1,4)%in%c("2015","2016","2017","2018","2019")]

no_morosos=no_morosos[,.N,by=c("Ano","Programa")]
no_morososF=dcast(no_morosos, Programa  ~ Ano , value.var = "N")


no_morosos2=empresas[Tienemoraenaportes=="No"]
no_morosos2=merge(no_morosos2, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")
no_morosos2=no_morosos2[ !substr(Fechadeafiliacion,1,4)%in%c("2015","2016","2017","2018","2019")]

no_morosos2=no_morosos2[,.N,by=c("Ano","Programa")]
no_morososF2=dcast(no_morosos2, Programa  ~ Ano , value.var = "N")


fwrite(no_morososF2,osPathJoin(resultadosPath,"antes5años.csv"),sep=";")


no_morosos=no_morosos[,m_fecha:=paste(AnoDeEntrega,MesDeEntrega,DiaDeEntrega,sep="-")]

no_morosos=no_morosos[,m_fecha2:=as.Date(m_fecha,format="%Y-%m-%d")]

no_morosos[,minimo:=min(m_fecha2),by="NITsinDV"]
no_morosos[,maximo:=max(m_fecha2),by="NITsinDV"]

fwrite(no_morososF,osPathJoin(resultadosPath,"mes.csv"),sep=";")
