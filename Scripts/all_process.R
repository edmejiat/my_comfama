########## Lectura de las tablas ########## 

empresas=fread(osPathJoin(datosPath,"MicrosPequenas_InfoCompleta_Abr21.csv"))
ventas=fread(osPathJoin(datosPath,"ventas_corregido.csv"))

########## Correccion de los nombres ########## 

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

rm(nombres)

########## Segmentos ########## 

#Segmento 1:
#Empresas de la sección Económica  afiliadas a 
#partir de 2015. Este sector en particular, no presentó disminución
#en ventas del 2019 al 2020. En total se tienen 154 Microempresas.

no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos.con.ventas=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

segmento1=no_morosos.con.ventas[Seccioneconomica=="F_Construcción"]

segmento1=segmento1[,.N,by=c("Ano","Sector.x","NITsinDV")]
segmento1=dcast(segmento1, NITsinDV+Sector.x   ~ Ano , value.var = "N")
segmento1[is.na(segmento1)] <- 0

#Segmento 2:

#Empresas que usaron las capacidades de Salud y Bienestar,
#y que se afiliaron antes de 2015, y en el 2020 presentaron 
#disminución respecto al año anterior. En total se tienen 630 Microempresas.

segmento2=no_morosos.con.ventas[Capacidad=="Salud y Bienestar"]
segmento2=segmento2[ !substr(Fechadeafiliacion,1,4)%in%c("2015","2016","2017","2018","2019","2020","2021")]
segmento2=segmento2[,.N,by=c("Ano","Sector.x","NITsinDV")]
segmento2[is.na(segmento2)] <- 0


segmento2=dcast(segmento2, NITsinDV+Sector.x   ~ Ano , value.var = "N")
segmento2[is.na(segmento2)] <- 0
segmento2=segmento2[`2019`-`2020`>0]

#Segmento 3:
#Empresas que pertenecen a la sección económica más grande, G_Comercio y Rep. Automotores, 
#esta sección fue la que más disminuyó en 2020 respecto 2019, pero aun así 267 micros no lo hicieron,

segmento3=no_morosos.con.ventas[Seccioneconomica=="G_Comercio y Rep. Automotores"]

segmento3=segmento3[`Claseeconomica-Texto` %in% c("Comercio al por menor de artículos de ferretería, pinturas y",
                                                "Comercio al por mayor de prendas de vestir.",
                                                "Comercio al por menor de prendas de vestir y sus accesorios")]

segmento3=segmento3[,.N,by=c("Ano","Sector.x","NITsinDV")]
segmento3[is.na(segmento3)] <- 0

segmento3=dcast(segmento3, NITsinDV+Sector.x   ~ Ano , value.var = "N")
segmento3[is.na(segmento3)] <- 0

#segmento3=segmento3[`2019`-`2020`<0]

#Union en unica tabla

rm(no_morosos.con.ventas,no_morosos)

segmento1=segmento1[,segmento:="Segmento 1"]
segmento1[,Sector.x:=NULL]
segmento1[,`2019`:=NULL]
segmento1[,`2020`:=NULL]

segmento2=segmento2[,segmento:="Segmento 2"]
segmento2[,Sector.x:=NULL]
segmento2[,`2019`:=NULL]
segmento2[,`2020`:=NULL]

segmento3=segmento3[,segmento:="Segmento 3"]
segmento3[,Sector.x:=NULL]
segmento3[,`2019`:=NULL]
segmento3[,`2020`:=NULL]

segmentos=rbind(segmento1,segmento2,segmento3)

rm(segmento1,segmento2,segmento3)

empresas_enriquecida=merge(empresas, segmentos, all.x=TRUE)
empresas_enriquecida=empresas_enriquecida[,.SD[1],by="NITsinDV"]

rm(segmentos)

########## Entiquecimiento ventas  ##########

#Cantidad de compras:
con_ventas=merge(empresas, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

ventas_by_nit=ventas[,.N,by=c("Ano","NIT1")]
ventas_19=ventas_by_nit[Ano==2019]
ventas_20=ventas_by_nit[Ano==2020]

names(ventas_19)=c("year","NIT1","numero_compras_2019")
names(ventas_20)=c("year","NIT1","numero_compras_2020")

ventas_19[,year:=NULL]
ventas_20[,year:=NULL]

empresas_enriquecida=merge(empresas_enriquecida, ventas_19, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, ventas_20, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")

empresas_enriquecida[is.na(numero_compras_2019),numero_compras_2019:=0]
empresas_enriquecida[is.na(numero_compras_2020),numero_compras_2020:=0]

rm(ventas_by_nit,ventas_19,ventas_20)

#Valor de compras
montos_by_nit=ventas[,.(valor_compras=sum(PVlrneto)),by=c("Ano","NIT1")]
montos_by_nit=montos_by_nit[!is.na(valor_compras)]
montos_19=montos_by_nit[Ano==2019]
montos_20=montos_by_nit[Ano==2020]

names(montos_19)=c("year","NIT1","valor_compras_2019")
names(montos_20)=c("year","NIT1","valor_compras_2020")

montos_19[,year:=NULL]
montos_20[,year:=NULL]

empresas_enriquecida=merge(empresas_enriquecida, montos_19, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, montos_20, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")

empresas_enriquecida[is.na(valor_compras_2019),valor_compras_2019:=0]
empresas_enriquecida[is.na(valor_compras_2020),valor_compras_2020:=0]

rm(montos_19,montos_20,montos_by_nit)

########## Preparacion basket market  ##########
#Capacidades:

#Totales
ventas_capacidades=unique(ventas[,c("Capacidad","NIT1")])

ventas_capacidades=ventas_capacidades[!is.na(Capacidad), lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_capacidades)=c("NIT1","capacidades_total")

#2019
ventas_capacidades_19=unique(ventas[,c("Capacidad","NIT1","Ano")])
ventas_capacidades_19=ventas_capacidades_19[Ano==2019]
ventas_capacidades_19[,Ano:=NULL]

cantidad_capacidades_19=ventas_capacidades_19[,.N,by="NIT1"][order(-N)]
names(cantidad_capacidades_19)=c("NIT1","cantidad_capacidades_2019")

ventas_capacidades_19=ventas_capacidades_19[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_capacidades_19)=c("NIT1","capacidades_2019")

#2020
ventas_capacidades_20=unique(ventas[,c("Capacidad","NIT1","Ano")])
ventas_capacidades_20=ventas_capacidades_20[Ano==2020]
ventas_capacidades_20[,Ano:=NULL]

cantidad_capacidades_20=ventas_capacidades_20[,.N,by="NIT1"][order(-N)]
names(cantidad_capacidades_20)=c("NIT1","cantidad_capacidades_2020")

ventas_capacidades_20=ventas_capacidades_20[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_capacidades_20)=c("NIT1","capacidades_2020")

capacidades_final=merge(ventas_capacidades, ventas_capacidades_19, all.x=TRUE)
capacidades_final=merge(capacidades_final, ventas_capacidades_20, all.x=TRUE)

empresas_enriquecida=merge(empresas_enriquecida, capacidades_final, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, cantidad_capacidades_19, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, cantidad_capacidades_20, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")

empresas_enriquecida[is.na(cantidad_capacidades_2019),cantidad_capacidades_2019:=0]
empresas_enriquecida[is.na(cantidad_capacidades_2020),cantidad_capacidades_2020:=0]

rm(ventas_capacidades,ventas_capacidades_19,ventas_capacidades_20,capacidades_final,cantidad_capacidades_19,cantidad_capacidades_20)

#Programas:

#Totales
ventas_programa=unique(ventas[,c("Programa","NIT1")])

ventas_programa=ventas_programa[!is.na(Programa), lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_programa)=c("NIT1","programa_total")

#2019
ventas_programa_19=unique(ventas[,c("Programa","NIT1","Ano")])
ventas_programa_19=ventas_programa_19[Ano==2019]
ventas_programa_19[,Ano:=NULL]

cantidad_programa_19=ventas_programa_19[,.N,by="NIT1"][order(-N)]
names(cantidad_programa_19)=c("NIT1","cantidad_programa_2019")

ventas_programa_19=ventas_programa_19[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_programa_19)=c("NIT1","programa_2019")

#2020
ventas_programa_20=unique(ventas[,c("Programa","NIT1","Ano")])
ventas_programa_20=ventas_programa_20[Ano==2020]
ventas_programa_20[,Ano:=NULL]

cantidad_programa_20=ventas_programa_20[,.N,by="NIT1"][order(-N)]
names(cantidad_programa_20)=c("NIT1","cantidad_programa_2020")

ventas_programa_20=ventas_programa[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_programa_20)=c("NIT1","programa_2020")

programa_final=merge(ventas_programa, ventas_programa_19, all.x=TRUE)
programa_final=merge(programa_final, ventas_programa_20, all.x=TRUE)

empresas_enriquecida=merge(empresas_enriquecida, programa_final, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, cantidad_programa_19, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, cantidad_programa_20, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")

empresas_enriquecida[is.na(cantidad_programa_2019),cantidad_programa_2019:=0]
empresas_enriquecida[is.na(cantidad_programa_2020),cantidad_programa_2020:=0]

rm(ventas_programa,ventas_programa_19,ventas_programa_20,programa_final,cantidad_programa_19,cantidad_programa_20)

#Productos:

#Totales
ventas_producto=unique(ventas[,c("Producto","NIT1")])

ventas_producto=ventas_producto[!is.na(Producto), lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_producto)=c("NIT1","producto_total")

#2019
ventas_producto_19=unique(ventas[,c("Producto","NIT1","Ano")])
ventas_producto_19=ventas_producto_19[Ano==2019]
ventas_producto_19[,Ano:=NULL]

cantidad_producto_19=ventas_producto_19[,.N,by="NIT1"][order(-N)]
names(cantidad_producto_19)=c("NIT1","cantidad_producto_2019")

ventas_producto_19=ventas_producto_19[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_producto_19)=c("NIT1","producto_2019")

#2020
ventas_producto_20=unique(ventas[,c("Producto","NIT1","Ano")])
ventas_producto_20=ventas_producto_20[Ano==2020]
ventas_producto_20[,Ano:=NULL]

cantidad_programa_20=ventas_producto_20[,.N,by="NIT1"][order(-N)]
names(cantidad_programa_20)=c("NIT1","cantidad_producto_2020")

ventas_producto_20=ventas_producto_20[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_producto_20)=c("NIT1","producto_2020")

producto_final=merge(ventas_producto, ventas_producto_19, all.x=TRUE)
producto_final=merge(producto_final, ventas_producto_20, all.x=TRUE)

empresas_enriquecida=merge(empresas_enriquecida, producto_final, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, cantidad_producto_19, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, cantidad_programa_20, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")

empresas_enriquecida[is.na(cantidad_producto_2019),cantidad_producto_2019:=0]
empresas_enriquecida[is.na(cantidad_producto_2020),cantidad_producto_2020:=0]

rm(ventas_producto,ventas_producto_19,ventas_producto_20,producto_final,cantidad_producto_19,cantidad_producto_20)

#Producto agrupado:

#Totales
ventas_producto_ag=unique(ventas[,c("ProductoAgrupado","NIT1")])

ventas_producto_ag=ventas_producto_ag[!is.na(ProductoAgrupado), lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_producto_ag)=c("NIT1","producto_ag_total")

#2019
ventas_producto_ag_19=unique(ventas[,c("ProductoAgrupado","NIT1","Ano")])
ventas_producto_ag_19=ventas_producto_ag_19[Ano==2019]
ventas_producto_ag_19[,Ano:=NULL]

cantidad_producto_ag_19=ventas_producto_ag_19[,.N,by="NIT1"][order(-N)]
names(cantidad_producto_ag_19)=c("NIT1","cantidad_producto_ag_2019")

ventas_producto_ag_19=ventas_producto_ag_19[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_producto_ag_19)=c("NIT1","producto_2019")

#2020
ventas_producto_ag_20=unique(ventas[,c("ProductoAgrupado","NIT1","Ano")])
ventas_producto_ag_20=ventas_producto_ag_20[Ano==2020]
ventas_producto_ag_20[,Ano:=NULL]

cantidad_producto_ag_20=ventas_producto_ag_20[,.N,by="NIT1"][order(-N)]
names(cantidad_producto_ag_20)=c("NIT1","cantidad_producto_ag_2020")

ventas_producto_ag_20=ventas_producto_ag_20[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_producto_ag_20)=c("NIT1","producto_ag_2020")

producto_ag_final=merge(ventas_producto_ag, ventas_producto_ag_19, all.x=TRUE)
producto_ag_final=merge(producto_ag_final, ventas_producto_ag_20, all.x=TRUE)

empresas_enriquecida=merge(empresas_enriquecida, producto_ag_final, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, cantidad_producto_ag_19, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, cantidad_producto_ag_20, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")

empresas_enriquecida[is.na(cantidad_producto_ag_2019),cantidad_producto_2019:=0]
empresas_enriquecida[is.na(cantidad_producto_ag_2020),cantidad_producto_2020:=0]

rm(ventas_producto_ag,ventas_producto_ag_19,ventas_producto_ag_20,producto_ag_final,cantidad_producto_ag_19,cantidad_producto_ag_20)




fwrite(empresas_enriquecida,osPathJoin(resultadosPath,"ultimate_be_20210524.csv"),sep = )

