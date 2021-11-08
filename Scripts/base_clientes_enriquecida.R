############### Suma de ventas en 2019 ############### 

con_ventas=merge(empresas, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

ventas_by_nit=ventas[,.N,by=c("Ano","NIT1")]
ventas_19=ventas_by_nit[Ano==2019]
ventas_20=ventas_by_nit[Ano==2020]

names(ventas_19)=c("year","NIT1","ventas_2019")
names(ventas_20)=c("year","NIT1","ventas_2020")

ventas_19[,year:=NULL]
ventas_20[,year:=NULL]


empresas_enriquecida=merge(empresas, ventas_19, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")
empresas_enriquecida=merge(empresas_enriquecida, ventas_20, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")


############### Capacidades solicitadas  ############### 

ventas_c=unique(ventas[,c("Capacidad","NIT1")])

ventas_capacidades=ventas_c[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_capacidades)=c("NIT1","capacidades_total")


ventas_c_19=unique(ventas[,c("Capacidad","NIT1","Ano")])
ventas_c_19=ventas_c_19[Ano==2019]
ventas_c_19[,Ano:=NULL]

ventas_capacidades_19=ventas_c_19[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_capacidades_19)=c("NIT1","capacidades_2019")

ventas_c_20=unique(ventas[,c("Capacidad","NIT1","Ano")])
ventas_c_20=ventas_c_20[Ano==2020]
ventas_c_20[,Ano:=NULL]

ventas_capacidades_20=ventas_c_20[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_capacidades_20)=c("NIT1","capacidades_2020")

capacidades_final=merge(ventas_capacidades, ventas_capacidades_19, all.x=TRUE)
capacidades_final=merge(capacidades_final, ventas_capacidades_20, all.x=TRUE)

empresas_enriquecida=merge(empresas_enriquecida, capacidades_final, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")

############### Programas solicitadas  ############### 

ventas_p=unique(ventas[,c("Programa","NIT1")])

ventas_programas=ventas_p[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_programas)=c("NIT1","programas_total")


ventas_p_19=unique(ventas[,c("Programa","NIT1","Ano")])
ventas_p_19=ventas_p_19[Ano==2019]
ventas_p_19[,Ano:=NULL]

ventas_programas_19=ventas_p_19[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_programas_19)=c("NIT1","programas_2019")

ventas_p_20=unique(ventas[,c("Programa","NIT1","Ano")])
ventas_p_20=ventas_p_20[Ano==2020]
ventas_p_20[,Ano:=NULL]

ventas_programa_20=ventas_p_20[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_programa_20)=c("NIT1","programas_2020")

programa_final=merge(ventas_programas, ventas_programas_19, all.x=TRUE)
programa_final=merge(programa_final, ventas_programa_20, all.x=TRUE)

empresas_enriquecida=merge(empresas_enriquecida, programa_final, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")

############### Productos solicitadas  ############### 

ventas_producto=unique(ventas[,c("Producto","NIT1")])

ventas_producto=ventas_producto[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_producto)=c("NIT1","productos_total")


ventas_pto_19=unique(ventas[,c("Producto","NIT1","Ano")])
ventas_pto_19=ventas_pto_19[Ano==2019]
ventas_pto_19[,Ano:=NULL]

ventas_productos_19=ventas_pto_19[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_productos_19)=c("NIT1","productos_2019")

ventas_pto_20=unique(ventas[,c("Producto","NIT1","Ano")])
ventas_pto_20=ventas_pto_20[Ano==2020]
ventas_pto_20[,Ano:=NULL]

ventas_productos_20=ventas_pto_20[, lapply(.SD, paste0, collapse=" - "), by = NIT1]
names(ventas_productos_20)=c("NIT1","productos_2020")

producto_final=merge(ventas_producto, ventas_productos_19, all.x=TRUE)
producto_final=merge(producto_final, ventas_productos_20, all.x=TRUE)

empresas_enriquecida=merge(empresas_enriquecida, producto_final, all.x=TRUE,by.x = "NITsinDV",by.y = "NIT1")

############### Segmentos  ############### 
#Ejecutar previamente el script de segmetnos

segmento1=no_morosos3[,segmento:="Segmento 1"]
segmento1[,Sector.x:=NULL]
segmento1[,`2019`:=NULL]
segmento1[,`2020`:=NULL]

segmento2=no_morososF[,segmento:="Segmento 2"]
segmento2[,Sector.x:=NULL]
segmento2[,`2019`:=NULL]
segmento2[,`2020`:=NULL]

segmento3=no_morososF[,segmento:="Segmento 3"]
segmento3[,Sector.x:=NULL]
segmento3[,`2019`:=NULL]
segmento3[,`2020`:=NULL]

segmentos=rbind(segmento1,segmento2,segmento3)

empresas_enriquecida=merge(empresas_enriquecida, segmentos, all.x=TRUE)

fwrite(empresas_enriquecida,osPathJoin(resultadosPath,"empresas_enriquecida.csv"),sep="|")












