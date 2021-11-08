########## segmento 1 ########## 

no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

no_morosos=no_morosos[Seccioneconomica=="F_Construcción"]
no_morosos_recientes=no_morosos[ substr(Fechadeafiliacion,1,4)%in%c("2015","2016","2017","2018","2019","2020","2021")]

no_morosos2=no_morosos[,.N,by=c("Ano","Sector.x","NITsinDV")]
no_morosos3=dcast(no_morosos2, NITsinDV+Sector.x   ~ Ano , value.var = "N")
no_morosos3[is.na(no_morosos3)] <- 0

no_morosos3=no_morosos3[Sector.x=="Micro Empresa"]

fwrite(no_morosos3,osPathJoin(resultadosPath,"segmento1.csv"),sep=";")

########## segmento 2 ########## 

no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

no_morosos=no_morosos[Capacidad=="Salud y Bienestar"]
no_morosos=no_morosos[ !substr(Fechadeafiliacion,1,4)%in%c("2015","2016","2017","2018","2019","2020","2021")]
no_morosos=no_morosos[,.N,by=c("Ano","Sector.x","NITsinDV")]
no_morosos[is.na(no_morosos)] <- 0


no_morosos3=dcast(no_morosos, NITsinDV+Sector.x   ~ Ano , value.var = "N")
no_morosos3[is.na(no_morosos3)] <- 0
no_morososF=no_morosos3[`2019`-`2020`>0]

no_morososF=no_morososF[Sector.x=="Micro Empresa"]

fwrite(no_morososF,osPathJoin(resultadosPath,"segmento2.csv"),sep=";")

########## segmento 3 ########## 

no_morosos=empresas[Tienemoraenaportes=="No"]
no_morosos=merge(no_morosos, ventas, all=FALSE,by.y = "NIT1",by.x = "NITsinDV")

no_morosos=no_morosos[Seccioneconomica=="G_Comercio y Rep. Automotores"]
no_morosos=no_morosos[,.N,by=c("Ano","Sector.x","NITsinDV")]
no_morosos[is.na(no_morosos)] <- 0

no_morosos3=dcast(no_morosos, NITsinDV+Sector.x   ~ Ano , value.var = "N")
no_morosos3[is.na(no_morosos3)] <- 0

no_morososF=no_morosos3[`2019`-`2020`<0]

no_morososF[,.N,by=Sector.x]

no_morososF=no_morososF[Sector.x=="Micro Empresa"]

fwrite(no_morososF,osPathJoin(resultadosPath,"segmento3.csv"),sep=";")
