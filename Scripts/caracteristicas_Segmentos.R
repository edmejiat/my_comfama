segmento1=empresas_enriquecida[segmento=="Segmento 2"]

columnas=c("NITsinDV","Sector","segmento","numero_compras_2019", "numero_compras_2020","valor_compras_2019","valor_compras_2020","capacidades_total"         
           ,"capacidades_2019","capacidades_2020","cantidad_capacidades_2019","cantidad_capacidades_2020" 
           ,"programa_total","programa_2019","programa_2020","cantidad_programa_2019"    
           ,"cantidad_programa_2020","producto_total","producto_2019","producto_2020"             
           ,"cantidad_producto_2019","cantidad_producto_2020" )

segmento1=segmento1[, ..columnas]

sum(segmento1$cantidad_producto_2019)

sum(segmento1$cantidad_producto_2020)


sum(segmento1$cantidad_capacidades_2019)

sum(segmento1$cantidad_capacidades_2020)

sum(segmento1$valor_compras_2019,na.rm=TRUE)

sum(segmento1$valor_compras_2020,na.rm=TRUE)


sum(segmento1$numero_compras_2019)

sum(segmento1$numero_compras_2020)

a=segmento1[,.N,by=c("capacidades_total"         
                   ,"capacidades_2019","capacidades_2020")]



empresas_enriquecida[!is.na(segmento),.N,by=c("segmento","Sector")][order(segmento)]



segmento1[numero_compras_2019==0,.N,by=Sector]

