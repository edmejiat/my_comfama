#https://www.datacamp.com/community/tutorials/market-basket-analysis-r


producto_final

install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)


transactionData=empresas_enriquecida[!is.na(productos_total),c("productos_total")]

write.csv(transactionData,osPathJoin(resultadosPath,"market_basket_transactions_productos.csv"), quote = FALSE, row.names = FALSE)


tr <- read.transactions(osPathJoin(resultadosPath,"market_basket_transactions_productos.csv"), format = 'basket', sep='-')

summary(tr)

if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}

itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=4))


write(association.rules,osPathJoin(resultadosPath,"reglas_productos.csv"))

a=inspect(association.rules[1:14])

a=fread(osPathJoin(resultadosPath,"reglas_productos.csv"))

subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)

plot(subRules,method="two-key plot")

top10subRules <- head(subRules, n = 10, by = "count")

plot(top10subRules, method = "graph",  engine = "htmlwidget")


setDT(a)
orden_regla=c(1:30)
a=a[][order(-count)]
a=cbind(a,orden_regla)

############# Aplicacion ###################

a$lhs=nombres=gsub("[][!#$%()*.:;<=>@^_`/|~.{}?¿]", "",a$lhs)
a=a[,cantidad_previos:=str_count(lhs, ',')+1]

foo <- data.frame(do.call('rbind', strsplit(as.character(a$lhs),',',fixed=TRUE)))

foo=cbind(foo,a$rhs,a$support,a$confidence,a$coverage,a$lift,a$count,a$cantidad_previos,a$orden_regla)
names(a)

names(foo)=c("X1","X2","rhs","support","confidence","coverage","lift","count","cantidad_previos","orden_regla")




bm_empresas=empresas_enriquecida[!is.na(productos_total)]

setDT(foo)
foo_oprtunidades=foo[confidence !=1]


foo_oprtunidades$rhs=gsub("[][!#$%()*.:;<=>@^_`/|~.{}?¿]", "",foo_oprtunidades$rhs)
a=a[,cantidad_previos:=str_count(lhs, ',')+1]

bm_empresas[ ,reglas_estrictas:=character(.N) ]
bm_empresas[ ,productos_ofrecidos_extrictos:=character(.N) ]
reglas_list=foo_oprtunidades$orden_regla

for (i in 1:length(reglas_list)) {
  my_regla=foo_oprtunidades[orden_regla==reglas_list[i]]
  
  if (my_regla$cantidad_previos==1) {
    bm_empresas[grepl( my_regla$rhs, productos_total, fixed = TRUE)==FALSE,regla_bool:=grepl( my_regla$X1, productos_total, fixed = TRUE)]
    bm_empresas=bm_empresas[regla_bool==TRUE ,reglas_estrictas:=paste(reglas_estrictas,paste("Regla:",my_regla$orden_regla,sep = " "),sep = "-")]
    bm_empresas=bm_empresas[regla_bool==TRUE ,productos_ofrecidos_extrictos:=paste(productos_ofrecidos_extrictos,my_regla$rhs,sep = "-")]
  } else if (my_regla$cantidad_previos==2) {
    bm_empresas[grepl( my_regla$rhs, productos_total, fixed = TRUE)==FALSE,regla_bool:=ifelse(grepl( my_regla$X1, productos_total, fixed = TRUE) & grepl( my_regla$X2, programas_total, fixed = TRUE),TRUE,FALSE)]
    bm_empresas=bm_empresas[regla_bool==TRUE ,reglas_estrictas:=paste(reglas_estrictas,paste("Regla:",my_regla$orden_regla,sep = " "),sep = "-")] 
    bm_empresas=bm_empresas[regla_bool==TRUE ,productos_ofrecidos_extrictos:=paste(productos_ofrecidos_extrictos,my_regla$rhs,sep = "-")]
  } 
  
  
  
}


grepl( "Danilo", "Danilo", fixed = TRUE)

bm_empresas[,regla_bool:=NULL]
bm_empresas[,reglas_estrictas:=NULL]

a$lhs=str_replace(a$lhs,","," - ")


intento1=merge(bm_empresas, a, all.x=TRUE,by.x="programas_total",by.y="lhs")




