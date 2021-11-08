library(arules)
library(arulesViz)
library(datasets)


transactionData=empresas_enriquecida[!is.na(producto_ag_total) & producto_ag_total!=""  & segmento=="Segmento 2" ,c("producto_ag_total")]

#transactionData=empresas_enriquecida[!is.na(producto_total) & producto_total!="" & segmento=="Segmento 3",c("producto_total")]

write.csv(transactionData,osPathJoin(resultadosPath,"market_basket_transactions.csv"), quote = FALSE, row.names = FALSE)

tr <- read.transactions(osPathJoin(resultadosPath,"market_basket_transactions.csv"), format = 'basket', sep='-')

summary(tr)

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=3))

subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)  #> 3913

subset.association.rules. <- association.rules[-subset.rules]

inspect(subset.association.rules.[1:10])

a=as(association.rules, "data.frame")



segmentos=merge(segmentos, empresas, all=FALSE)

fwrite(segmentos,osPathJoin(resultadosPath,"segmentos.csv"))
