#################################################################################################################
#GECON
#ÁREA:CRÉDITO
#PLANILHA ABERTURA DAS CONTRIBUIÇÕES DE CONCESSÕES DE CRÉDITO
#MARCELO VILAS BOAS DE CASTRO
#DATA:10-11-2020
#################################################################################################################

#PACOTES REQUERIDOS:
#INSTALAR QUANDO NECESSÁRIO
#EXEMPLO:install.packages("pryr")
library(RCurl)
library(XML)
library(rio)

#DEFINIR PASTAS DE RESULTADOS:
getwd()
setwd("C:\\Users\\User\\Documents")

#Função de coleta de dados
coleta_dados = function(series,datainicial="01/03/2011", datafinal = format(Sys.time(), "%d/%m/%Y")){
  
  for (i in 1:length(series)){
    dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",series[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
    nome_coluna = series[i]
    colnames(dados) = c('data', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "")
    assign(nome_arquivo, dados)
    
    if(i==1)
      base = dados1
    else
      base = merge(base, dados, by = "data", all = T)
    print(paste(i, length(serie), sep = '/'))
  }
  
  base$data = as.Date(base$data, "%d/%m/%Y")
  base = base[order(base$data),]
  base[,-1]=apply(base[,-1],2,function(x)as.numeric(gsub("\\.","",x)))
  rm(list=objects(pattern="^nome"))
  rm(list=objects(pattern="^dados"))
  return(base)
}

#Função de calculo de série por dia útil
dia_util = function(base, datainicial = '2011.03'){
  url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=459044792"
  ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
  ipea.table = ipea.table[-1:-4,-3]
  names(ipea.table) = c("Data", "Dias úteis")
  ipea.table = ipea.table[rowSums(is.na(ipea.table)) == 0,]
  ipea.table = ipea.table[-dim(ipea.table)[1],]
  ipea.table = ipea.table[-dim(ipea.table)[1],]
  dias_uteis= ipea.table[which(ipea.table$Data==datainicial):which(ipea.table$Data==format(as.Date(tail(base$data,1)),"%Y.%m")),]
  base_a = base
  base_a=apply(base_a[,2:length(base_a)],2,function(x){base_a=x/as.numeric(dias_uteis[,2]);return(base_a)})
  base_a = as.data.frame(base_a)
  base_a = cbind(base[,1], base_a)
  base = base_a
  colnames(base)[1] = 'data'
  return(base)
}

#Função para deflacionar séries com IPCA
deflaciona = function(base, datainicial = '2011.03'){
  url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=36482"
  ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
  ipea.table = ipea.table[-1:-4,-3]
  names(ipea.table) = c("Data", "IPCA")
  ipea.table = ipea.table[rowSums(is.na(ipea.table)) == 0,]
  ipea.table = ipea.table[-dim(ipea.table)[1],]
  ipea.table = ipea.table[-dim(ipea.table)[1],]
  deflator = ipea.table[which(ipea.table$Data==datainicial):which(ipea.table$Data==format(as.Date(tail(base$data,1)),"%Y.%m")),]
  deflator=as.numeric(gsub(",","\\.",gsub("\\.","",deflator[,2])))
  base=cbind(base,deflator)
  base=cbind(base[1],apply(base[,2:(length(base)-1)],2,function(x) x*(tail(deflator,1)/deflator)))
  colnames(base)[1] = 'data'
  return(base)
}

#Função para cálculo da contribuicao:
#A função apply irá aplicar a função em cada coluna da base[,-1] (em cada série do bcb)
contribuicao = function(base, total){
  variacao=apply(base[,-1],2,function(x){
    variacao_YoY=rep(NA,12)
    for(i in 13:dim(base)[1])
      variacao_YoY[i]=(x[i]/x[i-12])-1
    return(variacao_YoY)
  })
  
  peso=apply(base[,-1],2,function(x){
    peso=rep(NA,12)
    for(i in 13:dim(base)[1])
      peso[i]=(x[i-12]/base[i-12, total])
    return(peso)
  })
  
  contribuicao = (peso*variacao)
  contribuicao = as.data.frame(contribuicao)
  contribuicao = cbind(base[,1], contribuicao)
  contribuicao = contribuicao[-c(1:12),]
  colnames(contribuicao)[1] = 'data'
  return(contribuicao)
}

#1)Concessões totais
serie=c(20636, 20637, 20638, 20639, 20640, 20641, 20643, 20644, 20645, 20646, 20648, 20649, 20651, 20652, 20653, 20654, 20657, 20658, 20659, 20660, 20661, 20665, 20666, 20668, 20669, 20670, 20673, 20674, 20676, 20677, 20679, 20680,20683, 20684,20687, 20688, 20690, 20691, 20693, 20694, 20695, 20697, 20699, 20700, 20702, 20703, 20705, 20706, 20707, 20709, 20710, 20713, 20631)
base1 = coleta_dados(serie)

serie=c(20655, 20681)
base2 = coleta_dados(serie)

base1['20631'] = base1['20631'] - base2['20655'] - base2['20681']

base1 = dia_util(base1)
base1 = deflaciona(base1)
base1 = contribuicao(base1, '20631')
base1[is.na(base1)] <- 0 #Para poder calcular a soma das linhas
residuo <- base1[,length(base1)] - rowSums(base1[,-c(1, length(base1))])
base1 <- cbind(base1, residuo)

names(base1)=c("Data", "Concessões - Pessoas jurídicas - Desconto de duplicatas e recebíveis - Recursos Livres -  20366",
                       "Concessões - Pessoas jurídicas - Desconto de cheques - Recursos Livres - 20637",
                       "Concessões - Pessoas jurídicas - Antecipação de faturas de cartão de crédito - Recursos Livres - 20638",
                       "Concessões - Pessoas jurídicas - Capital de giro com prazo de até 365 dias - Recursos Livres - 20639",
                       "Concessões - Pessoas jurídicas - Capital de giro com prazo superior a 365 dias - Recursos Livres - 20640",
                       "Concessões - Pessoas jurídicas - Capital de giro rotativo - Recursos Livres - 20641",
                       "Concessões - Pessoas jurídicas - Conta garantida - Recursos Livres - 20643",
                       "Concessões - Pessoas jurídicas - Cheque especial - Recursos Livres - 20644",
                       "Concessões - Pessoas jurídicas - Aquisição de veículos - Recursos Livres - 20645",
                       "Concessões - Pessoas jurídicas - Aquisição de outros bens - Recursos Livres - 20646",
                       "Concessões - Pessoas jurídicas - Arrendamento mercantil de veículos - Recursos Livres - 20648",
                       "Concessões - Pessoas jurídicas - Arrendamento mercantil de outros bens - Recursos Livres - 20649",
                       "Concessões - Pessoas jurídicas - Vendor - Recursos Livres - 20651",
                       "Concessões - Pessoas jurídicas - Compror - Recursos Livres - 20652",
                       "Concessões - Pessoas jurídicas - Cartão de crédito rotativo  - Recursos Livres - 20653",
                       "Concessões - Pessoas jurídicas - Cartão de crédito parcelado - Recursos Livres - 20654",
                       "Concessões - Pessoas jurídicas - Adiantamento sobre contratos de câmbio (ACC) - Recursos Livres - 20657",
                       "Concessões - Pessoas jurídicas - Financiamento a importações - Recursos Livres - 20658",
                       "Concessões - Pessoas jurídicas - Financiamento a exportações - Recursos Livres	- 20659",
                       "Concessões - Pessoas jurídicas - Repasse externo - Recursos Livres - 20660",
                       "Concessões - Pessoas jurídicas - Outros créditos livres - Recursos Livres - 20661",
                      "Concessões - Pessoas físicas - Cheque especial - Recursos livres - 20665",
                      "Concessões - Pessoas físicas - Crédito pessoal não consignado - Recursos livres - 20666",
                      "Concessões - Pessoas físicas - Crédito pessoal consignado para trabalhadores do setor privado - Recursos livres - 20668",
                      "Concessões - Pessoas físicas - Crédito pessoal consignado para trabalhadores do setor público - Recursos livres - 20669",
                      "Concessões - Pessoas físicas - Crédito pessoal consignado para aposentados e pensionistas do INSS - Recursos livres - 20670",
                      "Concessões - Pessoas físicas - Aquisição de veículos - Recursos livres - 20673",
                      "Concessões - Pessoas físicas - Aquisição de outros bens - Recursos livres - 20674",
                      "Concessões - Pessoas físicas - Arrendamento mercantil de veículos - Recursos livres - 20676",
                      "Concessões - Pessoas físicas - Arrendamento mercantil de outros bens - Recursos livres - 20677",
                      "Concessões - Pessoas físicas - Cartão de crédito rotativo - Recursos livres - 20679",
                      "Concessões - Pessoas físicas - Cartão de crédito parcelado - Recursos livres - 20680",
                      "Concessões - Pessoas físicas - Desconto de cheques - Recursos livres - 20683",
                      "Concessões - Pessoas físicas - Outros créditos livres - Recursos livres - 20684",
                      "Concessões - Pessoas jurídicas - Crédito rural com taxas de mercado - Recursos direcionados - 20687",
                      "Concessões - Pessoas jurídicas - Crédito rural com taxas reguladas - Recursos direcionados - 20688",
                      "Concessões - Pessoas jurídicas - Financiamento imobiliário com taxas de mercado - Recursos direcionados - 20690",
                      "Concessões - Pessoas jurídicas - Financiamento imobiliário com taxas reguladas - Recursos direcionados - 20691",
                      "Concessões - Pessoas jurídicas - Capital de giro com recursos do BNDES - Recursos direcionados - 20693",
                      "Concessões - Pessoas jurídicas - Financiamento de investimentos com recursos do BNDES - Recursos direcionados - 20694",
                      "Concessões - Pessoas jurídicas - Financiamento agroindustrial com recursos do BNDES - Recursos direcionados - 20695",
                      "Concessões - Pessoas jurídicas - Outros créditos direcionados - Recursos direcionados - 20697",
                      "Concessões - Pessoas físicas - Crédito rural com taxas de mercado - Recursos direcionados - 20699",
                      "Concessões - Pessoas físicas - Crédito rural com taxas reguladas - Recursos direcionados - 20700",
                      "Concessões - Pessoas físicas - Financiamento imobiliário com taxas de mercado - Recursos direcionados- 20702",
                      "Concessões - Pessoas físicas - Financiamento imobiliário com taxas reguladas - Recursos direcionados - 20703",
                      "Concessões - Pessoas físicas - Capital de giro com recursos do BNDES - Recursos direcionados - 20705",
                      "Concessões - Pessoas físicas - Financiamento de investimentos com recursos do BNDES - Recursos direcionados - 20706",
                      "Concessões - Pessoas físicas - Financiamento agroindustrial com recursos do BNDES - Recursos direcionados - 20707",
                      "Concessões - Pessoas físicas - Microcrédito destinado a consumo - Recursos direcionados - 20709",
                      "Concessões - Pessoas físicas - Microcrédito destinado a microempreendedores - Recursos direcionados - 20710",
                      "Concessões - Pessoas físicas - Outros créditos direcionados - Recursos direcionados - 20713",
                      "Concessões - Total - 20631(menos crédito à vista)",
                      "Resíduo")


write.csv2(base1,"Contribuicoes concessoes para o total menos crédito a vista(fonte).csv", row.names = F)
export(base1, "Contribuicoes concessoes para o total menos crédito a vista(fonte).xlsx")