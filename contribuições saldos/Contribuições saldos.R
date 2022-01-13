#################################################################################################################
#GECON
#ÁREA:CRÉDITO
#PLANILHA ABERTURA DAS CONTRIBUIÇÕES DE CRÉDITO
#MARCELO VILAS BOAS DE CASTRO
#DATA:10-11-2020
#################################################################################################################

#PACOTES REQUERIDOS:
#INSTALAR QUANDO NECESSÁRIO
#EXEMPLO:install.packages("pryr")
#library(xlsx)
library(RCurl)
library(XML)
library(rio)

#DEFINIR PASTAS DE RESULTADOS:
getwd()
setwd("C:\\Users\\User\\Documents")

#Criando função para coleta de séries
coleta_dados_sgs = function(series,datainicial="01/03/2011", datafinal = format(Sys.time(), "%d/%m/%Y")){
  #Argumentos: vetor de séries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  #Cria estrutura de repetição para percorrer vetor com códigos de séries e depois juntar todas em um único dataframe
  for (i in 1:length(series)){
    dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",series[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
    dados[,-1] = as.numeric(gsub(",",".",dados[,-1])) #As colunas do dataframe em objetos numéricos exceto a da data
    nome_coluna = series[i] #Nomeia cada coluna do dataframe com o código da série
    colnames(dados) = c('data', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
    assign(nome_arquivo, dados)
    
    if(i==1)
      base = dados1 #Primeira repetição cria o dataframe
    else
      base = merge(base, dados, by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
    print(paste(i, length(series), sep = '/')) #Printa o progresso da repetição
  }
  
  base$data = as.Date(base$data, "%d/%m/%Y") #Transforma coluna de data no formato de data
  base = base[order(base$data),] #Ordena o dataframe de acordo com a data
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

#1)Pessoas Jurídicas - Recursos livres
series1=c(20544,20545,20546,20547,20548,20549,20551,20552,20553,20554,20556,20557,20559,20560,20561,20562,20563,20565,20566,20567,20568,20569,20543)

base1 <- coleta_dados_sgs(series1)

#Deflacionando séries com IPCA
base1 <- deflaciona(base1)

#Cálculo da contribuicao:
contribuicao1 <- contribuicao(base1, '20543')

base1=merge(base1,contribuicao1, by = "data")
base1=base1[,c(1,2,25,3,26,4,27,5,28,6,29,7,30,8,31,9,32,10,33,11,34,12,35,13,36,14,37,15,38,16,39,17,40,18,41,19,42,20,43,21,44,22,45,23,46,24,47)]

names(base1)=c("Data", "Saldo (em R$ milhões) - Pessoas jurídicas - Desconto de duplicatas e recebíveis - 20544", "20544 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Desconto de cheques- 20545", "20545 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Antecipação de faturas de cartão de crédito - 20546", "20546 - Contribuição para a variação total",  
              "Saldo (em R$ milhões) - Pessoas jurídicas - Capital de giro com prazo de até 365 dias - 20547", "20547 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Capital de giro com prazo superior a 365 dias - 20548", "20548 - Contribuição para a variação total",  
              "Saldo (em R$ milhões) - Pessoas jurídicas - Capital de giro rotativo - 20549", "20549 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Conta garantida - 20551", "20551 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Cheque especial - 20552", "20552 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Aquisição de veículos - 20553", "20553 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Aquisição de outros bens - 20554", "20554 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Arrendamento mercantil de veículos - 20556", "20556 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Arrendamento mercantil de outros bens - 20557", "20557 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Vendor - 20559", "20559 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Compror - 20560", "20560 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Cartão de crédito rotativo - 20561", "20561 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Cartão de crédito parcelado - 20562", "20562 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Cartão de crédito à vista - 20563", "20563 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Adiantamento sobre contratos de câmbio (ACC) - 20565", "20565 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Financiamento a importações - 20566", "20566 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Financiamento a exportações - 20567", "20567 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Repasse externo - 20568", "20568 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Outros créditos livres - 20569", "20569 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas jurídicas - Total - 20543", "20543 - Contribuição para a variação total")


write.csv2(base1,"01 - Contribuicoes saldo pessoa juridica recursos livres (em R$ milhões).csv", row.names = F)
export(base1, "Contribuições saldo pessoas jurídica e física com recursos livres e direcionados(fonte).xlsx", sheetName = "PJ Livres")

#2)Pessoas Físicas - Recursos livres
series2=c(20573,20574,20575,20576,20577,20578,20581,20582,20584,20585,20587,20588,20589,20591,20592,20570)

base2 <- coleta_dados_sgs(series2)

#Deflacionando séries com IPCA
base2 <- deflaciona(base2)

#Cálculo da contribuicao:
contribuicao2 <- contribuicao(base2, '20570')

base2=merge(base2,contribuicao2, by = "data")
base2=base2[,c(1,2,18,3,19,4,20,5,21,6,22,7,23,8,24,9,25,10,26,11,27,12,28,13,29,14,30,15,31,16,32,17,33)]

names(base2)=c("Data", "Saldo (em R$ milhões) - Pessoas físicas - Cheque especial - 20573", "20573 - Contribuição para a variação total",  
              "Saldo (em R$ milhões) - Pessoas físicas - Crédito pessoal não consignado - 20574", "20574 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Crédito pessoal não consignado vinculado à composição de dívidas - 20575", "20575 - Contribuição para a variação total",  
              "Saldo (em R$ milhões) - Pessoas físicas - Crédito pessoal consignado para trabalhadores do setor privado - 20576", "20576 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Crédito pessoal consignado para trabalhadores do setor público - 20577", "20577 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Crédito pessoal consignado para aposentados e pensionistas do INSS - 20578", "20578 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Aquisição de veículos - 20581", "20581 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Aquisição de outros bens - 20582", "20582 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Arrendamento mercantil de veículos - 20584", "20584 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Arrendamento mercantil de outros bens - 20585", "20585 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Cartão de crédito rotativo - 20587", "20587 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Cartão de crédito parcelado - 20588", "20588 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Cartão de crédito à vista - 20589", "20589 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Desconto de cheques - 20591", "20591 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) - Pessoas físicas - Outros créditos livres - 20592", "20592 - Contribuição para a variação total", 
              "Saldo (em R$ milhões) da carteira de crédito com recursos livres - Pessoas físicas - Total - 20570", "20570 - Contribuição para a variação total")


write.csv2(base2,"02 - Contribuicoes saldo pessoa fisica recursos livres (em R$ milhões).csv", row.names = F)
export(base2, "Contribuições saldo pessoas jurídica e física com recursos livres e direcionados(fonte).xlsx", which = "PF Livres")

#3)Pessoas Jurídicas - Recursos direcionados
series3=c(20595,20596,20598,20599,20601,20602,20603,20605,20594)

base3 <- coleta_dados_sgs(series3)

#Deflacionando séries com IPCA
base3 <- deflaciona(base3)

#Cálculo da contribuicao:
contribuicao3 <- contribuicao(base3, '20594')

base3=merge(base3,contribuicao3, by = "data")
base3=base3[,c(1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)]

names(base3)=c("Data", "Saldo (em R$ milhões) - Pessoas jurídicas - Crédito rural com taxas de mercado - 20595", "20595 - Contribuição para a variação total", 
               "Saldo (em R$ milhões) - Pessoas jurídicas - Crédito rural com taxas reguladas - 20596", "20596 - Contribuição para a variação total", 
               "Saldo (em R$ milhões) - Pessoas jurídicas - Financiamento imobiliário com taxas de mercado - 20598", "20598 - Contribuição para a variação total",  
               "Saldo (em R$ milhões) - Pessoas jurídicas - Financiamento imobiliário com taxas reguladas - 20599", "20599 - Contribuição para a variação total", 
               "Saldo (em R$ milhões) - Pessoas jurídicas - Capital de giro com recursos do BNDES - 20601", "20601 - Contribuição para a variação total",  
               "Saldo (em R$ milhões) - Pessoas jurídicas - Financiamento de investimentos com recursos do BNDES - 20602", "20602 - Contribuição para a variação total", 
               "Saldo (em R$ milhões) - Pessoas jurídicas - Financiamento agroindustrial com recursos do BNDES - 20603", "20603 - Contribuição para a variação total", 
               "Saldo (em R$ milhões) - Pessoas jurídicas - Outros créditos direcionados - 20605", "20605 - Contribuição para a variação total", 
               "Saldo (em R$ milhões) - Pessoas jurídicas - Total - 20594", "20594 - Contribuição para a variação total")

write.csv2(base3,"03 - Contribuicoes saldo pessoa juridica recursos direcionados (em R$ milhões).csv", row.names = F)
export(base3, "Contribuições saldo pessoas jurídica e física com recursos livres e direcionados(fonte).xlsx", which = "PJ Direcionados")

#4)Pessoas Físicas - Recursos direcionados
series4=c(20607,20608,20610,20611,20613,20614,20615,20617,20618,20621,20606)

base4 <- coleta_dados_sgs(series4)

#Deflacionando séries com IPCA
base4 <- deflaciona(base4)

#Cálculo da contribuicao:
contribuicao4 <- contribuicao(base4, '20606')

base4=merge(base4,contribuicao4, by = "data")
base4=base4[,c(1,2,13,3,14,4,15,5,16,6,17,7,18,8,19,9,20,10,21,11,22,12,23)]

names(base4)=c("Data", " Saldo (R$ milhões) - Pessoas físicas - Crédito rural com taxas de mercado - 20607", "20607 - Contribuição para a variação total", 
               "Saldo (R$ milhões) - Pessoas físicas - Crédito rural com taxas reguladas - 20608", "20608 - Contribuição para a variação total", 
               "Saldo (R$ milhões) - Pessoas físicas - Financiamento imobiliário com taxas de mercado - 20610", "20610 - Contribuição para a variação total",  
               "Saldo (R$ milhões) - Pessoas físicas - Financiamento imobiliário com taxas reguladas - 20611", "20611 - Contribuição para a variação total", 
               "Saldo (R$ milhões) - Pessoas físicas - Capital de giro com recursos do BNDES - 20613", "20613 - Contribuição para a variação total",  
               "Saldo (R$ milhões) - Pessoas físicas - Financiamento de investimentos com recursos do BNDES - 20614", "20614 - Contribuição para a variação total", 
               "Saldo (R$ milhões) - Pessoas físicas - Financiamento agroindustrial com recursos do BNDES - 20615", "20615 - Contribuição para a variação total", 
               "Saldo (R$ milhões) - Pessoas físicas - Microcrédito destinado a consumo - 20617", "20617 - Contribuição para a variação total", 
               "Saldo (R$ milhões) - Pessoas físicas - Microcrédito destinado a microempreendedores - 20618", "20618 - Contribuição para a variação total",
               "Saldo (R$ milhões) - Pessoas físicas - Outros créditos direcionados - 20621", "20621 - Contribuição para a variação total", 
               "Saldo (R$ milhões) - Pessoas físicas - Total - 20606", "20606 - Contribuição para a variação total")


write.csv2(base4,"04 - Contribuicoes saldo pessoa fisica recursos direcionados (em R$ milhões).csv", row.names = F)
export(base4, "Contribuições saldo pessoas jurídica e física com recursos livres e direcionados(fonte).xlsx", which = "PF Direcionados")