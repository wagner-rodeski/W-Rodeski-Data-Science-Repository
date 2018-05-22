#####
# MODELO DE PREVISAO DE FUNCIONAMENTO DOS ATMS
# VERSAO 2017-08-03
# filipe.zabala@gmail.com
#####

#####
# Limpa memoria, pacotes
#####

rm(list=ls());gc()
setwd('/home/rstudio/Rscripts')

# Carga dos pacotes
library(tidyverse) # tidyverse
library(progress)  # progress_bar
library(DBI)       # ConexaoDB, instalada no tidyverse
library(ROracle)   # Funcoes de conexao com o banco de dados
library(purrr)     # rbernoulli
library(compiler)  # Compila funcoes
library(parallel)  # mclapply

#####
# Funcoes
#####
source('./spFunctions.R')

#####
# Lendo dados
#####

# Limites por modelo
limites <- read.table(file = './limitesModelo.txt',
                      head = T, sep = '\t')
limites

# Conexão com o banco de dados
source('./ConexaoDB.R')

# Monta query para dados de ATM
(ATMFullQuery <- "select * from aux_previsao_abastecimento WHERE TO_date(\"dataCaptura\", 'DD/MM/YYYY') >= sysdate-90 order by \"atm\"")
# (ATMFullQuery <- "select * from aux_previsao_abastecimento WHERE TO_date(\"dataCaptura\", 'DD/MM/YYYY') >= TO_DATE('20170101', 'YYYYMMDD') AND TO_date(\"dataCaptura\", 'DD/MM/YYYY') <= TO_DATE('20170721', 'YYYYMMDD') order by \"atm\"")

# Executa carga de todos os dados historicos dos ATMs
ini <- Sys.time()
ATMFullDados <- fetch(dbSendQuery(mydb, ATMFullQuery))
Sys.time() - ini  # Time difference of 2.619342 mins (5MM)
gc()

# Lista dos ATMs
unATM <- unique(ATMFullDados$atm)
listaATM <- cbind(id = as.numeric(1:length(unATM)), atm = as.numeric(unATM))


#####
# Criando variaveis
#####

# Classificando datas e horas
ini <- Sys.time()
ATMFullDados$dataAtivacaoAtm2 <- as.POSIXct(ATMFullDados$dataAtivacaoAtm, format = "%d/%m/%Y", tz='GMT'); gc()
ATMFullDados$dataDesativacaoAtm2 <- as.POSIXct(ATMFullDados$dataDesativacaoAtm, format = "%d/%m/%Y", tz='GMT'); gc()
ATMFullDados$dataCaptura2 <- as.POSIXct(ATMFullDados$dataCaptura, format = "%d/%m/%Y", tz='GMT'); gc()
hora <- paste0(ATMFullDados$dataCaptura, ' ', ATMFullDados$horaCaptura, ':', ATMFullDados$minutoCaptura, ':00'); gc()
ATMFullDados$dataHoraCaptura <- as.POSIXct(hora, format="%d/%m/%Y %H:%M:%S", tz='GMT')
fim <- Sys.time(); fim-ini; gc() # Time difference of 28.95417 secs (5MM)

# Criando data frame ordenado por atm e dataCaptura2
ini <- Sys.time()
gc()
ordem <- with(ATMFullDados, order(atm, dataHoraCaptura));gc()
ATMFullDados <- ATMFullDados[ordem, ]; gc()
fim <- Sys.time(); fim-ini; gc() # Time difference of 23.11754 secs (5MM)

# Intervalo de tempo ativo
(intervalo <- range(ATMFullDados$dataHoraCaptura, na.rm = T))
diff(intervalo)

# Cria a coluna ID_DATA
ID_DATA <- as.character(format(Sys.Date(), "%Y%m%d"))
# ID_DATA <- as.character(format(max(ATMFullDados$dataHoraCaptura, na.rm = T), "%Y%m%d"))


# Split ATMFullDados em uma lista
ini <- Sys.time(); gc()
ATMFullLista <- split(ATMFullDados, ATMFullDados$atm)
fim <- Sys.time(); fim-ini; gc() # Time difference of 6.368232 secs (5MM)

# Deletando atmfull
rm(ATMFullDados); gc()

# Calcula ciclos na lista
ini <- Sys.time(); gc()
ATMFullLista <- lapply(ATMFullLista, calcCiclo2)
fim <- Sys.time(); fim-ini; gc() #  5.80028 secs

# Atribuindo classe 'Date' a dataCaptura2
ini <- Sys.time(); gc()
for(i in 1:length(ATMFullLista)){
  class(ATMFullLista[[i]]$dataCaptura2) <- 'Date'
}
fim <- Sys.time(); fim-ini; gc() # Time difference of 0.4083509 secs (5MM)


# Inicio e fim dos ciclos para a lista
cicloLista <- lapply(ATMFullLista, iniFimCiclo)
names(cicloLista) <- formatC(listaATM[,2], width = 8, format = "d", flag = "0")
fim <- Sys.time(); fim-ini; gc() # Time difference of 21.92101 secs (5MM)

# DELETA tabelas -- ATENCAO!!
dbSendQuery(conn = mydb,
            statement = paste0("delete from WSA_FATO_PREVISAO_ABAST where ID_DATA = ",
                               "'", ID_DATA, "'"))
dbCommit(conn = mydb)

dbSendQuery(conn = mydb,
            statement = paste0("delete from FATO_MOTIVO_PARADA_ATM where ID_DATA = ",
                               "'", ID_DATA, "'"))
dbCommit(conn = mydb)

# Cria tabelas para persistir no banco
modeloV2Full <- CreateTable.FatoPrevisaoAbast()
motivoParada <- CreateTable.FatoMotivoParadaAtm()

ativo <- rep(FALSE,length=nrow(listaATM))

# Numero maximo de linhas para persistir no banco
maxLinhas <- 10

# Numero da linha atual
numLinha  <- 0

iniFull <- Sys.time()
# i =  which(listaATM[,2]==09100285)
# Varre os ATMs para processar a simulacao e persistir no banco
for(i in 1:nrow(listaATM)){

  # Tempo inicial para RUNTIME
  ini <- Sys.time()
  
  # Filtrando o ATM para a i-esima iteracao
  atmChar <- formatC(as.numeric(listaATM[i,2]), width = 8, format = "d", flag = "0")
  temp <- ATMFullLista[[which(names(ATMFullLista) == atmChar)]]
  
  # Numero de observacoes
  n <- nrow(temp)
  
  # Rotulando como ativo ou inativo
  ifelse(median(temp$dataDesativacaoAtm2) == as.POSIXct("1900-01-01", tz = 'GMT'), 
         ativo[i] <- TRUE, ativo[i] <- FALSE)

  # Busca Limites do ATM (Recolhimento/Rejeitos)
  limRec2 <- limites$k7_rec[which(limites$modelo == unique(temp$modelo))]
  limRej2 <- limites$k7_rej[which(limites$modelo == unique(temp$modelo))]
  
  # Perfil numerario Qtd
  tabPerfQtd <- data.frame('CASH IN' = 0L, 'CASH OUT' = 0L)
  colnames(tabPerfQtd) <- c('CASH IN', 'CASH OUT')
  tabPerfQtd[1] <- length(temp$perfilNumerario[temp$perfilNumerario == 'CASH IN'])
  tabPerfQtd[2] <- length(temp$perfilNumerario[temp$perfilNumerario == 'CASH OUT'])
  
  # percentual de sacante/depositante por quantidade
  propPerfQtd <- round(prop.table(tabPerfQtd),3) 
  
  # Simulacao de cash in e cash out
  simula <- simStop3(x = temp,
                     tipo = 'a',
                     M = 1000,
                     maxBernoulli = 500,
                     pIn = as.numeric(propPerfQtd[1]),
                     limRec = limRec2, 
                     limRej = limRej2,
                     dropQtdCedulasTotal = temp$dropQtdCedulasTotal[n],
                     k7QtdFaceA = temp$k7QtdFaceA[n]+10,
                     k7QtdFaceB = temp$k7QtdFaceB[n]+10,
                     k7QtdFaceC = temp$k7QtdFaceC[n]+10,
                     k7QtdFaceD = temp$k7QtdFaceD[n]+10,
                     saldoTotal = temp$saldoTotal[n],
                     por = 'hora') #; beep()
  
  # Atribui os quantis
  (quant <- quantile(simula$timeStop, seq(0,1,.1)))
  
  # Crietrios de parada
  simula2 <- motivoParada2(simula)
  head(simula2)
  summary(simula2)
  (tabCriterioParada <- table(simula2$criterioParada))
  # length(tabCriterioParada)
  # prop.table(tabCriterioParada)
  
  # Cria tabela com criteiros de parada
  crit <- matrix(0, ncol = 14, nrow = 1)
  colnames(crit) <- paste0('Criterio', 1:14)
  ifelse(length(tabCriterioParada) != 0,
         crit[colnames(crit) == names(tabCriterioParada)] <- tabCriterioParada,
         crit <- crit)
  
  # Monitoramento
  print(paste0('i ', i, ', ATM ', temp$atm[1], ', n ', n))
  fim <- Sys.time()
  print(fim-ini)
  
  # Tempo final para RUNTIME
  fim <- Sys.time()
  
  # Quantidade de linhas na tabela
  numLinha <- numLinha + 1

  # Atribui valores para tabela que sera persistida
  modeloV2Full[numLinha, "ID_DATA"]        <- ID_DATA
  modeloV2Full[numLinha, "COD_ATM"]        <- as.character(temp$atm[1])
  modeloV2Full[numLinha, "NUM_OBS"]        <- as.character(n)
  modeloV2Full[numLinha, "DEC_0"]          <- as.character(quant[1])
  modeloV2Full[numLinha, "DEC_10"]         <- as.character(quant[2])
  modeloV2Full[numLinha, "DEC_20"]         <- as.character(quant[3])
  modeloV2Full[numLinha, "DEC_30"]         <- as.character(quant[4])
  modeloV2Full[numLinha, "DEC_40"]         <- as.character(quant[5])
  modeloV2Full[numLinha, "DEC_50"]         <- as.character(quant[6])
  modeloV2Full[numLinha, "DEC_60"]         <- as.character(quant[7])
  modeloV2Full[numLinha, "DEC_70"]         <- as.character(quant[8])
  modeloV2Full[numLinha, "DEC_80"]         <- as.character(quant[9])
  modeloV2Full[numLinha, "DEC_90"]         <- as.character(quant[10])
  modeloV2Full[numLinha, "DEC_100"]        <- as.character(quant[11])
  modeloV2Full[numLinha, "RUNTIME"]        <- as.character(round(as.numeric(fim-ini, units = 'secs'),2))
  modeloV2Full[numLinha, "ATIVO"]          <- as.character(ativo[i])
  modeloV2Full[numLinha, "LAMBDA_IN_DROP"] <- as.character(round(simula$lID[1], 2))
  modeloV2Full[numLinha, "LAMBDA_IN_A"]    <- as.character(round(simula$lI10[1],2))
  modeloV2Full[numLinha, "LAMBDA_IN_B"]    <- as.character(round(simula$lI20[1],2))
  modeloV2Full[numLinha, "LAMBDA_IN_C"]    <- as.character(round(simula$lI50[1],2))
  modeloV2Full[numLinha, "LAMBDA_IN_D"]    <- as.character(round(simula$lI100[1],2))
  modeloV2Full[numLinha, "LAMBDA_OUT_A"]   <- as.character(round(simula$lO10[1],2))
  modeloV2Full[numLinha, "LAMBDA_OUT_B"]   <- as.character(round(simula$lO20[1],2))
  modeloV2Full[numLinha, "LAMBDA_OUT_C"]   <- as.character(round(simula$lO50[1],2))
  modeloV2Full[numLinha, "LAMBDA_OUT_D"]   <- as.character(round(simula$lO100[1],2))
  modeloV2Full[numLinha, "K7QTDFACE_A"]    <- as.character(temp$k7QtdFaceA[n])
  modeloV2Full[numLinha, "K7QTDFACE_B"]    <- as.character(temp$k7QtdFaceB[n])
  modeloV2Full[numLinha, "K7QTDFACE_C"]    <- as.character(temp$k7QtdFaceC[n])
  modeloV2Full[numLinha, "K7QTDFACE_D"]    <- as.character(temp$k7QtdFaceD[n])
  modeloV2Full[numLinha, "ID_HORA"]        <- as.character(format(Sys.time(), "%H%M"))
  modeloV2Full[numLinha, "K7QTD_DROP"]     <- as.character(temp$dropQtdCedulasTotal[n])
  
  # Motivo parada
  motivoParada[numLinha, "ID_DATA"]             <- ID_DATA
  motivoParada[numLinha, "COD_ATM"]             <- as.character(temp$atm[1])
  motivoParada[numLinha, "QTD_CRITERIO_1"]      <- as.character(crit[1])
  motivoParada[numLinha, "QTD_CRITERIO_2"]      <- as.character(crit[2])
  motivoParada[numLinha, "QTD_CRITERIO_3"]      <- as.character(crit[3])
  motivoParada[numLinha, "QTD_CRITERIO_4"]      <- as.character(crit[4])
  motivoParada[numLinha, "QTD_CRITERIO_5"]      <- as.character(crit[5])
  motivoParada[numLinha, "QTD_CRITERIO_6"]      <- as.character(crit[6])
  motivoParada[numLinha, "QTD_CRITERIO_7"]      <- as.character(crit[7])
  motivoParada[numLinha, "QTD_CRITERIO_8"]      <- as.character(crit[8])
  motivoParada[numLinha, "QTD_CRITERIO_9"]      <- as.character(crit[9])
  motivoParada[numLinha, "QTD_CRITERIO_10"]     <- as.character(crit[10])
  motivoParada[numLinha, "QTD_CRITERIO_11"]     <- as.character(crit[11])
  motivoParada[numLinha, "QTD_CRITERIO_12"]     <- as.character(crit[12])
  motivoParada[numLinha, "QTD_CRITERIO_13"]     <- as.character(crit[13])
  motivoParada[numLinha, "QTD_CRITERIO_14"]     <- as.character(crit[14])
  

  # Se atingiu o limite de linhas para persistir
  if(numLinha == maxLinhas){
    # Grava no banco
    dbWriteTable(conn = mydb,
                 name = "WSA_FATO_PREVISAO_ABAST",
                 value = modeloV2Full,
                 append = TRUE)
    # dbCommit(conn = mydb)
    
    dbWriteTable(conn = mydb,
                 name = "FATO_MOTIVO_PARADA_ATM",
                 value = motivoParada,
                 append = TRUE)
    dbCommit(conn = mydb)
    
    # Inicializa numero de linhas
    numLinha <- 0

    # Recria tabelas
    modeloV2Full <- CreateTable.FatoPrevisaoAbast()
    motivoParada <- CreateTable.FatoMotivoParadaAtm()
  }  
} 

# Se possui registros para gravar
if(numLinha > 0){
  # Grava no banco
  dbWriteTable(conn = mydb,
               name = "WSA_FATO_PREVISAO_ABAST",
               value = modeloV2Full,
               append = TRUE)
  dbCommit(conn = mydb)
  
  dbWriteTable(conn = mydb,
               name = "FATO_MOTIVO_PARADA_ATM",
               value = motivoParada,
               append = TRUE)
  dbCommit(conn = mydb)
}

fimFull <- Sys.time(); fimFull-iniFull

# Visualiza dados gravados no banco
# ID_DATA <- as.POSIXct('20170726', '%Y%m%d', tz='GMT')
fetch(dbSendQuery(mydb, paste0("select * from WSA_FATO_PREVISAO_ABAST where ID_DATA = ", 
                               "'", ID_DATA, "'")))

fetch(dbSendQuery(mydb, paste0("select * from FATO_MOTIVO_PARADA_ATM where ID_DATA = ", 
                               "'", ID_DATA, "'")))
