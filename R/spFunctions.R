#####
# Funcoes
#####

#####
# cicloDrop2: calcula ciclos para drop - OTIMIZADO
#####
calcCiclo2 <- function(x){
  n <- nrow(x)
  cicloDrop <- rep(0, n)
  d <- c(-1,diff(x[['dropQtdCedulasTotal']]))
  menorZero <- d<0
  totCiclo <- sum(menorZero, na.rm = T)
  cicloDrop[menorZero] <- 1:totCiclo
  nRep <- diff(c(which(cicloDrop != 0), n))
  cicloDrop[!menorZero] <- rep(1:totCiclo, nRep-c(rep(1,length(nRep)-1),0))
  return(cbind(x, cicloDrop))
}

# calcCiclo2(ATMFullLista[[1]])
# head(ATMFullLista[[1]])

#####
# calcWeekDay: calcula dias da semana
#####
calcWeekDay <- function(x){
  weekDay <- weekdays(x[['dataCaptura2']])
  recode(weekDay, 
         " 'Monday'='1.Monday'; 'Tuesday'='2.Tuesday'; 'Wednesday'='3.Wednesday'; 'Thursday'='4.Thursday'; 
         'Friday'='5.Friday'; 'Saturday'='6.Saturday'; 'Sunday'='7.Sunday' ")
  return(cbind(x,weekDay))
}

#####
# calcMonthDay: calcula dias dos mes
#####
calcMonthDay <- function(x){
  monthDay <- as.POSIXlt(x[['dataCaptura2']])$mday
  # monthDay <- as.numeric(days(x[['dataCaptura2']]))
  return(cbind(x,monthDay))
}

#####
# lambdaInDrop: taxa de entrada drop. Retorna maximo de notas, tamanho do ciclo, lambdas drop
#####
lambdaInDrop <- function(x, por = 'hora'){
  
  # Total de notas
  totInDrop <- sum(by(x$dropQtdCedulasTotal, x$cicloDrop, max))
  
  # Tamanho do historico (dias)
  tempo <- as.numeric(diff(range(x[['dataHoraCaptura']])))
  
  # Lambda/dia
  lambda <- totInDrop/tempo
  switch(por,
         minuto = return(cbind(totInDrop = totInDrop, tempoMinuto = tempo*24*60, lambdaMinuto = as.numeric(lambda/(24*60)))),
         hora = return(cbind(totInDrop = totInDrop, tempoHora = tempo*24, lambdaHora = as.numeric(lambda/24))),
         dia =  return(cbind(totInDrop = totInDrop, tempoDia = tempo, lambdaDia = as.numeric(lambda))),
         semana = return(cbind(totInDrop = totInDrop, tempoSemana = tempo/7, lambdaSemana = as.numeric(lambda*7))),
         mes = return(cbind(totInDrop = totInDrop, tempoMes = tempo/30, lambdaMes = as.numeric(lambda*30)))
  )
}

#####
# lambdaIn: retorna maximo de notas, tamanho do ciclo, lambdas drop
#####
lambdaIn <- function(x, nota, por = 'hora') {
  
  # Total de notas depositadas
  dn <- paste0('depDinheiroQtdCedula', nota)
  vn <- paste0('depVarejistaQtdCedula', nota)
  rn <- paste0('recDinheiroQtdCedula', nota) # Recarga de celular, nao recolhimento
  rpn <- paste0('recPreDinheiroQtdCedula', nota) # Recarga de celular pre-pago, nao recolhimento
  totIn <- sum(c(x[[dn]], x[[vn]], x[[rn]], x[[rpn]]), na.rm = T)
  
  # Tamanho do historico (dias)
  tempo <- as.numeric(diff(range(x[['dataHoraCaptura']])))
  
  # Lambda/dia
  lambda <- totIn/tempo
  
  switch(por,
         minuto = return(cbind(totIn = totIn, tempoMinuto = tempo*24*60, lambdaMinuto = as.numeric(lambda/(24*60)))),
         hora = return(cbind(totIn = totIn, tempoHora = tempo*24, lambdaHora = as.numeric(lambda/24))),
         dia = return(cbind(totIn = totIn, tempoDia = tempo, lambdaDia = as.numeric(lambda))),
         semana = return(cbind(totIn = totIn, tempoSemana = tempo/7, lambdaSemana = as.numeric(lambda*7))),
         mes = return(cbind(totIn = totIn, tempoMes = tempo/30, lambdaMes = as.numeric(lambda*30)))
  )
}

#####
# lambdaIn por ciclo, depende: lambdaIn
#####
lambdaInCiclo <- function(x, nota, por = 'hora', coluna = 'cicloDrop') {
  cbind(ciclo = 0:max(x[[coluna]]),
        do.call(rbind.data.frame, by(x, x[[coluna]], lambdaIn, nota = nota, por = por)))
}
# lambdaInCiclo(AtmTemp, 10)

#####
# lambdaIn por diaSemana, depende: lambdaIn
#####
lambdaInWeekDay <- function(x, nota, por = 'hora') {
  cbind(do.call(rbind.data.frame, by(x, x[['weekDay']], lambdaIn, nota = nota, por = por)))
}

# lambdaInWeekDay(AtmTemp, 10, 'hora')

#####
# lambdaIn por diaMes, depende: lambdaIn
#####
lambdaInMonthDay <- function(x, nota, por = 'hora') {
  cbind(do.call(rbind.data.frame, by(x, x[['monthDay']], lambdaIn, nota = nota, por = por)))
}

#####
# lambdaOut
#####
lambdaOut <- function(x, nota, por = 'hora'){
  
  # Total de notas retiradas
  rn <- paste0('recolhimentoQtdCedula', nota)
  sn <- paste0('saqueQtdCedula', nota)
  tn <- paste0('transfereRapidoQtdCedula', nota)
  totOut <-  sum(c(x[[rn]], x[[sn]], x[[tn]]), na.rm = T)
  
  # Tamanho do historico (dias)
  tempo <- as.numeric(diff(range(x[['dataHoraCaptura']])))
  
  # Lambda/dia
  lambda <- totOut/tempo
  
  switch(por,
         minuto = return(cbind(totOut = totOut, tempoMinuto = tempo*24*60, lambdaMinuto = as.numeric(lambda/(24*60)))),
         hora = return(cbind(totOut = totOut, tempoHora = tempo*24, lambdaHora = as.numeric(lambda/24))),
         dia = return(cbind(totOut = totOut, tempoDia = tempo, lambdaDia = as.numeric(lambda))),
         semana = return(cbind(totOut = totOut, tempoSemana = tempo/7, lambdaSemana = as.numeric(lambda*7))),
         mes = return(cbind(totOut = totOut, tempoMes = tempo/30, lambdaMes = as.numeric(lambda*30)))
  )
}

#####
# lambdaOut por ciclo, depende: lambdaOut
#####
lambdaOutCiclo <- function(x, nota, por = 'hora') {
  cbind(ciclo = 0:max(x[['cicloDrop']]),
        do.call(rbind.data.frame, by(x, x[['cicloDrop']], lambdaOut, nota = nota, por = por)))
}

#####
# lambdaOut por diaSemana, depende: lambdaOut
#####
lambdaOutWeekDay <- function(x, nota, por = 'hora') {
  cbind(do.call(rbind.data.frame, by(x, x[['weekDay']], lambdaOut, nota = nota, por = por)))
}

#####
# lambdaOut por diaMes, depende: lambdaOut
#####
lambdaOutMonthDay <- function(x, nota, por = 'hora') {
  cbind(do.call(rbind.data.frame, by(x, x[['monthDay']], lambdaOut, nota = nota, por = por)))
}

#####
# thetaInDrop: frequencia de operacao do cassete de rejeito
#####
thetaInDrop <- function(x){
  
  # Percentual de operacoes no cassete de rejeito
  difDrop <- diff(x$dropQtdCedulasTotal)
  perInDrop <- sum(difDrop != 0)/length(difDrop)
  return(perInDrop)
}



#####
# thetaIn: frequencia de entrada nos cassetes A, B, C e D
#####
thetaIn <- function(x, nota) {
  
  # Total de notas depositadas
  dn <- paste0('depDinheiroQtdCedula', nota)
  vn <- paste0('depVarejistaQtdCedula', nota)
  rn <- paste0('recDinheiroQtdCedula', nota) # Recarga de celular, nao recolhimento
  rpn <- paste0('recPreDinheiroQtdCedula', nota) # Recarga de celular pre-pago, nao recolhimento
  totIn <- rowSums(cbind(x[[dn]], x[[vn]], x[[rn]], x[[rpn]]), na.rm = T)

  # Percentual de entradas no cassete da nota
  perIn <- sum(totIn != 0)/length(totIn)
  return(perIn)
}

#####
# thetaOut: frequencia de saida nos cassetes A, B, C e D
#####
thetaOut <- function(x, nota) {
  
  # Total de notas retiradas
  rn <- paste0('recolhimentoQtdCedula', nota)
  sn <- paste0('saqueQtdCedula', nota)
  tn <- paste0('transfereRapidoQtdCedula', nota)
  totOut <- rowSums(cbind(x[[rn]], x[[sn]], x[[tn]]), na.rm = T)
  
  # Percentual de entradas no cassete da nota
  perOut <- sum(totOut != 0)/length(totOut)
  return(perOut)
}

#####
# Retorna acumulado de notas, numero de dias da semana observados, lambda
#####
calcParWeek <- function(x){
  cumNotas <- sum(x$dropQtdCedulasTotal)
  numDias <- length(x$dropQtdCedulasTotal)
  lambda <- cumNotas/numDias
  return(cbind(cumNotas, numDias, lambda))
}

#####
# Motivo da parada, motivoParada2. depende: simStop3
#####
motivoParada2 <- function(x){
  criterioParada <- vector(length = nrow(x))
  for(i in 1:nrow(x)){
    ifelse(length(which.min(x[i,7:20])) == 0,
           criterioParada[i] <- NA,
           criterioParada[i] <- substr(names(which.min(x[i,7:20])),9,18))
  }
  return(cbind(x, criterioParada = criterioParada))
}

#####
# Mosaico de gráficos, lay_out
#####
lay_out <- function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

#####
# Graficos por ciclo para Drop, A-D, depende: lay_out
#####
grafCiclo <- function(x, cicloDown = 1, cicloUp = 10, limRec = limRec2, limRej = limRej2){
  
  plotDrop <- ggplot(x[x$cicloDrop %in% cicloDown:cicloUp,]) +
    aes(dataHoraCaptura, dropQtdCedulasTotal, colour = as.factor(cicloDrop)) +
    ylim(0,limRej) +
    theme(legend.position = 'top') +
    geom_point()
  
  plotFaceA <- ggplot(x[x$cicloFaceA %in% cicloDown:cicloUp,]) +
    aes(dataHoraCaptura, k7QtdFaceA, colour = as.factor(cicloFaceA)) +
    ylim(0,limRec) +
    theme(legend.position = 'none') +
    geom_point()
  
  plotFaceB <- ggplot(x[x$cicloFaceB %in% cicloDown:cicloUp,]) +
    aes(dataHoraCaptura, k7QtdFaceB, colour = as.factor(cicloFaceB)) +
    ylim(0,limRec) +
    theme(legend.position = 'none') +
    geom_point()
  
  plotFaceC <- ggplot(x[x$cicloFaceC %in% cicloDown:cicloUp,]) +
    aes(dataHoraCaptura, k7QtdFaceC, colour = as.factor(cicloFaceC)) +
    ylim(0,limRec) +
    theme(legend.position = 'none') +
    geom_point()
  
  plotFaceD <- ggplot(x[x$cicloFaceD %in% cicloDown:cicloUp,]) +
    aes(dataHoraCaptura, k7QtdFaceD, colour = as.factor(cicloFaceD)) +
    ylim(0,limRec) +
    theme(legend.position = 'none') +
    geom_point()
  
  # grid.arrange(plotDrop, plotFaceA, ncol = 2)
  lay_out(list(plotDrop, 1, 1:2),
          list(plotFaceA, 2, 1),
          list(plotFaceB, 2, 2),
          list(plotFaceC, 3, 1),
          list(plotFaceD, 3, 2))
}

#####
# Soma acumulada com zerada
#####
cumsumReset <- function(x, threshold = 0){
  somaAc <- rep(0, length(x))
  somaAc[1] <- x[1]
  ifelse(somaAc[1] < threshold, somaAc[1] <- 0, somaAc[1] <- somaAc[1])
  
  for(i in 2:length(x)){
    somaAc[i] <- somaAc[i-1] + x[i]
    ifelse(somaAc[i] < threshold, somaAc[i] <- 0, somaAc[i] <- somaAc[i])
  }
  return(somaAc)
}

#####
# Modelo (Bernoulli2-Poisson) tempo ate a parada do ATM, simStop3
#####
# x = temp
# tipo = 'a'
# M = 1000
# maxBernoulli = 500
# pIn = propPerfQtd[1]
# limRec = 1800
# limRej = 1650
# dropQtdCedulasTotal = 10
# k7QtdFaceA = 10
# k7QtdFaceB = 10
# k7QtdFaceC = 10
# k7QtdFaceD = 10
# saldoTotal = 0
# por = 'hora'
simStop3 <- function(x,
                     tipo = c('a', 'c'),
                     M = 1000, 
                     maxBernoulli = 500, 
                     pIn = propPerfQtd[1],
                     limRec = 1800, 
                     limRej = 1650,
                     dropQtdCedulasTotal = 10,
                     k7QtdFaceA = 10,
                     k7QtdFaceB = 10, 
                     k7QtdFaceC = 10, 
                     k7QtdFaceD = 10,
                     saldoTotal = 0,
                     por = 'hora'){
  
  # Calcula lambdas (taxas)
  # In
  lID       <- lambdaInDrop(x, por)[3]
  lI10      <- lambdaIn(x, 10, por)[3]
  lI20      <- lambdaIn(x, 20, por)[3]
  lI50      <- lambdaIn(x, 50, por)[3]
  lI100     <- lambdaIn(x, 100, por)[3]
  
  # Out
  lO10      <- lambdaOut(x, 10, por)[3]
  lO20      <- lambdaOut(x, 20, por)[3]
  lO50      <- lambdaOut(x, 50, por)[3]
  lO100     <- lambdaOut(x, 100, por)[3]
  
  # Calcula frequencia de operacao por cassete (independente)
  # In
  pID <- thetaInDrop(x)
  
  pI10 <- thetaIn(x, 10)
  pI20 <- thetaIn(x, 20)
  pI50 <- thetaIn(x, 50)
  pI100 <- thetaIn(x, 100)
  
  # Out
  pO10 <- thetaOut(x, 10)
  pO20 <- thetaOut(x, 20)
  pO50 <- thetaOut(x, 50)
  pO100 <- thetaOut(x, 100)
  
  # Data frame com as medidas de interesse
  # (colsTimeStop <- noquote(paste0('timeStopCriterio',1:14,' = 0,')))
  saidaFull <- data.frame(m = 1:M, totDrop = 0,
                          totFaceA = 0, totFaceB = 0,
                          totFaceC = 0, totFaceD = 0,
                          timeStopCriterio1 = 0,  timeStopCriterio2 = 0,  
                          timeStopCriterio3 = 0,  timeStopCriterio4 = 0,
                          timeStopCriterio5 = 0,  timeStopCriterio6 = 0,  
                          timeStopCriterio7 = 0,  timeStopCriterio8 = 0, 
                          timeStopCriterio9 = 0,  timeStopCriterio10 = 0,
                          timeStopCriterio11 = 0, timeStopCriterio12 = 0,
                          timeStopCriterio13 = 0, timeStopCriterio14 = 0,
                          timeStop = 0, runtime = 0, lID = lID, 
                          lI10 = lI10, lI20 = lI20, lI50 = lI50, lI100 = lI100,
                          lO10 = lO10, lO20 = lO20, lO50 = lO50, lO100 = lO100)

  # Se não possui probabilidades para todos os k7
  if(is.na(pID * pI10 * pI20 * pI50 * pI100 * pO10 * pO20 * pO50 * pO100)){
    return(saidaFull)
  }
  
  # Barra de progresso com tempo decorrido
  pb <- progress_bar$new(
    format = "  progresso [:bar] :percent em :elapsed",
    total = M, clear = FALSE, width = 60)
  
  # m=1
  # Loop
  for(m in 1:M){
    ini <- Sys.time()
    
    # Vetores para armazenar as maxBernoulli simulacoes
    simDrop <- vector(length = maxBernoulli)
    simFaceA <- vector(length = maxBernoulli)
    simFaceB <- vector(length = maxBernoulli)
    simFaceC <- vector(length = maxBernoulli)
    simFaceD <- vector(length = maxBernoulli)

    # Cria vetor de cash in (TRUE: cash in, FALSE: cash out) via Bernoulli
    vetorCashIn <- rbernoulli(n = maxBernoulli, p = pIn)

    totCashIn <- sum(vetorCashIn)
    totCashOut <- sum(!vetorCashIn)
    inOut <- 2 * vetorCashIn - 1  # 1 fica 1, 0 vira -1
    
    # Decidindo quais cassetes irao operar
    if(tipo == 'a'){
      
      # Vetores de Cash In
      vetorInDrop <- rbernoulli(n = totCashIn, p = pID)
      vetorIn10   <- rbernoulli(n = totCashIn, p = pI10)
      vetorIn20   <- rbernoulli(n = totCashIn, p = pI20)
      vetorIn50   <- rbernoulli(n = totCashIn, p = pI50)
      vetorIn100  <- rbernoulli(n = totCashIn, p = pI100)
      
      # Vetores de Cash Out
      vetorOut10  <- rbernoulli(n = totCashOut, p = pO10)
      vetorOut20  <- rbernoulli(n = totCashOut, p = pO20)
      vetorOut50  <- rbernoulli(n = totCashOut, p = pO50)
      vetorOut100 <- rbernoulli(n = totCashOut, p = pO100)
      
    } else if (tipo == 'c'){
      vetorInDrop <- rep(TRUE, n = totCashIn)
      vetorIn10 <- rep(TRUE, n = totCashIn)
      vetorIn20 <- rep(TRUE, n = totCashIn)
      vetorIn50 <- rep(TRUE, n = totCashIn)
      vetorIn100 <- rep(TRUE, n = totCashIn)

      vetorOut10 <- rep(TRUE, n = totCashOut)
      vetorOut20 <- rep(TRUE, n = totCashOut)
      vetorOut50 <- rep(TRUE, n = totCashOut)
      vetorOut100 <- rep(TRUE, n = totCashOut)
    }

    # Gerando simulacoes de Poisson vetorizado
    # In
    simDrop[vetorCashIn] <- vetorInDrop*rpois(totCashIn, lID)
    simFaceA[vetorCashIn] <- vetorIn10*rpois(totCashIn, lI10)
    simFaceB[vetorCashIn] <- vetorIn20*rpois(totCashIn, lI20)
    simFaceC[vetorCashIn] <- vetorIn50*rpois(totCashIn, lI50)
    simFaceD[vetorCashIn] <- vetorIn100*rpois(totCashIn, lI100)
    
    # Out
    simFaceA[!vetorCashIn] <- vetorOut10*rpois(totCashOut, lO10)
    simFaceB[!vetorCashIn] <- vetorOut20*rpois(totCashOut, lO20)
    simFaceC[!vetorCashIn] <- vetorOut50*rpois(totCashOut, lO50)
    simFaceD[!vetorCashIn] <- vetorOut100*rpois(totCashOut, lO100)
    
    # Adicionando sinal
    simFaceASinal <- inOut*simFaceA
    simFaceBSinal <- inOut*simFaceB
    simFaceCSinal <- inOut*simFaceC
    simFaceDSinal <- inOut*simFaceD
    
    # Quantidades simuladas de cedulas por cassete atraves de somas acumuladas das colunas com zeros em acumulados negativos
    qtdFaceA <- cumsum(cumsumReset(simFaceASinal))
    qtdFaceB <- cumsum(cumsumReset(simFaceBSinal))
    qtdFaceC <- cumsum(cumsumReset(simFaceCSinal))
    qtdFaceD <- cumsum(cumsumReset(simFaceDSinal))
    
    # Total de cedulas
    totDrop <- dropQtdCedulasTotal + cumsum(simDrop)
    totFaceA <- k7QtdFaceA + qtdFaceA
    totFaceB <- k7QtdFaceB + qtdFaceB
    totFaceC <- k7QtdFaceC + qtdFaceC
    totFaceD <- k7QtdFaceD + qtdFaceD
    
    # Anotando a linha (i.e., por = 'horas') em que cada criterio foi atingido
    timeStopCriterio <- rep(NA, length = 14)
    
    # Lotacao k7 rejeito
    timeStopCriterio[1] <- which(totDrop > limRej)[1] 
    
    # # Lotacao FaceA e FaceB
    # timeStopCriterio[2] <- which(totFaceA > limRec &
    #                                totFaceB > limRec)[1]
    # 
    # # Lotacao FaceA e FaceC
    # timeStopCriterio[3] <- which(totFaceA > limRec &
    #                                totFaceC > limRec)[1]
    # 
    # # Lotacao FaceA e FaceD
    # timeStopCriterio[4] <- which(totFaceA > limRec &
    #                                totFaceD > limRec)[1]
    # 
    # # Lotacao FaceB e FaceC
    # timeStopCriterio[5] <- which(totFaceB > limRec &
    #                                totFaceC > limRec)[1]
    # 
    # # Lotacao FaceB e FaceD
    # timeStopCriterio[6] <- which(totFaceB > limRec &
    #                                totFaceD > limRec)[1]
    # 
    # # Lotacao FaceC e FaceD
    # timeStopCriterio[7] <- which(totFaceC > limRec &
    #                                totFaceD > limRec)[1]
    
    # Esvaziamento FaceA e FaceB
    timeStopCriterio[8] <- which(totFaceA == 0 &
                                   totFaceB == 0)[1] 
    
    # Esvaziamento FaceA e FaceC
    timeStopCriterio[9] <- which(totFaceA == 0 &
                                   totFaceC == 0)[1]
    
    # Esvaziamento FaceA e FaceD
    timeStopCriterio[10] <- which(totFaceA == 0 &
                                    totFaceD == 0)[1]
    
    # Esvaziamento FaceB e FaceC
    timeStopCriterio[11] <- which(totFaceB == 0 &
                                    totFaceC == 0)[1]
    
    # Esvaziamento FaceB e FaceD
    timeStopCriterio[12] <- which(totFaceB == 0 &
                                    totFaceD == 0)[1]
    
    # Esvaziamento FaceC e FaceD
    timeStopCriterio[13] <- which(totFaceC == 0 &
                                    totFaceD == 0)[1]
    
    # Posto > 200k
    sT <- totFaceA*10 + totFaceB*20 + totFaceC*50 + totFaceD*100
    ifelse(grepl('POSTO', x$segmento[1]), posto <- T, posto <- F)
    timeStopCriterio[14] <- ifelse(posto, which(sT > 200000)[1], NA)
    
    # timeStop
    timeStop <- min(min(timeStopCriterio, na.rm = T), maxBernoulli)
    
    # Contando o tempo do loop em segundos
    fim <- Sys.time()
    runtime <- as.numeric(fim-ini, units = 'secs')
    
    # Escrevendo no data frame
    saidaFull[m,2] <- totDrop[timeStop]
    saidaFull[m,3] <- totFaceA[timeStop]
    saidaFull[m,4] <- totFaceB[timeStop]
    saidaFull[m,5] <- totFaceC[timeStop]
    saidaFull[m,6] <- totFaceD[timeStop]
    saidaFull[m,7:20] <- timeStopCriterio
    saidaFull[m,21] <- timeStop
    saidaFull[m,22] <- runtime
    
    # Atualizando a barra de progresso
    pb$tick()
  }
  return(saidaFull)
}

# Compila a funcao
# simStop3 <- cmpfun(f = simStop3, options = list(optimize = 3))

#####
# Indices de fim de cada ciclo
#####
indexOut <- function(x, coluna = 'cicloDrop'){
  n <- length(unique(x[[coluna]]))
  wmax <- by(x[['dataHoraCaptura']], x[[coluna]], which.max)
  return(cbind(cicloDrop = 0:(n-1), indexOut = cumsum(wmax)))
  
}
# indexOut <- cmpfun(f = indexOut, options = list(optimize = 3)) # Compila a funcao

# head(indexOut(AtmTemp))

#####
# Indices de fim de cada ciclo
#####
indexOut <- function(x, coluna = 'cicloDrop'){
  n <- length(unique(x[[coluna]]))
  wmax <- by(x[['dataHoraCaptura']], x[[coluna]], which.max)
  return(cbind(cicloDrop = 0:(n-1), indexOut = cumsum(wmax)))
  
}
# indexOut <- cmpfun(f = indexOut, options = list(optimize = 3)) # Compila a funcao


#####
# Indices de inicio de cada ciclo
#####
indexIn <- function(x, coluna = 'cicloDrop'){
  n <- length(unique(x[[coluna]]))
  indexIn <- c(1, indexOut(x, coluna)[1:(n-1),'indexOut']+1)
  return(cbind(cicloDrop = 0:(n-1), indexIn = indexIn))
}
# indexIn <- cmpfun(f = indexIn, options = list(optimize = 3)) # Compila a funcao

# head(indexIn(AtmTemp))

#####
# Tamanhos de ciclo
#####
tempoCiclo <- function(x, coluna = 'cicloDrop'){
  n <- length(unique(x[[coluna]]))
  iI <- x[indexIn(x)[,2],'dataHoraCaptura']
  iO <- x[indexOut(x)[,2],'dataHoraCaptura']
  return(data.frame(cicloDrop = 0:(n-1),
                    dataHoraCapturaIn = iI,
                    dataHoraCapturaOut = iO,
                    tempoCicloMinuto = lambdaInCiclo(x, 10, por = 'minuto')[,3],
                    tempoCicloHora = lambdaInCiclo(x, 10, por = 'hora')[,3],
                    tempoCicloDia = lambdaInCiclo(x, 10, por = 'dia')[,3],
                    tempoCicloSemana = lambdaInCiclo(x, 10, por = 'semana')[,3],
                    tempoCicloMes = lambdaInCiclo(x, 10, por = 'mes')[,3]) 
  )
}
# tempoCiclo <- cmpfun(f = tempoCiclo, options = list(optimize = 3)) # Compila a funcao


#####
# Inicio, fim e tamanho do ciclo
#####
iniFimCiclo <- function(x){
  intCiclo <- unlist(by(x$dataHoraCaptura, x$cicloDrop, range))
  intCiclo <- as.data.frame(matrix(intCiclo, ncol = 2, byrow = T))
  colnames(intCiclo) <- c('iniCiclo','fimCiclo')
  intCiclo[,1] <- as.POSIXct(intCiclo[,1], origin = '1970-01-01', tz = 'GMT')
  intCiclo[,2] <- as.POSIXct(intCiclo[,2], origin = '1970-01-01', tz = 'GMT')
  return(cbind(cicloDrop = unique(x$cicloDrop), intCiclo, 
               tamCicloSeg = as.numeric(intCiclo[,2]-intCiclo[,1], units = 'secs')))
}
# iniFimCiclo <- cmpfun(f = iniFimCiclo, options = list(optimize = 3)) # Compila a funcao


#####
# Fracao operante por ciclo
#####
fracOp <- function(atm = AtmInter[1]){
  
  # Diferenca de dataHoraParada
  paradaLista[[atm]]$difDataHoraParada <- c(diff(paradaLista[[atm]]$dataHoraParada),0)
  
  # Criando objetos uteis
  nCiclo <- length(cicloLista[[atm]]$cicloDrop)
  tempoInativo <- rep(0, nCiclo)
  tempoAtivo <- rep(0, nCiclo)
  tempoTotal <- rep(0, nCiclo)
  percInat <- rep(0, nCiclo)
  percAt <- rep(0, nCiclo)
  tamCicloEf <- rep(0, nCiclo)
  pL <- paradaLista[[atm]]
  
  # Loop
  for(i in 1:nCiclo){
    
    cL <- cicloLista[[atm]][i,]
    filtro <- pL$dataHoraParada >= cL$iniCiclo & pL$dataHoraParada <= cL$fimCiclo
    
    tempoInativo[i] <- sum(pL[filtro,]$difDataHoraParada[which(pL[filtro,]$FL_ATM_ATIVO == 'N')])
    tempoAtivo[i] <- sum(pL[filtro,]$difDataHoraParada[which(pL[filtro,]$FL_ATM_ATIVO == 'S')])
    tempoTotal[i] <- tempoInativo[i] + tempoAtivo[i]
    
    percInat[i] <- as.numeric(tempoInativo[i], units = 'secs')/as.numeric(tempoTotal[i], units = 'secs')
    percAt[i] <- as.numeric(tempoAtivo[i], units = 'secs')/as.numeric(tempoTotal[i], units = 'secs')
    
    ifelse(is.nan(percAt[i]), tamCicloEf[i] <- cL$tamCiclo, tamCicloEf[i] <- cL$tamCiclo*percAt[i])
  }
  
  return(cbind(cicloLista[[atm]], tempoInativo = tempoInativo, tempoAtivo = tempoAtivo,
               tempoTotal = tempoTotal, percInat = percInat, percAt = percAt, tamCicloEf = tamCicloEf))

}
# fracOp <- cmpfun(f = fracOp, options = list(optimize = 3)) # Compila a funcao

#####
# Cria tabela inicializada - FATO_PREVISAO_ABAST
#####
CreateTable.FatoPrevisaoAbast <- function(){
  return(data.frame(ID_DATA        = 0,
                    COD_ATM        = 0,
                    NUM_OBS        = 0,
                    DEC_0          = 0,
                    DEC_10         = 0,
                    DEC_20         = 0,
                    DEC_30         = 0,
                    DEC_40         = 0,
                    DEC_50         = 0,
                    DEC_60         = 0,
                    DEC_70         = 0,
                    DEC_80         = 0,
                    DEC_90         = 0,
                    DEC_100        = 0,
                    RUNTIME        = 0,
                    ATIVO          = 0,
                    LAMBDA_IN_DROP = 0,
                    LAMBDA_IN_A    = 0,
                    LAMBDA_IN_B    = 0,
                    LAMBDA_IN_C    = 0,
                    LAMBDA_IN_D    = 0,
                    LAMBDA_OUT_A   = 0,
                    LAMBDA_OUT_B   = 0,
                    LAMBDA_OUT_C   = 0,
                    LAMBDA_OUT_D   = 0,
                    K7QTDFACE_A    = 0,
                    K7QTDFACE_B    = 0,
                    K7QTDFACE_C    = 0,    
                    K7QTDFACE_D    = 0,
                    ID_HORA        = 0,
                    K7QTD_DROP     = 0))
}
# Compila a funcao
# CreateTable.FatoPrevisaoAbast <- cmpfun(f = CreateTable.FatoPrevisaoAbast, options = list(optimize = 3)) 


#####
# Cria tabela inicializada - FATO_MOTIVO_PARADA_ATM
#####
CreateTable.FatoMotivoParadaAtm <- function(){
  return(data.frame(ID_DATA        = 0,
                    COD_ATM        = 0,
                    QTD_CRITERIO_1 = 0,
                    QTD_CRITERIO_2 = 0,
                    QTD_CRITERIO_3 = 0,
                    QTD_CRITERIO_4 = 0,
                    QTD_CRITERIO_5 = 0,
                    QTD_CRITERIO_6 = 0,
                    QTD_CRITERIO_7 = 0,
                    QTD_CRITERIO_8 = 0,
                    QTD_CRITERIO_9 = 0,
                    QTD_CRITERIO_10 = 0,
                    QTD_CRITERIO_11 = 0,
                    QTD_CRITERIO_12 = 0,
                    QTD_CRITERIO_13 = 0,
                    QTD_CRITERIO_14 = 0))
}
# Compila a funcao
# CreateTable.FatoMotivoParadaAtm <- cmpfun(f = CreateTable.FatoMotivoParadaAtm, options = list(optimize = 3))

#####
# Fim
#####