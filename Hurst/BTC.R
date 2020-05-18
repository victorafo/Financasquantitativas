#Bibliotecas
library (TTR)
library(quantmod)
library(e1071)
library(data.table)
library(clue) #Predict para o K-Means
library(factoextra) #Visualizar o K-Means
library(ggplot2)
library(tseries)
library(lubridate)
library(tidyverse)
library(pracma) # Hurst

Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

#################
#Carregando o Dataset
BTC <- fread("BTC_USD_Bitfinex_Historical_Data.csv", header = TRUE, sep = ",")
BTC <- data.frame(BTC)
#Renomeando o Data Frame
names(BTC) <- c("data", "fechamento", "abertura", "maxima", "minima", "volume", "variacao")
#Tratamento dos dados
BTC <- BTC[nrow(BTC):1,]
BTC$data <- gsub(',','', BTC$data, fixed = TRUE)
BTC$fechamento <- gsub(',','', BTC$fechamento, fixed = TRUE)
BTC$abertura <- gsub(',','', BTC$abertura, fixed = TRUE)
BTC$maxima <- gsub(',','', BTC$maxima, fixed = TRUE)
BTC$minima <- gsub(',','', BTC$minima, fixed = TRUE)
BTC$volume <- gsub('K','', BTC$volume, fixed = TRUE)
BTC$variacao <- gsub('%', '', BTC$variacao, fixed = TRUE) 

BTC$fechamento <- as.numeric(BTC$fechamento)
BTC$abertura <- as.numeric(BTC$abertura)
BTC$maxima <- as.numeric(BTC$maxima)
BTC$minima <- as.numeric(BTC$minima)
BTC$volume <- as.numeric(BTC$volume)
BTC$variacao <- as.numeric(BTC$variacao)

#names(BTC)[1] <- "DATE"
BTC$data <- as.POSIXct(strptime(BTC$data, format= "%b %d %Y"))

#################

BTC$Rsi <- RSI(BTC$fechamento,14,"SMA")
BTC$RsiH <- ifelse(BTC$Rsi> 70,1,0)
BTC$RsiL <- ifelse(BTC$Rsi< 30,1,0)
BTC$CCI <- CCI(BTC$fechamento,20)
BTC$CCIH <- ifelse(BTC$CCI > 120,1,0)
BTC$CCIL <- ifelse(BTC$CCI < -120,1,0)
BTC$Macd <- MACD(BTC$fechamento,12,26,9,"SMA") 
BTC$Macd <- BTC$Macd[,1] - BTC$Macd[,2] 
BTC$MacdH <- ifelse(BTC$Macd > 2,1,0)
BTC$MacdL <- ifelse(BTC$Macd < -2,1,0)
BTC$Bbands <- BBands(BTC$fechamento,20,"SMA",2)
BTC$ROC <- ROC(BTC$fechamento, type = "continuous", na.pad = TRUE)
BTC$EMA <- EMA(BTC$fechamento, n = 9, wilder = FALSE, ratio = NULL)
BTC$SMA <- SMA(BTC$fechamento, n = 21)
BTC$ALMA <- ALMA(BTC$fechamento, n = 1, offset = 0.85, sigma = 6)


BTC$Fechamento_deslocado <- BTC$fechamento
BTC['Fechamento_deslocado'] <- c(NA, head(BTC['Fechamento_deslocado'], dim(BTC)[1] - 1)[[1]])
BTC$Fechamento_deslocado2 <- BTC$Fechamento_deslocado
BTC['Fechamento_deslocado2'] <- c(NA, head(BTC['Fechamento_deslocado2'], dim(BTC)[1] - 1)[[1]])
BTC$Fechamento_deslocado3 <- BTC$Fechamento_deslocado2
BTC['Fechamento_deslocado3'] <- c(NA, head(BTC['Fechamento_deslocado3'], dim(BTC)[1] - 1)[[1]])
BTC$Fechamento_deslocado4 <- BTC$Fechamento_deslocado3
BTC['Fechamento_deslocado4'] <- c(NA, head(BTC['Fechamento_deslocado4'], dim(BTC)[1] - 1)[[1]])
BTC$Fechamento_deslocado5 <- BTC$Fechamento_deslocado4
BTC['Fechamento_deslocado5'] <- c(NA, head(BTC['Fechamento_deslocado5'], dim(BTC)[1] - 1)[[1]])
BTC$Fechamento_deslocado6 <- BTC$Fechamento_deslocado5
BTC['Fechamento_deslocado6'] <- c(NA, head(BTC['Fechamento_deslocado6'], dim(BTC)[1] - 1)[[1]])
BTC$Fechamento_deslocado7 <- BTC$Fechamento_deslocado6
BTC['Fechamento_deslocado7'] <- c(NA, head(BTC['Fechamento_deslocado7'], dim(BTC)[1] - 1)[[1]])
BTC$Fechamento_deslocado8 <- BTC$Fechamento_deslocado7
BTC['Fechamento_deslocado8'] <- c(NA, head(BTC['Fechamento_deslocado8'], dim(BTC)[1] - 1)[[1]])
BTC$Fechamento_deslocado9 <- BTC$Fechamento_deslocado8
BTC['Fechamento_deslocado9'] <- c(NA, head(BTC['Fechamento_deslocado9'], dim(BTC)[1] - 1)[[1]])
BTC$Fechamento_deslocado10 <- BTC$Fechamento_deslocado9
BTC['Fechamento_deslocado10'] <- c(NA, head(BTC['Fechamento_deslocado10'], dim(BTC)[1] - 1)[[1]])
BTC$Return <- (BTC$fechamento/BTC$Fechamento_deslocado-1)
#BTC <- na.omit(BTC)
# Hurst exponent
#hurst_BTC <- hurstexp(BTC$Return)
BTC$HURST <- NA
aux <- 1
for (i in seq(5, dim(BTC)[1])) {
  BTC$HURST[i] <- hurstexp(log(BTC$fechamento[aux:i]))$Hal
  aux <- aux + 1
}

  #Criando o alvo
desloca <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

BTC$Alvo1 <- desloca(BTC$Return, 1)

BTC$Bbands <- BTC$Bbands[,4]

BTC_train <- na.omit(BTC[1:299,]) #Treinamento entre 2017 - Jun/2018
BTC_test <- BTC[300:dim(BTC)[1],] #Teste entre Jul/2018 - Jun/2019


#####
#Resultados na base de treinamento e inspeçao dos clusters
#Para achar o numero otimo de clusters
set.seed(42)
k.max <- 10
wss<- sapply(1:k.max,function(k){kmeans(BTC_train[,8:18],k,nstart = 20,iter.max = 20)$tot.withinss})
plot(1:k.max,wss, type= "b", xlab = "Numero de clusters(k)", ylab = "Soma do erro quadrado médio")

#Rodar o K-Means
clusters <- kmeans(BTC_train[,8:18], centers = 9, nstart = 34)

print(clusters)

BTC_train$clusters <- clusters$cluster

aggregate(Alvo1*100 ~ clusters, BTC_train, mean)


#Predicao dos resultados na base de teste
BTC_test$Cluster_predicao <- cl_predict(clusters,BTC_test[,8:18])
BTC_test$AlvoBIN <- ifelse(BTC_test$Alvo1 > 0,1,0)

#round(prop.table(table(BTC_test$AlvoBIN,BTC_test$Cluster_predicao),1)*100,2)

retorno_BH_acumulado <- exp(cumsum(BTC_test$Return))-1

retorno_kmeans <- ifelse(  BTC_test$Cluster_predicao == 8|
                           BTC_test$Cluster_predicao == 9|
                           BTC_test$Cluster_predicao == 1
                            
                         ,-BTC_test$Alvo1,BTC_test$Alvo1)
retorno_kmeans_acumulado <- exp(cumsum(retorno_kmeans))-1

#####
#Grafico da Performance
#par(mfrow=c(2,2))
plot(retorno_kmeans_acumulado*100, type = "l", col = "green", lwd = 2,
     main = "BTC - K-Means x B&H",
     xlab = "2018 - Jun/2019",
     ylab = "Retorno em %",
     ylim = c(-150,3000))
lines(retorno_BH_acumulado*100,col = "blue", lwd = 2)
abline(h=0,col = "black", lwd = 0.5)


tail(retorno_kmeans_acumulado)
tail(retorno_BH_acumulado)
tail(BTC_test$Cluster_predicao,1)
##############################
#HURST

TREINAMENTO <- BTC_train %>% select(Rsi,RsiH,RsiL,CCI,CCIH,CCIL,Macd,MacdH,MacdL,Bbands,ROC,HURST,Alvo1)
TESTE <- BTC_test %>% select(Rsi,RsiH,RsiL,CCI,CCIH,CCIL,Macd,MacdH,MacdL,Bbands,ROC,HURST,Alvo1)

#####
#Resultados na base de treinamento e inspeçao dos clusters
#Para achar o numero otimo de clusters
set.seed(42)
k.max <- 10
wss<- sapply(1:k.max,function(k){kmeans(TREINAMENTO[,1:12],k,nstart = 20,iter.max = 20)$tot.withinss})
plot(1:k.max,wss, type= "b", xlab = "Numero de clusters(k)", ylab = "Soma do erro quadrado médio")

#Rodar o K-Means
clusters <- kmeans(TREINAMENTO[,1:12], centers = 9, nstart = 34)

print(clusters)

TREINAMENTO$clusters <- clusters$cluster

aggregate(Alvo1*100 ~ clusters, TREINAMENTO, mean)


#Predicao dos resultados na base de teste
TESTE$Cluster_predicao <- cl_predict(clusters,TESTE[,1:12])
TESTE$AlvoBIN <- ifelse(TESTE$Alvo1 > 0,1,0)

#round(prop.table(table(TESTE$AlvoBIN,TESTE$Cluster_predicao),1)*100,2)

retorno_BH_acumulado <- exp(cumsum(TESTE$Return))-1

retorno_kmeans <- ifelse(  TESTE$Cluster_predicao == 8|
                             TESTE$Cluster_predicao == 9|
                             TESTE$Cluster_predicao == 1
                           
                           ,-TESTE$Alvo1,TESTE$Alvo1)
retorno_kmeans_acumulado <- exp(cumsum(retorno_kmeans))-1

#####
#Grafico da Performance
#par(mfrow=c(2,2))
plot(retorno_kmeans_acumulado*100, type = "l", col = "green", lwd = 2,
     main = "BTC - K-Means x B&H",
     xlab = "2018 - Jun/2019",
     ylab = "Retorno em %",
     ylim = c(-150,3000))
lines(retorno_BH_acumulado*100,col = "blue", lwd = 2)
abline(h=0,col = "black", lwd = 0.5)


tail(retorno_kmeans_acumulado)
tail(retorno_BH_acumulado)
tail(TESTE$Cluster_predicao,1)
