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

Sys.setlocale(category = "LC_ALL", locale = en_us.utf-8 )

#################
#Carregando o Dataset
AAON <- fread("AAON_Historical_Data.csv", header = TRUE, sep = ",")
AAON <- data.frame(AAON)
#Renomeando o Data Frame
names(AAON) <- c("data", "fechamento", "abertura", "maxima", "minima", "volume", "variacao")
#Tratamento dos dados
AAON <- AAON[nrow(AAON):1,]
AAON$data <- gsub(',','', AAON$data, fixed = TRUE)
AAON$fechamento <- gsub(',','', AAON$fechamento, fixed = TRUE)
AAON$abertura <- gsub(',','', AAON$abertura, fixed = TRUE)
AAON$maxima <- gsub(',','', AAON$maxima, fixed = TRUE)
AAON$minima <- gsub(',','', AAON$minima, fixed = TRUE)
AAON$volume <- gsub('K','', AAON$volume, fixed = TRUE)
AAON$variacao <- gsub('%', '', AAON$variacao, fixed = TRUE) 

AAON$fechamento <- as.numeric(AAON$fechamento)
AAON$abertura <- as.numeric(AAON$abertura)
AAON$maxima <- as.numeric(AAON$maxima)
AAON$minima <- as.numeric(AAON$minima)
#AAON$volume <- as.numeric(AAON$volume)
AAON$variacao <- as.numeric(AAON$variacao)

#names(AAON)[1] <- "DATE"
AAON$data <- as.POSIXct(strptime(AAON$data, format= "%b %d %Y"))

#################

AAON$Rsi <- RSI(AAON$fechamento,14,"SMA")
AAON$RsiH <- ifelse(AAON$Rsi> 70,1,0)
AAON$RsiL <- ifelse(AAON$Rsi< 30,1,0)
AAON$CCI <- CCI(AAON$fechamento,20)
AAON$CCIH <- ifelse(AAON$CCI > 120,1,0)
AAON$CCIL <- ifelse(AAON$CCI < -120,1,0)
AAON$Macd <- MACD(AAON$fechamento,12,26,9,"SMA") 
AAON$Macd <- AAON$Macd[,1] - AAON$Macd[,2] 
AAON$MacdH <- ifelse(AAON$Macd > 2,1,0)
AAON$MacdL <- ifelse(AAON$Macd < -2,1,0)
AAON$Bbands <- BBands(AAON$fechamento,20,"SMA",2)
AAON$ROC <- ROC(AAON$fechamento, type = "continuous", na.pad = TRUE)
AAON$EMA <- EMA(AAON$fechamento, n = 9, wilder = FALSE, ratio = NULL)
AAON$SMA <- SMA(AAON$fechamento, n = 21)
AAON$ALMA <- ALMA(AAON$fechamento, n = 1, offset = 0.85, sigma = 6)


AAON$Fechamento_deslocado <- AAON$fechamento
AAON['Fechamento_deslocado'] <- c(NA, head(AAON['Fechamento_deslocado'], dim(AAON)[1] - 1)[[1]])
AAON$Fechamento_deslocado2 <- AAON$Fechamento_deslocado
AAON['Fechamento_deslocado2'] <- c(NA, head(AAON['Fechamento_deslocado2'], dim(AAON)[1] - 1)[[1]])
AAON$Fechamento_deslocado3 <- AAON$Fechamento_deslocado2
AAON['Fechamento_deslocado3'] <- c(NA, head(AAON['Fechamento_deslocado3'], dim(AAON)[1] - 1)[[1]])
AAON$Fechamento_deslocado4 <- AAON$Fechamento_deslocado3
AAON['Fechamento_deslocado4'] <- c(NA, head(AAON['Fechamento_deslocado4'], dim(AAON)[1] - 1)[[1]])
AAON$Fechamento_deslocado5 <- AAON$Fechamento_deslocado4
AAON['Fechamento_deslocado5'] <- c(NA, head(AAON['Fechamento_deslocado5'], dim(AAON)[1] - 1)[[1]])
AAON$Fechamento_deslocado6 <- AAON$Fechamento_deslocado5
AAON['Fechamento_deslocado6'] <- c(NA, head(AAON['Fechamento_deslocado6'], dim(AAON)[1] - 1)[[1]])
AAON$Fechamento_deslocado7 <- AAON$Fechamento_deslocado6
AAON['Fechamento_deslocado7'] <- c(NA, head(AAON['Fechamento_deslocado7'], dim(AAON)[1] - 1)[[1]])
AAON$Fechamento_deslocado8 <- AAON$Fechamento_deslocado7
AAON['Fechamento_deslocado8'] <- c(NA, head(AAON['Fechamento_deslocado8'], dim(AAON)[1] - 1)[[1]])
AAON$Fechamento_deslocado9 <- AAON$Fechamento_deslocado8
AAON['Fechamento_deslocado9'] <- c(NA, head(AAON['Fechamento_deslocado9'], dim(AAON)[1] - 1)[[1]])
AAON$Fechamento_deslocado10 <- AAON$Fechamento_deslocado9
AAON['Fechamento_deslocado10'] <- c(NA, head(AAON['Fechamento_deslocado10'], dim(AAON)[1] - 1)[[1]])
AAON$Return <- (AAON$fechamento/AAON$Fechamento_deslocado-1)

#Criando o alvo
desloca <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

AAON$Alvo1 <- desloca(AAON$Return, 1)

AAON$Bbands <- AAON$Bbands[,4]

AAON_train <- na.omit(AAON[1:430,]) #Treinamento entre Jan/2017 - Out/2019
AAON_test <- AAON[431:dim(AAON)[1],] #Teste entre Out/2019 - Hoje

#####
#Resultados na base de treinamento e inspe?ao dos clusters
#Para achar o numero otimo de clusters
set.seed(42)
k.max <- 10
wss<- sapply(1:k.max,function(k){kmeans(AAON_train[,8:18],k,nstart = 20,iter.max = 20)$tot.withinss})
plot(1:k.max,wss, type= "b", xlab = "Numero de clusters(k)", ylab = "Soma do erro quadrado m?dio")

names(AAON_train)

#Rodar o K-Means
clusters <- kmeans(AAON_train[,8:18], centers = 9, nstart = 34)

print(clusters)

AAON_train$clusters <- clusters$cluster

aggregate(Alvo1*100 ~ clusters, AAON_train, mean)


#Predicao dos resultados na base de teste
AAON_test$Cluster_predicao <- cl_predict(clusters,AAON_test[,8:18])
AAON_test$AlvoBIN <- ifelse(AAON_test$Alvo1 > 0,1,0)

#round(prop.table(table(AAON_test$AlvoBIN,AAON_test$Cluster_predicao),1)*100,2)

retorno_BH_acumulado <- exp(cumsum(AAON_test$Return))-1

retorno_kmeans <- ifelse(  
                                AAON_test$Cluster_predicao == 8|
                                AAON_test$Cluster_predicao == 7|
                                AAON_test$Cluster_predicao == 4|
                                AAON_test$Cluster_predicao == 3|
                             AAON_test$Cluster_predicao == 1
                            
                         ,-AAON_test$Alvo1,AAON_test$Alvo1)
retorno_kmeans_acumulado <- exp(cumsum(retorno_kmeans))-1
ks.test(retorno_kmeans,AAON_test$Alvo1)
ks.test(retorno_kmeans,-AAON_test$Alvo1)
#####
#Grafico da Performance
#par(mfrow=c(2,2))
plot(retorno_kmeans_acumulado*100, type = "l", col = "green", lwd = 2,
     main = "AAON - K-Means x B&H",
     xlab = "Out 2019 - Hoje",
     ylab = "Retorno em %",
     ylim = c(-150,500))
lines(retorno_BH_acumulado*100,col = "blue", lwd = 2)
abline(h=0,col = "black", lwd = 0.5)


tail(retorno_kmeans_acumulado)
tail(retorno_BH_acumulado)

venda= 123478
compra= 569
tail(AAON_test$Cluster_predicao,1)

