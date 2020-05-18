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
LTC <- fread("LTC_USD_Bitfinex_Historical_Data.csv", header = TRUE, sep = ",")
LTC <- data.frame(LTC)
#Renomeando o Data Frame
names(LTC) <- c("data", "fechamento", "abertura", "maxima", "minima", "volume", "variacao")
#Tratamento dos dados
LTC <- LTC[nrow(LTC):1,]
LTC$data <- gsub(',','', LTC$data, fixed = TRUE)
LTC$fechamento <- gsub(',','', LTC$fechamento, fixed = TRUE)
LTC$abertura <- gsub(',','', LTC$abertura, fixed = TRUE)
LTC$maxima <- gsub(',','', LTC$maxima, fixed = TRUE)
LTC$minima <- gsub(',','', LTC$minima, fixed = TRUE)
LTC$volume <- gsub('K','', LTC$volume, fixed = TRUE)
LTC$variacao <- gsub('%', '', LTC$variacao, fixed = TRUE) 

LTC$fechamento <- as.numeric(LTC$fechamento)
LTC$abertura <- as.numeric(LTC$abertura)
LTC$maxima <- as.numeric(LTC$maxima)
LTC$minima <- as.numeric(LTC$minima)
LTC$volume <- as.numeric(LTC$volume)
LTC$variacao <- as.numeric(LTC$variacao)

#names(LTC)[1] <- "DATE"
LTC$data <- as.POSIXct(strptime(LTC$data, format= "%b %d %Y"))

#################

LTC$Rsi <- RSI(LTC$fechamento,14,"SMA")
LTC$RsiH <- ifelse(LTC$Rsi> 70,1,0)
LTC$RsiL <- ifelse(LTC$Rsi< 30,1,0)
LTC$CCI <- CCI(LTC$fechamento,20)
LTC$CCIH <- ifelse(LTC$CCI > 120,1,0)
LTC$CCIL <- ifelse(LTC$CCI < -120,1,0)
LTC$Macd <- MACD(LTC$fechamento,12,26,9,"SMA") 
LTC$Macd <- LTC$Macd[,1] - LTC$Macd[,2] 
LTC$MacdH <- ifelse(LTC$Macd > 2,1,0)
LTC$MacdL <- ifelse(LTC$Macd < -2,1,0)
LTC$Bbands <- BBands(LTC$fechamento,20,"SMA",2)
LTC$ROC <- ROC(LTC$fechamento, type = "continuous", na.pad = TRUE)
LTC$EMA <- EMA(LTC$fechamento, n = 9, wilder = FALSE, ratio = NULL)
LTC$SMA <- SMA(LTC$fechamento, n = 21)
LTC$ALMA <- ALMA(LTC$fechamento, n = 1, offset = 0.85, sigma = 6)


LTC$Fechamento_deslocado <- LTC$fechamento
LTC['Fechamento_deslocado'] <- c(NA, head(LTC['Fechamento_deslocado'], dim(LTC)[1] - 1)[[1]])
LTC$Fechamento_deslocado2 <- LTC$Fechamento_deslocado
LTC['Fechamento_deslocado2'] <- c(NA, head(LTC['Fechamento_deslocado2'], dim(LTC)[1] - 1)[[1]])
LTC$Fechamento_deslocado3 <- LTC$Fechamento_deslocado2
LTC['Fechamento_deslocado3'] <- c(NA, head(LTC['Fechamento_deslocado3'], dim(LTC)[1] - 1)[[1]])
LTC$Fechamento_deslocado4 <- LTC$Fechamento_deslocado3
LTC['Fechamento_deslocado4'] <- c(NA, head(LTC['Fechamento_deslocado4'], dim(LTC)[1] - 1)[[1]])
LTC$Fechamento_deslocado5 <- LTC$Fechamento_deslocado4
LTC['Fechamento_deslocado5'] <- c(NA, head(LTC['Fechamento_deslocado5'], dim(LTC)[1] - 1)[[1]])
LTC$Fechamento_deslocado6 <- LTC$Fechamento_deslocado5
LTC['Fechamento_deslocado6'] <- c(NA, head(LTC['Fechamento_deslocado6'], dim(LTC)[1] - 1)[[1]])
LTC$Fechamento_deslocado7 <- LTC$Fechamento_deslocado6
LTC['Fechamento_deslocado7'] <- c(NA, head(LTC['Fechamento_deslocado7'], dim(LTC)[1] - 1)[[1]])
LTC$Fechamento_deslocado8 <- LTC$Fechamento_deslocado7
LTC['Fechamento_deslocado8'] <- c(NA, head(LTC['Fechamento_deslocado8'], dim(LTC)[1] - 1)[[1]])
LTC$Fechamento_deslocado9 <- LTC$Fechamento_deslocado8
LTC['Fechamento_deslocado9'] <- c(NA, head(LTC['Fechamento_deslocado9'], dim(LTC)[1] - 1)[[1]])
LTC$Fechamento_deslocado10 <- LTC$Fechamento_deslocado9
LTC['Fechamento_deslocado10'] <- c(NA, head(LTC['Fechamento_deslocado10'], dim(LTC)[1] - 1)[[1]])
LTC$Return <- (LTC$fechamento/LTC$Fechamento_deslocado-1)

#Criando o alvo
desloca <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

LTC$Alvo1 <- desloca(LTC$Return, 1)

LTC$Bbands <- LTC$Bbands[,4]

LTC_train <- na.omit(LTC[1:971,]) #Treinamento entre 2017 - Jun/2018
LTC_test <- LTC[972:dim(LTC)[1],] #Teste entre Jul/2018 - Jun/2019

#####
#Resultados na base de treinamento e inspe?ao dos clusters
#Para achar o numero otimo de clusters
set.seed(42)
k.max <- 10
wss<- sapply(1:k.max,function(k){kmeans(LTC_train[,8:18],k,nstart = 20,iter.max = 20)$tot.withinss})
plot(1:k.max,wss, type= "b", xlab = "Numero de clusters(k)", ylab = "Soma do erro quadrado m?dio")

names(LTC_train)

#Rodar o K-Means
clusters <- kmeans(LTC_train[,8:18], centers = 9, nstart = 34)

print(clusters)

LTC_train$clusters <- clusters$cluster

aggregate(Alvo1*100 ~ clusters, LTC_train, mean)


#Predicao dos resultados na base de teste
LTC_test$Cluster_predicao <- cl_predict(clusters,LTC_test[,8:18])
LTC_test$AlvoBIN <- ifelse(LTC_test$Alvo1 > 0,1,0)

#round(prop.table(table(LTC_test$AlvoBIN,LTC_test$Cluster_predicao),1)*100,2)

retorno_BH_acumulado <- exp(cumsum(LTC_test$Return))-1

retorno_kmeans <- ifelse(  
                             LTC_test$Cluster_predicao == 9|
                               LTC_test$Cluster_predicao == 8|
                               LTC_test$Cluster_predicao == 2|
                               LTC_test$Cluster_predicao == 1
                             
                            
                         ,LTC_test$Alvo1,-LTC_test$Alvo1)
retorno_kmeans_acumulado <- exp(cumsum(retorno_kmeans))-1
ks.test(retorno_kmeans,LTC_test$Alvo1)
ks.test(retorno_kmeans,-LTC_test$Alvo1)
#####
#Grafico da Performance
#par(mfrow=c(2,2))
plot(retorno_kmeans_acumulado*100, type = "l", col = "green", lwd = 2,
     main = "LTC - K-Means x B&H",
     xlab = "2018 - Jun/2019",
     ylab = "Retorno em %",
     ylim = c(-150,2000))
lines(retorno_BH_acumulado*100,col = "blue", lwd = 2)
abline(h=0,col = "black", lwd = 0.5)
tail(retorno_kmeans_acumulado)
tail(retorno_BH_acumulado)
venda= 34567
compra= 1289
tail(LTC_test$Cluster_predicao,1)
