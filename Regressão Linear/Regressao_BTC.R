#Bibliotecas
library (TTR)
library(reshape2)
library(corrplot)
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

Sys.setlocale(category = "LC_TIME", "en_us")

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
BTC$volume <- NULL
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
#BTC$ALMA <- ALMA(BTC$fechamento, n = 1, offset = 0.85, sigma = 6)

#Criando o alvo
BTC$Fechamento_deslocado <- BTC$fechamento
BTC['Fechamento_deslocado'] <- c(NA, head(BTC['Fechamento_deslocado'], dim(BTC)[1] - 1)[[1]])
BTC$Return <- (BTC$fechamento/BTC$Fechamento_deslocado-1)

desloca <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

BTC$Alvo1 <- desloca(BTC$Return, 1)

BTC$Bbands <- BTC$Bbands[,4]

###########################################
#filtros
#BTC <- subset(BTC, volume > 0)
#BTC <- subset(BTC, candle != 0)

BTC_train <- BTC[1:700,] #327, 350, 360
BTC_test <- BTC[701:dim(BTC)[1],]

##### testes de regress?o
#Regressao  
regressao <- lm(Alvo1 ~ ., data = BTC_train)
sumario <- summary(regressao)
r.squared <- sumario$r.squared
fstatistic <- sumario$fstatistic
sumario
par(mfrow=c(2,2))
plot(regressao)

#####
#Selecionando apenas as variaveis com significancia menor que 1%
melhores_vars <- sumario$coeff[-1,4] < 0.1
melhores_vars <- names(melhores_vars)[melhores_vars == TRUE] 
#Nao se esqueca de adicionar o alvo
melhores_vars <- c("Alvo1",melhores_vars)
train_melhores <- BTC_train[melhores_vars] 

#Regressao2  
regressao2 <- lm(Alvo1 ~ ., data = train_melhores)
sumario2 <- summary(regressao2)
r.squared2 <- sumario2$r.squared
fstatistic2 <- sumario2$fstatistic
sumario2

par(mfrow=c(2,2))
plot(regressao2)


#Regressao3  
#regressao3 <- lm(Alvo ~ Rsi + RsiH + RSL + Bbands [,4] + EMA, data = BTC_train)
#sumario3 <- summary(regressao3)
#r.squared3 <- sumario3$r.squared
#fstatistic3 <- sumario3$fstatistic
#sumario3

#par(mfrow=c(2,2))
#plot(regressao3)

#####
#Predicao dos resultados na base de teste
BTC_test$predicao <- predict(regressao,BTC_test)
BTC_test$predicao2 <- predict(regressao2,BTC_test)
#BTC_test$predicao3 <- predict(regressao3,BTC_test)
BTC_test <- na.omit(BTC_test)
BTC_test$AlvoBIN <- ifelse(BTC_test$Alvo1 > 0,1,0)

BTC_test$predBIN <- ifelse(BTC_test$predicao > 0,1,0)
BTC_test$predBIN2 <- ifelse(BTC_test$predicao2 > 0,1,0)
#BTC_test$predBIN3 <- ifelse(BTC_test$predicao3 > 0,1,0)

round(prop.table(table(BTC_test$AlvoBIN,BTC_test$predBIN),1)*100,2)
round(prop.table(table(BTC_test$AlvoBIN,BTC_test$predBIN2),1)*100,2)
#round(prop.table(table(BTC_test$AlvoBIN,BTC_test$predBIN3),1)*100,2)

retorno_modelo <- ifelse(BTC_test$predBIN > 0,BTC_test$Alvo1, -BTC_test$Alvo1)
BTC_test$retorno_modelo <- retorno_modelo
retorno_modelo_acumulado <- cumsum(retorno_modelo)
tail(retorno_modelo_acumulado, 1)

retorno_modelo2 <- ifelse(BTC_test$predBIN2 > 0,BTC_test$Alvo1, -BTC_test$Alvo1)
BTC_test$retorno_modelo2 <- retorno_modelo2
retorno_modelo_acumulado2 <- cumsum(retorno_modelo2)
tail(retorno_modelo_acumulado2, 1)

#retorno_modelo3 <- ifelse(BTC_test$predBIN3 > 0,BTC_test$Alvo*(1-0.005), -1*BTC_test$Alvo*(1-0.005))
#BTC_test$retorno_modelo3 <- retorno_modelo3
#retorno_modelo_acumulado3 <- cumsum(retorno_modelo3)
#tail(retorno_modelo_acumulado3, 1)

#####
#Grafico das regressoes
par(mfrow=c(1,2))
plot(retorno_modelo_acumulado, type = "l", col = "red", lwd = 2,
     main = "Retorno da Regressao #1",
     xlab = "dias",
     ylab = "Retorno em %")

plot(retorno_modelo_acumulado2, type = "l", col = "red", lwd = 2,
     main = "Retorno da Regressao #2",
     xlab = "dias",
     ylab = "Retorno em %")

#plot(retorno_modelo_acumulado3, type = "l", col = "red", lwd = 2,
     #main = "Retorno da Regressao #3",
     #xlab = "Janeiro 2018 - Novembro 2019",
     #ylab = "Retorno em %")

######################
#library(PerformanceAnalytics)
#table.Drawdowns()
#table.DownsideRisk()
#charts.PerformanceSummary()
#Drawdowns(retorno_modelo1)
#Drawdowns(retorno_modelo2)
