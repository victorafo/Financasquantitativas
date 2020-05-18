#Support Vector Machine - SVM - For BTCUSD
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

library(quantmod)
library(e1071)
library(data.table)


#####
#Carregando o Dataset e ajustando a data
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

######################################
BTC$Rsi <- RSI(BTC$fechamento,5,"SMA")
BTC$Rsi60 <- ifelse(BTC$Rsi>60,1,0)
BTC$Rsi40 <- ifelse(BTC$Rsi<40,1,0)
BTC$RSL <- (SMA(BTC$fechamento,15)/BTC$fechamento)-1
BTC$CCI <- CCI(BTC$fechamento,14)
BTC$CCI60 <- ifelse(BTC$CCI>60,1,0)
BTC$CCI40 <- ifelse(BTC$CCI<40,1,0)
BTC$Macd <- MACD(BTC$fechamento,24,52,18,"SMA") 
BTC$Macd <- BTC$Macd[,1] - BTC$Macd[,2] 
BTC$Bbands <- BBands(BTC$fechamento,20,"SMA",2)
BTC$fechamento_Shift <- BTC$fechamento
BTC['fechamento_Shift'] <- c(NA, head(BTC['fechamento_Shift'], dim(BTC)[1] - 1)[[1]])
BTC$fechamento_Shift2 <- BTC$fechamento_Shift
BTC['fechamento_Shift2'] <- c(NA, head(BTC['fechamento_Shift2'], dim(BTC)[1] - 1)[[1]])
BTC$fechamento_Shift3 <- BTC$fechamento_Shift2
BTC['fechamento_Shift3'] <- c(NA, head(BTC['fechamento_Shift3'], dim(BTC)[1] - 1)[[1]])
BTC$fechamento_Shift4 <- BTC$fechamento_Shift3
BTC['fechamento_Shift4'] <- c(NA, head(BTC['fechamento_Shift4'], dim(BTC)[1] - 1)[[1]])
BTC$fechamento_Shift5 <- BTC$fechamento_Shift4
BTC['fechamento_Shift5'] <- c(NA, head(BTC['fechamento_Shift5'], dim(BTC)[1] - 1)[[1]])
BTC$fechamento_Shift6 <- BTC$fechamento_Shift5
BTC['fechamento_Shift6'] <- c(NA, head(BTC['fechamento_Shift6'], dim(BTC)[1] - 1)[[1]])
BTC$fechamento_Shift7 <- BTC$fechamento_Shift6
BTC['fechamento_Shift7'] <- c(NA, head(BTC['fechamento_Shift7'], dim(BTC)[1] - 1)[[1]])
BTC$fechamento_Shift8 <- BTC$fechamento_Shift7
BTC['fechamento_Shift8'] <- c(NA, head(BTC['fechamento_Shift8'], dim(BTC)[1] - 1)[[1]])
BTC$fechamento_Shift9 <- BTC$fechamento_Shift8
BTC['fechamento_Shift9'] <- c(NA, head(BTC['fechamento_Shift9'], dim(BTC)[1] - 1)[[1]])
BTC$fechamento_Shift10 <- BTC$fechamento_Shift9
BTC['fechamento_Shift10'] <- c(NA, head(BTC['fechamento_Shift10'], dim(BTC)[1] - 1)[[1]])
BTC$Return <- (BTC$fechamento/BTC$fechamento_Shift-1)*100
BTC$Return2 <- (BTC$fechamento/BTC$fechamento_Shift2-1)*100
BTC$Return3 <- (BTC$fechamento/BTC$fechamento_Shift3-1)*100
BTC$Return4 <- (BTC$fechamento/BTC$fechamento_Shift4-1)*100
BTC$Return5 <- (BTC$fechamento/BTC$fechamento_Shift5-1)*100
BTC$Return6 <- (BTC$fechamento/BTC$fechamento_Shift6-1)*100
BTC$Return7 <- (BTC$fechamento/BTC$fechamento_Shift7-1)*100
BTC$Return8 <- (BTC$fechamento/BTC$fechamento_Shift8-1)*100
BTC$Return9 <- (BTC$fechamento/BTC$fechamento_Shift9-1)*100
BTC$Return10 <- (BTC$fechamento/BTC$fechamento_Shift10-1)*100

######################
#Criando o alvo
desloca <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

BTC$Alvo1 <- desloca(BTC$Return, 1)
BTC$Alvo2 <- desloca(BTC$Return2, 2)
BTC$Alvo3 <- desloca(BTC$Return3, 3)
BTC$Alvo4 <- desloca(BTC$Return4, 4)
BTC$Alvo5 <- desloca(BTC$Return5, 5)
BTC$Alvo6 <- desloca(BTC$Return6, 6)
BTC$Alvo7 <- desloca(BTC$Return7, 7)
BTC$Alvo8 <- desloca(BTC$Return8, 8)
BTC$Alvo9 <- desloca(BTC$Return9, 9)
BTC$Alvo10 <- desloca(BTC$Return10, 10)

BTC$Bbands <- BTC$Bbands[,4]
BTC_train <- na.omit(BTC[1:700,]) #Treinamento
BTC_test <- na.omit(BTC[701:dim(BTC)[1],]) #Teste 


#####
#Ajustando a SVM
svm1 <- svm(Alvo1 ~ ., BTC_train,kernel="linear")
summary(svm1)
#Ajustando a regressao
regressao1 <- lm(Alvo1 ~ ., data = BTC_train)
summary(regressao1)

#RMSE da regressao 
residuos1 <- regressao1$residuals 
rmse <- sqrt(mean(residuos1^2)) # 3.270285522


#RMSE do SMV 1 e 2
predicaoSVM1 <- predict(svm1,BTC_train)
residuosSVM1 <- BTC_train$Alvo1 - predicaoSVM1
SVM1_rmse <- sqrt(mean(residuosSVM1^2)) # 3.3384889777

#Fazendo o tuning da SVM
svm_tune1 <- tune(svm, Alvo1 ~ .,data = BTC_train,ranges = list(epsilon = seq(0,1,0.005), cost = 2^(2:5)))
 plot(svm_tune1)

#Ajustando e calculando o RMSE para o melhor modelo apos o tuning
 melhor_svm1 <- svm_tune1$best.model
 melhor_predicao1 <- predict(melhor_svm1, BTC_train) 
 residuos_melhor_svm1 <- BTC_train$Alvo1 - melhor_predicao1
 melhor_RSME1 <- sqrt(mean(residuos_melhor_svm1^2)) # 0.5614146
 plot(svm_tune2)

#Predicao dos resultados na base de teste
BTC_test$predicao2 <- predict(regressao2,BTC_test)
BTC_test$predicao_svm2 <- predict(svm2,BTC_test)

BTC_test$AlvoBIN2 <- ifelse(BTC_test$Alvo5 > 0,1,0)
BTC_test$predBIN2 <- ifelse(BTC_test$predicao2 > 0,1,0)
BTC_test$predBIN_SVM2 <- ifelse(BTC_test$predicao_svm2 > 0,1,0)

round(prop.table(table(BTC_test$AlvoBIN2,BTC_test$predBIN2),1)*100,2)
round(prop.table(table(BTC_test$AlvoBIN2,BTC_test$predBIN_SVM2),1)*100,2)

retorno_BH <- ifelse(BTC_test$Return > 0,BTC_test$Return, BTC_test$Return)
retorno_BH_acumulado <- cumsum(retorno_BH)

retorno_modelo2 <- ifelse(BTC_test$predBIN2 > 0,BTC_test$Alvo5, -1*BTC_test$Alvo5)
retorno_modelo_acumulado2 <- cumsum(retorno_modelo2)

retorno_SVM2 <- ifelse(BTC_test$predBIN_SVM2 > 0,BTC_test$Alvo5, -1*BTC_test$Alvo5)
retorno_SVM2_acumulado2 <- cumsum(retorno_SVM2)

#####
#Grafico das regressoes
#par(mfrow=c(1,1))
plot(retorno_modelo_acumulado2, type = "l", col = "green", lwd = 2,
     main = "BTCUSD - Regressao Linear x SVM x B&H",
     xlab = "2018 - Jan/2019",
     ylab = "Retorno em %",
     ylim = c(-100,1000))
lines(retorno_SVM2_acumulado2,col = "black", lwd = 2)
lines(retorno_BH_acumulado,col = "blue", lwd = 2)

legend(1, 400, legend=c("RL", "SVM", "B&H"),
       col=c("green", "black","blue"), lty=1:1, cex=0.8)



