#Regressao Logistica - For BTC
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

library(stats)
library(rattle)
library(caret)
library(ROCR)
library(party)
library(mlbench)
library(quantmod)
library(e1071)
library(data.table)

#####
BTC <- fread("BTC_USD_Bitfinex_Historical_Data.csv", header = TRUE, sep = ",")
BTC <- data.frame(BTC)
Sys.setlocale(category = "LC_TIME", "en_US")
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
#####
#Criando os indicadores
BTC$RSI <- RSI(BTC$fechamento,14,"SMA")
BTC$RSL <- (SMA(BTC$fechamento,8)/BTC$fechamento)-1
BTC$CCI <- CCI(BTC$fechamento,14)
BTC$MACD <- MACD(BTC$fechamento,12,26,9,"SMA") 
BTC$MACD <- BTC$MACD[,1] - BTC$MACD[,2] 
BTC$Bbands <- BBands(BTC$fechamento,20,"SMA",2)[,4]

#Criando o alvo
BTC$fechamento_deslocado <- BTC$fechamento
BTC['fechamento_deslocado'] <- c(NA, head(BTC['fechamento_deslocado'], dim(BTC)[1] - 1)[[1]])
BTC$Return <- (BTC$fechamento/BTC$fechamento_deslocado-1)*100

desloca <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

BTC$Alvo1 <- desloca(BTC$Return, 1)
BTC$Alvo1 <- ifelse(BTC$Alvo1 > 0 ,1,0)

#Criando as variaveis
BTC$RSIH <- ifelse(BTC$RSI> 70,1,0)
BTC$RSIL <- ifelse(BTC$RSI< 30,1,0)
BTC$CCIH <- ifelse(BTC$CCI > 120,1,0)
BTC$CCIL <- ifelse(BTC$CCI < -120,1,0)
BTC$MACDH <- ifelse(BTC$MACD > 0.5,1,0)
BTC$MACDL <- ifelse(BTC$MACD < -0.5,1,0)



BTC <- na.omit(BTC)

#####
#Desenvolvimento do modelo

#Treinamento - 2017-2019
treinamento <- BTC[1:827,]

#Validacao - 2019-2020
validacao <- BTC[828:dim(BTC)[1],]

set.seed(42)
inTrain <- createDataPartition(y=treinamento$Alvo1,p = 0.7, list=FALSE)
training <- treinamento[inTrain,]
testing <- treinamento[-inTrain,]
dim(training); dim(testing)

#Regress?o Log?stica
modelo1 <- glm(Alvo1 ~ ., data = training, family=binomial(link='logit'))

summary(modelo1)

sumario <- summary(modelo1)
r.squared <- sumario$r.squared
fstatistic <- sumario$fstatistic

#####
#Selecionando apenas as variaveis com significancia menor que 1%
melhores_vars <- sumario$coeff[-1,4] < 0.1
melhores_vars <- names(melhores_vars)[melhores_vars == TRUE] 
#Nao se esqueca de adicionar o alvo
melhores_vars <- c("Alvo1",melhores_vars)
train_melhores <- training[melhores_vars] 

#####
#Ajustando a regressao2
regressao2 <- glm(Alvo1 ~ ., data = train_melhores)
sumario2 <- summary(regressao2)
r.squared2 <- sumario2$r.squared
fstatistic2 <- sumario2$fstatistic

r.squared 
r.squared2
fstatistic
fstatistic2


#Avaliacao do Modelo - Treinamento/Teste
#Plota a AUC
probabilidades <- predict(modelo1,type='response',testing) 
modelo1_data <- prediction(probabilidades, testing$Alvo1)
plot(performance(modelo1_data, "tpr", "fpr"), col = "red", main = "Area Under the Curve")
abline(0,1, lty = 8, col = "grey")

#AUC
auc_rdock <- performance(modelo1_data, "auc")
auc.area_rdock <- slot(auc_rdock, "y.values")[[1]]
auc.area_rdock #0.5545509

#Gini
gini_fin_test <- 2*auc.area_rdock-1
gini_fin_test #0.109107

# Ordena??o por classe de score
classe <- cut(probabilidades, quantile(probabilidades, seq(0,1,0.125)))
table(classe, testing$Alvo1)



#Avaliacao do Modelo - Validacao
#Plota a ROC
probabilidades_val <- predict(modelo1,type='response',validacao) 
modelo1_data_val <- prediction(probabilidades_val, validacao$Alvo1)
plot(performance(modelo1_data_val, "tpr", "fpr"), col = "red", main = "Area Under the Curve")
abline(0,1, lty = 8, col = "grey")

#AUC
auc_rdock_val <- performance(modelo1_data_val, "auc")
auc.area_rdock_val <- slot(auc_rdock_val, "y.values")[[1]]
auc.area_rdock_val #0.5655887

#Gini
gini_fin_test_val <- 2*auc.area_rdock_val-1
gini_fin_test_val #0.1311774


#####
#Avaliacao da Performance
par(mfrow=c(1,2))
#testing
testing$predicao <- predict(modelo1,type='response',testing) 

retorno_BH <- ifelse(testing$Return > 0,testing$Return, testing$Return)
retorno_BH_acumulado <- cumsum(retorno_BH)

retorno_modelo1 <- ifelse(testing$predicao > 0.575,testing$Return,
                          ifelse(testing$predicao < 0.546, -1*testing$Return,0))
retorno_modelo1_acumulado <- cumsum(retorno_modelo1)


plot(retorno_modelo1_acumulado, type = "l", col = "black", lwd = 2,
     main = "Base de Teste",
     xlab = "2013 - 2017",
     ylab = "Retorno em %",
     ylim = c(-90,150))
lines(retorno_BH_acumulado,col = "blue", lwd = 2)

legend(1, 20, legend=c("RL","B&H"),
       col=c("black","blue"), lty=1:1, cex=0.8)


#Validacao
validacao$predicao <- predict(modelo1,type='response',validacao) 

retorno_BH <- ifelse(validacao$Return > 0,validacao$Return, validacao$Return)
retorno_BH_acumulado <- cumsum(retorno_BH)

retorno_modelo1 <- ifelse(validacao$predicao > 0.575,validacao$Return,
                          ifelse(validacao$predicao < 0.546, -1*validacao$Return,0))
retorno_modelo1_acumulado <- cumsum(retorno_modelo1)

#####
#Grafico da Performance

plot(retorno_modelo1_acumulado, type = "l", col = "green", lwd = 2,
     main = "Base de Valida?ao",
     xlab = "2018 - Fev/2019",
     ylab = "Retorno em %",
     ylim = c(-70,150))
lines(retorno_BH_acumulado,col = "blue", lwd = 2)

legend(1, 20, legend=c("RL","B&H"),
       col=c("green","blue"), lty=1:1, cex=0.8)

