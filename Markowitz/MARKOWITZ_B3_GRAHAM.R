#K-Means - For CRPG
#https://www.outspokenmarket.com/trading-ai.html
#Leandro Guerra
library (TTR)
library(quantmod)
library(e1071)
library(data.table)
library(clue) #Predict para o K-Means
library(factoextra) #Visualizar o K-Means
Sys.setlocale(category = "LC_TIME", "en_US")
#####
#PETR4
#################
#Carregando o Dataset
PETR4 <- fread("PETR4_Historical_Data.csv", header = TRUE, sep = ",")
PETR4 <- data.frame(PETR4)
PETR4 <- PETR4[,-c(2:6)]
#Renomeando o Data Frame
names(PETR4) <- c("data","variacao_PETR4")
#Tratamento dos dados
PETR4 <- PETR4[nrow(PETR4):1,]
PETR4$data <- gsub(',','', PETR4$data, fixed = TRUE)
PETR4$variacao_PETR4 <- gsub('%', '', PETR4$variacao_PETR4, fixed = TRUE) 
PETR4$variacao_PETR4 <- as.numeric(PETR4$variacao_PETR4)
PETR4$variacao_PETR4 <- PETR4$variacao_PETR4/100
PETR4$data <- as.POSIXct(strptime(PETR4$data, format= "%b %d %Y"))
PETR4 <- na.omit(PETR4)
#####
#OIBR3
#################
#Carregando o Dataset
OIBR3 <- fread("OIBR3_Historical_Data.csv", header = TRUE, sep = ",")
OIBR3 <- data.frame(OIBR3)
OIBR3 <- OIBR3[,-c(2:6)]
#Renomeando o Data Frame
names(OIBR3) <- c("data","variacao_OIBR3")
#Tratamento dos dados
OIBR3 <- OIBR3[nrow(OIBR3):1,]
OIBR3$data <- gsub(',','', OIBR3$data, fixed = TRUE)
OIBR3$variacao_OIBR3 <- gsub('%', '', OIBR3$variacao_OIBR3, fixed = TRUE) 
OIBR3$variacao_OIBR3 <- as.numeric(OIBR3$variacao_OIBR3)
OIBR3$variacao_OIBR3 <- OIBR3$variacao_OIBR3/100
OIBR3$data <- as.POSIXct(strptime(OIBR3$data, format= "%b %d %Y"))
OIBR3 <- na.omit(OIBR3)
#####
#VVAR3
#################
#Carregando o Dataset
VVAR3 <- fread("VVAR3_Historical_Data.csv", header = TRUE, sep = ",")
VVAR3 <- data.frame(VVAR3)
VVAR3 <- VVAR3[,-c(2:6)]
#Renomeando o Data Frame
names(VVAR3) <- c("data","variacao_VVAR3")
#Tratamento dos dados
VVAR3 <- VVAR3[nrow(VVAR3):1,]
VVAR3$data <- gsub(',','', VVAR3$data, fixed = TRUE)
VVAR3$variacao_VVAR3 <- gsub('%', '', VVAR3$variacao_VVAR3, fixed = TRUE) 
VVAR3$variacao_VVAR3 <- as.numeric(VVAR3$variacao_VVAR3)
VVAR3$variacao_VVAR3 <- VVAR3$variacao_VVAR3/100
VVAR3$data <- as.POSIXct(strptime(VVAR3$data, format= "%b %d %Y"))
VVAR3 <- na.omit(VVAR3)
#####
#IRBR3
#################
#Carregando o Dataset
IRBR3 <- fread("IRBR3_Historical_Data.csv", header = TRUE, sep = ",")
IRBR3 <- data.frame(IRBR3)
IRBR3 <- IRBR3[,-c(2:6)]
#Renomeando o Data Frame
names(IRBR3) <- c("data","variacao_IRBR3")
#Tratamento dos dados
IRBR3 <- IRBR3[nrow(IRBR3):1,]
IRBR3$data <- gsub(',','', IRBR3$data, fixed = TRUE)
IRBR3$variacao_IRBR3 <- gsub('%', '', IRBR3$variacao_IRBR3, fixed = TRUE) 
IRBR3$variacao_IRBR3 <- as.numeric(IRBR3$variacao_IRBR3)
IRBR3$variacao_IRBR3 <- IRBR3$variacao_IRBR3/100
IRBR3$data <- as.POSIXct(strptime(IRBR3$data, format= "%b %d %Y"))
IRBR3 <- na.omit(IRBR3)
#####
#COGN3
#################
#Carregando o Dataset
COGN3 <- fread("COGN3_Historical_Data.csv", header = TRUE, sep = ",")
COGN3 <- data.frame(COGN3)
COGN3 <- COGN3[,-c(2:6)]
#Renomeando o Data Frame
names(COGN3) <- c("data","variacao_COGN3")
#Tratamento dos dados
COGN3 <- COGN3[nrow(COGN3):1,]
COGN3$data <- gsub(',','', COGN3$data, fixed = TRUE)
COGN3$variacao_COGN3 <- gsub('%', '', COGN3$variacao_COGN3, fixed = TRUE) 
COGN3$variacao_COGN3 <- as.numeric(COGN3$variacao_COGN3)
COGN3$variacao_COGN3 <- COGN3$variacao_COGN3/100
COGN3$data <- as.POSIXct(strptime(COGN3$data, format= "%b %d %Y"))
COGN3 <- na.omit(COGN3)
#####
#BBDC4
#################
#Carregando o Dataset
BBDC4 <- fread("BBDC4_Historical_Data.csv", header = TRUE, sep = ",")
BBDC4 <- data.frame(BBDC4)
BBDC4 <- BBDC4[,-c(2:6)]
#Renomeando o Data Frame
names(BBDC4) <- c("data","variacao_BBDC4")
#Tratamento dos dados
BBDC4 <- BBDC4[nrow(BBDC4):1,]
BBDC4$data <- gsub(',','', BBDC4$data, fixed = TRUE)
BBDC4$variacao_BBDC4 <- gsub('%', '', BBDC4$variacao_BBDC4, fixed = TRUE) 
BBDC4$variacao_BBDC4 <- as.numeric(BBDC4$variacao_BBDC4)
BBDC4$variacao_BBDC4 <- BBDC4$variacao_BBDC4/100
BBDC4$data <- as.POSIXct(strptime(BBDC4$data, format= "%b %d %Y"))
BBDC4 <- na.omit(BBDC4)
#####
#ITUB4
#################
#Carregando o Dataset
ITUB4 <- fread("ITUB4_Historical_Data.csv", header = TRUE, sep = ",")
ITUB4 <- data.frame(ITUB4)
ITUB4 <- ITUB4[,-c(2:6)]
#Renomeando o Data Frame
names(ITUB4) <- c("data","variacao_ITUB4")
#Tratamento dos dados
ITUB4 <- ITUB4[nrow(ITUB4):1,]
ITUB4$data <- gsub(',','', ITUB4$data, fixed = TRUE)
ITUB4$variacao_ITUB4 <- gsub('%', '', ITUB4$variacao_ITUB4, fixed = TRUE) 
ITUB4$variacao_ITUB4 <- as.numeric(ITUB4$variacao_ITUB4)
ITUB4$variacao_ITUB4 <- ITUB4$variacao_ITUB4/100
ITUB4$data <- as.POSIXct(strptime(ITUB4$data, format= "%b %d %Y"))
ITUB4 <- na.omit(ITUB4)
#####
#VALE3
#################
#Carregando o Dataset
VALE3 <- fread("VALE3_Historical_Data.csv", header = TRUE, sep = ",")
VALE3 <- data.frame(VALE3)
VALE3 <- VALE3[,-c(2:6)]
#Renomeando o Data Frame
names(VALE3) <- c("data","variacao_VALE3")
#Tratamento dos dados
VALE3 <- VALE3[nrow(VALE3):1,]
VALE3$data <- gsub(',','', VALE3$data, fixed = TRUE)
VALE3$variacao_VALE3 <- gsub('%', '', VALE3$variacao_VALE3, fixed = TRUE) 
VALE3$variacao_VALE3 <- as.numeric(VALE3$variacao_VALE3)
VALE3$variacao_VALE3 <- VALE3$variacao_VALE3/100
VALE3$data <- as.POSIXct(strptime(VALE3$data, format= "%b %d %Y"))
VALE3 <- na.omit(VALE3)
#####
#PETR3
#################
#Carregando o Dataset
PETR3 <- fread("PETR3_Historical_Data.csv", header = TRUE, sep = ",")
PETR3 <- data.frame(PETR3)
PETR3 <- PETR3[,-c(2:6)]
#Renomeando o Data Frame
names(PETR3) <- c("data","variacao_PETR3")
#Tratamento dos dados
PETR3 <- PETR3[nrow(PETR3):1,]
PETR3$data <- gsub(',','', PETR3$data, fixed = TRUE)
PETR3$variacao_PETR3 <- gsub('%', '', PETR3$variacao_PETR3, fixed = TRUE) 
PETR3$variacao_PETR3 <- as.numeric(PETR3$variacao_PETR3)
PETR3$variacao_PETR3 <- PETR3$variacao_PETR3/100
PETR3$data <- as.POSIXct(strptime(PETR3$data, format= "%b %d %Y"))
PETR3 <- na.omit(PETR3)
#####
#GGBR4
#################
#Carregando o Dataset
GGBR4 <- fread("GGBR4_Historical_Data.csv", header = TRUE, sep = ",")
GGBR4 <- data.frame(GGBR4)
GGBR4 <- GGBR4[,-c(2:6)]
#Renomeando o Data Frame
names(GGBR4) <- c("data","variacao_GGBR4")
#Tratamento dos dados
GGBR4 <- GGBR4[nrow(GGBR4):1,]
GGBR4$data <- gsub(',','', GGBR4$data, fixed = TRUE)
GGBR4$variacao_GGBR4 <- gsub('%', '', GGBR4$variacao_GGBR4, fixed = TRUE) 
GGBR4$variacao_GGBR4 <- as.numeric(GGBR4$variacao_GGBR4)
GGBR4$variacao_GGBR4 <- GGBR4$variacao_GGBR4/100
GGBR4$data <- as.POSIXct(strptime(GGBR4$data, format= "%b %d %Y"))
GGBR4 <- na.omit(GGBR4)
#####
#ITSA4
#################
#Carregando o Dataset
ITSA4 <- fread("ITSA4_Historical_Data.csv", header = TRUE, sep = ",")
ITSA4 <- data.frame(ITSA4)
ITSA4 <- ITSA4[,-c(2:6)]
#Renomeando o Data Frame
names(ITSA4) <- c("data","variacao_ITSA4")
#Tratamento dos dados
ITSA4 <- ITSA4[nrow(ITSA4):1,]
ITSA4$data <- gsub(',','', ITSA4$data, fixed = TRUE)
ITSA4$variacao_ITSA4 <- gsub('%', '', ITSA4$variacao_ITSA4, fixed = TRUE) 
ITSA4$variacao_ITSA4 <- as.numeric(ITSA4$variacao_ITSA4)
ITSA4$variacao_ITSA4 <- ITSA4$variacao_ITSA4/100
ITSA4$data <- as.POSIXct(strptime(ITSA4$data, format= "%b %d %Y"))
ITSA4 <- na.omit(ITSA4)
#####
#ABEV3
#################
#Carregando o Dataset
ABEV3 <- fread("ABEV3_Historical_Data.csv", header = TRUE, sep = ",")
ABEV3 <- data.frame(ABEV3)
ABEV3 <- ABEV3[,-c(2:6)]
#Renomeando o Data Frame
names(ABEV3) <- c("data","variacao_ABEV3")
#Tratamento dos dados
ABEV3 <- ABEV3[nrow(ABEV3):1,]
ABEV3$data <- gsub(',','', ABEV3$data, fixed = TRUE)
ABEV3$variacao_ABEV3 <- gsub('%', '', ABEV3$variacao_ABEV3, fixed = TRUE) 
ABEV3$variacao_ABEV3 <- as.numeric(ABEV3$variacao_ABEV3)
ABEV3$variacao_ABEV3 <- ABEV3$variacao_ABEV3/100
ABEV3$data <- as.POSIXct(strptime(ABEV3$data, format= "%b %d %Y"))
ABEV3 <- na.omit(ABEV3)
#####
#JBSS3
#################
#Carregando o Dataset
JBSS3 <- fread("JBSS3_Historical_Data.csv", header = TRUE, sep = ",")
JBSS3 <- data.frame(JBSS3)
JBSS3 <- JBSS3[,-c(2:6)]
#Renomeando o Data Frame
names(JBSS3) <- c("data","variacao_JBSS3")
#Tratamento dos dados
JBSS3 <- JBSS3[nrow(JBSS3):1,]
JBSS3$data <- gsub(',','', JBSS3$data, fixed = TRUE)
JBSS3$variacao_JBSS3 <- gsub('%', '', JBSS3$variacao_JBSS3, fixed = TRUE) 
JBSS3$variacao_JBSS3 <- as.numeric(JBSS3$variacao_JBSS3)
JBSS3$variacao_JBSS3 <- JBSS3$variacao_JBSS3/100
JBSS3$data <- as.POSIXct(strptime(JBSS3$data, format= "%b %d %Y"))
JBSS3 <- na.omit(JBSS3)
#####
#RENT3
#################
#Carregando o Dataset
RENT3 <- fread("RENT3_Historical_Data.csv", header = TRUE, sep = ",")
RENT3 <- data.frame(RENT3)
RENT3 <- RENT3[,-c(2:6)]
#Renomeando o Data Frame
names(RENT3) <- c("data","variacao_RENT3")
#Tratamento dos dados
RENT3 <- RENT3[nrow(RENT3):1,]
RENT3$data <- gsub(',','', RENT3$data, fixed = TRUE)
RENT3$variacao_RENT3 <- gsub('%', '', RENT3$variacao_RENT3, fixed = TRUE) 
RENT3$variacao_RENT3 <- as.numeric(RENT3$variacao_RENT3)
RENT3$variacao_RENT3 <- RENT3$variacao_RENT3/100
RENT3$data <- as.POSIXct(strptime(RENT3$data, format= "%b %d %Y"))
RENT3 <- na.omit(RENT3)
#####
#B3SA3
#################
#Carregando o Dataset
B3SA3 <- fread("B3SA3_Historical_Data.csv", header = TRUE, sep = ",")
B3SA3 <- data.frame(B3SA3)
B3SA3 <- B3SA3[,-c(2:6)]
#Renomeando o Data Frame
names(B3SA3) <- c("data","variacao_B3SA3")
#Tratamento dos dados
B3SA3 <- B3SA3[nrow(B3SA3):1,]
B3SA3$data <- gsub(',','', B3SA3$data, fixed = TRUE)
B3SA3$variacao_B3SA3 <- gsub('%', '', B3SA3$variacao_B3SA3, fixed = TRUE) 
B3SA3$variacao_B3SA3 <- as.numeric(B3SA3$variacao_B3SA3)
B3SA3$variacao_B3SA3 <- B3SA3$variacao_B3SA3/100
B3SA3$data <- as.POSIXct(strptime(B3SA3$data, format= "%b %d %Y"))
B3SA3 <- na.omit(B3SA3)
#####
#CIEL3
#################
#Carregando o Dataset
CIEL3 <- fread("CIEL3_Historical_Data.csv", header = TRUE, sep = ",")
CIEL3 <- data.frame(CIEL3)
CIEL3 <- CIEL3[,-c(2:6)]
#Renomeando o Data Frame
names(CIEL3) <- c("data","variacao_CIEL3")
#Tratamento dos dados
CIEL3 <- CIEL3[nrow(CIEL3):1,]
CIEL3$data <- gsub(',','', CIEL3$data, fixed = TRUE)
CIEL3$variacao_CIEL3 <- gsub('%', '', CIEL3$variacao_CIEL3, fixed = TRUE) 
CIEL3$variacao_CIEL3 <- as.numeric(CIEL3$variacao_CIEL3)
CIEL3$variacao_CIEL3 <- CIEL3$variacao_CIEL3/100
CIEL3$data <- as.POSIXct(strptime(CIEL3$data, format= "%b %d %Y"))
CIEL3 <- na.omit(CIEL3)
#####
#BBAS3
#################
#Carregando o Dataset
BBAS3 <- fread("BBAS3_Historical_Data.csv", header = TRUE, sep = ",")
BBAS3 <- data.frame(BBAS3)
BBAS3 <- BBAS3[,-c(2:6)]
#Renomeando o Data Frame
names(BBAS3) <- c("data","variacao_BBAS3")
#Tratamento dos dados
BBAS3 <- BBAS3[nrow(BBAS3):1,]
BBAS3$data <- gsub(',','', BBAS3$data, fixed = TRUE)
BBAS3$variacao_BBAS3 <- gsub('%', '', BBAS3$variacao_BBAS3, fixed = TRUE) 
BBAS3$variacao_BBAS3 <- as.numeric(BBAS3$variacao_BBAS3)
BBAS3$variacao_BBAS3 <- BBAS3$variacao_BBAS3/100
BBAS3$data <- as.POSIXct(strptime(BBAS3$data, format= "%b %d %Y"))
BBAS3 <- na.omit(BBAS3)
#####
#CSNA3
#################
#Carregando o Dataset
CSNA3 <- fread("CSNA3_Historical_Data.csv", header = TRUE, sep = ",")
CSNA3 <- data.frame(CSNA3)
CSNA3 <- CSNA3[,-c(2:6)]
#Renomeando o Data Frame
names(CSNA3) <- c("data","variacao_CSNA3")
#Tratamento dos dados
CSNA3 <- CSNA3[nrow(CSNA3):1,]
CSNA3$data <- gsub(',','', CSNA3$data, fixed = TRUE)
CSNA3$variacao_CSNA3 <- gsub('%', '', CSNA3$variacao_CSNA3, fixed = TRUE) 
CSNA3$variacao_CSNA3 <- as.numeric(CSNA3$variacao_CSNA3)
CSNA3$variacao_CSNA3 <- CSNA3$variacao_CSNA3/100
CSNA3$data <- as.POSIXct(strptime(CSNA3$data, format= "%b %d %Y"))
CSNA3 <- na.omit(CSNA3)
#####
#USIM5
#################
#Carregando o Dataset
USIM5 <- fread("USIM5_Historical_Data.csv", header = TRUE, sep = ",")
USIM5 <- data.frame(USIM5)
USIM5 <- USIM5[,-c(2:6)]
#Renomeando o Data Frame
names(USIM5) <- c("data","variacao_USIM5")
#Tratamento dos dados
USIM5 <- USIM5[nrow(USIM5):1,]
USIM5$data <- gsub(',','', USIM5$data, fixed = TRUE)
USIM5$variacao_USIM5 <- gsub('%', '', USIM5$variacao_USIM5, fixed = TRUE) 
USIM5$variacao_USIM5 <- as.numeric(USIM5$variacao_USIM5)
USIM5$variacao_USIM5 <- USIM5$variacao_USIM5/100
USIM5$data <- as.POSIXct(strptime(USIM5$data, format= "%b %d %Y"))
USIM5 <- na.omit(USIM5)
#####
#SUZB3
#################
#Carregando o Dataset
SUZB3 <- fread("SUZB3_Historical_Data.csv", header = TRUE, sep = ",")
SUZB3 <- data.frame(SUZB3)
SUZB3 <- SUZB3[,-c(2:6)]
#Renomeando o Data Frame
names(SUZB3) <- c("data","variacao_SUZB3")
#Tratamento dos dados
SUZB3 <- SUZB3[nrow(SUZB3):1,]
SUZB3$data <- gsub(',','', SUZB3$data, fixed = TRUE)
SUZB3$variacao_SUZB3 <- gsub('%', '', SUZB3$variacao_SUZB3, fixed = TRUE) 
SUZB3$variacao_SUZB3 <- as.numeric(SUZB3$variacao_SUZB3)
SUZB3$variacao_SUZB3 <- SUZB3$variacao_SUZB3/100
SUZB3$data <- as.POSIXct(strptime(SUZB3$data, format= "%b %d %Y"))
SUZB3 <- na.omit(SUZB3)
#####
#GOAU4
#################
#Carregando o Dataset
GOAU4 <- fread("GOAU4_Historical_Data.csv", header = TRUE, sep = ",")
GOAU4 <- data.frame(GOAU4)
GOAU4 <- GOAU4[,-c(2:6)]
#Renomeando o Data Frame
names(GOAU4) <- c("data","variacao_GOAU4")
#Tratamento dos dados
GOAU4 <- GOAU4[nrow(GOAU4):1,]
GOAU4$data <- gsub(',','', GOAU4$data, fixed = TRUE)
GOAU4$variacao_GOAU4 <- gsub('%', '', GOAU4$variacao_GOAU4, fixed = TRUE) 
GOAU4$variacao_GOAU4 <- as.numeric(GOAU4$variacao_GOAU4)
GOAU4$variacao_GOAU4 <- GOAU4$variacao_GOAU4/100
GOAU4$data <- as.POSIXct(strptime(GOAU4$data, format= "%b %d %Y"))
GOAU4 <- na.omit(GOAU4)
#####
#EMBR3
#################
#Carregando o Dataset
EMBR3 <- fread("EMBR3_Historical_Data.csv", header = TRUE, sep = ",")
EMBR3 <- data.frame(EMBR3)
EMBR3 <- EMBR3[,-c(2:6)]
#Renomeando o Data Frame
names(EMBR3) <- c("data","variacao_EMBR3")
#Tratamento dos dados
EMBR3 <- EMBR3[nrow(EMBR3):1,]
EMBR3$data <- gsub(',','', EMBR3$data, fixed = TRUE)
EMBR3$variacao_EMBR3 <- gsub('%', '', EMBR3$variacao_EMBR3, fixed = TRUE) 
EMBR3$variacao_EMBR3 <- as.numeric(EMBR3$variacao_EMBR3)
EMBR3$variacao_EMBR3 <- EMBR3$variacao_EMBR3/100
EMBR3$data <- as.POSIXct(strptime(EMBR3$data, format= "%b %d %Y"))
EMBR3 <- na.omit(EMBR3)
#####
#MRFG3
#################
#Carregando o Dataset
MRFG3 <- fread("MRFG3_Historical_Data.csv", header = TRUE, sep = ",")
MRFG3 <- data.frame(MRFG3)
MRFG3 <- MRFG3[,-c(2:6)]
#Renomeando o Data Frame
names(MRFG3) <- c("data","variacao_MRFG3")
#Tratamento dos dados
MRFG3 <- MRFG3[nrow(MRFG3):1,]
MRFG3$data <- gsub(',','', MRFG3$data, fixed = TRUE)
MRFG3$variacao_MRFG3 <- gsub('%', '', MRFG3$variacao_MRFG3, fixed = TRUE) 
MRFG3$variacao_MRFG3 <- as.numeric(MRFG3$variacao_MRFG3)
MRFG3$variacao_MRFG3 <- MRFG3$variacao_MRFG3/100
MRFG3$data <- as.POSIXct(strptime(MRFG3$data, format= "%b %d %Y"))
MRFG3 <- na.omit(MRFG3)
#####
#GOLL4
#################
#Carregando o Dataset
GOLL4 <- fread("GOLL4_Historical_Data.csv", header = TRUE, sep = ",")
GOLL4 <- data.frame(GOLL4)
GOLL4 <- GOLL4[,-c(2:6)]
#Renomeando o Data Frame
names(GOLL4) <- c("data","variacao_GOLL4")
#Tratamento dos dados
GOLL4 <- GOLL4[nrow(GOLL4):1,]
GOLL4$data <- gsub(',','', GOLL4$data, fixed = TRUE)
GOLL4$variacao_GOLL4 <- gsub('%', '', GOLL4$variacao_GOLL4, fixed = TRUE) 
GOLL4$variacao_GOLL4 <- as.numeric(GOLL4$variacao_GOLL4)
GOLL4$variacao_GOLL4 <- GOLL4$variacao_GOLL4/100
GOLL4$data <- as.POSIXct(strptime(GOLL4$data, format= "%b %d %Y"))
GOLL4 <- na.omit(GOLL4)
#####
#AZUL4
#################
#Carregando o Dataset
AZUL4 <- fread("AZUL4_Historical_Data.csv", header = TRUE, sep = ",")
AZUL4 <- data.frame(AZUL4)
AZUL4 <- AZUL4[,-c(2:6)]
#Renomeando o Data Frame
names(AZUL4) <- c("data","variacao_AZUL4")
#Tratamento dos dados
AZUL4 <- AZUL4[nrow(AZUL4):1,]
AZUL4$data <- gsub(',','', AZUL4$data, fixed = TRUE)
AZUL4$variacao_AZUL4 <- gsub('%', '', AZUL4$variacao_AZUL4, fixed = TRUE) 
AZUL4$variacao_AZUL4 <- as.numeric(AZUL4$variacao_AZUL4)
AZUL4$variacao_AZUL4 <- AZUL4$variacao_AZUL4/100
AZUL4$data <- as.POSIXct(strptime(AZUL4$data, format= "%b %d %Y"))
AZUL4 <- na.omit(AZUL4)
#####
#NEOE3
#################
#Carregando o Dataset
NEOE3 <- fread("NEOE3_Historical_Data.csv", header = TRUE, sep = ",")
NEOE3 <- data.frame(NEOE3)
NEOE3 <- NEOE3[,-c(2:6)]
#Renomeando o Data Frame
names(NEOE3) <- c("data","variacao_NEOE3")
#Tratamento dos dados
NEOE3 <- NEOE3[nrow(NEOE3):1,]
NEOE3$data <- gsub(',','', NEOE3$data, fixed = TRUE)
NEOE3$variacao_NEOE3 <- gsub('%', '', NEOE3$variacao_NEOE3, fixed = TRUE) 
NEOE3$variacao_NEOE3 <- as.numeric(NEOE3$variacao_NEOE3)
NEOE3$variacao_NEOE3 <- NEOE3$variacao_NEOE3/100
NEOE3$data <- as.POSIXct(strptime(NEOE3$data, format= "%b %d %Y"))
NEOE3 <- na.omit(NEOE3)
#####
#BRFS3
#################
#Carregando o Dataset
BRFS3 <- fread("BRFS3_Historical_Data.csv", header = TRUE, sep = ",")
BRFS3 <- data.frame(BRFS3)
BRFS3 <- BRFS3[,-c(2:6)]
#Renomeando o Data Frame
names(BRFS3) <- c("data","variacao_BRFS3")
#Tratamento dos dados
BRFS3 <- BRFS3[nrow(BRFS3):1,]
BRFS3$data <- gsub(',','', BRFS3$data, fixed = TRUE)
BRFS3$variacao_BRFS3 <- gsub('%', '', BRFS3$variacao_BRFS3, fixed = TRUE) 
BRFS3$variacao_BRFS3 <- as.numeric(BRFS3$variacao_BRFS3)
BRFS3$variacao_BRFS3 <- BRFS3$variacao_BRFS3/100
BRFS3$data <- as.POSIXct(strptime(BRFS3$data, format= "%b %d %Y"))
BRFS3 <- na.omit(BRFS3)
#####
#CMIG4
#################
#Carregando o Dataset
CMIG4 <- fread("CMIG4_Historical_Data.csv", header = TRUE, sep = ",")
CMIG4 <- data.frame(CMIG4)
CMIG4 <- CMIG4[,-c(2:6)]
#Renomeando o Data Frame
names(CMIG4) <- c("data","variacao_CMIG4")
#Tratamento dos dados
CMIG4 <- CMIG4[nrow(CMIG4):1,]
CMIG4$data <- gsub(',','', CMIG4$data, fixed = TRUE)
CMIG4$variacao_CMIG4 <- gsub('%', '', CMIG4$variacao_CMIG4, fixed = TRUE) 
CMIG4$variacao_CMIG4 <- as.numeric(CMIG4$variacao_CMIG4)
CMIG4$variacao_CMIG4 <- CMIG4$variacao_CMIG4/100
CMIG4$data <- as.POSIXct(strptime(CMIG4$data, format= "%b %d %Y"))
CMIG4 <- na.omit(CMIG4)
#####
#CYRE3
#################
#Carregando o Dataset
CYRE3 <- fread("CYRE3_Historical_Data.csv", header = TRUE, sep = ",")
CYRE3 <- data.frame(CYRE3)
CYRE3 <- CYRE3[,-c(2:6)]
#Renomeando o Data Frame
names(CYRE3) <- c("data","variacao_CYRE3")
#Tratamento dos dados
CYRE3 <- CYRE3[nrow(CYRE3):1,]
CYRE3$data <- gsub(',','', CYRE3$data, fixed = TRUE)
CYRE3$variacao_CYRE3 <- gsub('%', '', CYRE3$variacao_CYRE3, fixed = TRUE) 
CYRE3$variacao_CYRE3 <- as.numeric(CYRE3$variacao_CYRE3)
CYRE3$variacao_CYRE3 <- CYRE3$variacao_CYRE3/100
CYRE3$data <- as.POSIXct(strptime(CYRE3$data, format= "%b %d %Y"))
CYRE3 <- na.omit(CYRE3)
#####
#BRML3
#################
#Carregando o Dataset
BRML3 <- fread("BRML3_Historical_Data.csv", header = TRUE, sep = ",")
BRML3 <- data.frame(BRML3)
BRML3 <- BRML3[,-c(2:6)]
#Renomeando o Data Frame
names(BRML3) <- c("data","variacao_BRML3")
#Tratamento dos dados
BRML3 <- BRML3[nrow(BRML3):1,]
BRML3$data <- gsub(',','', BRML3$data, fixed = TRUE)
BRML3$variacao_BRML3 <- gsub('%', '', BRML3$variacao_BRML3, fixed = TRUE) 
BRML3$variacao_BRML3 <- as.numeric(BRML3$variacao_BRML3)
BRML3$variacao_BRML3 <- BRML3$variacao_BRML3/100
BRML3$data <- as.POSIXct(strptime(BRML3$data, format= "%b %d %Y"))
BRML3 <- na.omit(BRML3)
#####
#LREN3
#################
#Carregando o Dataset
LREN3 <- fread("LREN3_Historical_Data.csv", header = TRUE, sep = ",")
LREN3 <- data.frame(LREN3)
LREN3 <- LREN3[,-c(2:6)]
#Renomeando o Data Frame
names(LREN3) <- c("data","variacao_LREN3")
#Tratamento dos dados
LREN3 <- LREN3[nrow(LREN3):1,]
LREN3$data <- gsub(',','', LREN3$data, fixed = TRUE)
LREN3$variacao_LREN3 <- gsub('%', '', LREN3$variacao_LREN3, fixed = TRUE) 
LREN3$variacao_LREN3 <- as.numeric(LREN3$variacao_LREN3)
LREN3$variacao_LREN3 <- LREN3$variacao_LREN3/100
LREN3$data <- as.POSIXct(strptime(LREN3$data, format= "%b %d %Y"))
LREN3 <- na.omit(LREN3)
##### 
#KLBN11
#################
#Carregando o Dataset
KLBN11 <- fread("KLBN11_Historical_Data.csv", header = TRUE, sep = ",")
KLBN11 <- data.frame(KLBN11)
KLBN11 <- KLBN11[,-c(2:6)]
#Renomeando o Data Frame
names(KLBN11) <- c("data","variacao_KLBN11")
#Tratamento dos dados
KLBN11 <- KLBN11[nrow(KLBN11):1,]
KLBN11$data <- gsub(',','', KLBN11$data, fixed = TRUE)
KLBN11$variacao_KLBN11 <- gsub('%', '', KLBN11$variacao_KLBN11, fixed = TRUE) 
KLBN11$variacao_KLBN11 <- as.numeric(KLBN11$variacao_KLBN11)
KLBN11$variacao_KLBN11 <- KLBN11$variacao_KLBN11/100
KLBN11$data <- as.POSIXct(strptime(KLBN11$data, format= "%b %d %Y"))
KLBN11 <- na.omit(KLBN11)
#####
#HGTX3
#################
#Carregando o Dataset
HGTX3 <- fread("HGTX3_Historical_Data.csv", header = TRUE, sep = ",")
HGTX3 <- data.frame(HGTX3)
HGTX3 <- HGTX3[,-c(2:6)]
#Renomeando o Data Frame
names(HGTX3) <- c("data","variacao_HGTX3")
#Tratamento dos dados
HGTX3 <- HGTX3[nrow(HGTX3):1,]
HGTX3$data <- gsub(',','', HGTX3$data, fixed = TRUE)
HGTX3$variacao_HGTX3 <- gsub('%', '', HGTX3$variacao_HGTX3, fixed = TRUE) 
HGTX3$variacao_HGTX3 <- as.numeric(HGTX3$variacao_HGTX3)
HGTX3$variacao_HGTX3 <- HGTX3$variacao_HGTX3/100
HGTX3$data <- as.POSIXct(strptime(HGTX3$data, format= "%b %d %Y"))
HGTX3 <- na.omit(HGTX3)
#####
#CCRO3
#################
#Carregando o Dataset
CCRO3 <- fread("CCRO3_Historical_Data.csv", header = TRUE, sep = ",")
CCRO3 <- data.frame(CCRO3)
CCRO3 <- CCRO3[,-c(2:6)]
#Renomeando o Data Frame
names(CCRO3) <- c("data","variacao_CCRO3")
#Tratamento dos dados
CCRO3 <- CCRO3[nrow(CCRO3):1,]
CCRO3$data <- gsub(',','', CCRO3$data, fixed = TRUE)
CCRO3$variacao_CCRO3 <- gsub('%', '', CCRO3$variacao_CCRO3, fixed = TRUE) 
CCRO3$variacao_CCRO3 <- as.numeric(CCRO3$variacao_CCRO3)
CCRO3$variacao_CCRO3 <- CCRO3$variacao_CCRO3/100
CCRO3$data <- as.POSIXct(strptime(CCRO3$data, format= "%b %d %Y"))
CCRO3 <- na.omit(CCRO3)
#####
#RAIL3
#################
#Carregando o Dataset
RAIL3 <- fread("RAIL3_Historical_Data.csv", header = TRUE, sep = ",")
RAIL3 <- data.frame(RAIL3)
RAIL3 <- RAIL3[,-c(2:6)]
#Renomeando o Data Frame
names(RAIL3) <- c("data","variacao_RAIL3")
#Tratamento dos dados
RAIL3 <- RAIL3[nrow(RAIL3):1,]
RAIL3$data <- gsub(',','', RAIL3$data, fixed = TRUE)
RAIL3$variacao_RAIL3 <- gsub('%', '', RAIL3$variacao_RAIL3, fixed = TRUE) 
RAIL3$variacao_RAIL3 <- as.numeric(RAIL3$variacao_RAIL3)
RAIL3$variacao_RAIL3 <- RAIL3$variacao_RAIL3/100
RAIL3$data <- as.POSIXct(strptime(RAIL3$data, format= "%b %d %Y"))
RAIL3 <- na.omit(RAIL3)
#####
#MGLU3
#################
#Carregando o Dataset
MGLU3 <- fread("MGLU3_Historical_Data.csv", header = TRUE, sep = ",")
MGLU3 <- data.frame(MGLU3)
MGLU3 <- MGLU3[,-c(2:6)]
#Renomeando o Data Frame
names(MGLU3) <- c("data","variacao_MGLU3")
#Tratamento dos dados
MGLU3 <- MGLU3[nrow(MGLU3):1,]
MGLU3$data <- gsub(',','', MGLU3$data, fixed = TRUE)
MGLU3$variacao_MGLU3 <- gsub('%', '', MGLU3$variacao_MGLU3, fixed = TRUE) 
MGLU3$variacao_MGLU3 <- as.numeric(MGLU3$variacao_MGLU3)
MGLU3$variacao_MGLU3 <- MGLU3$variacao_MGLU3/100
MGLU3$data <- as.POSIXct(strptime(MGLU3$data, format= "%b %d %Y"))
MGLU3 <- na.omit(MGLU3)
#####
#POMO4
#################
#Carregando o Dataset
POMO4 <- fread("POMO4_Historical_Data.csv", header = TRUE, sep = ",")
POMO4 <- data.frame(POMO4)
POMO4 <- POMO4[,-c(2:6)]
#Renomeando o Data Frame
names(POMO4) <- c("data","variacao_POMO4")
#Tratamento dos dados
POMO4 <- POMO4[nrow(POMO4):1,]
POMO4$data <- gsub(',','', POMO4$data, fixed = TRUE)
POMO4$variacao_POMO4 <- gsub('%', '', POMO4$variacao_POMO4, fixed = TRUE) 
POMO4$variacao_POMO4 <- as.numeric(POMO4$variacao_POMO4)
POMO4$variacao_POMO4 <- POMO4$variacao_POMO4/100
POMO4$data <- as.POSIXct(strptime(POMO4$data, format= "%b %d %Y"))
POMO4 <- na.omit(POMO4)
#####
#BRKM5
#################
#Carregando o Dataset
BRKM5 <- fread("BRKM5_Historical_Data.csv", header = TRUE, sep = ",")
BRKM5 <- data.frame(BRKM5)
BRKM5 <- BRKM5[,-c(2:6)]
#Renomeando o Data Frame
names(BRKM5) <- c("data","variacao_BRKM5")
#Tratamento dos dados
BRKM5 <- BRKM5[nrow(BRKM5):1,]
BRKM5$data <- gsub(',','', BRKM5$data, fixed = TRUE)
BRKM5$variacao_BRKM5 <- gsub('%', '', BRKM5$variacao_BRKM5, fixed = TRUE) 
BRKM5$variacao_BRKM5 <- as.numeric(BRKM5$variacao_BRKM5)
BRKM5$variacao_BRKM5 <- BRKM5$variacao_BRKM5/100
BRKM5$data <- as.POSIXct(strptime(BRKM5$data, format= "%b %d %Y"))
BRKM5 <- na.omit(BRKM5)
#####
#TIMP3
#################
#Carregando o Dataset
TIMP3 <- fread("TIMP3_Historical_Data.csv", header = TRUE, sep = ",")
TIMP3 <- data.frame(TIMP3)
TIMP3 <- TIMP3[,-c(2:6)]
#Renomeando o Data Frame
names(TIMP3) <- c("data","variacao_TIMP3")
#Tratamento dos dados
TIMP3 <- TIMP3[nrow(TIMP3):1,]
TIMP3$data <- gsub(',','', TIMP3$data, fixed = TRUE)
TIMP3$variacao_TIMP3 <- gsub('%', '', TIMP3$variacao_TIMP3, fixed = TRUE) 
TIMP3$variacao_TIMP3 <- as.numeric(TIMP3$variacao_TIMP3)
TIMP3$variacao_TIMP3 <- TIMP3$variacao_TIMP3/100
TIMP3$data <- as.POSIXct(strptime(TIMP3$data, format= "%b %d %Y"))
TIMP3 <- na.omit(TIMP3)
#####
#BEEF3
#################
#Carregando o Dataset
BEEF3 <- fread("BEEF3_Historical_Data.csv", header = TRUE, sep = ",")
BEEF3 <- data.frame(BEEF3)
BEEF3 <- BEEF3[,-c(2:6)]
#Renomeando o Data Frame
names(BEEF3) <- c("data","variacao_BEEF3")
#Tratamento dos dados
BEEF3 <- BEEF3[nrow(BEEF3):1,]
BEEF3$data <- gsub(',','', BEEF3$data, fixed = TRUE)
BEEF3$variacao_BEEF3 <- gsub('%', '', BEEF3$variacao_BEEF3, fixed = TRUE) 
BEEF3$variacao_BEEF3 <- as.numeric(BEEF3$variacao_BEEF3)
BEEF3$variacao_BEEF3 <- BEEF3$variacao_BEEF3/100
BEEF3$data <- as.POSIXct(strptime(BEEF3$data, format= "%b %d %Y"))
BEEF3 <- na.omit(BEEF3)
#####
#NTCO3
#################
#Carregando o Dataset
NTCO3 <- fread("NTCO3_Historical_Data.csv", header = TRUE, sep = ",")
NTCO3 <- data.frame(NTCO3)
NTCO3 <- NTCO3[,-c(2:6)]
#Renomeando o Data Frame
names(NTCO3) <- c("data","variacao_NTCO3")
#Tratamento dos dados
NTCO3 <- NTCO3[nrow(NTCO3):1,]
NTCO3$data <- gsub(',','', NTCO3$data, fixed = TRUE)
NTCO3$variacao_NTCO3 <- gsub('%', '', NTCO3$variacao_NTCO3, fixed = TRUE) 
NTCO3$variacao_NTCO3 <- as.numeric(NTCO3$variacao_NTCO3)
NTCO3$variacao_NTCO3 <- NTCO3$variacao_NTCO3/100
NTCO3$data <- as.POSIXct(strptime(NTCO3$data, format= "%b %d %Y"))
NTCO3 <- na.omit(NTCO3)
#####
#UGPA3
#################
#Carregando o Dataset
UGPA3 <- fread("UGPA3_Historical_Data.csv", header = TRUE, sep = ",")
UGPA3 <- data.frame(UGPA3)
UGPA3 <- UGPA3[,-c(2:6)]
#Renomeando o Data Frame
names(UGPA3) <- c("data","variacao_UGPA3")
#Tratamento dos dados
UGPA3 <- UGPA3[nrow(UGPA3):1,]
UGPA3$data <- gsub(',','', UGPA3$data, fixed = TRUE)
UGPA3$variacao_UGPA3 <- gsub('%', '', UGPA3$variacao_UGPA3, fixed = TRUE) 
UGPA3$variacao_UGPA3 <- as.numeric(UGPA3$variacao_UGPA3)
UGPA3$variacao_UGPA3 <- UGPA3$variacao_UGPA3/100
UGPA3$data <- as.POSIXct(strptime(UGPA3$data, format= "%b %d %Y"))
UGPA3 <- na.omit(UGPA3)
#####
#AMAR3
#################
#Carregando o Dataset
AMAR3 <- fread("AMAR3_Historical_Data.csv", header = TRUE, sep = ",")
AMAR3 <- data.frame(AMAR3)
AMAR3 <- AMAR3[,-c(2:6)]
#Renomeando o Data Frame
names(AMAR3) <- c("data","variacao_AMAR3")
#Tratamento dos dados
AMAR3 <- AMAR3[nrow(AMAR3):1,]
AMAR3$data <- gsub(',','', AMAR3$data, fixed = TRUE)
AMAR3$variacao_AMAR3 <- gsub('%', '', AMAR3$variacao_AMAR3, fixed = TRUE) 
AMAR3$variacao_AMAR3 <- as.numeric(AMAR3$variacao_AMAR3)
AMAR3$variacao_AMAR3 <- AMAR3$variacao_AMAR3/100
AMAR3$data <- as.POSIXct(strptime(AMAR3$data, format= "%b %d %Y"))
AMAR3 <- na.omit(AMAR3)
#####
#BRDT3
#################
#Carregando o Dataset
BRDT3 <- fread("BRDT3_Historical_Data.csv", header = TRUE, sep = ",")
BRDT3 <- data.frame(BRDT3)
BRDT3 <- BRDT3[,-c(2:6)]
#Renomeando o Data Frame
names(BRDT3) <- c("data","variacao_BRDT3")
#Tratamento dos dados
BRDT3 <- BRDT3[nrow(BRDT3):1,]
BRDT3$data <- gsub(',','', BRDT3$data, fixed = TRUE)
BRDT3$variacao_BRDT3 <- gsub('%', '', BRDT3$variacao_BRDT3, fixed = TRUE) 
BRDT3$variacao_BRDT3 <- as.numeric(BRDT3$variacao_BRDT3)
BRDT3$variacao_BRDT3 <- BRDT3$variacao_BRDT3/100
BRDT3$data <- as.POSIXct(strptime(BRDT3$data, format= "%b %d %Y"))
BRDT3 <- na.omit(BRDT3)
#####
#TCSA3
#################
#Carregando o Dataset
TCSA3 <- fread("TCSA3_Historical_Data.csv", header = TRUE, sep = ",")
TCSA3 <- data.frame(TCSA3)
TCSA3 <- TCSA3[,-c(2:6)]
#Renomeando o Data Frame
names(TCSA3) <- c("data","variacao_TCSA3")
#Tratamento dos dados
TCSA3 <- TCSA3[nrow(TCSA3):1,]
TCSA3$data <- gsub(',','', TCSA3$data, fixed = TRUE)
TCSA3$variacao_TCSA3 <- gsub('%', '', TCSA3$variacao_TCSA3, fixed = TRUE) 
TCSA3$variacao_TCSA3 <- as.numeric(TCSA3$variacao_TCSA3)
TCSA3$variacao_TCSA3 <- TCSA3$variacao_TCSA3/100
TCSA3$data <- as.POSIXct(strptime(TCSA3$data, format= "%b %d %Y"))
TCSA3 <- na.omit(TCSA3)
#####
#TOTS3
#################
#Carregando o Dataset
TOTS3 <- fread("TOTS3_Historical_Data.csv", header = TRUE, sep = ",")
TOTS3 <- data.frame(TOTS3)
TOTS3 <- TOTS3[,-c(2:6)]
#Renomeando o Data Frame
names(TOTS3) <- c("data","variacao_TOTS3")
#Tratamento dos dados
TOTS3 <- TOTS3[nrow(TOTS3):1,]
TOTS3$data <- gsub(',','', TOTS3$data, fixed = TRUE)
TOTS3$variacao_TOTS3 <- gsub('%', '', TOTS3$variacao_TOTS3, fixed = TRUE) 
TOTS3$variacao_TOTS3 <- as.numeric(TOTS3$variacao_TOTS3)
TOTS3$variacao_TOTS3 <- TOTS3$variacao_TOTS3/100
TOTS3$data <- as.POSIXct(strptime(TOTS3$data, format= "%b %d %Y"))
TOTS3 <- na.omit(TOTS3)
#####
#ELET3
#################
#Carregando o Dataset
ELET3 <- fread("ELET3_Historical_Data.csv", header = TRUE, sep = ",")
ELET3 <- data.frame(ELET3)
ELET3 <- ELET3[,-c(2:6)]
#Renomeando o Data Frame
names(ELET3) <- c("data","variacao_ELET3")
#Tratamento dos dados
ELET3 <- ELET3[nrow(ELET3):1,]
ELET3$data <- gsub(',','', ELET3$data, fixed = TRUE)
ELET3$variacao_ELET3 <- gsub('%', '', ELET3$variacao_ELET3, fixed = TRUE) 
ELET3$variacao_ELET3 <- as.numeric(ELET3$variacao_ELET3)
ELET3$variacao_ELET3 <- ELET3$variacao_ELET3/100
ELET3$data <- as.POSIXct(strptime(ELET3$data, format= "%b %d %Y"))
ELET3 <- na.omit(ELET3)
#####
#JHSF3
#################
#Carregando o Dataset
JHSF3 <- fread("JHSF3_Historical_Data.csv", header = TRUE, sep = ",")
JHSF3 <- data.frame(JHSF3)
JHSF3 <- JHSF3[,-c(2:6)]
#Renomeando o Data Frame
names(JHSF3) <- c("data","variacao_JHSF3")
#Tratamento dos dados
JHSF3 <- JHSF3[nrow(JHSF3):1,]
JHSF3$data <- gsub(',','', JHSF3$data, fixed = TRUE)
JHSF3$variacao_JHSF3 <- gsub('%', '', JHSF3$variacao_JHSF3, fixed = TRUE) 
JHSF3$variacao_JHSF3 <- as.numeric(JHSF3$variacao_JHSF3)
JHSF3$variacao_JHSF3 <- JHSF3$variacao_JHSF3/100
JHSF3$data <- as.POSIXct(strptime(JHSF3$data, format= "%b %d %Y"))
JHSF3 <- na.omit(JHSF3)
#####
#BBSE3
#################
#Carregando o Dataset
BBSE3 <- fread("BBSE3_Historical_Data.csv", header = TRUE, sep = ",")
BBSE3 <- data.frame(BBSE3)
BBSE3 <- BBSE3[,-c(2:6)]
#Renomeando o Data Frame
names(BBSE3) <- c("data","variacao_BBSE3")
#Tratamento dos dados
BBSE3 <- BBSE3[nrow(BBSE3):1,]
BBSE3$data <- gsub(',','', BBSE3$data, fixed = TRUE)
BBSE3$variacao_BBSE3 <- gsub('%', '', BBSE3$variacao_BBSE3, fixed = TRUE) 
BBSE3$variacao_BBSE3 <- as.numeric(BBSE3$variacao_BBSE3)
BBSE3$variacao_BBSE3 <- BBSE3$variacao_BBSE3/100
BBSE3$data <- as.POSIXct(strptime(BBSE3$data, format= "%b %d %Y"))
BBSE3 <- na.omit(BBSE3)
#####
#RAPT4
#################
#Carregando o Dataset
RAPT4 <- fread("RAPT4_Historical_Data.csv", header = TRUE, sep = ",")
RAPT4 <- data.frame(RAPT4)
RAPT4 <- RAPT4[,-c(2:6)]
#Renomeando o Data Frame
names(RAPT4) <- c("data","variacao_RAPT4")
#Tratamento dos dados
RAPT4 <- RAPT4[nrow(RAPT4):1,]
RAPT4$data <- gsub(',','', RAPT4$data, fixed = TRUE)
RAPT4$variacao_RAPT4 <- gsub('%', '', RAPT4$variacao_RAPT4, fixed = TRUE) 
RAPT4$variacao_RAPT4 <- as.numeric(RAPT4$variacao_RAPT4)
RAPT4$variacao_RAPT4 <- RAPT4$variacao_RAPT4/100
RAPT4$data <- as.POSIXct(strptime(RAPT4$data, format= "%b %d %Y"))
RAPT4 <- na.omit(RAPT4)
#####
#MULT3
#################
#Carregando o Dataset
MULT3 <- fread("MULT3_Historical_Data.csv", header = TRUE, sep = ",")
MULT3 <- data.frame(MULT3)
MULT3 <- MULT3[,-c(2:6)]
#Renomeando o Data Frame
names(MULT3) <- c("data","variacao_MULT3")
#Tratamento dos dados
MULT3 <- MULT3[nrow(MULT3):1,]
MULT3$data <- gsub(',','', MULT3$data, fixed = TRUE)
MULT3$variacao_MULT3 <- gsub('%', '', MULT3$variacao_MULT3, fixed = TRUE) 
MULT3$variacao_MULT3 <- as.numeric(MULT3$variacao_MULT3)
MULT3$variacao_MULT3 <- MULT3$variacao_MULT3/100
MULT3$data <- as.POSIXct(strptime(MULT3$data, format= "%b %d %Y"))
MULT3 <- na.omit(MULT3)
#####
#WEGE3
#################
#Carregando o Dataset
WEGE3 <- fread("WEGE3_Historical_Data.csv", header = TRUE, sep = ",")
WEGE3 <- data.frame(WEGE3)
WEGE3 <- WEGE3[,-c(2:6)]
#Renomeando o Data Frame
names(WEGE3) <- c("data","variacao_WEGE3")
#Tratamento dos dados
WEGE3 <- WEGE3[nrow(WEGE3):1,]
WEGE3$data <- gsub(',','', WEGE3$data, fixed = TRUE)
WEGE3$variacao_WEGE3 <- gsub('%', '', WEGE3$variacao_WEGE3, fixed = TRUE) 
WEGE3$variacao_WEGE3 <- as.numeric(WEGE3$variacao_WEGE3)
WEGE3$variacao_WEGE3 <- WEGE3$variacao_WEGE3/100
WEGE3$data <- as.POSIXct(strptime(WEGE3$data, format= "%b %d %Y"))
WEGE3 <- na.omit(WEGE3)
#####
#EQTL3
#################
#Carregando o Dataset
EQTL3 <- fread("EQTL3_Historical_Data.csv", header = TRUE, sep = ",")
EQTL3 <- data.frame(EQTL3)
EQTL3 <- EQTL3[,-c(2:6)]
#Renomeando o Data Frame
names(EQTL3) <- c("data","variacao_EQTL3")
#Tratamento dos dados
EQTL3 <- EQTL3[nrow(EQTL3):1,]
EQTL3$data <- gsub(',','', EQTL3$data, fixed = TRUE)
EQTL3$variacao_EQTL3 <- gsub('%', '', EQTL3$variacao_EQTL3, fixed = TRUE) 
EQTL3$variacao_EQTL3 <- as.numeric(EQTL3$variacao_EQTL3)
EQTL3$variacao_EQTL3 <- EQTL3$variacao_EQTL3/100
EQTL3$data <- as.POSIXct(strptime(EQTL3$data, format= "%b %d %Y"))
EQTL3 <- na.omit(EQTL3)
#####       
#FLRY3
#################
#Carregando o Dataset
FLRY3 <- fread("FLRY3_Historical_Data.csv", header = TRUE, sep = ",")
FLRY3 <- data.frame(FLRY3)
FLRY3 <- FLRY3[,-c(2:6)]
#Renomeando o Data Frame
names(FLRY3) <- c("data","variacao_FLRY3")
#Tratamento dos dados
FLRY3 <- FLRY3[nrow(FLRY3):1,]
FLRY3$data <- gsub(',','', FLRY3$data, fixed = TRUE)
FLRY3$variacao_FLRY3 <- gsub('%', '', FLRY3$variacao_FLRY3, fixed = TRUE) 
FLRY3$variacao_FLRY3 <- as.numeric(FLRY3$variacao_FLRY3)
FLRY3$variacao_FLRY3 <- FLRY3$variacao_FLRY3/100
FLRY3$data <- as.POSIXct(strptime(FLRY3$data, format= "%b %d %Y"))
FLRY3 <- na.omit(FLRY3)
#####
#LAME4
#################
#Carregando o Dataset
LAME4 <- fread("LAME4_Historical_Data.csv", header = TRUE, sep = ",")
LAME4 <- data.frame(LAME4)
LAME4 <- LAME4[,-c(2:6)]
#Renomeando o Data Frame
names(LAME4) <- c("data","variacao_LAME4")
#Tratamento dos dados
LAME4 <- LAME4[nrow(LAME4):1,]
LAME4$data <- gsub(',','', LAME4$data, fixed = TRUE)
LAME4$variacao_LAME4 <- gsub('%', '', LAME4$variacao_LAME4, fixed = TRUE) 
LAME4$variacao_LAME4 <- as.numeric(LAME4$variacao_LAME4)
LAME4$variacao_LAME4 <- LAME4$variacao_LAME4/100
LAME4$data <- as.POSIXct(strptime(LAME4$data, format= "%b %d %Y"))
LAME4 <- na.omit(LAME4)
#####
#DTEX3
#################
#Carregando o Dataset
DTEX3 <- fread("DTEX3_Historical_Data.csv", header = TRUE, sep = ",")
DTEX3 <- data.frame(DTEX3)
DTEX3 <- DTEX3[,-c(2:6)]
#Renomeando o Data Frame
names(DTEX3) <- c("data","variacao_DTEX3")
#Tratamento dos dados
DTEX3 <- DTEX3[nrow(DTEX3):1,]
DTEX3$data <- gsub(',','', DTEX3$data, fixed = TRUE)
DTEX3$variacao_DTEX3 <- gsub('%', '', DTEX3$variacao_DTEX3, fixed = TRUE) 
DTEX3$variacao_DTEX3 <- as.numeric(DTEX3$variacao_DTEX3)
DTEX3$variacao_DTEX3 <- DTEX3$variacao_DTEX3/100
DTEX3$data <- as.POSIXct(strptime(DTEX3$data, format= "%b %d %Y"))
DTEX3 <- na.omit(DTEX3)
#####
#GNDI3
#################
#Carregando o Dataset
GNDI3 <- fread("GNDI3_Historical_Data.csv", header = TRUE, sep = ",")
GNDI3 <- data.frame(GNDI3)
GNDI3 <- GNDI3[,-c(2:6)]
#Renomeando o Data Frame
names(GNDI3) <- c("data","variacao_GNDI3")
#Tratamento dos dados
GNDI3 <- GNDI3[nrow(GNDI3):1,]
GNDI3$data <- gsub(',','', GNDI3$data, fixed = TRUE)
GNDI3$variacao_GNDI3 <- gsub('%', '', GNDI3$variacao_GNDI3, fixed = TRUE) 
GNDI3$variacao_GNDI3 <- as.numeric(GNDI3$variacao_GNDI3)
GNDI3$variacao_GNDI3 <- GNDI3$variacao_GNDI3/100
GNDI3$data <- as.POSIXct(strptime(GNDI3$data, format= "%b %d %Y"))
GNDI3 <- na.omit(GNDI3)
#####
#SBSP3  
#################
#Carregando o Dataset
SBSP3 <- fread("SBSP3_Historical_Data.csv", header = TRUE, sep = ",")
SBSP3 <- data.frame(SBSP3)
SBSP3 <- SBSP3[,-c(2:6)]
#Renomeando o Data Frame
names(SBSP3) <- c("data","variacao_SBSP3")
#Tratamento dos dados
SBSP3 <- SBSP3[nrow(SBSP3):1,]
SBSP3$data <- gsub(',','', SBSP3$data, fixed = TRUE)
SBSP3$variacao_SBSP3 <- gsub('%', '', SBSP3$variacao_SBSP3, fixed = TRUE) 
SBSP3$variacao_SBSP3 <- as.numeric(SBSP3$variacao_SBSP3)
SBSP3$variacao_SBSP3 <- SBSP3$variacao_SBSP3/100
SBSP3$data <- as.POSIXct(strptime(SBSP3$data, format= "%b %d %Y"))
SBSP3 <- na.omit(SBSP3)
#####
#MRVE3
#################
#Carregando o Dataset
MRVE3 <- fread("MRVE3_Historical_Data.csv", header = TRUE, sep = ",")
MRVE3 <- data.frame(MRVE3)
MRVE3 <- MRVE3[,-c(2:6)]
#Renomeando o Data Frame
names(MRVE3) <- c("data","variacao_MRVE3")
#Tratamento dos dados
MRVE3 <- MRVE3[nrow(MRVE3):1,]
MRVE3$data <- gsub(',','', MRVE3$data, fixed = TRUE)
MRVE3$variacao_MRVE3 <- gsub('%', '', MRVE3$variacao_MRVE3, fixed = TRUE) 
MRVE3$variacao_MRVE3 <- as.numeric(MRVE3$variacao_MRVE3)
MRVE3$variacao_MRVE3 <- MRVE3$variacao_MRVE3/100
MRVE3$data <- as.POSIXct(strptime(MRVE3$data, format= "%b %d %Y"))
MRVE3 <- na.omit(MRVE3)
#####
#BBDC3
#################
#Carregando o Dataset
BBDC3 <- fread("BBDC3_Historical_Data.csv", header = TRUE, sep = ",")
BBDC3 <- data.frame(BBDC3)
BBDC3 <- BBDC3[,-c(2:6)]
#Renomeando o Data Frame
names(BBDC3) <- c("data","variacao_BBDC3")
#Tratamento dos dados
BBDC3 <- BBDC3[nrow(BBDC3):1,]
BBDC3$data <- gsub(',','', BBDC3$data, fixed = TRUE)
BBDC3$variacao_BBDC3 <- gsub('%', '', BBDC3$variacao_BBDC3, fixed = TRUE) 
BBDC3$variacao_BBDC3 <- as.numeric(BBDC3$variacao_BBDC3)
BBDC3$variacao_BBDC3 <- BBDC3$variacao_BBDC3/100
BBDC3$data <- as.POSIXct(strptime(BBDC3$data, format= "%b %d %Y"))
BBDC3 <- na.omit(BBDC3)
#####
#HBOR3
#################
#Carregando o Dataset
HBOR3 <- fread("HBOR3_Historical_Data.csv", header = TRUE, sep = ",")
HBOR3 <- data.frame(HBOR3)
HBOR3 <- HBOR3[,-c(2:6)]
#Renomeando o Data Frame
names(HBOR3) <- c("data","variacao_HBOR3")
#Tratamento dos dados
HBOR3 <- HBOR3[nrow(HBOR3):1,]
HBOR3$data <- gsub(',','', HBOR3$data, fixed = TRUE)
HBOR3$variacao_HBOR3 <- gsub('%', '', HBOR3$variacao_HBOR3, fixed = TRUE) 
HBOR3$variacao_HBOR3 <- as.numeric(HBOR3$variacao_HBOR3)
HBOR3$variacao_HBOR3 <- HBOR3$variacao_HBOR3/100
HBOR3$data <- as.POSIXct(strptime(HBOR3$data, format= "%b %d %Y"))
HBOR3 <- na.omit(HBOR3)
#####
#SULA11
#################
#Carregando o Dataset
SULA11 <- fread("SULA11_Historical_Data.csv", header = TRUE, sep = ",")
SULA11 <- data.frame(SULA11)
SULA11 <- SULA11[,-c(2:6)]
#Renomeando o Data Frame
names(SULA11) <- c("data","variacao_SULA11")
#Tratamento dos dados
SULA11 <- SULA11[nrow(SULA11):1,]
SULA11$data <- gsub(',','', SULA11$data, fixed = TRUE)
SULA11$variacao_SULA11 <- gsub('%', '', SULA11$variacao_SULA11, fixed = TRUE) 
SULA11$variacao_SULA11 <- as.numeric(SULA11$variacao_SULA11)
SULA11$variacao_SULA11 <- SULA11$variacao_SULA11/100
SULA11$data <- as.POSIXct(strptime(SULA11$data, format= "%b %d %Y"))
SULA11 <- na.omit(SULA11)
#####               
#POSI3
#################
#Carregando o Dataset
POSI3 <- fread("POSI3_Historical_Data.csv", header = TRUE, sep = ",")
POSI3 <- data.frame(POSI3)
POSI3 <- POSI3[,-c(2:6)]
#Renomeando o Data Frame
names(POSI3) <- c("data","variacao_POSI3")
#Tratamento dos dados
POSI3 <- POSI3[nrow(POSI3):1,]
POSI3$data <- gsub(',','', POSI3$data, fixed = TRUE)
POSI3$variacao_POSI3 <- gsub('%', '', POSI3$variacao_POSI3, fixed = TRUE) 
POSI3$variacao_POSI3 <- as.numeric(POSI3$variacao_POSI3)
POSI3$variacao_POSI3 <- POSI3$variacao_POSI3/100
POSI3$data <- as.POSIXct(strptime(POSI3$data, format= "%b %d %Y"))
POSI3 <- na.omit(POSI3)
#####
#ECOR3
#################
#Carregando o Dataset
ECOR3 <- fread("ECOR3_Historical_Data.csv", header = TRUE, sep = ",")
ECOR3 <- data.frame(ECOR3)
ECOR3 <- ECOR3[,-c(2:6)]
#Renomeando o Data Frame
names(ECOR3) <- c("data","variacao_ECOR3")
#Tratamento dos dados
ECOR3 <- ECOR3[nrow(ECOR3):1,]
ECOR3$data <- gsub(',','', ECOR3$data, fixed = TRUE)
ECOR3$variacao_ECOR3 <- gsub('%', '', ECOR3$variacao_ECOR3, fixed = TRUE) 
ECOR3$variacao_ECOR3 <- as.numeric(ECOR3$variacao_ECOR3)
ECOR3$variacao_ECOR3 <- ECOR3$variacao_ECOR3/100
ECOR3$data <- as.POSIXct(strptime(ECOR3$data, format= "%b %d %Y"))
ECOR3 <- na.omit(ECOR3)
#####
#GFSA3
#################
#Carregando o Dataset
GFSA3 <- fread("GFSA3_Historical_Data.csv", header = TRUE, sep = ",")
GFSA3 <- data.frame(GFSA3)
GFSA3 <- GFSA3[,-c(2:6)]
#Renomeando o Data Frame
names(GFSA3) <- c("data","variacao_GFSA3")
#Tratamento dos dados
GFSA3 <- GFSA3[nrow(GFSA3):1,]
GFSA3$data <- gsub(',','', GFSA3$data, fixed = TRUE)
GFSA3$variacao_GFSA3 <- gsub('%', '', GFSA3$variacao_GFSA3, fixed = TRUE) 
GFSA3$variacao_GFSA3 <- as.numeric(GFSA3$variacao_GFSA3)
GFSA3$variacao_GFSA3 <- GFSA3$variacao_GFSA3/100
GFSA3$data <- as.POSIXct(strptime(GFSA3$data, format= "%b %d %Y"))
GFSA3 <- na.omit(GFSA3)
#####
#LCAM3
#################
#Carregando o Dataset
LCAM3 <- fread("LCAM3_Historical_Data.csv", header = TRUE, sep = ",")
LCAM3 <- data.frame(LCAM3)
LCAM3 <- LCAM3[,-c(2:6)]
#Renomeando o Data Frame
names(LCAM3) <- c("data","variacao_LCAM3")
#Tratamento dos dados
LCAM3 <- LCAM3[nrow(LCAM3):1,]
LCAM3$data <- gsub(',','', LCAM3$data, fixed = TRUE)
LCAM3$variacao_LCAM3 <- gsub('%', '', LCAM3$variacao_LCAM3, fixed = TRUE) 
LCAM3$variacao_LCAM3 <- as.numeric(LCAM3$variacao_LCAM3)
LCAM3$variacao_LCAM3 <- LCAM3$variacao_LCAM3/100
LCAM3$data <- as.POSIXct(strptime(LCAM3$data, format= "%b %d %Y"))
LCAM3 <- na.omit(LCAM3)
#####
#QUAL3
#################
#Carregando o Dataset
QUAL3 <- fread("QUAL3_Historical_Data.csv", header = TRUE, sep = ",")
QUAL3 <- data.frame(QUAL3)
QUAL3 <- QUAL3[,-c(2:6)]
#Renomeando o Data Frame
names(QUAL3) <- c("data","variacao_QUAL3")
#Tratamento dos dados
QUAL3 <- QUAL3[nrow(QUAL3):1,]
QUAL3$data <- gsub(',','', QUAL3$data, fixed = TRUE)
QUAL3$variacao_QUAL3 <- gsub('%', '', QUAL3$variacao_QUAL3, fixed = TRUE) 
QUAL3$variacao_QUAL3 <- as.numeric(QUAL3$variacao_QUAL3)
QUAL3$variacao_QUAL3 <- QUAL3$variacao_QUAL3/100
QUAL3$data <- as.POSIXct(strptime(QUAL3$data, format= "%b %d %Y"))
QUAL3 <- na.omit(QUAL3)
#####
#PRIO3
#################
#Carregando o Dataset
PRIO3 <- fread("PRIO3_Historical_Data.csv", header = TRUE, sep = ",")
PRIO3 <- data.frame(PRIO3)
PRIO3 <- PRIO3[,-c(2:6)]
#Renomeando o Data Frame
names(PRIO3) <- c("data","variacao_PRIO3")
#Tratamento dos dados
PRIO3 <- PRIO3[nrow(PRIO3):1,]
PRIO3$data <- gsub(',','', PRIO3$data, fixed = TRUE)
PRIO3$variacao_PRIO3 <- gsub('%', '', PRIO3$variacao_PRIO3, fixed = TRUE) 
PRIO3$variacao_PRIO3 <- as.numeric(PRIO3$variacao_PRIO3)
PRIO3$variacao_PRIO3 <- PRIO3$variacao_PRIO3/100
PRIO3$data <- as.POSIXct(strptime(PRIO3$data, format= "%b %d %Y"))
PRIO3 <- na.omit(PRIO3)
#####
#YDUQ3
#################
#Carregando o Dataset
YDUQ3 <- fread("YDUQ3_Historical_Data.csv", header = TRUE, sep = ",")
YDUQ3 <- data.frame(YDUQ3)
YDUQ3 <- YDUQ3[,-c(2:6)]
#Renomeando o Data Frame
names(YDUQ3) <- c("data","variacao_YDUQ3")
#Tratamento dos dados
YDUQ3 <- YDUQ3[nrow(YDUQ3):1,]
YDUQ3$data <- gsub(',','', YDUQ3$data, fixed = TRUE)
YDUQ3$variacao_YDUQ3 <- gsub('%', '', YDUQ3$variacao_YDUQ3, fixed = TRUE) 
YDUQ3$variacao_YDUQ3 <- as.numeric(YDUQ3$variacao_YDUQ3)
YDUQ3$variacao_YDUQ3 <- YDUQ3$variacao_YDUQ3/100
YDUQ3$data <- as.POSIXct(strptime(YDUQ3$data, format= "%b %d %Y"))
YDUQ3 <- na.omit(YDUQ3)
#####
#CEAB3
#################
#Carregando o Dataset
CEAB3 <- fread("CEAB3_Historical_Data.csv", header = TRUE, sep = ",")
CEAB3 <- data.frame(CEAB3)
CEAB3 <- CEAB3[,-c(2:6)]
#Renomeando o Data Frame
names(CEAB3) <- c("data","variacao_CEAB3")
#Tratamento dos dados
CEAB3 <- CEAB3[nrow(CEAB3):1,]
CEAB3$data <- gsub(',','', CEAB3$data, fixed = TRUE)
CEAB3$variacao_CEAB3 <- gsub('%', '', CEAB3$variacao_CEAB3, fixed = TRUE) 
CEAB3$variacao_CEAB3 <- as.numeric(CEAB3$variacao_CEAB3)
CEAB3$variacao_CEAB3 <- CEAB3$variacao_CEAB3/100
CEAB3$data <- as.POSIXct(strptime(CEAB3$data, format= "%b %d %Y"))
CEAB3 <- na.omit(CEAB3)
#####
#CPFE3
#################
#Carregando o Dataset
CPFE3 <- fread("CPFE3_Historical_Data.csv", header = TRUE, sep = ",")
CPFE3 <- data.frame(CPFE3)
CPFE3 <- CPFE3[,-c(2:6)]
#Renomeando o Data Frame
names(CPFE3) <- c("data","variacao_CPFE3")
#Tratamento dos dados
CPFE3 <- CPFE3[nrow(CPFE3):1,]
CPFE3$data <- gsub(',','', CPFE3$data, fixed = TRUE)
CPFE3$variacao_CPFE3 <- gsub('%', '', CPFE3$variacao_CPFE3, fixed = TRUE) 
CPFE3$variacao_CPFE3 <- as.numeric(CPFE3$variacao_CPFE3)
CPFE3$variacao_CPFE3 <- CPFE3$variacao_CPFE3/100
CPFE3$data <- as.POSIXct(strptime(CPFE3$data, format= "%b %d %Y"))
CPFE3 <- na.omit(CPFE3)
#####
#MOVI3
#################
#Carregando o Dataset
MOVI3 <- fread("MOVI3_Historical_Data.csv", header = TRUE, sep = ",")
MOVI3 <- data.frame(MOVI3)
MOVI3 <- MOVI3[,-c(2:6)]
#Renomeando o Data Frame
names(MOVI3) <- c("data","variacao_MOVI3")
#Tratamento dos dados
MOVI3 <- MOVI3[nrow(MOVI3):1,]
MOVI3$data <- gsub(',','', MOVI3$data, fixed = TRUE)
MOVI3$variacao_MOVI3 <- gsub('%', '', MOVI3$variacao_MOVI3, fixed = TRUE) 
MOVI3$variacao_MOVI3 <- as.numeric(MOVI3$variacao_MOVI3)
MOVI3$variacao_MOVI3 <- MOVI3$variacao_MOVI3/100
MOVI3$data <- as.POSIXct(strptime(MOVI3$data, format= "%b %d %Y"))
MOVI3 <- na.omit(MOVI3)
#####                 
#STBP3
#################
#Carregando o Dataset
STBP3 <- fread("STBP3_Historical_Data.csv", header = TRUE, sep = ",")
STBP3 <- data.frame(STBP3)
STBP3 <- STBP3[,-c(2:6)]
#Renomeando o Data Frame
names(STBP3) <- c("data","variacao_STBP3")
#Tratamento dos dados
STBP3 <- STBP3[nrow(STBP3):1,]
STBP3$data <- gsub(',','', STBP3$data, fixed = TRUE)
STBP3$variacao_STBP3 <- gsub('%', '', STBP3$variacao_STBP3, fixed = TRUE) 
STBP3$variacao_STBP3 <- as.numeric(STBP3$variacao_STBP3)
STBP3$variacao_STBP3 <- STBP3$variacao_STBP3/100
STBP3$data <- as.POSIXct(strptime(STBP3$data, format= "%b %d %Y"))
STBP3 <- na.omit(STBP3)
#####
#CNTO3
#################
#Carregando o Dataset
CNTO3 <- fread("CNTO3_Historical_Data.csv", header = TRUE, sep = ",")
CNTO3 <- data.frame(CNTO3)
CNTO3 <- CNTO3[,-c(2:6)]
#Renomeando o Data Frame
names(CNTO3) <- c("data","variacao_CNTO3")
#Tratamento dos dados
CNTO3 <- CNTO3[nrow(CNTO3):1,]
CNTO3$data <- gsub(',','', CNTO3$data, fixed = TRUE)
CNTO3$variacao_CNTO3 <- gsub('%', '', CNTO3$variacao_CNTO3, fixed = TRUE) 
CNTO3$variacao_CNTO3 <- as.numeric(CNTO3$variacao_CNTO3)
CNTO3$variacao_CNTO3 <- CNTO3$variacao_CNTO3/100
CNTO3$data <- as.POSIXct(strptime(CNTO3$data, format= "%b %d %Y"))
CNTO3 <- na.omit(CNTO3)
#####
#BTOW3
#################
#Carregando o Dataset
BTOW3 <- fread("BTOW3_Historical_Data.csv", header = TRUE, sep = ",")
BTOW3 <- data.frame(BTOW3)
BTOW3 <- BTOW3[,-c(2:6)]
#Renomeando o Data Frame
names(BTOW3) <- c("data","variacao_BTOW3")
#Tratamento dos dados
BTOW3 <- BTOW3[nrow(BTOW3):1,]
BTOW3$data <- gsub(',','', BTOW3$data, fixed = TRUE)
BTOW3$variacao_BTOW3 <- gsub('%', '', BTOW3$variacao_BTOW3, fixed = TRUE) 
BTOW3$variacao_BTOW3 <- as.numeric(BTOW3$variacao_BTOW3)
BTOW3$variacao_BTOW3 <- BTOW3$variacao_BTOW3/100
BTOW3$data <- as.POSIXct(strptime(BTOW3$data, format= "%b %d %Y"))
BTOW3 <- na.omit(BTOW3)
#####
#LOGN3
#################
#Carregando o Dataset
LOGN3 <- fread("LOGN3_Historical_Data.csv", header = TRUE, sep = ",")
LOGN3 <- data.frame(LOGN3)
LOGN3 <- LOGN3[,-c(2:6)]
#Renomeando o Data Frame
names(LOGN3) <- c("data","variacao_LOGN3")
#Tratamento dos dados
LOGN3 <- LOGN3[nrow(LOGN3):1,]
LOGN3$data <- gsub(',','', LOGN3$data, fixed = TRUE)
LOGN3$variacao_LOGN3 <- gsub('%', '', LOGN3$variacao_LOGN3, fixed = TRUE) 
LOGN3$variacao_LOGN3 <- as.numeric(LOGN3$variacao_LOGN3)
LOGN3$variacao_LOGN3 <- LOGN3$variacao_LOGN3/100
LOGN3$data <- as.POSIXct(strptime(LOGN3$data, format= "%b %d %Y"))
LOGN3 <- na.omit(LOGN3)
#####
#EVEN3
#################
#Carregando o Dataset
EVEN3 <- fread("EVEN3_Historical_Data.csv", header = TRUE, sep = ",")
EVEN3 <- data.frame(EVEN3)
EVEN3 <- EVEN3[,-c(2:6)]
#Renomeando o Data Frame
names(EVEN3) <- c("data","variacao_EVEN3")
#Tratamento dos dados
EVEN3 <- EVEN3[nrow(EVEN3):1,]
EVEN3$data <- gsub(',','', EVEN3$data, fixed = TRUE)
EVEN3$variacao_EVEN3 <- gsub('%', '', EVEN3$variacao_EVEN3, fixed = TRUE) 
EVEN3$variacao_EVEN3 <- as.numeric(EVEN3$variacao_EVEN3)
EVEN3$variacao_EVEN3 <- EVEN3$variacao_EVEN3/100
EVEN3$data <- as.POSIXct(strptime(EVEN3$data, format= "%b %d %Y"))
EVEN3 <- na.omit(EVEN3)
#####
#ALPA4
#################
#Carregando o Dataset
ALPA4 <- fread("ALPA4_Historical_Data.csv", header = TRUE, sep = ",")
ALPA4 <- data.frame(ALPA4)
ALPA4 <- ALPA4[,-c(2:6)]
#Renomeando o Data Frame
names(ALPA4) <- c("data","variacao_ALPA4")
#Tratamento dos dados
ALPA4 <- ALPA4[nrow(ALPA4):1,]
ALPA4$data <- gsub(',','', ALPA4$data, fixed = TRUE)
ALPA4$variacao_ALPA4 <- gsub('%', '', ALPA4$variacao_ALPA4, fixed = TRUE) 
ALPA4$variacao_ALPA4 <- as.numeric(ALPA4$variacao_ALPA4)
ALPA4$variacao_ALPA4 <- ALPA4$variacao_ALPA4/100
ALPA4$data <- as.POSIXct(strptime(ALPA4$data, format= "%b %d %Y"))
ALPA4 <- na.omit(ALPA4)
#####
#BRSR6
#################
#Carregando o Dataset
BRSR6 <- fread("BRSR6_Historical_Data.csv", header = TRUE, sep = ",")
BRSR6 <- data.frame(BRSR6)
BRSR6 <- BRSR6[,-c(2:6)]
#Renomeando o Data Frame
names(BRSR6) <- c("data","variacao_BRSR6")
#Tratamento dos dados
BRSR6 <- BRSR6[nrow(BRSR6):1,]
BRSR6$data <- gsub(',','', BRSR6$data, fixed = TRUE)
BRSR6$variacao_BRSR6 <- gsub('%', '', BRSR6$variacao_BRSR6, fixed = TRUE) 
BRSR6$variacao_BRSR6 <- as.numeric(BRSR6$variacao_BRSR6)
BRSR6$variacao_BRSR6 <- BRSR6$variacao_BRSR6/100
BRSR6$data <- as.POSIXct(strptime(BRSR6$data, format= "%b %d %Y"))
BRSR6 <- na.omit(BRSR6)
#####
#BPAC11
#################
#Carregando o Dataset
BPAC11 <- fread("BPAC11_Historical_Data.csv", header = TRUE, sep = ",")
BPAC11 <- data.frame(BPAC11)
BPAC11 <- BPAC11[,-c(2:6)]
#Renomeando o Data Frame
names(BPAC11) <- c("data","variacao_BPAC11")
#Tratamento dos dados
BPAC11 <- BPAC11[nrow(BPAC11):1,]
BPAC11$data <- gsub(',','', BPAC11$data, fixed = TRUE)
BPAC11$variacao_BPAC11 <- gsub('%', '', BPAC11$variacao_BPAC11, fixed = TRUE) 
BPAC11$variacao_BPAC11 <- as.numeric(BPAC11$variacao_BPAC11)
BPAC11$variacao_BPAC11 <- BPAC11$variacao_BPAC11/100
BPAC11$data <- as.POSIXct(strptime(BPAC11$data, format= "%b %d %Y"))
BPAC11 <- na.omit(BPAC11)
#####
#CRFB3
#################
#Carregando o Dataset
CRFB3 <- fread("CRFB3_Historical_Data.csv", header = TRUE, sep = ",")
CRFB3 <- data.frame(CRFB3)
CRFB3 <- CRFB3[,-c(2:6)]
#Renomeando o Data Frame
names(CRFB3) <- c("data","variacao_CRFB3")
#Tratamento dos dados
CRFB3 <- CRFB3[nrow(CRFB3):1,]
CRFB3$data <- gsub(',','', CRFB3$data, fixed = TRUE)
CRFB3$variacao_CRFB3 <- gsub('%', '', CRFB3$variacao_CRFB3, fixed = TRUE) 
CRFB3$variacao_CRFB3 <- as.numeric(CRFB3$variacao_CRFB3)
CRFB3$variacao_CRFB3 <- CRFB3$variacao_CRFB3/100
CRFB3$data <- as.POSIXct(strptime(CRFB3$data, format= "%b %d %Y"))
CRFB3 <- na.omit(CRFB3)
#####
#EZTC3
#################
#Carregando o Dataset
EZTC3 <- fread("EZTC3_Historical_Data.csv", header = TRUE, sep = ",")
EZTC3 <- data.frame(EZTC3)
EZTC3 <- EZTC3[,-c(2:6)]
#Renomeando o Data Frame
names(EZTC3) <- c("data","variacao_EZTC3")
#Tratamento dos dados
EZTC3 <- EZTC3[nrow(EZTC3):1,]
EZTC3$data <- gsub(',','', EZTC3$data, fixed = TRUE)
EZTC3$variacao_EZTC3 <- gsub('%', '', EZTC3$variacao_EZTC3, fixed = TRUE) 
EZTC3$variacao_EZTC3 <- as.numeric(EZTC3$variacao_EZTC3)
EZTC3$variacao_EZTC3 <- EZTC3$variacao_EZTC3/100
EZTC3$data <- as.POSIXct(strptime(EZTC3$data, format= "%b %d %Y"))
EZTC3 <- na.omit(EZTC3)
#####
#CVCB3
#################
#Carregando o Dataset
CVCB3 <- fread("CVCB3_Historical_Data.csv", header = TRUE, sep = ",")
CVCB3 <- data.frame(CVCB3)
CVCB3 <- CVCB3[,-c(2:6)]
#Renomeando o Data Frame
names(CVCB3) <- c("data","variacao_CVCB3")
#Tratamento dos dados
CVCB3 <- CVCB3[nrow(CVCB3):1,]
CVCB3$data <- gsub(',','', CVCB3$data, fixed = TRUE)
CVCB3$variacao_CVCB3 <- gsub('%', '', CVCB3$variacao_CVCB3, fixed = TRUE) 
CVCB3$variacao_CVCB3 <- as.numeric(CVCB3$variacao_CVCB3)
CVCB3$variacao_CVCB3 <- CVCB3$variacao_CVCB3/100
CVCB3$data <- as.POSIXct(strptime(CVCB3$data, format= "%b %d %Y"))
CVCB3 <- na.omit(CVCB3)
#####
#SANB11
#################
#Carregando o Dataset
SANB11 <- fread("SANB11_Historical_Data.csv", header = TRUE, sep = ",")
SANB11 <- data.frame(SANB11)
SANB11 <- SANB11[,-c(2:6)]
#Renomeando o Data Frame
names(SANB11) <- c("data","variacao_SANB11")
#Tratamento dos dados
SANB11 <- SANB11[nrow(SANB11):1,]
SANB11$data <- gsub(',','', SANB11$data, fixed = TRUE)
SANB11$variacao_SANB11 <- gsub('%', '', SANB11$variacao_SANB11, fixed = TRUE) 
SANB11$variacao_SANB11 <- as.numeric(SANB11$variacao_SANB11)
SANB11$variacao_SANB11 <- SANB11$variacao_SANB11/100
SANB11$data <- as.POSIXct(strptime(SANB11$data, format= "%b %d %Y"))
SANB11 <- na.omit(SANB11)
#####
#ENBR3
#################
#Carregando o Dataset
ENBR3 <- fread("ENBR3_Historical_Data.csv", header = TRUE, sep = ",")
ENBR3 <- data.frame(ENBR3)
ENBR3 <- ENBR3[,-c(2:6)]
#Renomeando o Data Frame
names(ENBR3) <- c("data","variacao_ENBR3")
#Tratamento dos dados
ENBR3 <- ENBR3[nrow(ENBR3):1,]
ENBR3$data <- gsub(',','', ENBR3$data, fixed = TRUE)
ENBR3$variacao_ENBR3 <- gsub('%', '', ENBR3$variacao_ENBR3, fixed = TRUE) 
ENBR3$variacao_ENBR3 <- as.numeric(ENBR3$variacao_ENBR3)
ENBR3$variacao_ENBR3 <- ENBR3$variacao_ENBR3/100
ENBR3$data <- as.POSIXct(strptime(ENBR3$data, format= "%b %d %Y"))
ENBR3 <- na.omit(ENBR3)
#####
#ENAT3
#################
#Carregando o Dataset
ENAT3 <- fread("ENAT3_Historical_Data.csv", header = TRUE, sep = ",")
ENAT3 <- data.frame(ENAT3)
ENAT3 <- ENAT3[,-c(2:6)]
#Renomeando o Data Frame
names(ENAT3) <- c("data","variacao_ENAT3")
#Tratamento dos dados
ENAT3 <- ENAT3[nrow(ENAT3):1,]
ENAT3$data <- gsub(',','', ENAT3$data, fixed = TRUE)
ENAT3$variacao_ENAT3 <- gsub('%', '', ENAT3$variacao_ENAT3, fixed = TRUE) 
ENAT3$variacao_ENAT3 <- as.numeric(ENAT3$variacao_ENAT3)
ENAT3$variacao_ENAT3 <- ENAT3$variacao_ENAT3/100
ENAT3$data <- as.POSIXct(strptime(ENAT3$data, format= "%b %d %Y"))
ENAT3 <- na.omit(ENAT3)
#####
#MEAL3
#################
#Carregando o Dataset
MEAL3 <- fread("MEAL3_Historical_Data.csv", header = TRUE, sep = ",")
MEAL3 <- data.frame(MEAL3)
MEAL3 <- MEAL3[,-c(2:6)]
#Renomeando o Data Frame
names(MEAL3) <- c("data","variacao_MEAL3")
#Tratamento dos dados
MEAL3 <- MEAL3[nrow(MEAL3):1,]
MEAL3$data <- gsub(',','', MEAL3$data, fixed = TRUE)
MEAL3$variacao_MEAL3 <- gsub('%', '', MEAL3$variacao_MEAL3, fixed = TRUE) 
MEAL3$variacao_MEAL3 <- as.numeric(MEAL3$variacao_MEAL3)
MEAL3$variacao_MEAL3 <- MEAL3$variacao_MEAL3/100
MEAL3$data <- as.POSIXct(strptime(MEAL3$data, format= "%b %d %Y"))
MEAL3 <- na.omit(MEAL3)
#####
#LIGT3
#################
#Carregando o Dataset
LIGT3 <- fread("LIGT3_Historical_Data.csv", header = TRUE, sep = ",")
LIGT3 <- data.frame(LIGT3)
LIGT3 <- LIGT3[,-c(2:6)]
#Renomeando o Data Frame
names(LIGT3) <- c("data","variacao_LIGT3")
#Tratamento dos dados
LIGT3 <- LIGT3[nrow(LIGT3):1,]
LIGT3$data <- gsub(',','', LIGT3$data, fixed = TRUE)
LIGT3$variacao_LIGT3 <- gsub('%', '', LIGT3$variacao_LIGT3, fixed = TRUE) 
LIGT3$variacao_LIGT3 <- as.numeric(LIGT3$variacao_LIGT3)
LIGT3$variacao_LIGT3 <- LIGT3$variacao_LIGT3/100
LIGT3$data <- as.POSIXct(strptime(LIGT3$data, format= "%b %d %Y"))
LIGT3 <- na.omit(LIGT3)
#####
#HAPV3
#################
#Carregando o Dataset
HAPV3 <- fread("HAPV3_Historical_Data.csv", header = TRUE, sep = ",")
HAPV3 <- data.frame(HAPV3)
HAPV3 <- HAPV3[,-c(2:6)]
#Renomeando o Data Frame
names(HAPV3) <- c("data","variacao_HAPV3")
#Tratamento dos dados
HAPV3 <- HAPV3[nrow(HAPV3):1,]
HAPV3$data <- gsub(',','', HAPV3$data, fixed = TRUE)
HAPV3$variacao_HAPV3 <- gsub('%', '', HAPV3$variacao_HAPV3, fixed = TRUE) 
HAPV3$variacao_HAPV3 <- as.numeric(HAPV3$variacao_HAPV3)
HAPV3$variacao_HAPV3 <- HAPV3$variacao_HAPV3/100
HAPV3$data <- as.POSIXct(strptime(HAPV3$data, format= "%b %d %Y"))
HAPV3 <- na.omit(HAPV3)
#####
#VIVT4
#################
#Carregando o Dataset
VIVT4 <- fread("VIVT4_Historical_Data.csv", header = TRUE, sep = ",")
VIVT4 <- data.frame(VIVT4)
VIVT4 <- VIVT4[,-c(2:6)]
#Renomeando o Data Frame
names(VIVT4) <- c("data","variacao_VIVT4")
#Tratamento dos dados
VIVT4 <- VIVT4[nrow(VIVT4):1,]
VIVT4$data <- gsub(',','', VIVT4$data, fixed = TRUE)
VIVT4$variacao_VIVT4 <- gsub('%', '', VIVT4$variacao_VIVT4, fixed = TRUE) 
VIVT4$variacao_VIVT4 <- as.numeric(VIVT4$variacao_VIVT4)
VIVT4$variacao_VIVT4 <- VIVT4$variacao_VIVT4/100
VIVT4$data <- as.POSIXct(strptime(VIVT4$data, format= "%b %d %Y"))
VIVT4 <- na.omit(VIVT4)
#####
#ELET6
#################
#Carregando o Dataset
ELET6 <- fread("ELET6_Historical_Data.csv", header = TRUE, sep = ",")
ELET6 <- data.frame(ELET6)
ELET6 <- ELET6[,-c(2:6)]
#Renomeando o Data Frame
names(ELET6) <- c("data","variacao_ELET6")
#Tratamento dos dados
ELET6 <- ELET6[nrow(ELET6):1,]
ELET6$data <- gsub(',','', ELET6$data, fixed = TRUE)
ELET6$variacao_ELET6 <- gsub('%', '', ELET6$variacao_ELET6, fixed = TRUE) 
ELET6$variacao_ELET6 <- as.numeric(ELET6$variacao_ELET6)
ELET6$variacao_ELET6 <- ELET6$variacao_ELET6/100
ELET6$data <- as.POSIXct(strptime(ELET6$data, format= "%b %d %Y"))
ELET6 <- na.omit(ELET6)
#####
#ALSO3
#################
#Carregando o Dataset
ALSO3 <- fread("ALSO3_Historical_Data.csv", header = TRUE, sep = ",")
ALSO3 <- data.frame(ALSO3)
ALSO3 <- ALSO3[,-c(2:6)]
#Renomeando o Data Frame
names(ALSO3) <- c("data","variacao_ALSO3")
#Tratamento dos dados
ALSO3 <- ALSO3[nrow(ALSO3):1,]
ALSO3$data <- gsub(',','', ALSO3$data, fixed = TRUE)
ALSO3$variacao_ALSO3 <- gsub('%', '', ALSO3$variacao_ALSO3, fixed = TRUE) 
ALSO3$variacao_ALSO3 <- as.numeric(ALSO3$variacao_ALSO3)
ALSO3$variacao_ALSO3 <- ALSO3$variacao_ALSO3/100
ALSO3$data <- as.POSIXct(strptime(ALSO3$data, format= "%b %d %Y"))
ALSO3 <- na.omit(ALSO3)
#####
#HYPE3
#################
#Carregando o Dataset
HYPE3 <- fread("HYPE3_Historical_Data.csv", header = TRUE, sep = ",")
HYPE3 <- data.frame(HYPE3)
HYPE3 <- HYPE3[,-c(2:6)]
#Renomeando o Data Frame
names(HYPE3) <- c("data","variacao_HYPE3")
#Tratamento dos dados
HYPE3 <- HYPE3[nrow(HYPE3):1,]
HYPE3$data <- gsub(',','', HYPE3$data, fixed = TRUE)
HYPE3$variacao_HYPE3 <- gsub('%', '', HYPE3$variacao_HYPE3, fixed = TRUE) 
HYPE3$variacao_HYPE3 <- as.numeric(HYPE3$variacao_HYPE3)
HYPE3$variacao_HYPE3 <- HYPE3$variacao_HYPE3/100
HYPE3$data <- as.POSIXct(strptime(HYPE3$data, format= "%b %d %Y"))
HYPE3 <- na.omit(HYPE3)
#####
#BRPR3
#################
#Carregando o Dataset
BRPR3 <- fread("BRPR3_Historical_Data.csv", header = TRUE, sep = ",")
BRPR3 <- data.frame(BRPR3)
BRPR3 <- BRPR3[,-c(2:6)]
#Renomeando o Data Frame
names(BRPR3) <- c("data","variacao_BRPR3")
#Tratamento dos dados
BRPR3 <- BRPR3[nrow(BRPR3):1,]
BRPR3$data <- gsub(',','', BRPR3$data, fixed = TRUE)
BRPR3$variacao_BRPR3 <- gsub('%', '', BRPR3$variacao_BRPR3, fixed = TRUE) 
BRPR3$variacao_BRPR3 <- as.numeric(BRPR3$variacao_BRPR3)
BRPR3$variacao_BRPR3 <- BRPR3$variacao_BRPR3/100
BRPR3$data <- as.POSIXct(strptime(BRPR3$data, format= "%b %d %Y"))
BRPR3 <- na.omit(BRPR3)
#####
#BRAP4
#################
#Carregando o Dataset
BRAP4 <- fread("BRAP4_Historical_Data.csv", header = TRUE, sep = ",")
BRAP4 <- data.frame(BRAP4)
BRAP4 <- BRAP4[,-c(2:6)]
#Renomeando o Data Frame
names(BRAP4) <- c("data","variacao_BRAP4")
#Tratamento dos dados
BRAP4 <- BRAP4[nrow(BRAP4):1,]
BRAP4$data <- gsub(',','', BRAP4$data, fixed = TRUE)
BRAP4$variacao_BRAP4 <- gsub('%', '', BRAP4$variacao_BRAP4, fixed = TRUE) 
BRAP4$variacao_BRAP4 <- as.numeric(BRAP4$variacao_BRAP4)
BRAP4$variacao_BRAP4 <- BRAP4$variacao_BRAP4/100
BRAP4$data <- as.POSIXct(strptime(BRAP4$data, format= "%b %d %Y"))
BRAP4 <- na.omit(BRAP4)
###############################################################################


##################      MARKOWITZ


###############################################################################

#install.packages("quadprog")
#install.packages("PerformanceAnalytics")
#install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
#install.packages("splitstackshape")
#install.packages("plyr")
library(plyr)
library(splitstackshape)
library(IntroCompFinR)

#########
#Criando a matriz de retornos
#ativos <- c("PETR4","OIBR3","VVAR3","COGN3","BBDC4","ITUB4","VALE3","PETR3","GGBR4",
#            "ITSA4","ABEV3","JBSS3","RENT3","B3SA3","CIEL3","BBAS3","CSNA3","USIM5",
#            "GOAU4","EMBR3","MRFG3","GOLL4","BRFS3","CYRE3","BRML3",
#            "LREN3","KLBN11","HGTX3","CCRO3","RAIL3","MGLU3","POMO4","BRKM5","TIMP3","BEEF3",
#            "NTCO3","UGPA3","AMAR3","TCSA3","TOTS3","ELET3","JHSF3","BBSE3","RAPT4",
#            "MULT3","WEGE3","EQTL3","LAME4","DTEX3","SBSP3","MRVE3","BBDC3",
#            "HBOR3","SULA11","POSI3","ECOR3","GFSA3","LCAM3","QUAL3","PRIO3","YDUQ3",
#            "CPFE3","BTOW3","LOGN3","EVEN3","ALPA4","BRSR6",
#            "EZTC3","CVCB3","SANB11","ENBR3","ENAT3","MEAL3","LIGT3","VIVT4",
#            "ELET6","ALSO3","HYPE3","BRPR3","BRAP4")
######### 
#MENORES DO QUE 1000 DADOS
#AZUL4 762  BPAC11 777  BRDT3 594 CEAB3 134 CMIG4 537 CNTO3 268 CRFB3 695 FLRY3 526
#GNDI3 510  HAPV3 508   IRBR3 687 MOVI3 805 NEOE3 218 STBP3 921 SUZB3 616
#OIBR3 RECUPERAÇÃO JUDICIAL
#########
lista <- list(PETR4,VVAR3,COGN3,BBDC4,ITUB4,VALE3,PETR3,GGBR4,ITSA4,
              ABEV3,JBSS3,RENT3,B3SA3,CIEL3,BBAS3,CSNA3,USIM5,GOAU4,
              EMBR3,MRFG3,GOLL4,BRFS3,CYRE3,BRML3,LREN3,KLBN11,HGTX3,
              CCRO3,RAIL3,MGLU3,POMO4,BRKM5,TIMP3,BEEF3,NTCO3,UGPA3,
              AMAR3,TCSA3,TOTS3,ELET3,JHSF3,BBSE3,RAPT4,MULT3,WEGE3,
              EQTL3,LAME4,DTEX3,SBSP3,MRVE3,BBDC3,HBOR3,SULA11,POSI3,
              ECOR3,GFSA3,LCAM3,QUAL3,PRIO3,YDUQ3,CPFE3,CNTO3,BTOW3,
              LOGN3,EVEN3,ALPA4,BRSR6,EZTC3,CVCB3,SANB11,ENBR3,ENAT3,
              MEAL3,LIGT3,HAPV3,VIVT4,ELET6,ALSO3,HYPE3,BRPR3,BRAP4)
retornos <- join_all(lista,"data")
retornos[is.na(retornos)] <- 0
retornos <- retornos[,-c(1)]

######
#versao antiga
#retornos <- Reduce(function(x,y) merge(x=x, y=y, by="data"),
#                   list(PETR4,OIBR3,VVAR3,COGN3,BBDC4,ITUB4,VALE3,PETR3,GGBR4,
#                        ITSA4,ABEV3,JBSS3,RENT3,B3SA3,CIEL3,BBAS3,CSNA3,USIM5,
#                        GOAU4,EMBR3,MRFG3,GOLL4,BRFS3,CYRE3,BRML3,
#                        LREN3,KLBN11,HGTX3,CCRO3,RAIL3,MGLU3,POMO4,BRKM5,TIMP3,BEEF3,
#                        NTCO3,UGPA3,AMAR3,TCSA3,TOTS3,ELET3,JHSF3,BBSE3,RAPT4,
#                        MULT3,WEGE3,EQTL3,LAME4,DTEX3,SBSP3,MRVE3,BBDC3,
#                        HBOR3,SULA11,POSI3,ECOR3,GFSA3,LCAM3,QUAL3,PRIO3,YDUQ3,
#                        CPFE3,CNTO3,BTOW3,LOGN3,EVEN3,ALPA4,BRSR6,
#                        EZTC3,CVCB3,SANB11,ENBR3,ENAT3,MEAL3,LIGT3,HAPV3,VIVT4,
#                       ELET6,ALSO3,HYPE3,BRPR3,BRAP4))


#########
#Construção de matrizes

#Construindo a matriz de retorno medio
retorno_medio <- colMeans(retornos)

#Matriz de covariancia
matriz_cov <- cov(retornos)
#rownames(matriz_cov) <- ativos
#colnames(matriz_cov) <- ativos

###########
taxa_livre_risco <- 0.03*0.85/360
short_selling <- FALSE

###########
# compute portfolio frontier
fronteira_eficiente <- efficient.frontier(retorno_medio, matriz_cov, nport = 40, shorts = short_selling)
fronteira_eficiente
attributes(fronteira_eficiente)

# Calculo da carteira com a menor risco possivel
carteira_min_risco <- globalMin.portfolio(retorno_medio, matriz_cov, shorts = short_selling)
carteira_min_risco
retorno_min_anual <- (carteira_min_risco$er+1)^(264*3)
retorno_min_anual
carteira_min <- carteira_min_risco$weights
#carteira_min <- cbind(carteira_min,ativos)

#Vamos calcular a nossa carteira mais eficiente - chamado de Tangency Portfolio
# Carteira Eficience
carteira_eficiente <- tangency.portfolio(retorno_medio, matriz_cov, taxa_livre_risco, shorts = short_selling)
carteira_eficiente
retorno_ef_anual <- (carteira_eficiente$er+1)^(264*3)
retorno_ef_anual
carteira_ef <- carteira_eficiente$weights
#carteirae_ef <- cbind(carteira_ef,ativos)

###############
#dinheiro hipotético
disponibilidade <- 1000
risco_min <- retorno_min_anual*disponibilidade
risco_min
risco_ef <- retorno_ef_anual*disponibilidade
risco_ef

###############
# Visualizaçao gráfica da saida

plot(fronteira_eficiente, plot.assets=F, col="blue", pch=16)

points(carteira_min_risco$sd, carteira_min_risco$er, col="green", pch=10, cex=2)
points(carteira_eficiente$sd, carteira_eficiente$er, col="red", pch=10, cex=2)

text(carteira_min_risco$sd, carteira_min_risco$er, labels="Risco Minimo", pos=2)
text(carteira_eficiente$sd, carteira_eficiente$er, labels="Carteira Eficiente", pos=2)

tangente <- (carteira_eficiente$er - taxa_livre_risco)/carteira_eficiente$sd
abline(a = taxa_livre_risco, b=tangente, col="green", lwd=2)

#Visualização das carteiras
carteira_min[carteira_min==0] <- NA
carteira_min <- na.omit(carteira_min)

carteira_ef[carteira_ef==0] <- NA
carteira_ef <- na.omit(carteira_ef)

View(carteira_min)
View(carteira_ef)
