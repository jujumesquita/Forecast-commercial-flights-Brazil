#Limpando a mem?ria
rm(list=ls())



#install.packages("seas")
library(readxl)
library(devtools)
library(RGraphics)
library(grDevices)
library(tseries)
library(TSA)
library(forecast)
library(dplyr)
library(seas)

#Definindo o caminho onde está minha base de dados
setwd("C:/Users/jujum/Desktop")
getwd()

#Lendo o dataset
my_data <- read_excel("VoosComerciais_2019_2020.xlsx")

#Gerando a serie temporal
voos <- my_data[2]
voos_st <- ts(voos, start = c(2019), frequency = 365)

#Plotando a serie
ggtsdisplay(voos_st)
plot.ts(voos_st, ylab="Voos", xlab="Ano/Mes", plot.type = c("single"), las = 1, col = c("gray"), lwd = 2, main= "Voos 2019 - 2020")
plot.ts(diff(voos_st)) #Primeira diferença da serie
log(voos_st) #Logaritmo da serie 

#Conferindo se a serie é estacionaria
kpss.test(voos_st, null = "Level", lshort=TRUE) #Resposta: Serie nao estacionaria
## calculo do k ##
kmax <- trunc(7*(nrow(voos_st)/100)**(1/4))
adf.test(voos_st, k=kmax) 
#Resposta: Serie nao estacionaria

#Plotando grafico ACF (para encontrar o valor de q minusculo):
acf(voos_st,60)

#Plotando grafico PACF (para encontrar o valor de p minusculo):
pacf(voos_st,60) 

#ESTIMAÇÃO DE UM MODELO SARIMA: (p, d, q) (P, D, Q)

#Primeira diferenca

## ACRESCENTAR PERIOD=7 POR QUE SEUS DADOS SAO DIÁRIOS E A SAZONALIDADE SEMANAL
arima_model_dif <- Arima(voos_st, order = c(0,1,0), seasonal = list(order=c(0,0,0), period=7))
arima_model_dif #AIC=14645.57   AICc=14645.58   BIC=14650.16  
acf(arima_model_dif$residual, lag.max=60)
pacf(arima_model_dif$residual, lag.max=60)

#Sazonalidade
arima_model_saz <- Arima(voos_st, order = c(0,0,0), seasonal = list(order=c(0,1,0), period=7))
arima_model_saz #AIC=14490.45   AICc=14490.46   BIC=14495.03
acf(arima_model_saz$residual, lag.max=60)
pacf(arima_model_saz$residual, lag.max=60)

#Primeira diferença + Sazonalidade
arima_model_dif_saz <- Arima(voos_st, order = c(0,1,0), seasonal = list(order=c(0,1,0), period=7))
arima_model_dif_saz #AIC=13663.96   AICc=13663.97   BIC=13668.54
acf(arima_model_dif_saz$residual, lag.max=60)
pacf(arima_model_dif_saz$residual, lag.max=60)

#Testando se a serie ficou estacionária depois do primeiro modelo (primeira diferenca + sazonalidade)
kpss.test(arima_model_dif_saz$residuals, null = "Level", lshort=TRUE) #Resposta: Estacionaria 
adf.test(arima_model_dif_saz$residual, k=kmax) #Resposta: Estacionaria

print(summary(arima_model_dif_saz))

#Analisar os gráficos ACF e PACF para encontrar os lags significativos 
#> ACF : encontramos q
#> PACF : encontramos p

#Temos que testar cada um dos lags significativos nos dois gráficos ACF e PACF e ver qual é o 
#melhor resultado, representado pelos menores valores de AIC e BIC,

#SARIMA: (p, d, q) (P, D, Q)

#PACF: p: 1,2,3,4,5,10
#ACF: q: 2, 7,9

#PACF: P: 1,2,3
#ACF: Q: 1,2


c_arima_model_dif_saz1 <- Arima(voos_st, order = c(5,1,2), seasonal = list(order=c(0,1,3), period=7))
c_arima_model_dif_saz1 
#AIC=13471.02   AICc=13471.39   BIC=13521.42

c_arima_model_dif_saz2 <- Arima(voos_st, order = c(5,1,2), seasonal = list(order=c(0,1,4), period=7))
c_arima_model_dif_saz2 
#AIC=13472.56   AICc=13473   BIC=13527.55

c_arima_model_dif_saz3 <- Arima(voos_st, order = c(5,1,2), seasonal = list(order=c(2,1,3), period=7))
c_arima_model_dif_saz3 
acf(c_arima_model_dif_saz$residual, lag.max=60)
pacf(c_arima_model_dif_saz$residual, lag.max=60)
#AIC=13473.92   AICc=13474.44   BIC=13533.49

j_arima_model_dif_saz_1 <- Arima(voos_st, order = c(2,1,2), seasonal = list(order=c(1,1,1), period=7))
j_arima_model_dif_saz_1
#AIC=13477.85   AICc=13478   BIC=13509.92

j_arima_model_dif_saz_4 <- Arima(voos_st, order = c(2,1,2), seasonal = list(order=c(1,1,2), period=7))
j_arima_model_dif_saz_4
#AIC=13479.03   AICc=13479.24   BIC=13515.69

j_arima_model_dif_saz_2 <- Arima(voos_st, order = c(2,1,7), seasonal = list(order=c(2,1,1), period=7))
j_arima_model_dif_saz_2
#AIC=13479.03   AICc=13479.24   BIC=13515.69

j_arima_model_dif_saz_3 <- Arima(voos_st, order = c(7,1,2), seasonal = list(order=c(0,1,0), period=7))
j_arima_model_dif_saz_3
#AIC=13569.98   AICc=13570.29   BIC=13615.8


j_arima_model_dif_saz_5 <- Arima(voos_st, order = c(7,1,2), seasonal = list(order=c(3,1,2), period=7))
j_arima_model_dif_saz_5
#AIC=13477.77   AICc=13478.45   BIC=13546.5


#TESTANDO AUTOARIMA
autoarima_model <- auto.arima(voos_st, seasonal = TRUE)
print(summary(autoarima_model)) #ARIMA(5,1,2)
#AIC=14058.73   AICc=14058.93   BIC=14095.46

#RESUMO DOS RESULTADOS
arima_model_dif_saz #ARIMA(0,1,0)(0,1,0) : AIC=13663.96   AICc=13663.97   BIC=13668.54
j_arima_model_dif_saz_1 #ARIMA(2,1,2)(1,1,1) : AIC=13477.85   AICc=13478   BIC=13509.92
j_arima_model_dif_saz_2 #ARIMA(2,1,7)(2,1,1) : AIC=13482.74   AICc=13483.25   BIC=13542.3
j_arima_model_dif_saz_3 #ARIMA(7,1,2)(0,1,0) : AIC=13569.98   AICc=13570.29   BIC=13615.8
j_arima_model_dif_saz_4 #ARIMA(2,1,2)(1,1,2) : AIC=13479.03   AICc=13479.24   BIC=13515.69
j_arima_model_dif_saz_5 #ARIMA(7,1,2)(3,1,2) : AIC=13477.77   AICc=13478.45   BIC=13546.5
c_arima_model_dif_saz1 #ARIMA(5,1,2)(0,1,3) : AIC=13471.02   AICc=13471.39   BIC=13521.42
c_arima_model_dif_saz2 #ARIMA(5,1,2)(0,1,4) : AIC=13472.56   AICc=13473   BIC=13527.55
c_arima_model_dif_saz3 #ARIMA(5,1,2)(2,1,3) : AIC=13473.92   AICc=13474.44   BIC=13533.49
autoarima_model #ARIMA(5,1,2) : AIC=14058.73   AICc=14058.93   BIC=14095.46


#De todos os modelos, o melhor foi o modelo c_arima_model_dif_saz1

#VERIFICAÇÃO DO MODELO ESCOLHIDO
acf(c_arima_model_dif_saz1$residual, lag.max=60)
pacf(c_arima_model_dif_saz1$residual, lag.max=60)


#FAZENDO A PREVISÃO

#Lendo o dataset previsão
my_data_2021 <- read_excel("Voos_comerciais_2021.xlsx")

#Gerando a serie temporal previsão
voos2021 <- my_data_2021[2]
voos_st2021 <- ts(voos2021, start = c(2021,1), frequency = 365)

#Plotando a serie
ggtsdisplay(voos_st2021)
plot.ts(voos_st2021, ylab="Voos", xlab="Ano/Mes", plot.type = c("single"), las = 1, col = c("gray"), lwd = 2, main= "Voos 2021")



fc <- forecast(voos_st, h=60, model=c_arima_model_dif_saz1)
prev <-ts.union(voos_st2021, fc$mean)
plot.ts(prev, col=c("blue","red"), plot.type="single")

Erro_Treino <- sqrt(mean(fc$residuals**2))
Erro_Teste <- sqrt(mean((fc$mean - voos_st2021)**2)) 

Erro_Treino_Perc <- mean(abs(fc$residual/fc$x))
Erro_Teste_Perc <- mean(abs((fc$mean - voos_st2021)/voos_st2021))

Erro_Treino
Erro_Teste
Erro_Treino_Perc
Erro_Teste_Perc








