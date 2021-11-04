#===============================================================================
#Pacotes
#===============================================================================

library(readr)      #Ler arquivo
library(dplyr)      #Tratar dados
library(ggplot2)    #Plotar gráficos
library(tidyverse)  #Manipulação de dataframe
library(corrplot)   #Plotar gráficos
library(arules)     #Agrupar pacotes
library(caTools)
library(class)
library(caret)
library(GGally)
library(e1071)

#===============================================================================
# Carregar Dataset
#===============================================================================

#Dados como datafame
dados_analise_C = (read_delim("C:/Users/samar/Dropbox/Samara/Mestrado/leites/milk.csv", 
                              delim = ";", escape_double = FALSE, na = "NA", 
                             trim_ws = FALSE))
#View(dados_analise_C)

#===============================================================================
# Classificando quando ao cumprimento dos padrões
#===============================================================================

dados_analise_C <- dados_analise_C %>%
  mutate(Quality = case_when(
    Grade == 1.0 ~ "1",
    Grade == 0.5 ~ "2",
    Grade == 0.0 ~ "3",
    TRUE ~ NA_character_))

dados_analise_C$Quality =  as.numeric(dados_analise_C$Quality)

print(dados_analise_C)

#Encoding da classe meta para factor
dados_analise_C$Quality = factor(dados_analise_C$Quality)

#Quality = as.factor (dados_analise_C $ Quality)
#dados_analise_C $ Quality = Quality


#print(Quality)
#print(dados_analise_C)

#===============================================================================
# Aplicando o classificador SMV
#===============================================================================

#Padronizar a escala da base de dados
dados_analise_C[,1:8] = scale(dados_analise_C[,1:8])


#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados_analise_C$ Quality, SplitRatio = 0.75)

View(divisao)

base_treinamento = subset(dados_analise_C, divisao == TRUE)
base_teste = subset(dados_analise_C, divisao == FALSE)


classificador = svm( formula = Quality ~ .,
                    data = base_treinamento,
                    type = "C-classification",
                    kernel = "linear",
                    cost = 1)

help(svm)


print(classificador)

#classificador
previsao = predict(classificador, newdata = base_teste[,-9, drop = FALSE])
print(previsao)

#previsao
matriz_confusao = table(base_teste$Quality, previsao)
print(matriz_confusao)

#matriz_confusao
confusionMatrix(matriz_confusao)
plot(previsao)

