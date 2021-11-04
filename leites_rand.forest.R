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
library(rpart)
library(randomForest)

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

#===============================================================================
# Aplicando o classificador Randown Forest
#===============================================================================
#Padronizar a escala da base de dados
dados_analise_C[,1:8] = scale(dados_analise_C[,1:8])


#dividindo conjunto em treino e teste
set.seed(1)
divisao = sample.split(dados_analise_C$ Quality, SplitRatio = 0.75)

View(divisao)

base_treinamento = subset(dados_analise_C, divisao == TRUE)
base_teste = subset(dados_analise_C, divisao == FALSE)

#analisando alguns gráficos

plot(base_treinamento,col = base_treinamento$Quality, pch = 19,  )

par(mfrow=c(1,1))
boxplot(base_treinamento)



#criando um classificador
classificador = rpart(formula = Quality ~ ., data = base_treinamento)
print(classificador)
#poda
#prune(classificador,0.02)
#install.packages("rpart.plot")
library(rpart.plot)
#mostrando árvore


set.seed(1)
classificador = randomForest(x = base_treinamento[-9], 
                             y = base_treinamento$Quality, ntree = 3)

rpart.plot(classificador)

previsoes = predict(classificador,base_teste[-9])
print(previsoes)

matriz_confusao = table(base_teste$Quality,previsoes)
print(matriz_confusao)
library(caret)
confusionMatrix(matriz_confusao)





