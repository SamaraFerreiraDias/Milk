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
library(class)

#===============================================================================
# Carregar Dataset
#===============================================================================

#Dados como datafame

dados_analise_C <- read_delim("milk.csv", delim = ";", 
                   escape_double = FALSE, na = "NA", trim_ws = TRUE)

#View(dados_analise_C)

#===============================================================================
# Histograma para cada tributo
#===============================================================================

#Histograma para características
dados_analise_C %>% gather(Attributes, value, 1:8) %>% ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) + facet_wrap(~Attributes, scales="free_x") +
  labs(x="Nivel de Qualidade", y="Frequencia",title="Atributos Qualidade dos Leites - Histogramas") + theme_bw()

#print(dados_analise_C)

#===============================================================================
# Matriz de Correlacao 
#===============================================================================

corrplot(cor(dados_analise_C), type="upper", method="square", tl.cex=0.9) 

#===============================================================================
# Classificando entre bom, moderado, ou ruim
#===============================================================================

dados_analise_C <- dados_analise_C %>%
  mutate(Quality = case_when(
    Grade == 1.0 ~ "Cumpre os padrões",
    Grade == 0.5 ~ "Cumpre parcialmente os padrões",
    Grade == 0.0 ~ "Descumpre os padrões",
    TRUE ~ NA_character_))

#agrupamento 
ggpairs(cbind(dados_analise_C, Quality=as.factor(dados_analise_C)),
        columns= 1:9, aes(colour = Quality, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()


#===============================================================================
# Implementacao do K-NN
#===============================================================================

#Padronizar a escala da base de dados
dados_analise_C[,1:8] = scale(dados_analise_C[,1:8])

#Dividir minha base de dados, treinamento e teste
set.seed(1)
divisao = sample.split(dados_analise_C$ Quality, SplitRatio = 0.75)
base_treinamento = subset(dados_analise_C, divisao == TRUE)
base_teste = subset(dados_analise_C, divisao == FALSE)

cl = base_treinamento$Quality

#K-NN
previsao = knn(base_treinamento[,-9, drop = FALSE], base_teste[,-9, drop = FALSE], cl , k=2)
help(knn)
print(previsao)
plot(previsao)

matriz_confusao = table(base_teste$Quality, previsao)
print(matriz_confusao)

confusionMatrix(matriz_confusao)
