#https://rpubs.com/MauryAntunes/508076

rm(list = ls())
source("funcoes.r") 

#Session work directory: C:\Users\junior\Google Drive\_Mestrado\SEAN\Dissertacao_Celso\resources\datasets\exp1_disp.csv


#df <- read.csv('exp1_sent_vader_subcateg_perc.csv', header = TRUE, sep = ',' , colClasses=c("NULL", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
#df <- read.csv('exp2_sent_vader_subcateg_perc.csv', header = TRUE, sep = ',' , colClasses=c("NULL", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
#df <- read.csv('exp2_sent_textblob_subcateg_perc.csv', header = TRUE, sep = ',' , colClasses=c("NULL", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
#df <- read.csv('exp2_sent_poli_subcateg_perc.csv', header = TRUE, sep = ',' , colClasses=c("NULL", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
df <- read.csv('exp2_sent_poli_subcateg_pt_perc.csv', header = TRUE, sep = ',' , colClasses=c("NULL", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))



View(df)

#Verificando se o somatório de cada linha de ambos os grupos(Antes e Depois) é igual a 1.

#Primeiras 7 colunas (Negativos)
min(rowSums(df[,(1:7)]))
max(rowSums(df[,(1:7)]))

#Útima 7 colunas (Negativos)
min(rowSums(df[,(8:14)]))
max(rowSums(df[,(8:14)]))



# ------------------------------------------------------------------------------------------------------------------------------------------
# Shapiro - Teste de normalidade
# ------------------------------
# O teste de Shapiro-Wilk testa a hipótese nula de que os dados foram extraídos de uma distribuição normal.
# H0: distribuição dos dados = normal, p > 0,05
# HA: distribuição dos dados <> normal, p <= 0,05
# ------------------------------------------------------------------------------------------------------------------------------------------

colname <- c()
pvalue <- c()
isNomal_H0 <- c()

for(i in 1:ncol(df)) {
  colname <- append(colname,colnames(df)[i])
  shap = shapiro.test(df[ , i])
  pvalue <- append(pvalue,shap$p.value)
  isNomal_H0 <- append(isNomal_H0, shap$p.value > 0.05)
}

result <- data.frame(colname, pvalue, isNomal_H0)
print (result)




# ------------------------------------------------------------------------------------------------------------------------------------------
# Levene - Teste de Homocedasticidade
# ------------------------------
# O teste de Levene(pacote lawstatdo R) recebe dois grupos de dados com o mesmo número de informações e verifica se eles têm a mesma variância
# H0: Aceitamos a hipótese de homocedasticidade para, p > 0,05
# HA: pelo menos 1 grupo apreseta variância diferente dos demais grupos, p <= 0,05
# ref: https://www.rdocumentation.org/packages/lawstat/versions/3.2/topics/levene.test
# ------------------------------------------------------------------------------------------------------------------------------------------


if(!require(lawstat)) install.packages("lawstat")
library(lawstat)

colname <- c()
pvalue <- c()
isHomocedasticos_H0 <- c()

sufix_cols <-c('NFO','NFR', 'NME', 'NEU', 'PFO', 'PFR', 'PME');
for (c in sufix_cols){
  
  colA = paste0('A_',c)
  colD = paste0('D_',c)
  
  colname <- append(colname, paste(colA,colD, sep=" e "))
  levne = levene.test(df[,colA], df[,colD], location = "mean")
  pvalue <- append(pvalue,levne$p.value)  
  isHomocedasticos_H0 <- append(isHomocedasticos_H0, levne$p.value > 0.05)
  
}
result <- data.frame(colname, pvalue, isHomocedasticos_H0)
print (result)


# ##############################
# Sobre o porquê não utilizamos o teste-t: Como os dados testados não possuem distribuição normal e 
# não apressentam amocedasticidade, o teste T não é indicado. Portanto partimos
# para o teste não paramétrico de Mann-Whitney
# ##############################



# ---------------------------------------------------------------------
# Teste de Mann-Whitney (Wilcoxon)
# ------------------------------
# 
# H0: Significa que as médias antes e depois são iguais, p > 0,05
# HA: Significa que as médias antes e depois não são iguais, p<= 0,05


colname <- c()
pvalue <- c()
mediaA <- c()
mediaD <- c()
desvA <- c()
desvD <- c()
isEquals_H0 <- c()

sufix_cols <-c('NFO','NFR', 'NME', 'NEU', 'PFO', 'PFR', 'PME');
for (c in sufix_cols){
  
  colA = paste0('A_',c)
  colD = paste0('D_',c)
  
  #** Não havia utilizado o parametro "paired = TRUE"
  wilcox = wilcox.test(df[,colA], df[,colD], paired = TRUE)
  
  colname <- append(colname, paste(colA,colD, sep=" e "))
  pvalue <- append(pvalue,wilcox$p.value)  
  mediaA <- append(mediaA, round(mean(df[,colA]), digits = 2))
  mediaD <- append(mediaD, round(mean(df[,colD]), digits = 2))
  desvA <- append(desvA,   round(sd(df[,colA]), digits = 2))
  desvD <- append(desvD,   round(sd(df[,colD]), digits = 2))
  isEquals_H0 <- append(isEquals_H0, wilcox$p.value > 0.05)
}
result <- data.frame(colname, pvalue, mediaA, desvA, mediaD, desvD, isEquals_H0)
print (result)




# ---------------------------------------------------------------------
# Vargha & Delaney's A12
# ------------------------------
# 
# - varia de 0 a 1 ( < 0.60 pequeno, <0.75 médio, > 0.75 grande)
# - O resultado do teste representa o número de vezes em que a série A foi maior do que a série B
# ------------------------------

#** Para o teste Vargha & Delaney's, não é preciso informar se é pareado? 

vargha.delaney <- function(r1, r2) {
  m <- length(r1);
  n <- length(r2);
  return ((sum(rank(c(r1, r2))[seq_along(r1)]) / m - (m + 1) / 2) / n);
}


colname <- c()
efeito <- c()

sufix_cols <-c('NFO','NFR', 'NME', 'NEU', 'PFO', 'PFR', 'PME');
for (c in sufix_cols){
  
  colA = paste0('A_',c)
  colD = paste0('D_',c)
  
  varg = vargha.delaney(df[,colA], df[,colD])
  
  colname <- append(colname, paste(colA,colD, sep=" e "))
  efeito <- append(efeito, varg)
  
}

result <- data.frame(colname, efeito )
print (result)

