

rm(list = ls())
source("funcoes.r")


df <- read.csv('exp1_perc.csv', header = TRUE, sep = ',' , colClasses=c("NULL", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

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

# Conclusão: como p-value < 0.05, rejeitamos a hipótese de normalidade (para todos os grupos)



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

#Conclusão: Rejeitamos a hipótese de homocedasticidade para todos os grupos comparados.


# ------------------------------------------------------------------------------------------------------------------------------------------
# Test T pareado de amostras
# ------------------------------
# As séries comparadas devem ter distribuição próxima a normal - ! Não tem.
# As séries comparadas devem ter variância similar (homocedasticidade) - (apenas a comparação A_NFO e D_NFO não tem variância similar/homocedasticidade )

# H0: Significa que as médias antes e depois são iguais, p > 0,05
# HA: Significa que as médias antes e depois não são iguais, p<= 0,05
# ------------------------------------------------------------------------------------------------------------------------------------------

colname <- c()
pvalue <- c()

sufix_cols <-c('NFO','NFR', 'NME', 'NEU', 'PFO', 'PFR', 'PME');
for (c in sufix_cols){
  
  colA = paste0('A_',c)
  colD = paste0('D_',c)
  
  ttest = t.test(df[,colA], df[,colD], var.equal=TRUE)
  
  colname <- append(colname, paste(colA,colD, sep=" e "))
  pvalue <- append(pvalue,ttest$p.value)  
  print(ttest$p.value > 0.05)
  
}
result <- data.frame(colname, pvalue)
print (result)

#Conclusão: Como os dados testados não possuem distribuição normal e 
# não apressentam amocedasticidade, o teste T não é indicado. Portanto partimos
# para o teste não paramétrico de Mann-Whitney




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
  
  wilcox = wilcox.test(df[,colA], df[,colD])
  
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

#Conclusão: é possível afirmar, com pelo menos 95% de certeza, que existe diferença na intensidade dos sentimentos apenas entre os grupos "A_NFO e D_NFO" e "A_PFR e D_PFR" (p-value < 0.05).  



# ---------------------------------------------------------------------
# Vargha & Delaney's A12
# ------------------------------
# 
# - varia de 0 a 1 ( < 0.60 pequeno, <0.75 médio, > 0.75 grande)
# - O resultado do teste representa o número de vezes em que a série A foi maior do que a série B
# ------------------------------

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
#Conclusão: Aplicando um teste de Wilcoxon observamos diferenças significativas com tamanhos de efeito 0,55 e 0,41 entre o antes e depois dos percentuais nos grupos "Negativo Forte" e "Positivo Fraco" respectivamente.
# A proximidade de 0,5 no tamanha de efeito calculado indica que, apesar de haver uma diferença significativa entre os grupos, essa diferença é considereda pequena.



