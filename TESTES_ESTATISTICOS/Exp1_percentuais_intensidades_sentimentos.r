rm(list = ls())


#Session work directory: C:\Users\junior\Google Drive\_Mestrado\SEAN\Dissertacao_Celso\resources\datasets
df <- read.csv('exp1_perc.csv', header = TRUE, sep = ',' , colClasses=c("NULL", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

View(df)

# Veiricando se o somentório de cada linha é igual a 2. (1 para a soma dos percentuais "Antes" e 1 para "Depois")
min(rowSums(df))
max(rowSums(df))



# ------------------------------------------------------------------------------------------------------------------------------------------
# Shapiro - Teste de normalidade
# ------------------------------
# O teste de Shapiro-Wilk testa a hipótese nula de que os dados foram extraídos de uma distribuição normal.
# H0: distribuição dos dados = normal, p > 0,05
# HA: distribuição dos dados <> normal, p <= 0,05
# ------------------------------------------------------------------------------------------------------------------------------------------

colname <- c()
pvalue <- c()

for(i in 1:ncol(df)) {
  shap = shapiro.test(df[ , i])
  colname <- append(colname,colnames(df)[i])
  pvalue <- append(pvalue,shap$p.value)
}

result <- data.frame(colname, pvalue)
print (result)

# Conclusão: como p-value < 0.05, rejeitamos a hipótese de normalidade (para todos os grupos)



# ------------------------------------------------------------------------------------------------------------------------------------------
# Levene - Teste de Homocedasticidade
# ------------------------------
# O teste de Levene(pacote lawstatdo R) recebe dois grupos de dados com o mesmo número de informações e verifica se eles têm a mesma variância
# H0: Aceitamos a hipótese de homocedasticidade para, p > 0,05
# HA: pelo menos 1 grupo apreseta variância diferente dos demais grupos, p <= 0,05
# ------------------------------------------------------------------------------------------------------------------------------------------

if(!require(lawstat)) install.packages("lawstat")
library(lawstat)

colname <- c()
pvalue <- c()

sufix_cols <-c('NFO','NFR', 'NME', 'NEU', 'PFO', 'PFR', 'PME');
for (c in sufix_cols){
  
  colA = paste0('A_',c)
  colD = paste0('D_',c)

  levne = levene.test(df[,colA], df[,colD])

  colname <- append(colname, paste(colA,colD, sep=" e "))
  pvalue <- append(pvalue,levne$p.value)  
  print(levne$p.value > 0.05)
}
result <- data.frame(colname, pvalue)
print (result)


#Conclusão: Aceitamos a hipótese de homocedasticidade (variância similar  p > 0,05) para todos os grupos comparados.


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

#Conclusão: não é possível afirmar com 95% de certeza que existe diferença na insidade dos sentimentos entre os grupos antes e depois (p > 0,05).




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

sufix_cols <-c('NFO','NFR', 'NME', 'NEU', 'PFO', 'PFR', 'PME');
for (c in sufix_cols){
  
  colA = paste0('A_',c)
  colD = paste0('D_',c)
  
  wilcox = wilcox.test(df[,colA], df[,colD])
  
  colname <- append(colname, paste(colA,colD, sep=" e "))
  pvalue <- append(pvalue,wilcox$p.value)  
  mediaA <- append(mediaA,mean(df[,colA]))
  mediaD <- append(mediaD,mean(df[,colD]))
  desvA <- append(desvA,sd(df[,colA]))
  desvD <- append(desvD,sd(df[,colD]))
  
  
  print(wilcox$p.value > 0.05)
  
}
result <- data.frame(colname, pvalue, mediaA, desvA, mediaD, desvD)
print (result)

#Conclusão: é possível afirmar, com pelo menos 95% de certeza, que existe diferença na intensidade dos sentimentos apenas entre os grupos "A_NFO e D_NFO" (p-value < 0.05).  

