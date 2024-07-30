rm(list=ls())

library(dplyr)
library(readxl)

caminho <- (".xlsx")

df <- read_excel(caminho)

# Encontrar os nomes da coluna1 que não estão na coluna2
nomes_nao_presentes <- setdiff(df$Wikipedia, df$Wikidata)

# Criar um data frame com a terceira coluna contendo os nomes não presentes na coluna2
resultado <- data.frame(nomes_nao_presentes = nomes_nao_presentes)

write.csv2(resultado, "XXXdiff.csv")
