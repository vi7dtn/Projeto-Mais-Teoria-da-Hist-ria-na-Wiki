### Limpar o environment do RStudio

rm(list=ls())

### Carregar os pacotes (lembrar de instalar com install.packages("") a primeira vez que for rodar o código)

library(dplyr)
library(readxl)

### Definir o caminho em que os arquivos gerados pelo Wikidata estão em seu PC

# Lembrar de sempre utilizar a barra invertida '/' no caminho do arquivo do seu PC"

caminho <- (".xlsx") # Arquivo que contém a junção da lista gerada pelo PetScan e WQS (uma coluna para cada)

### Carregar os arquivos

df <- read_excel(caminho)

### Encontrar os nomes da coluna1 que não estão na coluna2

nomes_nao_presentes <- setdiff(df$Wikipedia, df$Wikidata)

### Criar um data frame com a terceira coluna contendo os nomes não presentes na coluna2

resultado <- data.frame(nomes_nao_presentes = nomes_nao_presentes)

### Exportar

write.csv2(resultado, "XXXdiff.csv")
