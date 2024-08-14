rm(list=ls())

library(dplyr)
library(readxl)
library(writexl)

caminho <- "D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/impressos/periodicos/periodicos_a_criar2.xlsx"

periodicos <- read_excel(caminho)

# Criar coluna "Item"

periodicos$`Item no Wikidata` <- "style=\"text-align: center;\"|{{Botão clicável 2|class=mw-ui-progressive|Criar|url=https://www.wikidata.org/wiki/Special:NewItem}}"

# Formatar coluna "URL"

URL_transform <- function(x) {
  x <- gsub("^", "style=\"text-align: center;\"|{{Botão clicável 2|class=mw-ui-destructive|PDF|url=", x)
  x <- gsub("$", "}}", x)
}

periodicos$URL <- URL_transform(periodicos$URL)

periodicos <- select(periodicos, Título, Autoria, `Item no Wikidata`, `Instância de`, Periódico, Volume, Número, URL)

names(periodicos) <- c("Título", 
                   "Autoria",
                   "Item no Wikidata", 
                   "Instância de",
                   "Periódico",
                   "Volume",
                   "Número", 
                   "Carregar no Commons")

periodicos$Número <- as.integer(periodicos$Número)

# Centralizar texto na coluna

center <- "style=\"text-align: center;\"|"

periodicos$`Instância de` <- gsub("^", center, periodicos$`Instância de`)
periodicos$Periódico <- gsub("^", center, periodicos$Periódico)
periodicos$Volume <- gsub("^", center, periodicos$Volume)
periodicos$Número <- gsub("^", center, periodicos$Número)

# Exportar
setwd("D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/impressos/periodicos")
write_xlsx(periodicos, "periodicosfinal.xlsx")