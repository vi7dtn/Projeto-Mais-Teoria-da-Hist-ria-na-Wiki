### Limpar o environment do RStudio

rm(list=ls())

### Carregar os pacotes (lembrar de instalar com install.packages("") a primeira vez que for rodar o código)

library(dplyr)
library(readxl)
library(writexl)

### Definir o caminho em que estão no seu PC os arquivos História Intelectual, Epistemologia e Conceitos Duplicados

caminho_intelectual <- ".xlsx" 
caminho_epistemologia <- ".xlsx"
caminho_conceitos_duplicados <- ".xlsx"

#### História intelectual

# Aqui excluiremos da tabela História Intelectual tudo aqui que foi posto na tabela Conceitos duplicados como Epistemologia

intelectual<- read_excel(caminho_intelectual)
conceitos_duplicados <- read_excel(caminho_conceitos_duplicados)

conceitos_duplicados$Conceito <- gsub("^", "[[", conceitos_duplicados$Conceito)
conceitos_duplicados$Conceito <- gsub("$", "]]", conceitos_duplicados$Conceito)

conceitos_excluir_intelectual <-  conceitos_duplicados %>% filter(Tabela == "Epistemologia")
names(conceitos_excluir_intelectual) <- c("Artigo em português",
                                          "Tabela")
intelectual_filtrada <- anti_join(intelectual, conceitos_excluir_intelectual, by = "Artigo em português")

# Exportar apenas para ter o arquivo bruto em seu computador

setwd("D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/conceitos/historia intelectual")
write_xlsx(intelectual_filtrada, "intelectual_final.xlsx")

#### Epistemologia

# Aqui excluiremos da tabela Epistemologia tudo aqui que foi posto na tabela Conceitos duplicados como História Intelectual

epistemologia <- read_excel(caminho_epistemologia)

conceitos_duplicados$Conceito <- gsub("^", "[[", conceitos_duplicados$Conceito)
conceitos_duplicados$Conceito <- gsub("$", "]]", conceitos_duplicados$Conceito)

conceitos_excluir_epistemologia <-  conceitos_duplicados %>% filter(Tabela == "História intelectual")

names(conceitos_excluir_epistemologia) <- c("Artigo em português",
                                            "Tabela")

epistemologia_filtrada <- anti_join(epistemologia, conceitos_excluir_epistemologia, by = "Artigo em português")

# Exportar apenas para ter o arquivo bruto em seu computador

setwd("D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/conceitos/epistemologia")
write_xlsx(epistemologia_filtrada, "epistemologia_final.xlsx")

### Tratamento dos dados (o código é o mesmo para ambas tabelas, apenas altere o arquivo)

# Tratamento da coluna "Artigo em inglês"

tratamento0 <- function(x) {
  x <- gsub("\\[\\[", "", x)
  x <- gsub("\\]\\]","", x)
  x <- gsub(" ", "+", x)
}

# Na linha abaixo, selecionar as linhas em que estão presentes os valores na coluna "Artigo em inglês"

intelectual_filtrada$`Artigo em inglês`[1:347] <- tratamento0(intelectual_filtrada$`Artigo em inglês`[1:347]) 

trans_en <- "style=\"text-align: center;\"|{{User:Stanglavine/en|" # Padrão do botão clicável otimizado pelo Rafa

tratamentoI <- function(x) {
  x <- paste0(trans_en, x)
  x <- gsub("$", "}}", x)
}

intelectual_filtrada$`Artigo em inglês`[1:347] <- ifelse(
  is.na(intelectual_filtrada$`Artigo em inglês`[1:347]),
  intelectual_filtrada$`Artigo em inglês`[1:347],
  tratamentoI(intelectual_filtrada$`Artigo em inglês`[1:347])
)

# Tratamento da coluna "Artigo em espanhol"

trans_es <- "style=\"text-align: center;\"|{{User:Stanglavine/es|" # Padrão do botão clicável otimizado pelo Rafa

# Na linha abaixo, selecionar as linhas em que estão presentes os valores na coluna "Artigo em espanhol" mas pegando desde os "Artigos em inglês" também

intelectual_filtrada$`Artigo em espanhol`[1:367] <- tratamento0(intelectual_filtrada$`Artigo em espanhol`[1:367])

tratamentoII <- function(x) {
  x <- paste0(trans_es, x)
  x <- gsub("$", "}}", x)
}

intelectual_filtrada$`Artigo em espanhol`[1:367] <- ifelse(
  is.na(intelectual_filtrada$`Artigo em espanhol`[1:367]),
  intelectual_filtrada$`Artigo em espanhol`[1:367],
  tratamentoII(intelectual_filtrada$`Artigo em espanhol`[1:367])
)

# Tratamento da coluna "Wikidata"

# Padrão do botão clicável

new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar!|class=mw-ui-progressive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

intelectual_filtrada$`Item no Wikidata` <- ifelse(
  is.na(intelectual_filtrada$`Item no Wikidata`),
  paste0(new_item, intelectual_filtrada$`Item no Wikidata`),
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", intelectual_filtrada$`Item no Wikidata`)
)

# Exportar
setwd("")
write_xlsx(intelectual_filtrada, ".xlsx")
