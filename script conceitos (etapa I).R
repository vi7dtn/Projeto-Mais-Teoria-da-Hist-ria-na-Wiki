### Limpar o environment do RStudio

rm(list=ls())

### Carregar os pacotes (lembrar de instalar com install.packages("") a primeira vez que for rodar o código)

library(dplyr)
library(readxl)
library(writexl)

### Definir o caminho em que os arquivos gerados pelo PetScan estão em seu PC

# Lembrar de sempre utilizar a barra invertida '/' no caminho do arquivo do seu PC

# Carregar primeiro os dados relativos à "História intelectual" (depois altere os caminhos e rode para "Epistemologia")

caminho_pt_intelectual <- "csv"
caminho_en__intelectual <- ".csv"
caminho_es__intelectual <- ".csv"

### Carregar os arquivos

intelectual_pt <- read.csv(caminho_pt_intelectual)
intelectual_en <- read.csv(caminho_en_intelectual)
intelectual_es <- read.csv(caminho_es_intelectual)

### Tratamento dos dados (etapa I)

# Fazer a junção das tabelas com base na coluna 'item'

intelectual <- full_join(intelectual_en, intelectual_es, by = "item", suffix = c("_en", "_es")) %>%
  full_join(intelectual_pt, by = "item", suffix = c("", "_pt"))

# Fazer a junção das tabelas com base na coluna 'item'

intelectual <- select(intelectual, article, article_en, article_es, item)

# Copiar os valores das colunas 'title_en' e 'title' para a coluna 'title_pt' de acordo com as linhas

intelectual$article[1:347] <- intelectual$article_en[1:347] # Determinar no colchetes onde começa e onde termina as linhas dos artigos em inglês
intelectual$article[348:367] <- intelectual$article_es[348:367] # Determinar no colchetes onde começa e onde termina as linhas dos artigos em espanhol

# Renomear colunas

names(epistemologia) <- c("Artigo em português",
                          "Artigo em inglês",
                          "Artigo em espanhol",
                          "Item no Wikidata")

### Exportar

setwd("")
write_xlsx(intelectual, ".xlsx")

# Carregar os dados relativos à "Epistemologia"

caminho_pt_epistemologia <- "csv"
caminho_en__epistemologia <- ".csv"
caminho_es__epistemologia <- ".csv"

### Carregar os arquivos

epistemologia_pt <- read.csv(caminho_pt_epistemologia)
epistemologia_en <- read.csv(caminho_en_epistemologia)
epistemologia_es <- read.csv(caminho_es_epistemologia)

### Tratamento dos dados (etapa I)

# Fazer a junção das tabelas com base na coluna 'item'

epistemologia <- full_join(epistemologia_en, epistemologia_es, by = "item", suffix = c("_en", "_es")) %>%
  full_join(epistemologia_pt, by = "item", suffix = c("", "_pt"))

# Fazer a junção das tabelas com base na coluna 'item'

epistemologia <- select(epistemologia, article, article_en, article_es, item)

# Copiar os valores das colunas 'title_en' e 'title' para a coluna 'title_pt' de acordo com as linhas

epistemologia$article[1:347] <- epistemologia$article_en[1:347] # Determinar no colchetes onde começa e onde termina as linhas dos artigos em inglês
epistemologia$article[348:367] <- epistemologia$article_es[348:367] # Determinar no colchetes onde começa e onde termina as linhas dos artigos em espanhol

# Renomear colunas

names(epistemologia) <- c("Artigo em português",
                          "Artigo em inglês",
                          "Artigo em espanhol",
                          "Item no Wikidata")

### Exportar

setwd("")
write_xlsx(epistemologia, ".xlsx")

### Comando para identificar conceitos duplicados

conceitos_duplicados <- inner_join(intelectual, epistemologia, by = "Item no Wikidata")

### Exportar

setwd("")
write_xlsx(conceitos_duplicados, ".xlsx")

