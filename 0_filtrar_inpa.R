#filtrar o arquivo do inpa para usar o output no script - não rodar mais
library(readxl)
tabela_inpa_splink <- read_excel(path = "planilha_geral_splink_inpa.xlsx", sheet = 1)
head(tabela_inpa_splink)

library(dplyr)
names(tabela_inpa_splink)

inpa_filtered <- tabela_inpa_splink %>% filter(tabela_inpa_splink$scientificname %in% especies)
write.csv(inpa_filtered, "./data/inpa_filtrado.csv")
