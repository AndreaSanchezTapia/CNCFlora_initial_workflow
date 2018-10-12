# 11/out/18 revisando este script para inserir centroides em registro sem localidade e ordenar planilha por estado
# só as espçecies da lista que patrícia enviou
# carrega pacotes
library(dplyr)
library("stringr")
library("rgbif")
library(readxl)
library(textclean)
library(flora)
source("new_duplicated.R")
devtools::install_github("diogosbr/spfilt")
library(spfilt)
# le tudo
arvoresLC <- read_excel("./data/arvores_endemicas_possiveis_nao_ameacadas.xlsx", sheet = 1)
#tira autores
nomesLC <- lapply(arvoresLC$Species,FUN = remove.authors) %>% simplify2array()
#tabela <- read.delim("./data/especies.csv", sep = ";", header = F)
#familias <- as.character(tabela$V1)
#especies_complete <- as.character(tabela$V2)
#esta todo mundo na lista?
#setdiff(nomesLC, especies_complete)
#nomesLC %in% especies_complete
#sim
#especies <- word(especies_complete, start = 1, end = 2)

tabela_sistema <- read.csv(file = "./data/occurrences.csv", sep = ";", header = TRUE)s

#primeiro ler a planilha com a lista de coordenadas por centroide
tabela_centroides <- read.delim(file = "./data/centroide_municipio.csv",
                                header = TRUE, sep = ";",
                                stringsAsFactors = FALSE,
                                fileEncoding = "ISO-8859-9")

tabela_centroides_ucs <- read.delim(file = "./data/centroide_uc.csv",
                                    header = TRUE, sep = ";",
                                    stringsAsFactors = FALSE,
                                    fileEncoding = "ISO-8859-9")
#shape estados
library(rgdal)
estados <- rgdal::readOGR(dsn = "./data/BRA_adm_shp", layer = "BRA_adm1")
plot(estados)

centroides_estados <- rgeos::gCentroid(estados, byid = T, id = estados$NAME_1) %>% data.frame %>%
    tibble::rownames_to_column(var = "estados_shp") %>% mutate(stateProvince = replace_non_ascii(tolower(estados_shp))) %>%
    rename(x_estado = x, y_estado = y)
centroides_estados
# tira todos os caracteres e bota minuscula
tabela_centroides <- tabela_centroides %>%
    mutate(municipality = replace_non_ascii(tolower(NOME)),
           stateProvince = replace_non_ascii(tolower(NOMEUF)))
setdiff(centroides_estados$stateProvince, tabela_centroides$municipality)
tabela_centroides[which(tabela_centroides$municipality %in%  centroides_estados$stateProvince),]
dupl_names_state_city <- tabela_centroides$municipality[which(tabela_centroides$municipality %in%  centroides_estados$stateProvince)]
non_dupl_names <- setdiff(centroides_estados$stateProvince, dupl_names_state_city)
tabela_centroides_ucs <- tabela_centroides_ucs %>%
    mutate(uc = replace_non_ascii(tolower(NOME_UC1)))

especies <- nomesLC
familias <- arvoresLC$Family
for (i in 1:length(especies)) {
    print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))

    nome_clean <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                         "clean.csv")
    nome_centroides <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                         "centroides.csv")
    tabela_especie <- read.csv(nome_clean, row.names = 1, stringsAsFactors = F) %>%
        mutate(catalogNumber = factor(catalogNumber))

    #igual com o municipio
    tabela_especie <- tabela_especie %>%
        mutate(municipality = replace_non_ascii(tolower(municipality)),
               stateProvince = replace_non_ascii(tolower(stateProvince)),
               country = replace_non_ascii(tolower(country))) %>%
        mutate(stateProvince = ifelse(stateProvince == "espa-rito santo",
                                      "espirito santo",
                                      stateProvince)) %>%
        mutate(municipality = ifelse(municipality %in% non_dupl_names, NA, municipality))
    tabela_especie <- tabela_especie %>% left_join(tabela_centroides) %>% left_join(centroides_estados)

    tabela_corrigida <- tabela_especie %>%
        mutate(new_Lat1 = case_when(
            is.na(decimalLatitude) & is.na(municipality) &!is.na(stateProvince) ~ y_estado,
            is.na(decimalLatitude) & !is.na(municipality) ~ POINT_Y,
            is.numeric(decimalLatitude) ~ decimalLatitude
            )) %>%
                mutate(new_Lon1 = case_when(
                    is.na(decimalLongitude) & is.na(municipality) &!is.na(stateProvince) ~ x_estado,
                    is.na(decimalLongitude) & !is.na(municipality) ~ POINT_X,
                    is.numeric(decimalLongitude) ~ decimalLongitude
                )
                                                  )
    #corrigindo os 0, 0
    tabela_corrigida <- tabela_corrigida %>%
        mutate(new_Lat = case_when(
            new_Lat1 == 0 & new_Lon1 == 0 & is.na(municipality) & !is.na(stateProvince) ~ y_estado,
            new_Lat1 == 0 & new_Lon1 == 0 & !is.na(municipality)  ~ POINT_Y,
            new_Lat1 != 0 & new_Lon1 != 0 ~ new_Lat1)
        ) %>%
        mutate(new_Lon = case_when(
            new_Lat1 == 0 & new_Lon1 == 0 & is.na(municipality) & !is.na(stateProvince) ~ x_estado,
            new_Lat1 == 0 & new_Lon1 == 0 & !is.na(municipality)  ~ POINT_X,
            new_Lat1 != 0 & new_Lon1 != 0 ~ new_Lon1)
        ) %>%
        select(-new_Lat1, -new_Lon1)

    #para checar o resultado
    #tabela_corrigida %>% select(stateProvince, NOME, NOMEUF, y_estado, POINT_Y,municipality, decimalLatitude, new_Lat) %>% View()
    #tabela_corrigida %>% select(stateProvince, NOME, NOMEUF, x_estado, POINT_X,municipality, decimalLongitude, new_Lon) %>% View()
    write.csv(tabela_corrigida, file = nome_centroides)
}



####sp_filt de Diogo
mpos <- rgdal::readOGR(dsn = "./data/BRA_adm_shp", layer = "BRA_adm2")


for (i in 1:length(especies)) {
    print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
    nome_centroides <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                              "centroides.csv")
    nome_spfilt <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                              "sp_filt.csv")
    tabela_especie <- read.csv(nome_centroides, row.names = 1, stringsAsFactors = F)
    tabela_sppfilt <- tabela_especie %>% select("scientificName", 'new_Lon', 'new_Lat', 'municipality', "stateProvince") %>%
        rename(species = scientificName,
               lon = new_Lon,
               lat = new_Lat,
               adm1 = stateProvince)
    tabela_sppfilt <- na.exclude(tabela_sppfilt)
    sp_filt_res <- spfilt::filt(tabela_sppfilt, shape.municipios = mpos)
    write.csv(sp_filt_res, nome_spfilt)
}
