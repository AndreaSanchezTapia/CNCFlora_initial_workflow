# 11/out/18 revisando este script para inserir centroides em registro sem localidade e ordenar planilha por estado
# só as espécies da lista que patrícia enviou
# carrega pacotes
library(dplyr)
library("stringr")
library("rgbif")
library(readxl)
library(textclean)
library(flora)
#devtools::install_github("diogosbr/spfilt")
library(spfilt)
# le tudo
arvoresLC <- read_excel("./data/arvores_endemicas_possiveis_nao_ameacadas.xlsx", sheet = 1)
#tira autores
nomesLC <- lapply(arvoresLC$Species,FUN = remove.authors) %>% simplify2array()

tabela_sistema <- read.csv(file = "./data/occurrences.csv", sep = ";", header = TRUE)

#primeiro ler a planilha com a lista de coordenadas por centroide----
tabela_centroides <- read.delim(file = "./data/centroide_municipio.csv",
                                header = TRUE, sep = ";",
                                stringsAsFactors = FALSE,
                                fileEncoding = "ISO-8859-9")
# tira todos os caracteres e bota minuscula
tabela_centroides <- tabela_centroides %>%
    mutate(municipality = replace_non_ascii(tolower(NOME)),
           stateProvince = replace_non_ascii(tolower(NOMEUF)))
#centroides ucs----
tabela_centroides_ucs <- read.delim(file = "./data/centroide_uc.csv",
                                    header = TRUE, sep = ";",
                                    stringsAsFactors = FALSE,
                                    fileEncoding = "ISO-8859-9")
tabela_centroides_ucs <- tabela_centroides_ucs %>%
    mutate(uc = replace_non_ascii(tolower(NOME_UC1)))

#shape estados----
library(rgdal)
estados <- rgdal::readOGR(dsn = "./data/shape/Limites_v2017/", layer = "lim_unidade_federacao_a")
#tabela com a sigla pois uma limpeza é substituir a sigla ("rj") pelo nome completo
sigla_estados <- estados@data[,c("nome", "sigla")] %>% data.frame() %>%
    mutate(stateProvince = replace_non_ascii(tolower(nome))) %>%
    mutate(sigla = replace_non_ascii(tolower(sigla)))
#centroides estados----
#já deixa em minusculo sem acento tudo
centroides_estados <-
    rgeos::gCentroid(estados, byid = T, id = estados$nome) %>%
    data.frame %>%
    tibble::rownames_to_column(var = "estados_shp") %>%
    mutate(stateProvince = replace_non_ascii(tolower(estados_shp))) %>%
    rename(x_estado = x, y_estado = y) %>% left_join(sigla_estados)

centroides_estados

#compara o nome dos estados e dos municipios porque há municipios com o mesmo nome de alguns estados, importante para a limpeza----
setdiff(centroides_estados$stateProvince, tabela_centroides$municipality)
#mesmo nome estado e municipio
tabela_centroides[which(tabela_centroides$municipality %in%  centroides_estados$stateProvince),]
dupl_names_state_city <- tabela_centroides$municipality[which(tabela_centroides$municipality %in%  centroides_estados$stateProvince)]
#nomes de estado seguros
non_dupl_names <- setdiff(centroides_estados$stateProvince, dupl_names_state_city)

#nomes unicos de municipio
#hay 5570municipios, 282 son duplicados
unique_mpo <- tabela_centroides %>% distinct(municipality) %>% pull()
dupl_mpo <- tabela_centroides$municipality[duplicated(tabela_centroides$municipality)]
mpo_estado_unico <- setdiff(unique_mpo, dupl_mpo)

#assignação de centroides----
especies <- nomesLC
familias <- arvoresLC$Family
#cria um vetor vazio para ficar de olho em algumas espçecies que ainda tem NA nas notas.
checar <- vector()
for (i in 1:length(especies)) {
    print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))

    nome_clean <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                         "clean.csv")
    nome_centroides <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                         "centroides.csv")
    tabela_especie <- read.csv(nome_clean, row.names = 1, stringsAsFactors = F) %>%
        mutate(catalogNumber = factor(catalogNumber))

    #igual com o municipio
    tabela_especie_edit <- tabela_especie %>%
        mutate(municipality = replace_non_ascii(tolower(municipality)),
               stateProvince = replace_non_ascii(tolower(stateProvince)),
               country = replace_non_ascii(tolower(country))) %>%
#corrige espirito santo
                mutate(stateProvince = ifelse(stateProvince == "espa-rito santo",
                                      "espirito santo",
                                      stateProvince)) %>%
        #corrige estados na casa de municipios
        mutate(municipality = ifelse(municipality %in% c("brasil", "brazil", non_dupl_names), NA, municipality))
#corrige nombres de estados
    #corrige siglas
    substituir_siglas <- function(x) {
        if (any(sigla_estados$sigla %in% x)) {
            return(sigla_estados$stateProvince[which(sigla_estados$sigla == x)])
        } else {
            x
        }
    }

    tabela_especie_edit$stateProvince <- lapply(tabela_especie_edit$stateProvince, substituir_siglas) %>% simplify2array()

#junta con centroides
    tabela_especie_edit <- tabela_especie_edit %>%
        left_join(tabela_centroides) %>%
        left_join(centroides_estados)
#assignar centroides
        tabela_corrigida <- tabela_especie_edit %>%
            # cria as colunas
mutate(new_Lat = NA, new_Lon = NA, notes = NA) %>%
            # quando é numérico e não é zero
            mutate(new_Lat = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                                     decimalLatitude, new_Lat),
                   new_Lon = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                                     decimalLongitude, new_Lon),
                   notes = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                                  "original coordinates", notes)) %>%
            #cuando existen y valen cero
            mutate(new_Lat = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 & !is.na(municipality) &
                    !is.na(stateProvince),
                POINT_Y,
                new_Lat),
                   new_Lon = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 &  !is.na(municipality) &
                   !is.na(stateProvince),
                POINT_X,
                new_Lon),
                notes = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 &  !is.na(municipality) &
                    !is.na(stateProvince),
                "centroide mpo (0)",
                notes)) %>%
            #cuando existen y valen cero
            mutate(new_Lat = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 & is.na(municipality) &
                    !is.na(stateProvince),
                y_estado,
                new_Lat),
                   new_Lon = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 &  is.na(municipality) &
                   !is.na(stateProvince),
                x_estado,
                new_Lon),
                notes = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 &  is.na(municipality) &
                    !is.na(stateProvince),
                "centroide estado (0)",
                notes)) %>%
        #quando não existe , bota o estado
            mutate(new_Lat = ifelse(
                is.na(decimalLatitude) & is.na(decimalLongitude) &  is.na(municipality) &
                    !is.na(stateProvince), y_estado, new_Lat),
                new_Lon = ifelse(
                    is.na(decimalLatitude) & is.na(decimalLongitude) &  is.na(municipality) &
                        !is.na(stateProvince), y_estado, new_Lon),
                notes = ifelse(
                    is.na(decimalLatitude) & is.na(decimalLongitude) &  is.na(municipality) &
                        !is.na(stateProvince), "centroide estado", notes)) %>%
            #quando não existe mas tem municipio , bota o municipio
            mutate(new_Lat = ifelse(
                is.na(decimalLatitude) & is.na(decimalLongitude) &  !is.na(municipality) &
                    !is.na(stateProvince), POINT_Y, new_Lat),
                new_Lon = ifelse(
                    is.na(decimalLatitude) & is.na(decimalLongitude) &  !is.na(municipality) &
                        !is.na(stateProvince), POINT_Y, new_Lon),
                notes = ifelse(
                    is.na(decimalLatitude) & is.na(decimalLongitude) &  !is.na(municipality) &
                        !is.na(stateProvince), "centroide mpo", notes)) %>%
            mutate(notes = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 & is.na(municipality) &
                    is.na(stateProvince),
                "no coordinates, municipality or state provided (0)", notes)) %>%
            mutate(notes = ifelse(
                    is.na(decimalLatitude) & is.na(decimalLongitude) &  is.na(municipality) &
                        is.na(stateProvince),
                    "no coordinates, municipality or state provided", notes)) %>%
    mutate(new_Lat = ifelse(is.na(notes) & municipality %in% unique_mpo, POINT_Y, new_Lat),
           new_Lon = ifelse(is.na(notes) & municipality %in% unique_mpo, POINT_X, new_Lon),
           notes = ifelse(is.na(notes) & municipality %in% unique_mpo, "centroide mpo (no state)", notes)) %>%
            mutate(new_Lat = ifelse(is.na(notes) & municipality %in% dupl_mpo, POINT_Y, new_Lat),
                   new_Lon = ifelse(is.na(notes) & municipality %in% dupl_mpo, POINT_X, new_Lon),
                   notes = ifelse(is.na(notes) & municipality %in% dupl_mpo, "check coordinate: municipality name is duplicated", notes))


print(count(tabela_corrigida, notes))

    tabela_corrigida <- tabela_corrigida %>%
        select(-x_estado, -y_estado, -POINT_X, -POINT_Y,
               -NOME, -GEOCODIGO, -NOMEUF, -SIGLAUF, -estados_shp, -nome, -sigla)

    #para checar o resultado
    write.csv(tabela_corrigida, file = nome_centroides)
if (any(is.na(tabela_corrigida$notes))) {
    checar <- append(checar, i)
    tabela_checar <- tabela_corrigida %>% filter(is.na(notes))
    nome_checar <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                              "centroides_checar.csv")
    write.csv(tabela_checar, file = nome_checar)
}
}

checar

####sp_filt de Diogo
spfilt::filt()
# mpos <- rgdal::readOGR(dsn = "./data/shape/Limites_v2017/", layer = "lim_municipio_a")
# tabela_centroides_2 <- tabela_centroides %>% rename(geocodigo = GEOCODIGO, nome = NOME) %>% mutate(geocodigo = as.factor(geocodigo))
# mpos@data <- left_join(mpos@data, tabela_centroides_2)
# proj4string(mpos)
# mpos2 <- sp::spTransform(mpos, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# names(mpos2)
# dim(mpos@data)

for (i in 1:length(especies)) {
    print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
    nome_centroides <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                              "centroides.csv")
    nome_spfilt <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                              "sp_filt.csv")
    tabela_especie <- read.csv(nome_centroides, row.names = 1, stringsAsFactors = F)
    tabela_especie <- tabela_especie %>% dplyr::mutate(ID = row_number())
    tabela_sppfilt <- tabela_especie %>%
        filter(notes %in% c("original coordinates", "centroide mpo", "centroide mpo (0)", NA))

    tabela_sppfilt <- tabela_sppfilt %>%
        select("scientificName", 'new_Lon', 'new_Lat', 'municipality', "stateProvince") %>%
        rename(species = scientificName,
               lon = new_Lon,
               lat = new_Lat,
               adm1 = stateProvince)

        tabela_sppfilt <- tabela_sppfilt[complete.cases(cbind(tabela_sppfilt$lon, tabela_sppfilt$lat)),]


    sp_filt_res <- filt(pts = tabela_sppfilt,
                           inverted = T)

sp_filt_res <- sp_filt_res %>% rename(scientificName = species,
                                      new_Lon = lon,
                                      new_Lat = lat,
                                      municipality = county.orig)

#resultado_final <- left_join(tabela_sppfilt, sp_filt_res)
#if (nrow(tabela_especie) != nrow(resultado_final)) stop()
        write.csv(sp_filt_res, nome_spfilt)
}

