#### Script BGCI unificado
library(dplyr)
library("stringr")
library("rgbif")
library(readxl)

arvoresLC <- read_excel("./data/arvores_endemicas_possiveis_nao_ameacadas.xlsx", sheet = 1)
head(arvoresLC)
tabela <- read.delim("./data/especies.csv", sep = ";", header = F)
head(tabela)
familias <- as.character(tabela$V1)
especies_complete <- as.character(tabela$V2)
especies <- word(especies_complete, start = 1, end = 2)
#le inpa----
tabela_inpa_splink <- read.csv(file = "./data/inpa_filtrado.csv", header = TRUE)
############ aqui tem que entrar a parte referente ao inpa
# a coluna acceptedNameUsage não existe e a coluna scientifiName nãõ tem autor
tabela_inpa_splink <-     tabela_inpa_splink %>%
    dplyr::mutate(acceptedNameUsage = paste(scientificName, scientificNameAuthorship))
#os comentarios tem que ser cirados igual que para gbof
comm_inpa <- tabela_inpa_splink %>% dplyr::select(contains("Notes"), contains("Remarks"), contains("typeStatus")) %>%  tidyr::unite(comment)
tabela_inpa_splink <- bind_cols(tabela_inpa_splink, comm_inpa)

tombos_inpa_splink <- tabela_inpa_splink$catalogNumber #lista de tombos da planilha do

#isso pode entrar em outro lugar, acho que antes do loop, porque essa planilha só vai precisar ser lida uma vez
#----
tabela_sistema <- read.csv(file = "./data/occurrences.csv", sep = ";", header = TRUE)
#extração de registros por espécie do gbif
#dir.create("output")

#buscando registros de gbif
for (i in 1:length(especies)) {
    dir.create(paste0("./output/",familias[i]), showWarnings = F)
    nome_arquivo <- paste0("./output/", familias[i],"/",familias[i],"_", especies[i],"_", "raw.csv")
    print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))

    key <- name_backbone(name = especies[i])$speciesKey
    if (!is.null(key)) {
        occs <- list()
        for (k in 1:length(key)) {
            occs[[k]] <- occ_search(
                taxonKey = key[k],
                limit = 100000,
                #hasCoordinate = TRUE,
                basisOfRecord = "PRESERVED_SPECIMEN",
                #hasGeospatialIssue = F,
                return = 'data'
                #fields = "minimal"
            )
        }
        print(lapply(occs,dim))
        if (any(!is.null(lapply(occs,dim)))) {
            dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
            occs.f <- subset(occs, dim.null == T)
            occs.f <- dplyr::bind_rows(occs.f)
            print(dim(occs.f))
        }
    } else {
        cat(paste("No key found for", especies[i], "\n"))
        }

    #query.i <- occ_search(scientificName = especies[i])
    occs.df1 <- occs.f %>%
        #great way to extract names
        #tabela_neo[a,11] <-  word(tabela_gbif$scientificName[a], start = 3, end = str_count(tabela_gbif$nome_completo[a],"\\S+")) #nome de autor
        dplyr::mutate(acceptedNameUsage = scientificName,
                  scientificNameAuthorship = word(scientificName,
                                                  start = 3,
                                                  end = str_count(scientificName, "\\S+")))
        #as colunas que criam os comentários nem sempre existem:

    comm <- occs.df1 %>% dplyr::select(contains("Notes"), contains("Remarks"), contains("typeStatus"))
    if (ncol(comm) != 0) {
        comm <-  tidyr::unite(comm, comment)
        occs.df1 <- bind_cols(occs.df1, comm)
    }

tabela_especie <- occs.df1 %>% select(one_of(names(tabela_sistema)))
write.csv(tabela_especie, file = nome_arquivo)


#tenho que colocar em accepted name usage o nome científico da espécie aqui de acordo com a tabela original, com autor
#eu acabo de incluir a coluna scientificName - fica co autor. Andrea 9/10/18
#nem precisa ler mais aqui
#tabela_gbif <- read.csv(nome_arquivo, row.names = 1)
#as.vector(apply(tabela_gbif, 2, class))#é tudo character
#já é tudo caractere não precisa modificar
#e não precisa modificar linha pr linha, a coluna pode.

#isto é absolutamente necessário?
#x <- paste(as.character(tabela_gbif$observacoes1[a]), as.character(tabela_gbif$observacoes2[a]), as.character(tabela_gbif$observacoes3[a]), sep = " ")
#x <- gsub("\\;","",x)
#x <- gsub("\\:","",x)
#x <- gsub('\"',"",x)
#x <- gsub("/","",x)
#tabela_neo[a,27] <- x
#    }



#dir.create("inpa")
#for (i in 1:length(especies)) {
 #   nome_arquivo <- paste0("./output/", familias[i],"/",familias[i],"_", especies[i],"_", "raw.csv")
    nome_out <- paste0("./output/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa.csv")
    print(paste("Inpa", especies[i], i, "de", length(especies), sep = " "))

    #tabela_especie <- read.csv(nome_arquivo, row.names = 1, stringsAsFactors = F)

    query <- grep("inpa", tabela_especie$collectionCode, ignore.case = TRUE)
    tombos_inpa_especie <- tabela_especie$catalogNumber[query]
    overlap_tombos <- as.numeric(intersect(tombos_inpa_splink, tombos_inpa_especie))
    #problema
    if (length(overlap_tombos) > 0) { #só fazer isso se tiver coleta no inpa
        for (c in 1:length(overlap_tombos)) {
            tombo.i <- overlap_tombos[c]
            regex.i <- paste("^", tombo.i, "$", sep = "")
            query.i <- grep(tombo.i, tabela_inpa_splink$catalognumber)
            autor.i <- tabela_inpa_splink$collector[query.i]
            query2.i <- grep(regex.i, tabela_especie$catalogNumber)
            #nao vou fazer isto porque já não cria o problema
            #tabela_especie$recordedBy[query2.i] <- as.character(autor.i)
            proof <- paste(as.character(tabela_especie$collectionCode[query2.i]), as.character(tabela_especie$catalogNumber[query2.i]), sep = " ")
            proof_name <- paste0("./output/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa_proof.csv")
            write(proof, file = proof_name, append = TRUE)
        }
    }
#seleciona as colunas que tem o mesmo nome qu eas colunas de tabela especie
    inpa.sp <- tabela_inpa_splink %>%
        filter(catalogNumber %in% tombos_inpa_especie) %>%
        select(one_of(names(tabela_especie)))
    tabela_especie <- bind_rows(tabela_especie, inpa.sp)
    write.csv(tabela_especie, file = nome_out)
}



    #########################################

    #Limpeza de registros
for (i in 1:length(especies)) {
   nome_arquivo <- paste0("./output/", familias[i],"/",familias[i],"_", especies[i],"_", "raw.csv")
    nome_out <- paste0("./output/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa.csv")
    print(paste("Limpando", especies[i], i, "de", length(especies), sep = " "))
    nome_excluded <- paste0("./output/",familias[i],"/",familias[i], "_", especies[i],"_",
                            "excluded.csv")

#agora a "tabela" é tabela_especie, entao tenho que substituir isso tudo
tabela_especie <- read.csv(nome_out, row.names = 1, stringsAsFactors = F)

#remover registros não informativos, sem coletor, numero de coleta, ano e informações de localidade----


tabela_exclude1 <- tabela_especie %>% dplyr::filter(is.na(year) & is.na(recordedBy) & is.na(stateProvince) & is.na(municipality)& is.na(locality))
tabela_especie <- dplyr::anti_join(tabela_especie, tabela_exclude1)
#remover registros fora do brasil ou que não tem informação de país----
tabela_exclude2 <- tabela_especie %>% dplyr::filter(is.na(country) | country != "Brazil")
tabela_especie <- dplyr::anti_join(tabela_especie, tabela_exclude2)
#registros que não tem nome de coletor nem numero de coleta
tabela_exclude3 <- tabela_especie %>%
    dplyr::filter(is.na(recordedBy) & is.na(recordNumber))
tabela_especie <- dplyr::anti_join(tabela_especie, tabela_exclude3)
tabela_exclude <- dplyr::bind_rows(tabela_exclude1, tabela_exclude2, tabela_exclude3)
write.csv(tabela_exclude, file = nome_excluded)
}

#esse loop tá removendo todas linhas, já consertei uma vez
#remover registros com número de coleta, estado e ano iguais (coletas colaborativas duplicadas)
    # tá considerando s.n. como numero de coleta igual, vou incluir um escape de s.n. e de NA
    #o escape são as duas linhas com next
    #parece estar ok
    vetor_duplicata <- as.character()
    for (f in 1:dim(tabela_especie)[1]) {
        linha.i <- tabela_especie[f,]
        dado.i <- paste(linha.i$year, linha.i$recordNumber, linha.i$stateProvince, sep = "")
        #print(dado.i)
        vetor_duplicata <- append(vetor_duplicata, dado.i)
    }

    for (a in 1:length(vetor_duplicata)) {
        teste.i <- vetor_duplicata[a]
        if (length(grep("s.n.", vetor_duplicata[a])) >= 1) {next}
        if (length(grep("NA", vetor_duplicata[a], ignore.case = FALSE)) >= 1) {next}
        x <- tryCatch({grep(teste.i, vetor_duplicata)}, error = function(e) {x <- 0}) #inclui um trycatch aqui
        #print(x)
        if (length(x) > 1) {
            pos.i <- grep(teste.i, vetor_duplicata)
            pos2.i <- pos.i[(1 + 1):length(pos.i)]
            pos2.i <- pos2.i[!is.na(pos2.i)]
            #write(pos2.i, file = "exclude", append = TRUE, sep = "\n")
            rm(x)
        }
    }
tabela_exclude <- bind_rows(tabela_exclude,  slice(tabela_especie, pos2.i))
write.csv(tabela_exclude, file = nome_excluded)
tabela_especie <- tabela_especie %>% dplyr::slice(-pos2.i)


    if (length(query) >= 1) {
        positions_exclude <- as.numeric(unique(readLines("exclude")))
        file.remove("exclude")
        positions_exclude <- positions_exclude[!is.na(positions_exclude)] #isso remove os NA do vetor
        write.table(tabela_especie[positions_exclude,], file = nome_excluded, quote = F, row.names = F, col.names = T, sep = "*", append = TRUE)
        tabela_especie <- tabela_especie[-positions_exclude,] #não pode ter NA aqui
    }

    ####################################################
    #registros com mesmo coletor e número de coleta
    #preciso incluir escape s.n. e NA {next}

    vetor_duplicata <- as.character()
    for (b in 1:dim(tabela_especie)[1]) {
        linha.i <- tabela_especie[b,]
        dado.i <- paste(linha.i$recordedBy, linha.i$recordNumber, sep = "")
        #print(b)
        vetor_duplicata <- append(vetor_duplicata, dado.i)
    }


    for (a in 1:length(vetor_duplicata)) {
        teste.i <- vetor_duplicata[a]
        if (length(grep("s.n.", vetor_duplicata[a])) >= 1){next} #escape
        if (length(grep("NA", vetor_duplicata[a], ignore.case = FALSE)) >= 1){next} #escape
        #print(a)
        x <- tryCatch({grep(teste.i, vetor_duplicata)}, error = function(e) {x <- 0}) #inclui um trycatch aqui
        if (length(x) > 1) {
            pos.i <- grep(teste.i, vetor_duplicata)
            pos2.i <- pos.i[(1 + 1):length(pos.i)]
            write(pos2.i, file = "exclude", append = TRUE, sep = "\n")
            rm(x)
        }
    }


    query <- grep("^exclude$", list.files())
    if (length(query) >= 1) {
        positions_exclude <- as.numeric(unique(readLines("exclude")))
        positions_exclude <- positions_exclude[!is.na(positions_exclude)]
        file.remove("exclude")
        write.table(tabela_especie[positions_exclude,], file = nome_excluded, quote = F, row.names = F, col.names = T, sep = "*", append = TRUE)
        tabela_especie <- tabela_especie[-positions_exclude,]
    }

    nome_clean <- paste(familias[i], especies[i], "clean", ".csv", sep = "_")

    write.table(tabela_especie, file = nome_clean, quote = F, row.names = F, col.names = T, sep = "*", append = TRUE, na = "")


}



