#### Script BGCI unificado

library("stringr")
library("rgbif")
library("xlsx")

tabela <- read.delim("especies.csv", header = FALSE, sep = ";", fileEncoding = "UTF-8")
View(tabela)
familias <- as.character(tabela$V1)
especies_complete <- as.character(tabela$V2)
especies <- word(especies_complete, start = 1, end = 2)


#extração de registros por espécie do gbif
for (i in 1:length(especies)){
  nome_arquivo <- paste(familias[i], especies[i], "raw", ".csv", sep = "_")
  header <- paste("familia", "nome_completo", "nome_sem_autor", "status_tipo","codigo_colecao","numero_tombo","coletor","numero_coleta","identificador", "data_id", "pais","estado","municipio","localidade","observacoes1", "observacoes2","observações3","lat","long","datum","ano_coleta","mes_coleta","dia_coleta", sep = "*")
  write(header, file = nome_arquivo)
  print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
  query.i <- occ_search(scientificName = especies[i])
  for (z in 1:length(query.i$data$name)){
    a <- query.i$data$family[z]
    b <- query.i$data$scientificName[z]
    c <- query.i$data$species[z]
    d <- query.i$data$typeStatus[z]
    e <- query.i$data$collectionCode[z]
    f <- query.i$data$catalogNumber[z]
    g <- query.i$data$recordedBy[z]
    h <- query.i$data$recordNumber[z]
    y <- query.i$data$identifiedBy[z]
    yy <- query.i$data$dateIdentified[z]
    j <- query.i$data$country[z]
    l <- query.i$data$stateProvince[z]
    m <-query.i$data$municipality[z]
    n <- query.i$data$locality[z]
    o <- query.i$data$occurrenceRemarks[z]
    x <- query.i$data$fieldNotes[z]
    xx <- query.i$data$eventRemarks[z]
    p <- query.i$data$decimalLatitude[z]
    r <- query.i$data$decimalLongitude[z]
    v <- query.i$data$geodeticDatum[z]
    s <- query.i$data$year[z]
    t <- query.i$data$month[z]
    u <- query.i$data$day[z]
    output <- paste(a,b,c,d,e,f,g,h,y,yy,j,l,m,n,o,x,xx,p,r,v,s,t,u,sep = "*")
    write(output, file = nome_arquivo, append = T)
  }

  #começar a conversão da espécie para o template do sistema

  tabela_gbif <- read.csv(nome_arquivo, sep = "*", header = TRUE, quote = "", row.names = NULL)


  tabela_sistema <- read.csv(file = "occurrences.csv", sep = ";", header = TRUE)
  colunas <- colnames(tabela_sistema)
  n_colunas <- length(colunas)
  n_linhas <- dim(tabela_gbif[1])
  tabela_neo <- matrix(nrow = n_linhas+1, ncol = n_colunas)
  colnames(tabela_neo) <- colunas

  #tenho que colocar em accepted name usage o nome científico da espécie aqui de acordo com a tabela original, com autor
  for (a in 1:dim(tabela_gbif)[1]){
    tabela_neo[a,7] <- as.character(tabela_gbif$familia[a])
    tabela_neo[a,5] <- as.character(tabela_gbif$nome_completo[a])
    tabela_neo[a,3] <- as.character(tabela_gbif$codigo_colecao[a])
    tabela_neo[a,4] <- as.character(tabela_gbif$numero_tombo[a])
    tabela_neo[a,8] <- word(as.character(tabela_gbif$nome_sem_autor[a]), start = 1, end = 1) #genero
    tabela_neo[a,9] <- word(as.character(tabela_gbif$nome_sem_autor[a]), start = 2, end = 2) #epiteto
    tabela_neo[a,11] <-  word(tabela_gbif$nome_completo[a], start = 3, end = str_count(tabela_gbif$nome_completo[a],"\\S+")) #nome de autor
    tabela_neo[a, 12] <- as.character(tabela_gbif$identificador[a])
    tabela_neo[a,13] <- as.character(tabela_gbif$data_id[a])
    tabela_neo[a,14] <- as.character(tabela_gbif$status_tipo[a])
    tabela_neo[a,15] <- as.character(tabela_gbif$numero_coleta[a])
    tabela_neo[a,17] <- as.character(tabela_gbif$coletor[a])
    tabela_neo[a,18] <- as.character(tabela_gbif$ano_coleta[a])
    tabela_neo[a,19] <- as.character(tabela_gbif$mes_coleta[a])
    tabela_neo[a,20] <- as.character(tabela_gbif$dia_coleta[a])
    tabela_neo[a,21] <- as.character(tabela_gbif$pais[a])
    tabela_neo[a,22] <- as.character(tabela_gbif$estado[a])
    tabela_neo[a,23] <- as.character(tabela_gbif$municipio[a])
    tabela_neo[a,24] <- as.character(tabela_gbif$localidade[a])
    tabela_neo[a,25] <- as.character(tabela_gbif$long[a])
    tabela_neo[a,26] <- as.character(tabela_gbif$lat[a])
    x <- paste(as.character(tabela_gbif$observacoes1[a]), as.character(tabela_gbif$observacoes2[a]), as.character(tabela_gbif$observacoes3[a]), sep = " ")
    x <- gsub("\\;","",x)
    x <- gsub("\\:","",x)
    x <- gsub('\"',"",x)
    x <- gsub("/","",x)
    tabela_neo[a,27] <- x
    tabela_neo[a,31] <- as.character(especies_complete[i])
  }

  write.table(file = nome_arquivo,tabela_neo, row.names = FALSE, quote = F, sep = "*")

  ############ aqui tem que entrar a parte referente ao inpa

  #isso pode entrar em outro lugar, acho que antes do loop, porque essa planilha só vai precisar ser lida uma vez
  library(readxl)
  tabela_inpa_splink <- read_excel(path = "planilha_geral_splink_inpa.xlsx", sheet = 1)
  head(tabela_inpa_splink)

library(dplyr)
  names(tabela_inpa_splink)

  inpa_filtered <- tabela_inpa_splink %>% filter(tabela_inpa_splink$scientificname %in% especies)
  write.csv(inpa_filtered, "inpa_filtrado.csv")
  length(unique(inpa_filtered$scientificname))
  tombos_inpa_splink <- tabela_inpa_splink$catalognumber #lista de tombos da planilha do inpa

  tabela_especie <- read.csv(file = nome_arquivo, header = TRUE, sep = "*", na.strings = c("", NA), quote = "", stringsAsFactors = FALSE)
  query <- grep("inpa", tabela_especie$collectionCode, ignore.case = TRUE)
  tombos_inpa_especie <- tabela_especie$catalogNumber[query]
  overlap_tombos <- as.numeric(intersect(tombos_inpa_splink, tombos_inpa_especie)) #problema

  if(length(overlap_tombos) > 0){ #só fazer isso se tiver coleta no inpa
    for (c in 1:length(overlap_tombos)){
      tombo.i <- overlap_tombos[c]
      regex.i <- paste("^", tombo.i, "$", sep = "")
      query.i <- grep(tombo.i, tabela_inpa_splink$catalognumber)
      autor.i <- tabela_inpa_splink$collector[query.i]
      query2.i <- grep(regex.i, tabela_especie$catalogNumber)
      tabela_especie$recordedBy[query2.i] <- as.character(autor.i)
      proof <- paste(as.character(tabela_especie$collectionCode[query2.i]), as.character(tabela_especie$catalogNumber[query2.i]), sep = " ")
      output <- paste("proof_inpa", familias[i], especies[i], sep = "")
      write(proof, file = output, append = TRUE)
    }}







  #########################################

  #Limpeza de registros

  nome_excluded <- paste(familias[i], especies[i], "exxcluded", ".csv", sep = "")

  #agora a "tabela" é tabela_especie, entao tenho que substituir isso tudo

  #remover registros não informativos, sem coletor, numero de coleta, ano e informações de localidade
  lines_remove <- numeric() #essas variáveis vazias que eu gero servem pros append embaixo
  for(b in 1:dim(tabela_especie)[1]){
    linha.i <- tabela_especie[b,]
    #print(b)
    if(is.na(linha.i$year) & is.na(linha.i$recordedBy) & is.na(linha.i$stateProvince) & is.na(linha.i$municipality) & is.na(linha.i$locality)){
      lines_remove <- append(b, lines_remove)}}
  if (length(lines_remove) > 0){  #antes tava if (exists("lines_remove"))
    tabela_exclude <- tabela_especie[lines_remove,]
    tabela_especie <- tabela_especie[-lines_remove,]
    rm(lines_remove)
    write.table(tabela_exclude, file = nome_excluded, quote = F, row.names = F, col.names = T, sep = "*", append = TRUE)
  }


  #remover registros fora do brasil ou que não tem informação de país

  lines_remove <- numeric() #essas variáveis vazias que eu gero servem pros append embaixo
  for(d in 1:dim(tabela_especie)[1]){
    linha.i <- tabela_especie[d,]
    #print(d)
    if (length(linha.i$country) == 0){{next}} #aid
    if(linha.i$country != "Brazil" | is.na(linha.i$country)){
      lines_remove <- append(d, lines_remove)}}
  if (length(lines_remove) > 0){  #antes tava if (exists("lines_remove"))
    tabela_exclude <- tabela_especie[lines_remove,]
    tabela_especie <- tabela_especie[-lines_remove,]
    rm(lines_remove)
    write.table(tabela_exclude, file = nome_excluded, quote = F, row.names = F, col.names = T, sep = "*", append = TRUE)
  }

  ############################
  #registros que não tem nome de coletor nem numero de coleta
  #esse loop tá removendo todas linhas, já consertei uma vez
  lines_remove <- numeric()
  if(dim(tabela_especie)[1] >= 1){
    for(k in 1:dim(tabela_especie)[1]){
      linha.i <- tabela_especie[k,]
      #print(k)
      if((is.na(linha.i$recordedBy)) & (is.na(linha.i$recordNumber))){
        lines_remove <- append(k, lines_remove)}}}

  if (length(lines_remove) > 0){ #mudei isso aqui, antes era if (exists("lines_remove"))
    tabela_exclude2 <- tabela_especie[lines_remove,]
    tabela_especie <- tabela_especie[-lines_remove,]
    write.table(tabela_exclude2, file = nome_excluded, quote = F, row.names = F, col.names = F, sep = "*", append = TRUE)
    rm(lines_remove)
  }



  #remover registros com número de coleta, estado e ano iguais (coletas colaborativas duplicadas)
  # tá considerando s.n. como numero de coleta igual, vou incluir um escape de s.n. e de NA
  #o escape são as duas linhas com next
  #parece estar ok

  vetor_duplicata <- as.character()
  for(f in 1:dim(tabela_especie)[1]){
    linha.i <- tabela_especie[f,]
    dado.i <- paste(linha.i$year, linha.i$recordNumber, linha.i$stateProvince, sep = "")
    #print(dado.i)
    vetor_duplicata <- append(vetor_duplicata, dado.i)
  }


  for (a in 1:length(vetor_duplicata)){
    teste.i <- vetor_duplicata[a]
    if(length(grep("s.n.", vetor_duplicata[a])) >= 1){next}
    if(length(grep("NA", vetor_duplicata[a], ignore.case = FALSE)) >= 1){next}
    x <- tryCatch({grep(teste.i, vetor_duplicata)}, error = function(e) {x <- 0}) #inclui um trycatch aqui
    #print(x)
    if (length(x) > 1){
      pos.i <- grep(teste.i, vetor_duplicata)
      pos2.i <- pos.i[(1+1):length(pos.i)]
      pos2.i <- pos2.i[!is.na(pos2.i)]
      write(pos2.i, file = "exclude", append = TRUE, sep = "\n")
      rm(x)
    }
  }


  query <- grep("^exclude$", list.files())

  if (length(query) >= 1){
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
  for(b in 1:dim(tabela_especie)[1]){
    linha.i <- tabela_especie[b,]
    dado.i <- paste(linha.i$recordedBy, linha.i$recordNumber, sep = "")
    #print(b)
    vetor_duplicata <- append(vetor_duplicata, dado.i)
  }


  for (a in 1:length(vetor_duplicata)){
    teste.i <- vetor_duplicata[a]
    if(length(grep("s.n.", vetor_duplicata[a])) >= 1){next} #escape
    if(length(grep("NA", vetor_duplicata[a], ignore.case = FALSE)) >= 1){next} #escape
    #print(a)
    x <- tryCatch({grep(teste.i, vetor_duplicata)}, error = function(e) {x <- 0}) #inclui um trycatch aqui
    if (length(x) > 1){
      pos.i <- grep(teste.i, vetor_duplicata)
      pos2.i <- pos.i[(1+1):length(pos.i)]
      write(pos2.i, file = "exclude", append = TRUE, sep = "\n")
      rm(x)
    }
  }


  query <- grep("^exclude$", list.files())
  if (length(query) >= 1){
    positions_exclude <- as.numeric(unique(readLines("exclude")))
    positions_exclude <- positions_exclude[!is.na(positions_exclude)]
    file.remove("exclude")
    write.table(tabela_especie[positions_exclude,], file = nome_excluded, quote = F, row.names = F, col.names = T, sep = "*", append = TRUE)
    tabela_especie <- tabela_especie[-positions_exclude,]
  }

  nome_clean <- paste(familias[i], especies[i], "clean", ".csv", sep = "_")

  write.table(tabela_especie, file = nome_clean, quote = F, row.names = F, col.names = T, sep = "*", append = TRUE, na = "")


}



for (a in 1:length(familias)){
  setwd("C:/wd/")
  query_fam <- grep(familias[a], tabela$V1) #especies da familia

  for (b in 1:length(query_fam)){
    setwd("C:/wd/")
    especie.i <- as.character(tabela$V2[query_fam[b]])
    especie.i <- word(especie.i, start = 1, end = 2)
    print(especie.i)
    arquivos <- grep(especie.i, list.files(), value = TRUE)
    setwd(paste("C:/wd/", familias[a], sep = "/"))
    query2.i <- grep(especie.i, list.files(), value = TRUE)
    for (i in 1:length(arquivos)){
      output_origin <- paste("C:/wd/", arquivos[i], sep = "/")
      output_destiny <- paste("C:/wd/", familias[a], query2.i, sep = "/")
      file.copy(from = output_origin, to = output_destiny)
    }
  }
}

##

setwd("C:/wd/")
dump <- grep("clean|exx|raw", list.files(), value = TRUE)
file.remove(dump)


