#script para inserir centroides em registro sem localidade e ordenar planilha por estado

#nesse script não consta o fix que fiz para corrigir os frameshift por gsub tirando ; por readLines nas planilhas, talvez precise

#primeiro ler a planilha com a lista de coordenadas por centroide


tabela_centroides <- read.delim(file = "centroide_municipio.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = "ISO-8859-9")
tabela_centroides_ucs <- read.delim(file = "centroide_uc.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = "ISO-8859-9")
arquivos <- grep("revisada", list.files(), value = TRUE, ignore.case = TRUE)
fa <- function(x) iconv(x, to = "ASCII//TRANSLIT") #função para remover acentos

for (z in 1:length(arquivos)){
options(warn=0)
setwd("C:/wd/")
print(z)


beta <- readLines(arquivos[z])
for(i in 1:length(beta)){
beta[i] <- gsub(";", "", beta[i])
}

write(beta, file = arquivos[z])

tabela_modelo <- read.delim(file = arquivos[z], header = TRUE, sep = "*", fileEncoding = "ISO-8859-9", quote = "", stringsAsFactors = FALSE)
familia <- as.character(tabela_modelo$family[1])
especie <- as.character(tabela_modelo$acceptedNameUsage[1])
#primeiro incluir coordenadas de tudo que tem munic?pio e não tem localidade
for (i in 1:length(tabela_modelo$family)){
localidade <- as.character(tabela_modelo$locality[i])
if (is.na(localidade)){

localidade <- as.character("")

}

#if (nchar(localidade) > 0){{next}} #eu acho que esse next tá redundante
#if (length(localidade) == 0){{next}} ########### adicionei isso dps, pode ser retirado
if (nchar(localidade) == 0){
municipio <- as.character(tabela_modelo$municipality[i])
consulta <- paste("^", municipio, "$", sep = "")
estado <- as.character(tabela_modelo$stateProvince[i])
estado <- toupper(fa(estado))
tabela_estado <- tabela_centroides[tabela_centroides$NOMEUF == estado,]
query_mun <- grep(consulta, tabela_estado$NOME)
fid <- tabela_estado$FID[query_mun]
if (length(fid) == 0){{next}}
fid <- paste("^", fid, "$", sep = "")
query2 <- grep(fid, tabela_centroides$FID)
if (length(query2) > 0){
longitude <- tabela_centroides$POINT_X[query2]
latitude <- tabela_centroides$POINT_Y[query2]
tabela_modelo$decimalLongitude[i] <- longitude
print(paste(arquivos[z], longitude, sep = " "))
tabela_modelo$decimalLatitude[i] <- latitude
tabela_modelo$comments[i] <- as.character(paste(as.character(tabela_modelo$comments[i]), "centroide_inserido_municipio_R", sep = " ")) #desse jeito mantem-se o que já tava escrito em comments
write.table(tabela_modelo[i,], file = "centroides_inseridos.csv", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE, sep = "*", na = "")
}
}
}



output <- paste(as.character(familia), as.character(especie), "pronta.csv", sep = "")

setwd("C:/wd/")
write.table(tabela_modelo, file = output, quote = FALSE, sep = ";", row.names = FALSE, fileEncoding = "ISO-8859-9", na = "", dec = ".")
output_log1 <- paste(arquivos[z], length(beta), dim(tabela_modelo)[1], sep = ";")
write(output_log1, file = "log_registros.txt", append = TRUE)
setwd("C:/wd/")

}
