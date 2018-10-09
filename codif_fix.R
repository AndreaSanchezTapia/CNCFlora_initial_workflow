
arquivo <- grep("pronta", list.files(), ignore.case = T, value = T)

#vai ler o arquivo pronto com duas codificações

leitura1 <- read.delim(file = "CombretaceaeTerminalia acuminatapronta.csv", header = TRUE, sep = ";", fileEncoding = "ISO-8859-9")
leitura2 <- read.delim(file = "CombretaceaeTerminalia acuminatapronta.csv", header = TRUE, sep = ";", fileEncoding = "UTF-8")

#vai escrever com duas codificações, com os nomes teste1 e teste2

write.table(teste, file = "teste1.csv", row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "UTF-8", na = "")
write.table(teste, file = "teste2.csv", row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "ISO-8859-9", na = "")