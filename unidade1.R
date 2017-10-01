require(rvest) # importa a biblioteca rvest
library(stringr) #importa a biblioteca stringr
library(sp)
library(RColorBrewer)

cidades<-read_html("http://cidades.ibge.gov.br/download/mapa_e_municipios.php?lang=&uf=rn") %>%
  html_table(fill=TRUE)

base<-data.frame(cidades[[1]][c(1,2,4)]) # Selecionando os dados de interesse
names(base)<-c("cidade","cep","pop2010") # Nomeando as colunas
row.names(base)<-1:168
# Enumerando as linhas
base$pop2010<-str_replace_all(base$pop2010,"\\.","") # Retira os pontos
base$pop2010[is.na(base$pop2010)] <- 0# Substitui valores NAs por zeros
base <- head(base,-1)# Remove a ultima linha da tabela

base$pop2010<-as.numeric(base$pop2010) # Converte os caracteres em numericos
head(base)

setwd("/home/diego/Desktop/TI_na_saude/mapas") # Define o diretorio de trabalho
br <- readRDS("BRA_adm2.rds") # Importa os poligonos do arquivo no diretorio de trabalho

rn = (br[br$NAME_1=="Rio Grande do Norte",]) # Filtrando apenas os municípios do RN
#plot(rn)# Plotando o mapa do RN, organizado por municipios
#plot(rn[rn$NAME_2=="Natal",], add=T, col="red")
rn$NAME_2[which(rn$NAME_2=="Governador Dix-Sept Rosad")]<-"Governador Dix-Sept Rosado"
rn$NAME_2[which(rn$NAME_2=="Lagoa de Anta")]<-"Lagoa d`Anta"
rn$NAME_2[which(rn$NAME_2=="Lagoas de Velhos")]<-"Lagoa de Velhos"
rn$NAME_2[which(rn$NAME_2=="Jardim-Piranhas")]<-"Jardim de Piranhas"
rn$NAME_2[which(rn$NAME_2=="Olho-d'Água do Borges")]<-"Olho-d`Água do Borges"
rn$NAME_2[which(rn$NAME_2=="Passabém")]<-"Passagem"
rn$NAME_2[which(rn$NAME_2=="Santana")]<-"Santana do Seridó"
rn$NAME_2[which(rn$NAME_2=="Junco")]<-"Messias Targino"
rn$NAME_2[which(rn$NAME_2=="São Miguel de Touros")]<-"São Miguel do Gostoso"
rn$NAME_2[which(rn$NAME_2=="Presidente Juscelino")]<-"Serra Caiada"
rn$NAME_2[which(rn$NAME_2=="Groaíras")]<-"Grossos"

rn <- rn[-41,] # remove Fernando de Noronha
rn <- rn[-109,] # remove Poço dantas

rn <- merge(x=rn, y=base, by.x="NAME_2", by.y="cidade") # Faz um merge dos dataframes 41 109
# Criando os intervalos e classificando
col_no = as.factor(cut(rn$pop2010, breaks = c(0,3000,10000,100000,300000,500000,800000,1000000), labels=c("<3k", "3k-10k", "10k-
100k","100k-300k", "300k-500k", "500k-800k", ">800k"), right= FALSE))
# Nomeando os intervalos – irá aparecer na legenda do grafico
levels(col_no) = c("<3k", "3k-10k", "10k-100k","100k-300k", "300k-500k", "500k-800k", ">800k")
# Adicionando a informação da categoria no dataframe
rn$col_no = col_no
myPalette = brewer.pal(7,"Greens")

#png(filename="SEMnoronha.png")
spplot(rn, "col_no", col=grey(.9), col.regions=myPalette, main="Municípios do RN")
#dev.off()

