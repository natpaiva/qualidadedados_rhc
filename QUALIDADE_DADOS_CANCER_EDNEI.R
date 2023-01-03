### TCC ÉDNEI###
# ANÁLISE DE COMPLETUDE, INCOSISTENCIA E COBERTURA DOS RHC PARA CANCER DE MAMA E COLO DO UTERO 
# UNIVERSIDADE FEDERAL DO RIO DE JANEIRO
# INSTITUTO DE ESTUDOS EM SAÚDE COLETIVA
# BASE DE 2008-2016 BAIXADA NO DIA 04/09/2022

library(dplyr)
library(ggplot2)
library(rio)
library(kableExtra)
library(readxl)
library(stringr)
library(reshape2)
library(readr)
library(patchwork)
library(openxlsx)
library(tidyr)
library(descr)
library(glue)
library(tableone)
library(openxlsx)

memory.limit(9999999999)

# Diretorio

setwd("C:/Users/junio/Downloads/TCC - ÉDNEI/bases separadas RHC - 2008.2016")

# base de dados

RHC2008 <- import("rhc08.dbf")
RHC2009 <- import("rhc09.dbf")
RHC2010 <- import("rhc10.dbf")
RHC2011 <- import("rhc11.dbf")
RHC2012 <- import("rhc12.dbf")
RHC2013 <- import("rhc13.dbf")
RHC2014 <- import("rhc14.dbf")
RHC2015 <- import("rhc15.dbf")
RHC2016 <- import("rhc16.dbf")

# Juntando todas as bases

RHC.CCU <- rbind(RHC2008,RHC2009,RHC2010, RHC2011, RHC2012, 
                   RHC2013,RHC2014, RHC2015, RHC2016)

# FILTRANDO CASOS ANALITICOS

table(RHC.CCU$TPCASO) #1 CASOS ANALITICOS

RHC.CCU <- filter(RHC.CCU, TPCASO == "1")

# Filtrando para Cancer de Mama e Colo do Utero

table(RHC.CCU$LOCTUDET) 

RHC.CCU <- filter(RHC.CCU, LOCTUDET == "C50" | LOCTUDET == "C53")

# Salvando em XLSX

write.xlsx(RHC.CCU, "base_filtrada.xlsx")

# Excluindo as bases anteriores

rm(RHC2008)
rm(RHC2009)
rm(RHC2010)
rm(RHC2011)
rm(RHC2012)
rm(RHC2013)
rm(RHC2014)
rm(RHC2015)
rm(RHC2016)

# Selecionando apenas as variáveis que iremos trabalhar

RHC.CCU <- read.xlsx("base_filtrada.xlsx")

############################## Câncer do colo do útero##########################

RHC.CCU <- RHC.CCU%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.CCU <- RHC.CCU%>%
  filter(LOCTUDET == "C53")

# Analisando a completude das variáveis

names(RHC.CCU)[1] <- "Sexo"
names(RHC.CCU)[2] <- "Idade"
names(RHC.CCU)[3] <- "Raça/Cor da pele"
names(RHC.CCU)[4] <- "Escolaridade"
names(RHC.CCU)[5] <- "Estado de residência"
names(RHC.CCU)[6] <- "Data da primeira consulta"
names(RHC.CCU)[7] <- "Data de diagnóstico"
names(RHC.CCU)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.CCU)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.CCU)[10] <- "Localização primária do tumor"
names(RHC.CCU)[11] <- "Tipo histológico"
names(RHC.CCU)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.CCU)[13] <- "Lateralidade do tumor"
names(RHC.CCU)[14] <- "Data de início do tratamento"
names(RHC.CCU)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.CCU)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.CCU)[17] <- "Estado da doença ao final do tratamento"
names(RHC.CCU)[18] <- "Data de óbito"
names(RHC.CCU)[19] <- "TNM"

# Corrigindo a classificação das variáveis

RHC.CCU <- RHC.CCU%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         TNM = as.factor(TNM),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))
         
# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.CCU$Sexo)
RHC.CCU$Sexo <- factor(RHC.CCU$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.CCU$Sexo)
RHC.CCU$Sexo[RHC.CCU$Sexo %in% c("0", "3")] <- "9"
RHC.CCU$Sexo <- factor(RHC.CCU$Sexo)
table(RHC.CCU$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.CCU$`Raça/Cor da pele`)
RHC.CCU$`Raça/Cor da pele`[RHC.CCU$`Raça/Cor da pele` == "99"] <- "9"
RHC.CCU$`Raça/Cor da pele` <- factor(RHC.CCU$`Raça/Cor da pele`)
table(RHC.CCU$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.CCU$Escolaridade)
RHC.CCU$Escolaridade <- factor(RHC.CCU$Escolaridade)
RHC.CCU$Escolaridade[RHC.CCU$Escolaridade == "0"] <- "9"
RHC.CCU$Escolaridade <- factor(RHC.CCU$Escolaridade)
table(RHC.CCU$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.CCU$`Estado de residência`)
RHC.CCU$`Estado de residência` <- factor(RHC.CCU$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                        "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                        "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.CCU$`Estado de residência`)
RHC.CCU$`Estado de residência`[RHC.CCU$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.CCU$`Estado de residência` <- factor(RHC.CCU$`Estado de residência`)
table(RHC.CCU$`Estado de residência`)

# Idade

RHC.CCU$Idade <- as.numeric(RHC.CCU$Idade)
table(RHC.CCU$Idade)
levels(RHC.CCU$Idade)

RHC.CCU$Idade <- factor(RHC.CCU$Idade, levels = c("-59", "0", "1", "4", "7", "9", "10", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                                                      "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", 
                                                      "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", 
                                                      "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", 
                                                      "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100","101", "102", "103", "104",
                                                      "105","106", "107", "108", "116", "118", "999"))

table(RHC.CCU$Idade)                                                     
RHC.CCU$Idade[RHC.CCU$Idade == "-59"] <- "999"
RHC.CCU$Idade <- as.numeric(RHC.CCU$Idade)
table(RHC.CCU$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.CCU$`Data da primeira consulta` <- as.Date(RHC.CCU$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.CCU$`Data da primeira consulta`)
table(RHC.CCU$`Data da primeira consulta`)
levels(RHC.CCU$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.CCU$`Data de diagnóstico` <- as.Date(RHC.CCU$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.CCU$`Data de diagnóstico`)
table(RHC.CCU$`Data de diagnóstico`) #NA
levels(RHC.CCU$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.CCU$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU$`Diagnóstico e tratamento anteriores`)
RHC.CCU$`Diagnóstico e tratamento anteriores`[RHC.CCU$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.CCU$`Diagnóstico e tratamento anteriores` <- factor(RHC.CCU$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.CCU$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU$`Base mais importante para o diagnóstico do tumor`)
RHC.CCU$`Base mais importante para o diagnóstico do tumor`[RHC.CCU$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.CCU$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.CCU$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.CCU$`Tipo histológico`) 
table(RHC.CCU$`Tipo histológico`)
RHC.CCU$`Tipo histológico`[RHC.CCU$`Tipo histológico` %in% c("8000/2", "8077/3", "8090/3", "9990/3")] <- "9999/9"
RHC.CCU$`Tipo histológico` <- factor(RHC.CCU$`Tipo histológico`)
table(RHC.CCU$`Tipo histológico`)

# Estadiamento

summary(RHC.CCU$`Estadiamento clínico do tumor (TNM)`) 
RHC.CCU$`Estadiamento clínico do tumor (TNM)`[RHC.CCU$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98")] <- "99"
RHC.CCU$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.CCU$`Estadiamento clínico do tumor (TNM)`)
table(RHC.CCU$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.CCU$TNM <- as.factor(RHC.CCU$TNM)
summary(RHC.CCU$TNM)
RHC.CCU$TNM[RHC.CCU$TNM %in% c("9", "00", "01", "1", "10", "21", "23", "3", "30", "31",
                                                                 "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.CCU$TNM)

# 999 Sem informação

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.CCU$`Lateralidade do tumor`) 
table(RHC.CCU$`Lateralidade do tumor`)
RHC.CCU$`Lateralidade do tumor`[RHC.CCU$`Lateralidade do tumor` == "0"] <- "9"
RHC.CCU$`Lateralidade do tumor` <- factor(RHC.CCU$`Lateralidade do tumor`)
table(RHC.CCU$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.CCU$`Data de início do tratamento` <- as.Date(RHC.CCU$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.CCU$`Data de início do tratamento`)
table(RHC.CCU$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.CCU$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.CCU$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.CCU$`Estado da doença ao final do tratamento`) 
table(RHC.CCU$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.CCU$`Data de óbito` <- as.Date(RHC.CCU$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.CCU$`Data de óbito`)
table(RHC.CCU$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.CCU$anoDIAG <- str_sub(RHC.CCU$`Data de diagnóstico`, start = 1, end = 4)
RHC.CCU$anoPRIMEIRACONS <- str_sub(RHC.CCU$`Data da primeira consulta`, start = 1, end = 4)
RHC.CCU$anoINITRATA <- str_sub(RHC.CCU$`Data de início do tratamento`, start = 1, end = 4)
RHC.CCU$anoOBITO <- str_sub(RHC.CCU$`Data de óbito`, start = 1, end = 4)

table(RHC.CCU$anoDIAG)
table(RHC.CCU$anoPRIMEIRACONS)
table(RHC.CCU$anoINITRATA)
table(RHC.CCU$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.CCU <- RHC.CCU%>%
  mutate_if(is.factor, replace_factor_na)

RHC.CCU$`Data da primeira consulta` <- lubridate::as_date(RHC.CCU$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.CCU$Ano <- year(RHC.CCU$`Data da primeira consulta`)
RHC.CCU$Ano
table(RHC.CCU$Ano)
RHC.CCU1 <- RHC.CCU[RHC.CCU$Ano >= 2008 & RHC.CCU$Ano <= 2016,]

# Definindo as categorias

RHC.CCU$Sexo <- ifelse(RHC.CCU$Sexo == "9", "Incompletude", "Completude")
RHC.CCU$`Raça/Cor da pele` <- ifelse(RHC.CCU$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.CCU$Escolaridade <- ifelse(RHC.CCU$Escolaridade == "9", "Incompletude", "Completude")
RHC.CCU$`Estado de residência` <- ifelse(RHC.CCU$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.CCU$Idade <- ifelse(RHC.CCU$Idade == "999", "Incompletude", "Completude")
RHC.CCU$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.CCU$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.CCU$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.CCU$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.CCU$`Tipo histológico`<- ifelse(RHC.CCU$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.CCU$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.CCU$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.CCU$TNM <- ifelse(RHC.CCU$TNM == "999", "Incompletude", "Completude")
RHC.CCU$`Lateralidade do tumor` <- ifelse(RHC.CCU$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.CCU$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.CCU$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.CCU$`Estado da doença ao final do tratamento` <- ifelse(RHC.CCU$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.CCU$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.CCU$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.CCU$`Data da primeira consulta` <- ifelse(is.na(RHC.CCU$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.CCU$`Data de diagnóstico` <- ifelse(is.na(RHC.CCU$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.CCU$`Data de início do tratamento` <- ifelse(is.na(RHC.CCU$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.CCU$`Data de óbito` <- ifelse(RHC.CCU$`Data de óbito` == "0010-02-03","Incompletude", "Completude")

rhc.completude <- RHC.CCU%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]

colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                              caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer do colo do útero" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude

# ANALISE DE INCONSISTENCIA #####################################################

RHC.CCU <- read.xlsx("base_filtrada.xlsx")

RHC.CCU <- RHC.CCU%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.CCU <- RHC.CCU%>%
  filter(LOCTUDET == "C53")

# Analisando a completude das variáveis

names(RHC.CCU)[1] <- "Sexo"
names(RHC.CCU)[2] <- "Idade"
names(RHC.CCU)[3] <- "Raça/Cor da pele"
names(RHC.CCU)[4] <- "Escolaridade"
names(RHC.CCU)[5] <- "Estado de residência"
names(RHC.CCU)[6] 
<- "Data da primeira consulta"
names(RHC.CCU)[7] <- "Data de diagnóstico"
names(RHC.CCU)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.CCU)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.CCU)[10] <- "Localização primária do tumor"
names(RHC.CCU)[11] <- "Tipo histológico"
names(RHC.CCU)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.CCU)[13] <- "Lateralidade do tumor"
names(RHC.CCU)[14] <- "Data de início do tratamento"
names(RHC.CCU)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.CCU)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.CCU)[17] <- "Estado da doença ao final do tratamento"
names(RHC.CCU)[18] <- "Data de óbito"
names(RHC.CCU)[19] <- "TNM"

library(lubridate)

RHC.CCU$`Data da primeira consulta` <- lubridate::as_date(RHC.CCU$`Data da primeira consulta`, format = "%d/%m/%Y")
RHC.CCU$Ano <- year(RHC.CCU$`Data da primeira consulta`)
RHC.CCU$`Data de início do tratamento` <- as.Date(RHC.CCU$`Data de início do tratamento`, format = "%d/%m/%Y")
RHC.CCU$`Data de diagnóstico` <- as.Date(RHC.CCU$`Data de diagnóstico`, format = "%d/%m/%Y")

# BASE COM APENAS AS VARIÁVEIS DESEJADAS 

AUX.CCU <- RHC.CCU%>%
  select(`Data da primeira consulta`, `Data de diagnóstico`, `Data de início do tratamento`, `Sexo`,
         `Localização primária do tumor`, `Tipo histológico`, `TNM`,`Estadiamento clínico do tumor (TNM)`, Ano)

table(AUX.CCU$`Estadiamento clínico do tumor (TNM)`)
# DATA DA PRIMEIRA CONSULTA E DATA DE DIAGNOSTICO

DTPC_DTDIAG <- AUX.CCU%>%
  filter(`Data da primeira consulta` < `Data de diagnóstico`)%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

# DATA DA PRIMEIRA CONSULTA E DATA DE INICIO DO TRATAMENTO

DTPC_DTINIT <- AUX.CCU%>%
  filter(`Data da primeira consulta` > `Data de início do tratamento`)%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

DTPC_DTINIT$Total <- rep("97022")
DTPC_DTINIT$Total <- as.numeric(DTPC_DTINIT$Total)
DTPC_DTINIT$`% Inconsistência` <- (DTPC_DTINIT$n / (DTPC_DTINIT$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`


DTPC_DTINIT <- DTPC_DTINIT%>%
  pivot_wider(names_from = Ano,
              values_from = n)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

# DATA DE DIAGNOSTICO E DATA DE INICIO DO TRATAMENTO

DTDIAG_DTINIT <- AUX.CCU%>%
  filter(`Data de diagnóstico` > `Data de início do tratamento`)%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

# SEXO E LOCALIZACAO

SEXO_LOCALIZACAO <- AUX.CCU%>%
  filter(Sexo == "1" & `Localização primária do tumor` == "C53")%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

# LOCALIZACAO E TIPO HISTOLOGICO

LOCALIZACAO_HISTOLOGIA <- AUX.CCU%>%
  filter(!`Tipo histológico` %in% c("8070/3",
                                    "8071/3",
                                    "8072/3",
                                    "8083/3",
                                    "8051/3",
                                    "8051/3",
                                    "8052/3",
                                    "8082/3",
                                    "8120/3",
                                    "8076/3",
                                    "8077/2",
                                    "8070/2",
                                    "8052/0",
                                    "8140/3",
                                    "8480/3",
                                    "8482/3",
                                    "8144/3",
                                    "8490/3",
                                    "8480/3",
                                    "8262/3",
                                    "8380/3",
                                    "8310/3",
                                    "8441/3",
                                    "9110/3",
                                    "8140/3",
                                    "8140/2",
                                    "8560/3",
                                    "8015/3",
                                    "8200/3",
                                    "8098/3",
                                    "8240/3",
                                    "8249/3",
                                    "8041/3",
                                    "8013/3",
                                    "8020/3",
                                    "8890/3",
                                    "8931/3",
                                    "8805/3",
                                    "8910/3",
                                    "9581/3",
                                    "9120/3",
                                    "9540/3",
                                    "8890/0",
                                    "8905/0",
                                    "8980/3",
                                    "8933/3",
                                    "8960/3",
                                    "9013/0",
                                    "8932/0",
                                    "8720/3",
                                    "8780/0",
                                    "9071/3",
                                    "9084/0",
                                    "9080/0"))%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

#LOCALIZACAO E LATERALIDADE DO TUMOR

TNM_ESTADIAMENTO <- AUX.CCU%>%
  filter(TNM %in% c("000","001","011","100", "200", "300", "400", "110", "210", "310", "410",
                    "120", "220", "320", "420", "130", "230", "330", "430", "101", "201",
                    "301", "401", "111", "211", "311", "411", "121", "221", "321", "421",
                    "131", "231", "331", "431", "I00"))

TNM_INSITU <- TNM_ESTADIAMENTO%>%
  filter(TNM %in% c("I00", "000"))%>%
  filter(!`Estadiamento clínico do tumor (TNM)` %in% c("0"))%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

TNM_1A <- TNM_ESTADIAMENTO%>%
  filter(TNM %in% c("100"))%>%
  filter(!`Estadiamento clínico do tumor (TNM)` %in% c("1", "1A", "1B"))%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

TNM_2A <- TNM_ESTADIAMENTO%>%
  filter(TNM == "200")%>%
  filter(!`Estadiamento clínico do tumor (TNM)` %in% c("2", "2A", "2B"))%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

TNM_3A <- TNM_ESTADIAMENTO%>%
  filter(TNM == "300")%>%
  filter(!`Estadiamento clínico do tumor (TNM)` %in% c("3", "3A"))%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

TNM_3b <- TNM_ESTADIAMENTO%>%
  filter(TNM %in% c("110", "210", "310"))%>%
  filter(!`Estadiamento clínico do tumor (TNM)` %in% c("3", "3B"))%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

TNM_4A <- TNM_ESTADIAMENTO%>%
  filter(TNM %in% c("400", "410", "420", "430"))%>%
  filter(!`Estadiamento clínico do tumor (TNM)` %in% c("4", "4A"))%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

TNM_4B <- TNM_ESTADIAMENTO%>%
  filter(TNM %in% c("001","011","111", "211", "311", "411", "121", "221", "321", "421",
                    "131", "231", "331", "431"))%>%
  filter(!`Estadiamento clínico do tumor (TNM)` %in% c("4", "4B"))%>%
  group_by(Ano) %>%
  arrange(Ano)%>%
  tally()

TNM_ESTADIAMENTO_FINAL <- bind_rows(TNM_1A, TNM_2A, TNM_3A, TNM_3b, TNM_4A, TNM_4B, TNM_INSITU)

TNM_ESTADIAMENTO_FINAL <- TNM_ESTADIAMENTO_FINAL%>%
  group_by(Ano)
  tally()
  
TNM_ESTADIAMENTO_FINAL <- TNM_ESTADIAMENTO_FINAL %>%
    group_by(Ano) %>%
    summarise(Total = sum(n),
              .groups = "drop_last")

################################## CCU por Regiões ##############################

# Selecionando apenas as variáveis que iremos trabalhar

RHC.CCU <- read.xlsx("base_filtrada.xlsx")

############################## Câncer do colo do útero##########################

RHC.CCU <- RHC.CCU%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM, Ano)

RHC.CCU <- RHC.CCU%>%
  filter(LOCTUDET == "C53")

# Analisando a completude das variáveis

names(RHC.CCU)[1] <- "Sexo"
names(RHC.CCU)[2] <- "Idade"
names(RHC.CCU)[3] <- "Raça/Cor da pele"
names(RHC.CCU)[4] <- "Escolaridade"
names(RHC.CCU)[5] <- "Estado de residência"
names(RHC.CCU)[6] <- "Data da primeira consulta"
names(RHC.CCU)[7] <- "Data de diagnóstico"
names(RHC.CCU)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.CCU)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.CCU)[10] <- "Localização primária do tumor"
names(RHC.CCU)[11] <- "Tipo histológico"
names(RHC.CCU)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.CCU)[13] <- "Lateralidade do tumor"
names(RHC.CCU)[14] <- "Data de início do tratamento"
names(RHC.CCU)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.CCU)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.CCU)[17] <- "Estado da doença ao final do tratamento"
names(RHC.CCU)[18] <- "Data de óbito"
names(RHC.CCU)[19] <- "TNM"


table(RHC.CCU$`Estado de residência`)

levels(RHC.CCU$`Estado de residência`)
RHC.CCU$`Estado de residência` <- as.factor(RHC.CCU$`Estado de residência`)

RHC.CCU$Regioes = ""
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AC")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AM")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RR")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AP")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PA")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "TO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PI")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "CE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RN")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PB")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AL")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "BA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SP")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RJ")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "ES")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MG")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PR")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RS")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SC")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "DF")] = "Centro-oeste"

table(RHC.CCU$Regioes)

# Região Norte

RHC.CCU.NORTE <- RHC.CCU%>%
  filter(Regioes == "Norte")

# Corrigindo a classificação das variáveis

RHC.CCU.NORTE <- RHC.CCU.NORTE%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         TNM = as.factor(TNM),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.CCU.NORTE$Sexo)
RHC.CCU.NORTE$Sexo <- factor(RHC.CCU.NORTE$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.CCU.NORTE$Sexo)
RHC.CCU.NORTE$Sexo[RHC.CCU.NORTE$Sexo %in% c("0", "3")] <- "9"
RHC.CCU.NORTE$Sexo <- factor(RHC.CCU.NORTE$Sexo)
table(RHC.CCU.NORTE$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.CCU.NORTE$`Raça/Cor da pele`)
RHC.CCU.NORTE$`Raça/Cor da pele`[RHC.CCU.NORTE$`Raça/Cor da pele` == "99"] <- "9"
RHC.CCU.NORTE$`Raça/Cor da pele` <- factor(RHC.CCU.NORTE$`Raça/Cor da pele`)
table(RHC.CCU.NORTE$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.CCU.NORTE$Escolaridade)
RHC.CCU.NORTE$Escolaridade <- factor(RHC.CCU.NORTE$Escolaridade)
RHC.CCU.NORTE$Escolaridade[RHC.CCU.NORTE$Escolaridade == "0"] <- "9"
RHC.CCU.NORTE$Escolaridade <- factor(RHC.CCU.NORTE$Escolaridade)
table(RHC.CCU.NORTE$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.CCU.NORTE$`Estado de residência`)
RHC.CCU.NORTE$`Estado de residência` <- factor(RHC.CCU.NORTE$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                                "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                                "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.CCU.NORTE$`Estado de residência`)
RHC.CCU.NORTE$`Estado de residência`[RHC.CCU.NORTE$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.CCU.NORTE$`Estado de residência` <- factor(RHC.CCU.NORTE$`Estado de residência`)
table(RHC.CCU.NORTE$`Estado de residência`)

# Idade

RHC.CCU.NORTE$Idade <- as.numeric(RHC.CCU.NORTE$Idade)
table(RHC.CCU.NORTE$Idade)
levels(RHC.CCU.NORTE$Idade)

RHC.CCU.NORTE$Idade <- factor(RHC.CCU.NORTE$Idade, levels = c("-59", "0", "1", "4", "7", "9", "10", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                                                              "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", 
                                                              "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", 
                                                              "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", 
                                                              "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100","101", "102", "103", "104",
                                                              "105","106", "107", "108", "116", "118", "999"))

table(RHC.CCU.NORTE$Idade)                                                     
RHC.CCU.NORTE$Idade[RHC.CCU.NORTE$Idade == "-59"] <- "999"
RHC.CCU.NORTE$Idade <- as.numeric(RHC.CCU.NORTE$Idade)
table(RHC.CCU.NORTE$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.CCU.NORTE$`Data da primeira consulta` <- as.Date(RHC.CCU.NORTE$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.CCU.NORTE$`Data da primeira consulta`)
table(RHC.CCU.NORTE$`Data da primeira consulta`)
levels(RHC.CCU.NORTE$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.CCU.NORTE$`Data de diagnóstico` <- as.Date(RHC.CCU.NORTE$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.CCU.NORTE$`Data de diagnóstico`)
table(RHC.CCU.NORTE$`Data de diagnóstico`) #NA
levels(RHC.CCU.NORTE$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.CCU.NORTE$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.NORTE$`Diagnóstico e tratamento anteriores`)
RHC.CCU.NORTE$`Diagnóstico e tratamento anteriores`[RHC.CCU.NORTE$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.CCU.NORTE$`Diagnóstico e tratamento anteriores` <- factor(RHC.CCU.NORTE$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.NORTE$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.CCU.NORTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.NORTE$`Base mais importante para o diagnóstico do tumor`)
RHC.CCU.NORTE$`Base mais importante para o diagnóstico do tumor`[RHC.CCU.NORTE$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.CCU.NORTE$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.CCU.NORTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.NORTE$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.CCU.NORTE$`Tipo histológico`) 
table(RHC.CCU.NORTE$`Tipo histológico`)
RHC.CCU.NORTE$`Tipo histológico`[RHC.CCU.NORTE$`Tipo histológico` %in% c("8000/2", "8077/3", "8090/3", "9990/3")] <- "9999/9"
RHC.CCU.NORTE$`Tipo histológico` <- factor(RHC.CCU.NORTE$`Tipo histológico`)
table(RHC.CCU.NORTE$`Tipo histológico`)

# Estadiamento

summary(RHC.CCU.NORTE$`Estadiamento clínico do tumor (TNM)`) 
RHC.CCU.NORTE$`Estadiamento clínico do tumor (TNM)`[RHC.CCU.NORTE$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98")] <- "99"
RHC.CCU.NORTE$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.CCU.NORTE$`Estadiamento clínico do tumor (TNM)`)
table(RHC.CCU.NORTE$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.CCU.NORTE$TNM <- as.factor(RHC.CCU.NORTE$TNM)
summary(RHC.CCU.NORTE$TNM)
RHC.CCU.NORTE$TNM[RHC.CCU.NORTE$TNM %in% c("9", "00", "01", "1", "10", "21", "23", "3", "30", "31",
                               "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.CCU.NORTE$TNM)

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.CCU.NORTE$`Lateralidade do tumor`) 
table(RHC.CCU.NORTE$`Lateralidade do tumor`)
RHC.CCU.NORTE$`Lateralidade do tumor`[RHC.CCU.NORTE$`Lateralidade do tumor` == "0"] <- "9"
RHC.CCU.NORTE$`Lateralidade do tumor` <- factor(RHC.CCU.NORTE$`Lateralidade do tumor`)
table(RHC.CCU.NORTE$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.CCU.NORTE$`Data de início do tratamento` <- as.Date(RHC.CCU.NORTE$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.CCU.NORTE$`Data de início do tratamento`)
table(RHC.CCU.NORTE$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.CCU.NORTE$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.CCU.NORTE$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.CCU.NORTE$`Estado da doença ao final do tratamento`) 
table(RHC.CCU.NORTE$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.CCU.NORTE$`Data de óbito` <- as.Date(RHC.CCU.NORTE$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.CCU.NORTE$`Data de óbito`)
table(RHC.CCU.NORTE$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.CCU.NORTE$anoDIAG <- str_sub(RHC.CCU.NORTE$`Data de diagnóstico`, start = 1, end = 4)
RHC.CCU.NORTE$anoPRIMEIRACONS <- str_sub(RHC.CCU.NORTE$`Data da primeira consulta`, start = 1, end = 4)
RHC.CCU.NORTE$anoINITRATA <- str_sub(RHC.CCU.NORTE$`Data de início do tratamento`, start = 1, end = 4)
RHC.CCU.NORTE$anoOBITO <- str_sub(RHC.CCU.NORTE$`Data de óbito`, start = 1, end = 4)

table(RHC.CCU.NORTE$anoDIAG)
table(RHC.CCU.NORTE$anoPRIMEIRACONS)
table(RHC.CCU.NORTE$anoINITRATA)
table(RHC.CCU.NORTE$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.CCU.NORTE <- RHC.CCU.NORTE%>%
  mutate_if(is.factor, replace_factor_na)

RHC.CCU.NORTE$`Data da primeira consulta` <- lubridate::as_date(RHC.CCU.NORTE$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.CCU.NORTE$Ano <- year(RHC.CCU.NORTE$`Data da primeira consulta`)
RHC.CCU.NORTE$Ano
table(RHC.CCU.NORTE$Ano)
RHC.CCU.NORTE1 <- RHC.CCU.NORTE[RHC.CCU.NORTE$Ano >= 2008 & RHC.CCU.NORTE$Ano <= 2016,]

# Definindo as categorias

RHC.CCU.NORTE$Sexo <- ifelse(RHC.CCU.NORTE$Sexo == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$`Raça/Cor da pele` <- ifelse(RHC.CCU.NORTE$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$Escolaridade <- ifelse(RHC.CCU.NORTE$Escolaridade == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$`Estado de residência` <- ifelse(RHC.CCU.NORTE$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$Idade <- ifelse(RHC.CCU.NORTE$Idade == "999", "Incompletude", "Completude")
RHC.CCU.NORTE$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.CCU.NORTE$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.CCU.NORTE$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$`Tipo histológico`<- ifelse(RHC.CCU.NORTE$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.CCU.NORTE$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.CCU.NORTE$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.CCU.NORTE$TNM <- ifelse(RHC.CCU.NORTE$TNM == "999", "Incompletude", "Completude")
RHC.CCU.NORTE$`Lateralidade do tumor` <- ifelse(RHC.CCU.NORTE$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.CCU.NORTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$`Estado da doença ao final do tratamento` <- ifelse(RHC.CCU.NORTE$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.CCU.NORTE$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.NORTE$`Data da primeira consulta` <- ifelse(is.na(RHC.CCU.NORTE$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.CCU.NORTE$`Data de diagnóstico` <- ifelse(is.na(RHC.CCU.NORTE$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.CCU.NORTE$`Data de início do tratamento` <- ifelse(is.na(RHC.CCU.NORTE$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.CCU.NORTE$`Data de óbito` <- ifelse(RHC.CCU.NORTE$`Data de óbito` == "0010-02-03","Incompletude", "Completude")

rhc.completude <- RHC.CCU.NORTE%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]

colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.RGNORTE.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.norte <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                               caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer do colo do útero" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.norte

# Região Nordeste

# Selecionando apenas as variáveis que iremos trabalhar

RHC.CCU <- read.xlsx("base_filtrada.xlsx")

############################## Câncer do colo do útero##########################

RHC.CCU <- RHC.CCU%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.CCU <- RHC.CCU%>%
  filter(LOCTUDET == "C53")

# Analisando a completude das variáveis

names(RHC.CCU)[1] <- "Sexo"
names(RHC.CCU)[2] <- "Idade"
names(RHC.CCU)[3] <- "Raça/Cor da pele"
names(RHC.CCU)[4] <- "Escolaridade"
names(RHC.CCU)[5] <- "Estado de residência"
names(RHC.CCU)[6] <- "Data da primeira consulta"
names(RHC.CCU)[7] <- "Data de diagnóstico"
names(RHC.CCU)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.CCU)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.CCU)[10] <- "Localização primária do tumor"
names(RHC.CCU)[11] <- "Tipo histológico"
names(RHC.CCU)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.CCU)[13] <- "Lateralidade do tumor"
names(RHC.CCU)[14] <- "Data de início do tratamento"
names(RHC.CCU)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.CCU)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.CCU)[17] <- "Estado da doença ao final do tratamento"
names(RHC.CCU)[18] <- "Data de óbito"
names(RHC.CCU)[19] <- "TNM"

table(RHC.CCU$`Estado de residência`)

levels(RHC.CCU$`Estado de residência`)
RHC.CCU$`Estado de residência` <- as.factor(RHC.CCU$`Estado de residência`)

RHC.CCU$Regioes = ""
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AC")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AM")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RR")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AP")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PA")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "TO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PI")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "CE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RN")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PB")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AL")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "BA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SP")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RJ")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "ES")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MG")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PR")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RS")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SC")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "DF")] = "Centro-oeste"

table(RHC.CCU$Regioes)

RHC.CCU.NORDESTE <- RHC.CCU%>%
  filter(Regioes == "Nordeste")

# Corrigindo a classificação das variáveis

RHC.CCU.NORDESTE <- RHC.CCU.NORDESTE%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         TNM = as.factor(TNM),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.CCU.NORDESTE$Sexo)
RHC.CCU.NORDESTE$Sexo <- factor(RHC.CCU.NORDESTE$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.CCU.NORDESTE$Sexo)
RHC.CCU.NORDESTE$Sexo[RHC.CCU.NORDESTE$Sexo %in% c("0", "3")] <- "9"
RHC.CCU.NORDESTE$Sexo <- factor(RHC.CCU.NORDESTE$Sexo)
table(RHC.CCU.NORDESTE$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.CCU.NORDESTE$`Raça/Cor da pele`)
RHC.CCU.NORDESTE$`Raça/Cor da pele`[RHC.CCU.NORDESTE$`Raça/Cor da pele` == "99"] <- "9"
RHC.CCU.NORDESTE$`Raça/Cor da pele` <- factor(RHC.CCU.NORDESTE$`Raça/Cor da pele`)
table(RHC.CCU.NORDESTE$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.CCU.NORDESTE$Escolaridade)
RHC.CCU.NORDESTE$Escolaridade <- factor(RHC.CCU.NORDESTE$Escolaridade)
RHC.CCU.NORDESTE$Escolaridade[RHC.CCU.NORDESTE$Escolaridade == "0"] <- "9"
RHC.CCU.NORDESTE$Escolaridade <- factor(RHC.CCU.NORDESTE$Escolaridade)
table(RHC.CCU.NORDESTE$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.CCU.NORDESTE$`Estado de residência`)
RHC.CCU.NORDESTE$`Estado de residência` <- factor(RHC.CCU.NORDESTE$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                                      "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                                      "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.CCU.NORDESTE$`Estado de residência`)
RHC.CCU.NORDESTE$`Estado de residência`[RHC.CCU.NORDESTE$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.CCU.NORDESTE$`Estado de residência` <- factor(RHC.CCU.NORDESTE$`Estado de residência`)
table(RHC.CCU.NORDESTE$`Estado de residência`)

# Idade

RHC.CCU.NORDESTE$Idade <- as.numeric(RHC.CCU.NORDESTE$Idade)
table(RHC.CCU.NORDESTE$Idade)
levels(RHC.CCU.NORDESTE$Idade)

RHC.CCU.NORDESTE$Idade <- factor(RHC.CCU.NORDESTE$Idade, levels = c("-59", "0", "1", "4", "7", "9", "10", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                                                                    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", 
                                                                    "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", 
                                                                    "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", 
                                                                    "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100","101", "102", "103", "104",
                                                                    "105","106", "107", "108", "116", "118", "999"))

table(RHC.CCU.NORDESTE$Idade)                                                     
RHC.CCU.NORDESTE$Idade[RHC.CCU.NORDESTE$Idade == "-59"] <- "999"
RHC.CCU.NORDESTE$Idade <- as.numeric(RHC.CCU.NORDESTE$Idade)
table(RHC.CCU.NORDESTE$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.CCU.NORDESTE$`Data da primeira consulta` <- as.Date(RHC.CCU.NORDESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.CCU.NORDESTE$`Data da primeira consulta`)
table(RHC.CCU.NORDESTE$`Data da primeira consulta`)
levels(RHC.CCU.NORDESTE$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.CCU.NORDESTE$`Data de diagnóstico` <- as.Date(RHC.CCU.NORDESTE$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.CCU.NORDESTE$`Data de diagnóstico`)
table(RHC.CCU.NORDESTE$`Data de diagnóstico`) #NA
levels(RHC.CCU.NORDESTE$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.CCU.NORDESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.NORDESTE$`Diagnóstico e tratamento anteriores`)
RHC.CCU.NORDESTE$`Diagnóstico e tratamento anteriores`[RHC.CCU.NORDESTE$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.CCU.NORDESTE$`Diagnóstico e tratamento anteriores` <- factor(RHC.CCU.NORDESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.NORDESTE$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.CCU.NORDESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.NORDESTE$`Base mais importante para o diagnóstico do tumor`)
RHC.CCU.NORDESTE$`Base mais importante para o diagnóstico do tumor`[RHC.CCU.NORDESTE$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.CCU.NORDESTE$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.CCU.NORDESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.NORDESTE$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.CCU.NORDESTE$`Tipo histológico`) 
table(RHC.CCU.NORDESTE$`Tipo histológico`)
RHC.CCU.NORDESTE$`Tipo histológico`[RHC.CCU.NORDESTE$`Tipo histológico` %in% c("8000/2", "8077/3", "8090/3", "9990/3")] <- "9999/9"

RHC.CCU.NORDESTE$`Tipo histológico` <- factor(RHC.CCU.NORDESTE$`Tipo histológico`)
table(RHC.CCU.NORDESTE$`Tipo histológico`)

# Estadiamento

summary(RHC.CCU.NORDESTE$`Estadiamento clínico do tumor (TNM)`) 
RHC.CCU.NORDESTE$`Estadiamento clínico do tumor (TNM)`[RHC.CCU.NORDESTE$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98")] <- "99"
RHC.CCU.NORDESTE$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.CCU.NORDESTE$`Estadiamento clínico do tumor (TNM)`)
table(RHC.CCU.NORDESTE$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.CCU.NORDESTE$TNM <- as.factor(RHC.CCU.NORDESTE$TNM)
summary(RHC.CCU.NORDESTE$TNM)
RHC.CCU.NORDESTE$TNM[RHC.CCU.NORDESTE$TNM %in% c("9", "00", "01", "1", "10", "21", "23", "3", "30", "31",
                               "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.CCU.NORDESTE$TNM)

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.CCU.NORDESTE$`Lateralidade do tumor`) 
table(RHC.CCU.NORDESTE$`Lateralidade do tumor`)
RHC.CCU.NORDESTE$`Lateralidade do tumor`[RHC.CCU.NORDESTE$`Lateralidade do tumor` == "0"] <- "9"
RHC.CCU.NORDESTE$`Lateralidade do tumor` <- factor(RHC.CCU.NORDESTE$`Lateralidade do tumor`)
table(RHC.CCU.NORDESTE$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.CCU.NORDESTE$`Data de início do tratamento` <- as.Date(RHC.CCU.NORDESTE$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.CCU.NORDESTE$`Data de início do tratamento`)
table(RHC.CCU.NORDESTE$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.CCU.NORDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.CCU.NORDESTE$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.CCU.NORDESTE$`Estado da doença ao final do tratamento`) 
table(RHC.CCU.NORDESTE$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.CCU.NORDESTE$`Data de óbito` <- as.Date(RHC.CCU.NORDESTE$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.CCU.NORDESTE$`Data de óbito`)
table(RHC.CCU.NORDESTE$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.CCU.NORDESTE$anoDIAG <- str_sub(RHC.CCU.NORDESTE$`Data de diagnóstico`, start = 1, end = 4)
RHC.CCU.NORDESTE$anoPRIMEIRACONS <- str_sub(RHC.CCU.NORDESTE$`Data da primeira consulta`, start = 1, end = 4)
RHC.CCU.NORDESTE$anoINITRATA <- str_sub(RHC.CCU.NORDESTE$`Data de início do tratamento`, start = 1, end = 4)
RHC.CCU.NORDESTE$anoOBITO <- str_sub(RHC.CCU.NORDESTE$`Data de óbito`, start = 1, end = 4)

table(RHC.CCU.NORDESTE$anoDIAG)
table(RHC.CCU.NORDESTE$anoPRIMEIRACONS)
table(RHC.CCU.NORDESTE$anoINITRATA)
table(RHC.CCU.NORDESTE$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.CCU.NORDESTE <- RHC.CCU.NORDESTE%>%
  mutate_if(is.factor, replace_factor_na)

RHC.CCU.NORDESTE$`Data da primeira consulta` <- lubridate::as_date(RHC.CCU.NORDESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.CCU.NORDESTE$Ano <- year(RHC.CCU.NORDESTE$`Data da primeira consulta`)
RHC.CCU.NORDESTE$Ano
table(RHC.CCU.NORDESTE$Ano)
RHC.CCU.NORDESTE1 <- RHC.CCU.NORDESTE[RHC.CCU.NORDESTE$Ano >= 2008 & RHC.CCU.NORDESTE$Ano <= 2016,]

# Definindo as categorias

RHC.CCU.NORDESTE$Sexo <- ifelse(RHC.CCU.NORDESTE$Sexo == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Raça/Cor da pele` <- ifelse(RHC.CCU.NORDESTE$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$Escolaridade <- ifelse(RHC.CCU.NORDESTE$Escolaridade == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Estado de residência` <- ifelse(RHC.CCU.NORDESTE$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$Idade <- ifelse(RHC.CCU.NORDESTE$Idade == "999", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.CCU.NORDESTE$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.CCU.NORDESTE$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Tipo histológico`<- ifelse(RHC.CCU.NORDESTE$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.CCU.NORDESTE$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.CCU.NORDESTE$TNM <- ifelse(RHC.CCU.NORDESTE$TNM == "999", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Lateralidade do tumor` <- ifelse(RHC.CCU.NORDESTE$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.CCU.NORDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Estado da doença ao final do tratamento` <- ifelse(RHC.CCU.NORDESTE$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.CCU.NORDESTE$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.NORDESTE$`Data da primeira consulta` <- ifelse(is.na(RHC.CCU.NORDESTE$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.CCU.NORDESTE$`Data de diagnóstico` <- ifelse(is.na(RHC.CCU.NORDESTE$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.CCU.NORDESTE$`Data de início do tratamento` <- ifelse(is.na(RHC.CCU.NORDESTE$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.CCU.NORDESTE$`Data de óbito` <- ifelse(RHC.CCU.NORDESTE$`Data de óbito` == "0010-02-03","Incompletude", "Completude")

rhc.completude <- RHC.CCU.NORDESTE%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]


colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)
tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.RGNordeste.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.nordeste <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                               caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer do colo do útero" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.nordeste

# Região Centro-oeste

# Selecionando apenas as variáveis que iremos trabalhar

RHC.CCU <- read.xlsx("base_filtrada.xlsx")

############################## Câncer do colo do útero##########################

RHC.CCU <- RHC.CCU%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.CCU <- RHC.CCU%>%
  filter(LOCTUDET == "C53")

# Analisando a completude das variáveis

names(RHC.CCU)[1] <- "Sexo"
names(RHC.CCU)[2] <- "Idade"
names(RHC.CCU)[3] <- "Raça/Cor da pele"
names(RHC.CCU)[4] <- "Escolaridade"
names(RHC.CCU)[5] <- "Estado de residência"
names(RHC.CCU)[6] <- "Data da primeira consulta"
names(RHC.CCU)[7] <- "Data de diagnóstico"
names(RHC.CCU)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.CCU)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.CCU)[10] <- "Localização primária do tumor"
names(RHC.CCU)[11] <- "Tipo histológico"
names(RHC.CCU)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.CCU)[13] <- "Lateralidade do tumor"
names(RHC.CCU)[14] <- "Data de início do tratamento"
names(RHC.CCU)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.CCU)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.CCU)[17] <- "Estado da doença ao final do tratamento"
names(RHC.CCU)[18] <- "Data de óbito"
names(RHC.CCU)[19] <- "TNM"

table(RHC.CCU$`Estado de residência`)

levels(RHC.CCU$`Estado de residência`)
RHC.CCU$`Estado de residência` <- as.factor(RHC.CCU$`Estado de residência`)

RHC.CCU$Regioes = ""
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AC")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AM")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RR")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AP")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PA")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "TO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PI")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "CE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RN")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PB")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AL")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "BA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SP")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RJ")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "ES")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MG")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PR")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RS")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SC")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "DF")] = "Centro-oeste"

table(RHC.CCU$Regioes)

RHC.CCU.CENTRO.OESTE <- RHC.CCU%>%
  filter(Regioes == "Centro-oeste")

# Corrigindo a classificação das variáveis

RHC.CCU.CENTRO.OESTE <- RHC.CCU.CENTRO.OESTE%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         TNM = as.factor(TNM),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.CCU.CENTRO.OESTE$Sexo)
RHC.CCU.CENTRO.OESTE$Sexo <- factor(RHC.CCU.CENTRO.OESTE$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.CCU.CENTRO.OESTE$Sexo)
RHC.CCU.CENTRO.OESTE$Sexo[RHC.CCU.CENTRO.OESTE$Sexo %in% c("0", "3")] <- "9"
RHC.CCU.CENTRO.OESTE$Sexo <- factor(RHC.CCU.CENTRO.OESTE$Sexo)
table(RHC.CCU.CENTRO.OESTE$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.CCU.CENTRO.OESTE$`Raça/Cor da pele`)
RHC.CCU.CENTRO.OESTE$`Raça/Cor da pele`[RHC.CCU.CENTRO.OESTE$`Raça/Cor da pele` == "99"] <- "9"
RHC.CCU.CENTRO.OESTE$`Raça/Cor da pele` <- factor(RHC.CCU.CENTRO.OESTE$`Raça/Cor da pele`)
table(RHC.CCU.CENTRO.OESTE$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.CCU.CENTRO.OESTE$Escolaridade)
RHC.CCU.CENTRO.OESTE$Escolaridade <- factor(RHC.CCU.CENTRO.OESTE$Escolaridade)
RHC.CCU.CENTRO.OESTE$Escolaridade[RHC.CCU.CENTRO.OESTE$Escolaridade == "0"] <- "9"
RHC.CCU.CENTRO.OESTE$Escolaridade <- factor(RHC.CCU.CENTRO.OESTE$Escolaridade)
table(RHC.CCU.CENTRO.OESTE$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.CCU.CENTRO.OESTE$`Estado de residência`)
RHC.CCU.CENTRO.OESTE$`Estado de residência` <- factor(RHC.CCU.CENTRO.OESTE$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                                              "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                                              "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.CCU.CENTRO.OESTE$`Estado de residência`)
RHC.CCU.CENTRO.OESTE$`Estado de residência`[RHC.CCU.CENTRO.OESTE$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.CCU.CENTRO.OESTE$`Estado de residência` <- factor(RHC.CCU.CENTRO.OESTE$`Estado de residência`)
table(RHC.CCU.CENTRO.OESTE$`Estado de residência`)

# Idade

RHC.CCU.CENTRO.OESTE$Idade <- as.numeric(RHC.CCU.CENTRO.OESTE$Idade)
table(RHC.CCU.CENTRO.OESTE$Idade)
levels(RHC.CCU.CENTRO.OESTE$Idade)

RHC.CCU.CENTRO.OESTE$Idade <- factor(RHC.CCU.CENTRO.OESTE$Idade, levels = c("-59", "0", "1", "4", "7", "9", "10", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                                                                            "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", 
                                                                            "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", 
                                                                            "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", 
                                                                            "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100","101", "102", "103", "104",
                                                                            "105","106", "107", "108", "116", "118", "999"))

table(RHC.CCU.CENTRO.OESTE$Idade)                                                     
RHC.CCU.CENTRO.OESTE$Idade[RHC.CCU.CENTRO.OESTE$Idade == "-59"] <- "999"
RHC.CCU.CENTRO.OESTE$Idade <- as.numeric(RHC.CCU.CENTRO.OESTE$Idade)
table(RHC.CCU.CENTRO.OESTE$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.CCU.CENTRO.OESTE$`Data da primeira consulta` <- as.Date(RHC.CCU.CENTRO.OESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.CCU.CENTRO.OESTE$`Data da primeira consulta`)
table(RHC.CCU.CENTRO.OESTE$`Data da primeira consulta`)
levels(RHC.CCU.CENTRO.OESTE$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.CCU.CENTRO.OESTE$`Data de diagnóstico` <- as.Date(RHC.CCU.CENTRO.OESTE$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.CCU.CENTRO.OESTE$`Data de diagnóstico`)
table(RHC.CCU.CENTRO.OESTE$`Data de diagnóstico`) #NA
levels(RHC.CCU.CENTRO.OESTE$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.CCU.CENTRO.OESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.CENTRO.OESTE$`Diagnóstico e tratamento anteriores`)
RHC.CCU.CENTRO.OESTE$`Diagnóstico e tratamento anteriores`[RHC.CCU.CENTRO.OESTE$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.CCU.CENTRO.OESTE$`Diagnóstico e tratamento anteriores` <- factor(RHC.CCU.CENTRO.OESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.CENTRO.OESTE$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.CCU.CENTRO.OESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.CENTRO.OESTE$`Base mais importante para o diagnóstico do tumor`)
RHC.CCU.CENTRO.OESTE$`Base mais importante para o diagnóstico do tumor`[RHC.CCU.CENTRO.OESTE$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.CCU.CENTRO.OESTE$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.CCU.CENTRO.OESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.CENTRO.OESTE$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.CCU.CENTRO.OESTE$`Tipo histológico`) 
table(RHC.CCU.CENTRO.OESTE$`Tipo histológico`)
RHC.CCU.CENTRO.OESTE$`Tipo histológico`[RHC.CCU.CENTRO.OESTE$`Tipo histológico` %in% c("8000/2", "8077/3", "8090/3", "9990/3")] <- "9999/9"

RHC.CCU.CENTRO.OESTE$`Tipo histológico` <- factor(RHC.CCU.CENTRO.OESTE$`Tipo histológico`)
table(RHC.CCU.CENTRO.OESTE$`Tipo histológico`)

# Estadiamento

summary(RHC.CCU.CENTRO.OESTE$`Estadiamento clínico do tumor (TNM)`) 
RHC.CCU.CENTRO.OESTE$`Estadiamento clínico do tumor (TNM)`[RHC.CCU.CENTRO.OESTE$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98")] <- "99"
RHC.CCU.CENTRO.OESTE$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.CCU.CENTRO.OESTE$`Estadiamento clínico do tumor (TNM)`)
table(RHC.CCU.CENTRO.OESTE$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.CCU.CENTRO.OESTE$TNM <- as.factor(RHC.CCU.CENTRO.OESTE$TNM)
summary(RHC.CCU.CENTRO.OESTE$TNM)
RHC.CCU.CENTRO.OESTE$TNM[RHC.CCU.CENTRO.OESTE$TNM %in% c("9", "00", "01", "1", "10", "21", "23", "3", "30", "31",
                               "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.CCU.CENTRO.OESTE$TNM)

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.CCU.CENTRO.OESTE$`Lateralidade do tumor`) 
table(RHC.CCU.CENTRO.OESTE$`Lateralidade do tumor`)
RHC.CCU.CENTRO.OESTE$`Lateralidade do tumor`[RHC.CCU.CENTRO.OESTE$`Lateralidade do tumor` == "0"] <- "9"
RHC.CCU.CENTRO.OESTE$`Lateralidade do tumor` <- factor(RHC.CCU.CENTRO.OESTE$`Lateralidade do tumor`)
table(RHC.CCU.CENTRO.OESTE$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.CCU.CENTRO.OESTE$`Data de início do tratamento` <- as.Date(RHC.CCU.CENTRO.OESTE$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.CCU.CENTRO.OESTE$`Data de início do tratamento`)
table(RHC.CCU.CENTRO.OESTE$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.CCU.CENTRO.OESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.CCU.CENTRO.OESTE$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.CCU.CENTRO.OESTE$`Estado da doença ao final do tratamento`) 
table(RHC.CCU.CENTRO.OESTE$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.CCU.CENTRO.OESTE$`Data de óbito` <- as.Date(RHC.CCU.CENTRO.OESTE$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.CCU.CENTRO.OESTE$`Data de óbito`)
table(RHC.CCU.CENTRO.OESTE$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.CCU.CENTRO.OESTE$anoDIAG <- str_sub(RHC.CCU.CENTRO.OESTE$`Data de diagnóstico`, start = 1, end = 4)
RHC.CCU.CENTRO.OESTE$anoPRIMEIRACONS <- str_sub(RHC.CCU.CENTRO.OESTE$`Data da primeira consulta`, start = 1, end = 4)
RHC.CCU.CENTRO.OESTE$anoINITRATA <- str_sub(RHC.CCU.CENTRO.OESTE$`Data de início do tratamento`, start = 1, end = 4)
RHC.CCU.CENTRO.OESTE$anoOBITO <- str_sub(RHC.CCU.CENTRO.OESTE$`Data de óbito`, start = 1, end = 4)

table(RHC.CCU.CENTRO.OESTE$anoDIAG)
table(RHC.CCU.CENTRO.OESTE$anoPRIMEIRACONS)
table(RHC.CCU.CENTRO.OESTE$anoINITRATA)
table(RHC.CCU.CENTRO.OESTE$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.CCU.CENTRO.OESTE <- RHC.CCU.CENTRO.OESTE%>%
  mutate_if(is.factor, replace_factor_na)

RHC.CCU.CENTRO.OESTE$`Data da primeira consulta` <- lubridate::as_date(RHC.CCU.CENTRO.OESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.CCU.CENTRO.OESTE$Ano <- year(RHC.CCU.CENTRO.OESTE$`Data da primeira consulta`)
RHC.CCU.CENTRO.OESTE$Ano
table(RHC.CCU.CENTRO.OESTE$Ano)
RHC.CCU.CENTRO.OESTE1 <- RHC.CCU.CENTRO.OESTE[RHC.CCU.CENTRO.OESTE$Ano >= 2008 & RHC.CCU.CENTRO.OESTE$Ano <= 2016,]

# Definindo as categorias

RHC.CCU.CENTRO.OESTE$Sexo <- ifelse(RHC.CCU.CENTRO.OESTE$Sexo == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Raça/Cor da pele` <- ifelse(RHC.CCU.CENTRO.OESTE$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$Escolaridade <- ifelse(RHC.CCU.CENTRO.OESTE$Escolaridade == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Estado de residência` <- ifelse(RHC.CCU.CENTRO.OESTE$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$Idade <- ifelse(RHC.CCU.CENTRO.OESTE$Idade == "999", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.CCU.CENTRO.OESTE$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.CCU.CENTRO.OESTE$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Tipo histológico`<- ifelse(RHC.CCU.CENTRO.OESTE$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.CCU.CENTRO.OESTE$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$TNM <- ifelse(RHC.CCU.CENTRO.OESTE$TNM == "999", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Lateralidade do tumor` <- ifelse(RHC.CCU.CENTRO.OESTE$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.CCU.CENTRO.OESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Estado da doença ao final do tratamento` <- ifelse(RHC.CCU.CENTRO.OESTE$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.CCU.CENTRO.OESTE$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Data da primeira consulta` <- ifelse(is.na(RHC.CCU.CENTRO.OESTE$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Data de diagnóstico` <- ifelse(is.na(RHC.CCU.CENTRO.OESTE$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Data de início do tratamento` <- ifelse(is.na(RHC.CCU.CENTRO.OESTE$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.CCU.CENTRO.OESTE$`Data de óbito` <- ifelse(RHC.CCU.CENTRO.OESTE$`Data de óbito` == "0010-02-03","Incompletude", "Completude")

rhc.completude <- RHC.CCU.CENTRO.OESTE%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]


colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.RGCO.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.CO <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                               caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer do colo do útero" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.CO

# Sudeste

# Selecionando apenas as variáveis que iremos trabalhar

RHC.CCU <- read.xlsx("base_filtrada.xlsx")

############################## Câncer do colo do útero##########################

RHC.CCU <- RHC.CCU%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.CCU <- RHC.CCU%>%
  filter(LOCTUDET == "C53")

# Analisando a completude das variáveis

names(RHC.CCU)[1] <- "Sexo"
names(RHC.CCU)[2] <- "Idade"
names(RHC.CCU)[3] <- "Raça/Cor da pele"
names(RHC.CCU)[4] <- "Escolaridade"
names(RHC.CCU)[5] <- "Estado de residência"
names(RHC.CCU)[6] <- "Data da primeira consulta"
names(RHC.CCU)[7] <- "Data de diagnóstico"
names(RHC.CCU)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.CCU)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.CCU)[10] <- "Localização primária do tumor"
names(RHC.CCU)[11] <- "Tipo histológico"
names(RHC.CCU)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.CCU)[13] <- "Lateralidade do tumor"
names(RHC.CCU)[14] <- "Data de início do tratamento"
names(RHC.CCU)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.CCU)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.CCU)[17] <- "Estado da doença ao final do tratamento"
names(RHC.CCU)[18] <- "Data de óbito"
names(RHC.CCU)[19] <- "TNM"

table(RHC.CCU$`Estado de residência`)

levels(RHC.CCU$`Estado de residência`)
RHC.CCU$`Estado de residência` <- as.factor(RHC.CCU$`Estado de residência`)

RHC.CCU$Regioes = ""
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AC")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AM")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RR")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AP")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PA")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "TO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PI")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "CE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RN")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PB")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AL")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "BA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SP")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RJ")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "ES")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MG")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PR")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RS")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SC")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "DF")] = "Centro-oeste"

table(RHC.CCU$Regioes)


RHC.CCU.SUDESTE <- RHC.CCU%>%
  filter(Regioes == "Sudeste")

# Corrigindo a classificação das variáveis

RHC.CCU.SUDESTE <- RHC.CCU.SUDESTE%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         TNM = as.factor(TNM),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.CCU.SUDESTE$Sexo)
RHC.CCU.SUDESTE$Sexo <- factor(RHC.CCU.SUDESTE$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.CCU.SUDESTE$Sexo)
RHC.CCU.SUDESTE$Sexo[RHC.CCU.SUDESTE$Sexo %in% c("0", "3")] <- "9"
RHC.CCU.SUDESTE$Sexo <- factor(RHC.CCU.SUDESTE$Sexo)
table(RHC.CCU.SUDESTE$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.CCU.SUDESTE$`Raça/Cor da pele`)
RHC.CCU.SUDESTE$`Raça/Cor da pele`[RHC.CCU.SUDESTE$`Raça/Cor da pele` == "99"] <- "9"
RHC.CCU.SUDESTE$`Raça/Cor da pele` <- factor(RHC.CCU.SUDESTE$`Raça/Cor da pele`)
table(RHC.CCU.SUDESTE$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.CCU.SUDESTE$Escolaridade)
RHC.CCU.SUDESTE$Escolaridade <- factor(RHC.CCU.SUDESTE$Escolaridade)
RHC.CCU.SUDESTE$Escolaridade[RHC.CCU.SUDESTE$Escolaridade == "0"] <- "9"
RHC.CCU.SUDESTE$Escolaridade <- factor(RHC.CCU.SUDESTE$Escolaridade)
table(RHC.CCU.SUDESTE$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.CCU.SUDESTE$`Estado de residência`)
RHC.CCU.SUDESTE$`Estado de residência` <- factor(RHC.CCU.SUDESTE$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                                    "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                                    "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.CCU.SUDESTE$`Estado de residência`)
RHC.CCU.SUDESTE$`Estado de residência`[RHC.CCU.SUDESTE$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.CCU.SUDESTE$`Estado de residência` <- factor(RHC.CCU.SUDESTE$`Estado de residência`)
table(RHC.CCU.SUDESTE$`Estado de residência`)

# Idade

RHC.CCU.SUDESTE$Idade <- as.numeric(RHC.CCU.SUDESTE$Idade)
table(RHC.CCU.SUDESTE$Idade)
levels(RHC.CCU.SUDESTE$Idade)

RHC.CCU.SUDESTE$Idade <- factor(RHC.CCU.SUDESTE$Idade, levels = c("-59", "0", "1", "4", "7", "9", "10", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                                                                  "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", 
                                                                  "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", 
                                                                  "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", 
                                                                  "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100","101", "102", "103", "104",
                                                                  "105","106", "107", "108", "116", "118", "999"))

table(RHC.CCU.SUDESTE$Idade)                                                     
RHC.CCU.SUDESTE$Idade[RHC.CCU.SUDESTE$Idade == "-59"] <- "999"
RHC.CCU.SUDESTE$Idade <- as.numeric(RHC.CCU.SUDESTE$Idade)
table(RHC.CCU.SUDESTE$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.CCU.SUDESTE$`Data da primeira consulta` <- as.Date(RHC.CCU.SUDESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.CCU.SUDESTE$`Data da primeira consulta`)
table(RHC.CCU.SUDESTE$`Data da primeira consulta`)
levels(RHC.CCU.SUDESTE$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.CCU.SUDESTE$`Data de diagnóstico` <- as.Date(RHC.CCU.SUDESTE$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.CCU.SUDESTE$`Data de diagnóstico`)
table(RHC.CCU.SUDESTE$`Data de diagnóstico`) #NA
levels(RHC.CCU.SUDESTE$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.CCU.SUDESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.SUDESTE$`Diagnóstico e tratamento anteriores`)
RHC.CCU.SUDESTE$`Diagnóstico e tratamento anteriores`[RHC.CCU.SUDESTE$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.CCU.SUDESTE$`Diagnóstico e tratamento anteriores` <- factor(RHC.CCU.SUDESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.SUDESTE$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.CCU.SUDESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.SUDESTE$`Base mais importante para o diagnóstico do tumor`)
RHC.CCU.SUDESTE$`Base mais importante para o diagnóstico do tumor`[RHC.CCU.SUDESTE$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.CCU.SUDESTE$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.CCU.SUDESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.SUDESTE$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.CCU.SUDESTE$`Tipo histológico`) 
table(RHC.CCU.SUDESTE$`Tipo histológico`)
RHC.CCU.SUDESTE$`Tipo histológico`[RHC.CCU.SUDESTE$`Tipo histológico` %in% c("8000/2", "8077/3", "8090/3", "9990/3")] <- "9999/9"

RHC.CCU.SUDESTE$`Tipo histológico` <- factor(RHC.CCU.SUDESTE$`Tipo histológico`)
table(RHC.CCU.SUDESTE$`Tipo histológico`)

# Estadiamento

summary(RHC.CCU.SUDESTE$`Estadiamento clínico do tumor (TNM)`) 
RHC.CCU.SUDESTE$`Estadiamento clínico do tumor (TNM)`[RHC.CCU.SUDESTE$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98")] <- "99"
RHC.CCU.SUDESTE$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.CCU.SUDESTE$`Estadiamento clínico do tumor (TNM)`)
table(RHC.CCU.SUDESTE$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.CCU.SUDESTE$TNM <- as.factor(RHC.CCU.SUDESTE$TNM)
summary(RHC.CCU.SUDESTE$TNM)
RHC.CCU.SUDESTE$TNM[RHC.CCU.SUDESTE$TNM %in% c("9", "00", "01", "1", "10", "21", "23", "3", "30", "31",
                               "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.CCU.SUDESTE$TNM)

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.CCU.SUDESTE$`Lateralidade do tumor`) 
table(RHC.CCU.SUDESTE$`Lateralidade do tumor`)
RHC.CCU.SUDESTE$`Lateralidade do tumor`[RHC.CCU.SUDESTE$`Lateralidade do tumor` == "0"] <- "9"
RHC.CCU.SUDESTE$`Lateralidade do tumor` <- factor(RHC.CCU.SUDESTE$`Lateralidade do tumor`)
table(RHC.CCU.SUDESTE$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.CCU.SUDESTE$`Data de início do tratamento` <- as.Date(RHC.CCU.SUDESTE$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.CCU.SUDESTE$`Data de início do tratamento`)
table(RHC.CCU.SUDESTE$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.CCU.SUDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.CCU.SUDESTE$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.CCU.SUDESTE$`Estado da doença ao final do tratamento`) 
table(RHC.CCU.SUDESTE$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.CCU.SUDESTE$`Data de óbito` <- as.Date(RHC.CCU.SUDESTE$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.CCU.SUDESTE$`Data de óbito`)
table(RHC.CCU.SUDESTE$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.CCU.SUDESTE$anoDIAG <- str_sub(RHC.CCU.SUDESTE$`Data de diagnóstico`, start = 1, end = 4)
RHC.CCU.SUDESTE$anoPRIMEIRACONS <- str_sub(RHC.CCU.SUDESTE$`Data da primeira consulta`, start = 1, end = 4)
RHC.CCU.SUDESTE$anoINITRATA <- str_sub(RHC.CCU.SUDESTE$`Data de início do tratamento`, start = 1, end = 4)
RHC.CCU.SUDESTE$anoOBITO <- str_sub(RHC.CCU.SUDESTE$`Data de óbito`, start = 1, end = 4)

table(RHC.CCU.SUDESTE$anoDIAG)
table(RHC.CCU.SUDESTE$anoPRIMEIRACONS)
table(RHC.CCU.SUDESTE$anoINITRATA)
table(RHC.CCU.SUDESTE$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.CCU.SUDESTE <- RHC.CCU.SUDESTE%>%
  mutate_if(is.factor, replace_factor_na)

RHC.CCU.SUDESTE$`Data da primeira consulta` <- lubridate::as_date(RHC.CCU.SUDESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.CCU.SUDESTE$Ano <- year(RHC.CCU.SUDESTE$`Data da primeira consulta`)
RHC.CCU.SUDESTE$Ano
table(RHC.CCU.SUDESTE$Ano)
RHC.CCU.SUDESTE1 <- RHC.CCU.SUDESTE[RHC.CCU.SUDESTE$Ano >= 2008 & RHC.CCU.SUDESTE$Ano <= 2016,]

# Definindo as categorias

RHC.CCU.SUDESTE$Sexo <- ifelse(RHC.CCU.SUDESTE$Sexo == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Raça/Cor da pele` <- ifelse(RHC.CCU.SUDESTE$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$Escolaridade <- ifelse(RHC.CCU.SUDESTE$Escolaridade == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Estado de residência` <- ifelse(RHC.CCU.SUDESTE$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$Idade <- ifelse(RHC.CCU.SUDESTE$Idade == "999", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.CCU.SUDESTE$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.CCU.SUDESTE$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Tipo histológico`<- ifelse(RHC.CCU.SUDESTE$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.CCU.SUDESTE$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.CCU.SUDESTE$TNM <- ifelse(RHC.CCU.SUDESTE$TNM == "999", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Lateralidade do tumor` <- ifelse(RHC.CCU.SUDESTE$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.CCU.SUDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Estado da doença ao final do tratamento` <- ifelse(RHC.CCU.SUDESTE$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.CCU.SUDESTE$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.SUDESTE$`Data da primeira consulta` <- ifelse(is.na(RHC.CCU.SUDESTE$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.CCU.SUDESTE$`Data de diagnóstico` <- ifelse(is.na(RHC.CCU.SUDESTE$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.CCU.SUDESTE$`Data de início do tratamento` <- ifelse(is.na(RHC.CCU.SUDESTE$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.CCU.SUDESTE$`Data de óbito` <- ifelse(RHC.CCU.SUDESTE$`Data de óbito` == "0010-02-03","Incompletude", "Completude")

rhc.completude <- RHC.CCU.SUDESTE%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]


colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.RGSUDESTE.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.SUDESTE <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                               caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer do colo do útero" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.SUDESTE

# Sul

# Selecionando apenas as variáveis que iremos trabalhar

RHC.CCU <- read.xlsx("base_filtrada.xlsx")

############################## Câncer do colo do útero##########################

RHC.CCU <- RHC.CCU%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.CCU <- RHC.CCU%>%
  filter(LOCTUDET == "C53")

# Analisando a completude das variáveis

names(RHC.CCU)[1] <- "Sexo"
names(RHC.CCU)[2] <- "Idade"
names(RHC.CCU)[3] <- "Raça/Cor da pele"
names(RHC.CCU)[4] <- "Escolaridade"
names(RHC.CCU)[5] <- "Estado de residência"
names(RHC.CCU)[6] <- "Data da primeira consulta"
names(RHC.CCU)[7] <- "Data de diagnóstico"
names(RHC.CCU)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.CCU)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.CCU)[10] <- "Localização primária do tumor"
names(RHC.CCU)[11] <- "Tipo histológico"
names(RHC.CCU)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.CCU)[13] <- "Lateralidade do tumor"
names(RHC.CCU)[14] <- "Data de início do tratamento"
names(RHC.CCU)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.CCU)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.CCU)[17] <- "Estado da doença ao final do tratamento"
names(RHC.CCU)[18] <- "Data de óbito"
names(RHC.CCU)[19] <- "TNM"

table(RHC.CCU$`Estado de residência`)

levels(RHC.CCU$`Estado de residência`)
RHC.CCU$`Estado de residência` <- as.factor(RHC.CCU$`Estado de residência`)

RHC.CCU$Regioes = ""
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AC")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AM")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RR")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AP")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PA")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "TO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RO")] = "Norte"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PI")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "CE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RN")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PB")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SE")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "AL")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "BA")] = "Nordeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SP")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RJ")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "ES")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "MG")] = "Sudeste"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "PR")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "RS")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "SC")] = "Sul"
RHC.CCU$Regioes[str_detect(RHC.CCU$`Estado de residência`, "DF")] = "Centro-oeste"

table(RHC.CCU$Regioes)


RHC.CCU.SUL <- RHC.CCU%>%
  filter(Regioes == "Sul")

# Corrigindo a classificação das variáveis

RHC.CCU.SUL <- RHC.CCU.SUL%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         TNM = as.factor(TNM),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.CCU.SUL$Sexo)
RHC.CCU.SUL$Sexo <- factor(RHC.CCU.SUL$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.CCU.SUL$Sexo)
RHC.CCU.SUL$Sexo[RHC.CCU.SUL$Sexo %in% c("0", "3")] <- "9"
RHC.CCU.SUL$Sexo <- factor(RHC.CCU.SUL$Sexo)
table(RHC.CCU.SUL$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.CCU.SUL$`Raça/Cor da pele`)
RHC.CCU.SUL$`Raça/Cor da pele`[RHC.CCU.SUL$`Raça/Cor da pele` == "99"] <- "9"
RHC.CCU.SUL$`Raça/Cor da pele` <- factor(RHC.CCU.SUL$`Raça/Cor da pele`)
table(RHC.CCU.SUL$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.CCU.SUL$Escolaridade)
RHC.CCU.SUL$Escolaridade <- factor(RHC.CCU.SUL$Escolaridade)
RHC.CCU.SUL$Escolaridade[RHC.CCU.SUL$Escolaridade == "0"] <- "9"
RHC.CCU.SUL$Escolaridade <- factor(RHC.CCU.SUL$Escolaridade)
table(RHC.CCU.SUL$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.CCU.SUL$`Estado de residência`)
RHC.CCU.SUL$`Estado de residência` <- factor(RHC.CCU.SUL$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                            "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                            "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.CCU.SUL$`Estado de residência`)
RHC.CCU.SUL$`Estado de residência`[RHC.CCU.SUL$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.CCU.SUL$`Estado de residência` <- factor(RHC.CCU.SUL$`Estado de residência`)
table(RHC.CCU.SUL$`Estado de residência`)

# Idade

RHC.CCU.SUL$Idade <- as.numeric(RHC.CCU.SUL$Idade)
table(RHC.CCU.SUL$Idade)
levels(RHC.CCU.SUL$Idade)

RHC.CCU.SUL$Idade <- factor(RHC.CCU.SUL$Idade, levels = c("-59", "0", "1", "4", "7", "9", "10", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                                                          "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", 
                                                          "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", 
                                                          "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", 
                                                          "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100","101", "102", "103", "104",
                                                          "105","106", "107", "108", "116", "118", "999"))

table(RHC.CCU.SUL$Idade)                                                     
RHC.CCU.SUL$Idade[RHC.CCU.SUL$Idade == "-59"] <- "999"
RHC.CCU.SUL$Idade <- as.numeric(RHC.CCU.SUL$Idade)
table(RHC.CCU.SUL$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.CCU.SUL$`Data da primeira consulta` <- as.Date(RHC.CCU.SUL$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.CCU.SUL$`Data da primeira consulta`)
table(RHC.CCU.SUL$`Data da primeira consulta`)
levels(RHC.CCU.SUL$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.CCU.SUL$`Data de diagnóstico` <- as.Date(RHC.CCU.SUL$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.CCU.SUL$`Data de diagnóstico`)
table(RHC.CCU.SUL$`Data de diagnóstico`) #NA
levels(RHC.CCU.SUL$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.CCU.SUL$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.SUL$`Diagnóstico e tratamento anteriores`)
RHC.CCU.SUL$`Diagnóstico e tratamento anteriores`[RHC.CCU.SUL$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.CCU.SUL$`Diagnóstico e tratamento anteriores` <- factor(RHC.CCU.SUL$`Diagnóstico e tratamento anteriores`)
table(RHC.CCU.SUL$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.CCU.SUL$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.SUL$`Base mais importante para o diagnóstico do tumor`)
RHC.CCU.SUL$`Base mais importante para o diagnóstico do tumor`[RHC.CCU.SUL$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.CCU.SUL$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.CCU.SUL$`Base mais importante para o diagnóstico do tumor`)
table(RHC.CCU.SUL$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.CCU.SUL$`Tipo histológico`) 
table(RHC.CCU.SUL$`Tipo histológico`)
RHC.CCU.SUL$`Tipo histológico`[RHC.CCU.SUL$`Tipo histológico` %in% c("8000/2", "8077/3", "8090/3", "9990/3")] <- "9999/9"

RHC.CCU.SUL$`Tipo histológico` <- factor(RHC.CCU.SUL$`Tipo histológico`)
table(RHC.CCU.SUL$`Tipo histológico`)

# Estadiamento

summary(RHC.CCU.SUL$`Estadiamento clínico do tumor (TNM)`) 
RHC.CCU.SUL$`Estadiamento clínico do tumor (TNM)`[RHC.CCU.SUL$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98")] <- "99"
RHC.CCU.SUL$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.CCU.SUL$`Estadiamento clínico do tumor (TNM)`)
table(RHC.CCU.SUL$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.CCU.SUL$TNM <- as.factor(RHC.CCU.SUL$TNM)
summary(RHC.CCU.SUL$TNM)
RHC.CCU.SUL$TNM[RHC.CCU.SUL$TNM %in% c("9", "00", "01", "1", "10", "21", "23", "3", "30", "31",
                               "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.CCU.SUL$TNM)

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.CCU.SUL$`Lateralidade do tumor`) 
table(RHC.CCU.SUL$`Lateralidade do tumor`)
RHC.CCU.SUL$`Lateralidade do tumor`[RHC.CCU.SUL$`Lateralidade do tumor` == "0"] <- "9"
RHC.CCU.SUL$`Lateralidade do tumor` <- factor(RHC.CCU.SUL$`Lateralidade do tumor`)
table(RHC.CCU.SUL$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.CCU.SUL$`Data de início do tratamento` <- as.Date(RHC.CCU.SUL$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.CCU.SUL$`Data de início do tratamento`)
table(RHC.CCU.SUL$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.CCU.SUL$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.CCU.SUL$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.CCU.SUL$`Estado da doença ao final do tratamento`) 
table(RHC.CCU.SUL$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.CCU.SUL$`Data de óbito` <- as.Date(RHC.CCU.SUL$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.CCU.SUL$`Data de óbito`)
table(RHC.CCU.SUL$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.CCU.SUL$anoDIAG <- str_sub(RHC.CCU.SUL$`Data de diagnóstico`, start = 1, end = 4)
RHC.CCU.SUL$anoPRIMEIRACONS <- str_sub(RHC.CCU.SUL$`Data da primeira consulta`, start = 1, end = 4)
RHC.CCU.SUL$anoINITRATA <- str_sub(RHC.CCU.SUL$`Data de início do tratamento`, start = 1, end = 4)
RHC.CCU.SUL$anoOBITO <- str_sub(RHC.CCU.SUL$`Data de óbito`, start = 1, end = 4)

table(RHC.CCU.SUL$anoDIAG)
table(RHC.CCU.SUL$anoPRIMEIRACONS)
table(RHC.CCU.SUL$anoINITRATA)
table(RHC.CCU.SUL$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.CCU.SUL <- RHC.CCU.SUL%>%
  mutate_if(is.factor, replace_factor_na)

RHC.CCU.SUL$`Data da primeira consulta` <- lubridate::as_date(RHC.CCU.SUL$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.CCU.SUL$Ano <- year(RHC.CCU.SUL$`Data da primeira consulta`)
RHC.CCU.SUL$Ano
table(RHC.CCU.SUL$Ano)
RHC.CCU.SUL1 <- RHC.CCU.SUL[RHC.CCU.SUL$Ano >= 2008 & RHC.CCU.SUL$Ano <= 2016,]

# Definindo as categorias

RHC.CCU.SUL$Sexo <- ifelse(RHC.CCU.SUL$Sexo == "9", "Incompletude", "Completude")
RHC.CCU.SUL$`Raça/Cor da pele` <- ifelse(RHC.CCU.SUL$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.CCU.SUL$Escolaridade <- ifelse(RHC.CCU.SUL$Escolaridade == "9", "Incompletude", "Completude")
RHC.CCU.SUL$`Estado de residência` <- ifelse(RHC.CCU.SUL$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.CCU.SUL$Idade <- ifelse(RHC.CCU.SUL$Idade == "999", "Incompletude", "Completude")
RHC.CCU.SUL$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.CCU.SUL$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.CCU.SUL$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.CCU.SUL$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.SUL$`Tipo histológico`<- ifelse(RHC.CCU.SUL$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.CCU.SUL$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.CCU.SUL$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.CCU.SUL$TNM <- ifelse(RHC.CCU.SUL$TNM == "999", "Incompletude", "Completude")
RHC.CCU.SUL$`Lateralidade do tumor` <- ifelse(RHC.CCU.SUL$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.CCU.SUL$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.CCU.SUL$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.SUL$`Estado da doença ao final do tratamento` <- ifelse(RHC.CCU.SUL$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.CCU.SUL$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.CCU.SUL$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.CCU.SUL$`Data da primeira consulta` <- ifelse(is.na(RHC.CCU.SUL$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.CCU.SUL$`Data de diagnóstico` <- ifelse(is.na(RHC.CCU.SUL$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.CCU.SUL$`Data de início do tratamento` <- ifelse(is.na(RHC.CCU.SUL$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.CCU.SUL$`Data de óbito` <- ifelse(RHC.CCU.SUL$`Data de óbito` == "0010-02-03","Incompletude", "Completude")

rhc.completude <- RHC.CCU.SUL%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]


colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.RGSUL.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.SUL <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                               caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer do colo do útero" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.SUL

tab.completude
tab.completude.norte
tab.completude.nordeste
tab.completude.CO
tab.completude.SUDESTE
tab.completude.SUL
##################################Câncer de mama ################################

RHC.MAMA <- read.xlsx("base_filtrada.xlsx")

RHC.MAMA <- RHC.MAMA%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.MAMA <- RHC.MAMA%>%
  filter(LOCTUDET == "C50")

# Analisando a completude das variáveis

names(RHC.MAMA)[1] <- "Sexo"
names(RHC.MAMA)[2] <- "Idade"
names(RHC.MAMA)[3] <- "Raça/Cor da pele"
names(RHC.MAMA)[4] <- "Escolaridade"
names(RHC.MAMA)[5] <- "Estado de residência"
names(RHC.MAMA)[6] <- "Data da primeira consulta"
names(RHC.MAMA)[7] <- "Data de diagnóstico"
names(RHC.MAMA)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.MAMA)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.MAMA)[10] <- "Localização primária do tumor"
names(RHC.MAMA)[11] <- "Tipo histológico"
names(RHC.MAMA)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.MAMA)[13] <- "Lateralidade do tumor"
names(RHC.MAMA)[14] <- "Data de início do tratamento"
names(RHC.MAMA)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.MAMA)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.MAMA)[17] <- "Estado da doença ao final do tratamento"
names(RHC.MAMA)[18] <- "Data de óbito"
names(RHC.MAMA)[19] <- "TNM"

# Corrigindo a classificação das variáveis

RHC.MAMA <- RHC.MAMA%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         TNM = as.factor(TNM),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.MAMA$Sexo)
RHC.MAMA$Sexo <- factor(RHC.MAMA$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.MAMA$Sexo)
RHC.MAMA$Sexo[RHC.MAMA$Sexo %in% c("0", "3")] <- "9"
RHC.MAMA$Sexo <- factor(RHC.MAMA$Sexo)
table(RHC.MAMA$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.MAMA$`Raça/Cor da pele`)
RHC.MAMA$`Raça/Cor da pele`[RHC.MAMA$`Raça/Cor da pele` == "99"] <- "9"
RHC.MAMA$`Raça/Cor da pele` <- factor(RHC.MAMA$`Raça/Cor da pele`)
table(RHC.MAMA$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.MAMA$Escolaridade)
RHC.MAMA$Escolaridade <- factor(RHC.MAMA$Escolaridade)
RHC.MAMA$Escolaridade[RHC.MAMA$Escolaridade == "0"] <- "9"
RHC.MAMA$Escolaridade <- factor(RHC.MAMA$Escolaridade)
table(RHC.MAMA$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.MAMA$`Estado de residência`)
RHC.MAMA$`Estado de residência` <- factor(RHC.MAMA$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                      "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                      "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.MAMA$`Estado de residência`)
RHC.MAMA$`Estado de residência`[RHC.MAMA$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.MAMA$`Estado de residência` <- factor(RHC.MAMA$`Estado de residência`)
table(RHC.MAMA$`Estado de residência`)

# Idade

RHC.MAMA$Idade <- as.numeric(RHC.MAMA$Idade)
table(RHC.MAMA$Idade)
summary(RHC.MAMA$Idade)
levels(RHC.MAMA$Idade)
table(RHC.MAMA$Idade)                                                     
RHC.MAMA$Idade[RHC.MAMA$Idade  %in% c("000", NA)] <- "999"
RHC.MAMA$Idade <- as.numeric(RHC.MAMA$Idade)
table(RHC.MAMA$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.MAMA$`Data da primeira consulta` <- as.Date(RHC.MAMA$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.MAMA$`Data da primeira consulta`)
table(RHC.MAMA$`Data da primeira consulta`)
levels(RHC.MAMA$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.MAMA$`Data de diagnóstico` <- as.Date(RHC.MAMA$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.MAMA$`Data de diagnóstico`)
table(RHC.MAMA$`Data de diagnóstico`) #NA
levels(RHC.MAMA$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.MAMA$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA$`Diagnóstico e tratamento anteriores`)
RHC.MAMA$`Diagnóstico e tratamento anteriores`[RHC.MAMA$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.MAMA$`Diagnóstico e tratamento anteriores` <- factor(RHC.MAMA$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.MAMA$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA$`Base mais importante para o diagnóstico do tumor`)
RHC.MAMA$`Base mais importante para o diagnóstico do tumor`[RHC.MAMA$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.MAMA$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.MAMA$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.MAMA$`Tipo histológico`) 
table(RHC.MAMA$`Tipo histológico`)
RHC.MAMA$`Tipo histológico`[RHC.MAMA$`Tipo histológico` %in% c("/","8000/2","8010/","8100/3","8500/9","8503/","9713/3", "9990/3", "9991/6", "9991/7",
                                                               "9992/0", "9992/2", "9992/6" ,NA)] <- "9999/9"
RHC.MAMA$`Tipo histológico` <- factor(RHC.MAMA$`Tipo histológico`)
table(RHC.MAMA$`Tipo histológico`)

# Estadiamento

summary(RHC.MAMA$`Estadiamento clínico do tumor (TNM)`) 
RHC.MAMA$`Estadiamento clínico do tumor (TNM)`[RHC.MAMA$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98", "33","9",NA)] <- "99"
RHC.MAMA$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.MAMA$`Estadiamento clínico do tumor (TNM)`)
table(RHC.MAMA$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.MAMA$TNM <- as.factor(RHC.MAMA$TNM)
summary(RHC.MAMA$TNM)
RHC.MAMA$TNM[RHC.MAMA$TNM %in% c("9", "00", "01", "1", "10", "2","20","21", "22","23", "3", "30", "31",
                                 "32", "33", "4", "40", "41", "43", "99", "9")] <- "999"
table(RHC.MAMA$TNM)

# 999 Sem informação

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.MAMA$`Lateralidade do tumor`) 
table(RHC.MAMA$`Lateralidade do tumor`)
RHC.MAMA$`Lateralidade do tumor`[RHC.MAMA$`Lateralidade do tumor` == "0"] <- "9"
RHC.MAMA$`Lateralidade do tumor` <- factor(RHC.MAMA$`Lateralidade do tumor`)
table(RHC.MAMA$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.MAMA$`Data de início do tratamento` <- as.Date(RHC.MAMA$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.MAMA$`Data de início do tratamento`)
table(RHC.MAMA$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.MAMA$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.MAMA$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.MAMA$`Estado da doença ao final do tratamento`) 
table(RHC.MAMA$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.MAMA$`Data de óbito` <- as.Date(RHC.MAMA$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.MAMA$`Data de óbito`)
table(RHC.MAMA$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.MAMA$anoDIAG <- str_sub(RHC.MAMA$`Data de diagnóstico`, start = 1, end = 4)
RHC.MAMA$anoPRIMEIRACONS <- str_sub(RHC.MAMA$`Data da primeira consulta`, start = 1, end = 4)
RHC.MAMA$anoINITRATA <- str_sub(RHC.MAMA$`Data de início do tratamento`, start = 1, end = 4)
RHC.MAMA$anoOBITO <- str_sub(RHC.MAMA$`Data de óbito`, start = 1, end = 4)

table(RHC.MAMA$anoDIAG)
table(RHC.MAMA$anoPRIMEIRACONS)
table(RHC.MAMA$anoINITRATA)
table(RHC.MAMA$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.MAMA <- RHC.MAMA%>%
  mutate_if(is.factor, replace_factor_na)

RHC.MAMA$`Data da primeira consulta` <- lubridate::as_date(RHC.MAMA$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.MAMA$Ano <- year(RHC.MAMA$`Data da primeira consulta`)
RHC.MAMA$Ano
table(RHC.MAMA$Ano)
RHC.MAMA1 <- RHC.MAMA[RHC.MAMA$Ano >= 2008 & RHC.MAMA$Ano <= 2016,]

# Definindo as categorias

RHC.MAMA$Sexo <- ifelse(RHC.MAMA$Sexo == "9", "Incompletude", "Completude")
RHC.MAMA$`Raça/Cor da pele` <- ifelse(RHC.MAMA$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.MAMA$Escolaridade <- ifelse(RHC.MAMA$Escolaridade == "9", "Incompletude", "Completude")
RHC.MAMA$`Estado de residência` <- ifelse(RHC.MAMA$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.MAMA$Idade <- ifelse(RHC.MAMA$Idade == "999", "Incompletude", "Completude")
RHC.MAMA$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.MAMA$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.MAMA$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.MAMA$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA$`Tipo histológico`<- ifelse(RHC.MAMA$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.MAMA$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.MAMA$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.MAMA$TNM <- ifelse(RHC.MAMA$TNM == "999", "Incompletude", "Completude")
RHC.MAMA$`Lateralidade do tumor` <- ifelse(RHC.MAMA$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.MAMA$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA$`Estado da doença ao final do tratamento` <- ifelse(RHC.MAMA$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.MAMA$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.MAMA$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA$`Data da primeira consulta` <- ifelse(is.na(RHC.MAMA$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.MAMA$`Data de diagnóstico` <- ifelse(is.na(RHC.MAMA$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.MAMA$`Data de início do tratamento` <- ifelse(is.na(RHC.MAMA$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.MAMA$`Data de óbito` <- ifelse(RHC.MAMA$`Data de óbito` == "2022-03-10","Incompletude", "Completude")

rhc.completude <- RHC.MAMA%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]

colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.mama.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.mama <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                                    caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer de mama" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.mama


# NORTE

RHC.MAMA.NORTE <- read.xlsx("base_filtrada.xlsx")

RHC.MAMA.NORTE <- RHC.MAMA.NORTE%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.MAMA.NORTE <- RHC.MAMA.NORTE%>%
  filter(LOCTUDET == "C50")

# Analisando a completude das variáveis

names(RHC.MAMA.NORTE)[1] <- "Sexo"
names(RHC.MAMA.NORTE)[2] <- "Idade"
names(RHC.MAMA.NORTE)[3] <- "Raça/Cor da pele"
names(RHC.MAMA.NORTE)[4] <- "Escolaridade"
names(RHC.MAMA.NORTE)[5] <- "Estado de residência"
names(RHC.MAMA.NORTE)[6] <- "Data da primeira consulta"
names(RHC.MAMA.NORTE)[7] <- "Data de diagnóstico"
names(RHC.MAMA.NORTE)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.MAMA.NORTE)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.MAMA.NORTE)[10] <- "Localização primária do tumor"
names(RHC.MAMA.NORTE)[11] <- "Tipo histológico"
names(RHC.MAMA.NORTE)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.MAMA.NORTE)[13] <- "Lateralidade do tumor"
names(RHC.MAMA.NORTE)[14] <- "Data de início do tratamento"
names(RHC.MAMA.NORTE)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.MAMA.NORTE)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.MAMA.NORTE)[17] <- "Estado da doença ao final do tratamento"
names(RHC.MAMA.NORTE)[18] <- "Data de óbito"
names(RHC.MAMA.NORTE)[19] <- "TNM"

table(RHC.MAMA.NORTE$`Estado de residência`)
levels(RHC.MAMA.NORTE$`Estado de residência`)
RHC.MAMA.NORTE$`Estado de residência` <- as.factor(RHC.MAMA.NORTE$`Estado de residência`)

RHC.MAMA.NORTE$Regioes = ""
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "AC")] = "Norte"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "AM")] = "Norte"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "RR")] = "Norte"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "AP")] = "Norte"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "PA")] = "Norte"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "TO")] = "Norte"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "RO")] = "Norte"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "MA")] = "Nordeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "PI")] = "Nordeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "CE")] = "Nordeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "RN")] = "Nordeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "PE")] = "Nordeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "PB")] = "Nordeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "SE")] = "Nordeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "AL")] = "Nordeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "BA")] = "Nordeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "SP")] = "Sudeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "RJ")] = "Sudeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "ES")] = "Sudeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "MG")] = "Sudeste"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "PR")] = "Sul"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "RS")] = "Sul"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "SC")] = "Sul"
RHC.MAMA.NORTE$Regioes[str_detect(RHC.MAMA.NORTE$`Estado de residência`, "DF")] = "Centro-oeste"

table(RHC.MAMA.NORTE$Regioes)

# Região Norte

RHC.MAMA.NORTE <- RHC.MAMA.NORTE%>%
  filter(Regioes == "Norte")

# Corrigindo a classificação das variáveis

RHC.MAMA.NORTE <- RHC.MAMA.NORTE%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         TNM = as.factor(TNM),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.MAMA.NORTE$Sexo)
RHC.MAMA.NORTE$Sexo <- factor(RHC.MAMA.NORTE$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.MAMA.NORTE$Sexo)
RHC.MAMA.NORTE$Sexo[RHC.MAMA.NORTE$Sexo %in% c("0", "3")] <- "9"
RHC.MAMA.NORTE$Sexo <- factor(RHC.MAMA.NORTE$Sexo)
table(RHC.MAMA.NORTE$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.MAMA.NORTE$`Raça/Cor da pele`)
RHC.MAMA.NORTE$`Raça/Cor da pele`[RHC.MAMA.NORTE$`Raça/Cor da pele` == "99"] <- "9"
RHC.MAMA.NORTE$`Raça/Cor da pele` <- factor(RHC.MAMA.NORTE$`Raça/Cor da pele`)
table(RHC.MAMA.NORTE$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.MAMA.NORTE$Escolaridade)
RHC.MAMA.NORTE$Escolaridade <- factor(RHC.MAMA.NORTE$Escolaridade)
RHC.MAMA.NORTE$Escolaridade[RHC.MAMA.NORTE$Escolaridade == "0"] <- "9"
RHC.MAMA.NORTE$Escolaridade <- factor(RHC.MAMA.NORTE$Escolaridade)
table(RHC.MAMA.NORTE$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.MAMA.NORTE$`Estado de residência`)
RHC.MAMA.NORTE$`Estado de residência` <- factor(RHC.MAMA.NORTE$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                                  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                                  "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.MAMA.NORTE$`Estado de residência`)
RHC.MAMA.NORTE$`Estado de residência`[RHC.MAMA.NORTE$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.MAMA.NORTE$`Estado de residência` <- factor(RHC.MAMA.NORTE$`Estado de residência`)
table(RHC.MAMA.NORTE$`Estado de residência`)

# Idade

RHC.MAMA.NORTE$Idade <- as.numeric(RHC.MAMA.NORTE$Idade)
table(RHC.MAMA.NORTE$Idade)
levels(RHC.MAMA.NORTE$Idade)
table(RHC.MAMA.NORTE$Idade)                                                     
RHC.MAMA.NORTE$Idade[RHC.MAMA.NORTE$Idade %in% c("000", NA)] <- "999"
RHC.MAMA.NORTE$Idade <- as.numeric(RHC.MAMA.NORTE$Idade)
table(RHC.MAMA.NORTE$Idade)


# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.MAMA.NORTE$`Data da primeira consulta` <- as.Date(RHC.MAMA.NORTE$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.MAMA.NORTE$`Data da primeira consulta`)
table(RHC.MAMA.NORTE$`Data da primeira consulta`)
levels(RHC.MAMA.NORTE$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.MAMA.NORTE$`Data de diagnóstico` <- as.Date(RHC.MAMA.NORTE$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.MAMA.NORTE$`Data de diagnóstico`)
table(RHC.MAMA.NORTE$`Data de diagnóstico`) #NA
levels(RHC.MAMA.NORTE$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.MAMA.NORTE$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.NORTE$`Diagnóstico e tratamento anteriores`)
RHC.MAMA.NORTE$`Diagnóstico e tratamento anteriores`[RHC.MAMA.NORTE$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.MAMA.NORTE$`Diagnóstico e tratamento anteriores` <- factor(RHC.MAMA.NORTE$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.NORTE$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.MAMA.NORTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.NORTE$`Base mais importante para o diagnóstico do tumor`)
RHC.MAMA.NORTE$`Base mais importante para o diagnóstico do tumor`[RHC.MAMA.NORTE$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.MAMA.NORTE$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.MAMA.NORTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.NORTE$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.MAMA.NORTE$`Tipo histológico`) 
table(RHC.MAMA.NORTE$`Tipo histológico`)
RHC.MAMA.NORTE$`Tipo histológico`[RHC.MAMA.NORTE$`Tipo histológico` %in% c("/","8000/2","8010/","8100/3","8500/9","8503/","9713/3", "9990/3", "9991/6", "9991/7",
                                                                           "9992/0", "9992/2", "9992/6" ,NA)] <- "9999/9"
RHC.MAMA.NORTE$`Tipo histológico` <- factor(RHC.MAMA.NORTE$`Tipo histológico`)
table(RHC.MAMA.NORTE$`Tipo histológico`)

# Estadiamento

summary(RHC.MAMA.NORTE$`Estadiamento clínico do tumor (TNM)`) 
RHC.MAMA.NORTE$`Estadiamento clínico do tumor (TNM)`[RHC.MAMA.NORTE$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98", "33","9",NA)] <- "99"
RHC.MAMA.NORTE$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.MAMA.NORTE$`Estadiamento clínico do tumor (TNM)`)
table(RHC.MAMA.NORTE$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.MAMA.NORTE$TNM <- as.factor(RHC.MAMA.NORTE$TNM)
summary(RHC.MAMA.NORTE$TNM)
RHC.MAMA.NORTE$TNM[RHC.MAMA.NORTE$TNM %in% c("9", "00", "01", "1", "10", "2","20","21", "22","23", "3", "30", "31",
                                             "32", "33", "4", "40", "41", "43", "99", "9")] <- "999"
table(RHC.MAMA.NORTE$TNM)

# 999 Sem informação

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.MAMA.NORTE$`Lateralidade do tumor`) 
table(RHC.MAMA.NORTE$`Lateralidade do tumor`)
RHC.MAMA.NORTE$`Lateralidade do tumor`[RHC.MAMA.NORTE$`Lateralidade do tumor` == "0"] <- "9"
RHC.MAMA.NORTE$`Lateralidade do tumor` <- factor(RHC.MAMA.NORTE$`Lateralidade do tumor`)
table(RHC.MAMA.NORTE$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.MAMA.NORTE$`Data de início do tratamento` <- as.Date(RHC.MAMA.NORTE$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.MAMA.NORTE$`Data de início do tratamento`)
table(RHC.MAMA.NORTE$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.MAMA.NORTE$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.MAMA.NORTE$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.MAMA.NORTE$`Estado da doença ao final do tratamento`) 
table(RHC.MAMA.NORTE$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.MAMA.NORTE$`Data de óbito` <- as.Date(RHC.MAMA.NORTE$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.MAMA.NORTE$`Data de óbito`)
table(RHC.MAMA.NORTE$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.MAMA.NORTE$anoDIAG <- str_sub(RHC.MAMA.NORTE$`Data de diagnóstico`, start = 1, end = 4)
RHC.MAMA.NORTE$anoPRIMEIRACONS <- str_sub(RHC.MAMA.NORTE$`Data da primeira consulta`, start = 1, end = 4)
RHC.MAMA.NORTE$anoINITRATA <- str_sub(RHC.MAMA.NORTE$`Data de início do tratamento`, start = 1, end = 4)
RHC.MAMA.NORTE$anoOBITO <- str_sub(RHC.MAMA.NORTE$`Data de óbito`, start = 1, end = 4)

table(RHC.MAMA.NORTE$anoDIAG)
table(RHC.MAMA.NORTE$anoPRIMEIRACONS)
table(RHC.MAMA.NORTE$anoINITRATA)
table(RHC.MAMA.NORTE$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.MAMA.NORTE <- RHC.MAMA.NORTE%>%
  mutate_if(is.factor, replace_factor_na)

RHC.MAMA.NORTE$`Data da primeira consulta` <- lubridate::as_date(RHC.MAMA.NORTE$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.MAMA.NORTE$Ano <- year(RHC.MAMA.NORTE$`Data da primeira consulta`)
RHC.MAMA.NORTE$Ano
table(RHC.MAMA.NORTE$Ano)
RHC.MAMA.NORTE <- RHC.MAMA.NORTE[RHC.MAMA.NORTE$Ano >= 2008 & RHC.MAMA.NORTE$Ano <= 2016,]

# Definindo as categorias

RHC.MAMA.NORTE$Sexo <- ifelse(RHC.MAMA.NORTE$Sexo == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Raça/Cor da pele` <- ifelse(RHC.MAMA.NORTE$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$Escolaridade <- ifelse(RHC.MAMA.NORTE$Escolaridade == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Estado de residência` <- ifelse(RHC.MAMA.NORTE$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$Idade <- ifelse(RHC.MAMA.NORTE$Idade == "999", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.MAMA.NORTE$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.MAMA.NORTE$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Tipo histológico`<- ifelse(RHC.MAMA.NORTE$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.MAMA.NORTE$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.MAMA.NORTE$TNM <- ifelse(RHC.MAMA.NORTE$TNM == "999", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Lateralidade do tumor` <- ifelse(RHC.MAMA.NORTE$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.MAMA.NORTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Estado da doença ao final do tratamento` <- ifelse(RHC.MAMA.NORTE$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.MAMA.NORTE$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.NORTE$`Data da primeira consulta` <- ifelse(is.na(RHC.MAMA.NORTE$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.MAMA.NORTE$`Data de diagnóstico` <- ifelse(is.na(RHC.MAMA.NORTE$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.MAMA.NORTE$`Data de início do tratamento` <- ifelse(is.na(RHC.MAMA.NORTE$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.MAMA.NORTE$`Data de óbito` <- ifelse(RHC.MAMA.NORTE$`Data de óbito` == "2022-03-10","Incompletude", "Completude")

rhc.completude <- RHC.MAMA.NORTE%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]

colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.mama.norte.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.mama.norte <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                                          caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer de mama" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.mama.norte

# NORDESTE

RHC.MAMA.NORDESTE <- read.xlsx("base_filtrada.xlsx")

RHC.MAMA.NORDESTE <- RHC.MAMA.NORDESTE%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.MAMA.NORDESTE <- RHC.MAMA.NORDESTE%>%
  filter(LOCTUDET == "C50")

# Analisando a completude das variáveis

names(RHC.MAMA.NORDESTE)[1] <- "Sexo"
names(RHC.MAMA.NORDESTE)[2] <- "Idade"
names(RHC.MAMA.NORDESTE)[3] <- "Raça/Cor da pele"
names(RHC.MAMA.NORDESTE)[4] <- "Escolaridade"
names(RHC.MAMA.NORDESTE)[5] <- "Estado de residência"
names(RHC.MAMA.NORDESTE)[6] <- "Data da primeira consulta"
names(RHC.MAMA.NORDESTE)[7] <- "Data de diagnóstico"
names(RHC.MAMA.NORDESTE)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.MAMA.NORDESTE)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.MAMA.NORDESTE)[10] <- "Localização primária do tumor"
names(RHC.MAMA.NORDESTE)[11] <- "Tipo histológico"
names(RHC.MAMA.NORDESTE)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.MAMA.NORDESTE)[13] <- "Lateralidade do tumor"
names(RHC.MAMA.NORDESTE)[14] <- "Data de início do tratamento"
names(RHC.MAMA.NORDESTE)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.MAMA.NORDESTE)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.MAMA.NORDESTE)[17] <- "Estado da doença ao final do tratamento"
names(RHC.MAMA.NORDESTE)[18] <- "Data de óbito"
names(RHC.MAMA.NORDESTE)[19] <- "TNM"

table(RHC.MAMA.NORDESTE$`Estado de residência`)

levels(RHC.MAMA.NORDESTE$`Estado de residência`)
RHC.MAMA.NORDESTE$`Estado de residência` <- as.factor(RHC.MAMA.NORDESTE$`Estado de residência`)

RHC.MAMA.NORDESTE$Regioes = ""
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "AC")] = "Norte"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "AM")] = "Norte"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "RR")] = "Norte"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "AP")] = "Norte"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "PA")] = "Norte"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "TO")] = "Norte"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "RO")] = "Norte"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "MA")] = "Nordeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "PI")] = "Nordeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "CE")] = "Nordeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "RN")] = "Nordeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "PE")] = "Nordeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "PB")] = "Nordeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "SE")] = "Nordeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "AL")] = "Nordeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "BA")] = "Nordeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "SP")] = "Sudeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "RJ")] = "Sudeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "ES")] = "Sudeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "MG")] = "Sudeste"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "PR")] = "Sul"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "RS")] = "Sul"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "SC")] = "Sul"
RHC.MAMA.NORDESTE$Regioes[str_detect(RHC.MAMA.NORDESTE$`Estado de residência`, "DF")] = "Centro-oeste"

table(RHC.MAMA.NORDESTE$Regioes)

# Região Nordeste

RHC.MAMA.NORDESTE.NORDESTE <- RHC.MAMA.NORDESTE%>%
  filter(Regioes == "Nordeste")

# Corrigindo a classificação das variáveis

RHC.MAMA.NORDESTE <- RHC.MAMA.NORDESTE.NORDESTE%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         TNM = as.factor(TNM),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.MAMA.NORDESTE$Sexo)
RHC.MAMA.NORDESTE$Sexo <- factor(RHC.MAMA.NORDESTE$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.MAMA.NORDESTE$Sexo)
RHC.MAMA.NORDESTE$Sexo[RHC.MAMA.NORDESTE$Sexo %in% c("0", "3")] <- "9"
RHC.MAMA.NORDESTE$Sexo <- factor(RHC.MAMA.NORDESTE$Sexo)
table(RHC.MAMA.NORDESTE$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.MAMA.NORDESTE$`Raça/Cor da pele`)
RHC.MAMA.NORDESTE$`Raça/Cor da pele`[RHC.MAMA.NORDESTE$`Raça/Cor da pele` == "99"] <- "9"
RHC.MAMA.NORDESTE$`Raça/Cor da pele` <- factor(RHC.MAMA.NORDESTE$`Raça/Cor da pele`)
table(RHC.MAMA.NORDESTE$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.MAMA.NORDESTE$Escolaridade)
RHC.MAMA.NORDESTE$Escolaridade <- factor(RHC.MAMA.NORDESTE$Escolaridade)
RHC.MAMA.NORDESTE$Escolaridade[RHC.MAMA.NORDESTE$Escolaridade == "0"] <- "9"
RHC.MAMA.NORDESTE$Escolaridade <- factor(RHC.MAMA.NORDESTE$Escolaridade)
table(RHC.MAMA.NORDESTE$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.MAMA.NORDESTE$`Estado de residência`)
RHC.MAMA.NORDESTE$`Estado de residência` <- factor(RHC.MAMA.NORDESTE$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                                        "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                                        "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.MAMA.NORDESTE$`Estado de residência`)
RHC.MAMA.NORDESTE$`Estado de residência`[RHC.MAMA.NORDESTE$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.MAMA.NORDESTE$`Estado de residência` <- factor(RHC.MAMA.NORDESTE$`Estado de residência`)
table(RHC.MAMA.NORDESTE$`Estado de residência`)

# Idade

RHC.MAMA.NORDESTE$Idade <- as.numeric(RHC.MAMA.NORDESTE$Idade)
table(RHC.MAMA.NORDESTE$Idade)
levels(RHC.MAMA.NORDESTE$Idade)
table(RHC.MAMA.NORDESTE$Idade)                                                     
RHC.MAMA.NORDESTE$Idade[RHC.MAMA.NORDESTE$Idade %IN% C("000", NA)] <- "999"
RHC.MAMA.NORDESTE$Idade <- as.numeric(RHC.MAMA.NORDESTE$Idade)
table(RHC.MAMA.NORDESTE$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.MAMA.NORDESTE$`Data da primeira consulta` <- as.Date(RHC.MAMA.NORDESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.MAMA.NORDESTE$`Data da primeira consulta`)
table(RHC.MAMA.NORDESTE$`Data da primeira consulta`)
levels(RHC.MAMA.NORDESTE$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.MAMA.NORDESTE$`Data de diagnóstico` <- as.Date(RHC.MAMA.NORDESTE$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.MAMA.NORDESTE$`Data de diagnóstico`)
table(RHC.MAMA.NORDESTE$`Data de diagnóstico`) #NA
levels(RHC.MAMA.NORDESTE$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.MAMA.NORDESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.NORDESTE$`Diagnóstico e tratamento anteriores`)
RHC.MAMA.NORDESTE$`Diagnóstico e tratamento anteriores`[RHC.MAMA.NORDESTE$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.MAMA.NORDESTE$`Diagnóstico e tratamento anteriores` <- factor(RHC.MAMA.NORDESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.NORDESTE$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.MAMA.NORDESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.NORDESTE$`Base mais importante para o diagnóstico do tumor`)
RHC.MAMA.NORDESTE$`Base mais importante para o diagnóstico do tumor`[RHC.MAMA.NORDESTE$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.MAMA.NORDESTE$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.MAMA.NORDESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.NORDESTE$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.MAMA.NORDESTE$`Tipo histológico`) 
table(RHC.MAMA.NORDESTE$`Tipo histológico`)
RHC.MAMA.NORDESTE$`Tipo histológico`[RHC.MAMA.NORDESTE$`Tipo histológico` %in% c("/","8000/2","8010/","8100/3","8500/9","8503/","9713/3", "9990/3", "9991/6", "9991/7",
                                                                                 "9992/0", "9992/2", "9992/6" ,NA)] <- "9999/9"
RHC.MAMA.NORDESTE$`Tipo histológico` <- factor(RHC.MAMA.NORDESTE$`Tipo histológico`)
table(RHC.MAMA.NORDESTE$`Tipo histológico`)

# Estadiamento

summary(RHC.MAMA.NORDESTE$`Estadiamento clínico do tumor (TNM)`) 
RHC.MAMA.NORDESTE$`Estadiamento clínico do tumor (TNM)`[RHC.MAMA.NORDESTE$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98", "33","9",NA)] <- "99"
RHC.MAMA.NORDESTE$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.MAMA.NORDESTE$`Estadiamento clínico do tumor (TNM)`)
table(RHC.MAMA.NORDESTE$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.MAMA.NORDESTE$TNM <- as.factor(RHC.MAMA.NORDESTE$TNM)
summary(RHC.MAMA.NORDESTE$TNM)
RHC.MAMA.NORDESTE$TNM[RHC.MAMA.NORDESTE$TNM %in% c("9", "00", "01", "1", "10", "2","20","21", "22","23", "3", "30", "31",
                                                   "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.MAMA.NORDESTE$TNM)

# 999 Sem informação

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.MAMA.NORDESTE$`Lateralidade do tumor`) 
table(RHC.MAMA.NORDESTE$`Lateralidade do tumor`)
RHC.MAMA.NORDESTE$`Lateralidade do tumor`[RHC.MAMA.NORDESTE$`Lateralidade do tumor` == "0"] <- "9"
RHC.MAMA.NORDESTE$`Lateralidade do tumor` <- factor(RHC.MAMA.NORDESTE$`Lateralidade do tumor`)
table(RHC.MAMA.NORDESTE$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.MAMA.NORDESTE$`Data de início do tratamento` <- as.Date(RHC.MAMA.NORDESTE$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.MAMA.NORDESTE$`Data de início do tratamento`)
table(RHC.MAMA.NORDESTE$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.MAMA.NORDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.MAMA.NORDESTE$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.MAMA.NORDESTE$`Estado da doença ao final do tratamento`) 
table(RHC.MAMA.NORDESTE$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.MAMA.NORDESTE$`Data de óbito` <- as.Date(RHC.MAMA.NORDESTE$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.MAMA.NORDESTE$`Data de óbito`)
table(RHC.MAMA.NORDESTE$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.MAMA.NORDESTE$anoDIAG <- str_sub(RHC.MAMA.NORDESTE$`Data de diagnóstico`, start = 1, end = 4)
RHC.MAMA.NORDESTE$anoPRIMEIRACONS <- str_sub(RHC.MAMA.NORDESTE$`Data da primeira consulta`, start = 1, end = 4)
RHC.MAMA.NORDESTE$anoINITRATA <- str_sub(RHC.MAMA.NORDESTE$`Data de início do tratamento`, start = 1, end = 4)
RHC.MAMA.NORDESTE$anoOBITO <- str_sub(RHC.MAMA.NORDESTE$`Data de óbito`, start = 1, end = 4)

table(RHC.MAMA.NORDESTE$anoDIAG)
table(RHC.MAMA.NORDESTE$anoPRIMEIRACONS)
table(RHC.MAMA.NORDESTE$anoINITRATA)
table(RHC.MAMA.NORDESTE$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.MAMA.NORDESTE <- RHC.MAMA.NORDESTE%>%
  mutate_if(is.factor, replace_factor_na)

RHC.MAMA.NORDESTE$`Data da primeira consulta` <- lubridate::as_date(RHC.MAMA.NORDESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.MAMA.NORDESTE$Ano <- year(RHC.MAMA.NORDESTE$`Data da primeira consulta`)
RHC.MAMA.NORDESTE$Ano
table(RHC.MAMA.NORDESTE$Ano)
RHC.MAMA.NORDESTE1 <- RHC.MAMA.NORDESTE[RHC.MAMA.NORDESTE$Ano >= 2008 & RHC.MAMA.NORDESTE$Ano <= 2016,]

# Definindo as categorias

RHC.MAMA.NORDESTE$Sexo <- ifelse(RHC.MAMA.NORDESTE$Sexo == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Raça/Cor da pele` <- ifelse(RHC.MAMA.NORDESTE$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$Escolaridade <- ifelse(RHC.MAMA.NORDESTE$Escolaridade == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Estado de residência` <- ifelse(RHC.MAMA.NORDESTE$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$Idade <- ifelse(RHC.MAMA.NORDESTE$Idade == "999", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.MAMA.NORDESTE$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.MAMA.NORDESTE$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Tipo histológico`<- ifelse(RHC.MAMA.NORDESTE$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.MAMA.NORDESTE$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$TNM <- ifelse(RHC.MAMA.NORDESTE$TNM == "999", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Lateralidade do tumor` <- ifelse(RHC.MAMA.NORDESTE$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.MAMA.NORDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Estado da doença ao final do tratamento` <- ifelse(RHC.MAMA.NORDESTE$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.MAMA.NORDESTE$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Data da primeira consulta` <- ifelse(is.na(RHC.MAMA.NORDESTE$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Data de diagnóstico` <- ifelse(is.na(RHC.MAMA.NORDESTE$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Data de início do tratamento` <- ifelse(is.na(RHC.MAMA.NORDESTE$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.MAMA.NORDESTE$`Data de óbito` <- ifelse(RHC.MAMA.NORDESTE$`Data de óbito` == "2022-03-10","Incompletude", "Completude")

rhc.completude <- RHC.MAMA.NORDESTE%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]

colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.mama.NORDESTE.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.mama.nordeste <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                                             caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer de mama" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.mama.nordeste

# Centro-oeste

RHC.MAMA.CO <- read.xlsx("base_filtrada.xlsx")

RHC.MAMA.CO <- RHC.MAMA.CO%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.MAMA.CO <- RHC.MAMA.CO%>%
  filter(LOCTUDET == "C50")

# Analisando a completude das variáveis

names(RHC.MAMA.CO)[1] <- "Sexo"
names(RHC.MAMA.CO)[2] <- "Idade"
names(RHC.MAMA.CO)[3] <- "Raça/Cor da pele"
names(RHC.MAMA.CO)[4] <- "Escolaridade"
names(RHC.MAMA.CO)[5] <- "Estado de residência"
names(RHC.MAMA.CO)[6] <- "Data da primeira consulta"
names(RHC.MAMA.CO)[7] <- "Data de diagnóstico"
names(RHC.MAMA.CO)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.MAMA.CO)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.MAMA.CO)[10] <- "Localização primária do tumor"
names(RHC.MAMA.CO)[11] <- "Tipo histológico"
names(RHC.MAMA.CO)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.MAMA.CO)[13] <- "Lateralidade do tumor"
names(RHC.MAMA.CO)[14] <- "Data de início do tratamento"
names(RHC.MAMA.CO)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.MAMA.CO)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.MAMA.CO)[17] <- "Estado da doença ao final do tratamento"
names(RHC.MAMA.CO)[18] <- "Data de óbito"
names(RHC.MAMA.CO)[19] <- "TNM"

table(RHC.MAMA.CO$`Estado de residência`)

levels(RHC.MAMA.CO$`Estado de residência`)
RHC.MAMA.CO$`Estado de residência` <- as.factor(RHC.MAMA.CO$`Estado de residência`)


RHC.MAMA.CO$Regioes = ""
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "AC")] = "Norte"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "AM")] = "Norte"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "RR")] = "Norte"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "AP")] = "Norte"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "PA")] = "Norte"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "TO")] = "Norte"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "RO")] = "Norte"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "MA")] = "Nordeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "PI")] = "Nordeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "CE")] = "Nordeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "RN")] = "Nordeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "PE")] = "Nordeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "PB")] = "Nordeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "SE")] = "Nordeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "AL")] = "Nordeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "BA")] = "Nordeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "SP")] = "Sudeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "RJ")] = "Sudeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "ES")] = "Sudeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "MG")] = "Sudeste"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "PR")] = "Sul"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "RS")] = "Sul"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "SC")] = "Sul"
RHC.MAMA.CO$Regioes[str_detect(RHC.MAMA.CO$`Estado de residência`, "DF")] = "Centro-oeste"


table(RHC.MAMA.CO$Regioes)

# Região centro-oeste

RHC.MAMA.CO.CO <- RHC.MAMA.CO%>%
  filter(Regioes == "Centro-oeste")

# Corrigindo a classificação das variáveis

RHC.MAMA.CO <- RHC.MAMA.CO.CO%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         TNM = as.factor(TNM),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.MAMA.CO$Sexo)
RHC.MAMA.CO$Sexo <- factor(RHC.MAMA.CO$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.MAMA.CO$Sexo)
RHC.MAMA.CO$Sexo[RHC.MAMA.CO$Sexo %in% c("0", "3")] <- "9"
RHC.MAMA.CO$Sexo <- factor(RHC.MAMA.CO$Sexo)
table(RHC.MAMA.CO$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.MAMA.CO$`Raça/Cor da pele`)
RHC.MAMA.CO$`Raça/Cor da pele`[RHC.MAMA.CO$`Raça/Cor da pele` == "99"] <- "9"
RHC.MAMA.CO$`Raça/Cor da pele` <- factor(RHC.MAMA.CO$`Raça/Cor da pele`)
table(RHC.MAMA.CO$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.MAMA.CO$Escolaridade)
RHC.MAMA.CO$Escolaridade <- factor(RHC.MAMA.CO$Escolaridade)
RHC.MAMA.CO$Escolaridade[RHC.MAMA.CO$Escolaridade == "0"] <- "9"
RHC.MAMA.CO$Escolaridade <- factor(RHC.MAMA.CO$Escolaridade)
table(RHC.MAMA.CO$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.MAMA.CO$`Estado de residência`)
RHC.MAMA.CO$`Estado de residência` <- factor(RHC.MAMA.CO$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                            "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                            "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.MAMA.CO$`Estado de residência`)
RHC.MAMA.CO$`Estado de residência`[RHC.MAMA.CO$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.MAMA.CO$`Estado de residência` <- factor(RHC.MAMA.CO$`Estado de residência`)
table(RHC.MAMA.CO$`Estado de residência`)

# Idade

RHC.MAMA.CO$Idade <- as.numeric(RHC.MAMA.CO$Idade)
table(RHC.MAMA.CO$Idade)
levels(RHC.MAMA.CO$Idade)
table(RHC.MAMA.CO$Idade)                                                     
RHC.MAMA.CO$Idade[RHC.MAMA.CO$Idade %in% c("000", NA)] <- "999"
RHC.MAMA.CO$Idade <- as.numeric(RHC.MAMA.CO$Idade)
table(RHC.MAMA.CO$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.MAMA.CO$`Data da primeira consulta` <- as.Date(RHC.MAMA.CO$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.MAMA.CO$`Data da primeira consulta`)
table(RHC.MAMA.CO$`Data da primeira consulta`)
levels(RHC.MAMA.CO$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.MAMA.CO$`Data de diagnóstico` <- as.Date(RHC.MAMA.CO$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.MAMA.CO$`Data de diagnóstico`)
table(RHC.MAMA.CO$`Data de diagnóstico`) #NA
levels(RHC.MAMA.CO$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.MAMA.CO$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.CO$`Diagnóstico e tratamento anteriores`)
RHC.MAMA.CO$`Diagnóstico e tratamento anteriores`[RHC.MAMA.CO$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.MAMA.CO$`Diagnóstico e tratamento anteriores` <- factor(RHC.MAMA.CO$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.CO$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.MAMA.CO$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.CO$`Base mais importante para o diagnóstico do tumor`)
RHC.MAMA.CO$`Base mais importante para o diagnóstico do tumor`[RHC.MAMA.CO$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.MAMA.CO$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.MAMA.CO$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.CO$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.MAMA.CO$`Tipo histológico`) 
table(RHC.MAMA.CO$`Tipo histológico`)
RHC.MAMA.CO$`Tipo histológico`[RHC.MAMA.CO$`Tipo histológico` %in% c("/","8000/2","8010/","8100/3","8500/9","8503/","9713/3", "9990/3", "9991/6", "9991/7",
                                                                     "9992/0", "9992/2", "9992/6" ,NA)] <- "9999/9"
RHC.MAMA.CO$`Tipo histológico` <- factor(RHC.MAMA.CO$`Tipo histológico`)
table(RHC.MAMA.CO$`Tipo histológico`)

# Estadiamento

summary(RHC.MAMA.CO$`Estadiamento clínico do tumor (TNM)`) 
RHC.MAMA.CO$`Estadiamento clínico do tumor (TNM)`[RHC.MAMA.CO$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98", "33","9",NA)] <- "99"
RHC.MAMA.CO$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.MAMA.CO$`Estadiamento clínico do tumor (TNM)`)
table(RHC.MAMA.CO$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.MAMA.CO$TNM <- as.factor(RHC.MAMA.CO$TNM)
summary(RHC.MAMA.CO$TNM)
RHC.MAMA.CO$TNM[RHC.MAMA.CO$TNM %in% c("9", "00", "01", "1", "10", "2","20","21", "22","23", "3", "30", "31",
                                       "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.MAMA.CO$TNM)

# 999 Sem informação

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.MAMA.CO$`Lateralidade do tumor`) 
table(RHC.MAMA.CO$`Lateralidade do tumor`)
RHC.MAMA.CO$`Lateralidade do tumor`[RHC.MAMA.CO$`Lateralidade do tumor` == "0"] <- "9"
RHC.MAMA.CO$`Lateralidade do tumor` <- factor(RHC.MAMA.CO$`Lateralidade do tumor`)
table(RHC.MAMA.CO$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.MAMA.CO$`Data de início do tratamento` <- as.Date(RHC.MAMA.CO$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.MAMA.CO$`Data de início do tratamento`)
table(RHC.MAMA.CO$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.MAMA.CO$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.MAMA.CO$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.MAMA.CO$`Estado da doença ao final do tratamento`) 
table(RHC.MAMA.CO$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.MAMA.CO$`Data de óbito` <- as.Date(RHC.MAMA.CO$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.MAMA.CO$`Data de óbito`)
table(RHC.MAMA.CO$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.MAMA.CO$anoDIAG <- str_sub(RHC.MAMA.CO$`Data de diagnóstico`, start = 1, end = 4)
RHC.MAMA.CO$anoPRIMEIRACONS <- str_sub(RHC.MAMA.CO$`Data da primeira consulta`, start = 1, end = 4)
RHC.MAMA.CO$anoINITRATA <- str_sub(RHC.MAMA.CO$`Data de início do tratamento`, start = 1, end = 4)
RHC.MAMA.CO$anoOBITO <- str_sub(RHC.MAMA.CO$`Data de óbito`, start = 1, end = 4)

table(RHC.MAMA.CO$anoDIAG)
table(RHC.MAMA.CO$anoPRIMEIRACONS)
table(RHC.MAMA.CO$anoINITRATA)
table(RHC.MAMA.CO$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.MAMA.CO <- RHC.MAMA.CO%>%
  mutate_if(is.factor, replace_factor_na)

RHC.MAMA.CO$`Data da primeira consulta` <- lubridate::as_date(RHC.MAMA.CO$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.MAMA.CO$Ano <- year(RHC.MAMA.CO$`Data da primeira consulta`)
RHC.MAMA.CO$Ano
table(RHC.MAMA.CO$Ano)
RHC.MAMA.CO1 <- RHC.MAMA.CO[RHC.MAMA.CO$Ano >= 2008 & RHC.MAMA.CO$Ano <= 2016,]

# Definindo as categorias

RHC.MAMA.CO$Sexo <- ifelse(RHC.MAMA.CO$Sexo == "9", "Incompletude", "Completude")
RHC.MAMA.CO$`Raça/Cor da pele` <- ifelse(RHC.MAMA.CO$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.MAMA.CO$Escolaridade <- ifelse(RHC.MAMA.CO$Escolaridade == "9", "Incompletude", "Completude")
RHC.MAMA.CO$`Estado de residência` <- ifelse(RHC.MAMA.CO$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.MAMA.CO$Idade <- ifelse(RHC.MAMA.CO$Idade == "999", "Incompletude", "Completude")
RHC.MAMA.CO$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.MAMA.CO$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.MAMA.CO$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.MAMA.CO$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.CO$`Tipo histológico`<- ifelse(RHC.MAMA.CO$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.MAMA.CO$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.MAMA.CO$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.MAMA.CO$TNM <- ifelse(RHC.MAMA.CO$TNM == "999", "Incompletude", "Completude")
RHC.MAMA.CO$`Lateralidade do tumor` <- ifelse(RHC.MAMA.CO$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.CO$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.MAMA.CO$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.CO$`Estado da doença ao final do tratamento` <- ifelse(RHC.MAMA.CO$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.MAMA.CO$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.MAMA.CO$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.CO$`Data da primeira consulta` <- ifelse(is.na(RHC.MAMA.CO$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.MAMA.CO$`Data de diagnóstico` <- ifelse(is.na(RHC.MAMA.CO$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.MAMA.CO$`Data de início do tratamento` <- ifelse(is.na(RHC.MAMA.CO$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.MAMA.CO$`Data de óbito` <- ifelse(RHC.MAMA.CO$`Data de óbito` == "2022-03-10","Incompletude", "Completude")

rhc.completude <- RHC.MAMA.CO%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]

colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.mama.CO.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.mama.CO <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                                       caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer de mama" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.mama.CO

# SUDESTE


RHC.MAMA.SUDESTE <- read.xlsx("base_filtrada.xlsx")

RHC.MAMA.SUDESTE <- RHC.MAMA.SUDESTE%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.MAMA.SUDESTE <- RHC.MAMA.SUDESTE%>%
  filter(LOCTUDET == "C50")

# Analisando a completude das variáveis

names(RHC.MAMA.SUDESTE)[1] <- "Sexo"
names(RHC.MAMA.SUDESTE)[2] <- "Idade"
names(RHC.MAMA.SUDESTE)[3] <- "Raça/Cor da pele"
names(RHC.MAMA.SUDESTE)[4] <- "Escolaridade"
names(RHC.MAMA.SUDESTE)[5] <- "Estado de residência"
names(RHC.MAMA.SUDESTE)[6] <- "Data da primeira consulta"
names(RHC.MAMA.SUDESTE)[7] <- "Data de diagnóstico"
names(RHC.MAMA.SUDESTE)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.MAMA.SUDESTE)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.MAMA.SUDESTE)[10] <- "Localização primária do tumor"
names(RHC.MAMA.SUDESTE)[11] <- "Tipo histológico"
names(RHC.MAMA.SUDESTE)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.MAMA.SUDESTE)[13] <- "Lateralidade do tumor"
names(RHC.MAMA.SUDESTE)[14] <- "Data de início do tratamento"
names(RHC.MAMA.SUDESTE)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.MAMA.SUDESTE)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.MAMA.SUDESTE)[17] <- "Estado da doença ao final do tratamento"
names(RHC.MAMA.SUDESTE)[18] <- "Data de óbito"
names(RHC.MAMA.SUDESTE)[19] <- "TNM"

table(RHC.MAMA.SUDESTE$`Estado de residência`)

levels(RHC.MAMA.SUDESTE$`Estado de residência`)
RHC.MAMA.SUDESTE$`Estado de residência` <- as.factor(RHC.MAMA.SUDESTE$`Estado de residência`)

RHC.MAMA.SUDESTE$Regioes = ""
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "AC")] = "Norte"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "AM")] = "Norte"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "RR")] = "Norte"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "AP")] = "Norte"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "PA")] = "Norte"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "TO")] = "Norte"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "RO")] = "Norte"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "MA")] = "Nordeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "PI")] = "Nordeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "CE")] = "Nordeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "RN")] = "Nordeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "PE")] = "Nordeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "PB")] = "Nordeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "SE")] = "Nordeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "AL")] = "Nordeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "BA")] = "Nordeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "SP")] = "Sudeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "RJ")] = "Sudeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "ES")] = "Sudeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "MG")] = "Sudeste"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "PR")] = "Sul"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "RS")] = "Sul"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "SC")] = "Sul"
RHC.MAMA.SUDESTE$Regioes[str_detect(RHC.MAMA.SUDESTE$`Estado de residência`, "DF")] = "Centro-oeste"

table(RHC.MAMA.SUDESTE$Regioes)

# Região SUDESTE

RHC.MAMA.SUDESTE <- RHC.MAMA.SUDESTE%>%
  filter(Regioes == "Sudeste")

# Corrigindo a classificação das variáveis

RHC.MAMA.SUDESTE <- RHC.MAMA.SUDESTE.CO%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         TNM = as.factor(TNM),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.MAMA.SUDESTE$Sexo)
RHC.MAMA.SUDESTE$Sexo <- factor(RHC.MAMA.SUDESTE$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.MAMA.SUDESTE$Sexo)
RHC.MAMA.SUDESTE$Sexo[RHC.MAMA.SUDESTE$Sexo %in% c("0", "3")] <- "9"
RHC.MAMA.SUDESTE$Sexo <- factor(RHC.MAMA.SUDESTE$Sexo)
table(RHC.MAMA.SUDESTE$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.MAMA.SUDESTE$`Raça/Cor da pele`)
RHC.MAMA.SUDESTE$`Raça/Cor da pele`[RHC.MAMA.SUDESTE$`Raça/Cor da pele` == "99"] <- "9"
RHC.MAMA.SUDESTE$`Raça/Cor da pele` <- factor(RHC.MAMA.SUDESTE$`Raça/Cor da pele`)
table(RHC.MAMA.SUDESTE$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.MAMA.SUDESTE$Escolaridade)
RHC.MAMA.SUDESTE$Escolaridade <- factor(RHC.MAMA.SUDESTE$Escolaridade)
RHC.MAMA.SUDESTE$Escolaridade[RHC.MAMA.SUDESTE$Escolaridade == "0"] <- "9"
RHC.MAMA.SUDESTE$Escolaridade <- factor(RHC.MAMA.SUDESTE$Escolaridade)
table(RHC.MAMA.SUDESTE$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.MAMA.SUDESTE$`Estado de residência`)
RHC.MAMA.SUDESTE$`Estado de residência` <- factor(RHC.MAMA.SUDESTE$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                                      "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                                      "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.MAMA.SUDESTE$`Estado de residência`)
RHC.MAMA.SUDESTE$`Estado de residência`[RHC.MAMA.SUDESTE$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.MAMA.SUDESTE$`Estado de residência` <- factor(RHC.MAMA.SUDESTE$`Estado de residência`)
table(RHC.MAMA.SUDESTE$`Estado de residência`)

# Idade

RHC.MAMA.SUDESTE$Idade <- as.numeric(RHC.MAMA.SUDESTE$Idade)
table(RHC.MAMA.SUDESTE$Idade)
levels(RHC.MAMA.SUDESTE$Idade)
table(RHC.MAMA.SUDESTE$Idade)                                                     
RHC.MAMA.SUDESTE$Idade[RHC.MAMA.SUDESTE$Idade %in% c("000", NA)] <- "999"
RHC.MAMA.SUDESTE$Idade <- as.numeric(RHC.MAMA.SUDESTE$Idade)
table(RHC.MAMA.SUDESTE$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.MAMA.SUDESTE$`Data da primeira consulta` <- as.Date(RHC.MAMA.SUDESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.MAMA.SUDESTE$`Data da primeira consulta`)
table(RHC.MAMA.SUDESTE$`Data da primeira consulta`)
levels(RHC.MAMA.SUDESTE$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.MAMA.SUDESTE$`Data de diagnóstico` <- as.Date(RHC.MAMA.SUDESTE$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.MAMA.SUDESTE$`Data de diagnóstico`)
table(RHC.MAMA.SUDESTE$`Data de diagnóstico`) #NA
levels(RHC.MAMA.SUDESTE$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.MAMA.SUDESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.SUDESTE$`Diagnóstico e tratamento anteriores`)
RHC.MAMA.SUDESTE$`Diagnóstico e tratamento anteriores`[RHC.MAMA.SUDESTE$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.MAMA.SUDESTE$`Diagnóstico e tratamento anteriores` <- factor(RHC.MAMA.SUDESTE$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.SUDESTE$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.MAMA.SUDESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.SUDESTE$`Base mais importante para o diagnóstico do tumor`)
RHC.MAMA.SUDESTE$`Base mais importante para o diagnóstico do tumor`[RHC.MAMA.SUDESTE$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.MAMA.SUDESTE$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.MAMA.SUDESTE$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.SUDESTE$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.MAMA.SUDESTE$`Tipo histológico`) 
table(RHC.MAMA.SUDESTE$`Tipo histológico`)
RHC.MAMA.SUDESTE$`Tipo histológico`[RHC.MAMA.SUDESTE$`Tipo histológico` %in% c("/","8000/2","8010/","8100/3","8500/9","8503/","9713/3", "9990/3", "9991/6", "9991/7",
                                                                               "9992/0", "9992/2", "9992/6" ,NA)] <- "9999/9"
RHC.MAMA.SUDESTE$`Tipo histológico` <- factor(RHC.MAMA.SUDESTE$`Tipo histológico`)
table(RHC.MAMA.SUDESTE$`Tipo histológico`)

# Estadiamento

summary(RHC.MAMA.SUDESTE$`Estadiamento clínico do tumor (TNM)`) 
RHC.MAMA.SUDESTE$`Estadiamento clínico do tumor (TNM)`[RHC.MAMA.SUDESTE$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98", "33","9",NA)] <- "99"
RHC.MAMA.SUDESTE$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.MAMA.SUDESTE$`Estadiamento clínico do tumor (TNM)`)
table(RHC.MAMA.SUDESTE$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.MAMA.SUDESTE$TNM <- as.factor(RHC.MAMA.SUDESTE$TNM)
summary(RHC.MAMA.SUDESTE$TNM)
RHC.MAMA.SUDESTE$TNM[RHC.MAMA.SUDESTE$TNM %in% c("9", "00", "01", "1", "10", "2","20","21", "22","23", "3", "30", "31",
                                                 "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.MAMA.SUDESTE$TNM)

# 999 Sem informação

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.MAMA.SUDESTE$`Lateralidade do tumor`) 
table(RHC.MAMA.SUDESTE$`Lateralidade do tumor`)
RHC.MAMA.SUDESTE$`Lateralidade do tumor`[RHC.MAMA.SUDESTE$`Lateralidade do tumor` == "0"] <- "9"
RHC.MAMA.SUDESTE$`Lateralidade do tumor` <- factor(RHC.MAMA.SUDESTE$`Lateralidade do tumor`)
table(RHC.MAMA.SUDESTE$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.MAMA.SUDESTE$`Data de início do tratamento` <- as.Date(RHC.MAMA.SUDESTE$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.MAMA.SUDESTE$`Data de início do tratamento`)
table(RHC.MAMA.SUDESTE$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.MAMA.SUDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.MAMA.SUDESTE$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.MAMA.SUDESTE$`Estado da doença ao final do tratamento`) 
table(RHC.MAMA.SUDESTE$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.MAMA.SUDESTE$`Data de óbito` <- as.Date(RHC.MAMA.SUDESTE$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.MAMA.SUDESTE$`Data de óbito`)
table(RHC.MAMA.SUDESTE$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.MAMA.SUDESTE$anoDIAG <- str_sub(RHC.MAMA.SUDESTE$`Data de diagnóstico`, start = 1, end = 4)
RHC.MAMA.SUDESTE$anoPRIMEIRACONS <- str_sub(RHC.MAMA.SUDESTE$`Data da primeira consulta`, start = 1, end = 4)
RHC.MAMA.SUDESTE$anoINITRATA <- str_sub(RHC.MAMA.SUDESTE$`Data de início do tratamento`, start = 1, end = 4)
RHC.MAMA.SUDESTE$anoOBITO <- str_sub(RHC.MAMA.SUDESTE$`Data de óbito`, start = 1, end = 4)

table(RHC.MAMA.SUDESTE$anoDIAG)
table(RHC.MAMA.SUDESTE$anoPRIMEIRACONS)
table(RHC.MAMA.SUDESTE$anoINITRATA)
table(RHC.MAMA.SUDESTE$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.MAMA.SUDESTE <- RHC.MAMA.SUDESTE%>%
  mutate_if(is.factor, replace_factor_na)

RHC.MAMA.SUDESTE$`Data da primeira consulta` <- lubridate::as_date(RHC.MAMA.SUDESTE$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.MAMA.SUDESTE$Ano <- year(RHC.MAMA.SUDESTE$`Data da primeira consulta`)
RHC.MAMA.SUDESTE$Ano
table(RHC.MAMA.SUDESTE$Ano)
RHC.MAMA.SUDESTE1 <- RHC.MAMA.SUDESTE[RHC.MAMA.SUDESTE$Ano >= 2008 & RHC.MAMA.SUDESTE$Ano <= 2016,]

# Definindo as categorias

RHC.MAMA.SUDESTE$Sexo <- ifelse(RHC.MAMA.SUDESTE$Sexo == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Raça/Cor da pele` <- ifelse(RHC.MAMA.SUDESTE$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$Escolaridade <- ifelse(RHC.MAMA.SUDESTE$Escolaridade == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Estado de residência` <- ifelse(RHC.MAMA.SUDESTE$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$Idade <- ifelse(RHC.MAMA.SUDESTE$Idade == "999", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.MAMA.SUDESTE$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.MAMA.SUDESTE$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Tipo histológico`<- ifelse(RHC.MAMA.SUDESTE$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.MAMA.SUDESTE$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$TNM <- ifelse(RHC.MAMA.SUDESTE$TNM == "999", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Lateralidade do tumor` <- ifelse(RHC.MAMA.SUDESTE$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.MAMA.SUDESTE$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Estado da doença ao final do tratamento` <- ifelse(RHC.MAMA.SUDESTE$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.MAMA.SUDESTE$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Data da primeira consulta` <- ifelse(is.na(RHC.MAMA.SUDESTE$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Data de diagnóstico` <- ifelse(is.na(RHC.MAMA.SUDESTE$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Data de início do tratamento` <- ifelse(is.na(RHC.MAMA.SUDESTE$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.MAMA.SUDESTE$`Data de óbito` <- ifelse(RHC.MAMA.SUDESTE$`Data de óbito` == "2022-03-10","Incompletude", "Completude")

rhc.completude <- RHC.MAMA.SUDESTE%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]

colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.mama.SUDESTE.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.mama.SUDESTE <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                                            caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer de mama" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.mama.SUDESTE

# SUL

RHC.MAMA.SUL <- read.xlsx("base_filtrada.xlsx")

RHC.MAMA.SUL <- RHC.MAMA.SUL%>%
  select(SEXO, IDADE, RACACOR, INSTRUC, ESTADRES, DATAPRICON, DTDIAGNO, DIAGANT, BASMAIMP,
         LOCTUDET, TIPOHIST, ESTADIAM, LATERALI, DATAINITRT, RZNTR, PRITRATH, ESTDFIMT,
         DATAOBITO, TNM)

RHC.MAMA.SUL <- RHC.MAMA.SUL%>%
  filter(LOCTUDET == "C50")

# Analisando a completude das variáveis

names(RHC.MAMA.SUL)[1] <- "Sexo"
names(RHC.MAMA.SUL)[2] <- "Idade"
names(RHC.MAMA.SUL)[3] <- "Raça/Cor da pele"
names(RHC.MAMA.SUL)[4] <- "Escolaridade"
names(RHC.MAMA.SUL)[5] <- "Estado de residência"
names(RHC.MAMA.SUL)[6] <- "Data da primeira consulta"
names(RHC.MAMA.SUL)[7] <- "Data de diagnóstico"
names(RHC.MAMA.SUL)[8] <- "Diagnóstico e tratamento anteriores"
names(RHC.MAMA.SUL)[9] <- "Base mais importante para o diagnóstico do tumor"
names(RHC.MAMA.SUL)[10] <- "Localização primária do tumor"
names(RHC.MAMA.SUL)[11] <- "Tipo histológico"
names(RHC.MAMA.SUL)[12] <- "Estadiamento clínico do tumor (TNM)"
names(RHC.MAMA.SUL)[13] <- "Lateralidade do tumor"
names(RHC.MAMA.SUL)[14] <- "Data de início do tratamento"
names(RHC.MAMA.SUL)[15] <- "Principal razão para a não realização do tratamento antineoplásico no hospital"
names(RHC.MAMA.SUL)[16] <- "Primeiro tratamento recebido no hospital"
names(RHC.MAMA.SUL)[17] <- "Estado da doença ao final do tratamento"
names(RHC.MAMA.SUL)[18] <- "Data de óbito"
names(RHC.MAMA.SUL)[19] <- "TNM"

table(RHC.MAMA.SUL$`Estado de residência`)

levels(RHC.MAMA.SUL$`Estado de residência`)
RHC.MAMA.SUL$`Estado de residência` <- as.factor(RHC.MAMA.SUL$`Estado de residência`)

RHC.MAMA.SUL$Regioes = ""
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "AC")] = "Norte"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "AM")] = "Norte"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "RR")] = "Norte"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "AP")] = "Norte"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "PA")] = "Norte"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "TO")] = "Norte"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "RO")] = "Norte"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "MA")] = "Nordeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "PI")] = "Nordeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "CE")] = "Nordeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "RN")] = "Nordeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "PE")] = "Nordeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "PB")] = "Nordeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "SE")] = "Nordeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "AL")] = "Nordeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "BA")] = "Nordeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "MT")] = "Centro-oeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "MS")] = "Centro-oeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "GO")] = "Centro-oeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "SP")] = "Sudeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "RJ")] = "Sudeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "ES")] = "Sudeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "MG")] = "Sudeste"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "PR")] = "Sul"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "RS")] = "Sul"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "SC")] = "Sul"
RHC.MAMA.SUL$Regioes[str_detect(RHC.MAMA.SUL$`Estado de residência`, "DF")] = "Centro-oeste"

table(RHC.MAMA.SUL$Regioes)

# Região SUDESTE

RHC.MAMA.SUL <- RHC.MAMA.SUL%>%
  filter(Regioes == "Sul")

# Corrigindo a classificação das variáveis

RHC.MAMA.SUL <- RHC.MAMA.SUL.CO%>%
  mutate(Sexo = as.factor(Sexo),
         #Idade = as.numeric(Idade),
         `Raça/Cor da pele` = as.factor(`Raça/Cor da pele`),
         `Estado de residência` = as.factor(`Estado de residência`),
         `Diagnóstico e tratamento anteriores` = as.factor(`Diagnóstico e tratamento anteriores`),
         `Base mais importante para o diagnóstico do tumor` = as.factor(`Base mais importante para o diagnóstico do tumor`),
         `Localização primária do tumor` = as.factor(`Localização primária do tumor`),
         `Tipo histológico` = as.factor(`Tipo histológico`),
         `Estadiamento clínico do tumor (TNM)` = as.factor(`Estadiamento clínico do tumor (TNM)`),
         `Lateralidade do tumor` = as.factor(`Lateralidade do tumor`),
         `Principal razão para a não realização do tratamento antineoplásico no hospital` = as.factor(`Principal razão para a não realização do tratamento antineoplásico no hospital`),
         `Primeiro tratamento recebido no hospital` = as.factor(`Primeiro tratamento recebido no hospital`),
         TNM = as.factor(TNM),
         `Estado da doença ao final do tratamento` = as.factor(`Estado da doença ao final do tratamento`))

# Transferindo todas as inequivalencias (divergências no preencimento de acordo com o dicionário) nas variáveis factor para o 9

# Bloco de identificação do paciente

# Sexo, Raça/cor da pele, escolaridade e UF de procedencia

# Sexo - 1 = Masculino e 2 = Feminino | necessário criar o 9

summary(RHC.MAMA.SUL$Sexo)
RHC.MAMA.SUL$Sexo <- factor(RHC.MAMA.SUL$Sexo, levels = c("0", "1", "2", "3", "9"))
table(RHC.MAMA.SUL$Sexo)
RHC.MAMA.SUL$Sexo[RHC.MAMA.SUL$Sexo %in% c("0", "3")] <- "9"
RHC.MAMA.SUL$Sexo <- factor(RHC.MAMA.SUL$Sexo)
table(RHC.MAMA.SUL$Sexo)

# Raça/cor da pele - 1 = Branca, 2 = Preta, 3 = Amarela, 4 = Parda, 5 = Indígena, 9 =Sem informação

summary(RHC.MAMA.SUL$`Raça/Cor da pele`)
RHC.MAMA.SUL$`Raça/Cor da pele`[RHC.MAMA.SUL$`Raça/Cor da pele` == "99"] <- "9"
RHC.MAMA.SUL$`Raça/Cor da pele` <- factor(RHC.MAMA.SUL$`Raça/Cor da pele`)
table(RHC.MAMA.SUL$`Raça/Cor da pele`)

# Escolaridade -  1.Nenhuma; 2.Fundamental incompleto; 3.Fundamental completo; 4.Nível médio; 5.Nível superior incompleto; 6.Nível superior completo; 9.Sem informação

summary(RHC.MAMA.SUL$Escolaridade)
RHC.MAMA.SUL$Escolaridade <- factor(RHC.MAMA.SUL$Escolaridade)
RHC.MAMA.SUL$Escolaridade[RHC.MAMA.SUL$Escolaridade == "0"] <- "9"
RHC.MAMA.SUL$Escolaridade <- factor(RHC.MAMA.SUL$Escolaridade)
table(RHC.MAMA.SUL$Escolaridade)

# Estado de Residência 
# 77 e 99 inconsistencias - criar 9 (sem informação)
summary(RHC.MAMA.SUL$`Estado de residência`)
RHC.MAMA.SUL$`Estado de residência` <- factor(RHC.MAMA.SUL$`Estado de residência`, levels = c("77","99","AC",  "AL",  "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                                                                                              "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", 
                                                                                              "RR", "RS", "SC",  "SE", "SP", "TO", "9"))
table(RHC.MAMA.SUL$`Estado de residência`)
RHC.MAMA.SUL$`Estado de residência`[RHC.MAMA.SUL$`Estado de residência` %in% c("77", "99")] <- "9"
RHC.MAMA.SUL$`Estado de residência` <- factor(RHC.MAMA.SUL$`Estado de residência`)
table(RHC.MAMA.SUL$`Estado de residência`)

# Idade

RHC.MAMA.SUL$Idade <- as.numeric(RHC.MAMA.SUL$Idade)
table(RHC.MAMA.SUL$Idade)
levels(RHC.MAMA.SUL$Idade)
table(RHC.MAMA.SUL$Idade)                                                     
RHC.MAMA.SUL$Idade[RHC.MAMA.SUL$Idade %in% c("000", NA)] <- "999"
RHC.MAMA.SUL$Idade <- as.numeric(RHC.MAMA.SUL$Idade)
table(RHC.MAMA.SUL$Idade)

# Bloco de caracterização do diagnóstico-----------------------------------------

#Data da primeira consulta, Data do primeiro diagnóstico, Diagnóstico e tratamento anteriores e 
#Base mais importante para o diagnóstico do tumor

# Data da primeira consulta

RHC.MAMA.SUL$`Data da primeira consulta` <- as.Date(RHC.MAMA.SUL$`Data da primeira consulta`, format = "%d/%m/%Y")
summary(RHC.MAMA.SUL$`Data da primeira consulta`)
table(RHC.MAMA.SUL$`Data da primeira consulta`)
levels(RHC.MAMA.SUL$`Data da primeira consulta`)

# Data do primeiro diagnóstico

RHC.MAMA.SUL$`Data de diagnóstico` <- as.Date(RHC.MAMA.SUL$`Data de diagnóstico`, format = "%d/%m/%Y")
summary(RHC.MAMA.SUL$`Data de diagnóstico`)
table(RHC.MAMA.SUL$`Data de diagnóstico`) #NA
levels(RHC.MAMA.SUL$`Data da primeira consulta`)

#Diagnóstico e tratamento anteriores

# 1.Sem diag./Sem trat.; 2.Com diag./Sem trat.; 3.Com diag./Com trat.; 4.Outros; 9. Sem informação

summary(RHC.MAMA.SUL$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.SUL$`Diagnóstico e tratamento anteriores`)
RHC.MAMA.SUL$`Diagnóstico e tratamento anteriores`[RHC.MAMA.SUL$`Diagnóstico e tratamento anteriores` == "0"] <- "9"
RHC.MAMA.SUL$`Diagnóstico e tratamento anteriores` <- factor(RHC.MAMA.SUL$`Diagnóstico e tratamento anteriores`)
table(RHC.MAMA.SUL$`Diagnóstico e tratamento anteriores`)

#Base mais importante para o diagnóstico do tumor

#1.Clínica; 2.Pesquisa clínica; 3.Exame por imagem; 4.Marcadores tumorais; 5.Citologia; 6.Histologia da metástase; 7.Histologia do tumor primário; 9.Sem informação

summary(RHC.MAMA.SUL$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.SUL$`Base mais importante para o diagnóstico do tumor`)
RHC.MAMA.SUL$`Base mais importante para o diagnóstico do tumor`[RHC.MAMA.SUL$`Base mais importante para o diagnóstico do tumor` == "0"] <- "9"
RHC.MAMA.SUL$`Base mais importante para o diagnóstico do tumor` <- factor(RHC.MAMA.SUL$`Base mais importante para o diagnóstico do tumor`)
table(RHC.MAMA.SUL$`Base mais importante para o diagnóstico do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do tumor-----------------------------------------------

#Localização primária, Tipo histológico, Estadiamento clínico do tumor (TNM) e 
#Lateralidade do tumor

# Tipo histologico

summary(RHC.MAMA.SUL$`Tipo histológico`) 
table(RHC.MAMA.SUL$`Tipo histológico`)
RHC.MAMA.SUL$`Tipo histológico`[RHC.MAMA.SUL$`Tipo histológico` %in% c("/","8000/2","8010/","8100/3","8500/9","8503/","9713/3", "9990/3", "9991/6", "9991/7",
                                                                       "9992/0", "9992/2", "9992/6" ,NA)] <- "9999/9"
RHC.MAMA.SUL$`Tipo histológico` <- factor(RHC.MAMA.SUL$`Tipo histológico`)
table(RHC.MAMA.SUL$`Tipo histológico`)

# Estadiamento

summary(RHC.MAMA.SUL$`Estadiamento clínico do tumor (TNM)`) 
RHC.MAMA.SUL$`Estadiamento clínico do tumor (TNM)`[RHC.MAMA.SUL$`Estadiamento clínico do tumor (TNM)` %in% c("00", "98", "33","9",NA)] <- "99"
RHC.MAMA.SUL$`Estadiamento clínico do tumor (TNM)` <- factor(RHC.MAMA.SUL$`Estadiamento clínico do tumor (TNM)`)
table(RHC.MAMA.SUL$`Estadiamento clínico do tumor (TNM)`)

# tnm

RHC.MAMA.SUL$TNM <- as.factor(RHC.MAMA.SUL$TNM)
summary(RHC.MAMA.SUL$TNM)
RHC.MAMA.SUL$TNM[RHC.MAMA.SUL$TNM %in% c("9", "00", "01", "1", "10", "2","20","21", "22","23", "3", "30", "31",
                                         "32", "33", "4", "40", "41", "43", "99")] <- "999"
table(RHC.MAMA.SUL$TNM)

# 999 Sem informação

# Lateralidade

# 1.Direita; 2. Esquerda; 3.Bilateral; 8.Não se aplica; 9.Sem informação

summary(RHC.MAMA.SUL$`Lateralidade do tumor`) 
table(RHC.MAMA.SUL$`Lateralidade do tumor`)
RHC.MAMA.SUL$`Lateralidade do tumor`[RHC.MAMA.SUL$`Lateralidade do tumor` == "0"] <- "9"
RHC.MAMA.SUL$`Lateralidade do tumor` <- factor(RHC.MAMA.SUL$`Lateralidade do tumor`)
table(RHC.MAMA.SUL$`Lateralidade do tumor`)

#--------------------------------------------------------------------------------

# Bloco de Caracterização do primeiro tratamento---------------------------------

#Data do início do primeiro tratamento, Principal razão para a não realização do tratamento, 
#Primeiro tratamento recebido, Estado da doença ao final do primeiro tratamento e Data do óbito

#Data do início do primeiro tratamento

RHC.MAMA.SUL$`Data de início do tratamento` <- as.Date(RHC.MAMA.SUL$`Data de início do tratamento`, format = "%d/%m/%Y")
summary(RHC.MAMA.SUL$`Data de início do tratamento`)
table(RHC.MAMA.SUL$`Data de início do tratamento`) #NA

# Principal razão para a não realização do tratamento

#1.Recusa do tratamento; 2.Tratamento realizado fora; 3.Doença avançada, falta de condições clínicas ou outras doenças associadas; 4.Abandono do tratamento; 5.Complicações de tratamento; 6.Óbito; 7.Outras razões; 8.Não se aplica; 9. Sem informação

summary(RHC.MAMA.SUL$`Principal razão para a não realização do tratamento antineoplásico no hospital`) 

# Tudo certo nesse

#Primeiro tratamento recebido

summary(RHC.MAMA.SUL$`Primeiro tratamento recebido no hospital`) #Conferir

#Estado da doença ao final do primeiro tratamento

#1.Sem evidência da doença (remissão completa); 2.Remissão parcial; 3.Doença estável; 4.Doença em progressão; 5.Suporte terapêutico oncológico; 6. Óbito; 8. Não se aplica; 9. Sem informação

summary(RHC.MAMA.SUL$`Estado da doença ao final do tratamento`) 
table(RHC.MAMA.SUL$`Estado da doença ao final do tratamento`) #Certo

# Data do óbito

RHC.MAMA.SUL$`Data de óbito` <- as.Date(RHC.MAMA.SUL$`Data de óbito`, format = "%d/%m/%Y")
summary(RHC.MAMA.SUL$`Data de óbito`)
table(RHC.MAMA.SUL$`Data de óbito`) #NA

# Conferindo se tem a informação 99/99/9999 (sem informação)

RHC.MAMA.SUL$anoDIAG <- str_sub(RHC.MAMA.SUL$`Data de diagnóstico`, start = 1, end = 4)
RHC.MAMA.SUL$anoPRIMEIRACONS <- str_sub(RHC.MAMA.SUL$`Data da primeira consulta`, start = 1, end = 4)
RHC.MAMA.SUL$anoINITRATA <- str_sub(RHC.MAMA.SUL$`Data de início do tratamento`, start = 1, end = 4)
RHC.MAMA.SUL$anoOBITO <- str_sub(RHC.MAMA.SUL$`Data de óbito`, start = 1, end = 4)

table(RHC.MAMA.SUL$anoDIAG)
table(RHC.MAMA.SUL$anoPRIMEIRACONS)
table(RHC.MAMA.SUL$anoINITRATA)
table(RHC.MAMA.SUL$anoOBITO)

#--------------------------------------------------------------------------------

# Criando completude e incompletude

############ TROCANDO NA POR 9 ############################

replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "9", x)
  x <- as.factor(x)
}

RHC.MAMA.SUL <- RHC.MAMA.SUL%>%
  mutate_if(is.factor, replace_factor_na)

RHC.MAMA.SUL$`Data da primeira consulta` <- lubridate::as_date(RHC.MAMA.SUL$`Data da primeira consulta`, format = "%d/%m/%Y")
library(lubridate)
RHC.MAMA.SUL$Ano <- year(RHC.MAMA.SUL$`Data da primeira consulta`)
RHC.MAMA.SUL$Ano
table(RHC.MAMA.SUL$Ano)
RHC.MAMA.SUL1 <- RHC.MAMA.SUL[RHC.MAMA.SUL$Ano >= 2008 & RHC.MAMA.SUL$Ano <= 2016,]

# Definindo as categorias

RHC.MAMA.SUL$Sexo <- ifelse(RHC.MAMA.SUL$Sexo == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$`Raça/Cor da pele` <- ifelse(RHC.MAMA.SUL$`Raça/Cor da pele` == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$Escolaridade <- ifelse(RHC.MAMA.SUL$Escolaridade == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$`Estado de residência` <- ifelse(RHC.MAMA.SUL$`Estado de residência` == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$Idade <- ifelse(RHC.MAMA.SUL$Idade == "999", "Incompletude", "Completude")
RHC.MAMA.SUL$`Diagnóstico e tratamento anteriores` <- ifelse(RHC.MAMA.SUL$`Diagnóstico e tratamento anteriores` == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$`Base mais importante para o diagnóstico do tumor` <- ifelse(RHC.MAMA.SUL$`Base mais importante para o diagnóstico do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$`Tipo histológico`<- ifelse(RHC.MAMA.SUL$`Tipo histológico` == "9999/9", "Incompletude", "Completude")
RHC.MAMA.SUL$`Estadiamento clínico do tumor (TNM)` <- ifelse(RHC.MAMA.SUL$`Estadiamento clínico do tumor (TNM)` == "99", "Incompletude", "Completude")
RHC.MAMA.SUL$TNM <- ifelse(RHC.MAMA.SUL$TNM == "999", "Incompletude", "Completude")
RHC.MAMA.SUL$`Lateralidade do tumor` <- ifelse(RHC.MAMA.SUL$`Lateralidade do tumor` == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$`Principal razão para a não realização do tratamento antineoplásico no hospital` <- ifelse(RHC.MAMA.SUL$`Principal razão para a não realização do tratamento antineoplásico no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$`Estado da doença ao final do tratamento` <- ifelse(RHC.MAMA.SUL$`Estado da doença ao final do tratamento` == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$`Primeiro tratamento recebido no hospital` <- ifelse(RHC.MAMA.SUL$`Primeiro tratamento recebido no hospital` == "9", "Incompletude", "Completude")
RHC.MAMA.SUL$`Data da primeira consulta` <- ifelse(is.na(RHC.MAMA.SUL$`Data da primeira consulta`),"Incompletude", "Completude")
RHC.MAMA.SUL$`Data de diagnóstico` <- ifelse(is.na(RHC.MAMA.SUL$`Data de diagnóstico`),"Incompletude", "Completude")
RHC.MAMA.SUL$`Data de início do tratamento` <- ifelse(is.na(RHC.MAMA.SUL$`Data de início do tratamento`),"Incompletude", "Completude")
RHC.MAMA.SUL$`Data de óbito` <- ifelse(RHC.MAMA.SUL$`Data de óbito` == "2022-03-10","Incompletude", "Completude")

rhc.completude <- RHC.MAMA.SUL%>%
  group_by(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
           `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
           `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
           `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`,.drop = F)%>%
  select(Sexo, `Raça/Cor da pele`, Escolaridade, `Estado de residência`, Idade, `Data da primeira consulta`, `Data de diagnóstico`,`Diagnóstico e tratamento anteriores`,
         `Base mais importante para o diagnóstico do tumor`, `Tipo histológico`, `Estadiamento clínico do tumor (TNM)`, TNM,
         `Lateralidade do tumor`,`Data de início do tratamento`,`Principal razão para a não realização do tratamento antineoplásico no hospital`,
         `Primeiro tratamento recebido no hospital`,`Estado da doença ao final do tratamento`, `Data de óbito`, Ano)

rhc.completude = melt (rhc.completude, id = c("Ano"), variable.name = "variable.name", value.name = "Total")

rhc.completude$completude[rhc.completude$Total == "Completude"] <- "Completude"
rhc.completude$Incompletude[rhc.completude$Total == "Incompletude"] <- "Incompletude"

tabela.completude <- rhc.completude%>%
  group_by(Ano, variable.name, Total,.drop = F)%>%
  arrange(Ano)%>%
  tally()

tabela.completude1 <- tabela.completude%>%
  filter(Total == "Completude")%>% 
  select(Ano,variable.name, n)

tabela.completude1$Completude <- rep("Completude")

tabela.incompletude <- tabela.completude%>%
  filter(Total == "Incompletude")%>% 
  select(Ano,variable.name, n)

tabela.incompletude$Incompletude <- rep("Incompletude")

tabela.geral <- left_join(tabela.completude1, tabela.incompletude, by = c("Ano","variable.name"))

tabela.geral<- tabela.geral%>%
  mutate_all(replace_na, 0)

tabela.geral$total <- round(tabela.geral$n.x + tabela.geral$n.y)

tabela.completude <- tabela.geral%>%
  group_by(Ano, variable.name, n.y, total)%>%
  arrange(Ano)%>%
  tally()

tabela.completude <- tabela.completude[,c(-5)]

colnames(tabela.completude)[1:4] <- c("Ano", "Variáveis", "N incompletude", "Total")

tabela.completude$`% Incompletude` <- (tabela.completude$`N incompletude` / (tabela.completude$Total)*100)
tabela.completude$`% Incompletude` <- round(tabela.completude$`% Incompletude`, 2)

tabela.completude$`totaldados` <- rep("100")

tabela.completude$totaldados <- as.numeric(tabela.completude$totaldados)

tabela.completude$completude <- tabela.completude$totaldados  - tabela.completude$`% Incompletude`

write.csv(tabela.completude, "tabela.completude.mama.SUL.csv")

tabela.completa <- tabela.completude%>%
  select(Ano, Variáveis, completude)

tabela.completa <- tabela.completa[,c(-1)]

tabela.final <- tabela.completa%>%
  pivot_wider(names_from = Ano,
              values_from = completude)%>%
  unnest(cols = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

tab.completude.mama.SUL <- knitr::kable(tabela.final, align = rep(c('l','c','c')),  format.args = list(decimal.mark = ',', big.mark = "."),
                                        caption = "") %>%
  kable_classic(full_width = F, html_font = "Calibri") %>%
  column_spec(1, bold = T)%>%
  row_spec(0, bold=T) %>%
  #row_spec(6, bold=T)%>%
  add_header_above(c(" " = 1, "Câncer de mama" = 9))%>%
  footnote(general = "IntegradorRHC", general_title = "Fonte de dados:")

tab.completude.mama.SUL

tab.completude.mama
tab.completude.mama.norte
tab.completude.mama.nordeste
tab.completude.mama.CO
tab.completude.mama.SUDESTE
tab.completude.mama.SUL
