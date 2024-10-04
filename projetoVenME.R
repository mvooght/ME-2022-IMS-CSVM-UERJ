#Etapa preeliminar---------
##PACOTES----

#CARREGAR PACOTES
#especificar a biblioteca para a extensão do arquivo
#library(readr)#ler os arquivos em CSV
#library(data.table)
library(dplyr)
#install.packages("tidyverse", dependencies = TRUE )
#install.packages("readr")
library(tidyverse)
#install.packages("janitor")
library(janitor)# pacote de tabelas
library(devtools)
#remotes::install_github('rstudio/DT')
#if (!require("DT")) install.packages('DT')
#xfun::session_info('DT')
#library(tibble)
library('DT')
library(readxl)#ler os arquivos do excel
#library(epiDisplay)#Pacote de dados epidemiológicos
#install.packages("ludibriate")
library(lubridate)#Pacote para manejar datas
#install.packages("forcats")
library(forcats)
#require(ggplot2)

##ABRINDO BANCO-----
#Abrindo o banco de dados

#caminho no pc
#setwd("G:/Meu Drive/Defesa Dissertação ME/Parte quantitativa/Planilhas/Bancos")#vem do google drive no pc


setwd("K:/Meu Drive/Defesa Dissertação ME/Parte quantitativa/Planilhas/Bancos")

#vamos abrir o banco de dados?
#csv
#banco_geral<- read.csv("ProjetoVenezuelanos_RAW DATA_ate10.mai.2021_bancogeral_v5.csv")

#xlsx
library(readxl)
banco_geral=read_excel("ProjetoVenezuelanos_RAW DATA_ate10.mai.2021_bancogeral_v7_xlsx.xlsx")

#lembrar do attach, senão usar $
#attach(banco_geral)

#vamos fazer alguams verificações?

is.na.data.frame(banco_geral)# verifica dados faltantes na planilha
head(banco_geral)# verifica os cabecalhos
dim(banco_geral) #verifica o tamanho da planilha linhas x colunas
length (banco_geral) #numero de colunas 
str(dados) #verifica as categorias das variaveis
names(banco_geral) #retorna com os nomes das variaveis

#Para conferir o banco, retirar o # da linha abaixo.
##View()


##LIMPANDO O BANCO DE DADOS----

#LIMPANDO O BANCO DE DADOS - CONFERINDO OS CRITÉRIOS DE ELEGIBILIDADE

#DELETANDO OS INCOMPLETOS, ou seja todos aqueles com o zero na variavel abaixo.
dados <- banco_geral[banco_geral$questionario_final_es_complete!=0,]


#CRIADO O BANCO DADOS - QUE SERÁ UTILIZADO PARA ANALISES.

#ADOTANDO OS CRITÉRIO DE EXCLUSÃO

#TCLE= ACEITE
##View(tcle_es)
summary(dados$tcle_es)
str(dados$tcle_es)
which(dados$tcle_es == 2)

table(dados$tcle_es)
dados$tcle_es=factor(dados$tcle_es, levels = c(1,2), labels = c("sim", "não"))
summary(dados$tcle_es)
which(dados$tcle_es == "não")
##View(dados$tcle_es)

#excluindo todos aqueles que não deram aceite, mas enviaram o questionário.
dados <- dados[dados$tcle_es!="não",]

DT::datatable(tabyl(dados$tcle_es)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#COMPREENSÃO= RESP_SOZ
#Quem não está apto ou capaz de preencher o questionário sozinho
summary(dados$resp_soz_es)
str(dados$resp_soz_es)
which(dados$resp_soz_es == 2)

table(dados$resp_soz_es)
dados$resp_soz_es=factor(dados$resp_soz_es, levels = c(1,2), labels = c("sim", "não"))
summary(dados$resp_soz_es)
which(dados$resp_soz_es == "não")
##View(dados$resp_soz_es)

#excluindo todos aqueles que não se sentiram aptos a preencher o questionário sozinho

dados <- dados[dados$resp_soz_es!="não",]

DT::datatable(tabyl(dados$resp_soz_es)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

table(dados$resp_soz_es)

#NACIONALIDADE
#Quem não é nacional da venezuela
summary(dados$nacion_es)
str(dados$nacion_es)
which(dados$nacion_es == 2)

table(dados$nacion_es)
dados$nacion_es=factor(dados$nacion_es, levels = c(1,2), labels = c("sim", "não"))
summary(dados$nacion_es)
which(dados$nacion_es == "não")
##View(dados$nacion_es)

#excluindo todos aqueles que não são venezuelanos

dados <- dados[dados$nacion_es!="não",]

DT::datatable(tabyl(dados$nacion_es)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

table(dados$nacion_es)

#o banco final deverá ter 318 respondentes!


#EXCLUINDO DUAS COLUNAS INUTEIS
##View(dados)
dados<-dados[,-c(2,115,130)]

#exclui a coluna de identificação (o questionário é anonimo), uma coluna que no formulário consistem em instruções e a coluna dos comentários.



#ultimo confere
##View(dados)
dim(dados)#verifica o tamanho da planilha linhas x colunas
dim(banco_geral)#verifica o tamanho da planilha linhas x colunas

#AGORA TEMOS O BANCO FINAL! = N=318

##INICIO DAS ANALISES-----
#vamos carregar o banco de dados?

#lembrar do attach e do detach, senão usar $
attach(dados)

###CHECKLIST##### 
#Vamos começar nossas analises?

#TCLE
##View(tcle_es)
summary(tcle_es)

DT::datatable(tabyl(tcle_es)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#SEXO
summary(sex_es)
sexo=factor(sex_es, levels = c(1, 2), labels = c("feminino", "masculino"))
table(sexo)
prop.table(table(sexo))*100

DT::datatable(tabyl(sexo)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=207,n=318, conf.level = 0.95)#mulheres 
prop.test(x=111,n=318, conf.level = 0.95)#homens

#IDADE
summary(idade_es)#media da idade
sd(idade_es, na.rm=TRUE)#desvio padrão da idade
tapply(idade_es,sexo,summary)#media da idade por sexo
tapply(idade_es,sexo,sd)#desvio padrão da idade por sexo

#SITUAÇÃO MIGRATÓRIA
summary(situmigra_es)
situmigra=factor(situmigra_es, levels = c(1:7), labels = c("Solicitante de refúgio",  "Refugiado",  "Solicitante de autorização de residencia",  "Permissão de residencia por prazo determinado",  "Permissão de residencia por prazo indeterminado",  "Sem documentos",  "Outro"), ordered = F)

#situmigra2=factor(situmigra_es, levels = c(1:7), labels = c("Solicitante de refúgio",  "Refugiado",  "Solicitante de autorização de residencia",  "Permissão de residencia por plazo determinado",  "Permissão de residencia por plazo indeterminado",  "Sem documentos",  "Outro"), ordered = F) #fiz não ordenada, para que o R não faça analises erradas nas regressões ver em: https://stats.stackexchange.com/questions/233455/interpretation-of-l-q-output-from-a-negative-binomial-glm-with-categorical-d

#IC
prop.test(x=25,n=318, conf.level = 0.95)#Solicitante de refugio
prop.test(x=49,n=318, conf.level = 0.95)#Refugiado
prop.test(x=37,n=318, conf.level = 0.95)#Solicitante de aut. residência
prop.test(x=120,n=318, conf.level = 0.95)#Residencia temporária
prop.test(x=51,n=318, conf.level = 0.95)#Residencia Identerminado
prop.test(x=36,n=318, conf.level = 0.95)#Indocumentado

#summary(situmigra)

DT::datatable(tabyl(situmigra)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

DT::datatable(tabyl(situmigra2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

tab1(situmigra, decimal = 1, sort.group = "decreasing", 
     cum.percent = !any(is.na(situmigra)), graph = F, 
     missing = TRUE, bar.values = "percent", 
     horiz = T, cex = 1, cex.names = 1, main = "auto", xlab = "auto", 
     ylab = "auto", col = "auto", gen.ind.vars = FALSE)

#ESTADO RESIDENCIA
summary(estado_es)
estado=factor(estado_es, levels = c(1:27), labels = c( "Acre (AC)",
                                                       "Alagoas (AL)",
                                                       "Amapá (AP)",
                                                       "Amazonas (AM)",
                                                       "Bahia (BA)",
                                                       "Ceará (CE)",
                                                       "Distrito Federal  (DF)",
                                                       "Espírito Santo (ES)",
                                                       "Goiás (GO)",
                                                       "Maranhão (MA)",
                                                       "Mato Grosso (MT)",
                                                       "Mato Grosso do Sul (MS)",
                                                       "Minas Gerais (MG)",
                                                       "Pará (PA)",
                                                       "Paraíba (PB)",
                                                       "Paraná (PR)",
                                                       "Pernambuco (PE)",
                                                       "Piauí (PI)",
                                                       "Rio de Janeiro (RJ)",
                                                       "Rio Grande do Norte (RN)",
                                                       "Rio Grande do Sul (RS)",
                                                       "Rondônia (RO)",
                                                       "Roraima (RR)",
                                                       "Santa Catarina (SC)",
                                                       "São Paulo (SP)",
                                                       "Sergipe (SE)",
                                                       "Tocantins (TO)"))

summary(estado)
##View(estado)
DT::datatable(tabyl(estado)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

est.dec= tab1(estado, decimal = 1, sort.group = "decreasing", 
              cum.percent = !any(is.na(situmigra_es)), graph = F, 
              missing = TRUE, bar.values = "frequency", 
              horiz = F, cex = 1, cex.names = 1, main = "Venezuelanos por UF do Brasil",  xlab = "Frequência por UF", 
              ylab = "UF", col = "auto", cex.main=1, cex.name=1, cex.axis=1, cex.lab=1, gen.ind.vars = F)


tab1(estado, sort.group = "decreasing", cum.percent = T, graph = F)

#IC - em ordem decrescente
prop.test(x=63,n=318, conf.level = 0.95)#RR
prop.test(x=51,n=318, conf.level = 0.95)#AM
prop.test(x=41,n=318, conf.level = 0.95)#RJ
prop.test(x=36,n=318, conf.level = 0.95)#SP
prop.test(x=28,n=318, conf.level = 0.95)#PR
prop.test(x=26,n=318, conf.level = 0.95)#RS
prop.test(x=22,n=318, conf.level = 0.95)#SC
prop.test(x=9,n=318, conf.level = 0.95)#BA
prop.test(x=8,n=318, conf.level = 0.95)#MG e PB
prop.test(x=7,n=318, conf.level = 0.95)#DF
prop.test(x=4,n=318, conf.level = 0.95)#CE e RO
prop.test(x=2,n=318, conf.level = 0.95)#ES, MT, MS, PA
prop.test(x=1,n=318, conf.level = 0.95)#GO, PE, SE


#REGIAO DO BRASIL

regiao_brasil_residencia = fct_collapse(estado,
                                        `Centro-Oeste` =c('Distrito Federal  (DF)', 'Goiás (GO)', 'Mato Grosso (MT)', 'Mato Grosso do Sul (MS)'),
                                        `Nordeste` = c('Alagoas (AL)', 'Bahia (BA)', 'Ceará (CE)', 'Maranhão (MA)', 'Paraíba (PB)', 'Pernambuco (PE)', 'Piauí (PI)', 'Rio Grande do Norte (RN)', 'Sergipe (SE)'),
                                        `Norte`=c('Amapá (AP)','Amazonas (AM)','Pará (PA)','Rondônia (RO)','Tocantins (TO)','Acre (AC)', 'Roraima (RR)'),
                                        `Sudeste` = c('Espírito Santo (ES)', 'Minas Gerais (MG)', 'Rio de Janeiro (RJ)', 'São Paulo (SP)'),
                                        `Sul` = c('Paraná (PR)', 'Rio Grande do Sul (RS)', 'Santa Catarina (SC)'))

DT::datatable(tabyl(regiao_brasil_residencia)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#CURSO DE PT NA CARITAS
summary(curso_es)
curso=factor(curso_es, levels = c(1, 2), labels = c("sim", "não"))
table(curso)
prop.table(table(curso_es))*100

DT::datatable(tabyl(curso)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

29+12
#IC
prop.test(x=29,n=41, conf.level = 0.95)#Sim
prop.test(x=12,n=41, conf.level = 0.95)#Não


###BLOCO A##### 

#Cor ou raça
summary(a2_es)
#dados$a2_es=factor(dados$a2_es, levels = c(1:6),labels = c("Negro(a)","Afrodescendente","Moreno(a)","Branco(a)","indígena","outro(a)"))
#aqui eu criei no mesmo banco, sem fazer "por fora" - deve ter que salvar no final

a2=factor(a2_es, levels = c(1:6), labels = c("Negro(a)","Afrodescendente","Moreno(a)","Branco(a)","indígena","outro(a)"))
table(a2)
prop.table(table(a2))*100

DT::datatable(tabyl(a2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#vamos avaliar a normalidade? #desconsiderar
#hist(a2_es)
#shapiro.test(a2_es)
#O p-valor for < 0.05 indica que os dados não apresentam normalidade.amostras maiores que 30 tendem a normalidade, independente do formato dos dados. Ref https://medium.com/bio-data-blog/testes-de-normalidade-no-r-fccefe7ae0fd

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=8,n=318, conf.level = 0.95)#negro
prop.test(x=6,n=318, conf.level = 0.95)#Afrodescendente
prop.test(x=168,n=318, conf.level = 0.95)#Moreno
prop.test(x=110,n=318, conf.level = 0.95)#Branco
prop.test(x=5,n=318, conf.level = 0.95)#Indigena
prop.test(x=21,n=318, conf.level = 0.95)#NA


#Cor ou raça - outro
##View(a2_1_es)

#Atualmente você é? - Estado civil
summary(a3_es)
a3=factor(a3_es, levels = c(1:5), labels = c("Solteiro(a)","Casado(a)","Separado(a)","Viuvo(a)","Outro(a)")) 
table(a3) 
prop.table(table(a3))*100

DT::datatable(tabyl(a3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=122,n=318, conf.level = 0.95)#Solteiro
prop.test(x=165,n=318, conf.level = 0.95)#Casado
prop.test(x=25,n=318, conf.level = 0.95)#Separado
prop.test(x=3,n=318, conf.level = 0.95)#Viuvo
prop.test(x=3,n=318, conf.level = 0.95)#NA

# estado civil - outro
summary(a3_1_es)

##View(a3_1_es)
a3_1_es#

#ver como fazer nuvem de palavras?
#http://www.estatisticacomr.uff.br/?p=322


#Alem de você alguem reside na sua casa?
summary(a4_es)
a4=factor(a4_es, levels = c(1:2), labels = c("sim","não")) 
table(a4) 
prop.table(table(a4))*100   

DT::datatable(tabyl(a4)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=289,n=318, conf.level = 0.95)#Sim
prop.test(x=27,n=318, conf.level = 0.95)#Não
prop.test(x=2,n=318, conf.level = 0.95)#NA

#Quantas pessoas residem na sua casa? / número de pessoas no domicilio
summary(a4_1_es)
sd(a4_1_es, na.rm=TRUE)
a4_1=as.numeric(a4_1_es)
summary(a4_1)
#uma média de 3,5 pessoas com um máximo de 10 pessoas por residência.

#Parentesco -- 
#São 7 variaveis, Cônjuge/companheiro(a) ,Filho[s](as) /Enteado[s](as), Pai, Mãe, Outros parentes, Amigos, Outros que estão classificadas entre 0=não e 1=sim. Logo, cada uma é vista individualmente. Tudo foi conferido com auxilio dos filtros do excel para avaliar se está de acordo.

##Conjugue/Companheiro
summary(a4_2_es___1)
##View(a4_2_es___1)
a4.2.1=factor(a4_2_es___1, levels = c(1:0), labels = c("sim","não")) 
table(a4.2.1) 
prop.table(table(a4.2.1))*100

#frequencia total (incluindo quem marcou sim em outras variaveis adiante)
DT::datatable(tabyl(a4.2.1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=174,n=318, conf.level = 0.95)#Sim
prop.test(x=144,n=318, conf.level = 0.95)#Não


#Frequencia daqueles que apenas declararam sim a ##Conjugue/Companheiro. Nesse caso ele retorna como vetor logico, mas comparei no excel e é isso. 
a4.2.1_only = c(a4=="sim" & a4.2.1 == "sim" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")

#tabela
DT::datatable(tabyl(a4.2.1_only)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#quais?
which(a4=="sim" & a4.2.1 == "sim" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")#aqueles que apenas marcaram morar com o conjugue

#contando quantos
length(which(a4=="sim" & a4.2.1 == "sim" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")) 
  
  
##Filho(os)/enteados
summary(a4_2_es___2)
a4.2.2=factor(a4_2_es___2, levels = c(1:0), labels = c("sim","não"))
table(a4.2.2) 
prop.table(table(a4.2.2))*100

DT::datatable(tabyl(a4.2.2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=195,n=318, conf.level = 0.95)#Sim
prop.test(x=123,n=318, conf.level = 0.95)#Não

#Frequencia daqueles que apenas declararam sim a ##Filho(os)/enteados. Nesse caso ele retorna como vetor logico, mas comparei no excel e é isso. 
a4.2.2_only = c(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "sim" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")

#tabela
DT::datatable(tabyl(a4.2.2_only)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#quais?
which(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "sim" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")#aqueles que apenas marcaram morar com o filhos/enteados

#contando quantos
length(which(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "sim" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")) 

##Pai
summary(a4_2_es___3)
a4.2.3=factor(a4_2_es___3, levels = c(1:0), labels = c("sim","não"))
table(a4.2.3) 
prop.table(table(a4.2.3))*100

DT::datatable(tabyl(a4.2.3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=17,n=318, conf.level = 0.95)#Sim
prop.test(x=301,n=318, conf.level = 0.95)#Não

#Frequencia daqueles que apenas declararam sim a ##Pai. Nesse caso ele retorna como vetor logico, mas comparei no excel e é isso. 
a4.2.3_only = c(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "sim" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")

#tabela
DT::datatable(tabyl(a4.2.3_only)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#quais?
which(a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "sim" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")#aqueles que apenas marcaram morar com o pai

#contando quantos
length(which(a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "sim" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")) 


##Mãe
summary(a4_2_es___4)
a4.2.4=factor(a4_2_es___4, levels = c(1:0), labels = c("sim","não"))
table(a4.2.4)
prop.table(table(a4.2.4))*100

DT::datatable(tabyl(a4.2.4)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=37,n=318, conf.level = 0.95)#Sim
prop.test(x=281,n=318, conf.level = 0.95)#Não

#Frequencia daqueles que apenas declararam sim a ##Mãe. Nesse caso ele retorna como vetor logico, mas comparei no excel e é isso. 
a4.2.4_only = c(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "sim"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")

#tabela
DT::datatable(tabyl(a4.2.4_only)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#quais?
which(a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "sim"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")#aqueles que apenas marcaram morar com o conjugue

#contando quantos
length(which(a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "sim"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")) 

##Outros Parentes
summary(a4_2_es___5)
a4.2.5=factor(a4_2_es___5, levels = c(1:0), labels = c("sim","não"))
table(a4.2.5) 
prop.table(table(a4.2.5))*100

DT::datatable(tabyl(a4.2.5)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=87,n=318, conf.level = 0.95)#Sim
prop.test(x=231,n=318, conf.level = 0.95)#Não

#Frequencia daqueles que apenas declararam sim a ##outros parentes. Nesse caso ele retorna como vetor logico, mas comparei no excel e é isso. 
a4.2.5_only = c(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "sim"& a4.2.6 == "não"& a4.2.7== "não")

#tabela
DT::datatable(tabyl(a4.2.5_only)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#quais?
which(a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "sim"& a4.2.6 == "não"& a4.2.7== "não")#aqueles que apenas marcaram morar com outros parentes

#contando quantos
length(which(a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "sim"& a4.2.6 == "não"& a4.2.7== "não")) 

##Amigos
summary(a4_2_es___6)
a4.2.6=factor(a4_2_es___6, levels = c(1:0), labels = c("sim","não"))
table(a4.2.6) 
prop.table(table(a4.2.6))*100

DT::datatable(tabyl(a4.2.6)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=47,n=318, conf.level = 0.95)#Sim
prop.test(x=271,n=318, conf.level = 0.95)#Não

#Frequencia daqueles que apenas declararam sim a ##amigos parentes. Nesse caso ele retorna como vetor logico, mas comparei no excel e é isso. 
a4.2.6_only = c(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "sim"& a4.2.7== "não")

#tabela
DT::datatable(tabyl(a4.2.6_only)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#quais?
which(a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "sim"& a4.2.7== "não")#aqueles que apenas marcaram morar com amigos

#contando quantos
length(which(a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "sim"& a4.2.7== "não"))

##Outros 
#esses contam o banco inteiro e não aqueles que escolheram exclusivamente esses valores.
summary(a4_2_es___7)
which(a4_2_es___7==1)
which(a4_2_es___7==0)
length(which(a4_2_es___7==1))
length(which(a4_2_es___7==0))
#
a4.2.7=factor(a4_2_es___7, levels = c(1:0), labels = c("sim","não"))
table(a4.2.7) 
prop.table(table(a4.2.7))*100

DT::datatable(tabyl(a4.2.7)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=15,n=318, conf.level = 0.95)#Sim
prop.test(x=303,n=318, conf.level = 0.95)#Não
#Frequencia daqueles que apenas declararam sim a ##outros. Nesse caso ele retorna como vetor logico, mas comparei no excel e é isso. 
a4.2.7_only = c(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "sim")

#tabela
DT::datatable(tabyl(a4.2.7_only)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#quais?
which(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "sim")#aqueles que apenas marcaram morar com outros
      
#contando quantos
length(which(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "sim")) 


#Quantas não marcaram nenhuma?
#Frequencia daqueles que deixaram em branco. 
a4.2.8_only = c(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")

#tabela
DT::datatable(tabyl(a4.2.8_only)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#quais?
which(a4=="sim" & a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")#aqueles que falaram que vivem com outras pessoas em sua casa mas dnão marcaram nenhuma alternaiva relativa ao parentesco

#contando quantos
length(which(a4=="sim" &a4.2.1 == "não" & a4.2.2 == "não" & a4.2.3 == "não" & a4.2.4 == "não"& a4.2.5 == "não"& a4.2.6 == "não"& a4.2.7== "não")) 

#Se outro, qual? -- parentesco
summary(a4_2_a_es)
table(a4_2_a_es)
##View(a4_2_a_es)

##Apatrida
summary(a5_es)
a5=factor(a5_es, levels = c(1:2), labels = c("sim","não"), exclude="")
table(a5)
prop.table(table(a5))*100

DT::datatable(tabyl(a5)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=57,n=318, conf.level = 0.95)#Sim
prop.test(x=251,n=318, conf.level = 0.95)#Não
prop.test(x=10,n=318, conf.level = 0.95)#NA

#Religião
summary(a6_es)
a6=factor(a6_es, levels = c(1:8), labels = c("Catolica","Evangelica","Testemunhas de Jeova"," Cultos Afroamericanos","Adventista","Mormom","Não tenho religão","Outra"))#candomblé e umbanda não são as mesmas coisas, e não ha ateu (deveria?)
table(a6)
prop.table(table(a6))*100

DT::datatable(tabyl(a6)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#IC
prop.test(x=128,n=318, conf.level = 0.95)#Catolica
prop.test(x=91,n=318, conf.level = 0.95)#Evangelica
prop.test(x=11,n=318, conf.level = 0.95)#Testemunhas de Jeova
prop.test(x=2,n=318, conf.level = 0.95)#Cultos AfroAmericanos
prop.test(x=8,n=318, conf.level = 0.95)#Adventista
prop.test(x=19,n=318, conf.level = 0.95)#Mormon
prop.test(x=48,n=318, conf.level = 0.95)#Não tenho religião
prop.test(x=4,n=318, conf.level = 0.95)#Outra
prop.test(x=7,n=318, conf.level = 0.95)#NA

#Religiao - outro 
summary(a6_1_es)
class(a6_1_es)
#outras religiões relatadas são xamanismo, judaismo e espiritismo

#Filhos
summary(a7_es)
a7=factor(a7_es, levels = c(1:4), labels = c("0","1","2","3 ou mais"))
summary(a7)

DT::datatable(tabyl(a7)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=66,n=318, conf.level = 0.95)#0
prop.test(x=99,n=318, conf.level = 0.95)#1
prop.test(x=94,n=318, conf.level = 0.95)#2
prop.test(x=59,n=318, conf.level = 0.95)#NA

#escolaridade
summary(a8_es)
a8=factor(a8_es, levels = c(1:10), labels = c("Nunca frequentou a escola","Educação básica","Ensino fundamental incompleto","Ensino fundamental completo","Ensino médio incompleto","Ensino médio completo","Ensino superior incompleto","Ensino superior completo","Pós-graduação incompleta","Pós-graduação completa"))
summary(a8)
table(a8)
prop.table(table(a8))*100

datatable(tabyl(a8)%>%
            adorn_totals("row") %>%
            adorn_pct_formatting(digits = 2))



#IC
prop.test(x=5,n=318, conf.level = 0.95)# Nunca frequentou a escola - esducação básica. 
prop.test(x=7,n=318, conf.level = 0.95)#Fundamental incompleto
prop.test(x=3,n=318, conf.level = 0.95)#Fundamental completo
prop.test(x=21,n=318, conf.level = 0.95)# Médio incompleto 
prop.test(x=64,n=318, conf.level = 0.95)# Medio completo
prop.test(x=63,n=318, conf.level = 0.95)#Superior incompleto
prop.test(x=107,n=318, conf.level = 0.95)#Superior completo
prop.test(x=19,n=318, conf.level = 0.95)#Pós graduação incompleta
prop.test(x=22,n=318, conf.level = 0.95)#Pós graduação completa
prop.test(x=2,n=318, conf.level = 0.95)#NA


#library(REdaS)
#freqCI(a8, level = .95)
#library(dplyr)
#library(freqtables)
#freq_table(dados,a8_es, percent_ci = 0.95)

#Fala outro idioma além do espanhol?
summary(a9_es)
a9=factor(a9_es, levels = c(1:2), labels = c("sim","não"))
summary(a9)
table(a9)#ver como calcular a tabela com NA's pra ter no percetual total também
prop.table(table(a9))*100


DT::datatable(tabyl(a9)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=172,n=318, conf.level = 0.95)#Sim
prop.test(x=143,n=318, conf.level = 0.95)#Não
prop.test(x=3,n=318, conf.level = 0.95)#NA

#idioma- outros
summary(a9_1_es)#ver nuvem de palavras

#Trabalho na ven
summary(a10_es)
a10=factor(a10_es, levels = c(1:4), labels = c("não", "Sim, em tempo parcial (até 20 horas semanais)","Sim, em tempo integral (mais de 30 horas semanais","Sim, mas se trata de trabalho eventual"))
summary(a10)
table(a10)#ver como calcular a tabela com NA's pra ter no percetual total também
prop.table(table(a10))*100

DT::datatable(tabyl(a10)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=144,n=318, conf.level = 0.95)#Não
prop.test(x=29,n=318, conf.level = 0.95)#Até 20hrs
prop.test(x=92,n=318, conf.level = 0.95)#+ de 30hrs
prop.test(x=34,n=318, conf.level = 0.95)#Eventual
prop.test(x=19,n=318, conf.level = 0.95)#NA


#dicotomizando em sim e não
#install.packages("tidyverse")
library(tidyverse)
library(forcats)

a10_dicot = fct_collapse(a10,`Sim` = c('Sim, em tempo parcial (até 20 horas semanais)','Sim, em tempo integral (mais de 30 horas semanais','Sim, mas se trata de trabalho eventual'),
                         `Não` = c('não'))

DT::datatable(tabyl(a10_dicot)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#qual trab? >> na venezuela
summary(a10_1_es)
table((a10_1_es))#parece que table resolve o problema com as variaveis que são character
##View(a10_1_es)

#trab no brasil
summary(a11_es)
a11=factor(a11_es, levels = c(1:4), labels = c("não", "Sim, em tempo parcial (até 20 horas semanais)","Sim, em tempo integral (mais de 30 horas semanais","Sim, mas se trata de trabalho eventual"))
summary(a11)
table(a11)#ver como calcular a tabela com NA's pra ter no percetual total também
prop.table(table(a11))*100

DT::datatable(tabyl(a11)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=168,n=318, conf.level = 0.95)#Não
prop.test(x=19,n=318, conf.level = 0.95)#Até 20hrs
prop.test(x=59,n=318, conf.level = 0.95)#+ de 30hrs
prop.test(x=55,n=318, conf.level = 0.95)#Eventual
prop.test(x=17,n=318, conf.level = 0.95)#NA

a11_dicot = fct_collapse(a11,`Sim` = c('Sim, em tempo parcial (até 20 horas semanais)','Sim, em tempo integral (mais de 30 horas semanais','Sim, mas se trata de trabalho eventual'),
                         `Não` = c('não'))

DT::datatable(tabyl(a11_dicot)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#qual trab >> no brasil
table(a11_1_es)#conferir o que foi respondido, teve gente que só preencheu por preencher...

#rendimentos venezuela (sera que não é melhor a A12 fica prox a A10?)
summary(a12_es)
a12=factor(a12_es,levels=c(1:10),labels=c("Nenhuma renda",
                                         "Até 5 dólares",
                                         "Entre 6 e 10 dólares",
                                         "Entre 11 e 25 dólares",
                                         "Entre 26 e 40 dólares",
                                         "Entre 41 e 55 dólares",
                                         "Entre 56 e 100 dólares",
                                         "Entre 101 e 150 dólares",
                                         "Entre 151 e 200 dólares",
                                         "Mais de 200 dólares"))
summary(a12)
table(a12)
prop.table(table(a12))*100

DT::datatable(tabyl(a12)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=77,n=318, conf.level = 0.95)#Nenhuma renda
prop.test(x=98,n=318, conf.level = 0.95)#
prop.test(x=46,n=318, conf.level = 0.95)# 
prop.test(x=17,n=318, conf.level = 0.95)#
prop.test(x=9,n=318, conf.level = 0.95)# 
prop.test(x=11,n=318, conf.level = 0.95)#
prop.test(x=6,n=318, conf.level = 0.95)#
prop.test(x=4,n=318, conf.level = 0.95)#
prop.test(x=8,n=318, conf.level = 0.95)#
prop.test(x=38,n=318, conf.level = 0.95)#NA

#quantas pessoas dependiam desses rendimentos
a13=as.numeric(a13_es)
summary(a13)
summary(a13)
sd(a13, na.rm=TRUE)


#rendimentos brasil #será que é melhor perguntar por faixa salarial ou nesse modelo do IBGE?
summary(a14_es)
a14=factor(a14_es,levels=c(1:9),labels=c("Até 500 reais",
                                         "Entre 501 e 1.000 reais",
                                         "Entre 1.001 e 1.500 reais",
                                         "Entre 1.501 e 2.000 reais",
                                         "Entre 2.001 e 2.500 reais",
                                         "Entre 2.501 e 3.000 reais",
                                         "Entre 3.001 e 4.000 reais",
                                         "Entre 4.001 e 5.000 reais",
                                         "Mais de 5.000 reais"))
summary(a14)
prop.table(table(a14))*100


DT::datatable(tabyl(a14)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=45,n=318, conf.level = 0.95)#Ate 500 reais
prop.test(x=80,n=318, conf.level = 0.95)#501 a 1000
prop.test(x=71,n=318, conf.level = 0.95)# 1001 a 1500
prop.test(x=46,n=318, conf.level = 0.95)#1501 a 2000
prop.test(x=19,n=318, conf.level = 0.95)# 2001 a 2500
prop.test(x=13,n=318, conf.level = 0.95)#Entre 2,501 e 3,000 reais
prop.test(x=7,n=318, conf.level = 0.95)#Entre 3,001 e 4,000 reais
prop.test(x=5,n=318, conf.level = 0.95)#Entre 4,001 e 5,000 reais
prop.test(x=2,n=318, conf.level = 0.95)#Mais de 5,000 reais
prop.test(x=30,n=318, conf.level = 0.95)#NA

#dicotomizando renda e faixas maiores
a14_dicot = fct_collapse(a14,`Até 500 reais` = c('Até 500 reais'),
                         `501 a 1.500 reais` = c('Entre 501 e 1.000 reais','Entre 1.001 e 1.500 reais'),
                         `1.501 a 3.000 reais` = c('Entre 1.501 e 2.000 reais','Entre 2.001 e 2.500 reais','Entre 2.501 e 3.000 reais'),
                         `3.000 a 5000 reais` = c('Entre 3.001 e 4.000 reais','Entre 4.001 e 5.000 reais'),
                         `+ de 5.000 reais` = c('Mais de 5.000 reais'))

DT::datatable(tabyl(a14_dicot)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))                              
#pensar na quantidade de horas trabalhada com renda. Como avaliar isso?


#quantas pessoas dependem dessa renda
summary(a15_es)
a15=as.numeric(a15_es)
summary(a15)


#Como vocÊ classifica seu domínio de português? - sera que não é melhor ficar perto da pergunta de dialeto?
summary(a16_es)
a16=factor(a16_es,levels=c(1:4),labels=c("nenhum",
                                         "basico",
                                         "intermediario",
                                         "avançado"))
summary(a16)
prop.table(table(a16))*100


DT::datatable(tabyl(a16)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=15,n=318, conf.level = 0.95)#Nenhum
prop.test(x=123,n=318, conf.level = 0.95)#Basico
prop.test(x=116,n=318, conf.level = 0.95)#Intermediario
prop.test(x=53,n=318, conf.level = 0.95)#Avançado
prop.test(x=11,n=318, conf.level = 0.95)#NA


#Discriminação
summary(a17_es)
a17=factor(a17_es,levels=c(1:2),labels=c("sim",
                                         "não"))
summary(a17)
prop.table(table(a17))*100

DT::datatable(tabyl(a17)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=106,n=318, conf.level = 0.95)#Sim
prop.test(x=210,n=318, conf.level = 0.95)#Não
prop.test(x=2,n=318, conf.level = 0.95)#NA


###BLOCO B##### 

#Veio da Venezuela ou de outro país?

summary(b1_es)
b1=factor(b1_es,levels=c(1:2),labels=c("Venezuela", "Outro"))

DT::datatable(tabyl(b1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#De outro país
summary(b1_1_es)
b1.1=as.factor(b1_1_es)
summary(b1.1)

DT::datatable(tabyl(b1.1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=275,n=318, conf.level = 0.95)#Venezuela
prop.test(x=1,n=318, conf.level = 0.95)#Argetina, Equador e Serra Leoa
prop.test(x=4,n=318, conf.level = 0.95)#Chile
prop.test(x=6,n=318, conf.level = 0.95)#Colombia, NA
prop.test(x=24,n=318, conf.level = 0.95)#Peru


#Da Venezuela
#Estado de origem B1.3!!!!

summary(b1_3_es)
class(b1_3_es)
##View(b1_3_es)
b1.3=factor(b1_3_es,levels=c(1:24),labels=c("Amazonas","Anzoátegui","Apure","Aragua","Barinas", "Bolívar","Carabobo","Cojedes","Delta Amacuro","Distrito Federal","Falcón","Guárico","Lara","Mérida","Miranda","Monagas","Nueva Esparta","Portuguesa","Sucre","Táchira","Trujillo","Vargas","Yaracuy","Zulia"))

DT::datatable(tabyl(b1.3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

b1.3.dec= tab1(b1.3, decimal = 1, sort.group = "decreasing", 
               cum.percent = !any(is.na(situmigra_es)), graph = F, 
               missing = TRUE, bar.values = "frequency", 
               horiz = F, cex = 1, cex.names = 1, main = "Venezuelanos por UF do Brasil",  xlab = "Frequência por UF", 
               ylab = "UF", col = "auto", cex.main=1, cex.name=1, cex.axis=1, cex.lab=1, gen.ind.vars = F)

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=82,n=318, conf.level = 0.95)#Bolivar
prop.test(x=65,n=318, conf.level = 0.95)#Anzoategui
prop.test(x=43,n=318, conf.level = 0.95)#NA
prop.test(x=33,n=318, conf.level = 0.95)#Monaguas
prop.test(x=22,n=318, conf.level = 0.95)#Distrito Federal
prop.test(x=15,n=318, conf.level = 0.95)#Carabobo
prop.test(x=7,n=318, conf.level = 0.95)#Aragua, Nueva Esparta, Zulia
prop.test(x=6,n=318, conf.level = 0.95)#Lara
prop.test(x=5,n=318, conf.level = 0.95)#Sucre
prop.test(x=4,n=318, conf.level = 0.95)#Miranda, Táchira
prop.test(x=3,n=318, conf.level = 0.95)#Falcon
prop.test(x=2,n=318, conf.level = 0.95)#"Apure, Barinas, Delta Amacuro,Guárico, Mérida, Yaracuy
prop.test(x=1,n=318, conf.level = 0.95)#Amazonas, Portuguesa
prop.test(x=0,n=318, conf.level = 0.95)#Cojedes,Trujillo


#b1.4 são as cidades dos estados contidos na b1.3


#data de saída da cidade de origem
#install.packages("ludibriate")
library(lubridate)
summary(b2_es)
class(b2_es)
##View(b2_es)
b2=as.Date(b2_es)
class(b2)
summary(b2)
b2.1=year(b2)


DT::datatable(tabyl(b2.1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=1,n=318, conf.level = 0.95)#2010,2013,2014
prop.test(x=8,n=318, conf.level = 0.95)#2015,2016, NA
prop.test(x=20,n=318, conf.level = 0.95)#2017
prop.test(x=86,n=318, conf.level = 0.95)#2018
prop.test(x=99,n=318, conf.level = 0.95)#2019
prop.test(x=67,n=318, conf.level = 0.95)#2020
prop.test(x=19,n=318, conf.level = 0.95)#2021


#Tempo de saída calculado em dias e meses

#Tempo de saida em dias
tempo_saída_dias=difftime(as.Date("2021-05-10"),b2, units = "days")
summary(tempo_saída_dias)
class(tempo_saída_dias)
tempo_saída_d=as.numeric(tempo_saída_dias)
class(tempo_saída_d)
summary(tempo_saída_d)

#tempo de saída em meses
tempo_saída_meses= interval(b2,as.Date("2021-05-10")) %/% months(1)    # Apply interval & months
summary(tempo_saída_meses)
class(tempo_saída_meses)
tempo_saída_m=as.numeric(tempo_saída_meses)
class(tempo_saída_m)
summary(tempo_saída_m)
plot(tempo_saída_m)
hist(tempo_saída_m)

DT::datatable(tabyl(tempo_saída_m)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#b3== #Você teve cia - até chegada ao Brail #qual a pertinencia da pergunta pra pesquisa?
summary(b3_es)
b3=factor(b3_es,levels=c(1:2),labels=c("sim","não"))
table(b3)
prop.table(table(b3))*100

DT::datatable(tabyl(b3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=219,n=318, conf.level = 0.95)#SIM
prop.test(x=93,n=318, conf.level = 0.95)#NÃO
prop.test(x=6,n=318, conf.level = 0.95)#NA

##B3.1!!! qual cia?
summary(b3_1_es___1) #familiar
b3.1.1=factor(b3_1_es___1,levels=c(1,0),labels=c("sim","não"))

DT::datatable(tabyl(b3.1.1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=166,n=318, conf.level = 0.95)#SIM
prop.test(x=152,n=318, conf.level = 0.95)#NÃO

summary(b3_1_es___2) #amigo
b3.1.2=factor(b3_1_es___2,levels=c(1,0),labels=c("sim","não"))

DT::datatable(tabyl(b3.1.2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=59,n=318, conf.level = 0.95)#SIM
prop.test(x=259,n=318, conf.level = 0.95)#NÃO


summary(b3_1_es___3) #desconhecido
b3.1.3=factor(b3_1_es___3,levels=c(1,0),labels=c("sim","não"))

DT::datatable(tabyl(b3.1.3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=24,n=318, conf.level = 0.95)#SIM
prop.test(x=294,n=318, conf.level = 0.95)#NÃO

#Quais transportes você utilizou ate o Bra
summary(b4_es___1)#aviao
b4.1=factor(b4_es___1,levels=c(1:0),labels=c("sim","não"))
table(b4.1)
prop.table(table(b4.1))*100

DT::datatable(tabyl(b4.1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=69,n=318, conf.level = 0.95)#SIM
prop.test(x=249,n=318, conf.level = 0.95)#NÃO

summary(b4_es___2)#maritimo
b4.2=factor(b4_es___2,levels=c(1:0),labels=c("sim","não"))
table(b4.2)
prop.table(table(b4.2))*100

DT::datatable(tabyl(b4.2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=8,n=318, conf.level = 0.95)#SIM
prop.test(x=310,n=318, conf.level = 0.95)#NÃO


summary(b4_es___3)#a pé
b4.3=factor(b4_es___3,levels=c(1:0),labels=c("sim","não"))
table(b4.3)
prop.table(table(b4.3))*100

DT::datatable(tabyl(b4.3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=54,n=318, conf.level = 0.95)#SIM
prop.test(x=264,n=318, conf.level = 0.95)#NÃO

summary(b4_es___4)#onibus
b4.4=factor(b4_es___4,levels=c(1:0),labels=c("sim","não"))
table(b4.4)
prop.table(table(b4.4))*100

DT::datatable(tabyl(b4.4)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=218,n=318, conf.level = 0.95)#SIM
prop.test(x=100,n=318, conf.level = 0.95)#NÃO

summary(b4_es___5)#carro ou caminhão - carona?
b4.5=factor(b4_es___5,levels=c(1:0),labels=c("sim","não"))
table(b4.5)
prop.table(table(b4.5))*100

DT::datatable(tabyl(b4.5)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=62,n=318, conf.level = 0.95)#SIM
prop.test(x=256,n=318, conf.level = 0.95)#NÃO

#pernoite até o Bra 
summary(b5_es)
b5=factor(b5_es,levels=c(1:2),labels=c("sim","não"))
table(b5)
prop.table(table(b5))*100

DT::datatable(tabyl(b5)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=127,n=318, conf.level = 0.95)#Sim
prop.test(x=186,n=318, conf.level = 0.95)#Não
prop.test(x=5,n=318, conf.level = 0.95)#NA


#quais cidades?
summary(b5_1_es)
table(b5_1_es)

#B6 Estado de entrada 
summary(b6_es)
b6=factor(b6_es,levels=c(1:27),labels=c("Acre (AC)",
                                        "Alagoas (AL)",
                                        "Amapá (AP)",
                                        "Amazonas (AM)",
                                        "Bahia (BA)",
                                        "Ceará (CE)",
                                        "Distrito Federal  (DF)",
                                        "Espírito Santo (ES)",
                                        "Goiás (GO)",
                                        "Maranhão (MA)",
                                        "Mato Grosso (MT)",
                                        "Mato Grosso do Sul (MS)",
                                        "Minas Gerais (MG)",
                                        "Pará (PA)",
                                        "Paraíba (PB)",
                                        "Paraná (PR)",
                                        "Pernambuco (PE)",
                                        "Piauí (PI)",
                                        "Rio de Janeiro (RJ)",
                                        "Rio Grande do Norte (RN)",
                                        "Rio Grande do Sul (RS)",
                                        "Rondônia (RO)",
                                        "Roraima (RR)",
                                        "Santa Catarina (SC)",
                                        "São Paulo (SP)",
                                        "Sergipe (SE)",
                                        "Tocantins (TO)"))


DT::datatable(tabyl(b6)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=231,n=318, conf.level = 0.95)#RR
prop.test(x=33,n=318, conf.level = 0.95)#AM
prop.test(x=17,n=318, conf.level = 0.95)#AC
prop.test(x=12,n=318, conf.level = 0.95)#SP
prop.test(x=9,n=318, conf.level = 0.95)#RJ
prop.test(x=6,n=318, conf.level = 0.95)#NA
prop.test(x=3,n=318, conf.level = 0.95)#MS, RS
prop.test(x=2,n=318, conf.level = 0.95)#SC
prop.test(x=1,n=318, conf.level = 0.95)#BA, PR 
prop.test(x=0,n=318, conf.level = 0.95)#Alagoas (AL), Amapá (AP), Ceará (CE), Distrito Federal (DF), Espírito Santo (ES), Goiás (GO), Maranhão (MA), Mato Grosso (MT), Minas Gerais (MG), Pará (PA), Paraíba (PB), Pernambuco (PE), Piauí (PI), Rio Grande do Norte (RN), Rondônia (RO), Sergipe (SE),   Tocantins (TO)


#Qual data chegou ao país?
summary(b7_es)
class(b7_es)
b7=as.Date(b7_es)
class(b7)
summary(b7)
anob7=year(b7)
summary(anob7)
#View(dados)

DT::datatable(tabyl(year(b7))%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#tabelas
anob7.dec= tab1(anob7, decimal = 1, sort.group = "decreasing", 
                cum.percent = T, graph = F, 
                missing = TRUE, bar.values = "frequency", 
                horiz = F, cex = 1, cex.names = 1, main = "Ano de Chegada ao Brasil",  xlab = "Frequência por Ano", 
                ylab = "Ano", col = "auto", cex.main=1, cex.name=1, cex.axis=1, cex.lab=1, gen.ind.vars = F)

anob7.dec= tab1(anob7, decimal = 1, sort.group = F, 
                cum.percent = T, graph = F, 
                missing = TRUE, bar.values = "frequency", 
                horiz = F, cex = 1, cex.names = 1, main = "Ano de Chegada ao Brasil",  xlab = "Frequência por Ano", 
                ylab = "Ano", col = "auto", cex.main=1, cex.name=1, cex.axis=1, cex.lab=1, gen.ind.vars = F)

DT::datatable(anob7.dec)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 2)

#tempo no Brasil
#como calcular?
#b7=as.Date(b7_es) - b7 ja está como data

tempo_brasil_dias=difftime(as.Date("2021-05-10"),b7, units = "days")
summary(tempo_brasil_dias)
class(tempo_brasil_dias)
tempo_brasil_d=as.numeric(tempo_brasil_dias)
class(tempo_brasil_d)
summary(tempo_brasil_d)

tempo_brasil_meses= interval(b7,as.Date("2021-05-10")) %/% months(1)    # Apply interval & months
summary(tempo_brasil_meses)
class(tempo_brasil_meses)
tempo_brasil_m=as.numeric(tempo_brasil_meses)
class(tempo_brasil_m)
summary(tempo_brasil_m)


#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=1,n=318, conf.level = 0.95)#2010,2013,2014
prop.test(x=8,n=318, conf.level = 0.95)#2015
prop.test(x=7,n=318, conf.level = 0.95)#2016
prop.test(x=19,n=318, conf.level = 0.95)#2017
prop.test(x=85,n=318, conf.level = 0.95)#2018
prop.test(x=97,n=318, conf.level = 0.95)#2019
prop.test(x=66,n=318, conf.level = 0.95)#2020
prop.test(x=21,n=318, conf.level = 0.95)#2021
prop.test(x=12,n=318, conf.level = 0.95)#NA


#Você portava algum documento?
summary(b8_es)
b8=factor(b8_es,levels=c(1:2),labels=c("sim","não"))
table(b8)

DT::datatable(tabyl(b8)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=291,n=318, conf.level = 0.95)#SIM
prop.test(x=17,n=318, conf.level = 0.95)#NÃO
prop.test(x=12,n=318, conf.level = 0.95)#NA

#Identidade
summary(b8_1_es___1)
b8.1=factor(b8_1_es___1,levels=c(1:0),labels=c("sim","não"))
table(b8.1)
prop.table(table(b8.1))*100

DT::datatable(tabyl(b8.1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=122,n=318, conf.level = 0.95)#SIM

#Certidão de Nascimento
summary(b8_1_es___2)
b8.2=factor(b8_1_es___2,levels=c(1:0),labels=c("sim","não"))
table(b8.2)
prop.table(table(b8.2))*100

DT::datatable(tabyl(b8.2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=18,n=318, conf.level = 0.95)#SIM


#Passaporte Nacional
summary(b8_1_es___3)
b8.3=factor(b8_1_es___3,levels=c(1:0),labels=c("sim","não"))
table(b4.3)
prop.table(table(b8.3))*100

DT::datatable(tabyl(b8.3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=257,n=318, conf.level = 0.95)#SIM

#Carteira de habilitação
summary(b8_1_es___4)
b8.4=factor(b8_1_es___4,levels=c(1:0),labels=c("sim","não"))
table(b8.4)
prop.table(table(b8.4))*100

DT::datatable(tabyl(b8.4)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=41,n=318, conf.level = 0.95)#SIM

#Autorização de viagem
summary(b8_1_es___5)
b8.5=factor(b8_1_es___5,levels=c(1:0),labels=c("sim","não"))
table(b8.5)
prop.table(table(b8.5))*100

DT::datatable(tabyl(b8.5)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=131,n=318, conf.level = 0.95)#SIM

#Outros...
summary(b8_1_es___6)
b8.6=factor(b8_1_es___6,levels=c(1:0),labels=c("sim","não"))
table(b8.6)
prop.table(table(b8.6))*100

DT::datatable(tabyl(b8.6)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=19,n=318, conf.level = 0.95)#SIM

#Se outro documento, qual?
summary(b8_1_a_es)
table(b8_1_a_es)

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=291,n=318, conf.level = 0.95)#Sim
prop.test(x=17,n=318, conf.level = 0.95)#Não
prop.test(x=10,n=318, conf.level = 0.95)#Na
prop.test(x=122,n=318, conf.level = 0.95)#Identidade
prop.test(x=18,n=318, conf.level = 0.95)#Certidão de Nascimento
prop.test(x=257,n=318, conf.level = 0.95)#Passporte
prop.test(x=41,n=318, conf.level = 0.95)#Habilitação
prop.test(x=131,n=318, conf.level = 0.95)#Autorização de Viagem
prop.test(x=19,n=318, conf.level = 0.95)#Outros


#ATUALMENTE você VIVE NO MESMO ESTADO em que deu entrada no Brasil?
summary(b9_es)
b9=factor(b9_es,levels=c(1:2),labels=c("Sim","Não"))
table(b9)
prop.table(table(b9))*100

DT::datatable(tabyl(b9)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=99,n=318, conf.level = 0.95)#Sim
prop.test(x=214,n=318, conf.level = 0.95)#Não
prop.test(x=5,n=318, conf.level = 0.95)#NA

#Quais tranportes utilizou pra chegar no estado de residencia atual?

summary(b9_1_es___1)#aviao
b9.1.1=factor(b9_1_es___1,levels=c(1,0),labels=c("sim","não"))
table(b9.1)
prop.table(table(b9.1))*100

DT::datatable(tabyl(b9.1.1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=148,n=318, conf.level = 0.95)#Sim

summary(b9_1_es___2)#maritmo
b9.1.2=factor(b9_1_es___2,levels=c(1,0),labels=c("sim","não"))
table(b9.1.2)
prop.table(table(b9.2))*100

DT::datatable(tabyl(b9.1.2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=8,n=318, conf.level = 0.95)#Sim

summary(b9_1_es___3)#terrestre 1 - a pé
b9.1.3=factor(b9_1_es___3,levels=c(1,0),labels=c("sim","não"))

DT::datatable(tabyl(b9.1.3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=13,n=318, conf.level = 0.95)#Sim

summary(b9_1_es___4)#Terrestre 2 - Onibus
b9.1.4=factor(b9_1_es___4,levels=c(1,0),labels=c("sim","não"))
DT::datatable(tabyl(b9.1.4)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=77,n=318, conf.level = 0.95)#Sim


summary(b9_1_es___5)# Terrestre 3 - Carro ou Caminhão
b9.1.5=factor(b9_1_es___5,levels=c(1,0),labels=c("sim","não"))
DT::datatable(tabyl(b9.1.5)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=14,n=318, conf.level = 0.95)#Sim


#Pernoite até o estado de residencia atual?
summary(b9_2_es)
b9.2=factor(b9_2_es,levels=c(1,2),labels=c("sim","não"))
table(b9.2)
prop.table(table(b9.2))*100

DT::datatable(tabyl(b9.2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=129,n=318, conf.level = 0.95)#Sim
prop.test(x=84,n=318, conf.level = 0.95)#Não
prop.test(x=105,n=318, conf.level = 0.95)#NA

#quais foram as cidades?
summary(b9_2_a_es)

#você teve cia até o local de residencia atual?
summary(b9_3_es)
b9.3=factor(b9_3_es,levels=c(1,2),labels=c("sim","não"))
DT::datatable(tabyl(b9.3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC - aqui ja vem incluso o shapiro wilk teste = pvalor
prop.test(x=148,n=318, conf.level = 0.95)#Sim
prop.test(x=50,n=318, conf.level = 0.95)#Não
prop.test(x=120,n=318, conf.level = 0.95)#NA

#você realizou a trajetória com quem?
summary(b9_3_a_es___1)#Familiar
b9.3.a.1=factor(b9_3_a_es___1,levels=c(1,0),labels=c("sim","não"))
table(b9.3.a.1)
prop.table(table(b9.3.a.1))*100

DT::datatable(tabyl(b9.3.a.1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=123,n=318, conf.level = 0.95)#Sim

#
summary(b9_3_a_es___2)#amigo
b9.3.a.2=factor(b9_3_a_es___2,levels=c(1,0),labels=c("sim","não"))
table(b9.3.a.2)
prop.table(table(b9.3.a.2))*100

DT::datatable(tabyl(b9.3.a.2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=32,n=318, conf.level = 0.95)#Sim

#
summary(b9_3_a_es___3)#desconhecido
b9.3.a.3=factor(b9_3_a_es___3,levels=c(1,0),labels=c("sim","não"))
table(b9.3.a.35)
prop.table(table(b9.3.a.3))*100

DT::datatable(tabyl(b9.3.a.3)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

prop.test(x=15,n=318, conf.level = 0.95)#Sim

#data de chegada ao local de residencia atual
summary(b9_4_es)
class(b9_4_es)
b9.4=as.Date(b9_4_es)
class(b9.4)
summary(b9.4)
tabyl(year(b9.4))
anob9.4=year(b9.4)
summary(anob9.4)

DT::datatable(tabyl(year(b9.4))%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=1,n=318, conf.level = 0.95)#2010
prop.test(x=2,n=318, conf.level = 0.95)#2015,2016
prop.test(x=5,n=318, conf.level = 0.95)#2017
prop.test(x=33,n=318, conf.level = 0.95)#2018
prop.test(x=67,n=318, conf.level = 0.95)#2019
prop.test(x=56,n=318, conf.level = 0.95)#2020
prop.test(x=21,n=318, conf.level = 0.95)#2021
prop.test(x=131,n=318, conf.level = 0.95)#NA



#Em que cidade reside?
summary(b10_es)
tabyl(b10_es)

#bairro
table(b11_es)
tabyl(b11_es)
#ver nuvem de palavras


#Menores de 18 anos?
summary(b12_es)
b12=factor(b12_es,levels=c(1:2),labels=c("Sim","Não"))

DT::datatable(tabyl(b12)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=203,n=318, conf.level = 0.95)#Sim
prop.test(x=107,n=318, conf.level = 0.95)#Não
prop.test(x=8,n=318, conf.level = 0.95)#NA

#data de solicitação de refugio
summary(b13_es)
class(b13_es)
b13=as.Date(b13_es)
class(b13)
summary(b13)
tabyl(year(b13))
anob13=year(b13)
summary(anob13)

DT::datatable(tabyl(year(b13))%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=1,n=318, conf.level = 0.95)#2015
prop.test(x=18,n=318, conf.level = 0.95)#2018
prop.test(x=21,n=318, conf.level = 0.95)#2019,2020
prop.test(x=3,n=318, conf.level = 0.95)#2021
prop.test(x=254,n=318, conf.level = 0.95)#NA


###BLOCO C##### 
####WHOQOL EXTRA DOMINIO##### 

#PERGUNTAS EXTRA DOMINIO

#C1
summary(c1_es)
c1=factor(c1_es,levels=c(1:5),labels=c("Muy mal",
                                       "Poco",
                                       "Lo normal",
                                       "Bastante bien",
                                       "Muy bien"))

#c1.c=DT::datatable(tabyl(c1)%>%
#adorn_totals("row") %>%
#adorn_pct_formatting(digits = 2))
#DT::datatable(c1.c)

#C2 - satisfação com a saúde.

summary(c2_es)
sd(c2_es, na.rm=TRUE)
plot(c2_es)
boxplot(c2_es)
str(c2_es)

c2=factor(c2_es,levels=c(1:5),labels=c("Muito insatisfeito",
                                       "Insatisfeito",
                                       "Nem satisfeito, nem insatisfeito",
                                       "Satisfeito",
                                       "Muito satisfeito"), ordered=T) 

class(c2)

DT::datatable(tabyl(c2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=14,n=318, conf.level = 0.95)#Muito insatisfeito
prop.test(x=65,n=318, conf.level = 0.95)#Insatisfeito
prop.test(x=120,n=318, conf.level = 0.95)#Nem Satisfeito, Nem Insatisfeito
prop.test(x=73,n=318, conf.level = 0.95)#Satisfeito
prop.test(x=43,n=318, conf.level = 0.95)#Muito Satisfeito
prop.test(x=3,n=318, conf.level = 0.95)#NA


#Calculando o score de autopercepção da saúde 
#São duas transformações

# Primeira transformação 
summary( c2_es)
health_satis_4a20 = c2_es*4 
summary( health_satis_4a20)
round(summary( health_satis_4a20, na.rm=TRUE), 1) #arredondando para 1 casa decimal
sd( health_satis_4a20, na.rm=TRUE) 
round(sd( health_satis_4a20, na.rm=TRUE), 1) #arredondando para 1 casa decimal

#Agora na escala até 100

health_satis_0a100=( health_satis_4a20 - 4)*(100/16) #extradomínio saude autopercebida
summary( health_satis_0a100)
round(summary( health_satis_0a100, na.rm=TRUE), 1) #arredondando para 1 casa decimal
sd( health_satis_0a100, na.rm=TRUE) 
round(sd( health_satis_0a100, na.rm=TRUE), 1) #arredondando para 1 casa decimal

####WHOQOL IARALYZ##### 
library(devtools)
devtools::install_github("lebebr01/SPSStoR")
library(SPSStoR)

#Passo 1 WHoqol = 
library(dplyr)
library(tidyverse)

#o primeiro passo eu não preciso fazer... preciso?

#segundo passo
c3=c3_es
c3_rec=recode(c3, "1" = "5", "2"="4","3"="3","4"="2","5"="1")
c3_rec_num=as.numeric(c3_rec)


c4=c4_es
c4_rec=recode(c4, "1" = "5", "2"="4","3"="3","4"="2","5"="1")
c4_rec_num=as.numeric(c4_rec)

c26=c26_es
c26_rec=recode(c26, "1" = "5", "2"="4","3"="3","4"="2","5"="1")
c26_rec_num=as.numeric(c26_rec)



#C3
summary(c3_es)
c3=factor(c3_es,levels=c(1:5),labels=c("Nada ",
                                       "Un poco ",
                                       "Lo normal ",
                                       "Bastante",
                                       "Extremadamente"))

c3.c=DT::datatable(tabyl(c3)%>%
                     adorn_totals("row") %>%
                     adorn_pct_formatting(digits = 2))
DT::datatable(c3.c)

#C4
summary(c4_es)
c4=factor(c4_es,levels=c(1:5),labels=c("Nada ",
                                       "Un poco ",
                                       "Lo normal ",
                                       "Bastante",
                                       "Extremadamente"))

c4.c=DT::datatable(tabyl(c4)%>%
                     adorn_totals("row") %>%
                     adorn_pct_formatting(digits = 2))
DT::datatable(c4.c)
#C5
summary(c5_es)
c5=factor(c5_es,levels=c(1:5),labels=c("Nada ",
                                       "Un poco ",
                                       "Lo normal ",
                                       "Bastante",
                                       "Extremadamente"))

c5.c=DT::datatable(tabyl(c5)%>%
                     adorn_totals("row") %>%
                     adorn_pct_formatting(digits = 2))
DT::datatable(c5.c)
#C6
summary(c6_es)
c6=factor(c6_es,levels=c(1:5),labels=c("Nada ",
                                       "Un poco ",
                                       "Lo normal ",
                                       "Bastante",
                                       "Extremadamente"))

c6.c=DT::datatable(tabyl(c6)%>%
                     adorn_totals("row") %>%
                     adorn_pct_formatting(digits = 2))
DT::datatable(c6.c)
#C7Lida mes
summary(c7_es)
c7=factor(c7_es,levels=c(1:5),labels=c("Nada ",
                                       "Un poco ",
                                       "Lo normal ",
                                       "Bastante",
                                       "Extremadamente"))

c7.c=DT::datatable(tabyl(c7)%>%
                     adorn_totals("row") %>%
                     adorn_pct_formatting(digits = 2))
DT::datatable(c7.c)
#C8
summary(c8_es)
c8=factor(c8_es,levels=c(1:5),labels=c("Nada ",
                                       "Un poco ",
                                       "Lo normal ",
                                       "Bastante",
                                       "Extremadamente"))

c8.c=DT::datatable(tabyl(c8)%>%
                     adorn_totals("row") %>%
                     adorn_pct_formatting(digits = 2))
DT::datatable(c8.c)
#C9
summary(c9_es)
c9=factor(c9_es,levels=c(1:5),labels=c("Nada ",
                                       "Un poco ",
                                       "Lo normal ",
                                       "Bastante",
                                       "Extremadamente"))

c9.c=DT::datatable(tabyl(c9)%>%
                     adorn_totals("row") %>%
                     adorn_pct_formatting(digits = 2))
DT::datatable(c9.c)

####

#C10
summary(c10_es)
c10=factor(c10_es,levels=c(1:5),labels=c("Nada ",
                                         "Un poco ",
                                         "Lo normal ",
                                         "Bastante",
                                         "Extremadamente"))

c10.c=DT::datatable(tabyl(c10)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c10.c)
#C11
summary(c11_es)
c11=factor(c11_es,levels=c(1:5),labels=c("Nada ",
                                         "Un poco ",
                                         "Lo normal ",
                                         "Bastante",
                                         "Extremadamente"))

c11.c=DT::datatable(tabyl(c11)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c11.c)
#C12
summary(c12_es)
c12=factor(c12_es,levels=c(1:5),labels=c("Nada ",
                                         "Un poco ",
                                         "Lo normal ",
                                         "Bastante",
                                         "Extremadamente"))

c12.c=DT::datatable(tabyl(c12)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c12.c)
#C13
summary(c13_es)
c13=factor(c13_es,levels=c(1:5),labels=c("Nada ",
                                         "Un poco ",
                                         "Lo normal ",
                                         "Bastante",
                                         "Extremadamente"))

c13.c=DT::datatable(tabyl(c13)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c13.c)

#C14
summary(c14_es)
c14=factor(c14_es,levels=c(1:5),labels=c("Nada ",
                                         "Un poco ",
                                         "Lo normal ",
                                         "Bastante",
                                         "Extremadamente"))

c14.c=DT::datatable(tabyl(c14)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c14.c)
####

#C15
summary(c15_es)
c15=factor(c15_es,levels=c(1:5),labels=c("Nada ",
                                         "Un poco ",
                                         "Lo normal ",
                                         "Bastante",
                                         "Extremadamente"))

c15.c=DT::datatable(tabyl(c15)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c15.c)


####

#C16
summary(c16_es)
c16=factor(c16_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c16.c=DT::datatable(tabyl(c16)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c16.c)

#C17
summary(c17_es)
c17=factor(c17_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c17.c=DT::datatable(tabyl(c17)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c17.c)
#C18
summary(c18_es)
c18=factor(c18_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c18.c=DT::datatable(tabyl(c18)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c18.c)
#C19
summary(c19_es)
c19=factor(c19_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c19.c=DT::datatable(tabyl(c19)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c19.c)
#C20
summary(c20_es)
c20=factor(c20_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c20.c=DT::datatable(tabyl(c20)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c20.c)
#C21
summary(c21_es)
c21=factor(c21_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c21.c=DT::datatable(tabyl(c21)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c21.c)
#C22
summary(c22_es)
c22=factor(c22_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c22.c=DT::datatable(tabyl(c22)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c22.c)
#C23
summary(c23_es)
c23=factor(c23_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c23.c=DT::datatable(tabyl(c23)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c23.c)
#C24
summary(c24_es)
c24=factor(c24_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c24.c=DT::datatable(tabyl(c24)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c24.c)
#C25
summary(c25_es)
c25=factor(c25_es,levels=c(1:5),labels=c("Muy insatisfecho",
                                         "Poco",
                                         "Lo normal",
                                         "Bastante satisfecho",
                                         "Muy satisfecho"))

c25.c=DT::datatable(tabyl(c25)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c25.c)

####

#C26
summary(c26_es)
c26=factor(c26_es,levels=c(1:5),labels=c("Nunca",
                                         "Raramente",
                                         "Medianamente",
                                         "Frecuentemente",
                                         "Siempre"))

c26.c=DT::datatable(tabyl(c26)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c26.c)
####

####SAÚDE AUTOREFERIDA##### 

#C27 - possui doença?
summary(c27_es)
c27=factor(c27_es,levels=c(1:2),labels=c("Sim","Não"))

DT::datatable(tabyl(c27)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=86,n=318, conf.level = 0.95)#Sim
prop.test(x=225,n=318, conf.level = 0.95)#Não
prop.test(x=7,n=318, conf.level = 0.95)#NA

#C27.1 - Quais?
summary(c27_1_es)
tabyl(c27_1_es)

#C28 - recebe tratamento médico?
summary(c28_es)
c28=factor(c28_es,levels=c(1:2),labels=c("Sim","Não"))

DT::datatable(tabyl(c28)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=35,n=318, conf.level = 0.95)#Sim
prop.test(x=274,n=318, conf.level = 0.95)#Não
prop.test(x=9,n=318, conf.level = 0.95)#NA


#C28.1 - quais?
summary(c28_1_es)
tabyl(c28_1_es)

#C29 - Recebe tratamento psicologico?
summary(c29_es)
c29=factor(c29_es,levels=c(1:2),labels=c("Sim","Não"))

DT::datatable(tabyl(c29)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=10,n=318, conf.level = 0.95)#Sim
prop.test(x=300,n=318, conf.level = 0.95)#Não
prop.test(x=8,n=318, conf.level = 0.95)#NA

#C29.1 - quais? 
summary(c29_1_es)
tabyl(c29_1_es)

#C30 - Tem alguma deficiencia física?
summary(c30_es)
c30=factor(c30_es,levels=c(1:2),labels=c("Sim","Não"))

DT::datatable(tabyl(c30)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=8,n=318, conf.level = 0.95)#Sim
prop.test(x=305,n=318, conf.level = 0.95)#Não
prop.test(x=5,n=318, conf.level = 0.95)#NA


#C30.1 - quais?
summary(c30_1_es)
tabyl(c30_1_es)


#C31 -Tem alguma deficiencia auditiva? 
summary(c31_es)
c31=factor(c31_es,levels=c(1:2),labels=c("Sim","Não"))

DT::datatable(tabyl(c31)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#IC
prop.test(x=6,n=318, conf.level = 0.95)#Sim
prop.test(x=304,n=318, conf.level = 0.95)#Não
prop.test(x=8,n=318, conf.level = 0.95)#NA

#C31.1 - quais?
summary(c31_1_es)
tabyl(c31_1_es)

#C32 - Tem alguma deficiência visual? 
summary(c32_es)
c32=factor(c32_es,levels=c(1:2),labels=c("Sim","Não"))

DT::datatable(tabyl(c32)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
#IC
prop.test(x=90,n=318, conf.level = 0.95)#Sim
prop.test(x=221,n=318, conf.level = 0.95)#Não
prop.test(x=7,n=318, conf.level = 0.95)#NA


#C32.1 - quais?
summary(c32_1_es)
tabyl(c32_1_es)

####COVID JOÃO##### 

#C33
summary(c33_es)
c33=factor(c33_es,levels=c(1:2),labels=c("Sim","Não"))

c33.c=DT::datatable(tabyl(c33)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c33.c)
#C34
summary(c34_es)
c34=factor(c34_es,levels=c(1:2),labels=c("Sim","Não"))

c34.c=DT::datatable(tabyl(c34)%>%
                      adorn_totals("row") %>%
                      adorn_pct_formatting(digits = 2))
DT::datatable(c25.c)

#COMENTARIOS
table(coment_es)


###CRIANDO e DICOTOMIZANDO VARIAVEIS----

#Dicotomizando ou criando faixas

#library(tidyverse)
library(forcats)
#preciso pegar as outras e trazer pra cá. O que mais eu vou dicotomizar?

#CRIANDO A VARIAVEL FAIXA ETARIA IBGE = DE 5 EM CINCO ANOS = FONTE IBGE
#faixa_etaria_ibge=cut(idade_es, breaks = c(18,19,24,29,34,39,44,49,54,59,+Inf), labels = c("18-19 anos","20-24 anos","25-29 anos","30-34 anos","35-39 anos","40-44 anos","45-49 anos","50-54 anos","55-59 anos",">= 60 anos"))
#summary(faixa_etaria_ibge)

#idade_cat=cut(idade, breaks =  c(-Inf,18,39,49,59,+Inf), labels = c("<18 anos", "18-39 anos", "40-49 anos","50-59 anos", ">= 60 anos"))

attach(dados)
idade_cat=cut(idade_es, breaks =  c(-Inf,29,44,59,+Inf), labels = c("18-29 anos", "30-44 anos", "45-59 anos",">= 60 anos"))
summary(idade_cat)

DT::datatable(tabyl(idade_cat)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#Situação migratória

#Ordenada
situmigra_faixa= fct_collapse(situmigra,`Refugiado ou solicitante de refugio` = c("Solicitante de refúgio","Refugiado"),
                              `Residente ou solicitante de residência`=c( "Solicitante de autorização de residencia",  "Permissão de residencia por prazo determinado",  "Permissão de residencia por prazo indeterminado"),  
                              `Indocumentado`= c("Sem documentos"), 
                              `Outros`=("Outro"))
class(situmigra_faixa)

#Não ordenada
situmigra_faixa2= fct_collapse(situmigra2,`Refugiado ou solicitante de refugio` = c("Solicitante de refúgio","Refugiado"),
                               `Residente ou solicitante de residência`=c( "Solicitante de autorização de residencia",  "Permissão de residencia por plazo determinado",  "Permissão de residencia por plazo indeterminado"),  
                               `Indocumentado`= c("Sem documentos"), 
                               `Outros`=("Outro"))
class(situmigra_faixa2)
summary(situmigra_faixa2)

#fiz não ordenada, para que o R não faça analises erradas nas regressões ver em: https://stats.stackexchange.com/questions/233455/interpretation-of-l-q-output-from-a-negative-binomial-glm-with-categorical-d - e isso foi lá do inicio, passandor por aqui, e chegando até a regressão....

DT::datatable(tabyl(situmigra_faixa)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#região do Brasil

regiao_brasil_residencia = fct_collapse(estado,
                                        `Norte`=c('Amapá (AP)','Amazonas (AM)','Pará (PA)','Rondônia (RO)','Tocantins (TO)','Acre (AC)', 'Roraima (RR)'),
                                        `Nordeste` = c('Alagoas (AL)', 'Bahia (BA)', 'Ceará (CE)', 'Maranhão (MA)', 'Paraíba (PB)', 'Pernambuco (PE)', 'Piauí (PI)', 'Rio Grande do Norte (RN)', 'Sergipe (SE)'), 
                                        `Sul` = c('Paraná (PR)', 'Rio Grande do Sul (RS)', 'Santa Catarina (SC)'),
                                        `Sudeste` = c('Espírito Santo (ES)', 'Minas Gerais (MG)', 'Rio de Janeiro (RJ)', 'São Paulo (SP)'), 
                                        `Centro-Oeste` =c('Distrito Federal  (DF)', 'Goiás (GO)', 'Mato Grosso (MT)', 'Mato Grosso do Sul (MS)'))

DT::datatable(tabyl(regiao_brasil_residencia)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#Religião
religiao_dic = fct_collapse(a6,
                            `Tenho` = c('Catolica','Evangelica','Testemunhas de Jeova',' Cultos Afroamericanos','Adventista' ,'Mormom'),
                            `Não Tenho` = c('Não tenho religão'),
                            `Outro` = c('Outra'))

DT::datatable(tabyl(religiao_dic)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#Filhos
filhos_dic = fct_collapse(a7,
                          `Tenho` = c('1','2','3 ou mais'),
                          `Não Tenho` = c('0'))

DT::datatable(tabyl(filhos_dic)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#Escolaridade
escolaridade_dic = fct_collapse(a8,
                                `Nunca frequentrou a escola` = c('Nunca frequentou a escola'),
                                `Educação básica` = c('Educação básica'),
                                `Ensino Fundamental` = c('Ensino fundamental incompleto','Ensino fundamental completo'),
                                `Ensino Médio` = c('Ensino médio incompleto','Ensino médio completo'),
                                `Ensino Superior` = c('Ensino superior incompleto','Ensino superior completo','Pós-graduação incompleta','Pós-graduação completa'))

DT::datatable(tabyl(escolaridade_dic )%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#Renda
a14_dicot = fct_collapse(a14,`Até 500 reais` = c('Até 500 reais'),
                         `501 a 1.500 reais` = c('Entre 501 e 1.000 reais','Entre 1.001 e 1.500 reais'),
                         `1.501 a 3.000 reais` = c('Entre 1.501 e 2.000 reais','Entre 2.001 e 2.500 reais','Entre 2.501 e 3.000 reais'),
                         `3.000 a 5000 reais` = c('Entre 3.001 e 4.000 reais','Entre 4.001 e 5.000 reais'),
                         `+ de 5.000 reais` = c('Mais de 5.000 reais'))

DT::datatable(tabyl(a14_dicot )%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#Trabalho
a10_dicot = fct_collapse(a10,`Sim` = c('Sim, em tempo parcial (até 20 horas semanais)','Sim, em tempo integral (mais de 30 horas semanais','Sim, mas se trata de trabalho eventual'),
                         `Não` = c('não'))

DT::datatable(tabyl(a10_dicot )%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

a11_dicot = fct_collapse(a11,`Sim` = c('Sim, em tempo parcial (até 20 horas semanais)','Sim, em tempo integral (mais de 30 horas semanais','Sim, mas se trata de trabalho eventual'),
                         `Não` = c('não'))

DT::datatable(tabyl(a11_dicot )%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#data que chegou ao brasil
summary(b7_es)
class(b7_es)
b7=as.Date(b7_es)
class(b7)
summary(b7)
anob7_numeric=year(b7)
summary(anob7)
table(anob7)
class(anob7)
anob7_factor=factor(anob7)
class(anob7)

DT::datatable(tabyl(anob7_numeric)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

DT::datatable(tabyl(anob7_factor)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


#dicotomizando o ano
ano_chegada_dic = fct_collapse(anob7_factor,`2010-2015` = c('2010','2013','2014','2015'),
                               `2016` = c('2016'),
                               `2017` = c('2017'),
                               `2018` = c('2018'),
                               `2019` = c('2019'),
                               `2020` = c('2020'),
                               `2021` = c('2021'))

DT::datatable(tabyl(ano_chegada_dic)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

ano_chegada_dic1 = fct_collapse(anob7_factor,`2010-2015` = c('2010','2013','2014','2015'),
                               `2016-2017` = c('2016','2017'),
                               `2018-2019` = c('2018','2019'),
                               `2020` = c('2020'),
                               `2021` = c('2021'))

DT::datatable(tabyl(ano_chegada_dic1)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))


anob7=as.factor(anob7)
ano_chegada_dic2= fct_collapse(anob7_factor,
                               `2010-2015` = c('2010','2013','2014','2015'),
                               `2016-2018` = c('2016','2017','2018'),
                               `2019-2021`=c('2019','2020','2021'))

#ano_chegada_dic2=droplevels(ano_chegada_dic2, exclude= is.na(ano_chegada_dic2))

DT::datatable(tabyl(ano_chegada_dic2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))
# e tem as variaveis do bloco C...

#### AB >> por sexo.----

#checklist
tabyl(sexo)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

tapply(idade_es,sexo,summary)#sexo por idade
#sexo por faixa etaria
tapply(faixa_etaria_ibge,sexo,summary)
#boxplot(idade_es~sexo)
#table(faixa_etaria_ibge,sexo)
#prop.table(table(faixa_etaria_ibge,sexo))*100
#(cbind(table(faixa_etaria_ibge,sexo),prop.table(table(faixa_etaria_ibge,sexo))*100))
tapply(idade_es,sexo,summary)#sexo por idade
tapply(situmigra_faixa,sexo, summary)#sexo por status migratório
tapply(regiao_brasil_residencia,sexo, summary)#regiao de residencia

#bloco A
tapply(a2,sexo, summary)#Sexo por cor de pele.
tapply(a3,sexo, summary)#Sexo por estado civil
tapply(a6,sexo, summary)#Sexo por religião
tapply(religiao_dic,sexo, summary)#Sexo por religião
tapply(a7,sexo, summary)#sexo por quantidade de filhos
tapply(a8,sexo, summary)#sexo por escolaridade
tapply(escolaridade_dic,sexo, summary)#sexo por escolaridade
tapply(a10,sexo, summary)#sexo por trab ven
tapply(a10_dicot,sexo, summary)#sexo por trab ven
tapply(a11,sexo, summary)#sexo por trab Br
tapply(a11_dicot,sexo, summary)#sexo por trab Br
tapply(a12,sexo, summary)#sexo por Renda Ven
levels(a12)
tapply(a14,sexo, summary)#sexo por Renda Br
levels(a14)
tapply(a14_dicot,sexo, summary)#sexo por Renda Br

#Bloco B
tapply(ano_chegada_dic,sexo, table)#Quando chegou ao Brasil
tapply(anob7,sexo, table)#Quando chegou ao Brasil
summary(anob7)
hist(anob7)
tapply(estado,situmigra_faixa, summary)#por estado em que ingressou no pais- não vai entrar agora...### NÃO FIZ
#Coincidencia entre estado de ingresso e residencia.


#bloco C
tapply(c2,sexo, summary)#Sexo por satisfação com saúde
tapply(c27,sexo, summary)
tapply(c28,sexo, summary)
tapply(c29,sexo, summary)
tapply(c30,sexo, summary)#fisica
tapply(c31,sexo, summary)#auditiva
tapply(c32,sexo, summary)#visual

#### AB >> #Status Migratório----
#Status Migratório
#checklist
tapply(sexo,situmigra_faixa,table)#por sexo

#a=tapply(sexo,situmigra_faixa,table)
#aa=table(sexo,situmigra_faixa)
#library(pander)                         # load pkg
#panderOptions('table.split.table', Inf) # not to split table
#set.caption('Hello Fisher!')            # add caption
#pander(head(aa))# show (almost) any R object in markdown
tapply(idade_cat,situmigra_faixa,summary)
tapply(idade_es,situmigra_faixa,summary)#media de idade
tapply(faixa_etaria_ibge, situmigra_faixa,table)#faixa etaria
tapply(regiao_brasil_residencia,situmigra_faixa, summary)#por região

summary(situmigra_faixa)

#blocoA
tapply(a2,situmigra_faixa, summary)# por cor de pele.
tapply(a3,situmigra_faixa, summary)# por estado civil
tapply(religiao_dic,situmigra_faixa, table) #religiao
tapply(filhos_dic,situmigra_faixa, summary)# por quantidade de filhos
tapply(escolaridade_dic,situmigra_faixa, summary)# por escolaridade
tapply(a10_dicot,situmigra_faixa, summary)#trab ven
tapply(a11_dicot,situmigra_faixa, summary)#trab bra
tapply(a14_dicot,situmigra_faixa, plot)#renda

#bloco B
tapply(ano_chegada_dic2,situmigra_faixa, summary)#Quando chegou ao Brasil
tapply(estado,situmigra_faixa, summary)#por estado em que ingressou no pais- não vai entrar agora...
#Coincidencia entre estado de ingresso e residencia.

#Bloco C
tapply(c2,situmigra_faixa, table)
tapply(c27,situmigra_faixa, table)#doença
tapply(c28,situmigra_faixa, table)#tratamento medico
tapply(c29,situmigra_faixa, table)#tratamento psicológico
tapply(c30,situmigra_faixa, table)#fisica
tapply(c31,situmigra_faixa, table)#audivitva
tapply(c32,situmigra_faixa, table)#visual


#### AB >> #ANO de chegada ao Brasil----
#Data de chegada ao Brasil

anob7=as.factor(anob7)
ano_chegada_dic2= fct_collapse(anob7,
                               `2010-2015` = c('2010','2013','2014','2015'),
                               `2016-2018` = c('2016','2017','2018'),
                               `2019-2021`=c('2019','2020','2021'))

ano_chegada_dic2=droplevels(ano_chegada_dic2, exclude =c("1955","1992","1999", is.na(ano_chegada_dic2)))

ano_chegada_dic2=droplevels(ano_chegada_dic2, exclude= is.na(ano_chegada_dic2))

DT::datatable(tabyl(ano_chegada_dic2)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting())

#checklist
tapply(sexo,ano_chegada_dic2,table)#por sexo
tapply(idade_es,ano_chegada_dic2,mean)
#tapply(estado,ano_chegada_dic2, summary)#por estado em que ingressou no pais
tapply(regiao_brasil_residencia,ano_chegada_dic2, summary)#por região
tapply(idade_cat, ano_chegada_dic2,table)#faixa etaria
tapply(situmigra_faixa, ano_chegada_dic2,table)#faixa etaria

#bolco A
tapply(a2,ano_chegada_dic2, summary)# por cor de pele.
tapply(a3,ano_chegada_dic2, summary)# por estado civil
tapply(religiao_dic,ano_chegada_dic2, summary) #religiao
tapply(filhos_dic,ano_chegada_dic2, summary)# por quantidade de filhos
tapply(escolaridade_dic,ano_chegada_dic2, summary)# por escolaridade
tapply(a10_dicot,ano_chegada_dic2, summary)#trab ven
tapply(a11_dicot,ano_chegada_dic2, summary)#trab bra
tapply(a14_dicot,ano_chegada_dic2, summary)#renda

#bloco B
#Quando chegou ao Brasil
tapply(escolaridade_dic,ano_chegada_dic2, summary)

#Coincidencia entre estado de ingresso e residencia.

#Bloco C
tapply(c2,ano_chegada_dic2, table)
tapply(c27,ano_chegada_dic2, table)#doença
tapply(c28,ano_chegada_dic2, table)#tratamento medico
tapply(c29,ano_chegada_dic2, table)#tratamento psicológico
tapply(c31,ano_chegada_dic2, table)#audivitva
tapply(c32,ano_chegada_dic2, table)#visual

#### AB >> #Por possuir doenças----
#Agora vamos ver por possuir doenças

tapply(c2,c27, summary)
tapply(c28,c27, summary)
tapply(c29,c27, summary)
tapply(c30,c27, summary)
tapply(c31,c27, summary)
tapply(c32,c27, summary)

#Agora vamos ver por satisfação com a saúde
tapply(c2,c27, summary)
tapply(c28,c27, summary)
tapply(c29,c27, summary)
tapply(c30,c27, summary)
tapply(c31,c27, summary)
tapply(c32,c27, summary)





###Porcentagens e IC - descrição ----- 

#ano de chagada_
table(ano_chegada_dic2)
prop.table(ano_chegada_dic2)
plot(ano_chegada_dic)
prop.test(x=11,n=318, conf.level = 0.95)#2010-2015
prop.test(x=111,n=318, conf.level = 0.95)#2016-2018
prop.test(x=185,n=318, conf.level = 0.95)#2019-2021
prop.test(x=12,n=318, conf.level = 0.95)#2019-2021

#status migratório
table(situmigra_faixa)

DT::datatable(tabyl(situmigra_faixa)%>%
                 adorn_totals("row") %>%
                 adorn_pct_formatting(digits = 2))


prop.test(x=74,n=318, conf.level = 0.95)#Refugiados
prop.test(x=208,n=318, conf.level = 0.95)#Residentes
prop.test(x=33,n=318, conf.level = 0.95)#Idocumentado
prop.test(x=4,n=318, conf.level = 0.95)#Outros

#Sexo
DT::datatable(tabyl(sexo)%>%
                 adorn_totals("row") %>%
                 adorn_pct_formatting(digits = 2))

prop.test(x=208,n=318, conf.level = 0.95)#Mulheres
prop.test(x=111,n=318, conf.level = 0.95)#Homens


#faixa etaria

#CLassificação OMS(?)
idade_cat=cut(idade_es, breaks =  c(-Inf,29,44,59,+Inf), labels = c("18-29 anos", "30-44 anos", "45-59 anos",">= 60 anos"))
summary(idade_cat)
#faixa_etaria_OMS=cut(idade_es, breaks = seq(18,29,44,59+Inf), include.lowest = T) ta errado também
#summary(faixa_etaria_OMS)
#which(lowest(idade_es)) 
table(idade_es)
hist(idade_es)
DT::datatable(tabyl(idade_cat)%>%
                 adorn_totals("row") %>%
                 adorn_pct_formatting(digits = 2))


prop.test(x=83,n=318, conf.level = 0.95)#18-29
prop.test(x=158,n=318, conf.level = 0.95)#30-44
prop.test(x=72,n=318, conf.level = 0.95)#45-59
prop.test(x=6,n=318, conf.level = 0.95)#>=60

#cor de pele
DT::datatable(tabyl(a2)%>%
                 adorn_totals("row") %>%
                 adorn_pct_formatting(digits = 2))

prop.test(x=8,n=318, conf.level = 0.95)
prop.test(x=6,n=318, conf.level = 0.95)
prop.test(x=164,n=318, conf.level = 0.95)
prop.test(x=111,n=318, conf.level = 0.95)
prop.test(x=5,n=318, conf.level = 0.95)
prop.test(x=4,n=318, conf.level = 0.95)
prop.test(x=21,n=318, conf.level = 0.95)

Escolaridade

DT::datatable(tabyl(escolaridade_dic)%>%
                 adorn_totals("row") %>%
                 adorn_pct_formatting(digits = 2))
lenght(escolaridade)
prop.test(x=5,n=318, conf.level = 0.95)
prop.test(x=5,n=318, conf.level = 0.95)
prop.test(x=10,n=318, conf.level = 0.95)
prop.test(x=85,n=318, conf.level = 0.95)
prop.test(x=212,n=318, conf.level = 0.95)
prop.test(x=2,n=318, conf.level = 0.95)

# Agora vamos para os calculos mais robustos
##ANALISES AVANÇADAS##### 
### Medidas de Associação -----
##Teste Qui-quadrado - desfecho presença de doença ##### 

### Correlações- testes qui quadrado.
#aqui serão realizados testes qui-quadrado de indepedência, para avaliar se há alguma associação entre as variaveis de desfecho - presença ou não de doenças, utilização ou não dos serviços de saúde e satisfação ou não com a saúde, com as variaveis sociodemograficas.

# H 0 :  Não há associação
# H a :  Há Associação  ?? = 0.05

library(descr)

##Ano de Chegada
#anob7
descr::CrossTable(anob7,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(anob7,c27,chisq = T)$CST %>%
  pander::pander()

##Sexo
#tabela de contigência
descr::CrossTable(sexo,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(sexo,c27,chisq = T)$CST %>%
  pander::pander()

#sugere associação

##situação Migratória
#tabela de contigência
descr::CrossTable(situmigra,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(situmigra,c27,chisq = T)$CST %>%
  pander::pander()

# Não sugere associação

#situmigra_faixa
#tabela de contigência
descr::CrossTable(situmigra_faixa,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(situmigra_faixa,c27,chisq = T)$CST %>%
  pander::pander()
# Não sugere associação

#Faixa etaria
#tabela de contigência
descr::CrossTable(idade_cat,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(idade_cat,c27,chisq = T)$CST %>%
  pander::pander()

#Sugere associação

#Estado de residencia
#tabela de contigencia
descr::CrossTable(estado,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(estado,c27,chisq = T)$CST %>%
  pander::pander()
#região
descr::CrossTable(regiao_brasil_residencia,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(regiao_brasil_residencia,c27,chisq = T)$CST %>%
  pander::pander()
#não sugere associação com o estado de residencia (nem região)

#curso pt caritas
#tabela de contigencia
descr::CrossTable(curso,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(curso,c27,chisq = T)$CST %>%
  pander::pander()

#não sugere associação.

#bloco A
#cor/raça
#tabela de contigencia
descr::CrossTable(a2,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a2,c27,chisq = T)$CST %>%
  pander::pander()

#não sugere associação.

#estado civil
#tabela de contigencia
descr::CrossTable(a3,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a3,c27,chisq = T)$CST %>%
  pander::pander()
#não sugere associação.

#morar com outra pessoa
#tabela de contigencia
descr::CrossTable(a4,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a4,c27,chisq = T)$CST %>%
  pander::pander()
#não sugere associação.

#apatridia
#tabela de contigencia
descr::CrossTable(a5,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a5,c27,chisq = T)$CST %>%
  pander::pander()
#sugere associação

#religiao
#tabela de contigencia
descr::CrossTable(a6,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a6,c27,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#filhos
#tabela de contigencia
descr::CrossTable(a7,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a7,c27,chisq = T)$CST %>%
  pander::pander()
# não sugere associação, por pouco pvalor = 0.05114

#escolaridade
#tabela de contigencia
descr::CrossTable(a8,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a8,c27,chisq = T)$CST %>%
  pander::pander()

#tabela de contigencia
descr::CrossTable(escolaridade_dic,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(escolaridade_dic,c27,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#falar outros idiomas
#tabela de contigencia
descr::CrossTable(a9,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a9,c27,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#Trab Ven
#tabela de contigencia
descr::CrossTable(a10,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a10,c27,chisq = T)$CST %>%
  pander::pander()
#tabela de contigencia
descr::CrossTable(a10_dicot,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a10_dicot,c27,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#trab Brasil
#tabela de contigencia
descr::CrossTable(a11,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a11,c27,chisq = T)$CST %>%
  pander::pander()
#tabela de contigencia
descr::CrossTable(a11_dicot,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a11_dicot,c27,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#Renda Ven
#tabela de contigencia
descr::CrossTable(a12,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a12,c27,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#renda Brasil
#tabela de contigencia
descr::CrossTable(a14,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a14,c27,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#Domínio da lingua portuguesa
#tabela de contigencia
descr::CrossTable(a16,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a16,c27,chisq = T)$CST %>%
  pander::pander()
# sugere associação

#discriminação
#tabela de contigencia
descr::CrossTable(a17,c27, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a17,c27,chisq = T)$CST %>%
  pander::pander()
# não sugere associação





##Teste Qui-quadrado - desfecho utilização de serviços ##### 

### Correlações- testes qui quadrado.
#aqui serão realizados testes qui-quadrado de indepedência, para avaliar se há alguma associação entre as variaveis de desfecho - presença ou não de doenças, utilização ou não dos serviços de saúde e satisfação ou não com a saúde, com as variaveis sociodemograficas.

# H 0 :  Não há associação
# H a :  Há Associação  ?? = 0.05

#library(descr)


## Anos de Chegada
#anob7
descr::CrossTable(anob7,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(anob7,c28,chisq = T)$CST %>%
  pander::pander()

##Sexo
#tabela de contigência
descr::CrossTable(sexo,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(sexo,c28,chisq = T)$CST %>%
  pander::pander()

#sugere associação? - 0.05341 (fraca?)

##situação Migratória
#tabela de contigência
descr::CrossTable(situmigra,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(situmigra,c28,chisq = T)$CST %>%
  pander::pander()

# Não sugere associação

#situmigra_faixa
#tabela de contigência
descr::CrossTable(situmigra_faixa,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(situmigra_faixa,c28,chisq = T)$CST %>%
  pander::pander()
# Não sugere associação

#Faixa etaria
#tabela de contigência
descr::CrossTable(idade_cat,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(idade_cat,c28,chisq = T)$CST %>%
  pander::pander()

#Sugere associação

#Estado de residencia
#tabela de contigencia
descr::CrossTable(estado,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(estado,c28,chisq = T)$CST %>%
  pander::pander()
#região
descr::CrossTable(regiao_brasil_residencia,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(regiao_brasil_residencia,c28,chisq = T)$CST %>%
  pander::pander()
#não há associação com o estado de residencia (nem região)

#curso pt caritas
#tabela de contigencia
descr::CrossTable(curso,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(curso,c28,chisq = T)$CST %>%
  pander::pander()

#não sugere associação.

#bloco A
#cor/raça
#tabela de contigencia
descr::CrossTable(a2,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a2,c28,chisq = T)$CST %>%
  pander::pander()

#não sugere associação.

#estado civil
#tabela de contigencia
descr::CrossTable(a3,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a3,c28,chisq = T)$CST %>%
  pander::pander()
#não sugere associação.

#morar com outra pessoa
#tabela de contigencia
descr::CrossTable(a4,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a4,c28,chisq = T)$CST %>%
  pander::pander()
#não sugere associação.

#apatridia
#tabela de contigencia
descr::CrossTable(a5,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a5,c28,chisq = T)$CST %>%
  pander::pander()
#sugere associação

#religiao
#tabela de contigencia
descr::CrossTable(a6,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a6,c28,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#filhos
#tabela de contigencia
descr::CrossTable(a7,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a7,c28,chisq = T)$CST %>%
  pander::pander()
# não sugere associação.

#escolaridade
#tabela de contigencia
descr::CrossTable(a8,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a8,c28,chisq = T)$CST %>%
  pander::pander()

#tabela de contigencia
descr::CrossTable(escolaridade_dic,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(escolaridade_dic,c28,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#falar outros idiomas
#tabela de contigencia
descr::CrossTable(a9,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a9,c28,chisq = T)$CST %>%
  pander::pander()
# sugere associação

#Trab Ven
#tabela de contigencia
descr::CrossTable(a10,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a10,c28,chisq = T)$CST %>%
  pander::pander()

#tabela de contigencia
descr::CrossTable(a10_dicot,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a10_dicot,c28,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#trab Brasil
#tabela de contigencia
descr::CrossTable(a11,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a11,c28,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#tabela de contigencia
descr::CrossTable(a11_dicot,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a11_dicot,c28,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#Renda Ven
#tabela de contigencia
descr::CrossTable(a12,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a12,c28,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#renda Brasil
#tabela de contigencia
descr::CrossTable(a14,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a14,c28,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#Domínio da lingua portuguesa
#tabela de contigencia
descr::CrossTable(a16,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a16,c28,chisq = T)$CST %>%
  pander::pander()
# sugere associação

#discriminação
#tabela de contigencia
descr::CrossTable(a17,c28, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a17,c28,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

##Teste Qui-quadrado - satisfação com saúde ##### 

## DICOTOMIZANDO Satisfação com saúde
#install.packages("tidyverse")
#library(tidyverse)
#library(forcats)

#Neste estudo, a medida foi dicotomizada pela combinação das categorias "muito bom" e "bom", para fazer referência a uma boa autopercepção de saúde, e em "regular", "ruim" e "muito ruim", para referir uma autopercepção de saúde ruim.


#
c2=factor(c2_es,levels=c(1:5),labels=c("Muito insatisfeito",
                                       "Insatisfeito",
                                       "Nem satisfeito, nem insatisfeito",
                                       "Satisfeito",
                                       "Muito satisfeito"), ordered=T) 

#
satisf_saude_dic = fct_collapse(c2,             `Insatisfeito` = c('Muito insatisfeito','Insatisfeito'),`Nem satisfeito, nem insatisfeito` = c('Nem satisfeito, nem insatisfeito'),             `Satisfeito ou muito satisfeito` = c('Satisfeito','Muito satisfeito'))

        

### Correlações- testes qui quadrado.
#aqui serão realizados testes qui-quadrado de indepedência, para avaliar se há alguma associação entre as variaveis de desfecho - presença ou não de doenças, utilização ou não dos serviços de saúde e satisfação ou não com a saúde, com as variaveis sociodemograficas.

# H 0 :  Não há associação
# H a :  Há Associação  ?? = 0.05

library(descr)

## Anos de Chegada
#anob7
descr::CrossTable(anob7,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(anob7,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()

#não sugere associação

##Sexo
#tabela de contigência
descr::CrossTable(sexo,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(sexo,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()

#não sugere associação

##situação Migratória
#tabela de contigência
descr::CrossTable(situmigra,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(situmigra,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()

# Não sugere associação

#situmigra_faixa
#tabela de contigência
descr::CrossTable(situmigra_faixa,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(situmigra_faixa,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# Não sugere associação

#Faixa etaria
#tabela de contigência
descr::CrossTable(idade_cat,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(idade_cat,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()

#Não sugere associação

#Estado de residencia
#tabela de contigencia
descr::CrossTable(estado,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(estado,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
#região
descr::CrossTable(regiao_brasil_residencia,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(regiao_brasil_residencia,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
#não há associação com o estado de residencia (nem região)

#curso pt caritas
#tabela de contigencia
descr::CrossTable(curso,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(curso,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()

#não sugere associação.

#bloco A
#cor/raça
#tabela de contigencia
descr::CrossTable(a2,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a2,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()

#não sugere associação.

#estado civil
#tabela de contigencia
descr::CrossTable(a3,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a3,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
#não sugere associação.

#morar com outra pessoa
#tabela de contigencia
descr::CrossTable(a4,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a4,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
#não sugere associação.

#apatridia
#tabela de contigencia
descr::CrossTable(a5,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a5,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
#não sugere associação

#religiao
#tabela de contigencia
descr::CrossTable(a6,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a6,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#filhos
#tabela de contigencia
descr::CrossTable(a7,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a7,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#escolaridade
#tabela de contigencia
descr::CrossTable(a8,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a8,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()

#tabela de contigencia
descr::CrossTable(escolaridade_dic,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(escolaridade_dic,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#falar outros idiomas
#tabela de contigencia
descr::CrossTable(a9,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a9,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#Trab Ven
#tabela de contigencia
descr::CrossTable(a10,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a10,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()

#tabela de contigencia
descr::CrossTable(a10_dicot,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a10_dicot,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#trab Brasil
#tabela de contigencia
descr::CrossTable(a11,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a11,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#tabela de contigencia
descr::CrossTable(a11_dicot,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a11_dicot,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#Renda Ven
#tabela de contigencia
descr::CrossTable(a12,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a12,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#renda Brasil
#tabela de contigencia
descr::CrossTable(a14,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a14,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação

#Domínio da lingua portuguesa
#tabela de contigencia
descr::CrossTable(a16,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a16,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# sugere associação

#discriminação
#tabela de contigencia
descr::CrossTable(a17,satisf_saude_dic, expected = T, prop.c = F, prop.chisq = F, prop.t = F) %>% 
  pander::pander()
#Teste com P valor e graus de liberdade
descr::CrossTable(a17,satisf_saude_dic,chisq = T)$CST %>%
  pander::pander()
# não sugere associação


### REGRESSÕES  -----
###Doença como desfecho  ----

#if(!require(pacman)) install.packages("pacman")
#library(pacman)

#pacman::p_load(dplyr, psych, car, MASS, DescTools, QuantPsyc, ggplot2)

#desfecho >> presença ou não de doenças. VARIAVEL DEPENDENTE.
#Regressão log binomial para calcular o ODDS RATIO do desfecho "dicotomomico" Family = logit = odds-ratio// Family= log= razão de prevalencia.
#Também viRegressão poisson ara calcular o RAZÃO DE PREVALENCIA do desfecho "dicotomomico"

#Regressão Logisitica (LOGIT = Odds-Ratio) Binomial. Onde a variavel dependente será a presença ou não de doenças, de acordo com as variavéis sociodemograficas (BLOCO A)

#Como é feito? Exemplo abaixo.

#chamando o modelo. Prestar atenção nas explicações que seguem.

#m1=glm(c27bin~anob7,family=binomial(link=logit))#modelo

#summary(m1)#Fornece as estimativas pontuais dos parâmetros e os seus erros padrão, os valores observados da estatística de Wald e os p-valores do teste de Wald, entre outras informações.
#OR1=exp(coef(m1))# Pode-se demonstrar matematicamente que a razão de chance é o exponencial da estimativa pontual
#Não se usa nas tabelas porém>>> confint(m1)#Os intervalos de 95% de confiança para os parâmetros do modelo, com base na estatística de Wald
#ICOR1=exp(confint(m1))#Os intervalos de confiança para as razões de chance (odds ratio - OR), fixando o nível de confiança de 95%
#round((cbind(OR1, ICOR1)),3)#aqui junta todos e arredonda pra 3 casa decimais

#Vamos, lá!

#organizando a variavel C27= presença de doenças.
#no banco, ano está codificado em 1 e 2, sendo 1= Sim, 2=Não.


#Como factor, sim ou não
#c27=factor(c27_es,levels=c(1:2),labels=c("Sim","Não"))

#Para a descrição das frequências absolutas e relativas ela foi transformada em factor (sim,não), porém aqui ela deverá ser transformada em 0 ou 1 onde 0=não ocorrencia de doenças e 1= onde há ocorrência de doenças.

#então vamos recodificar pra avaliar...
summary(c27_es)
c27bin <- ifelse(c27_es == 1, 1,0)

DT::datatable(tabyl(c27bin)%>%
                 adorn_totals("row") %>%
                 adorn_pct_formatting(digits = 2))

#Usando o dplyr é case_when( variável == 1 ~ 1,
#Variável ==2 ~ 0)




#presença de doença por ano de chegada 
#o Ano de chegada está disitribuido de algumas formas.

#anob7>>Todos os anos (numérico)
#ano_chegada_dic >>2010-2015,2016,2017,2018,2019,2020,2021
#ano_chegada_dic1 >>2010-2015,2016-2017,2018-2019,2020,2021
#ano_chegada_dic2>> 2010-2015,2016-2018,2019-2021

attributes(anob7)
#attributes(ano_chegada_dic)
#attributes(ano_chegada_dic1)
#attributes(ano_chegada_dic2)

#tabelas

table(anob7,c27bin)
#table(anob77,c27bin)
#table(ano_chegada_dic,c27bin)
#table(ano_chegada_dic1,c27bin)
#table(ano_chegada_dic2,c27bin)


#View(c27bin)
#tabela
table(anob7,c27)
#chamando o modelo. 
m1=glm(c27bin~anob7,family=binomial(link=logit),data=dados)#ESSE!!!
summary(m1)
OR1=exp(coef(m1))
ICOR1=exp(confint(m1))
round((cbind(OR1, ICOR1)),3)


#presença de doença por status migratório
table(situmigra,c27)
m2=glm(c27bin~situmigra,family=binomial(link=logit))
summary(m2)
OR2=exp(coef(m2))
ICOR2=exp(confint(m2))
round((cbind(OR2, ICOR2)),3)


#Por sexo
table(sexo,c27)
m3=glm(c27bin~sexo,family=binomial(link=logit))
summary(m3)
OR3=exp(coef(m3))
ICOR3=exp(confint(m3))
round((cbind(OR3, ICOR3)),3)


#por idade 
table(idade_cat,c27)
m4=glm(c27bin~idade_cat,family=binomial(link=logit))
summary(m4)
OR4=exp(coef(m4))
ICOR4=exp(confint(m4))
round((cbind(OR4, ICOR4)),3)

#cor da pele
table(a2,c27)
m5=glm(c27bin~a2,family=binomial(link=logit))
summary(m5)
OR5=exp(coef(m5))
ICOR5=exp(confint(m5))
round((cbind(OR5, ICOR5)),3)

#Estado civil
table(a3,c27)
m6=glm(c27bin~a3,family=binomial(link=logit))
summary(m6)
OR6=exp(coef(m6))
ICOR6=exp(confint(m6))
round((cbind(OR6, ICOR6)),3)

#Se vive com mais alguém - compoisção domiciliar
table(a4,c27)
m7=glm(c27bin~a4,family=binomial(link=logit))
summary(m7)
OR7=exp(coef(m7))
ICOR7=exp(confint(m7))
round((cbind(OR7, ICOR7)),3)

#Apatridia
table(a5,c27)
m8=glm(c27bin~a5,family=binomial(link=logit))
summary(m8)
OR8=exp(coef(m8))
ICOR8=exp(confint(m8))
round((cbind(OR8, ICOR8)),3)

#Religião
table(a6,c27)
m9=glm(c27bin~a6,family=binomial(link=logit))
summary(m9)
OR9=exp(coef(m9))
ICOR9=exp(confint(m9))
round((cbind(OR9, ICOR9)),3)

#Filhos
table(a7,c27bin)
m10=glm(c27bin~a7,family=binomial(link=logit))
summary(m10)
OR10=exp(coef(m10))
ICOR10=exp(confint(m10))
round((cbind(OR10, ICOR10)),3)

#Escolaridade
table(a8,c27bin)
m11=glm(c27bin~a8,family=binomial(link=logit))
summary(m11)
OR11=exp(coef(m11))
ICOR11=exp(confint(m11))
round((cbind(OR11, ICOR11)),3)

#Falar outros idiomas
table(a9,c27)
m12=glm(c27bin~a9,family=binomial(link=logit))
summary(m12)
OR12=exp(coef(m12))
ICOR12=exp(confint(m12))
round((cbind(OR12, ICOR12)),3)

#Trab na Ven
table(a10,c27)
m13=glm(c27bin~a10,family=binomial(link=logit))
summary(m13)
OR13=exp(coef(m13))
ICOR13=exp(confint(m13))
round((cbind(OR13, ICOR13)),3)

#Trab no Br
table(a11,c27)
m14=glm(c27bin~a11,family=binomial(link=logit))
summary(m14)
OR14=exp(coef(m14))
ICOR14=exp(confint(m14))
round((cbind(OR14, ICOR14)),3)

#Renda Ven
table(a12,c27)
m15=glm(c27bin~a12,family=binomial(link=logit))
summary(m15)
OR15=exp(coef(m15))
ICOR15=exp(confint(m15))
round((cbind(OR15, ICOR15)),3)

#Renda Bra
table(a14,c27)
m16=glm(c27bin~a14,family=binomial(link=logit))
summary(m16)
OR16=exp(coef(m16))
ICOR16=exp(confint(m16))
round((cbind(OR16, ICOR16)),3)

#Dominio do portugues
table(a16,c27)
m17=glm(c27bin~a16,family=binomial(link=logit))
summary(m17)
OR17=exp(coef(m17))
ICOR17=exp(confint(m17))
round((cbind(OR17, ICOR17)),3)

#Sofreu discriminação
table(a17,c27)
m18=glm(c27bin~a17,family=binomial(link=logit))
summary(m18)
OR18=exp(coef(m18))
ICOR18=exp(confint(m18))
round((cbind(OR18, ICOR18)),3)

#Modelo ajustado = todas as variaveis em conjunto

mz=glm(c27bin~ situmigra+sexo+idade_cat+a3+a4+a5+a7+a8+a9+a10+a11+a14+a16+a17,family=binomial(link=logit))
summary(mz)
ORz=exp(coef(mz))
ICORz=exp(confint(mz))
round((cbind(ORz, ICORz)),3)

#So os que deram p valor significativo
mzz=glm(c27bin~ sexo+idade_cat+a5+a7+a16,family=binomial(link=logit))
summary(mzz)
ORzz=exp(coef(mzz))
ICORzz=exp(confint(mzz))
round((cbind(ORzz, ICORzz)),3)

#lembrar que isso é pra tirar a notação cientifica do codigo. e depois deve retornar a 0 (zero) que é o default.
getOption("scipen")
options(scipen=0)

#Since the ln (odds ratio) = log odds, elog odds = odds ratio. So to turn our -2.2513 above into an odds ratio, we calculate e-2.2513, which happens to be about 0.1053:1. So the probability we have a thief is 0.1053/1.1053 = 0.095, so 9.5 %. Notice how we converted the odds ratio to a probability by dividing the first part of the ratio with the sum of both parts (the total). Notice also that we came to our final answer without any involved calculations, assuming, of course, we have a calculator to help us with the logarithms Fonte https://www.statisticshowto.com/log-odds/

###Utilização dos serviços de saúde como desfecho  ----


#então vamos recodificar pra avaliar...
summary(c28_es)
c28bin <- ifelse(c28_es == 1, 1,0)

DT::datatable(tabyl(c28bin)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#Usando o dplyr é case_when( variável == 1 ~ 1,
#Variável ==2 ~ 0)


#presença de doença por ano de chegada 
#o Ano de chegada está disitribuido de algumas formas.

#anob7>>Todos os anos (numérico)
#ano_chegada_dic >>2010-2015,2016,2017,2018,2019,2020,2021
#ano_chegada_dic1 >>2010-2015,2016-2017,2018-2019,2020,2021
#ano_chegada_dic2>> 2010-2015,2016-2018,2019-2021

attributes(anob7)
#attributes(anob77)
#attributes(ano_chegada_dic)
#attributes(ano_chegada_dic1)
#attributes(ano_chegada_dic2)

#tabelas

table(anob7,c28bin)
#table(anob77,c28bin)
#table(ano_chegada_dic,c28bin)
#table(ano_chegada_dic1,c28bin)
#table(ano_chegada_dic2,c28bin)


#View(c28bin)

#chamando o modelo. 

table(anob7,c28)
m19=glm(c28bin~anob7,family=binomial(link=logit),data=dados)
summary(m19)
OR19=exp(coef(m19))
ICOR19=exp(confint(m19))
round((cbind(OR19, ICOR19)),3)


#presença de doença por status migratório.
table(situmigra,c28)
summary(c28)
m20=glm(c28bin~situmigra,family=binomial(link=logit),data=dados)
summary(m20)
OR20=exp(coef(m20))
ICOR20=exp(confint(m20))
round((cbind(OR20, ICOR20)),3)

#Por sexo
table(sexo,c28)
m21=glm(c28bin~sexo,family=binomial(link=logit))
summary(m21)
OR21=exp(coef(m21))
ICOR21=exp(confint(m21))
round((cbind(OR21, ICOR21)),21)


#Presença de Doença por idade - idade em faixas - idade _cat
table(idade_cat,c28)
m22=glm(c28bin~idade_cat,family=binomial(link=logit))
summary(m22)
OR22=exp(coef(m22))
ICOR22=exp(confint(m22))
round((cbind(OR22, ICOR22)),3)

#cor da pele
table(a2,c28)
m23=glm(c28bin~a2,family=binomial(link=logit))
summary(m23)
OR23=exp(coef(m23))
ICOR23=exp(confint(m23))
round((cbind(OR23, ICOR23)),3)

#Estado civil
table(a3,c28bin)
m24=glm(c28bin~a3,family=binomial(link=logit))
summary(m24)
OR24=exp(coef(m24))
ICOR24=exp(confint(m24))
round((cbind(OR24, ICOR24)),3)

#Se vive com mais alguém - compoisção domiciliar
table(a4,c28)
m25=glm(c28bin~a4,family=binomial(link=logit))
summary(m25)
OR25=exp(coef(m25))
ICOR25=exp(confint(m25))
round((cbind(OR25, ICOR25)),3)

#Apatridia
table(a5,c28)
m26=glm(c28bin~a5,family=binomial(link=logit))
summary(m26)
OR26=exp(coef(m26))
ICOR26=exp(confint(m26))
round((cbind(OR26, ICOR26)),3)

#Religião
table(a6,c28)
m27=glm(c28bin~a6,family=binomial(link=logit))
summary(m27)
OR27=exp(coef(m27))
ICOR27=exp(confint(m27))
round((cbind(OR27, ICOR27)),3)

#Filhos
table(a7,c28bin)
m29=glm(c28bin~a7,family=binomial(link=logit))
summary(m29)
OR29=exp(coef(m29))
ICOR29=exp(confint(m29))
round((cbind(OR29, ICOR29)),3)

#Escolaridade
table(a8,c28)
m30=glm(c28bin~a8,family=binomial(link=logit))
summary(m30)
OR30=exp(coef(m30))
ICOR30=exp(confint(m30))
round((cbind(OR30, ICOR30)),3)

#Falar outros idiomas
table(a9,c28)
m31=glm(c28bin~a9,family=binomial(link=logit))
summary(m31)
OR31=exp(coef(m31))
ICOR31=exp(confint(m31))
round((cbind(OR31, ICOR31)),3)

#Trab na Ven
table(a10,c28)
m32=glm(c28bin~a10,family=binomial(link=logit))
summary(m32)
OR32=exp(coef(m32))
ICOR32=exp(confint(m32))
round((cbind(OR32, ICOR32)),3)

#Trab no Br
table(a11,c28)
m33=glm(c28bin~a11,family=binomial(link=logit))
summary(m33)
OR33=exp(coef(m33))
ICOR33=exp(confint(m33))
round((cbind(OR33, ICOR33)),3)

#Renda Ven
table(a12,c28)
m34=glm(c28bin~a12,family=binomial(link=logit))
summary(m34)
OR34=exp(coef(m34))
ICOR34=exp(confint(m34))
round((cbind(OR34, ICOR34)),3)

#Renda Bra
table(a14,c28)
m35=glm(c28bin~a14,family=binomial(link=logit))
summary(m35)
OR35=exp(coef(m35))
ICOR35=exp(confint(m35))
round((cbind(OR35, ICOR35)),3)

#Dominio do portugues
table(a16,c28)
m36=glm(c28bin~a16,family=binomial(link=logit))
summary(m36)
OR36=exp(coef(m36))
ICOR36=exp(confint(m36))
round((cbind(OR36, ICOR36)),3)

#Sofreu discriminação
table(a17,c28)
m37=glm(c28bin~a17,family=binomial(link=logit))
summary(m37)
OR37=exp(coef(m37))
ICOR37=exp(confint(m37))
round((cbind(OR37, ICOR37)),3)

#Modelo ajustado = todas as variaveis em conjunto
my=glm(c28bin~ anob7+situmigra+sexo+idade_cat+ a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a14+a16+a17,family=binomial(link=logit))
summary(my)
ORy=exp(coef(my))
ICORy=exp(confint(my))
round((cbind(ORy, ICORy)),3)

#So as variaveis que apontaram pvalor sig
myy=glm(c28bin~ sexo+a9+a11,family=binomial(link=logit))
summary(myy)
ORyy=exp(coef(myy))
ICORyy=exp(confint(myy))
round((cbind(ORyy, ICORyy)),3)

#### Diagnosticos & Ajustes dos modelos ------

## install.packages('jtools')

# Carregando o pacote
library(jtools)
summ(logit_fruit, exp = T)

#Além de fornecer algumas medidas adicionais de qualidade de ajuste como Pseudo-R² e executar um teste chi quadrado, a função summ() retorna os valores estimados de cada parâmetro na escala de probabilidades, associado aos respectivos intervalos de confiança, valores de z e p. https://www.rpubs.com/dudubiologico/545528



#outra maneira de ver os resultados >> coeficientes
#install.packages("ResourceSelection")
library(stargazer)
stargazer(m1, title="Resultados",type = "text")
stargazer(m2, title="Resultados",type = "text")
stargazer(m3, title="Resultados",type = "text")
stargazer(m4, title="Resultados",type = "text")
stargazer(m5, title="Resultados",type = "text")
stargazer(m6, title="Resultados",type = "text")
stargazer(m7, title="Resultados",type = "text")
stargazer(m8, title="Resultados",type = "text")
stargazer(m9, title="Resultados",type = "text")
stargazer(m10, title="Resultados",type = "text")
stargazer(m11, title="Resultados",type = "text")
stargazer(m12, title="Resultados",type = "text")
stargazer(m13, title="Resultados",type = "text")
stargazer(m14, title="Resultados",type = "text")
stargazer(m15, title="Resultados",type = "text")
stargazer(m16, title="Resultados",type = "text")
stargazer(m17, title="Resultados",type = "text")
stargazer(m18, title="Resultados",type = "text")
#Diagnostico do Modelo.

#Pseudo R² 
#Semelhante ao coeficiente de determinação R2 da regressão múltipla, a medida de pseudo R2 representam o ajuste geral do modelo proposto. Sua interpretação, portanto, é semelhante à regressão múltipla

#calculates those of McFadden (1974), Cox & Snell (1989), Nagelkerke (1991), Tjur (2009), and the squared Pearson correlation between observed and predicted values >>https://modtools.wordpress.com/2014/10/30/rsqglm/

#R2 for Logistic Regression. 

#In logistic regression, there is no true R2 value as there is in OLS regression. However, because deviance can be thought of as a measure of how poorly the model fits (i.e., lack of fit   between observed and predicted values), an analogy can be made to sum of squares residual in ordinary least squares. The proportion of unaccounted for variance that is reduced by adding variables to the model is the same as the proportion of variance accounted for, or R2

#Where the null model is the logistic model with just the constant and the k model contains all thepredictors in the model.

#There are a number of pseudo-R2 values that have been proposed using this general logic, including the Cox and Snell (Cox & Snell, 1989; Cragg & Uhler, 1970; Maddala,1983), Nagelkerke (1991), McFadden Newsom Psy 525/625 Categorical Data Analysis, Spring 2021 6 (1974), and Tjur (2009) indexes, among others (see Allison, 2014, for a review) FONTE>> http://web.pdx.edu/~newsomj/cdaclass/ho_logistic.pdf


library(modEvA)
RsqGLM(m1)
RsqGLM(m2)
RsqGLM(m3)
RsqGLM(m4)
RsqGLM(m5)
RsqGLM(m6)
RsqGLM(m7)
RsqGLM(m8)
RsqGLM(m9)
RsqGLM(m10)
RsqGLM(m11)
RsqGLM(m12)
RsqGLM(m13)
RsqGLM(m14)
RsqGLM(m15)
RsqGLM(m16)
RsqGLM(m17)
RsqGLM(m18)
RsqGLM(m19)
RsqGLM(m20)
RsqGLM(m21)
RsqGLM(m22)
RsqGLM(m23)
RsqGLM(m24)
RsqGLM(m25)
RsqGLM(m26)
RsqGLM(m27)
RsqGLM(m28)
RsqGLM(m22)
RsqGLM(m29)
RsqGLM(m30)
RsqGLM(m31)
RsqGLM(m32)
RsqGLM(m33)
RsqGLM(m34)
RsqGLM(m35)
RsqGLM(m36)
RsqGLM(m37)

t=rbind(RsqGLM(m1),
        RsqGLM(m2),
        RsqGLM(m3),
        RsqGLM(m4),
        RsqGLM(m5),
        RsqGLM(m6),
        RsqGLM(m7),
        RsqGLM(m8),
        RsqGLM(m9),
        RsqGLM(m10),
        RsqGLM(m11),
        RsqGLM(m12),
        RsqGLM(m13),
        RsqGLM(m14),
        RsqGLM(m15),
        RsqGLM(m16),
        RsqGLM(m17),
        RsqGLM(m18),
        RsqGLM(m20),
        RsqGLM(m21),
        RsqGLM(m22),
        RsqGLM(m23),
        RsqGLM(m24),
        RsqGLM(m25),
        RsqGLM(m26),
        RsqGLM(m27),
        RsqGLM(m28),
        RsqGLM(m22),
        RsqGLM(m29),
        RsqGLM(m30),
        RsqGLM(m31),
        RsqGLM(m32),
        RsqGLM(m33),
        RsqGLM(m34),
        RsqGLM(m35),
        RsqGLM(m36),
        RsqGLM(m37))

DT::datatable(t)

#At this point, there does not seem to be much agreement on which R-square approach is best (https://statisticalhorizons.com/r2logistic for a brief discussion and references), and researchers do not seem to report any one of them as often as they should. My recommendation for any that you choose to use, do not use them as definitive or exact values for the percentage of variance accounted for and to make some reference to the "approximate percentage of variance accounted for". 

#Logistic Regression, Chi-squared, and Loglinear Models Compared 

#As you might have wondered by now, the simple logistic regression model with a binary independent variable could be used to analyze a two-way contingency table. And, in fact, for that special case, thelikelihood ratio test from the contingency table analysis and the logistic regression are the same. Though the loglinear model does not distinguish between explanatory and response variables-all are essentially treated as response variables-the simple logistic and the likelihood ratio test from the loglinear model in the 2 × 2 case will equal the likelihood ratio test from the logistic regression. So, in the simple case, these analyses converge. With more complex analyses, it becomes more difficult to always see the connection. The three-way contingency table analysis also relates to the logistic regression model. A logistic model that tests the same hypothesis as tests from the loglinear and three-way contingency tests can b constructed if we consider a logistic model with more than one predictor (e.g., X and Z predicting Y). 

### Satisfação com a saúde como desfecho (DESCONSIDERAR)------
 
#desfecho >> Satisfação com a saúde. VARIAVEL DEPENDENTE. DESCONSIDERAR

#Será feita uma regressão Logistica Ordinal

## DICOTOMIZANDO Satisfação com saúde.


satisf_saude_dic= fct_collapse(c2,             `Insatisfeito` = c('Muito insatisfeito','Insatisfeito'),`Nem satisfeito, nem insatisfeito` = c('Nem satisfeito, nem insatisfeito'),             `Satisfeito ou muito satisfeito` = c('Satisfeito','Muito satisfeito'))

class(satisf_saude_dic) # já está ordenada, foi ordenada ainda na C2...
table(satisf_saude_dic)

#if(!require(pacman)) install.packages("pacman")
#install.packages("MASS", "lmtest","gtsummary", "reshape2")
                 
library(dplyr)
#install.packages("lme4", type = "binary")
#install.packages("car")
library(car)
library(MASS)
library(lmtest)
library(reshape2)
library(ggplot2)
#install.packages("gtsummary")
library('gtsummary')


# Passo 4: Checagem dos pressupostos

## 1. Variável dependente ordinal
## 2. Independência das observações (sem medidas repetidas)


## 3. Ausência de multicolinearidade
m <- lm(as.numeric(satisf_saude_dic) ~ anob7+situmigra+sexo+idade_cat+ a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a14+a16+a17, dados)
vif(m)
### Multicolinearidade: VIF > 10
#Se multicolinearidade for grave, você pode não ser capaz de determinar qual dos preditores deve ser incluído no modelo. Fonte: https://support.minitab.com/pt-br/minitab/18/help-and-how-to/modeling-statistics/regression/how-to/ordinal-logistic-regression/before-you-start/data-considerations/ - Aqui anob7, situmigra,religião (a6), escolaridade (a8), renda na ven (a12), renda no Brasil (a14) e dominio do portugues (a16) apresentam niveis altissimos, maiores ou bem maiores que 10 - modelo cheio. 

m1 <- lm(as.numeric(satisf_saude_dic) ~ anob7+situmigra+sexo+idade_cat, dados)
vif(m1)

#todos perto de 1

m2 <- lm(as.numeric(satisf_saude_dic) ~ + a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a14+a16+a17, dados)
vif(m2)
#religiao, escolaridade, renda na ven e renda no brasil influenciam. Deixar renda na ven de fora...

m3 <- lm(as.numeric(satisf_saude_dic) ~ + a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a14+a16+a17, dados)
vif(m3)
#ja está melhor, embora religiao e escolaridade permaneçãm altos...

m4 <- lm(as.numeric(satisf_saude_dic) ~ + a2+a3+a4+a5+a7+a8+a9+a10+a11+a14+a16+a17, dados)
vif(m4)
#Tirando religião, ainda melhor


## Construção do modelo ordinal:

m <- polr(satisf_saude_dic ~ anob7+situmigra+sexo+idade_cat+ a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a14+a16+a17, data = dados, Hess = T)#modelo cheio


mod1 <- polr(satisf_saude_dic ~ anob7+situmigra+sexo+idade_cat,data = dados, Hess = T)# modelo so com checklist

mod2 <- polr(satisf_saude_dic ~ anob7+a2+a3+a4+a5+a7+a8+a9+a10+a11+a14+a16+a17, data = dados, Hess = T)#modelo com ano de chegada e bloco a - excluindo a6-religiao e a12-renda na ven

## 4. Proportional odds (parallel lines)

poTest(mod1)
poTest(mod2)
poTest(m)

# Passo 5: Análise do modelo

## Overall effects
car::Anova(m, type = "II", test = "Chisq")
car::Anova(mod1, type = "II", test = "Wald")
car::Anova(mod2, type = "II", test = "Wald")

## Efeitos específicos
summary(m)
summary(mod1)
summary(mod)

ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- as.data.frame(round(cbind(ctable, "p value" = p), 4))
ctable


## Ou:
lmtest::coeftest(m)


## Obtenção das razões de chance com IC 95% (usando log-likelihood)

exp(cbind(OR = coef(m), confint(m)))



## Obtenção de uma tabela-resumo completa

gtsummary::tbl_regression(m, exponentiate = F,
                          estimate_fun = purrr::partial(style_ratio, digits = 3)) %>% 
  gtsummary::add_global_p()


####### Como modificar as categorias de referência? ########

levels(dados$Genero)

dados$Genero <- relevel(dados$Genero, ref = "M")


### ATENÇÃO: é necessário rodar o modelo novamente!




 ----

#Vamos testar as correlações- Vamos utilizar o de pearson - que é o default da função. https://www.rdocumentation.org/packages/PerformanceAnalytics/versions/2.0.4/topics/chart.Correlation

#precisamos recodificar as variaveis:

#Vamos, lá!

#organizando a variavel C27= presença de doenças.
#no banco, ano está codificado em 1 e 2, sendo 1= Sim, 2=Não.
#O mesmo acontece pra todas as variaveis binárias. Leia-se c28,c29,c30,c31,c32...


#Como factor, sim ou não
#c27=factor(c27_es,levels=c(1:2),labels=c("Sim","Não"))

#Para a descrição das frequências absolutas e relativas ela foi transformada em factor (sim,não), porém aqui ela deverá ser transformada em 0 ou 1 onde 0=não ocorrencia de doenças e 1= onde há ocorrência de doenças.
#O mesmo acontece pra todas as variaveis binárias. Leia-se c28,c29,c30,c31,c32...

#então vamos recodificar pra avaliar...


summary(c27_es)
c27bin <- ifelse(c27_es == 1, 1,0)

DT::datatable(tabyl(c27bin)%>%
                adorn_totals("row") %>%
                adorn_pct_formatting(digits = 2))

#Usando o dplyr é case_when( variável == 1 ~ 1,
#Variável ==2 ~ 0)


#Vamos olhar a correlação entre a presença de doenças e as variaveis sociodemograficas 

library("PerformanceAnalytics")
chart.Correlation(cbind(ano_chegada_dic,c27bin), histogram=TRUE)
chart.Correlation(cbind(situmigra_faixa2,c27bin), histogram=TRUE)
chart.Correlation(cbind(sexo,c27bin), histogram=TRUE)#diminui pra homens? Deve ser pelo N...
chart.Correlation(cbind(idade_cat,c27bin), histogram=TRUE)# A chance(?) de doença aumenta com idade?
chart.Correlation(cbind(idade_es,c27bin), histogram=TRUE)# A chance(?) de doença aumenta com idade?
chart.Correlation(cbind(escolaridade_dic,c27bin), histogram=TRUE)# 
chart.Correlation(cbind(a3,c27bin), histogram=TRUE)# 
chart.Correlation(cbind(a5,c27bin), histogram=TRUE)# O risco de doença diminui nos não apatridas (???)
chart.Correlation(cbind(religiao_dic,c27bin), histogram=TRUE)# 
chart.Correlation(cbind(a11_dicot,c27bin), histogram=TRUE)# 
chart.Correlation(cbind(a14_dicot,c27bin), histogram=TRUE)# 
chart.Correlation(cbind(a16,c27bin), histogram=TRUE)# A chance (?) de doença diminui com quem tem maior dominio da lingua...
chart.Correlation(cbind(a117,c27bin), histogram=TRUE)#

#Vamos olhar a correlação entre utilização de serviços e as variaveis sociodemograficas  

library("PerformanceAnalytics")
chart.Correlation(cbind(ano_chegada_dic,c28bin), histogram=TRUE)
chart.Correlation(cbind(situmigra_faixa2,c28bin), histogram=TRUE)
chart.Correlation(cbind(sexo,c28bin), histogram=TRUE)#diminui pra homens? Deve ser pelo N...
chart.Correlation(cbind(idade_cat,c28bin), histogram=TRUE)# A chance(?) de doença aumenta com idade?
chart.Correlation(cbind(idade_es,c28bin), histogram=TRUE)# A chance(?) de doença aumenta com idade?
chart.Correlation(cbind(escolaridade_dic,c28bin), histogram=TRUE)# 
chart.Correlation(cbind(a3,c28bin), histogram=TRUE)# 
chart.Correlation(cbind(a5,c28bin), histogram=TRUE)# O risco de doença diminui nos não apatridas (???)
chart.Correlation(cbind(religiao_dic,c28bin), histogram=TRUE)# 
chart.Correlation(cbind(a11_dicot,c28bin), histogram=TRUE)# 
chart.Correlation(cbind(a14_dicot,c28bin), histogram=TRUE)# 
chart.Correlation(cbind(a16,c28bin), histogram=TRUE)# A chance (?) de doença diminui com quem tem maior dominio da lingua...
chart.Correlation(cbind(a117,c28bin), histogram=TRUE)#

#Vamos olhar a correlação entre a satisfação com a saúde e as variaveis sociodemograficas 

library("PerformanceAnalytics")
chart.Correlation(cbind(ano_chegada_dic,c2_es), histogram=TRUE)
chart.Correlation(cbind(situmigra_faixa2,c2_es), histogram=TRUE)
chart.Correlation(cbind(sexo,c2_es), histogram=TRUE)#diminui pra homens? Deve ser pelo N...
chart.Correlation(cbind(idade_cat,c2_es), histogram=TRUE)# A chance(?) de doença aumenta com idade?
chart.Correlation(cbind(idade_es,c2_es), histogram=TRUE)# A chance(?) de doença aumenta com idade?
chart.Correlation(cbind(escolaridade_dic,c2_es), histogram=TRUE)# 
chart.Correlation(cbind(a3,c2_es), histogram=TRUE)# 
chart.Correlation(cbind(a5,c2_es), histogram=TRUE)# O risco de doença diminui nos não apatridas (???)
chart.Correlation(cbind(religiao_dic,c2_es), histogram=TRUE)# 
chart.Correlation(cbind(a11_dicot,c2_es), histogram=TRUE)# 
chart.Correlation(cbind(a14_dicot,c2_es), histogram=TRUE)# 
chart.Correlation(cbind(a16,c2_es), histogram=TRUE)# A chance (?) de doença diminui com quem tem maior dominio da lingua...
chart.Correlation(cbind(a117,c2_es), histogram=TRUE)#



chart.Correlation(cbind(a17bin,c27bin), histogram=TRUE)# 
cor.test(a17bin,c27bin)
#Bonus >> não ha correlação entre quem sofreu preconceito e a chance(?) de ter doença.

chart.Correlation(cbind(a17bin,c29bin), histogram=TRUE)# 
cor.test(a17bin,c29bin)
#Bonus >> Correlação leve, mas não estatisticamente significante entre quem sofreu preconceito e quem faz tratamente psicologico

chart.Correlation(cbind(escolaridade_dic,c29bin), histogram=TRUE) 
cor.test(a6_es,c29bin)

chart.Correlation(cbind(escolaridade_dic,c28bin), histogram=TRUE) 
cor.test(a6_es,c28bin)

chart.Correlation(cbind(a16,c29bin), histogram=TRUE)# 
cor.test(a16_es,c29bin)
#Bonus 3 >> Uma leve relação não estatisticamente significante entre escolaridade e uso de tratamento psicologico e não ha com quem utiliza serviços de saúde. E uma relação significante do dominio da lingua com a utilização de serviço de atenção psicologica

chart.Correlation(cbind(a16,c28bin), histogram=TRUE)# 
cor.test(a16_es,c28bin)

a9bin <- ifelse(a9_es == 1, 1,0)

chart.Correlation(cbind(a9bin,c28bin), histogram=TRUE)# 
cor.test(a9bin,c28bin)
#Bonus 4 >> Não há relação entre escolaridade ou dominio da lingua portuguesa e utilização de serviços de saúde, mas ha com o dominio de outras linguas

chart.Correlation(cbind(b1_es,a17bin), histogram=TRUE)# 
cor.test(b1_es,a17bin)#Ha uma correlação - ou chance - de que quem não veio direto da venezuela ter sofrido menos praconceito

chart.Correlation(cbind(b6,a17bin), histogram=TRUE)
cor.test(b6_es,a17bin)#Correlação entre quem entrou por RR e sofreu preconceito - certeza que não é estatisticamente significante (bingo)

chart.Correlation(cbind(anob7,a17bin), histogram=TRUE)# 
cor.test(anob7,a17bin)#Ha uma correlação negativa entre preconceito e ano de chegada - ou ao fluxo. 


#
chart.Correlation(cbind(a8,a10_dicot), histogram=TRUE)# 
cor.test(a8_es,a10_es)

chart.Correlation(cbind(a8,a11_dicot), histogram=TRUE)# 
cor.test(a8_es,a11_es)#Ha uma correlação entre escolaridade e trabalho na venezuela, mas o mesmo não se mantem aqui no Brasil

chart.Correlation(cbind(a10_dicot,a11_dicot), histogram=TRUE)
cor.test(a10_es,a11_es)#Ha uma correlação entre desemprego na venezuela e no Brasil

chart.Correlation(cbind(idade_es,escolaridade_dic), histogram=TRUE)# 
cor.test(idade_es,a8_es)#Ha uma correlação entre idade e escolaridade (nossa, quem diria?)

b3bin <- ifelse(b3_es == 1, 1,0)

chart.Correlation(cbind(b3bin,c29bin), histogram=TRUE)# 
cor.test(b3bin,c29bin)#Ha uma correlação (negativa) entre vir acompanhado e fazer atendimento psicologico



### Anotações & Rascunhos ------

#Como modificar as categorias de referência?
levels(dados$Hab_Fumar)

dados$Hab_Fumar <- relevel(dados$Hab_Fumar, ref = "Sim")


### ATENÇÃO: é necessário rodar o modelo novamente!


levels(dados$Cancer)

dados$Cancer <- relevel(dados$Cancer, ref = "Sim")

#Diagnostico do Modelo.

par(mfrow=c(1,2))
parmfrow
residuosp=residuals(m1, type="pearson") 
qqnorm(residuosp, title="pearson")
qqline(residuosp, col="red") 


residuosd=residuals(m1, type="deviance") 
qqnorm(residuosd) 
qqline(residuosd, col="red")


modelo

modelo=glm(variavel dependente ~ variaveis independentes,family="poisson"(link="log"))
exp(modelo$coefficients)
summary(modelo)

# Para os intervalos de confiança use a função confint:

exp(confint(modelo)) # 95% CI for exponentiated coefficients


#outra maneira (mais artesanal)
>>>>>>  # Exemplo de utilização do epi.2by2 para calculo da razão de
  >>>>>>>  # prevalencia
  >>>>>>>
  >>>>>>>  library(epiR)
>>>>>>>
  >>>>>>>  dat <- matrix(c(13,2163,5,3349), nrow = 2, byrow = TRUE)
>>>>>>>  rownames(dat) <- c("DF+", "DF-")
>>>>>>>  colnames(dat) <- c("FUS+", "FUS-")
>>>>>>>  dat
>>>>>>>  epi.2by2(dat = as.table(dat), method = "cross.sectional",
                  >>>>>>>  conf.level = 0.95, units = 100,  homogeneity = "breslow.day",
                  >>>>>>>  outcome = "as.columns")

situmigra=factor(situmigra_es, levels = c(1:7), labels = c("Solicitante de refúgio",  "Refugiado",  "Solicitante de autorização de residencia",  "Permissão de residencia por plazo determinado",  "Permissão de residencia por plazo indeterminado",  "Sem documentos",  "Outro"))




#observações >> salvar essas novas obs numa fortmato mais amigavel (sim ou não e não numeros?????)


#vamos fazer um data frame com essas outras variaveis? 
#dados=cbind(faixa_etaria_ibge,faixa_etaria_pea,tempo_no_brasil,graus_escolaridade)
#dados
#as.data.frame(dados)

#dai a gente junta e faz uma planilha nova
#planilha_nova=cbind(planilha_caritas,dados)
#########Criando o banco de dados pro joão.-------


#Deixando apenas as colunas de interesse do João
View(dados)
dados_joao_covid <- dados[,c(126,127)]
View(dados_joao_covid)
#Salvar esse banco de dados... 
#salva O em csv, com o nome de planilha_nova_caritas
write.csv2(dados_joao_covid, file = "dados_joao_covid.csv") #salva em csv

install.packages("writexl")
library(writexl)

install.packages("xlsx")
library("xlsx")

writexl(dados_joao_covid, file = "dados_joao_covid.xlsx")#salva em xlsx - library(writexl) >> (https://livro.curso-r.com/5-3-readxl.html)
