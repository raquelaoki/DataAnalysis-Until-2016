##------------------------------- TCC ----------------------------------
##					Raquel Aoki
##				script com preparação do banco
##----------------------------------------------------------------------


rm(list=ls(all=TRUE))

#--------------------------- Bases de dados iniciais 
rsg = read.table( "banco.rsg.V2.csv", header = T, sep = ";", dec = ",")
bd = read.table( "bd.alunos.disciplinas.csv", header = T, sep = ";", dec = ",")

disc = as.character(data.frame(table(bd$DISCIPLINA ))$Var1)

id2 = paste(bd$identificador,sep="_",bd$PERIODO)
id3 = paste(rsg$identificador, sep = "_", rsg$PERIODO)

bd = cbind(bd, id2)
rsg = cbind(rsg, id3)

#--------------------------- Acrescentando o RSG do periodo nas bases de dados
#------------------ selecionando a disciplina Calculo Diferencial e Integral I
bd_18 = subset(bd, DISCIPLINA == disc[18]) 
dim(bd_18)
#------------------ tirando os trancamentos 
bd_18 = subset(bd_18, !is.na(FREQUENCIA))

rsg$id3 = as.character(rsg$id3)
alunos = as.character(bd_18$id2)

# rsg somente dos alunos que fizeram calculo 
rsg_18 = read.table( "rsg_18.csv", header = T, sep = ";", dec = ",")
bd_18$id2 = as.character(bd_18$id2)
rsg_18$id3=as.character(rsg_18$id3)

banco_18 = merge(bd_18, rsg_18, by.x = "id2", by.y = "id3",all.x = T)
dim(banco_18); dim(rsg_18); dim(bd_18)

#--------------- Organizando o banco
CONCEITO = rep("xx", dim(banco_18)[1])
i = 1

while (i <= dim(banco_18)[1]){
	if(banco_18$RENDIMENTO[i]<=39){ CONCEITO[i] = "F"}
	if(banco_18$RENDIMENTO[i]<=59 && banco_18$RENDIMENTO[i] >= 40 ){ CONCEITO[i] = "E"}	
	if(banco_18$RENDIMENTO[i]<=69 && banco_18$RENDIMENTO[i] >= 60 ){ CONCEITO[i] = "D"}
	if(banco_18$RENDIMENTO[i]<=79 && banco_18$RENDIMENTO[i] >= 70 ){ CONCEITO[i] = "C"}
	if(banco_18$RENDIMENTO[i]<=89 && banco_18$RENDIMENTO[i] >= 80 ){ CONCEITO[i] = "B"}
	if(banco_18$RENDIMENTO[i]<=180 && banco_18$RENDIMENTO[i] >= 90 ){ CONCEITO[i] = "A"}
	i = i + 1}

APROVACAO = rep(2, length(CONCEITO))
APROVACAO[CONCEITO =="A"] = 1
APROVACAO[CONCEITO =="B"] = 1
APROVACAO[CONCEITO =="C"] = 1
APROVACAO[CONCEITO =="D"] = 1
APROVACAO[CONCEITO =="E"] = 0
APROVACAO[CONCEITO =="F"] = 0

table(APROVACAO);table(CONCEITO)
length(CONCEITO);length(APROVACAO)
banco_18 = cbind(banco_18, CONCEITO, APROVACAO)

#---------------  considerando somente a primeira vez que a disciplina foi feita
head(banco_18)
banco_18 = banco_18[order(banco_18$identificador.x, banco_18$PERIODO.x), ]
aux.duplicated = paste(banco_18$identificador.x, banco_18$DISCIPLINA, sep = "_")
head(aux.duplicated)
banco_18 = cbind(banco_18, aux.duplicated)
banco_18_nd = subset(banco_18, !duplicated(aux.duplicated))
dim(banco_18_nd);dim(banco_18)

INSTITUICAO = rep("XX", dim(banco_18_nd)[1]) 
banco_18_nd = cbind(banco_18_nd, INSTITUICAO)
banco_18_nd$INSTITUICAO = as.character(banco_18_nd$INSTITUICAO) 
banco_18_nd$APROVACAO = as.numeric(as.character(banco_18_nd$APROVACAO))
banco_18_nd$CURSO = as.character(banco_18_nd$CURSO)

banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10001PD001-ENGENHARIA DE CONTROLE E AUTOMACAO"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10001PN001-ENGENHARIA DE CONTROLE E AUTOMACAO"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10003PD001-ENGENHARIA AEROESPACIAL"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10004PD001-ENGENHARIA AMBIENTAL"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10005PD001-ENGENHARIA CIVIL"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10006PD001-ENGENHARIA DE MINAS"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10007PD001-ENGENHARIA DE PRODUCAO"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10008PN001-ENGENHARIA DE SISTEMAS"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10009PD001-ENGENHARIA ELETRICA"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10010PD001-ENGENHARIA MECANICA"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10010PN002-ENGENHARIA MECANICA"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10011PD001-ENGENHARIA METALURGICA"] = "ENGENHARIA"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="10012PD001-ENGENHARIA QUIMICA"] = "ENGENHARIA"

banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02001PD001-CIENCIA DA COMPUTACAO"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02002PD001-CIENCIAS ATUARIAIS"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02003PD001-ESTATISTICA"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02004PN002-FISICA"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02004PD001-FISICA"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02005PD001-MATEMATICA"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02005PN002-MATEMATICA"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02006PD001-MATEMATICA COMPUTACIONAL"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02009PD001-SISTEMAS DE INFORMACAO"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02007PD001-QUIMICA"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02007PN002-QUIMICA"] = "ICEX"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="02008PN001-QUIMICA TECNOLOGICA"] = "ICEX"

banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="12003PD001-CIENCIAS ECONOMICAS"] = "OUTROS"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="12004PD001-CONTROLADORIA E FINANCAS"] = "OUTROS"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="03001PD001-CIENCIAS BIOLOGICAS"] = "OUTROS"
banco_18_nd$INSTITUICAO[banco_18_nd$CURSO=="04002PD001-GEOLOGIA"] = "OUTROS"

banco_18_nd$CURSO = as.character(banco_18_nd$CURSO)
banco_18_nd$CURSO[banco_18_nd$CURSO == "02001PD001-CIENCIA DA COMPUTACAO"]= "CIENCIA DA COMPUTACAO"             
banco_18_nd$CURSO[banco_18_nd$CURSO == "02002PD001-CIENCIAS ATUARIAIS"]= "CIENCIAS ATUARIAIS"                             
banco_18_nd$CURSO[banco_18_nd$CURSO == "02003PD001-ESTATISTICA"]= "ESTATISTICA"                                    
banco_18_nd$CURSO[banco_18_nd$CURSO == "02004PD001-FISICA"]= "FISICA DIURNO"                                         
banco_18_nd$CURSO[banco_18_nd$CURSO == "02004PN002-FISICA"]= "FISICA NOTURNO"                                         
banco_18_nd$CURSO[banco_18_nd$CURSO == "02005PD001-MATEMATICA"]= "MATEMATICA DIURNO"                                     
banco_18_nd$CURSO[banco_18_nd$CURSO == "02005PN002-MATEMATICA"]= "MATEMATICA NOTURNO"                                     
banco_18_nd$CURSO[banco_18_nd$CURSO == "02006PD001-MATEMATICA COMPUTACIONAL"]= "MATEMATICA COMPUTACIONAL"                       
banco_18_nd$CURSO[banco_18_nd$CURSO == "02007PD001-QUIMICA"]= "QUIMICA DIURNO"                                        
banco_18_nd$CURSO[banco_18_nd$CURSO == "02007PN002-QUIMICA"]= "QUIMICA NOTURNO"                                        
banco_18_nd$CURSO[banco_18_nd$CURSO == "02008PN001-QUIMICA TECNOLOGICA"]= "QUIMICA TECNOLOGICA"                            
banco_18_nd$CURSO[banco_18_nd$CURSO == "02009PD001-SISTEMAS DE INFORMACAO"]= "SISTEMAS DE INFORMACAO"                         
banco_18_nd$CURSO[banco_18_nd$CURSO == "03001PD001-CIENCIAS BIOLOGICAS"]= "CIENCIAS BIOLOGICAS"                            
banco_18_nd$CURSO[banco_18_nd$CURSO == "04002PD001-GEOLOGIA"]= "GEOLOGIA"                                       
banco_18_nd$CURSO[banco_18_nd$CURSO == "10001PD001-ENGENHARIA DE CONTROLE E AUTOMACAO"]= "ENGENHARIA DE CONTROLE E AUTOMACAO DIURNO"             
banco_18_nd$CURSO[banco_18_nd$CURSO == "10001PN001-ENGENHARIA DE CONTROLE E AUTOMACAO"]= "ENGENHARIA DE CONTROLE E AUTOMACAO NOTURNO"             
banco_18_nd$CURSO[banco_18_nd$CURSO == "10003PD001-ENGENHARIA AEROESPACIAL"]= "ENGENHARIA AEROESPACIAL"                        
banco_18_nd$CURSO[banco_18_nd$CURSO == "10004PD001-ENGENHARIA AMBIENTAL"]= "ENGENHARIA AMBIENTAL"                           
banco_18_nd$CURSO[banco_18_nd$CURSO == "10005PD001-ENGENHARIA CIVIL"]= "ENGENHARIA CIVIL"                               
banco_18_nd$CURSO[banco_18_nd$CURSO == "10006PD001-ENGENHARIA DE MINAS"]= "ENGENHARIA DE MINAS"                            
banco_18_nd$CURSO[banco_18_nd$CURSO == "10007PD001-ENGENHARIA DE PRODUCAO"]= "ENGENHARIA DE PRODUCAO"                         
banco_18_nd$CURSO[banco_18_nd$CURSO == "10008PN001-ENGENHARIA DE SISTEMAS"]= "ENGENHARIA DE SISTEMAS"                         
banco_18_nd$CURSO[banco_18_nd$CURSO == "10009PD001-ENGENHARIA ELETRICA"]= "ENGENHARIA ELETRICA"                            
banco_18_nd$CURSO[banco_18_nd$CURSO == "10010PD001-ENGENHARIA MECANICA"]= "ENGENHARIA MECANICA DIURNO"                            
banco_18_nd$CURSO[banco_18_nd$CURSO == "10010PN002-ENGENHARIA MECANICA"]= "ENGENHARIA MECANICA NOTURNO"                            
banco_18_nd$CURSO[banco_18_nd$CURSO == "10011PD001-ENGENHARIA METALURGICA"]= "ENGENHARIA METALURGICA"                         
banco_18_nd$CURSO[banco_18_nd$CURSO == "10012PD001-ENGENHARIA QUIMICA"]= "ENGENHARIA QUIMICA"                             
banco_18_nd$CURSO[banco_18_nd$CURSO == "12003PD001-CIENCIAS ECONOMICAS"]= "CIENCIAS ECONOMICAS"                            
banco_18_nd$CURSO[banco_18_nd$CURSO == "12004PD001-CONTROLADORIA E FINANCAS"]= "CONTROLADORIA E FINANCAS"    

tab.aux1 = data.frame(table(as.character(banco_18_nd$CURSO)))   
eliminar = tab.aux1$Var1[tab.aux1$Freq <= 100]
banco_18_nd = subset(banco_18_nd, CURSO != as.character(eliminar)) 

bd = banco_18_nd[,c(26,10,28,23,21)]
names(bd) = c("ap", "curso", "inst", "rsg","ano")
bd$ap = as.factor(bd$ap)
bd$inst = as.factor(bd$inst)
bd$curso = as.factor(bd$curso)
bd$inst  = relevel(bd$inst, ref = "ICEX")

#ano1 = bd$ANO
#bd = cbind(bd, ano1) 
#bd$ano1[bd$ano1==1993 | bd$ano1 == 1994|bd$ano1==1995 |
#	 bd$ano1 == 1996| bd$ano1 == 1997]= 1
#bd$ano1[bd$ano1==1998 | bd$ano1 == 1999|bd$ano1==2000 |
#	 bd$ano1 == 2001| bd$ano1 == 2002]= 2
#bd$ano1[bd$ano1==2003| bd$ano1 == 2004|bd$ano1==2005|
#	 bd$ano1 == 2006| bd$ano1 == 2007]= 3
#bd$ano1[bd$ano1==2008| bd$ano1 == 2009|bd$ano1==2010|
#	 bd$ano1 == 2011| bd$ano1 == 2012| bd$ano1 == 2013]= 4
#bd$ano1 = as.factor(bd$ano1) 

#RSG2 = bd$RSG*bd$RSG
#RSG3 = bd$RSG*bd$RSG*bd$RSG

#ano2 = bd$ANO
#bd = cbind(bd, ano2) 
#bd$ano2[bd$ano2 == 1993 | bd$ano2 == 1994|bd$ano2==1995 |
#	 bd$ano2 == 1996]= 1
#bd$ano2[bd$ano2 == 1997|bd$ano2 == 1998 | bd$ano2 == 1999|
#	bd$ano2==2000 ]= 2
#bd$ano2[bd$ano2==2001| bd$ano2 == 2002|bd$ano2==2003|
#	 bd$ano2 == 2004]= 3
#bd$ano2[bd$ano2==2005| bd$ano2 == 2006|bd$ano2==2007|
#	 bd$ano2 == 2008]= 4
#bd$ano2[bd$ano2==2009| bd$ano2 == 2010|bd$ano2==2011|
#	 bd$ano2 == 2012| bd$ano2 == 2013]=5
#bd$ano2 = as.factor(bd$ano2) 

rsg2 = bd$rsg^2
bd= cbind(bd, rsg2) 
               

#---- Trocando os levels dos cursos

icex <- subset(bd, inst =="ICEX")
icex$curso <- as.factor(as.character(icex$curso))
icex$curso <- relevel(icex$curso, ref="MATEMATICA DIURNO")
icex.cursos = levels(icex$curso)
icex$curso <- as.factor(as.numeric(icex$curso))
codificacao.icex = data.frame(rep("ICEX", length(icex.cursos)),
	icex.cursos,names(table(icex$curso))) 

eng <- subset(bd, inst=="ENGENHARIA")
eng$curso <- as.factor(as.character(eng$curso))
eng.cursos= levels(eng$curso)
eng$curso <- as.factor(as.numeric(eng$curso))
codificacao.eng= data.frame(rep("ENGENHARIA", length(eng.cursos)),
	eng.cursos, names(table(eng$curso))) 

outros <- subset(bd, inst=="OUTROS")
outros$curso <- as.factor(as.character(outros$curso))
outros.cursos= levels(outros$curso)
outros$curso <- as.factor(as.numeric(outros$curso))
codificacao.outros= data.frame(rep("OUTROS", length(outros.cursos)),
	outros.cursos, names(table(outros$curso))) 
names(codificacao.icex)= names(codificacao.eng) = c("Instituicao", "Curso","Numero")
names(codificacao.outros) = c("Instituicao", "Curso","Numero")

codificacao = rbind(codificacao.outros,codificacao.eng,codificacao.icex)
letra = codificacao$Numero

dados <- rbind(icex,eng,outros)
dados$curso <- as.character(dados$curso)
table(dados$curso  )
dados$curso  = gsub("10", "J", dados$curso);letra = gsub("10", "J", letra)
dados$curso  = gsub("11", "K", dados$curso);letra = gsub("11", "K", letra)
dados$curso  = gsub("12", "L", dados$curso);letra = gsub("12", "L", letra)
dados$curso  = gsub("13", "M", dados$curso);letra = gsub("13", "M", letra)
dados$curso  = gsub("2", "B", dados$curso); letra = gsub("2", "B", letra)
dados$curso  = gsub("3", "C", dados$curso); letra = gsub("3", "C", letra)
dados$curso  = gsub("4", "D", dados$curso); letra = gsub("4", "D", letra)
dados$curso  = gsub("5", "E", dados$curso); letra = gsub("5", "E", letra)
dados$curso  = gsub("6", "F", dados$curso); letra = gsub("6", "F", letra)
dados$curso  = gsub("7", "G", dados$curso); letra = gsub("7", "G", letra)
dados$curso  = gsub("8", "H", dados$curso); letra = gsub("8", "H", letra)
dados$curso  = gsub("9", "I", dados$curso); letra = gsub("9", "I", letra)
dados$curso  = gsub("1", "A", dados$curso); letra = gsub("1", "A", letra)
table(dados$curso  )
codificacao = data.frame(codificacao, letra) 

#write.table(codificacao, "codificacao_cursos_versaofinal1.csv", row.names = F, sep = ";")

#write.table(dados, "dados_versaofinal1.csv", row.names = F, sep = ";")
#write.table(dados, "dados_versaofinal1_sas.csv", sep=",",dec=".",row.names=F)
