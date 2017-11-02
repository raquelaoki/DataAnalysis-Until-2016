#-----##-----##-----##-----##-----##-----##-----##-----##-----#
#-----##-----##-----##-----##-----##-----##-----##-----##-----#
#-----#		Raquel Aoki				#-----#
#-----##-----##-----##-----##-----##-----##-----##-----##-----#
#-----##-----##-----##-----##-----##-----##-----##-----##-----#


#-----##Carregando Dados
setwd("C:\\Users\\Raquel Aoki\\Google Drive\\UFMG\\Stats4Good")
bd = read.table("dados_versaofinal1.csv",header=T, sep=";")
bd = subset(bd, !is.na(ano))
g1 = table(bd$ano)

#-----##Gráfico quantidade de alunos ao longo dos anos
require(ggplot2)
require(gridExtra)
g2 = data.frame(g1)
g2$perc =paste(round(g2$Freq*100/sum(g2$Freq),2),"%",sep="")
windows(10,6)
ggplot(data=g2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=perc), vjust=-0.3, size=2.9)+
  scale_y_continuous('Quantidade de alunos')+
  scale_x_discrete('Ano em que a disciplina foi feita')+
  theme_minimal()
ggsave(filename = "graf1.png")

#-----## RSG ao longo dos anos
windows(10,6)
ggplot(bd, aes(x=as.factor(ano), y=as.numeric(rsg)))+
	geom_boxplot(fill="seagreen1",colour="seagreen4")+
	geom_hline(yintercept = mean(bd$rsg),colour ="snow4",size=1) +
  	scale_y_continuous('RSG médio')+
	scale_x_discrete('Ano em que a disciplina foi feita')+
	theme_minimal() 
ggsave(filename = "graf2.png")

#-----## RSG por instituição
ggplot(bd, aes(x=as.factor(inst), y=as.numeric(rsg)))+
	geom_boxplot(fill="orange",colour="orange4")+
  	scale_y_continuous('RSG médio')+
	scale_x_discrete('Instituição dos Estudantes')+
	geom_hline(yintercept = mean(bd$rsg),colour ="snow4",size=1) +
	theme_minimal() 
ggsave(filename = "graf3.png")

#-----## Quantidade de alunos 
library(plotrix)
label = paste(g3$Instituição, " (",g3$Freq,"%)",sep="")
pie3D(g3$Freq,labels=label,start=0,explode=0.1,main="",mar=c(4,1,4,4),
	labelcex=1.5,radius=0.8,theta=0.9,border=c("white"),
	col=c("darkorange","green2","royalblue3"))
savePlot(filename ="graf4", type="png",device = dev.cur())

#-----## Percentual de aprovados por instituição 
library(plyr)
bd$ap[bd$ap==1] = "Aprovado"
bd$ap[bd$ap==0] = "Reprovado"
names(bd)[names(bd)=="ap"]="Resultado"
t3 = table(bd$Resultado, bd$inst)
t3[,1]=t3[,1]/colSums(table(bd$Resultado, bd$inst))[1]
t3[,2]=t3[,2]/colSums(table(bd$Resultado, bd$inst))[2]
t3[,3]=t3[,3]/colSums(table(bd$Resultado, bd$inst))[3]
g4 = data.frame(t3)
g4 = ddply(g4, .(Var2), transform, position = cumsum(Freq) - 0.5*(Freq) )
names(g4)=c("Resultado", "Instituição", "Percentual", "position")
ggplot(g4,aes(x = Instituição, y = Percentual, fill =Resultado)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = sprintf("%1.2f%%", 100*Percentual), y = position))+
  scale_fill_manual(values=c("cyan3","coral1"))+
  scale_y_continuous('Percentual',labels = percent)+
  scale_x_discrete('Instituição')+
  theme_minimal()
ggsave(filename = "graf5.png")

#-----## Comparações Significativas
c1 = c(49,17);s1 = sum(c1) ;p1= c1/sum(c1)
c2 = c(64,14);s2 = sum(c2) ;p2= c2/sum(c2)
c3 = c(2,1)	 ;s3 = sum(c3) ;p3= c3/sum(c3)

label1 = paste(c1,c(" Significativas\n(", " Não significativas \n ("),round(p1*100,2),"%)",sep="")
label2 = paste(c2,c(" Significativas\n(", " Não significativas \n("),round(p2*100,2),"%)",sep="")
label3 = paste(c3,c(" Significativas\n(", " Não significativas \n("),round(p3*100,2),"%)",sep="")

windows(10,5)
par(mfrow=c(1,3))
pie3D(p1,labels=label1,start=2,explode=0.1,main="ICEX",mar=c(3,3,3,3),
	labelcex=0.9,radius=0.8,theta=0.9,border=c("white"),
	col=c("darkgreen","green2"))
pie3D(p2,labels=label2,start=2,explode=0.1,main="E. ENGENHARIA",mar=c(3,3,3,3),
	labelcex=0.9,radius=0.8,theta=0.9,border=c("white"),
	col=c("darkblue","lightblue"))
pie3D(p3,labels=label3,start=2,explode=0.1,main="FACE e IGC",mar=c(3,3,3,3),
	labelcex=0.9,radius=0.8,theta=0.9,border=c("white"),
	col=c("chocolate2","orange"))

savePlot(filename ="graf6", type="png",device = dev.cur())

#-----## Razao de chances
inst = c("E. Engenharia", "ICEX", "E. Engenharia")
ref = c("ICEX", "Outros", "Outros")
odds= c(6.692, 5.917, 39.688)
var1 =  paste(inst, " \n Referência: ", ref, sep="")
g5 = data.frame(var1, odds)
g5 <- within(g5, var1 <- factor(var1, levels= g5$var1[order(g5$odds)]))
ggplot(data=g5, aes(x=var1, y=odds)) +
  geom_bar(stat="identity", fill="darkorchid1")+
  geom_text(aes(label=odds), vjust=-0.3, size=2.9)+
  scale_y_continuous('Razão de Chances')+
  scale_x_discrete('')+
  theme_minimal()
ggsave(filename = "graf7.png")

#-----## Razao de chances ICEX
curso = c("Ciência da Comp.", "Ciências Atuariais", "Matemática D.","Física D.","Física N.", "Matemática Comp.",
	"Matemática N.", "Matemática D.", "Química Noturno","Matemática D.", "Matemática D" )
ref = c("Matemática D.","Matemática D.","Estatística","Matemática D.","Matemática D.","Matemática D.",
	"Matemática D.","Química D.", "Matemática D.","Química Tec.", "Sistemas de Inf.")
odds= c(3.084, 2.475, 1/0.468, 1.945, 3.754, 1.541, 5.233, 1/0.722, 1.109, 1/0.541, 1/0.863)

var2 =  paste(curso, " \n Referência: ", ref, sep="")
g6 = data.frame(var2, odds)
g6 <- within(g6, var2 <- factor(var2, levels= g6$var2[order(g6$odds)]))
ggplot(data=g6, aes(x=var2, y=odds)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size=12))+
  geom_bar(stat="identity", fill="chartreuse3")+
  geom_text(aes(label=round(odds,3)), vjust=-0.3, size=3)+
  scale_y_continuous('Razão de Chances')+
  scale_x_discrete('')
ggsave(filename = "graf8.png")

#-----## Razao de chances ENGENHARIA
ref = c("E. Ambiental","E. Civil", "E. Controle e A. D", 
	"E. Controle e A. N","E. de Minas", "E. de Produção","E. de Sistemas",
	"E. Elétrica","E. Mecânica D.","E. Mecânica N.","E. Metalúrgica",
	"E. Química")

curso = rep("E. Aeroespacial",length(ref))

odds = 1/c(0.016, 0.123, 0.364, 0.443, 0.143, 0.101,
	 0.636, 0.114, 0.166, 0.221, 0.094, 0.069)

var2 =  paste(curso, " \n Referência: ", ref, sep="")
g7 = data.frame(var2, odds)
g7 <- within(g7, var2 <- factor(var2, levels= g7$var2[order(g7$odds)]))
ggplot(data=g7, aes(x=var2, y=odds)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size=12))+
  geom_bar(stat="identity", fill="cornflowerblue")+
  geom_text(aes(label=round(odds,3)), vjust=-0.3, size=3)+
  scale_y_continuous('Razão de Chances')+
  scale_x_discrete('')
ggsave(filename = "graf9.png")


#-----## Razao de chances FACE e ICg
curso = c("Controladoria e Finanças", "Geologia")
ref = rep("Ciências Econômicas",length(2))
odds = c(2.951, 4.159)
var2 =  paste(curso, " \n Referência: ", ref, sep="")
g8 = data.frame(var2, odds)
g8 <- within(g8, var2 <- factor(var2, levels= g8$var2[order(g8$odds)]))
ggplot(data=g8, aes(x=var2, y=odds)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, size=12))+
  geom_bar(stat="identity", fill="sandybrown")+
  geom_text(aes(label=round(odds,3)), vjust=-0.3, size=3)+
  scale_y_continuous('Razão de Chances')+
  scale_x_discrete('')
ggsave(filename = "graf10.png")
