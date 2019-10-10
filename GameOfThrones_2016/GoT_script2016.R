rm(list = ls( all = T))
#Kaggle Datasets
bd1 = read.csv("battles.csv", header = T , stringsAsFactors=TRUE)
bd2 = read.csv("character-deaths.csv", header = T , stringsAsFactors=TRUE)
bd3 = read.csv("character-predictions.csv", header = T , stringsAsFactors=TRUE)

#packages
require(igraph)
require(xtable)
require(plotrix)


#graphics with batles per year and exercite size
par(mfrow=c(1,2))
tab1 = table(bd1$year)
tab1 = tab1*100/sum(tab1)
barplot(tab1, ylab="Percentual",axes=F,xlab="Ano",col="dodgerblue3", main="(a)", ylim = c(0,60))
axis(2,seq(0,60,10),paste(seq(0,60,10),"%"))
abline(h = seq(0,60,10), col="grey", lty=2)
barplot(tab1, axes=F,ylim = c(0,60), add=T,col="dodgerblue3")

tab2 = data.frame(bd1[,18],rep("Ataque"))
names(tab2)=c("Tamanho","Tipo")
tab2.aux = data.frame(bd1[,19],rep("Defesa"))
names(tab2.aux) = names(tab2)
tab2 = rbind(tab2, tab2.aux)
tab2 = subset(tab2, Tamanho<100000)
boxplot(tab2$Tamanho~tab2$Tipo, col = "dodgerblue3", main="(b)", ylab="Tamanho do Exército")

#graph with the relations inside the wars
setas = data.frame(bd1$attacker_1, bd1$defender_1, bd1$attacker_outcome)
setas = subset(setas,bd1.attacker_1!="")
setas = subset(setas,bd1.defender_1!="")
cor.seta = as.character(setas[,3])
cor.seta[cor.seta=="win"]="green3"
cor.seta[cor.seta=="loss"]="red2"
cor.seta[cor.seta==""]="royalblue3"
setas = setas[,-3]
setas = data.frame(t(setas))
setas = unlist(setas)
setas = as.character(setas)
n = length(unique(setas))
g1 <- graph(setas)
g1 <- simplify( g1, remove.loops = F, remove.multiple = F )
casas = data.frame(row.names(cbind(V(g1))),1:16);names(casas)=c("casa","aux")
casas2 = data.frame(table(setas));names(casas2)=c("casa","freq")
casas = merge(casas,casas2,by.x="casa",by.y="casa",all=T)
casas = casas[order(casas$aux),]
casas$freq = (casas$freq+10)*0.6
E(g1)$type = 1:(length(setas)/2); E(g1)$weight <- 10 

par(mfrow=c(1,2))
plot(g1,vertex.size = 8, edge.arrow.size=.8, vertex.color="gray70", vertex.frame.color="gray70", vertex.label.color="black", vertex.label.cex=1.2, vertex.label.dist=0.5,  edge.color = cor.seta )


setas1 = data.frame(bd1$attacker_1, bd1$defender_1, bd1$attacker_outcome);names(setas1)=c("attacker","defender","res")
setas2 = data.frame(bd1$attacker_1, bd1$defender_2, bd1$attacker_outcome);names(setas2)=c("attacker","defender","res")
setas3 = data.frame(bd1$attacker_1, bd1$defender_3, bd1$attacker_outcome);names(setas3)=c("attacker","defender","res")
setas4 = data.frame(bd1$attacker_1, bd1$defender_4, bd1$attacker_outcome);names(setas4)=c("attacker","defender","res")
setas5 = data.frame(bd1$attacker_2, bd1$defender_1, bd1$attacker_outcome);names(setas5)=c("attacker","defender","res")
setas6 = data.frame(bd1$attacker_2, bd1$defender_2, bd1$attacker_outcome);names(setas6)=c("attacker","defender","res")
setas7 = data.frame(bd1$attacker_2, bd1$defender_3, bd1$attacker_outcome);names(setas7)=c("attacker","defender","res")
setas8 = data.frame(bd1$attacker_2, bd1$defender_4, bd1$attacker_outcome);names(setas8)=c("attacker","defender","res")
setas9 = data.frame(bd1$attacker_3, bd1$defender_1, bd1$attacker_outcome);names(setas9)=c("attacker","defender","res")
setas10 = data.frame(bd1$attacker_3, bd1$defender_2, bd1$attacker_outcome);names(setas10)=c("attacker","defender","res")
setas11 = data.frame(bd1$attacker_3, bd1$defender_3, bd1$attacker_outcome);names(setas11)=c("attacker","defender","res")
setas12 = data.frame(bd1$attacker_3, bd1$defender_4, bd1$attacker_outcome);names(setas12)=c("attacker","defender","res")
setas13 = data.frame(bd1$attacker_4, bd1$defender_1, bd1$attacker_outcome);names(setas13)=c("attacker","defender","res")
setas14 = data.frame(bd1$attacker_4, bd1$defender_2, bd1$attacker_outcome);names(setas14)=c("attacker","defender","res")
setas15 = data.frame(bd1$attacker_4, bd1$defender_3, bd1$attacker_outcome);names(setas15)=c("attacker","defender","res")
setas16 = data.frame(bd1$attacker_4, bd1$defender_4, bd1$attacker_outcome);names(setas16)=c("attacker","defender","res")

setas = rbind(setas1,setas2,setas3  ,setas4  ,setas5  ,setas6  ,setas7  ,setas8 ,setas9,setas10 ,setas11 ,setas12 ,setas13 ,setas14 ,setas15 ,setas16)
rm(setas1,setas2,setas3  ,setas4  ,setas5  ,setas6  ,setas7  ,setas8 ,setas9,setas10 ,setas11 ,setas12 ,setas13 ,setas14 ,setas15 ,setas16)
setas = subset(setas,attacker!="")
setas = subset(setas,defender!="")
setas = subset(setas,!is.na(attacker))
setas = subset(setas,!is.na(defender))

cor.seta = as.character(setas[,3])
cor.seta[cor.seta=="win"]="green3"
cor.seta[cor.seta=="loss"]="red2"
cor.seta[cor.seta==""]="royalblue3"
setas = setas[,-3]
setas = data.frame(t(setas))
setas = unlist(setas)
setas = as.character(setas)
setas[setas=="Brotherhood without Banners"]="Brotherhood"
n = length(unique(setas))
g1 <- graph(setas)
g1 <- simplify( g1, remove.loops = F, remove.multiple = F )
casas = data.frame(row.names(cbind(V(g1))),1:21);names(casas)=c("casa","aux")
casas2 = data.frame(table(setas));names(casas2)=c("casa","freq")
casas = merge(casas,casas2,by.x="casa",by.y="casa",all=T)
casas = casas[order(casas$aux),]
casas$freq = (casas$freq+10)*0.6
E(g1)$type = 1:(length(setas)/2); E(g1)$weight <- 10 
plot(g1,vertex.size = 8, vertex.color="gray70", vertex.frame.color="gray70", vertex.label.color="black", vertex.label.cex=1, vertex.label.dist=0.5,  edge.color = cor.seta, layout = layout_as_star  ) #


#table in LaTeX with the battles of each house
at = unlist(c(as.character(bd1$attacker_1),as.character(bd1$attacker_2),as.character(bd1$attacker_3)
              ,as.character(bd1$attacker_4)))

df = unlist(c(as.character(bd1$defender_1),as.character(bd1$defender_2),as.character(bd1$defender_3)
              ,as.character(bd1$defender_4)))

at = subset(at, !is.na(at) & at!="")
df = subset(df, !is.na(df) & df!="")

at = data.frame(table(at))
df = data.frame(table(df))
tab3 = merge(at,df,by.x="at",by.y="df",all=T)
names(tab3) = c("Casa","Ataques","Defesas")
tab3[is.na(tab3)]=0
tab3$Total = tab3$Ataques+tab3$Defesas

xtab3 = xtable(tab3,align = "rc|c|c|c",digits=0,caption = "Quantidade de ataques e defesas realizados por cada Casa}\\label{bloco5b")
print(xtab3,caption.placement='top',table.placement="H", include.rownames = F, include.colnames=T )

#Table with the distribution of wars is Westeros
tab1 = data.frame(table(bd1$region))
tab1$perc  = paste(round(tab1$Freq*100/sum(tab1$Freq),2),"%",sep="")
names(tab1) = c("Região","Frequência","Percentual")
xtab1 = xtable(tab1,align = "rc|c|c",caption = "Distribuição das Batalhas por Westeros}\\label{bloco5")
print(xtab1,caption.placement='top',table.placement="H", include.rownames = F, include.colnames=T )


#Graphics with the number of deaths per year and book
par(mfrow=c(1,2))
barplot(table(bd2$Death.Year), col="tomato2", ylab="Número de Mortos", xlab="Ano", main="(a)")
barplot(table(bd2$Book.of.Death),col="tomato2", ylab="Número de Mortos", xlab="Livro",main="(b)")

#distribution of deaths per gender and class
par(mfrow=c(1,2))
p1 = table(bd2$Gender)*100/sum(table(bd2$Gender))
label1 = c(paste("Feminino (", round(p1[1],2),"%)",sep=""),paste("Masculino (",round(p1[2],2),"%)",sep=""))
pie3D(p1,labels=label1,start=2,explode=0.1,main="(a)",mar=c(3,3,3,3),
	labelcex=0.9,radius=0.8,theta=0.9,border=c("white"),	col=c("red","darkred"))

p2 = table(bd2$Nobility)*100/sum(table(bd2$Nobility))
label2 = c(paste("Comum (",round(p2[1],2),"%)",sep=""),paste("Nobre (",round(p2[2],2),"%)",sep=""))
pie3D(p2,labels=label2,start=2,explode=0.1,main="(a)",mar=c(3,3,3,3),
	labelcex=0.9,radius=0.8,theta=0.9,border=c("white"),	col=c("red","darkred"))

bd2$Allegiances = gsub("House ","",bd2$Allegiances)
tab2 = data.frame(table(bd2$Allegiances))
tab2$perc = paste(round(tab2$Freq*100/sum(tab2$Freq),2),"%",sep="")
names(tab2) = c("Casa","Frequência","Percentual")
xtab2 = xtable(tab2,align = "rc|c|c",caption = "Mortos por Casa de Westeros}\\label{bloco8")
print(xtab2,caption.placement='top',table.placement="H", include.rownames = F, include.colnames=T )




