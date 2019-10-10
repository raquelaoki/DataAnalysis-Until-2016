#-------------------------TRABALHO FINAL MACHINE LEARNING

#--Alterando o diretorio 
#rm(list = ls( all = T))
#Developed by Raquel Aoki and Fabricio Rodrigues


#-- Carregando as bases
# Dataset from Kaggle 
treino= read.table("train.csv",header=T,sep=",")
teste=read.table("test.csv",header=T,sep=",")

#-- Carregando pacotes
if(!require(e1071)){install.packages("e1071")}
require(e1071)

if(!require(tsne)){install.packages("tsne")}
require(tsne)

if(!require(Rtsne)){install.packages("Rtsne")}
require(Rtsne)

if(!require(deepnet)){install.packages("deepnet")}
require(deepnet)
########################### FUNCAO F1

# erro = data.frame(pred1,bd$target) #tem o vetor predito e correto
classes = c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6",
	"Class_7","Class_8","Class_9")

f1 <- function(bd, classes){
	f1=c()
	for(i in 1:length(classes)){
	tp_fn = subset(bd, bd[,2]==classes[i])
	tp = subset(tp_fn, tp_fn[,1]==tp_fn[,2])
	recall= dim(tp)[1]/dim(tp_fn)[1] 
	tp_fp = subset(bd, bd[,1]==classes[i])
	precision = dim(tp)[1]/ dim(tp_fp)[1]
	f1[i] = 2*precision*recall/(precision+recall)
	}
	return (f1)
}
########################### MODELO SVM 
#conferir se os parametros utilizados sao os melhores
#----- Ajuste modelo 
bd = treino[,-1]
#------------------------- REDUZIR BASES 
########## MUDAR AQUI
#N = 3000
#sub = sample(size=N,x=1:dim(treino)[1])
#bd= treino[c(sub),-1]

########################## REDUZINDO AS BASES COM O TESTE

teste1 = teste[,-1]
treino1 = bd[,-c(94)]
reduct.aux1 = as.matrix(rbind(treino1, teste1))

#PCA --- t1 e t2 serão usados para definir o tamanho da redução no t-sne e RBM
reduct.aux2 = scale(reduct.aux1)
pca1 = prcomp(reduct.aux2)
t1 = length(pca1$sdev[pca1$sdev>=1])
var = apply(pca1$x, 2, var)
t2 = length(cumsum(var/sum(var))[cumsum(var/sum(var))<=0.91])
reduct1 = pca1$x[,c(1:t1)]# OPCAO1: truncando em sdev maior que 1
reduct2 = pca1$x[,c(1:t2)] #OPCAO2: truncando em variancia explicada de 90%

#RBM
reduct.aux3=reduct.aux1
for(i in 1:dim(reduct.aux3)[2]){
	reduct.aux3[,i] = reduct.aux3[,i]/(max(reduct.aux3[,i])-min(reduct.aux3[,i])) }
ptm <- proc.time()
rbm1 = rbm.train(reduct.aux3,t1,numepochs = 10, cd = 1,learningrate = 0.1,
	learningrate_scale = 0.5,momentum = 1.) ; w1 = rbm1$W
##################################################################################
#TEMPO TREINAMENTO RBM 10 Epocas  PCA 1
proc.time() - ptm
##################################################################################
reduct5 = reduct.aux3%*%t(w1)

ptm <- proc.time()
rbm2 = rbm.train(reduct.aux3,t2,numepochs = 10, cd = 1,learningrate = 0.1,
	learningrate_scale = 0.5,momentum = 1.) ; w2 = rbm2$W
##################################################################################
#TEMPO TREINAMENTO RBM 10 Epocas  PCA 2
proc.time() - ptm
##################################################################################
reduct6 = reduct.aux3%*%t(w2)

ptm <- proc.time()
rbm3 = rbm.train(reduct.aux3,t1,numepochs = 100, cd = 1,learningrate = 0.1,
	learningrate_scale = 0.5,momentum = 1.) ; w3 = rbm3$W
##################################################################################
#TEMPO TREINAMENTO RBM 100 Epocas  PCA 1
proc.time() - ptm
##################################################################################
reduct7 = reduct.aux3%*%t(w3)


ptm <- proc.time()
rbm4 = rbm.train(reduct.aux3,t2,numepochs = 100, cd = 1,learningrate = 0.1,
	learningrate_scale = 0.5,momentum = 1.) ; w4 = rbm4$W
##################################################################################
#TEMPO TREINAMENTO RBM 100 Epocas  PCA 1
proc.time() - ptm
##################################################################################
reduct8 = reduct.aux3%*%t(w4)

reduct1.1 = data.frame(reduct1[1:dim(bd)[1],],bd$target) #PCA 1
reduct2.1 = data.frame(reduct2[1:dim(bd)[1],],bd$target) #PCA 2
reduct5.1 = data.frame(reduct5[1:dim(bd)[1],],bd$target) #RBM 10 PCA 1
reduct6.1 = data.frame(reduct6[1:dim(bd)[1],],bd$target) #RBM 10 PCA 2
reduct7.1 = data.frame(reduct7[1:dim(bd)[1],],bd$target) #RBM 100 PCA 1
reduct8.1 = data.frame(reduct8[1:dim(bd)[1],],bd$target) #RBM 100 PCA 2



########################## AJUSTE DO SVM NAS BASES REDUZIDAS
ptm <- proc.time()
mod1 = svm(bd.target~.,data=reduct1.1,gamma=0.9,tolerance=0.9,epsilon=0.5,kernel="radial",cross=5)
##################################################################################
#TEMPO SVM PCA 1
proc.time() - ptm
##################################################################################


ptm <- proc.time()
mod5 = svm(bd.target~.,data=reduct5.1,gamma=0.9,tolerance=0.9,epsilon=0.5,kernel="radial",cross=5)
##################################################################################
#TEMPO SVM RBM 10 EPOCAS PCA 1
proc.time() - ptm
##################################################################################


ptm <- proc.time()
mod7 = svm(bd.target~.,data=reduct7.1,gamma=0.9,tolerance=0.9,epsilon=0.5,kernel="radial",cross=5)
##################################################################################
#TEMPO SVM RBM 100 EPOCAS PCA 1
proc.time() - ptm
##################################################################################


save.image("machine_learning_01.RData")


########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
#2º Dia


ptm <- proc.time()
mod6 = svm(bd.target~.,data=reduct6.1,gamma=0.9,tolerance=0.9,epsilon=0.5,kernel="radial",cross=5)
##################################################################################
#TEMPO SVM RBM 10 EPOCAS PCA 2
proc.time() - ptm
##################################################################################



ptm <- proc.time()
mod8 = svm(bd.target~.,data=reduct8.1,gamma=0.9,tolerance=0.9,epsilon=0.5,kernel="radial",cross=5)
##################################################################################
#TEMPO SVM RBM 100 EPOCAS PCA 2
proc.time() - ptm
##################################################################################

save.image("machine_learning_02.RData")





########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
#3º Dia

ptm <- proc.time()
mod2 = svm(bd.target~.,data=reduct2.1,gamma=0.9,tolerance=0.9,epsilon=0.5,kernel="radial",cross=5)
##################################################################################
#TEMPO SVM PCA 2
proc.time() - ptm
##################################################################################

save.image("machine_learning_03.RData")




########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
#4º Dia


########################## SVM PURO
ptm <- proc.time()
mod0 = svm(target~.,data=bd,gamma=0.9,tolerance=0.9,epsilon=0.5,kernel="radial",cross=5)
##################################################################################
#TEMPO SVM PURO
proc.time() - ptm
##################################################################################

save.image("machine_learning_04.RData")




########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
# ANALISE DO F1
#----- testando o novo modelo no proprio treino
pred0=fitted(mod0)
pred1=fitted(mod1)
pred2=fitted(mod2)
pred5=fitted(mod5)
pred6=fitted(mod6)
pred7=fitted(mod7)
pred8=fitted(mod8)

erro0 = data.frame(pred0, bd$target)
erro1 = data.frame(pred1, bd$target)
erro2 = data.frame(pred2, bd$target)
erro5 = data.frame(pred5, bd$target)
erro6 = data.frame(pred6, bd$target)
erro7 = data.frame(pred7, bd$target)
erro8 = data.frame(pred8, bd$target)

erro0.1 = f1(erro0, classes)
erro1.1 = f1(erro1, classes)
erro2.1 = f1(erro2, classes)
erro5.1 = f1(erro5, classes)
erro6.1 = f1(erro6, classes)
erro7.1 = f1(erro7, classes)
erro8.1 = f1(erro8, classes)

rbind(erro0.1,erro1.1,erro2.1,erro5.1,erro6.1,erro7.1,erro8.1)

##########################  VALORES PREDITOS

base.teste1 = as.data.frame(reduct1[(dim(bd)[1]+1):dim(reduct1)[1],])
base.teste2 = as.data.frame(reduct2[(dim(bd)[1]+1):dim(reduct2)[1],])
base.teste5 = as.data.frame(reduct5[(dim(bd)[1]+1):dim(reduct5)[1],])
base.teste6 = as.data.frame(reduct6[(dim(bd)[1]+1):dim(reduct6)[1],])
base.teste7 = as.data.frame(reduct7[(dim(bd)[1]+1):dim(reduct7)[1],])
base.teste8 = as.data.frame(reduct8[(dim(bd)[1]+1):dim(reduct8)[1],])

names(base.teste5) = names(reduct5.1)[-length(names(reduct5.1))]
names(base.teste6) = names(reduct6.1)[-length(names(reduct6.1))]
names(base.teste7) = names(reduct7.1)[-length(names(reduct7.1))]
names(base.teste8) = names(reduct8.1)[-length(names(reduct8.1))]

predito0 = predict(mod0,teste)
predito2 = predict(mod2,base.teste2)
predito6 = predict(mod6,base.teste6)
predito8 = predict(mod8,base.teste8)
predito1 = predict(mod1,base.teste1)
predito5 = predict(mod5,base.teste5)
predito7 = predict(mod7,base.teste7)

rbind(erro0.1,erro1.1,erro2.1,erro5.1,erro6.1,erro7.1,erro8.1)

save.image("machine_learning_.RData")


coluna_svm_concordancia = c(sum(predito0==predito0)/length(predito0),
sum(predito0==predito2)/length(predito0),sum(predito0==predito6)/length(predito0),
sum(predito0==predito8)/length(predito0),sum(predito0==predito1)/length(predito0),
sum(predito0==predito5)/length(predito0),sum(predito0==predito7)/length(predito0))

coluna_svm.pca.63_concordancia = c(sum(predito2!=predito0)/length(predito0),
sum(predito2==predito2)/length(predito0),sum(predito2==predito6)/length(predito0),
sum(predito2==predito8)/length(predito0),sum(predito2==predito1)/length(predito0),
sum(predito2==predito5)/length(predito0),sum(predito2==predito7)/length(predito0))

coluna_svm.rbm1.63_concordancia = c(sum(predito6!=predito0)/length(predito0),
sum(predito6!=predito2)/length(predito0),sum(predito6==predito6)/length(predito0),
sum(predito6==predito8)/length(predito0),sum(predito6==predito1)/length(predito0),
sum(predito6==predito5)/length(predito0),sum(predito6==predito7)/length(predito0))

coluna_svm.rbm2.63_concordancia = c(sum(predito8!=predito0)/length(predito0),
sum(predito8!=predito2)/length(predito0),sum(predito8!=predito6)/length(predito0),
sum(predito8==predito8)/length(predito0),sum(predito8==predito1)/length(predito0),
sum(predito8==predito5)/length(predito0),sum(predito8==predito7)/length(predito0))

coluna_svm.pca.27_concordancia = c(sum(predito1!=predito0)/length(predito0),
sum(predito1!=predito2)/length(predito0),sum(predito1!=predito6)/length(predito0),
sum(predito1!=predito8)/length(predito0),sum(predito1==predito1)/length(predito0),
sum(predito1==predito5)/length(predito0),sum(predito1==predito7)/length(predito0))

coluna_svm.rbm1.27_concordancia = c(sum(predito5!=predito0)/length(predito0),
sum(predito5!=predito2)/length(predito0),sum(predito5!=predito6)/length(predito0),
sum(predito5!=predito8)/length(predito0),sum(predito5!=predito1)/length(predito0),
sum(predito5==predito5)/length(predito0),sum(predito5==predito7)/length(predito0))

coluna_svm.rbm2.27_concordancia = c(sum(predito7!=predito0)/length(predito0),
sum(predito7!=predito2)/length(predito0),sum(predito7!=predito6)/length(predito0),
sum(predito7!=predito8)/length(predito0),sum(predito7!=predito1)/length(predito0),
sum(predito7!=predito5)/length(predito0),sum(predito7==predito7)/length(predito0))


concordancia = data.frame(coluna_svm_concordancia,coluna_svm.pca.63_concordancia,
coluna_svm.rbm1.63_concordancia,coluna_svm.rbm2.63_concordancia,
coluna_svm.pca.27_concordancia,coluna_svm.rbm1.27_concordancia,
coluna_svm.rbm2.27_concordancia )
 

names(concordancia)=c("svm","svm+pca.63","svm+rbm1.63","svm+rbm2.63", 
	"svm+pca.27","svm+rbm1.27", "svm+rbm2.27")
rownames(concordancia)=c("svm","svm+pca.63","svm+rbm1.63","svm+rbm2.63", 
	"svm+pca.27","svm+rbm1.27", "svm+rbm2.27")

require(xtable) 
xtable(concordancia)

t0 = table(predito0)*100/length(predito0)
t2 = table(predito2)*100/length(predito2)
t6 = table(predito6)*100/length(predito6)
t8 = table(predito8)*100/length(predito8)
t1 = table(predito1)*100/length(predito1)
t5 = table(predito5)*100/length(predito5)
t7 = table(predito7)*100/length(predito7)

par(mfrow=c(2,2))

c1 = c(t0[1], t2[1],t6[1],t8[1],t1[1], t5[1], t7[1])
names(c1) = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)")
barplot(c1, col = "green",ylim =c(0,2), axes=F,main = "Classe 1",
	ylab = "Percentual da classe na base teste", xlab="Técnica")
box()
axis(2, seq(0,2, by = 0.5), paste(seq(0,2, by = 0.5),"%",sep=""))

c2 = c(t0[2], t2[2],t6[2],t8[2],t1[2], t5[2], t7[2])
names(c2) = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)")
barplot(c2, col = "green",ylim =c(0,40), axes=F, main = "Classe 2",
	ylab = "Percentual da classe na base teste", xlab="Técnica")
box()
axis(2, seq(0,40, by = 5), paste(seq(0,40, by = 5),"%",sep=""))

c3 = c(t0[3], t2[3],t6[3],t8[3],t1[3], t5[3], t7[3])
names(c3) = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)")
barplot(c3, col = "green",ylim =c(0,12), axes=F,  main = "Classe 3",
	ylab = "Percentual da classe na base teste", xlab="Técnica")
box()
axis(2, seq(0,12, by = 3), paste(seq(0,12, by =3),"%",sep=""))

c4 = c(t0[4], t2[4],t6[4],t8[4],t1[4], t5[4], t7[4])
names(c4) = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)")
barplot(c4, col = "green",ylim =c(0,2), axes=F, main = "Classe 4",
	ylab = "Percentual da classe na base teste", xlab="Técnica")
box()
axis(2, seq(0,2, by = 0.5), paste(seq(0,2, by = 0.5),"%",sep=""))

savePlot(filename ="figura4" ,type = "eps", device = dev.cur())

c5 = c(t0[5], t2[5],t6[5],t8[5],t1[5], t5[5], t7[5])
names(c5) = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)")
barplot(c5, col = "green",ylim =c(0,5), axes=F,main = "Classe 5", 
	ylab = "Percentual da classe na base teste", xlab="Técnica")
box()
axis(2, seq(0,5, by = 1), paste(seq(0,5, by = 1),"%",sep=""))

c6 = c(t0[6], t2[6],t6[6],t8[6],t1[6], t5[6], t7[6])
names(c6) = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)")
barplot(c6, col = "green",ylim =c(0,75), axes=F, main = "Classe 6",
	ylab = "Percentual da classe na base teste", xlab="Técnica")
box()
axis(2, seq(0,72, by = 9), paste(seq(0,72, by =9),"%",sep=""))

c7 = c(t0[7], t2[7],t6[7],t8[7],t1[7], t5[7], t7[7])
names(c7) = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)")
barplot(c7, col = "green",ylim =c(0,5), axes=F, main = "Classe 7",
	ylab = "Percentual da classe na base teste", xlab="Técnica")
box()
axis(2, seq(0,5, by = 1), paste(seq(0,5, by = 1),"%",sep=""))

#install.packages("gplots")
#require(gplots)
txt = c("(a) = SVM","(b) = SVM+PCA 63","(c) = SVM+RBM1 63","(d) = SVM+RBM2 63", 
	"(e) = SVM+PCA 27","(f) = SVM+RBM1 27", "(g) = SVM+RBM2 27")
textplot(txt, cex=1.5)

savePlot(filename ="figura5" ,type = "eps", device = dev.cur())
