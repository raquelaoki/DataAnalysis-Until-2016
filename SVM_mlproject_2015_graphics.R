bd = read.table("SVM_mlproject_2015_time.txt",header=T)
class(bd$metodo)
class(bd$tempo)

bar=barplot(bd$tempo,col = "mediumblue",ylim = c(0,500),ylab="Tempo em minutos do SVM",xlab="Método de redução de dimensão")
box()
axis(1,at = bar, labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)"))

legenda: (a) nenhum
	   (b) PCA com 63 colunas
	   (c) RBM 10 épocas e 63 colunas
	   (d  RBM 100 épocas e 63 colunas
	   (e) PCA com 27 colunas	
	   (f) RBM 10 épocas e 27 colunas
	   (g) RBM 100 épocas e 27 colunsa



savePlot(filename ="figura1" ,type = "eps", device = dev.cur())

\begin{figure}[!ht]
\begin{center}
\centering
\includegraphics[scale = 1.25]{figura1}
\caption{\label{fig_1}Bla bla bla)
\end{center} 
\end{figure}

ac = read.table("SVM_mlproject_2015_accuracy.txt",header=T)
require(xtable)
ac[,2] = paste(round(100*as.numeric(as.character(ac[,2])),2),"%",sep="")
ac[,3] = paste(round(100*as.numeric(as.character(ac[,3])),2),"%",sep="")
ac[,4] = paste(round(100*as.numeric(as.character(ac[,4])),2),"%",sep="")
ac[,5] = paste(round(100*as.numeric(as.character(ac[,5])),2),"%",sep="")
ac[,6] = paste(round(100*as.numeric(as.character(ac[,6])),2),"%",sep="")
ac[,7] = paste(round(100*as.numeric(as.character(ac[,7])),2),"%",sep="")
ac[,8] = paste(round(100*as.numeric(as.character(ac[,8])),2),"%",sep="")
ac[,9] = paste(round(100*as.numeric(as.character(ac[,9])),2),"%",sep="")
ac[,10] = paste(round(100*as.numeric(as.character(ac[,10])),2),"%",sep="")

xtable(ac) 



ac = read.table("acuracia.txt",header=T)
ac[,2] = 100*as.numeric(as.character(ac[,2]))
ac[,3] = 100*as.numeric(as.character(ac[,3]))
ac[,4] = 100*as.numeric(as.character(ac[,4]))
ac[,5] = 100*as.numeric(as.character(ac[,5]))
ac[,6] = 100*as.numeric(as.character(ac[,6]))
ac[,7] = 100*as.numeric(as.character(ac[,7]))
ac[,8] = 100*as.numeric(as.character(ac[,8]))
ac[,9] = 100*as.numeric(as.character(ac[,9]))
ac[,10] = 100*as.numeric(as.character(ac[,10]))


media = (ac$classe1+ac$classe2+ac$classe3+
	ac$classe4+ac$classe5+ac$classe6+
	ac$classe7+ac$classe8+ac$classe9)/9
bd.1 = data.frame(media, bd$tempo)
names(bd.1) = c("media", "tempo")
rownames(bd.1) = c("svm","svm+pca.63","svm+rbm1.63","svm+rbm2.63", 
	"svm+pca.27","svm+rbm1.27", "svm+rbm2.27")
plot(tempo~media,ylab="Tempo em minutos do SVM",xlab="Acurácia Média (%)",
	col = "red",type="p",lwd = 6,xlim = c(55,100),data=bd.1)
with(bd.1,text(bd$tempo~media,labels = row.names(bd.1),
	 pos = c(2,2,4,3,1,4,2), cex=0.9))

savePlot(filename ="figura2" ,type = "eps", device = dev.cur())


mediana = c(median(as.numeric(ac[1,2:10])),median(as.numeric(ac[2,2:10])),
	median(as.numeric(ac[3,2:10])),median(as.numeric(ac[4,2:10])),
	median(as.numeric(ac[5,2:10])),median(as.numeric(ac[6,2:10])),
	median(as.numeric(ac[7,2:10])))

bd.2 = data.frame(mediana, bd$tempo)
names(bd.2) = c("mediana", "tempo")
rownames(bd.2) = c("svm","svm+pca.63","svm+rbm1.63","svm+rbm2.63", 
	"svm+pca.27","svm+rbm1.27", "svm+rbm2.27")
plot(tempo~mediana,ylab="Tempo em minutos do SVM",xlab="Acurácia Mediana (%)",
	col = "red",type="p",lwd = 6,xlim = c(55,100),data=bd.2)
with(bd.2,text(bd$tempo~mediana,labels = row.names(bd.2),
	 pos = c(2,2,4,3,1,4,2), cex=0.9))


savePlot(filename ="figura3" ,type = "eps", device = dev.cur())



#---------------------- CROSS VALIDATION
load(file="machine_learning_.RData")



mod0$tot.accuracy
mod2$tot.accuracy
mod6$tot.accuracy
mod8$tot.accuracy
mod1$tot.accuracy
mod5$tot.accuracy
mod7$tot.accuracy



