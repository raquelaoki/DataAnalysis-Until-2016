#-------------Item Response Theory - 2014


#--DADOS
dados=read.table("ITR_data_.txt", header=TRUE)
dados_p=read.table("ITR_data_piloto.txt", header=TRUE)

require(ltm)
require(polycor)
require ( difR)
itens = dados [,4:9]
itens_o = dados_p[,6:11]


#--SUPOSIÇÕES
descript(itens)
rcor.test(itens)

matriz.resp= itens         # <- colocar aqui matriz de respostas 
n=ncol(matriz.resp)
mcorr <- matrix(1,n,n)
for (i in 1:(n-1)){
for (j in (i+1):n){
	mcorr[i,j]=polychor(matriz.resp[,i], matriz.resp[,j], ML = FALSE, maxcor=.9999)
	mcorr[j,i]=mcorr[i,j]}}
mcorr=as.data.frame(mcorr)
row.names(mcorr)=names(mcorr)=names(matriz.resp)
mcorr   # matriz de correlação policórica
mcorr
pca3 = eigen(mcorr)
plot(pca3$values , type = "b")


itens2 = itens[,2:6] #sem medicos
matriz.resp= itens2
n=ncol(matriz.resp)
mcorr <- matrix(1,n,n)
for (i in 1:(n-1)){
for (j in (i+1):n){
	mcorr[i,j]=polychor(matriz.resp[,i], matriz.resp[,j], ML = FALSE, maxcor=.9999)
	mcorr[j,i]=mcorr[i,j]}}
mcorr=as.data.frame(mcorr)
row.names(mcorr)=names(mcorr)=names(matriz.resp)
mcorr 
pca3 = eigen(mcorr)
plot(pca3$values , type = "b",ylab = "Autovalores" , xlab = "")

#-- MODELO 

names(itens2)=c("1","2","3","4","5")

fit1 = rasch(data = itens2 )

#--GRAFICOS
par(mfrow=c(2,3))
plot(fit1, type = "ICC", zrange = c(-3.8, 3.8),main="ICC Item 'feliciano'", item=c(1),ylab="Probabilidade", xlab="Habilidade")
plot(fit1, type = "ICC", zrange = c(-3.8, 3.8),main="ICC Item 'diretasja'", item=c(2),ylab="Probabilidade", xlab="Habilidade")
plot(fit1, type = "ICC", zrange = c(-3.8, 3.8),main="ICC Item 'pec'", item=c(3),ylab="Probabilidade", xlab="Habilidade")
plot(fit1, type = "ICC", zrange = c(-3.8, 3.8),main="ICC Item 'vicegov'", item=c(4),ylab="Probabilidade", xlab="Habilidade")
plot(fit1, type = "ICC", zrange = c(-3.8, 3.8),main="ICC Item 'vicepres'", item=c(5),ylab="Probabilidade", xlab="Habilidade")

windows()
par(mfrow=c(2,3))
plot(fit1, type = "IIC", zrange = c(-3.8, 3.8),main="IIC Item 'feliciano'", item=c(1),ylab="Informação", xlab="Habilidade")
plot(fit1, type = "IIC", zrange = c(-3.8, 3.8),main="IIC Item 'diretasja'", item=c(2),ylab="Informação", xlab="Habilidade")
plot(fit1, type = "IIC", zrange = c(-3.8, 3.8),main="IIC Item 'pec'", item=c(3),ylab="Informação", xlab="Habilidade")
plot(fit1, type = "IIC", zrange = c(-3.8, 3.8),main="IIC Item 'vicegov'", item=c(4),ylab="Informação", xlab="Habilidade")
plot(fit1, type = "IIC", zrange = c(-3.8, 3.8),main="IIC Item 'vicepres'", item=c(5),ylab="Informação", xlab="Habilidade")

windows()
par(mfrow=c(1,3))
plot(fit1, type = "ICC", zrange = c(-3.8, 3.8),main="ICC",ylab="Probabilidade", xlab="Habilidade")
plot(fit1, type = "IIC", zrange = c(-3.8, 3.8),main="CCI",ylab="Informação", xlab="Habilidade")
plot(fit1, type = "IIC", zrange = c(-3.8, 3.8), items=0, main = "Função do Teste de Informação", ylab = "Informação", xlab = "Habilidade" )

#--DIF

names(dados)
attach(dados)
dif_universidade = cbind(universidade, itens2)
dif_sexo = cbind (sexo , itens2)
dif_area = cbind (area, itens2)
dif_tipo = cbind(tipo, itens2)
dif_manifestante=dados[,4:10]
dif_manifestante=dados[,5:10]
detach(dados)

res1=difMH(Data=dif_manifestante, group="manif" , focal.name=1)
W1=difLogistic(dif_manifestante, group="manif" , focal.name=1)
raju1=difRaju(dif_manifestante,group="manif", focal.name=1, model="1PL",purify=TRUE)
par(mfrow=c(1,3))
plot(res1)
plot(W1)
plot(raju1)

res2=difMH(Data=dif_universidade, group="universidade" , focal.name=1)
W2=difLogistic(dif_universidade, group="universidade" , focal.name=1)
raju2=difRaju(dif_universidade,group="universidade", focal.name=1, model="1PL",purify=TRUE)
par(mfrow=c(1,3))
plot(res2)
plot(W2)
plot(raju2)

res3=difMH(Data=dif_sexo, group="sexo" , focal.name=1)
W3=difLogistic(dif_sexo, group="sexo" , focal.name=1)
raju3=difRaju(dif_sexo,group="sexo", focal.name=1, model="1PL",purify=TRUE)
par(mfrow=c(1,3))
plot(res3)
plot(W3)
plot(raju3)

res4=difMH(Data=dif_area, group="area" , focal.name=1)
W4=difLogistic(dif_area, group="area" , focal.name=1)
raju4=difRaju(dif_area,group="area", focal.name=1, model="1PL",purify=TRUE)
par(mfrow=c(1,3))
plot(res4)
plot(W4)
plot(raju4)

res5=difMH(Data=dif_tipo, group="tipo" , focal.name=1)
W5=difLogistic(dif_tipo, group="tipo" , focal.name=1)
raju5=difRaju(dif_tipo,group="tipo", focal.name=1, model="1PL",purify=TRUE)
par(mfrow=c(1,3))
plot(res5)
plot(W5)
plot(raju5)

#--SCORES

scores=factor.scores(fit1,resp.patterns=itens2)
scores
notas=scores$score.dat$z1
#factor.scores(modelo1)

#--COMPARAÇÃO SCORES
dif_universidade = cbind(dif_universidade, notas)
dif_sexo = cbind(dif_sexo, notas)
dif_area = cbind(dif_area, notas)
dif_tipo= cbind(dif_tipo, notas)
dif_manifestante = cbind (dif_manifestante, notas)

uni_puc = subset (dif_universidade, universidade == 0) 
uni_ufmg = subset (dif_universidade, universidade == 1) 
sexo_fem = subset ( dif_sexo, sexo == 1 ) 
sexo_masc = subset ( dif_sexo, sexo == 2 ) 
area_hum = subset ( dif_area , area == 1)
area_exe = subset ( dif_area, area == 2) 
area_bio = subset ( dif_area, area == 3 ) 
tipo_amostra = subset ( dif_tipo , tipo == 1)
tipo_piloto = subset ( dif_tipo , tipo == 0)
manifestante_sim = subset ( dif_manifestante , manif == 1)
manifestante_nao = subset ( dif_manifestante , manif == 0)

wilcox.test(uni_puc$notas, uni_ufmg$notas)
wilcox.test(sexo_fem$notas, sexo_masc$notas)
ks.test(area_hum$notas,area_exe$notas,area_bio$notas)
wilcox.test(tipo_amostra$notas, tipo_piloto$notas)
wilcox.test(manifestante_sim$notas,manifestante_nao$notas)

#--DESCRITIVA
mean(uni_puc$notas);mean(uni_ufmg$notas)
mean(sexo_fem$notas);mean( sexo_masc$notas)
mean(area_hum$notas);mean(area_exe$notas);mean(area_bio$notas)
mean(tipo_amostra$notas);mean( tipo_piloto$notas)
mean(manifestante_sim$notas);mean(manifestante_nao$notas)


 

fit1 = rasch(data = itens2 )
fit2 = ltm( itens2 ~z1)
fit3 = tpm(data = itens2 )

anova(fit1,fit2)   # rej, fit 2 melhor


fit4 <- tpm(itens2, type = "latent.trait", max.guessing = 1)#ruim
anova(fit2, fit4)

plot(fit2)
plot(fit2, type = "IIC", zrange = c(-3.8, 3.8), items=0, main = "Função do Teste de Informação", ylab = "Informação", xlab = "Habilidade" )
plot(fit1 ,type = "IIC", zrange = c(-3.8, 3.8), items=0, main = "Função do Teste de Informação", ylab = "Informação", xlab = "Habilidade" )

dpilo=read.table("piloto.txt", header=TRUE)
dim(dpilo)
head(dpilo)
dpilo=dpilo[,6:11]
fit1 = rasch(data = dpilo )
fit2 = ltm( dpilo ~z1)
fit3 = tpm(data = dpilo )


anova(fit1,fit2)
anova(fit1,fit3)
anova(fit2,fit3)
