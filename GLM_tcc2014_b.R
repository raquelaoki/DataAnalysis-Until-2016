##------------------------------- TCC ----------------------------------
##					Raquel Aoki
##				script com analise
##----------------------------------------------------------------------

rm(list=ls(all=TRUE))

dados = read.table( "dados_versaofinal1.csv", header = T, sep = ";", dec = ",")
head(dados); dim(dados) 
cod = read.table("codificacao_cursos_versaofinal1.csv", header =T, sep = ";")
head(cod)



dados$curso <- as.factor(dados$curso)
dados$inst 	<- as.factor(dados$inst)
nomes.inst  <- levels(dados$inst)
dados$ap    <- as.factor(dados$ap)
dados$rsg   <- as.numeric(as.character(dados$rsg))
dados$rsg2  <- as.numeric(dados$rsg2)

dados1 = subset(dados, !is.na(rsg))
tapply(dados1$rsg,dados1$inst, sd)/tapply(dados1$rsg,dados1$inst, mean)


#------------ Analise Descritiva 

perc = 100*round(table(dados$ano)/sum(table(dados$ano)),4)
barplot(perc, axes = F, col = "lightgreen", xlab = "Anos", ylab="Percentual de alunos")
axis(2, 0:8,paste(0:8,"%"))
table(dados$ano,dados$rsg)
dados = subset(dados, !is.na(rsg))
rsg.m = median(dados$rsg)
boxplot(dados$rsg, col = "lightgreen", ylab = "RSG") 
boxplot(dados$rsg~dados$ano, col = "lightgreen", ylab = "RSG") 
abline(h=rsg.m, col = "red")
boxplot(dados$rsg~dados$inst, col = "lightgreen", ylab = "RSG") 
abline(h=rsg.m, col = "red")

#--------Apresentação 
barplot(perc, axes = F,cex.names = 1.2,cex.axis=1.2, cex.lab = 1.2,col = "lightgreen",
	 xlab = "Anos", ylab="Percentual de alunos")
axis(2, 0:8,paste(0:8,"%"), cex.axis = 1.2)
boxplot(dados$rsg, col = "lightgreen", ylab = "RSG") 
boxplot(dados$rsg~dados$ano,cex.axis=1.2, cex.lab=1.2, col = "lightgreen", ylab = "RSG") 
abline(h=rsg.m, col = "red")
boxplot(dados$rsg~dados$inst,cex.axis=1.2, cex.lab=1.2, col = "lightgreen", ylab = "RSG") 
abline(h=rsg.m, col = "red")

#-------- Modelo Logistico m1		: Modelo normal com fun??o de ligacao logit

dados$curso <- relevel(dados$curso, ref="A")
dados$inst	<- relevel(dados$inst, ref="ICEX")
m1 = glm(ap ~ inst/curso+rsg, data = dados,family = binomial(link = 'logit'))

summary(m1)
names(m1)
pchisq(m1$deviance, m1$df.residual, ncp = 0, lower.tail = F, log.p = FALSE)


a = table(dados$ap)
a/sum(a)


odds <- function(modelo){

coef = coefficients(modelo)
coef[is.na(coef)]=0
coef  = data.frame(coef)
var = rownames(coef)
rownames(coef ) = NULL
coef = data.frame(var, coef)
names(coef)= c("var", "coef")
coef = coef[order(coef$var),]
coef = subset(coef, coef != 0)
#x = rep(1, dim(coef)[1])

beta0 = coef[1,2]
rsg = coef[dim(coef)[1],2]
coef = coef[-c(1,dim(coef)[1]),]

cat("\n" ,1,"--", as.character(coef[1,1]), "\n")
for( i in 2:dim(coef)[1]){
	cat(i,"--", as.character(coef[i,1]), "\n")}


cat("\n Entre com a quantidade de níveis(principais) \n")
niveis = scan(n=1)
cat("\n Qual é a referencia \n")
ref = scan(n=1)
inicio = rep(0,niveis)
final  = rep(0,niveis)

for ( i in 1:niveis){
cat("\n DIGITE O PRIMEIRO NUMERO DA SEQUENCIA DOS COEFICICIENTES DO NIVEL  ",i,"\n")
inicio[i] = scan(n=1)
cat("\n DIGITE O ULTIMO NUMERO DA SEQUENCIA DOS COEFICICIENTES DO NIVEL  ",i,"\n")
final[i] = scan(n=1)
}

resul = data.frame(pi.coef=c(0),pi.soma = c(0))
for( j in 1:niveis){
   if(j!= ref){
	coef.aux = coef
	coef.aux = coef.aux[inicio[j]:final[j],]
	cov1 = coef.aux[1,2]
	cov2 = c(0,coef.aux[2:dim(coef.aux)[1],2])
	pi = exp(beta0 + rsg + cov1 + cov2)/(1+exp(beta0 + rsg + cov1 + cov2))
	pi.soma = round(sum(pi),4)
	pi.coef = as.character(coef.aux[1,1])
	aux =data.frame(pi.coef, pi.soma)
	resul = rbind(resul, aux)
   }
  if( j == ref){
      coef.aux = coef
	coef.aux = coef.aux[inicio[j]:final[j],]
	cov2 = c(0,coef.aux[2:dim(coef.aux)[1],2])
	pi = exp(beta0 + rsg + cov2)/(1+exp(beta0 + rsg + cov2))
	pi.soma = round(sum(pi),4)
	pi.coef = "Referencia"
	aux =data.frame(pi.coef, pi.soma)
	resul = rbind(resul, aux)

  }
}
resul = resul[-1,]
return (resul)}	


resul = odds(m1) 

(resul$pi.soma[1]/(1-resul$pi.soma[1]))/(resul$pi.soma[2]/(1-resul$pi.soma[2]))
(resul$pi.soma[3]/(1-resul$pi.soma[3]))/(resul$pi.soma[2]/(1-resul$pi.soma[2]))
(resul$pi.soma[1]/(1-resul$pi.soma[1]))/(resul$pi.soma[3]/(1-resul$pi.soma[3]))


coef = coefficients(m1)
coef = subset(coef, !is.na(coef))
coef  = data.frame(coef)
var = rownames(coef)
rownames(coef ) = NULL
coef = data.frame(var, coef)
names(coef)= c("var", "coef")
coef = coef[order(coef$var),]

coef = coef[-c(1,2,26,29),]
data.frame(coef$var, exp(coef$coef) )


#-------------------------------- COMPARAÇÕES MULTIPLAS 

if(!require(aod)){install.packages("aod") }
require(aod) 

# funcao que faz as comparacoes
tab.contraste <- function(modelo, nomes.variaveis, contrastes, nomes.cursos){
	cov = vcov(modelo)[nomes.variaveis, nomes.variaveis]
	# fazendo para o primeira linha	
	teste = wald.test(b = coef(modelo)[nomes.variaveis],
		 Sigma = cov, L =matrix(c(contrastes[1,]), ncol = dim(contrastes)[2]))
	valor.p= round(teste$result$chi2[3],4)
	chisq  = round(teste$result$chi2[1],4)
	L = teste$L
	col = c()
	for ( i in 1:length(L)){
		if(L[i]!= 0) {col = c(col, i)}
	}
	if( length(col)==1){col = c(col, 1)}
	comparacao = c(paste(nomes.cursos[col[1]],nomes.cursos[col[2]], sep = "--"))
	contraste.l = paste(L[1], L[2], sep = " ,")	
	for( i in 3:length(L)){
		contraste.l = paste(contraste.l, L[i], sep = " ,")
	}	
	resultado = data.frame(comparacao, chisq, valor.p, contraste.l)

	# fazendo comparacao para as demais
	for ( i in 2:dim(contrastes)[1]){
		teste = wald.test(b = coef(modelo)[nomes.variaveis],
		 	Sigma = cov, L =matrix(c(contrastes[i,]), ncol = dim(contrastes)[2]))
		valor.p= round(teste$result$chi2[3],4)
		chisq  = round(teste$result$chi2[1],4)
		L     = teste$L
		col = c()
		for ( j in 1:length(L)){
			if(L[j]!= 0) {col = c(col, j)}
		}
		if( length(col)==1){col = c(col, 1)}
		comparacao = c(paste(nomes.cursos[col[1]],nomes.cursos[col[2]], sep = "--"))
		contraste.l = paste(L[1], L[2], sep = " ,")	
		for( i in 3:length(L)){
			contraste.l = paste(contraste.l, L[i], sep = " ,")
		}	
		aux = data.frame(comparacao, chisq, valor.p, contraste.l)
		resultado = rbind(resultado, aux)
	}
 	# arrumando tabela
	a <- c(-0.1,0.001, 0.01, 0.05, 0.1, 1)
  	sign <- as.numeric(cut(resultado$valor.p,a))
  	sign <- gsub(1,"***",sign)
  	sign <- gsub(2,"**",sign)
  	sign <- gsub(3,"*",sign)
  	sign <- gsub(4,".",sign)
  	sign <- gsub(5,"",sign)
	resultado = data.frame( resultado, sign) 
		
	resultado = resultado[, c(1,2,3,5)]; rownames(resultado) = NULL
	names(resultado) = c("Cursos", "Chi-squared test","p-values","")
#	resultado = resultado[, c(1,2,3,5,4)]; rownames(resultado) = NULL
#	names(resultado) = c("Cursos", "Chi-squared test","p-values","","Contraste")
	sig.code  <- "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"
	return ( list(resultado, sig.code, nomes.variaveis))
}


icex.cursos = as.character(subset(cod, Instituicao == "ICEX")$Curso)
eng.cursos=as.character(subset(cod, Instituicao == "ENGENHARIA")$Curso)
outros.cursos=as.character(subset(cod, Instituicao == "OUTROS")$Curso)

# --- comparando instituicoes

nomes.var.inst = c("(Intercept)" ,"instENGENHARIA","instOUTROS" )
l.inst = rbind( cbind(0,1,0),cbind(0, 0, 1),cbind(0, 1,-1))
inst = tab.contraste(m1, nomes.var.inst,l.inst,nomes.inst)
inst[[1]]

# ---- comparando cursos icex

nomes.icex = c("(Intercept)","instICEX:cursoB","instICEX:cursoC",      
 "instICEX:cursoD","instICEX:cursoE","instICEX:cursoF",
 "instICEX:cursoG","instICEX:cursoH","instICEX:cursoI",
 "instICEX:cursoJ","instICEX:cursoK","instICEX:cursoL")        

l.icex = rbind( 
cbind(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),cbind(0, 1, -1,0, 0, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 1, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0),cbind(0, 1, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0),cbind(0, 1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0),
cbind(0, 1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0),cbind(0, 1, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0),
cbind(0, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0),cbind(0, 1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0),
cbind(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1),cbind(0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 1, 0,-1, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 1, 0, 0,-1, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 1, 0, 0, 0,-1, 0, 0, 0, 0, 0),cbind(0, 0, 1, 0, 0, 0, 0,-1, 0, 0, 0, 0),
cbind(0, 0, 1, 0, 0, 0, 0, 0,-1, 0, 0, 0),cbind(0, 0, 1, 0, 0, 0, 0, 0, 0,-1, 0, 0),
cbind(0, 0, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0),cbind(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,-1),
cbind(0, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 0, 1, 0,-1, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 1, 0, 0,-1, 0, 0, 0, 0, 0),cbind(0, 0, 0, 1, 0, 0, 0,-1, 0, 0, 0, 0),
cbind(0, 0, 0, 1, 0, 0, 0, 0,-1, 0, 0, 0),cbind(0, 0, 0, 1, 0, 0, 0, 0, 0,-1, 0, 0),
cbind(0, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1, 0),cbind(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,-1),
cbind(0, 0, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0),cbind(0, 0, 0, 0, 1, 0,-1, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 0, 0),cbind(0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0, 0),
cbind(0, 0, 0, 0, 1, 0, 0, 0, 0,-1, 0, 0),cbind(0, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1, 0),
cbind(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1),cbind(0, 0, 0, 0, 0, 1,-1, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 1, 0,-1, 0, 0, 0, 0),cbind(0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0),cbind(0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1, 0),
cbind(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1),cbind(0, 0, 0, 0, 0, 0, 1,-1, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 1, 0,-1, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0),cbind(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1),
cbind(0, 0, 0, 0, 0, 0, 0, 1,-1, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 0, 1, 0,-1, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0),cbind(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 1,-1, 0, 0),cbind(0, 0, 0, 0, 0, 0, 0, 0, 1, 0,-1, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,-1),cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 1,-1, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,-1),cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,-1))

icex = tab.contraste(m1, nomes.icex,l.icex,icex.cursos)
icex[[1]]
#write.table(eng[[1]], "teste.csv", row.names=F, sep = ";", dec = ",")


# ---- comparando cursos engenharia

nomes.eng = c("instENGENHARIA" , "instENGENHARIA:cursoB", "instENGENHARIA:cursoC",
"instENGENHARIA:cursoD","instENGENHARIA:cursoE", "instENGENHARIA:cursoF",
"instENGENHARIA:cursoG", "instENGENHARIA:cursoH","instENGENHARIA:cursoI" ,
"instENGENHARIA:cursoJ","instENGENHARIA:cursoK" ,"instENGENHARIA:cursoL",
 "instENGENHARIA:cursoM")       

# 78 combinações
l.eng = rbind( 
cbind(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
cbind(0, 1, -1,0, 0, 0, 0, 0, 0, 0, 0, 0, 0),cbind(0, 1, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 1, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0),cbind(0, 1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0),cbind(0, 1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0),
cbind(0, 1, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0),cbind(0, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0),
cbind(0, 1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0),cbind(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0),
cbind(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1),cbind(0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 1, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 1, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0),cbind(0, 0, 1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0),
cbind(0, 0, 1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0),cbind(0, 0, 1, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0),
cbind(0, 0, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0),cbind(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0),
cbind(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1),cbind(0, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 1, 0,-1, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 0, 1, 0, 0,-1, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 1, 0, 0, 0,-1, 0, 0, 0, 0, 0),cbind(0, 0, 0, 1, 0, 0, 0, 0,-1, 0, 0, 0, 0),
cbind(0, 0, 0, 1, 0, 0, 0, 0, 0,-1, 0, 0, 0),cbind(0, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1, 0, 0),
cbind(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0),cbind(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,-1),
cbind(0, 0, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0),cbind(0, 0, 0, 0, 1, 0,-1, 0, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 0, 0, 0),cbind(0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 1, 0, 0, 0, 0,-1, 0, 0, 0),cbind(0, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1, 0, 0),
cbind(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1, 0),cbind(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,-1),
cbind(0, 0, 0, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0),cbind(0, 0, 0, 0, 0, 1, 0,-1, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 0, 0),cbind(0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1, 0, 0),cbind(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1, 0),
cbind(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1),cbind(0, 0, 0, 0, 0, 0, 1,-1, 0, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 1, 0,-1, 0, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0),cbind(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1, 0),
cbind(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1),cbind(0, 0, 0, 0, 0, 0, 0, 1,-1, 0, 0, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 1, 0,-1, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0),cbind(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 1,-1, 0, 0, 0),cbind(0, 0, 0, 0, 0, 0, 0, 0, 1, 0,-1, 0, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0),cbind(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 1,-1, 0, 0),cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,-1, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,-1),cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,-1, 0),
cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,-1),cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,-1))

eng = tab.contraste(m1, nomes.eng,l.eng,eng.cursos)
eng[[1]]



# ---- comparando cursos outros

nomes.outros = c("instOUTROS","instOUTROS:cursoB","instOUTROS:cursoC")        

l.outros = rbind( 
cbind(0,1, 0),
cbind(0,0, 1),
cbind(0,1,-1))

outros = tab.contraste(m1, nomes.outros,l.outros,outros.cursos)
outros[[1]]


# residuos 


residuos <- function(fit.model){

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
h = hatvalues(fit.model)
ts <- resid(fit.model,type="pearson")/sqrt(1-h)
td <- resid(fit.model,type="deviance")/sqrt(1-h)

lim.h = 2*length(coef(fit.model))/(fit.model$df.null+1)


inf = influence.measures(fit.model)
di <- inf$infmat[,"cook.d"]
a <- max(td)
b <- min(td)
par(mfrow=c(2,2))
plot(fitted(fit.model),h,xlab="Valores Ajustados", ylab="Medida h",
main="Pontos de Alavanca", pch=16)
abline(lim.h,0, col = "red")
#identify(fitted(fit.model), h, n=1)
#
plot(di,xlab="Indice", ylab="Distancia de Cook",
main="Pontos Influentes",pch=16)
#identify(di, n=1)
#
plot(td,xlab="Indice", ylab="Residuo Componente do Desvio",
main="Pontos Aberrantes", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2,col = "red")
abline(-2,0,lty=2,col = "red")
# identify(td, n=1)
#
plot(predict(fit.model),td,xlab="Preditor Linear", 
ylab="Residuo Componente do Desvio",
main="Funcao de Ligacao", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2, col = "red")
abline(-2,0,lty=2,col = "red")


}
residuos(m1)
