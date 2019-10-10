/* limpa a memoria do SAS */
proc datasets kill;
run;

proc import 
	datafile="E:\tcc\SAS\dados_versaofinal1_SAS.csv"
	out=work.dados;
run;

proc print DATA = dados (obs = 5);
run;

/* MODELO 1 CONTRASTE CERTO*/

proc genmod DATA=dados; 
	class inst(ref="ICEX" param=ref) curso(ref="A" param=ref);
	model ap = inst curso(inst) rsg/ dist = bin
										link = logit;
	 
	estimate 'ICEX-ENGENHARIA'  	inst -1  0  1 ;
	estimate 'ICEX-OUTROS' 			inst  0 -1  1 ;
	estimate 'ENGENHARIA-OUTROS' 	inst  1 -1  0 ;
		output out=out ;
run;
