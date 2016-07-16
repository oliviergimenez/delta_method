# calcul de Pr(C3) avec intervalles de confiance (delta-method - lis ça http://www.phidot.org/software/mark/docs/book/pdf/app_2.pdf)
library(msm) # package qui permet d'appliquer la delta-method
# paramètres estimés
PiA = 0.846480573
Psi1 = 0.050581811
ext = 0.063149115
colo = 0.009457465
PA11 = 0.135604272
PB11 = 0.551090661
PA10 = 0.001087288
PB10 = 0.02427791
b = 0.803265902

# standard errors   
PiA_SE = 0.019407509
Psi1_SE = 0.006529989
ext_SE = 0.006612061
colo_SE = 0.000682517
PA11_SE = 0.008392564
PB11_SE = 0.013680708
PA10_SE = 0.000214101
PB10_SE = 0.002114077
b_SE = 0.008449695

# on calcule les psi et les SEs
Psi = matrix(0, nrow = 19, ncol = 1)
Psi_SE = matrix(0, nrow = 19, ncol = 1)
Psi[1,] <- Psi1
Psi_SE[1,] <- Psi1_SE
for(i in 2: 19){
	psi_current <- Psi[i-1,]
	psi_se_current <- Psi_SE[i-1,]
	estmean <- c(psi_current,ext,colo)
	estvar <- diag(c(psi_se_current,ext_SE,colo_SE)^2)
	Psi[i,] = (psi_current*(1-ext)) + ((1-psi_current)*colo)
	Psi_SE[i,] = deltamethod(~ x1*(1-x2) + (1-x1)*x3, estmean, estvar)
}


# on calcule P10, P11 et les SEs associées
P10 = PiA*PA10 + (1-PiA)*PB10
P11 = PiA*PA11 + (1-PiA)*PB11
estmean <- c(PiA,PA11,PB11,PA10,PB10)
estvar <- diag(c(PiA_SE,PA11_SE,PB11_SE,PA10_SE,PB10_SE)^2)
P10_SE <- deltamethod(~ x1*x4 + (1-x1)*x5, estmean, estvar)
P11_SE <- deltamethod(~ x1*x2 + (1-x1)*x3, estmean, estvar)


# on calcule la proportion of C3 in presence data would be expected to be around : 
S_est <- matrix(0, nrow = 19, ncol = 1) 
S_SE_est <- matrix(0, nrow = 19, ncol = 1) 
for(i in 1: 19){
	psi_current <- Psi[i,]
	psi_current_SE <- Psi_SE[i,]	
	estmean <- c(psi_current,P10,P11,b)
	estvar <- diag(c(psi_current_SE,P10_SE,P11_SE,b_SE)^2)
	S_est[i,] = (((1-psi_current)*P10) + (psi_current*P11*(1-b)))*2183
	S_SE_est[i,] = deltamethod(~ (((1-x1)*x2) + (x1*x3*(1-x4)))*2183, estmean, estvar)	  
}

# res
data.frame(PrC3 = S_est,SE_PrC3 = S_SE_est)





