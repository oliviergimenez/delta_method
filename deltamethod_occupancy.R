# illustration of the delta method to calculate the standard 
# error of any function of some parameter estimates with 
# standard errors 

# as an example, I use the calculation of time-dependent 
# occupancy probabilities with the following formula 
# psi_(t+1) = psi_t (1 - epsilon_t) + (1 - psi_t) gamma_t
# where epsilon is extinction, gamma is colonisation and 
# psi_t is occupancy year t

# have a look to the following for more info
# http://www.phidot.org/software/mark/docs/book/pdf/app_2.pdf

# load package with relevant function to implement the delta-method
library(msm)

# get some help on the delta-method function
?deltamethod

# to see examples
example(deltamethod)

# parameter estimates
epsilon = 0.39
gamma = 0.07
psi_init = 0.1 # initial occupancy

# standard errors of parameter estimates
se_epsilon = 0.08
se_psi_init = 0.01
se_gamma = 0.05

# memory pre-allocation
psi = matrix(0, nrow = 10, ncol = 1)
psi_se = matrix(0, nrow = 10, ncol = 1)

# initialisation
psi[1,] <- psi_init
psi_se[1,] <- se_psi_init

# iterate calculations 
for(i in 2:10){
	psi_current <- psi[i-1,]
	psi_se_current <- psi_se[i-1,]
	estmean <- c(psi_current,epsilon,gamma)
	estvar <- diag(c(psi_se_current,se_epsilon,se_gamma)^2)
	psi[i,] = (psi_current*(1-epsilon)) + ((1-psi_current)*gamma) # recurrence formula
	psi_se[i,] = deltamethod(~ x1*(1-x2) + (1-x1)*x3, estmean, estvar) # delta-method
}

# display results
data.frame(psi = psi,sterr_psi = psi_se)
