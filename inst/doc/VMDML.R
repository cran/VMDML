## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

##Example how the package works
library(VMDML)

#Application
# A Random time series dataset generation

set.seed(6)
data3 <- rnorm(300,6.6,.36)

#Parameter setting
alpha = 2000
tau = 0
K = 3
k=0.8
DC = FALSE
init = 1
tol = 1e-6
#Application of VMDARIMA model
VMDARIMA(data3,.8,alpha,tau,K,DC,init,tol)

#Application of VMDELM model
#VMDELM(data3,0.8,alpha,tau,K,DC,init,tol)


#Parameter setting for RF model
m = 3
n =5

#Application of VMDRF model
VMDRF(data3,k,alpha,tau,K,DC,init,tol,m,n)

#Application of VMDSVR model
VMDSVR(data3,.8,alpha,tau,K,DC,init,tol,"radial","nu-regression")

#Application of VMDTDNN model
#VMDTDNN(data3,.8,alpha,tau,K,DC,init,tol,1,5,20,100)





