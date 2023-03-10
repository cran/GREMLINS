## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
 # fig.path = "man/figures/README-",
  out.width = "100%"
)

## ----param FG-----------------------------------------------------------------
namesFG <- c('A','B')
v_NQ <-  c(60,50) #size of each FG
list_pi = list(c(0.16 ,0.40 ,0.44),c(0.3,0.7)) #proportion of each block in each  FG
list_pi[[1]]

## ----param 1 net--------------------------------------------------------------
E  <-  rbind(c(1,2),c(2,2),c(1,1))
typeInter <- c( "inc","diradj", "adj")
v_distrib <- c('ZIgaussian','bernoulli','poisson')

## ----param net----------------------------------------------------------------
list_theta <- list()
list_theta[[1]] <- list()
list_theta[[1]]$mean  <- matrix(c(6.1, 8.9, 6.6, 9.8, 2.6, 1.0), 3, 2)
list_theta[[1]]$var  <-  matrix(c(1.6, 1.6, 1.8, 1.7 ,2.3, 1.5),3, 2)
list_theta[[1]]$p0  <-  matrix(c(0.4, 0.1, 0.6, 0.5 , 0.2, 0),3, 2)
list_theta[[2]] <- matrix(c(0.7,1.0, 0.4, 0.6),2, 2)
m3 <- matrix(c(2.5, 2.6 ,2.2 ,2.2, 2.7 ,3.0 ,3.6, 3.5, 3.3),3,3 )
list_theta[[3]] <- (m3 + t(m3))/2# for symetrisation

## ----simul 2, eval = TRUE, echo = TRUE----------------------------------------
library(GREMLINS)
dataSim <- rMBM(v_NQ,E , typeInter, v_distrib, list_pi,
                list_theta, namesFG = namesFG, seed = 4,keepClassif  = TRUE)
list_Net <- dataSim$list_Net
length(list_Net)
names(list_Net[[1]])
list_Net[[1]]$typeInter
list_Net[[1]]$rowFG
list_Net[[1]]$colFG

## ----MBM simul, echo = TRUE, eval = TRUE--------------------------------------
res_MBMsimu <- multipartiteBM(list_Net, 
                              v_distrib = v_distrib, 
                              namesFG = c('A','B'),
                              v_Kinit = c(2,2),
                              nbCores = 2,
                              initBM = FALSE,
                              keep = FALSE)

## ----estim param, eval=TRUE---------------------------------------------------
res_MBMsimu$fittedModel[[1]]$paramEstim$list_theta$AB$mean

## ----extract cluster, eval=TRUE-----------------------------------------------
Cl <- extractClustersMBM(res_MBMsimu)

## ----MBM fixed, echo = TRUE, eval = TRUE--------------------------------------
res_MBMsimu_fixed <- multipartiteBMFixedModel(list_Net, v_distrib = v_distrib, nbCores = 2,namesFG = namesFG, v_K = c(3,2))
res_MBMsimu_fixed$fittedModel[[1]]$paramEstim$v_K
extractClustersMBM(res_MBMsimu_fixed)$A

## ----sim NA-------------------------------------------------------------------
############# NA data at random in any matrix
epsilon =  10/100
list_Net_NA <- list_Net
for (m in 1:nrow(E)){
   U <-  sample(c(1,0),v_NQ[E[m,1]]*v_NQ[E[m,2]],replace=TRUE,prob  = c(epsilon, 1-epsilon))
   matNA <- matrix(U,v_NQ[E[m,1]],v_NQ[E[m,2]])
   list_Net_NA[[m]]$mat[matNA== 1] = NA
   if (list_Net_NA[[m]]$typeInter == 'adj') {
     M <- list_Net_NA[[m]]$mat
     diag(M) <- NA
     M[lower.tri(M)] = t(M)[lower.tri(M)]
     list_Net_NA[[m]]$mat <- M
     }
}

## ----MBM simul NA, echo = TRUE, eval = TRUE-----------------------------------
res_MBMsimuNA <- multipartiteBM(list_Net_NA, 
                              v_distrib = v_distrib, 
                              namesFG = c('A','B'),
                              v_Kinit = c(2,2),
                              nbCores = 2,
                              keep = FALSE)

## ----MBM predict NA, echo = TRUE, eval = TRUE---------------------------------
pred <- predictMBM(res_MBMsimuNA)

