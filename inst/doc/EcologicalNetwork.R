## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)

## ----loading dataset, eval=TRUE-----------------------------------------------
library(GREMLINS)
data(MPEcoNetwork, package = "GREMLINS")
names(MPEcoNetwork)

## ----transform dataset,  eval=TRUE--------------------------------------------
PlantFlovis = defineNetwork(MPEcoNetwork$Inc_plant_flovis,"inc","Plants","Flovis")
PlantAnt = defineNetwork(MPEcoNetwork$Inc_plant_ant,"inc","Plants","Ants")
PlantBird = defineNetwork(MPEcoNetwork$Inc_plant_bird,"inc","Plants","Birds")
list_net <- list(PlantFlovis,PlantAnt,PlantBird)
names(PlantFlovis)

## ----example of dataset, eval=TRUE--------------------------------------------
PlantFlovis$mat[1:2,1:2]


## ----MBM, echo = TRUE, eval = FALSE-------------------------------------------
#  RES_MBM = multipartiteBM(
#      list_Net = list(PlantFlovis, PlantAnt, PlantBird),
#      namesFG = c('Plants','Flovis','Ants','Birds'),
#      v_distrib  = c('bernoulli','bernoulli','bernoulli'),
#      initBM = TRUE,
#      keep = TRUE,
#      nbCores = 2)

## ----MBM load, echo = FALSE, eval = TRUE--------------------------------------
load(file='res_EcologicalNetwork.Rda')

## ----MBM what-----------------------------------------------------------------
names(RES_MBM)

## ----MBM v_K------------------------------------------------------------------
RES_MBM$fittedModel[[1]]$paramEstim$v_K

## ----MBM param----------------------------------------------------------------
RES_MBM$fittedModel[[1]]$paramEstim$list_pi$Plants
RES_MBM$fittedModel[[1]]$paramEstim$list_theta$PlantsFlovis

## ----MBM Z--------------------------------------------------------------------
table(RES_MBM$fittedModel[[1]]$paramEstim$Z$Plants)
table(RES_MBM$fittedModel[[1]]$paramEstim$Z$Ants)      

## ----plot, eval = TRUE--------------------------------------------------------
plotMBM(RES_MBM,mycol= c("darkolivegreen3", "gold", "tomato","cadetblue3")) 

