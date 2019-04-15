library(mlr)
source("auxiliary.R")
configureMlr(on.par.without.desc="quiet")

run_experiment_cf4cf <- function(type,maxzeroes,goal){
  
  targets_filename <- ""
  if(type == "IR"){
    targets_filename <- "targets/IR.csv"
    ratings_matrix <- read.csv("targets/IR_corrected.csv", sep=";", col.names = c("dataset","algorithm","rating"))
    ratings_landmarkers <- read.csv("targets/IR_landmarkers_corrected.csv", sep=";", col.names = c("dataset","algorithm","rating"))
  }
  else {
    targets_filename <- "targets/RP.csv"
    ratings_matrix <- read.csv("targets/RP_corrected.csv", sep=";", col.names = c("dataset","algorithm","rating"))
    ratings_landmarkers <- read.csv("targets/RP_landmarkers_corrected.csv", sep=";", col.names = c("dataset","algorithm","rating"))
  }
  
  metafeatures <- read.csv("metafeatures_statistical/mf_final.csv",sep=";")
  targets_matrix <- createTargetRankings(
    merge(
      metafeatures,
      read.csv(targets_filename, sep=";"), 
      by.x="dataset",by.y="dataset")
  )
  
  performance <- read.csv(targets_filename, sep=";")
  performance <- performance[,c(1,4)]
  colnames(performance) <- c("dataset","ranking")
  perf <- createTargetRankingsPerf(metafeatures,performance,targets_matrix)
  
  #construir multioutput regressor para mapear metafeatures para dataset latent representation
  # regressor -> ‘MultivariateRandomForest’
  # metafeatures -> variar mf, sl, gr, comp
  # obter factorized matrices from ALS (UBCF não pode ser usado)
  # https://github.com/cran/recommenderlab/blob/4f04bdcb4934355efb0548a6d874cfe431dd6ae2/R/RECOM_ALS.R -> obter U e M (custom recommenderlab ou novo algoritmo???)
  # map(metafeatures -> user latent space) matrix.multiplication item latent space = prediction
  
  #cf4cf ratings
  alors <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_matrix, 
                 rating_matrix_landmarkers=ratings_landmarkers, 
                 method="ALS", type="alors", operations = NULL, zeroes = 1,
                 maxzeroes = maxzeroes,  perf = perf,goal=goal)
  
  c(
    alors = alors
  )
  
}


IR <- run_experiment_cf4cf("IR",4,"max")
RP <- run_experiment_cf4cf("RP",8,"min")

printResults(IR)
printResults(RP)

save(IR,RP,file="results/alors_mf_base.Rda")
