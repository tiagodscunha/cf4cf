source("auxiliary.R")
source("tuningCF.R")
source('NEW_ALS.R')


run_experiment_cf4cf <- function(type,maxzeroes,iterations){
  
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
  
  A <- read.csv("metafeatures_statistical/mf_final.csv", sep=";")
  C <- read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=",")
  
  if(type == "IR"){
    B <- read.csv("metafeatures_landmarkers/B_IR_final.csv", sep=";")  #landmarkers datasets have NAs
  }
  else{
    B <- read.csv("metafeatures_landmarkers/B_RP_final.csv", sep=";")
  }
  
  metafeatures <- mergeUnifiedDataset(A,B,C)
  
  targets_matrix <- createTargetRankings(
    merge(
      metafeatures,
      read.csv(targets_filename, sep=";"), 
      by.x="dataset",by.y="dataset")
  )
  
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
                 maxzeroes = maxzeroes)
  
  all <- c()
  
  for(t in 1:iterations){
    
    #cf4cf ratings
    cf4cf_als <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_matrix, 
                       rating_matrix_landmarkers=ratings_landmarkers, 
                       method="ALS", type="alors", operations = NULL, zeroes = 1,
                       maxzeroes = maxzeroes, metric = "ndcg",N=t)
    
    tmp <- c(
      alors=cf4cf_als
    )
    
    names(tmp) <- c(paste0("alors_",t))
    all <- c(all,tmp)
  }
  
  all
  
}


IR <- run_experiment_cf4cf("IR",4,3)
RP <- run_experiment_cf4cf("RP",8,5)

printResults(IR)
printResults(RP)

save(IR,RP,file="results/alors_ndcg_comp.Rda")
