
source("auxiliary.R")
source("tuningCF.R")


run_experiment_cf4cf <- function(type,maxzeroes,goal){
  
  metafeatures <- read.csv("metafeatures_statistical/mf_final.csv",sep=";")
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
  
  #cf4cf ratings
  cf4cf_ubcf <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_matrix, 
                      rating_matrix_landmarkers=ratings_landmarkers, 
                      method="UBCF", type="cf4cf", operations = c("sampling","adjustmentRating","convertRatings"), zeroes = 1,
                      maxzeroes = maxzeroes, perf = perf,goal=goal)
  cf4cf_als <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_matrix, 
                     rating_matrix_landmarkers=ratings_landmarkers, 
                     method="ALS", type="cf4cf", operations = c("sampling","adjustmentRating","convertRatings"), zeroes = 1,
                     maxzeroes = maxzeroes, perf = perf,goal=goal)
  c(
    ubcf=cf4cf_ubcf,
    als =cf4cf_als
  )
}


IR <- run_experiment_cf4cf("IR",4,"max")
RP <- run_experiment_cf4cf("RP",8,"min")

printResults(IR)
printResults(RP)


save(IR,RP,file="results/meta_cf4cf_base.Rda")