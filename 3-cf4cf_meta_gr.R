source("auxiliary.R")
source("tuningCF.R")


run_experiment_cf4cf_meta <- function(type,maxzeroes){

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
  
  metafeatures <- read.csv("metafeatures_graph/graph_metafeatures_final.csv",sep=",")
  
  targets_matrix <- createTargetRankings(
    merge(
      metafeatures,
      read.csv(targets_filename, sep=";"), 
      by.x="dataset",by.y="dataset")
  )

  #cf4cf ratings
  cf4cf_meta_knn <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_landmarkers, 
                          rating_matrix_landmarkers=ratings_landmarkers, 
                          method="kNN", type="hyb", operations = c("sampling","adjustmentRating","contextual"), zeroes = 1,
                          maxzeroes = maxzeroes)
  cf4cf_meta_rt <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_landmarkers, 
                         rating_matrix_landmarkers=ratings_landmarkers, 
                         method="RT", type="hyb", operations = c("sampling","adjustmentRating","contextual"), zeroes = 1,
                         maxzeroes = maxzeroes)
  cf4cf_meta_rfr <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_landmarkers, 
                          rating_matrix_landmarkers=ratings_landmarkers, 
                          method="RFR", type="hyb", operations = c("sampling","adjustmentRating","contextual"), zeroes = 1,
                          maxzeroes = maxzeroes)
  
  c(
    decomposeResults(cf4cf_meta_knn,"cf4cf_meta_knn",maxzeroes),
    decomposeResults(cf4cf_meta_rt,"cf4cf_meta_rt",maxzeroes),
    decomposeResults(cf4cf_meta_rfr,"cf4cf_meta_rfr",maxzeroes)
  )
}


IR <- run_experiment_cf4cf_meta("IR",4)
RP <- run_experiment_cf4cf_meta("RP",8)

printResults(IR)
printResults(RP)

save(IR,RP,file="results/cf4cf_meta_gr.Rda")

