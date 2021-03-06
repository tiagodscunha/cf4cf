
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

  #cf4cf ratings
  cf4cf_meta_knn <- createExperiment(metafeatures,targets_matrix,ratings_landmarkers,ratings_landmarkers,"kNN","hyb",c("sampling","adjustmentRating","contextual"),maxzeroes)
  cf4cf_meta_rt <- createExperiment(metafeatures,targets_matrix,ratings_landmarkers,ratings_landmarkers,"RT","hyb",c("sampling","adjustmentRating","contextual"),maxzeroes)
  cf4cf_meta_rfr <- createExperiment(metafeatures,targets_matrix,ratings_landmarkers,ratings_landmarkers,"RFR","hyb",c("sampling","adjustmentRating","contextual"),maxzeroes)
  
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


save(IR,RP,file="results/cf4cf_meta_comp.Rda")

