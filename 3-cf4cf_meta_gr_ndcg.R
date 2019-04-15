source("auxiliary.R")
source("tuningCF.R")

run_experiment_cf4cf_meta <- function(type,maxzeroes,iterations){

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
  
  all <- c()
  
  for(t in 1:iterations){

    #cf4cf ratings
    cf4cf_meta_knn <- createExperiment(metafeatures,targets_matrix,ratings_landmarkers,ratings_landmarkers,"kNN","hyb",c("sampling","adjustmentRating","contextual"),maxzeroes=maxzeroes,metric = "ndcg",N=t)
    cf4cf_meta_rt <- createExperiment(metafeatures,targets_matrix,ratings_landmarkers,ratings_landmarkers,"RT","hyb",c("sampling","adjustmentRating","contextual"),maxzeroes=maxzeroes,metric = "ndcg",N=t)
    cf4cf_meta_rfr <- createExperiment(metafeatures,targets_matrix,ratings_landmarkers,ratings_landmarkers,"RFR","hyb",c("sampling","adjustmentRating","contextual"),maxzeroes=maxzeroes,metric = "ndcg",N=t)
    
    c(
      ccf4cf_meta_knn = cf4cf_meta_knn,
      cf4cf_meta_rt = cf4cf_meta_rt,
      cf4cf_meta_rfr = cf4cf_meta_rfr
    )
    all <- c(all,tmp)
  }
  all
}


IR <- run_experiment_cf4cf_meta("IR",4,3)
RP <- run_experiment_cf4cf_meta("RP",8,5)

printResults(IR)
printResults(RP)


save(IR,RP,file="results/cf4cf_meta_gr_ndcg.Rda")

