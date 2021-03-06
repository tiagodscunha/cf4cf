source("auxiliary.R")

run_experiment_lr <- function(type,goal){
  
  A <- read.csv("metafeatures_statistical/mf_final.csv", sep=";")
  C <- read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=",")
  
  if(type == "IR"){
    B <- read.csv("metafeatures_landmarkers/B_IR_final.csv", sep=";")  #landmarkers datasets have NAs
  }
  else{
    B <- read.csv("metafeatures_landmarkers/B_RP_final.csv", sep=";")
  }
  
  metafeatures <- mergeUnifiedDataset(A,B,C)  
  
  targets_filename <- ""
  if(type == "IR"){
    targets_filename <- "targets/IR.csv"
  }
  else {
    targets_filename <- "targets/RP.csv"
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
  
  #REMOVER COLUNA DATASET!!!!
  rownames(metafeatures) <- metafeatures$dataset
  metafeatures$dataset <- NULL
  
  performance_knn <- LOOCV(data=metafeatures,targets=targets_matrix, method="kNN", type="mtl",perf=perf,goal=goal)
  performance_rt <- LOOCV(data=metafeatures,targets=targets_matrix, method="RT", type="mtl",perf=perf,goal=goal)
  performance_rfr <- LOOCV(data=metafeatures,targets=targets_matrix, method="RFR", type="mtl",perf=perf,goal=goal)
  performance_avg <- LOOCV(data=metafeatures,targets=targets_matrix, method="baseline", algorithms=sort(colnames(targets_matrix)), type="mtl",perf=perf,goal=goal)
  
  c(
    knn=performance_knn,
    rt = performance_rt,
    rfr = performance_rfr,
    avg=performance_avg
  )
}

IR <- run_experiment_lr("IR","max")
RP <- run_experiment_lr("RP","min")

printResults(IR)
printResults(RP)

save(IR,RP,file="results/meta_lr_comp_base.Rda")



