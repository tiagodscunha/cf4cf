source("auxiliary.R")

run_experiment_lr <- function(type){
  
  metafeatures <- read.csv("metafeatures_graph/graph_metafeatures_final.csv",sep=",")
  
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
  
  #REMOVER COLUNA DATASET!!!!
  rownames(metafeatures) <- metafeatures$dataset
  metafeatures$dataset <- NULL
  
  performance_knn <- LOOCV(data=metafeatures,targets=targets_matrix, method="kNN", type="mtl")
  performance_rt <- LOOCV(data=metafeatures,targets=targets_matrix, method="RT", type="mtl")
  performance_rfr <- LOOCV(data=metafeatures,targets=targets_matrix, method="RFR", type="mtl")
  performance_avg <- LOOCV(data=metafeatures,targets=targets_matrix, method="baseline", algorithms=sort(colnames(targets_matrix)), type="mtl")
  
  c(
    knn=performance_knn,
    rt = performance_rt,
    rfr = performance_rfr,
    avg=performance_avg
  )
}

IR <- run_experiment_lr("IR")
RP <- run_experiment_lr("RP")

printResults(IR)
printResults(RP)


save(IR,RP,file="results/meta_lr_gr.Rda")



