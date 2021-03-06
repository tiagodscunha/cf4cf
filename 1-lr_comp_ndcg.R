source("auxiliary.R")

run_experiment_lr <- function(type,iterations){
  
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
  
  #REMOVER COLUNA DATASET!!!!
  rownames(metafeatures) <- metafeatures$dataset
  metafeatures$dataset <- NULL
  
  all <- c()
  
  for(t in 1:iterations){
    
    performance_knn <- LOOCV(data=metafeatures,targets=targets_matrix, method="kNN", type="mtl",metric = "ndcg",N=t)
    performance_rt <- LOOCV(data=metafeatures,targets=targets_matrix, method="RT", type="mtl",metric = "ndcg",N=t)
    performance_rfr <- LOOCV(data=metafeatures,targets=targets_matrix, method="RFR", type="mtl",metric = "ndcg",N=t)
    performance_avg <- LOOCV(data=metafeatures,targets=targets_matrix, method="baseline", algorithms=sort(colnames(targets_matrix)), type="mtl",metric = "ndcg",N=t)
    
    tmp <- c(
      knn=performance_knn,
      rt = performance_rt,
      rfr = performance_rfr,
      avg=performance_avg
    )
    names(tmp) <- c(paste0("kNN_",t),paste0("RT_",t),paste0("RFR_",t),paste0("baseline_",t))
    
    all <- c(all,tmp)
  }
  all
}

IR <- run_experiment_lr("IR",3)
RP <- run_experiment_lr("RP",5)

printResults(IR)
printResults(RP)

save(IR,RP,file="results/meta_lr_comp_ndcg.Rda")


