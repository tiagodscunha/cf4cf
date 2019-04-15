library(mlr)
source("auxiliary.R")

library(llama)
library(recommenderlab)
library(ParamHelpers)
library(rrecsys)

configureMlr(on.par.without.desc="quiet")

run_experiment_lr <- function(type,iterations){
  
  metafeatures <- as.data.frame(read.csv("metafeatures_statistical/mf_final.csv", sep=";"))
  
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
  
  ################### BUILD ALL PREDICTIONS (bypass LOOCV)
  
  A <- read.csv("metafeatures_statistical/mf_final.csv", sep=";")
  C <- read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=",")
  
  if(type == "IR"){
    B <- read.csv("metafeatures_landmarkers/B_IR_final.csv", sep=";")  #landmarkers datasets have NAs
  }
  else{
    B <- read.csv("metafeatures_landmarkers/B_RP_final.csv", sep=";")
  }
  
  features <- mergeUnifiedDataset(A,B,C)
  
  if(type == "IR"){
    performances <- read.csv("targets/IR.csv", sep=";")
  }
  else {
    performances <- read.csv("targets/RP.csv", sep=";")
  }
  
  res <- apply(performances, 1, function(row){
    data.frame(
      dataset <- row[1],
      algorithm <- unlist(strsplit(toString(row[2]),",")),
      performance <- as.numeric(unlist(strsplit(toString(row[4]),","))),
      row.names = NULL
    )
  })
  
  all_perf <- do.call(rbind,res)
  colnames(all_perf) <- c("dataset","algorithm","performance")
  performance <- as.data.frame(as(as(all_perf,"realRatingMatrix"), "matrix"))
  
  performance[is.na(performance)] = 0
  performance$dataset <- rownames(performance)
  
  rm(all_perf,performances,res)
  
  mtl_data <- input(
    features = features, 
    performances = performance,
    minimize = F)
  
  folds = cvFolds(mtl_data, nfolds = -1L)
  
 
  
  lm <- predictionsToRankings(tuneLearner(mtl_data,"regr.lm")$predictions)
  xgboost <- predictionsToRankings(tuneLearner(mtl_data,"regr.xgboost")$predictions)
  svm <- predictionsToRankings(tuneLearner(mtl_data,"regr.svm")$predictions)
  rf_ranger <- predictionsToRankings(tuneLearner(mtl_data,"regr.ranger")$predictions)
  rpart <- predictionsToRankings(tuneLearner(mtl_data,"regr.rpart")$predictions)
  rknn <- predictionsToRankings(tuneLearner(mtl_data,"regr.rknn")$predictions)
  

  ###################################
  
  all <- c()
  
  for(t in iterations){
    
    performance_lm <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = lm, metric = "ndcg",N=t)
    performance_xgboost <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = xgboost, metric = "ndcg", N=t)
    performance_svm <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = svm, metric = "ndcg", N=t)
    performance_rf_ranger <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = rf_ranger, metric = "ndcg",N=t)
    performance_rpart <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = rpart, metric = "ndcg",N=t)
    performance_rknn <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = rknn, metric = "ndcg",N=t)
  
    tmp <- c(
      lm=performance_lm,
      xgboost=performance_xgboost,
      svm=performance_svm,
      rrf=performance_rf_ranger,
      rpart=performance_rpart,
      rknn=performance_rknn
    )
    names(tmp) <- c(paste0("lm_",t),paste0("xgboost_",t),paste0("svm_",t),paste0("rrf_",t),paste0("rpart_",t),paste0("rknn_",t))
    
    all <- c(all,tmp)
  
  }
  
  all
}

IR <- run_experiment_lr("IR",iterations=c(1:3))
RP <- run_experiment_lr("RP",iterations=c(1:5))

printResults(IR)
printResults(RP)


save(IR,RP,file="results/meta_aslib_comp_ndcg.Rda")