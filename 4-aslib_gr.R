library(mlr)
source("auxiliary.R")
configureMlr(on.par.without.desc="quiet")


library(llama)
library(recommenderlab)
library(ParamHelpers)


run_experiment_lr <- function(type){
  
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
  
  features <- as.data.frame(read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=","))
  
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
  
  performance_lm <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = lm)
  performance_xgboost <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = xgboost)
  performance_svm <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = svm)
  performance_rf_ranger <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = rf_ranger)
  performance_rpart <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = rpart)
  performance_rknn <- LOOCV(data=metafeatures,targets=targets_matrix, method = "ASLIB", type="ASLIB", predictions = rknn)
  
  c(
    lm=performance_lm,
    xgboost=performance_xgboost,
    svm=performance_svm,
    rrf=performance_rf_ranger,
    rpart=performance_rpart,
    rknn=performance_rknn
  )
}

IR <- run_experiment_lr("IR")
RP <- run_experiment_lr("RP")

printResults(IR)
printResults(RP)


save(IR,RP,file="results/meta_aslib_gr.Rda")
