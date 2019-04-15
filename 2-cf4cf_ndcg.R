
source("auxiliary.R")
source("tuningCF.R")


run_experiment_cf4cf <- function(type,maxzeroes,iterations){ 
  
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
  
  all <- c()
  
  for(t in 1:iterations){
  
      #cf4cf ratings
      cf4cf_ubcf <- createExperiment(metafeatures,targets_matrix,ratings_matrix,ratings_landmarkers,"UBCF","cf4cf",c("sampling","adjustmentRating","convertRatings"),maxzeroes=maxzeroes,metric = "ndcg",N=t)
      cf4cf_als <- createExperiment(metafeatures,targets_matrix,ratings_matrix,ratings_landmarkers,"ALS","cf4cf",c("sampling","adjustmentRating","convertRatings"),maxzeroes=maxzeroes,metric = "ndcg",N=t)
    
      
      tmp <- c(
        ubcf=cf4cf_ubcf,
        als=cf4cf_als
      )
      
      names(tmp) <- c(paste0("cf4cf_ubcf_",t),
                      paste0("cf4cf_als_",t)
                      )
      
      all <- c(all,tmp)
  }
  all
}


IR <- run_experiment_cf4cf("IR",4,3)
RP <- run_experiment_cf4cf("RP",8,5)

printResults(IR)
printResults(RP)


save(IR,RP,file="results/meta_cf4cf_ndcg.Rda")