############################################################################################################
######################################   KENDALL'S TAU GRAPHICS  ###########################################
############################################################################################################


library(ggplot2)
library(reshape2)

createSimpleGraphic <- function(tmp){

  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")

  g <- ggplot(tmp, aes(x=algorithm,y=performance,group=algorithm,fill=algorithm)) +
    scale_fill_manual(values=cbPalette) +
    geom_bar(stat = "identity", width = 0.7, position = "dodge") +
    guides(fill = guide_legend(title = "Meta-algorithms"))+
    facet_grid(. ~ strategy) +
    ylab("Kendall's tau") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(text= element_text(size = 16)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  g

}


load("results/meta_lr_comp.Rda")
LR_IR <- unlist(IR$rfr)
LR_RP <- unlist(RP$rfr)

AVG_IR <- unlist(IR$avg)
AVG_RP <- unlist(RP$avg)

load("results/meta_cf4cf.Rda")
CF4CF_IR <- unlist(IR$cf4cf_als_1)
CF4CF_RP <- unlist(RP$cf4cf_als_1)

load("results/cf4cf_meta_mf_knn.Rda")
CF4CF_META_IR <- unlist(IR$cf4cf_meta_knn_1)
CF4CF_META_RP <- unlist(RP$cf4cf_meta_knn_1)

load("results/alors_comp.Rda")
ALORS_IR <- unlist(IR$alors)
ALORS_RP <- unlist(RP$alors)

load("results/meta_aslib_comp.Rda")
ASLIB_IR <- unlist(IR$rknn)
ASLIB_RP <- unlist(RP$rknn)

rm(IR,RP)

tmp_ir <- data.frame(
  AVG = AVG_IR,
  LR = LR_IR,
  CF4CF = CF4CF_IR,
  CF4CF_META = CF4CF_META_IR,
  ALORS = ALORS_IR,
  ASLIB = ASLIB_IR
)

tmp_rp <- data.frame(
  AVG = AVG_RP,
  LR = LR_RP,
  CF4CF = CF4CF_RP,
  CF4CF_META = CF4CF_META_RP,
  ALORS = ALORS_RP,
  ASLIB = ASLIB_RP
)

#kendall's tau
new_tmp_ir <- apply(tmp_ir,2,mean)
new_tmp_ir <- melt(new_tmp_ir)
new_tmp_ir$metatarget <- "IR"
new_tmp_ir$algorithm <- rownames(new_tmp_ir)

new_tmp_rp <- apply(tmp_rp,2,mean)
new_tmp_rp <- melt(new_tmp_rp)
new_tmp_rp$metatarget <- "RP"
new_tmp_rp$algorithm <- rownames(new_tmp_rp)

new_tmp <- rbind(new_tmp_ir,new_tmp_rp)
colnames(new_tmp) <- c("performance","strategy","algorithm")
createSimpleGraphic(new_tmp)


############################################################################################################
####################################   KENDALL'S TAU CD DIAGRAM  ###########################################
############################################################################################################

library(scmamp)
plotCD (rbind(tmp_ir,tmp_rp), alpha=0.05, cex=1.25)


############################################################################################################
##########################################   NDCG GRAPHICS  ################################################
############################################################################################################

load("results/meta_lr_comp_ndcg.Rda")
LR_IR_1 <- unlist(IR$RFR_1)
LR_IR_2 <- unlist(IR$RFR_2)
LR_IR_3 <- unlist(IR$RFR_3)

LR_RP_1 <- unlist(RP$RFR_1)
LR_RP_3 <- unlist(RP$RFR_3)
LR_RP_5 <- unlist(RP$RFR_5)

load("results/meta_lr_avg_ndcg.Rda")

AVG_IR_1 <- unlist(IR$baseline_1)
AVG_IR_2 <- unlist(IR$baseline_2)
AVG_IR_3 <- unlist(IR$baseline_3)

AVG_RP_1 <- unlist(RP$baseline_1)
AVG_RP_3 <- unlist(RP$baseline_3)
AVG_RP_5 <- unlist(RP$baseline_5)

load("results/meta_cf4cf_ndcg.Rda")
CF4CF_IR_1 <- unlist(IR$cf4cf_als_1)
CF4CF_IR_2 <- unlist(IR$cf4cf_als_2)
CF4CF_IR_3 <- unlist(IR$cf4cf_als_3)

CF4CF_RP_1 <- unlist(RP$cf4cf_als_1)
CF4CF_RP_3 <- unlist(RP$cf4cf_als_3)
CF4CF_RP_5 <- unlist(RP$cf4cf_als_5)

load("results/cf4cf_meta_mf_ndcg_knn.Rda")
CF4CF_META_IR_1 <- unlist(IR$cf4cf_meta_knn_1)
CF4CF_META_IR_2 <- unlist(IR$cf4cf_meta_knn_2)
CF4CF_META_IR_3 <- unlist(IR$cf4cf_meta_knn_3)

CF4CF_META_RP_1 <- unlist(RP$cf4cf_meta_knn_1)
CF4CF_META_RP_3 <- unlist(RP$cf4cf_meta_knn_3)
CF4CF_META_RP_5 <- unlist(RP$cf4cf_meta_knn_5)

load("results/alors_ndcg_comp.Rda")
ALORS_IR_1 <- unlist(IR$alors_1)
ALORS_IR_2 <- unlist(IR$alors_2)
ALORS_IR_3 <- unlist(IR$alors_3)

ALORS_RP_1 <- unlist(RP$alors_1)
ALORS_RP_3 <- unlist(RP$alors_3)
ALORS_RP_5 <- unlist(RP$alors_5)

load("results/meta_aslib_comp_ndcg.Rda")
ASLIB_IR_1 <- unlist(IR$rknn_1)
ASLIB_IR_2 <- unlist(IR$rknn_2)
ASLIB_IR_3 <- unlist(IR$rknn_3)

ASLIB_RP_1 <- unlist(RP$rknn_1)
ASLIB_RP_3 <- unlist(RP$rknn_3)
ASLIB_RP_5 <- unlist(RP$rknn_5)

rm(IR,RP)

ndcg_1_ir <- data.frame(
    AVG = AVG_IR_1,
    LR = LR_IR_1,
    CF4CF = CF4CF_IR_1,
    CF4CF_META = CF4CF_META_IR_1,
    ALORS = ALORS_IR_1,
    ASLIB = ASLIB_IR_1
)

ndcg_2_ir <- data.frame(
  AVG = AVG_IR_2,
  LR = LR_IR_2,
  CF4CF = CF4CF_IR_2,
  CF4CF_META = CF4CF_META_IR_2,
  ALORS = ALORS_IR_2,
  ASLIB = ASLIB_IR_2
)

ndcg_3_ir <- data.frame(
  AVG = AVG_IR_3,
  LR = LR_IR_3,
  CF4CF = CF4CF_IR_3,
  CF4CF_META = CF4CF_META_IR_3,
  ALORS = ALORS_IR_3,
  ASLIB = ASLIB_IR_3
)


ndcg_1_rp <- data.frame(
  AVG = AVG_RP_1,
  LR = LR_RP_1,
  CF4CF = CF4CF_RP_1,
  CF4CF_META = CF4CF_META_RP_1,
  ALORS = ALORS_RP_1,
  ASLIB = ASLIB_RP_1
)

ndcg_3_rp <- data.frame(
  AVG = AVG_RP_3,
  LR = LR_RP_3,
  CF4CF = CF4CF_RP_3,
  CF4CF_META = CF4CF_META_RP_3,
  ALORS = ALORS_RP_3,
  ASLIB = ASLIB_RP_3
)

ndcg_5_rp <- data.frame(
  AVG = AVG_RP_5,
  LR = LR_RP_5,
  CF4CF = CF4CF_RP_5,
  CF4CF_META = CF4CF_META_RP_5,
  ALORS = ALORS_RP_5,
  ASLIB = ASLIB_RP_5
)

processDF <- function(dt,tg,th){
  tmp <- as.data.frame(t(t(apply(dt,2,mean))))
  tmp$algorithm <- rownames(tmp)
  rownames(tmp) <- NULL
  colnames(tmp) <- c("performance","algorithm")
  tmp$metatarget <- tg
  tmp$threshold <- th
  tmp
}

ndcg_1_ir <- processDF(ndcg_1_ir,"IR","NDCG@1")
ndcg_2_ir <- processDF(ndcg_2_ir,"IR","NDCG@2")
ndcg_3_ir <- processDF(ndcg_3_ir,"IR","NDCG@3")

ndcg_1_rp <- processDF(ndcg_1_rp,"RP","NDCG@1")
ndcg_3_rp <- processDF(ndcg_3_rp,"RP","NDCG@3")
ndcg_5_rp <- processDF(ndcg_5_rp,"RP","NDCG@5")

normalizeDF <- function(dt){
 #dt$performance <- dt$performance - dt[which(dt$algorithm == "AVG"),]$performance  #UNUSED
  dt
}

all_ir <- rbind(normalizeDF(ndcg_1_ir),normalizeDF(ndcg_2_ir),normalizeDF(ndcg_3_ir))

all_rp <- rbind(normalizeDF(ndcg_1_rp),normalizeDF(ndcg_3_rp),normalizeDF(ndcg_5_rp))

createSimpleGraphicNDCG <- function(tmp){

  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")

    g <- ggplot(tmp, aes(x=algorithm,y=performance,group=algorithm,fill=algorithm)) +
    scale_fill_manual(values=cbPalette) +
    geom_bar(stat = "identity", width = 0.7, position = "dodge") +
    guides(fill = guide_legend(title = "Meta-algorithms"))+
    facet_grid(metatarget ~ threshold) +
    ylab("NDCG") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(text= element_text(size = 16)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  g

}

library(ggplot2)
g <- createSimpleGraphicNDCG(all_ir)
g

g1 <- createSimpleGraphicNDCG(all_rp)
g1


############################################################################################################
############################   IMPACT ON THE BASELEVEL PERFORMANCE GRAPHICS  ###############################
############################################################################################################

load("results/meta_lr_comp_base.Rda")
LR_IR <- unlist(IR$rfr)
LR_RP <- unlist(RP$rfr)

AVG_IR <- unlist(IR$avg)
AVG_RP <- unlist(RP$avg)

load("results/meta_cf4cf_base.Rda")
CF4CF_IR <- unlist(IR$als)
CF4CF_RP <- unlist(RP$als)

load("results/cf4cf_meta_mf_base.Rda")
CF4CF_META_IR <- unlist(IR$knn)
CF4CF_META_RP <- unlist(RP$knn)

load("results/alors_comp_base.Rda")
ALORS_IR <- unlist(IR$alors)
ALORS_RP <- unlist(RP$alors)

load("results/meta_aslib_comp_base.Rda")
ASLIB_IR <- unlist(IR$rknn)
ASLIB_RP <- unlist(RP$rknn)

rm(IR,RP)

normalizeDF <- function(dt,inverted){

  data_graph <- as.data.frame(apply(dt,1,function(t){
    unlist(lapply(t,function(y){

      if(inverted){
        res <- (- (y - t[which(names(t) == "AVG")]))*100
      }
      else{
        res <- (y - t[which(names(t) == "AVG")])*100
      }
      res
    }))
  }))

  row.names(data_graph) <- colnames(dt)

  data_graph
}

tmp_ir <- data.frame(
  AVG = AVG_IR,
  LR = LR_IR,
  CF4CF = CF4CF_IR,
  CF4CF_META = CF4CF_META_IR,
  ALORS = ALORS_IR,
  ASLIB = ASLIB_IR
)

tmp_rp <- data.frame(
  AVG = AVG_RP,
  LR = LR_RP,
  CF4CF = CF4CF_RP,
  CF4CF_META = CF4CF_META_RP,
  ALORS = ALORS_RP,
  ASLIB = ASLIB_RP
)

make_graph_base <- function(data){

  cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")

  data<- data[which(data$algorithm != "AVG"),]

  ggplot(data=data, aes(x=threshold, y=performance, group=algorithm, color=algorithm)) +
    geom_line() +
    geom_point() +
    xlab("Algorithms") + ylab("Lift (%)") +
    facet_grid(. ~ problem, scales = "free") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(text= element_text(size = 16)) +
    theme(legend.title=element_blank()) +
    scale_color_manual(values = cbPalette) +
    theme(axis.title.x=element_blank())


}

tmp_ir <- normalizeDF(t(t(tmp_ir)),F)
colnames(tmp_ir) <- c(1:5)
tmp_ir$algorithm <- rownames(tmp_ir)
tmp_ir <- melt(tmp_ir)
colnames(tmp_ir) <- c("algorithm","threshold","performance")
tmp_ir$problem <- "IR"

tmp_rp <- normalizeDF(t(t(tmp_rp)),T)
colnames(tmp_rp) <- c(1:9)
tmp_rp$algorithm <- rownames(tmp_rp)
tmp_rp <- melt(tmp_rp)
colnames(tmp_rp) <- c("algorithm","threshold","performance")
tmp_rp$problem <- "RP"


make_graph_base(rbind(tmp_ir,tmp_rp))

############################################################################################################
######################################   METAFEATURE IMPORTANCE  ###########################################
############################################################################################################


source("auxiliary.R")
source("tuningCF.R")


removeCFS <- function(df,threshold){
  library(caret)
  
  corr_m <- cor(df)
  corr_m[is.na(corr_m)] <- 0
  
  
  toRemove <- findCorrelation(corr_m, cutoff = threshold)
  print(length(toRemove))
  df1 <- as.data.frame(df[,-(which(colnames(df) %in% colnames(df)[toRemove]))])
  
  print("metafeatures removed due to CFS")
  print(setdiff(colnames(df),colnames(df1)))
  
  df1
  
}


replaceNA <- function(df){
  df <- apply(df,2,function(x){
    x[which(is.na(x))] <- mean(x, na.rm = T)
    x
  })
}

normalizeMatrix <- function(df){
  df1 <- as.data.frame(normalize(df))
  colnames(df1) <- colnames(df)
  
  tmp <- data.frame(dataset=rownames(df))
  df1 <- cbind(tmp,df1)
  df1
}

mergeUnifiedDataset <- function(A,B,C){
  
  library(data.table) ## 1.9.3
  library(splitstackshape)
  library(plyr)
  library(BBmisc)
  
  D1 <- merge(A,B,by.x="dataset",by.y="dataset")
  df <- merge(D1,C,by.x="dataset",by.y="dataset")
  
  rownames(df) <- df$dataset
  df$dataset <- NULL
  df <- removeCFS(df,0.9)
  df <- normalizeMatrix(df)
  
  df
}


run_experiment_cf4cf <- function(type){
  
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
  
  metafeatures <- read.csv("metafeatures_statistical/mf_final.csv", sep=";")
 
  
  targets_matrix <- createTargetRankings(
    merge(
      metafeatures,
      read.csv(targets_filename, sep=";"), 
      by.x="dataset",by.y="dataset")
  )
  
  #cf4cf ratings
  model <- trainModelVarIMp(metafeatures,ratings_landmarkers,targets_matrix)
  
  model
}


IR <- run_experiment_cf4cf("IR")
RP <- run_experiment_cf4cf("RP")


print(IR)
print(RP)


averageResults <- function(data){
  
  metafeatures <- unique(data$metafeatures)
  avg_rank <- unlist(lapply(metafeatures, function(mf){
    ranks <- data[data$metafeatures == mf,]$rank
    mean(ranks)
  }))
  
  tmp <- data.frame(
    metafeatures = metafeatures,
    rank = avg_rank
  )
  
  tmp <- tmp[order(tmp$rank),]
  tmp
}


IR <- averageResults(IR)
RP <- averageResults(RP)

final <- data.frame(
  order = c(1:10),
  IR = IR$metafeatures[1:10],
  RP = RP$metafeatures[1:10]
)


A <- IR[which(IR$rank < 15),]

A$metafeatures <- as.character(A$metafeatures)
A[which(A$metafeatures == "colMeans_entropy"),]$metafeatures <- "I.mean.entropy"
A[which(A$metafeatures == "colSums_skewness"),]$metafeatures <- "I.sum.skew"
A[which(A$metafeatures == "colSums_entropy"),]$metafeatures <- "I.sum.entropy"
A[which(A$metafeatures == "MostPopular"),]$metafeatures <- "MP"


g1 <- ggplot(A,aes(x=metafeatures,y=rank)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  ggtitle("IR")
g1

B <- RP[which(RP$rank < 17),]

B$metafeatures <- as.character(B$metafeatures)
B[which(B$metafeatures == "colMeans_entropy"),]$metafeatures <- "I.mean.entropy"
B[which(B$metafeatures == "colMeans_kurtosis"),]$metafeatures <- "I.mean.kurtosis"
B[which(B$metafeatures == "colMeans_mode"),]$metafeatures <- "I.mean.mode"
B[which(B$metafeatures == "colCounts_min"),]$metafeatures <- "I.count.min"
B[which(B$metafeatures == "colSums_skewness"),]$metafeatures <- "I.sum.skew"


g2 <- ggplot(B,aes(x=metafeatures,y=rank)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  ggtitle("RP")
g2


library(grid)
library(gridExtra)
grid.arrange(arrangeGrob(g1,g2,nrow = 1, ncol=2,left=textGrob("Metafeature importance (average rank)", rot = 90, vjust = 1)) )




