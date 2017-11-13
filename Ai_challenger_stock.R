# setwd("C:\\Users\\TEMP.Main.001\\Documents\\GitHub\\CatBoostRepository\\catboost-master\\catboost\\R-package")
# install.packages('devtools')
# devtools::build()
# devtools::install()



setwd("C:\\Users\\TEMP.Main.001\\Documents\\GitHub\\AI_china\\stock")
# install.packages("ggraph")
# library(rattle)
# rattle()

# Importing the dataset
dataset <- read.csv('stock_train_data_20171111.csv')
# dataset = dataset[4:25]
t_set <- read.csv('stock_test_data_20171111.csv')
t_set_id <- t_set$id


# feature importance
# head(dataset)
data2 = dataset[,2:105]
corMat <- cor(data2, use = "pairwise.complete.obs")
# View(round(corMat, 2))
library("qgraph")
corMat <- cor_auto(dataset) # Correlate data
Graph_pcor <- qgraph(corMat, graph = "pcor", layout = "spring")
Graph_pcor <- qgraph(corMat, graph = "pcor", layout = "spring", threshold = "bonferroni",
                     sampleSize = nrow(Data2), alpha = 0.05)
Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
                      sampleSize = nrow(Data2))
centRes <- centrality(Graph_lasso)
# Node strength (degree):
centRes$OutDegree # Or InDegree, it's the same in unweighted networks
# Closeness:
centRes$Closeness
# Betweenness:
centRes$Betweenness
centralityPlot(Graph_lasso)
centralityPlot(GGM = list(unregularized = Graph_pcor, regularized = Graph_lasso),
               Ising = list(unregularized = Graph_Ising1, regularized = Graph_Ising2))
# To make edges in graphs comparable in qgraph, the cut, minimum and maximum arguments need to be set to the same values. Check these with details = TRUE. For example:
qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
       sampleSize = nrow(Data2), minimum = 0,
       cut = 0.15, maximum = 1, details = TRUE,
       esize = 20)
# The averageLayout function can be used to compute comparable layouts:
Layout <- averageLayout(Graph_pcor,Graph_lasso)
layout(t(1:2))
qgraph(corMat, graph = "pcor", layout = Layout, threshold = "bonferroni",
       sampleSize = nrow(Data2), minimum = 0,
       cut = 0.15, maximum = 1, details = TRUE,
       esize = 20, title = "Partial correlations")
layout(1)
# The groups argument can be used to specify which nodes belong together. These are colored and a legend is added:
  
Groups <- c(rep("Agreeableness",5),rep("Conscientiousness",5),rep("Extraversion",5),rep("Neuroticism",5),rep("Opennness",5))
qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
       sampleSize = nrow(Data2), groups = Groups)
# The nodeNames argument can be used to plot a legend with names for each node:
  
Names <- scan("http://sachaepskamp.com/files/BFIitems.txt",what = "character", sep = "\n")
qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
       sampleSize = nrow(Data2), nodeNames = Names, legend.cex = 0.3)
# Using both does something nice:
  
qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
         sampleSize = nrow(Data2), nodeNames = Names, 
         groups = Groups, legend.cex = 0.2)

qgraph(corMat, graph = "glasso", layout = Layout, tuning = 0.25,
       sampleSize = nrow(Data2), minimum = 0,
       cut = 0.15, maximum = 1, details = TRUE,
       esize = 20, title = "LASSO regularization")

library("graphicalVAR")
Res <- graphicalVAR(dataset, gamma = 0, nLambda = 10)
# The mlVAR package can be used to estimate a model based on multiple subjects. This returns three types of networks: temporal, contemporaneous and between-subjects networks. The following code simulates a dataset:
#   
library("mlVAR")
Model <- mlVARsim(nPerson = 50,nTime = 50,nNode = 4)
Data <- Model$Data
head(Data)
# Next, we can analyze this data and plot the results:
Res <- mlVAR(Data,vars = c("V1","V2","V3"), idvar = "ID")
# Fixed effects contemporaneous network:
plot(Res, "contemporaneous", nonsig = "hide")
# Fixed effects temporal network:
plot(Res, "temporal", nonsig = "hide")
# Fixed between-subjects network:
plot(Res, "between", nonsig = "hide")


# dataset$feature82 <- NULL
# t_set$feature82 <- NULL
dataset$era <- NULL
# t_set$era <- NULL
dataset$weight <- NULL
# t_set$weight <- NULL
dataset$id <- NULL
t_set$id <- NULL
dataset$code_id <- NULL
t_set$code_id <- NULL
dataset$feature43 <- NULL
t_set$feature43 <- NULL

dataset$feature58 <-NULL
t_set$feature58 <-NULL
dataset$feature34 <-NULL
t_set$feature34 <-NULL
dataset$feature41 <-NULL
t_set$feature41 <-NULL
dataset$feature50 <-NULL
t_set$feature50 <-NULL
dataset$feature38 <-NULL
t_set$feature38 <-NULL
dataset$feature28 <-NULL
t_set$feature28 <-NULL
dataset$feature83 <-NULL
t_set$feature83 <-NULL
dataset$feature21 <-NULL
t_set$feature21 <-NULL
dataset$feature53 <-NULL
t_set$feature53 <-NULL
dataset$feature73 <-NULL
t_set$feature73 <-NULL
dataset$feature2 <-NULL
t_set$feature2 <-NULL

dataset$feature12_8_95 <- dataset$feature12/dataset$feature8/dataset$feature95
t_set$feature12_8_95 <- t_set$feature12/t_set$feature8/t_set$feature95
dataset$feature97_12_8_95 <- dataset$feature97/dataset$feature12_8_95
t_set$feature97_12_8_95 <- t_set$feature97/t_set$feature12_8_95

dataset$feature12 <-NULL
t_set$feature12 <-NULL
dataset$feature8 <-NULL
t_set$feature8 <-NULL
dataset$feature95 <-NULL
t_set$feature95 <-NULL
dataset$feature97 <-NULL
t_set$feature97 <-NULL
dataset$feature70_91_92 <- dataset$feature70/dataset$feature91/dataset$feature92
t_set$feature70_91_92 <- t_set$feature70/t_set$feature91/t_set$feature92

dataset$feature80_92 <- dataset$feature80/dataset$feature92
t_set$feature80_92 <- t_set$feature80/t_set$feature92
dataset$feature80_70 <- dataset$feature80/dataset$feature70
t_set$feature80_70 <- t_set$feature80/t_set$feature70
dataset$feature70 <-NULL
t_set$feature70 <-NULL
dataset$feature91 <-NULL
t_set$feature91 <-NULL
dataset$feature92 <-NULL
t_set$feature92 <-NULL
dataset$feature80 <-NULL
t_set$feature80 <-NULL


dataset$feature81_17 <- dataset$feature81/dataset$feature17
t_set$feature81_17 <- t_set$feature81/t_set$feature17

dataset$feature81 <-NULL
t_set$feature81 <-NULL
dataset$feature17 <-NULL
t_set$feature17 <-NULL
dataset$feature9_61 <- dataset$feature9/dataset$feature61
t_set$feature9_61 <- t_set$feature9/t_set$feature61
dataset$feature61_89 <- dataset$feature61/dataset$feature89
t_set$feature61_89 <- t_set$feature61/t_set$feature89
dataset$feature9 <-NULL
t_set$feature9 <-NULL
dataset$feature61 <-NULL
t_set$feature61 <-NULL
dataset$feature89 <-NULL
t_set$feature89 <-NULL


dataset$feature13_90 <- dataset$feature13/dataset$feature90
t_set$feature13_90 <- t_set$feature13/t_set$feature90
dataset$feature13 <-NULL
t_set$feature13 <-NULL
dataset$feature90 <-NULL
t_set$feature90 <-NULL

dataset$feature24_27 <- dataset$feature24/dataset$feature27
t_set$feature24_27 <- t_set$feature24/t_set$feature27
dataset$feature24 <-NULL
t_set$feature24 <-NULL
dataset$feature27 <-NULL
t_set$feature27 <-NULL

dataset$feature14_75 <- dataset$feature14/dataset$feature75
t_set$feature14_75 <- t_set$feature14/t_set$feature75
dataset$feature14 <-NULL
t_set$feature14 <-NULL
dataset$feature75 <-NULL
t_set$feature75 <-NULL


dataset$feature6_15 <- dataset$feature6/dataset$feature15
t_set$feature6_15 <- t_set$feature6/t_set$feature15
dataset$feature6 <-NULL
t_set$feature6 <-NULL
dataset$feature15 <-NULL
t_set$feature15 <-NULL
dataset$feature1_55 <- dataset$feature1/dataset$feature55
t_set$feature1_55 <- t_set$feature1/t_set$feature55
dataset$feature44_77 <- dataset$feature44/dataset$feature77
t_set$feature44_77 <- t_set$feature44/t_set$feature77
dataset$feature94_4 <- dataset$feature94/dataset$feature4
t_set$feature94_4 <- t_set$feature94/t_set$feature4
dataset$feature0_63 <- dataset$feature0/dataset$feature63
t_set$feature0_63 <- t_set$feature0/t_set$feature63
dataset$feature5_29 <- dataset$feature5/dataset$feature29
t_set$feature5_29 <- t_set$feature5/t_set$feature29
dataset <- subset(dataset, select = -c(feature1, feature55, feature44, feature77, feature94,feature4,feature0,feature63,feature5,feature29) )
t_set <- subset(t_set, select = -c(feature1, feature55, feature44, feature77, feature94,feature4,feature0,feature63,feature5,feature29) )



dlabel<-as.matrix(dataset$label)
vlabel <-as.vector(dataset$label)

dataset$label <- NULL
# View(dataset[30:108])

library(xgboost)

data <- as.matrix(dataset)
dtrain <- xgb.DMatrix(data,label = dlabel) 
t_set <- as.matrix(t_set)
dtest <- xgb.DMatrix(t_set)

# params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
# xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

bstSparse <- xgboost(data = data, label = dlabel, max.depth = 5, eta = 0.1, nround = 1500, eval.metric = "error", eval.metric = "logloss", print.every.n = 50, objective = "binary:logistic")


# classifier = xgboost(data = as.matrix(dataset), objective = "binary:logistic",max_depth = 4, eta = 1, label = dataset$label, nrounds = 100)


pred = predict(bstSparse, t_set)


df_final <- data.frame(id = t_set_id, proba = pred);
write.csv(df_final, paste0("preds.csv"), row.names =F, quote=F)

# save model to binary local file
xgb.save(bstSparse, "xgboost.model")
# load binary model to R
bst2 <- xgb.load("xgboost.model")
pred2 <- predict(bst2, test$data)

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(dtrain),model = bstSparse)
xgb.plot.importance (importance_matrix = mat[1:80]) 
require(Ckmeans)
xgb.ggplot.importance(importance_matrix = mat[3:99])
#confusion matrix
# library(caret)
# confusionMatrix (pred, t_set$label)

#MXNET R
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages("mxnet")
# install.packages("mlbench")
require(mxnet)
# require(mlbench)
train.x = as.matrix(dataset)
train.y = vlabel
  
mx.set.seed(0)
model <- mx.mlp(train.x, train.y, hidden_node=100, out_node=2, out_activation="softmax",
                num.round=50, array.batch.size=15, learning.rate=0.01, momentum=0.9,
                eval.metric=mx.metric.accuracy)

test.x = as.matrix(t_set)
preds = predict(model, test.x)
# preds = predict(test.x, model)
df_final <- data.frame(id = t_set_id, proba = preds[1,]);
write.csv(df_final, paste0("preds.csv"), row.names =F, quote=F)
## Auto detect layout of input matrix, use rowmajor.
# pred.label = max.col(t(preds))-1
# table(pred.label, test.y)

#SeaClass 
install.packages('devtools')
devtools::install_github('ChrisDienes/SeaClass')
require(SeaClass)

SeaClass()

library(tensorflow)
library(keras)

#KERAS
library(keras)
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y
