setwd("C:\\Users\\TEMP.Main.001\\Documents\\GitHub\\AI_china_ts\\data")

# library(rattle)
# rattle()

# Importing the dataset
dataset <- read.csv('stock_train_data_20171020.csv')
# dataset = dataset[4:25]
t_set <- read.csv('stock_test_data_20171020.csv')
t_set_id <- t_set$id

dataset$feature82 <- NULL
t_set$feature82 <- NULL
dataset$era <- NULL
# t_set$era <- NULL
dataset$weight <- NULL
# t_set$weight <- NULL
dataset$id <- NULL
t_set$id <- NULL

dlabel<-as.matrix(dataset$label)
dataset$label <- NULL


library(xgboost)

data <- as.matrix(dataset)
dtrain <- xgb.DMatrix(data,label = dlabel) 
t_set <- as.matrix(t_set)
dtest <- xgb.DMatrix(t_set)

# params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
# xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

bstSparse <- xgboost(data = data, label = dlabel, max.depth = 4, eta = 0.1, nround = 400, eval.metric = "error", eval.metric = "logloss", print.every.n = 50, objective = "binary:logistic")


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
#confusion matrix
# library(caret)
# confusionMatrix (pred, t_set$label)
