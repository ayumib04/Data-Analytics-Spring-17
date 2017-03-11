library(data.table)
library(xgboost)
library(Metrics)
library(foreach)
ID = 'id'
TARGET = 'loss'
SEED = 0
SHIFT = 200
TRAIN_FILE = "allstate_train.csv"
#TRAIN_FILE = "allstate_train_new.csv"
#TRAIN_FILE = "allstate_train_rf.csv"
TEST_FILE = "allstate_test.csv"
#TEST_FILE = "allstate_test_new.csv"
#TEST_FILE = "allstate_test_rf.csv"
SUBMISSION_FILE = "../input/sample_submission.csv"
train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)
y_train = log(train[,TARGET, with = FALSE] + SHIFT)[[TARGET]]
train[, c(ID, TARGET) := NULL]
test[, c(ID) := NULL]
testframe <- as.data.frame(test)
head(testframe)
colnames(testframe)
testframe$loss
trainframe <- as.data.frame(train)
colnames(trainframe)
colnames(testframe)
testrows <- 125546
ntrain = nrow(train)
train_test = rbind(train, test)
features = names(train)
# One-Hot Encoding ##
for (f in features) {
  if (class(train_test[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- sort(unique(train_test[[f]]))
    train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
  }
}
x_train = train_test[1:ntrain,]
x_test = train_test[(ntrain+1):nrow(train_test),]
dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest = xgb.DMatrix(as.matrix(x_test))
set.seed(123)
xgb_params = list(
  seed = 0,
  colsample_bytree = 0.5,
  subsample = 0.8,
  eta = 0.01,
  objective = 'reg:linear',
  max_depth = 12,
  alpha = 1,
  gamma = 2,
  min_child_weight = 1,
  base_score = 7.76
)
xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}
res = xgb.cv(xgb_params,
             dtrain,
             nrounds=5000,
             nfold=5,
             early_stopping_rounds=15,
             print_every_n = 10,
             verbose= 2,
             feval=xg_eval_mae,
             maximize=FALSE)
res$train.error.mean
res$test.error.mean
res
#best_nrounds = res$best_iteration # for xgboost v0.6 users 
best_nrounds = which.min(res[, res$train.error.mean]) # for xgboost v0.4-4 users
best_nrounds = 3125

#id <- which(res[, res$test.error.mean]==best_nrounds)
cv_mean = res$test.error.mean[best_nrounds]
cv_std = res$test.error.std[best_nrounds]

cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))
#best_nrounds 3410
#CV-Mean: 1134.61902176964 5.41080688478779
gbdt = xgb.train(xgb_params, dtrain, nrounds = as.integer(3125/0.8),verbose= 2 )
gbdt$handle
submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = exp(predict(gbdt,dtest)) - SHIFT
loss_pred <- exp(predict(gbdt,dtrain)) - SHIFT
nrow(loss_pred)
loss_pred <- as.data.frame(loss_pred)
write.csv(loss_pred,'xgb1_loss.csv',row.names = FALSE)
write.csv(submission,'xgb_starter_v7o.sub.csv',row.names = FALSE)
sessionInfo()
## Ensemble ####
set.seed(123)
install.packages("foreach")

iterations <- 5
best_nrounds <- c(4995,4996,4997,4998,4999)
best_nrounds <- as.array(best_nrounds)
best_nrounds[1]

#For submission - test data
predictions <- foreach(m=1:iterations,.combine=cbind) %do% {
  gbdt = xgb.train(xgb_params, dtrain, nrounds = as.integer(best_nrounds[m]/0.8),verbose= 2 )
  exp(predict(gbdt,dtest)) - SHIFT
}
nrow(predictions)
ncol(predictions)

pred_original <- predictions
nrow(pred_original)
ncol(pred_original)
predictions<- rowMeans(predictions)
loss_pred_ensemble <- as.data.frame(predictions)
nrow(loss_pred_ensemble)
ncol(loss_pred_ensemble)
write.csv(loss_pred_ensemble,'xgb5_ensemble_loss.csv',row.names = FALSE)

#For MAE - Train data
predictions_mae <- foreach(m=1:iterations,.combine=cbind) %do% {
  gbdt = xgb.train(xgb_params, dtrain, nrounds = as.integer(best_nrounds[m]/0.8),verbose= 2 )
  exp(predict(gbdt,dtrain)) - SHIFT
}
nrow(predictions_mae)
ncol(predictions_mae)
predmae_original <- predictions_mae
predictions_mae <- rowMeans(predictions_mae)
predictions_mae <- as.data.frame(predictions_mae)
head(predictions_mae)
write.csv(predictions_mae,'xgb3_ensemble_train_loss.csv',row.names = FALSE)
write.csv(predmae_original,'xgb3_all_train_loss.csv',row.names = FALSE)
mae_train <- abs(sum(predictions_mae-trainframe$loss) / nrow(predictions_mae))
mae_train
nn_pred <- as.data.frame(read.csv("allstate-nn-train-prediction.csv"))
nn_pred <- rowMeans(nn_pred)
nn_pred <- as.data.frame(nn_pred)
head(nn_pred)
head(nn_pred)
nn_pred$X = NULL
s = 0.0
for (k in 1:nrow(nn_pred)){
  s = s + ((nn_pred[k,4]-trainframe[k,"loss"]))
}
trainframe[1,"loss"]
nrow(trainframe)
nrow(nn_pred)
abs(s/nrow(nn_pred))
mae_nn_train <- abs(sum(nn_pred-trainframe$loss)/nrow(nn_pred))

nn_pred_test <- as.data.frame(read.csv("allstate-nn-test-prediction.csv"))
nn_pred_test <- as.data.frame(nn_pred_test)
head(nn_pred_test)
nn_pred_test$X = NULL
nn_pred_test <- rowMeans(nn_pred_test)
nn_pred_test <- as.data.frame(nn_pred_test)
nrow(nn_pred_test)
## ENSEMBLE XGBOOST+NN ####
xgb_pred = read.csv("xgb3_ensemble_train_loss.csv")
xgb_pred = as.data.frame(xgb_pred)
head(xgb_pred)
nrow(xgb_pred)
ensemble_xgb_NN <- (xgb_pred+nn_pred_test*9)/10
head(ensemble_xgb_NN)
MAE_ensemble <- abs(sum(ensemble_xgb_NN-trainframe$loss)/nrow(ensemble_xgb_NN))
xgb_pred_test <- read.csv("xgb3_ensemble_loss.csv")
xgb_pred_test = as.data.frame(xgb_pred_test)
head(xgb_pred_test)
nn_pred_test = read.csv("allstate-nn-submission1.csv")
nn_pred_test = as.data.frame(nn_pred_test)
head(nn_pred_test)
#id <- as.data.frame(nn_pred_test$id)
id <- as.data.frame(testframe$id)
head(id)
colnames(id) = "id"
nn_pred_test$id <- NULL
ensemble_xgb_NN_submission <- (nn_pred_test+xgb_pred_test*9)/10
head(ensemble_xgb_NN_submission)
ensemble_xgb_NN_submission <- as.data.frame(ensemble_xgb_NN_submission)
ensemble_xgb_NN_submission <- as.data.frame(c(id,ensemble_xgb_NN_submission))
head(ensemble_xgb_NN_submission)
write.csv(ensemble_xgb_NN_submission,'ensemble_xgb_NN_submission.csv',row.names = FALSE)
#ensemble_xgb_submission <- xgb_pred_test
ensemble_xgb_submission <- read.csv("xgb5_ensemble_loss.csv")
head(ensemble_xgb_submission)
head(ensemble_xgb_submission)

colnames(ensemble_xgb_submission)<- "loss"
ensemble_xgb_submission <- as.data.frame(c(id,ensemble_xgb_submission))
head(ensemble_xgb_submission)
write.csv(ensemble_xgb_submission,'ensemble_xgb5_submission.csv',row.names = FALSE)
head(predmae_original)
head(pred_original)
head(predictions)
