library(xgboost)
ID = 'id'
TARGET = 'loss'
SEED = 0
SHIFT = 200
#TRAIN_FILE = "allstate_train.csv"
#TRAIN_FILE = "allstate_train_new.csv"
TRAIN_FILE = "allstate_train_rf.csv"
#TEST_FILE = "allstate_test.csv"
#TEST_FILE = "allstate_test_new.csv"
TEST_FILE = "allstate_test_rf.csv"
SUBMISSION_FILE = "../input/sample_submission.csv"
train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)
y_train = log(train[,TARGET, with = FALSE] + SHIFT)[[TARGET]]
train[, c(ID, TARGET) := NULL]
test[, c(ID) := NULL]
ntrain = nrow(train)
train_test = rbind(train, test)
features = names(train)
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
#best_nrounds = res$best_iteration # for xgboost v0.6 users 
best_nrounds = which.min(res[, res$test.error.mean]) # for xgboost v0.4-4 users
best_nrounds
#id <- which(res[, res$test.error.mean]==best_nrounds)
cv_mean = res$test.error.mean[best_nrounds]
cv_std = res$test.error.std[best_nrounds]
res$
cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))
#best_nrounds 3410
#CV-Mean: 1134.61902176964 5.41080688478779
gbdt = xgb.train(xgb_params, dtrain, nrounds = as.integer(best_nrounds/0.8),verbose= 2 )
gbdt$handle
submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = exp(predict(gbdt,dtest)) - SHIFT
write.csv(submission,'xgb_starter_v7o.sub.csv',row.names = FALSE)
sessionInfo()
