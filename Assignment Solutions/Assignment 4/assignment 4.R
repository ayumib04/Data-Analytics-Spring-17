df <- read.table("party.txt",header = TRUE,stringsAsFactors = TRUE)
library(ggplot2)
ggplot(df,aes(x = df$handicapped_infants))+geom_bar()
library(reshape2)
library(plyr)
library(psych)
library(party)
library(tree)
str(df)
head(df)
df <- as.data.frame(df)

#rename
names(df)[1] <- "pr"   
names(df)[2] <- "hi"
names(df)[3] <- "wpcs"
names(df)[4] <- "abr" 
names(df)[5] <- "pff"
names(df)[6] <- "esa"
names(df)[7] <- "rgis"
names(df)[8] <- "nstb"
names(df)[9] <- "atnc"
names(df)[10] <- "mxm"
names(df)[11] <- "imm"
names(df)[12] <- "scc"
names(df)[13] <- "es"
names(df)[14] <- "srts"
names(df)[15] <- "cr"
names(df)[16] <- "dfe"
names(df)[17] <- "eaas"

##D-Tree Rpart full##
library(rpart)
myform <- pr~.
r_tree <- rpart(myform, data = df, method = "class")
print(r_tree)
pred <- predict(r_tree, newdata = df, type = "class")
mc <- table(pred,df$pr)
mc
accuracy <- (mc[1,1]+mc[2,2])/sum(mc)
sensitivity = (mc[1,1])/(mc[1,1]+mc[1,2])
specificity = (mc[2,2])/(mc[2,1]+mc[2,2])
ppval <- (mc[1,1])/(mc[1,1]+mc[2,1])
npval <- (mc[2,2])/(mc[1,2]+mc[2,2])
accuracy
sensitivity
specificity
ppval
npval

p4 <- as.data.frame(pred)
p4$actuals <- as.vector(df$pr)
ff_p <- as.data.frame(table(p4$pred))
ff_a <- as.data.frame(table(p4$actuals))
ff_p
ff_a
dfn2 <- data.frame(name = c('dem_p','rep_p','dem_a','rep_a'), period=c('predicted','predicted','actuals','actuals'),val = c(258,177,267,168))
p <- ggplot(data = dfn2, aes(x = reorder(name, val),y = val))
p = p+geom_bar(stat='identity')
p = p+facet_grid(~period)
p




##D-Tree Rpart 80-20 random##
dt = sort(sample(nrow(df),nrow(df)*.8))
train <- df[dt,]
test <- df[-dt,]
myform <- pr~.
r_tree_train <- rpart(myform, data = train, method = "class")
pred2 <- predict(r_tree_train, newdata = test, type = "class")
mc2 <- table(pred2,test$pr)
mc2
accuracy2 <- (mc2[1,1]+mc2[2,2])/sum(mc2)
sensitivity2 = (mc2[1,1])/(mc2[1,1]+mc2[1,2])
specificity2 = (mc2[2,2])/(mc2[2,1]+mc2[2,2])
ppval2 <- (mc2[1,1])/(mc2[1,1]+mc2[2,1])
npval2 <- (mc2[2,2])/(mc2[1,2]+mc2[2,2])
accuracy2
sensitivity2
specificity2
ppval2
npval2

p3 <- as.data.frame(pred2)
p3$actuals <- as.vector(test$pr)
head(p3)
f_p <- as.data.frame(table(p3$pred))
f_a <- as.data.frame(table(p3$actuals))
f_p
f_a
dfn1 <- data.frame(name = c('dem_p','rep_p','dem_a','rep_a'), period=c('predicted','predicted','actuals','actuals'),val = c(39,48,41,46))
p <- ggplot(data = dfn1, aes(x = reorder(name, val),y = val))
p = p+geom_bar(stat='identity')
p = p+facet_grid(~period)
p


##D-Tree Rpart CV 60-20-20 random##
dt2 = sort(sample(nrow(df),nrow(df)*.8))
train2 <- df[dt2,]
test2 <- df[-dt2,]
n <- nrow(train2) #number of observations
K <- 4 #4 fold cross validation to enable a 60-20-20 split
cut <- n%/%K #determine size of each block
set.seed(5) #for the same sequence all the time
randval <- runif(n) #generate a column of random values
rows <- rank(randval) #associate with each individual random value a row
block <- (rows-1)%/%cut +1 #associate a number to each individual block (block1, block2...etc)
block <- as.factor(block) #convert into factors
print(summary(block))
all.err <- numeric(0)
all.acc <- numeric(0)
all.sen <- numeric(0)
all.spe <- numeric(0)
all.ppv <- numeric(0)
all.npv <- numeric(0)
for(k in 1:K) {
  r_tree_cvtrain <- rpart(pr~.,data = train2[block!=k,], method = "class")
  pred_cv <- predict(r_tree_cvtrain, newdata = train2[block==k,], type = "class")
  mc3 <- table(pred_cv,train2$pr[block==k])
  acc <- (mc3[1,1]+mc3[2,2])/sum(mc3)
  err <- 1.0 - acc
  sen <- (mc3[1,1])/(mc3[1,1]+mc3[1,2])
  spe <- (mc3[2,2])/(mc3[2,1]+mc3[2,2])
  ppv <- (mc3[1,1])/(mc3[1,1]+mc3[2,1])
  npv <- (mc3[2,2])/(mc3[1,2]+mc3[2,2])
  all.err <- rbind(all.err,err)
  all.acc <- rbind(all.acc,acc)
  all.sen <- rbind(all.sen,sen)
  all.spe <- rbind(all.spe,spe)
  all.ppv <- rbind(all.ppv,ppv)
  all.npv <- rbind(all.npv,npv)
}
print(all.err)
print(all.acc)
print(all.spe)
print(all.sen)
print(all.ppv)
print(all.npv)
err.cv <- mean(all.err)
acc.cv <- mean(all.acc)
sen.cv <- mean(all.sen)
spe.cv <- mean(all.spe)
ppv.cv <- mean(all.ppv)
npv.cv <- mean(all.npv)
print(err.cv)
print(acc.cv)
print(sen.cv)
print(spe.cv)
print(ppv.cv)
print(npv.cv)
  #' Now build model on train+validation to finally test on pred data
  r_tree_train2 <- rpart(pr~.,data = train2, method = "class")
  pred3 <- predict(r_tree_train2, newdata = test2, type = "class")
  mc4 <- table(pred3,test2$pr)
  acc2 <- (mc4[1,1]+mc4[2,2])/(sum(mc4))
  sen2 <- (mc4[1,1])/(mc4[1,1]+mc4[1,2])
  spe2 <- (mc4[2,2])/(mc4[2,1]+mc4[2,2])
  ppv2 <- (mc4[1,1])/(mc4[1,1]+mc4[2,1])
  npv2 <- (mc4[2,2])/(mc4[1,2]+mc4[2,2])
  acc2
  sen2
  spe2
  ppv2
  npv2
  
  p2 <- as.data.frame(pred2)
  t2 <- as.data.frame(test2$pr)
  p2$actuals <- as.vector(t2)
  fre_pred_t <- as.data.frame(table(p2$pred2))
  fre_actuals_t <- as.data.frame(table(p2$actuals))
  fre_pred_t
  fre_actuals_t
  dfn <- data.frame(name = c('dem_p','rep_p','dem_a','rep_a'), period=c('predicted','predicted','actuals','actuals'),val = c(39,48,58,29))
  p <- ggplot(data = dfn, aes(x = reorder(name, val),y = val))
  p = p+geom_bar(stat='identity')
  p = p+facet_grid(~period)
  p
##Leave one out Cross Validation##
  nrow(df)
  dt3 = sort(sample(nrow(df),nrow(df)*.8))
  train3 <- df[dt2,]
  test3 <- df[-dt2,]
  n <- nrow(train3) #number of observations
  K <- 348 #435 fold cross validation
  cut <- n%/%K #determine size of each block
  set.seed(5) #for the same sequence all the time
  randval <- runif(n) #generate a column of random values
  rows <- rank(randval) #associate with each individual a row
  block <- (rows-1)%/%cut +1 #associate a number to each individual block
  block <- as.factor(block) #convert into factor
  print(summary(block))
  all.err2 <- numeric(0)
  all.acc2 <- numeric(0)
  all.sen2 <- numeric(0)
  all.spe2 <- numeric(0)
  all.ppv2 <- numeric(0)
  all.npv2 <- numeric(0)
  for(k in 1:K) {
    r_tree_cvtrain <- rpart(pr~.,data = train3[block!=k,], method = "class")
    pred_cv <- predict(r_tree_cvtrain, newdata = train3[block==k,], type = "class")
    mc5 <- table(pred_cv,train3$pr[block==k])
    acc <- (mc5[1,1]+mc5[2,2])/sum(mc5)
    err <- 1.0 - acc
    sen <- (mc5[1,1])/(mc5[1,1]+mc5[1,2])
    spe <- (mc5[2,2])/(mc5[2,1]+mc5[2,2])
    ppv <- (mc5[1,1])/(mc5[1,1]+mc5[2,1])
    npv <- (mc5[2,2])/(mc5[1,2]+mc5[2,2])
    all.err2 <- rbind(all.err2,err)
    all.acc2 <- rbind(all.acc2,acc)
    all.sen2 <- rbind(all.sen2,sen)
    all.spe2 <- rbind(all.spe2,spe)
    all.ppv2 <- rbind(all.ppv2,ppv)
    all.npv2 <- rbind(all.npv2,npv)
  }
  print(all.err2)
  print(all.acc2)
  print(all.spe2)
  print(all.sen2)
  print(all.ppv2)
  print(all.npv2)
  err.cv <- mean(all.err2)
  acc.cv <- mean(all.acc2)
  sen.cv <- mean(all.sen2)
  spe.cv <- mean(all.spe2)
  ppv.cv <- mean(all.ppv2)
  npv.cv <- mean(all.npv2)
  print(err.cv)
  print(acc.cv)
  print(sen.cv)
  print(spe.cv)
  print(ppv.cv)
  print(npv.cv)
  
  #for final testing
  r_tree_train3 <- rpart(pr~.,data = train3, method = "class")
  pred4 <- predict(r_tree_train3, newdata = test3, type = "class")
  mc6 <- table(pred4,test3$pr)
  acc3 <- (mc6[1,1]+mc6[2,2])/(sum(mc6))
  sen3 <- (mc6[1,1])/(mc6[1,1]+mc6[1,2])
  spe3 <- (mc6[2,2])/(mc6[2,1]+mc6[2,2])
  ppv3 <- (mc6[1,1])/(mc6[1,1]+mc6[2,1])
  npv3 <- (mc6[2,2])/(mc6[1,2]+mc6[2,2])
  acc3
  sen3
  spe3
  ppv3
  npv3
  
  
  p5 <- as.data.frame(pred4)
  p5$actuals <- as.vector(test3$pr)
  fl_p <- as.data.frame(table(p5$pred4))
  fl_p
  fl_t <- as.data.frame(table(p5$actuals))
  fl_t
  dfn3 <- data.frame(name = c('dem_p','rep_p','dem_a','rep_a'), period=c('predicted','predicted','actuals','actuals'),val = c(54,33,58,29))
  p <- ggplot(data = dfn, aes(x = reorder(name, val),y = val))
  p = p+geom_bar(stat='identity')
  p = p+facet_grid(~period)
  p
  