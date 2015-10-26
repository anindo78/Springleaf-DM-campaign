setwd("~/Kaggle/SpringLeaf DM Campaign")

train <- read.csv("train.csv", header=T)
f4 <- read.csv("fcols4.csv", header=T)
# subset data based on fcols4
strain <- train[, names(train) %in% fcols12 | names(train) %in% c("target", "ID")]
head(strain)




# preprocessing the columns

c2 <- function(x) {
  if (max(nchar(x)) - min(nchar(x)) >= 5) {
    miss = ifelse(substr(as.character(x),1,2)=="99" | substr(as.character(x),1,3)== "-99" |
                    substr(as.character(x),1,2)== "-1", 0, x)
  } else {
    miss = ifelse(substr(as.character(x),1,2)=="99" | substr(as.character(x),1,3)== "-99" |
                    substr(as.character(x),1,2)=="98" | substr(as.character(x),1,3)== "-98" 
                  , 0, x)
  }
}

train_int <- as.data.frame(lapply(strain[, !sapply(strain, is.factor)], c2))
train_int[is.na(train_int)] <- 0
train_int[train_int==-1] = 0

# Visual check to drop few vars
# VAR_0212, VAR_0531
train_int <- train_int[, !names(train_int) %in% c("VAR_0212", "VAR_0531")]
#646

prop.table(table(train_int$target))
#         0         1 
# 0.7674532 0.2325468 

ischr <- sapply(strain, is.factor)
train_chr <- strain[ischr]

train_chr <- train_chr[, names(train_chr) %in% c("VAR_0001", "VAR_0005", "VAR_0226", "VAR_0230","VAR_0232",
                                                 "VAR_0236","VAR_0283","VAR_0305","VAR_0325","VAR_0342",
                                                 "VAR_0352", "VAR_0353", "VAR_0354", "VAR_1934")]

library(caret)
chrvars_train <- predict(dummyVars(~ ., data = train_chr), newdata = train_chr)
head(chrvars_train)

train_tot <- data.frame(cbind(train_int, chrvars_train))
varlist_train <- colnames(train_tot)

# Predictive Modeling on Integer data
library(caret)

train$target[train$target==1] <- 'yes'
train$target[train$target==0] <- 'no'

intrain <- createDataPartition(train$target, times=1, p = 0.7, list=F)
dftrain <- train[intrain, ]
dftest <- train[-intrain, ]

trainX <- dftrain[, names(dftrain) != "target"]
trainY <- as.factor(dftrain$target)

#preProcValues <- preProcess(trainX, method = c("center", "scale"))

###############################################################################
# RUN GLMNET USING LASSO 

library(glmnet)

preProcValues <- preProcess(trainX, method = c("center", "scale"))
trainX1 <- predict(preProcValues, trainX)

trainX1 <- data.matrix(trainX1)
trainY <- as.vector(trainY)
masterlm <- cv.glmnet(trainX1, trainY, family = "binomial", alpha = 1)

plot(masterlm)
masterlm$lambda.min
coef(masterlm, s = "lambda.min")

dftrain$predicted <- predict(masterlm, trainX1, s = "lambda.min", type = "response")

#making submission
preProcValues <- preProcess(train_tot, method = c("center", "scale"))
train_tot1 <- predict(preProcValues, train_tot[, !names(train_tot) %in% c("ID","target")])


trainX1 <- data.matrix(train_tot1)
trainY <- as.factor(train_tot$target)

library(glmnet)
masterlm <- cv.glmnet(trainX1, trainY, family = "binomial", alpha = 1, type.measure = "auc")
plot(masterlm)
masterlm$lambda.min

train_tot1$predicted <- predict(masterlm, trainX1, s = "lambda.min", type = "response")
train_tot1$predclass <- predict(masterlm, trainX1, s = "lambda.min", type = "class")

train_tot1$target <- train_tot$target

library(pROC)
auc <- roc(train_tot1$target, train_tot1$predicted)
auc$auc
plot(auc)

train_tot1$ID <- train_tot$ID

#finally submitted file
fin_sub <- train_tot1[, names(train_tot1) %in% c("ID","predclass")]
table(fin_sub[,1])

write.csv(fin_sub, file="fin_sub.csv")

head(fin_sub)

head(dftrain$predclass)
head(dftrain$predicted)

dfsort <- dftrain[order(dftrain$predicted),]

library(pROC)
auc <- roc(dftrain$target, dftrain$predicted)
auc$auc
plot(auc)

library(gmodels)
CrossTable(dftrain$target, dftrain$predclass, chisq=T, dnn=c('actual target', 'predicted target'))


# On test data
testX <- dftest[, names(dftest) != "target"]
testY <- as.factor(dftest$target)

preProcValues <- preProcess(trainX, method = c("center", "scale"))
testX1 <- predict(preProcValues, testX)
testX1 <- data.matrix(testX1)

dftest$predicted <- predict(masterlm, testX1, s = "lambda.min", type = "response")

head(dftrain$predicted)

library(pROC)
auct <- roc(dftest$target, dftest$predicted)
auct$auc
plot(auct)
# GLMNET gives an AUC: 0.76, WITH AN ERROR RATE CLOSE TO (3657 + 17773)/101663 ~ 21% ERROR RATE.





#################################################################################
# RUNNING GBM USING PREPROCESS AND TRAIN() IN CARET

intrain <- createDataPartition(train$target, times=1, p = 0.1, list=F)
dftrain1 <- train[intrain, ]
dftest1 <- train[-intrain, ]

train1X <- dftrain1[, names(dftrain1) != "target"]
train1Y <- as.factor(dftrain1$target)

preProcValues <- preProcess(train1X, method = c("center", "scale"))
train1X1 <- predict(preProcValues, train1X)

train1X1 <- data.matrix(train1X1)
train1Y <- as.factor(train1Y)

gbmGrid <-  expand.grid(interaction.depth = 2:5,
                        n.trees = 100,
                        shrinkage = 0.1,
                        n.minobsinnode = 200)

fitControl <- trainControl(
  method = "cv",
  number = 10,
  ## repeated ten times
  #repeats = 10,
  classProbs = T,
  summaryFunction = twoClassSummary
)


fit <- train(
  x = train1X1,
  y = train1Y,
  
  method="gbm", 
  metric="ROC",
  #preProcess = c("center", "scale"),
  trControl = fitControl,
  tuneGrid = gbmGrid
  #trControl = trainControl(preProcOptions = list(thresh = 0.95), 
)


summary(fit)
plot(fit)
plot(fit, metric="Kappa")
fit$results
fit$finalModel

# HIGEHST ROC, N.tree = 100, interaction.depth = 5, shrinkage=0.1

# ADD IN FACTOR VARS.




