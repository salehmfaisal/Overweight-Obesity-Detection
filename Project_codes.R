## Reading Data Set
load("bmodeldat.RData")
data=bmodel.dat2

## Feature Engineering
names(data)
data$age_grp <- data$age
data$age_grp <- ifelse((data$age>=15 & data$age<=25) , '15-25',data$age_grp)
data$age_grp <- ifelse((data$age>25 & data$age<=36) , '26-36',data$age_grp)
data$age_grp <- ifelse((data$age>36 & data$age<=49) , '37-49',data$age_grp)
data$age_grp<-as.factor(data$age_grp)
data$child<- data$livchildren
data$child=ifelse(data$livchildren == c("1-2", "3+"), 'have children', 'no children')
data$child=as.factor(data$child)

data$age<-NULL
data$livchildren<- NULL
data$caseid <-NULL
names(data)

##Checking for missing values
#install.packages("Amelia")
library(Amelia)
missmap(data,main= "missing value map", col=c("yellow", "red"), legend=FALSE)

## Libraries
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RcolorBrewer")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

## Set up the Decision tree classification model
set.seed(4640)
#model<-rpart(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn, data=data, method="class", control = rpart.control(cp = 0.0001,
#    minsplit=10,xval=5)
#)

control <- rpart.control( minsplit = 10,
    minbucket = round(10/3),
    maxdepth = 30,
    cp =  0.001,#8730 
    xval=5)

# full tree 

set.seed(3316)
model_tuned <- rpart(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn, data = data, method = 'class', control = control)
tuning_accuracy(model_tuned)

#######Pruning part #######
printcp(model_tuned)
bestcp <- model_tuned$cptable[which.min(model_tuned$cptable[,"xerror"]),"CP"]

# Prune the tree by using the best cp.
set.seed(616)
model_pruned <- prune(model_tuned, cp = bestcp)
summary(model_pruned)
# graphical view of pruned tree
prp(model_pruned, faclen = 0, cex = 0.5, extra = 1)
printcp(model_pruned)

### Coverage Table
#View(rpart.rules(model_pruned, extra=4, cover=TRUE))

# confusion matrix 
bnr2treepreds <- predict(model_pruned, data, type="class")
bnr2treeconfuse <- confusionMatrix(bnr2treepreds, data$owob, positive="yes")
bnr2treeconfuse 

rpart.plot(model_pruned, box.palette="BuRd",
           branch.lty=1, branch.lwd=2, branch.col="darkgreen", type=0,shadow.col="white", nn=TRUE, cex=.95)

nr2predsRF <- predict(model_pruned, newdata=data, type="prob")[,2]
auc(data$owob,nr2predsRF)


# CHECK THIS 60% THRESHOLD CAREFULLY (AM I RIGHT OR WRONG?). 
# After correction, you can apply this on 50-50 split analysis (mentioned below)

bnr2treepreds60 <- as.factor(ifelse(predict(model_pruned, data, type="prob")[,1]>0.60, 'yes', 'no'))
bnr2treeconfuse60 <- confusionMatrix(bnr2treepreds60, data$owob, positive="yes")
bnr2treeconfuse60 


# splitted data in to 50-50, and then applied same aforementioned approach 
library(caret)
set.seed(1166)
train<- createDataPartition(y = data$owob , p = .50, list=F)
dtrain<-data[train,]
dtest<-data[-train,]

set.seed(3316)
model_tuned1 <- rpart(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn, data = dtrain, method = 'class', control = control)
tuning_accuracy(model_tuned1)

#######Pruning part #######
printcp(model_tuned)
bestcp1 <- model_tuned1$cptable[which.min(model_tuned1$cptable[,"xerror"]),"CP"]
# Prune the tree by using the best cp.
set.seed(616)
model_pruned1 <- prune(model_tuned1, cp = bestcp1)
summary(model_pruned1)

rpart.plot(model_pruned1, box.palette="BuRd",
           branch.lty=1, branch.lwd=2, branch.col="darkgreen", type=0,shadow.col="white", nn=TRUE, cex=.95)

View(rpart.rules(model_pruned1, extra=4, cover=TRUE))

bnr2treepreds1<-predict(model_pruned1, dtrain, type="class")
bnr2treeconfuse1 <- confusionMatrix(bnr2treepreds1, dtrain$owob, positive="yes")
bnr2treeconfuse1 

nr2predsRF1 <- predict(model_pruned1, newdata=dtrain, type="prob")[,2]
auc(dtrain$owob,nr2predsRF1)


set.seed(3316)
model_tuned2 <- rpart(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn, data = dtest, method = 'class', control = control)
tuning_accuracy(model_tuned2)

#######Pruning part #######
printcp(model_tuned2)
bestcp2 <- model_tuned2$cptable[which.min(model_tuned2$cptable[,"xerror"]),"CP"]
# Prune the tree by using the best cp.
set.seed(616)
model_pruned2 <- prune(model_tuned2, cp = bestcp2)
summary(model_pruned2)

rpart.plot(model_pruned2, box.palette="BuRd",
           branch.lty=1, branch.lwd=2, branch.col="darkgreen", type=0,shadow.col="white", nn=TRUE, cex=.95)

View(rpart.rules(model_pruned1, extra=4, cover=TRUE))

bnr2treepreds2<-predict(model_pruned2, dtest, type="class")
bnr2treeconfuse2 <- confusionMatrix(bnr2treepreds2, dtest$owob, positive="yes")
bnr2treeconfuse2 

nr2predsRF2 <- predict(model_pruned1, newdata=dtest, type="prob")[,2]
auc(dtest$owob,nr2predsRF2)


# Some performance measures for Binary Classification 
# in presence of imbalance problem

# sn = sensitivity and sp= specificity

ameasures <- function(sn, sp){

	gmean = sqrt(sn*sp);
	bacc = 0.5*(sn+sp);
	yindex = sn-(1-sp);
	plr = sn/(1-sp);
	nlr = (1-sn)/sp;

return(round(c(GMEAN=gmean, BACC=bacc, YINDEX=yindex, PLR=plr, NLR=nlr), 4))
}

ameasures(0.8571, 0.6114)


#### Logistic Regression ####

glm.fit <- glm(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn,
  data = data, family = binomial)

summary(glm.fit)
glm.probs <- predict(glm.fit,type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "yes", "no")
attach(data)
confusionmatrix=table(glm.pred,owob)
model.accuracy= mean(glm.pred == owob)
library(caret)
library(caret)
library(e1071)
confusionMatrix(factor(glm.pred),owob)

library(pROC)
roccurve= roc(glm.fit$y, glm.probs)
plot(roccurve)

auc(roccurve)

##### Bootstrap Analysis ########

library("partykit")

plot(as.party(model_pruned), tp_args = list(id = FALSE))
trees <- vector(mode = "list", length = 1000)
n <- nrow(data)
bootsamples <- rmultinom(length(trees), n, rep(1, n)/n)
mod<-rpart(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn, data=data, method="class", 
control = rpart.control(cp = 0.001,minsplit=10, minbucket = round(10/3), maxdepth = 20, xval=0))

for (i in 1:length(trees)){
trees[[i]] <- update(mod, weights = bootsamples[,i])
}

table(sapply(trees, function(x) as.character(x$frame$var[5])))

classprob <- matrix(0, nrow = n, ncol = length(trees))
for (i in 1:length(trees)) {
classprob[,i] <- predict(trees[[i]],newdata = data)[,1]
classprob[bootsamples[,i] > 0,i] <- NA
}
avg <- rowMeans(classprob, na.rm = TRUE)
predictions <- factor(ifelse(avg > 0.5, "yes","no"))
predtab <- table(data$owob, predictions)
predtab
model_accuracy <- 1-(sum(diag(predtab)) / sum(predtab))
print(model_accuracy)
