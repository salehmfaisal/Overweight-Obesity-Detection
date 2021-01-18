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
install.packages("Amelia")
library(Amelia)
missmap(data,main= "missing value map", col=c("yellow", "red"), legend=FALSE)


## Libraries
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RcolorBrewer")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

## Set up the Decision tree classification model
set.seed(4640)
model<-rpart(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn, data=data, method="class", control = rpart.control(cp = 0.0001,
    minsplit=10,xval=5)
)

## Sensitivity and Specificity ####
library(caret)
library(e1071)
attach(data)
predictions= predict(model,type="class")
confusionMatrix(factor(predictions),owob)


 ##### Printing and Plotting  model ####
printcp(model)
plotcp(model)
fancyRpartPlot(model)
#Plotting tree
plot(model)
text(model)

#tree plot type 1
prp(model, faclen = 0, cex = .60, extra = 1)

#tree plot with total count at each node
counts <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}
prp(model, faclen = 0, cex = 0.50, node.fun=counts)

##Tuning for accuracy manually
tuning_accuracy<- function(model) {
    predicted_value <- predict(model, data, type = 'class')
    confusion_matrix <- table(data$owob, predicted_value)
    model_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    model_accuracy
}

##Tuning by using a function
control <- rpart.control( minsplit = 10,
    minbucket = round(10/3),
    maxdepth = 20,
    cp =  0.0018730 ,
    xval=5)
set.seed(3316)
model_tuned <- rpart(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn, data = data, method = 'class', control = control)
tuning_accuracy(model_tuned)
printcp(model_tuned)
##Tree for tuned model
prp(model_tuned, faclen = 0, cex = 0.8, extra = 1)


#######Pruning part #######
printcp(model_tuned)
bestcp <- model_tuned$cptable[which.min(model_tuned$cptable[,"xerror"]),"CP"]

# Prune the tree by using the best cp.
set.seed(616)
model_pruned <- prune(model_tuned, cp = bestcp)
summary(model_pruned)
# graphical view of pruned tree
prp(model_pruned, faclen = 0, cex = 0.5, extra = 4)
printcp(model_pruned)

### Coverage Table

view(rpart.rules(model_pruned, extra=4, cover=TRUE))


## Sensitivity and Specificity ####
library(caret)
library(e1071)
predictions= predict(model_pruned,type="class")
confusionMatrix(factor(predictions),owob)

### Tree Plot ####
tiff("test.tiff", width = 9, height = 8.5, units = 'in', res=400)
rpart.plot(model_pruned,clip.right.labs = FALSE, type = 1, extra = 101, cex=.7)
dev.off()

tiff("test2.tiff", width = 10, height = 8.5, units = 'in', res=400)
rpart.plot(model_pruned,clip.right.labs = FALSE, type = 1, extra = 104, cex=.7)
dev.off()



#Scoring with test_data
library(ROCR)
test_data1 = predict(model_pruned, data, type = "prob")
#Storing Model Performance Scores in new column
test_predicted <-prediction(test_data1[,2],data$owob)



# Calculating Area under Curve
test_performance <- performance(test_predicted, "auc")
test_performance

# Plotting Lift curve
plot(performance(test_predicted, measure="lift", x.measure="rpp"), colorize=TRUE)

# Calculating True Positive and False Positive Rate
test_performance <- performance(test_predicted, "tpr", "fpr")

# Plot the ROC curve
rc=plot(test_performance, col = "green", lwd = 1.5)
auc(rc)
#Calculating Kolmogorov- Smirnov(KS) statistics
ks1.tree <- max(attr(test_performance, "y.values")[[1]] - (attr(test_performance, "x.values")[[1]]))
ks1.tree

#######################No Need ###############################################################
if(1==3){
#Random Forest
library(randomForest)
set.seed(109)
Ranforest <- randomForest(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn,
   data=data, importance=TRUE, ntree=500)

predicted_value <-predict(Ranforest, data, type = 'class')
confusion_matrix <- table(data$owob, predicted_value)
confusion_matrix
model_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste('Model accuracy is', model_accuracy))



importance(Ranforest)
varImpPlot(Ranforest)

##Conditional forest

install.packages('party')
library(party)
set.seed(150)
Conditional_forest<- cforest(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn,
  data = data, controls=cforest_unbiased(ntree=500, mtry=3)) 

predicted_value <- predict(Conditional_forest,data, OOB=TRUE, type = "response")
confusion_mat <- table(data$owob, predicted_value)
confusion_mat
model_accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
print(paste('Model accuracy is', model_accuracy))
}
########################################################################################

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


#### Bivariate Descriptive Statistics ########
names(data)
attach(data)

######## Bivariate Descriptive Statistics and Chi-square test #####

#### Bivariate description of region ####
table(owob,region)
prop.table(table(owob,region),2)*100
chisq.test(table(owob,region),correct=TRUE)


#### Bivariate description of education ####
table(owob,educ)
prop.table(table(owob,educ),2)*100
chisq.test(table(owob,educ),correct=TRUE)

#### Bivariate description of place ####
table(owob,place)
prop.table(table(owob,place),2)*100
chisq.test(table(owob,place),correct=TRUE)

#### Bivariate description of partner education ####
table(owob,partnered)
prop.table(table(owob,partnered),2)*100
chisq.test(table(owob,partnered),correct=TRUE)

#### Bivariate description of wealth ####
table(owob,wealth)
prop.table(table(owob,wealth),2)*100
chisq.test(table(owob,wealth),correct=TRUE)

#### Bivariate description of work ####
table(owob,work)
prop.table(table(owob,work),2)*100
chisq.test(table(owob,work),correct=TRUE)


#### Bivariate description of watching television ####
table(owob,watchtv)
prop.table(table(owob,watchtv),2)*100
chisq.test(table(owob,watchtv),correct=TRUE)


#### Bivariate description of breastfeed ####
table(owob,breastfeed)
prop.table(table(owob,breastfeed),2)*100
chisq.test(table(owob,breastfeed),correct=TRUE)

#### Bivariate description of women health care decision maker ####
table(owob,womenhcaredcsn)
prop.table(table(owob,womenhcaredcsn),2)*100
chisq.test(table(owob,womenhcaredcsn),correct=TRUE)


#### Bivariate description of age group ####
table(owob,age_grp)
prop.table(table(owob,age_grp),2)*100
chisq.test(table(owob,age_grp),correct=TRUE)

#### Bivariate description of Children Number ####
table(owob,child)
prop.table(table(owob,child),2)*100
chisq.test(table(owob,child),correct=TRUE)


#### Logistic Analysis ##############
library(epitools)

#### Odds ratio and 95% CI for region ####
oddsratio.wald(region, owob)


#### Odds ratio and 95% CI for education ####
oddsratio.wald(educ, owob)


#### Odds ratio and 95% CI for place ####
oddsratio.wald(place, owob)

#### Odds ratio and 95% CI for partner education ####
oddsratio.wald(partnered, owob)


#### Odds ratio and 95% CI for wealth ####
oddsratio.wald(wealth, owob)


#### Odds ratio and 95% CI for working Status ####
oddsratio.wald(work, owob)


#### Odds ratio and 95% CI for watching TV ####
oddsratio.wald(watchtv, owob)


#### Odds ratio and 95% CI for breastfeed ####
oddsratio.wald(breastfeed, owob)


#### Odds ratio and 95% CI for women health care decision ####
oddsratio.wald(womenhcaredcsn, owob)


#### Odds ratio and 95% CI for Age Group ####
oddsratio.wald(age_grp, owob)


#### Odds ratio and 95% CI for children Status ####
oddsratio.wald(child, owob)

##### Bootstrap Analysis ########

library("partykit")

plot(as.party(model_pruned), tp_args = list(id = FALSE))
trees <- vector(mode = "list", length = 1000)
n <- nrow(data)
bootsamples <- rmultinom(length(trees), n, rep(1, n)/n)
mod<-rpart(owob~region+age_grp+child+educ+place+partnered+wealth+work+watchtv+breastfeed+womenhcaredcsn, data=data, method="class", 
control = rpart.control(cp = 0.0001,minsplit=10,xval=0))

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

