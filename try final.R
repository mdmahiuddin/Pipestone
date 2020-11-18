library(readxl)
#load the data set
RetentionDataRaw <- read_excel("C:/Users/DELETE ME METTE/Desktop/My Folder/Spring 2019/551/Final/draft final/RetentionDataRaw.xlsx")

#after examining the structure of the dataset, the good customer score shown as character variable, so change it.
RetentionDataRaw$`Good Customer Score` <- as.factor(RetentionDataRaw$`Good Customer Score`)

RetentionDataRaw$`Good Customer Score` <- as.numeric(RetentionDataRaw$`Good Customer Score`)

#applying all condition given in the first part
#d1 <- subset(RetentionDataRaw, RetentionDataRaw$`Days Deliq` > 0 | !is.na(RetentionDataRaw$`External Status`) | RetentionDataRaw$`Opening Balance` > RetentionDataRaw$`Credit Limit` | RetentionDataRaw$`Ending Balance` > RetentionDataRaw$`Credit Limit` )
 
###
conditionalldraft <- with(RetentionDataRaw, which(RetentionDataRaw$`Days Deliq` > 0 | !is.na(RetentionDataRaw$`External Status`) | RetentionDataRaw$`Opening Balance` > RetentionDataRaw$`Credit Limit` | RetentionDataRaw$`Ending Balance` > RetentionDataRaw$`Credit Limit` ))
newdata <- RetentionDataRaw[-conditionalldraft, ]

dim(newdata)

#taking only first row to dataset from new data set 
###d2 <- d1[!duplicated(d1$DebtDimId), ]

d2 <- newdata[!duplicated(newdata$DebtDimId), ]
dim(d2)

#taking only last row from new dataset
#d3 <- d1[!duplicated(d1$DebtDimId, fromLast = T), ]

d3 <- newdata[!duplicated(newdata$DebtDimId, fromLast = T), ]

#creating new variable for last row dataset

d3$Bad <- ifelse(d3$`External Status` %in% c("E", "F", "I", "Z") | d3$`Days Deliq` > 90, 1 ,0)

#Bad variable brining to first row dataset
d4 <- data.frame(d2[ , ], d3$Bad)

#keeping only first row
d5 <- subset(d4, d4$Row.Num==1)

#make sure we have bad variable with 1 and 0
summary(factor(d5$d3.Bad))




#Create New Variable
#d5$UtilizationEnding <- d5$Ending.Balance/d5$Credit.Limit
d5$UtilizationEnding <- ifelse(!d5$Ending.Balance,0, d5$Ending.Balance/d5$Credit.Limit)
d5$OverLimitFlag <- ifelse(d5$Over.limit.Amount > 0, 1, 0)
d5$NetBalchange <- d5$Ending.Balance - d5$Opening.Balance
#d5$OverLimitProp <- d5$Over.limit.Amount/d5$Credit.Limit



summary(d5)

#dropping the date and unneessary variables from dataset
d5$DebtDimId <- NULL
d5$Row.Num <- NULL
d5$ClosureReason <- NULL
d5$Open.Date <- NULL
d5$Last.Statement.Date <- NULL
d5$Cycle.Date <-NULL
d5$Month.End.Date <- NULL
d5$Last.Payment.Date <- NULL


#rename the NA in final status to open
d5$External.Status <- as.character(d5$External.Status)
d5$External.Status[is.na(d5$External.Status)] <- "O"

summary(factor(d5$External.Status))


summary(d5)

#bin two variables
#Net.Purchases.During.Cycle
#Net.Premier.Fees.Billed.During.Cycle

d5$Net.Purchases.During.Cycle.bin <- cut(d5$Net.Purchases.During.Cycle, 5)
d5$Net.Premier.Fees.Billed.During.Cycle.bin <- cut(d5$Net.Premier.Fees.Billed.During.Cycle, 5)

#drop previous version of variable which binned

d5$Net.Purchases.During.Cycle <- NULL
d5$Net.Premier.Fees.Billed.During.Cycle <- NULL


#rename the bad variable
names(d5)[names(d5)== 'd3.Bad'] <- 'Bad'


##changing the type of variables
#d5$Bad <- as.factor(d5$Bad)
d5$OverLimitFlag <- as.factor(d5$OverLimitFlag)



#making final dataset for model
names(d5)
finaldata <- subset(d5[ , -c(14:16)])
names(finaldata)




#finding all missing value for all column
sapply(d5, function(d5) sum(is.na(d5)))

##treating missing replace by zero as those are in utilization variable and come from 0/0.0
####is.nan.data.frame <- function(d5)
####do.call(cbind, lapply(d5, is.nan))

####d5[is.nan(d5)] <- 0

#check again about missing value

###sapply(d5, function(d5) sum(is.na(d5)))

##no more missing values in the dataset


#split the dataset
ind<- sample(2, nrow(finaldata), replace = T, prob = c(0.8, 0.2))
train <- finaldata[ind==1,]
test <- finaldata[ind==2,]

#logistic regression
#logisticmodel <- glm(Bad~ Months.On.Book+ External.Status  +  Days.Deliq+  Credit.Limit + Opening.Balance+ Ending.Balance  +Over.limit.Amount+    Actual.Min.Pay.Due   +  Total.Min.Pay.Due+       Net.Payments.During.Cycle   +            
     Net.Cash.Advances.During.Cycle+    Net.Behavior.Fees.Billed.During.Cycle+
    Net.Concessions.Billed.During.Cycle + OverLimitFlag   +Net.Purchases.During.Cycle.bin+Net.Premier.Fees.Billed.During.Cycle.bin, data = train, family = "binomial")


##logisticmodel <- glm(Bad~ Months.On.Book+ External.Status  +  Days.Deliq+  Credit.Limit  +  Opening.Balance+ Ending.Balance  +Over.limit.Amount+    Actual.Min.Pay.Due   +  Total.Min.Pay.Due+       Net.Payments.During.Cycle   +            
                       Net.Cash.Advances.During.Cycle+    Net.Behavior.Fees.Billed.During.Cycle+ Net.Purchases.During.Cycle.bin+        
                       Net.Premier.Fees.Billed.During.Cycle.bin, data = train, family = "binomial")

names(finaldata)


logisticmodel <- glm(Bad~., data = train, family = "binomial")
print("Summary of logistics regression")

summary(logisticmodel)


print("Prediction")
pred <- predict(logisticmodel, newdata = test, type = "response")


library(InformationValue)
library(gains)
print("Misclassification error rate")

misClassError(test$Bad, pred)

print("confusion matrix")
confusionMatrix(test$Bad, pred)


print("ROC")
pred.full <- prediction(pred, test$Bad)
perf.full <- performance(pred.full, "tpr", "fpr")
plot(perf.full, main = "ROC from logistic regression", col = 1)
abline(0,1,col="grey")

print("Performance rate")

performance(pred.full, "auc")@y.values

#ks plt
ks_stat(test$Bad, pred)
ks_stat(test$Bad, pred, returnKSTable = T)
ks_plot(test$Bad, pred)

#lift curve
loglift <- performance(pred.full,"lift","rpp")
plot(loglift, main="lift curve", colorize=T)


#gain table

print("Gains table")
actual <- ifelse(test$Bad==1,1,0)
gains.cross <- gains(actual=actual ,
                     predicted= pred,
                     groups=10)

print(gains.cross)


library(earth)
##MARS model

##simple earth function 
marsearth <- earth(Bad ~ ., data = train)

print("Summary of the MARS model")

print(marsearth)
summary(marsearth)

print("Some important plot from MARS model")
plot(marsearth, which = 1)
plot(marsearth, which = 2)
plot(marsearth, which = 3)
plot(marsearth, which = 4)


plotmo(marsearth)
plotd(marsearth)
evimp(marsearth)

print("Prediction")
test.earth.probs <- predict(marsearth, newdata = test, type = "response")
print("Misclassification error")
misClassError(test$Bad, test.earth.probs)
print("Confusion Matrix")
confusionMatrix(test$Bad, test.earth.probs)

print("ROC")
pred.full.earth <- prediction(test.earth.probs, test$Bad)
perf.full.earth <- performance(pred.full.earth, "tpr", "fpr")
plot(perf.full.earth, main = "ROC from MARS", col = 1)
abline(0,1,col="grey")

print("Performance rate")
performance(pred.full.earth, "auc")@y.values

#ks plt
ks_stat(test$Bad, test.earth.probs)
ks_stat(test$Bad, test.earth.probs, returnKSTable = T)
ks_plot(test$Bad, test.earth.probs)

#lift curve
earthlift <- performance(pred.full.earth,"lift","rpp")
plot(earthlift, main="lift curve", colorize=T)


#gain table
print("Gains table")
actual1 <- ifelse(test$Bad==1,1,0)
gains.cross1 <- gains(actual=actual1 ,
                      predicted= test.earth.probs,
                      groups=10)

print(gains.cross1)



##ref: http://rstudio-pubs-static.s3.amazonaws.com/356008_52144511fe5d4334b4c84bbf26532fd5.html

###model with good customer score

ind<- sample(2, nrow(d5), replace = T, prob = c(0.8, 0.2))
traingood <- d5[ind==1,]
testgood <- d5[ind==2,]

loggood <- glm(Bad~ Good.Customer.Score, data= traingood, family = "binomial")
summary(loggood)


print("Prediction")
predf <- predict(loggood, newdata = testgood, type = "response")


library(InformationValue)
library(gains)
print("Misclassification error rate")

misClassError(testgood$Bad, predf)

print("confusion matrix")
confusionMatrix(testgood$Bad, predf)


print("ROC")
pred.fullf <- prediction(predf, testgood$Bad)
perf.fullf <- performance(pred.fullf, "tpr", "fpr")
plot(perf.fullf, main = "ROC from logistic regression", col = 1)
abline(0,1,col="grey")

print("Performance rate")

performance(pred.fullf, "auc")@y.values


library(InformationValue)
#ks plt
ks_stat(testgood$Bad, predf)
ks_stat(testgood$Bad, predf, returnKSTable = T)
ks_plot(testgood$Bad, predf)

#lift curve
perf1 <- performance(pred.fullf,"lift","rpp")
plot(perf1, main="lift curve", colorize=T)

#gain table
print("Gains table")
actualf <- ifelse(testgood$Bad==1,1,0)
gains.crossf <- gains(actual=actualf ,
                      predicted= predf,
                      groups=10)

print(gains.crossf)

library(blorr)
library(readr)
library(ROCR)
library(earth)
library(InformationValue)
library(gains)
library(dplyr)

blr_gains_table(loggood)

loggood %>%
  blr_gains_table() %>%
  plot()

loggood %>%
  blr_gains_table() %>%
  blr_roc_curve()

loggood %>%
  blr_gains_table() %>%
  blr_ks_chart()

###


