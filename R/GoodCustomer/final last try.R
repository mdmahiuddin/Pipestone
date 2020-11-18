library(readxl)
library(blorr)
library(readr)
library(ROCR)
library(earth)
library(InformationValue)
library(gains)
library(dplyr)
#load the data set
RetentionDataRaw <- read_excel("C:/Users/DELETE ME METTE/Desktop/My Folder/Spring 2019/551/Final/draft final/RetentionDataRaw.xlsx")

#check the structure of the dataset
str(RetentionDataRaw)


#make only class dataframe
RetentionDataRaw <- as.data.frame(RetentionDataRaw)

#check again
str(RetentionDataRaw)


#changing the good customer score variable into numeric value
RetentionDataRaw$`Good Customer Score` <- as.character(RetentionDataRaw$`Good Customer Score`)
RetentionDataRaw$`Good Customer Score` <- as.numeric(RetentionDataRaw$`Good Customer Score`)


#details of good customer score
summary(RetentionDataRaw$`Good Customer Score`)


#check again str of main data
str(RetentionDataRaw)


##

#creating a data set for first row
datarow1 <- RetentionDataRaw[!duplicated(RetentionDataRaw$DebtDimId), ]
dim(datarow1)


#creating data set for last row
datarowlast <- RetentionDataRaw[!duplicated(RetentionDataRaw$DebtDimId, fromLast = T), ]
dim(datarowlast)



##condition apply to first row dataset
condition1 <- with(datarow1, which(datarow1$`Days Deliq` > 0 | !is.na(datarow1$`External Status`) | datarow1$`Opening Balance` > datarow1$`Credit Limit` | datarow1$`Ending Balance` > datarow1$`Credit Limit` ))
dataaftercondition1 <- datarow1[-condition1, ]
dim(dataaftercondition1)

##define bad variable 
####datarowlast$Bad <- ifelse((datarowlast$`External Status` %in% c("E", "F", "I", "Z") | datarowlast$`Days Deliq` > 90) & datarowlast$`Row Num` >= 7, 1, 0)


##need to drop last row who are not in row 1 dataset.
datalastrowcommoninrow1 <- subset(datarowlast, DebtDimId %in% dataaftercondition1$DebtDimId )
datalastrowcommoninrow1$Bad <- ifelse((datalastrowcommoninrow1$`External Status` %in% c("E", "F", "I", "Z") | datalastrowcommoninrow1$`Days Deliq` > 90) & datalastrowcommoninrow1$`Row Num` >= 7, 1, 0)
summary(factor(datalastrowcommoninrow1$Bad))



#make sure about row 7 or more
only1 <- subset(datalastrowcommoninrow1, datalastrowcommoninrow1$Bad ==1)
summary(only1$`Row Num`)

#take only bad variable and drop others
dataonlybadfromlastrow <- datalastrowcommoninrow1[c(1, 27)]

# merge two data frames by ID
datawithcondition1andBad <- merge(dataonlybadfromlastrow, dataaftercondition1 ,by="DebtDimId")
dim(datawithcondition1andBad)



#create new variables
#datawithcondition1andBad$UtilizationEnding <- datawithcondition1andBad$`Ending Balance`/datawithcondition1andBad$`Credit Limit`
datawithcondition1andBad$UtilizationEnding <- ifelse(!datawithcondition1andBad$`Ending Balance`,0, datawithcondition1andBad$`Ending Balance`/datawithcondition1andBad$`Credit Limit`)
datawithcondition1andBad$OverLimitFlag <- ifelse(datawithcondition1andBad$`Over limit Amount` > 0, 1, 0)
datawithcondition1andBad$NetBalchange <- datawithcondition1andBad$`Ending Balance` - datawithcondition1andBad$`Opening Balance`
datawithcondition1andBad$OverLimitProp <- datawithcondition1andBad$`Over limit Amount`/datawithcondition1andBad$`Credit Limit`

datawithcondition1andBad$limitbalanceprop <- (datawithcondition1andBad$`Ending Balance` + datawithcondition1andBad$`Opening Balance`)/datawithcondition1andBad$`Credit Limit`


#rename the NA in final status to open
datawithcondition1andBad$`External Status` <- as.character(datawithcondition1andBad$`External Status`)
datawithcondition1andBad$`External Status`[is.na(datawithcondition1andBad$`External Status`)] <- "O"
summary(factor(datawithcondition1andBad$`External Status`))


#binned two variables
datawithcondition1andBad$Net.Purchases.During.Cycle.bin <- cut(datawithcondition1andBad$`Net Purchases During Cycle`, 5)
datawithcondition1andBad$Net.Premier.Fees.Billed.During.Cycle.bin <- cut(datawithcondition1andBad$`Net Premier Fees Billed During Cycle`, 5)

#transfer bad numeric into factor
datawithcondition1andBad$Bad <- as.factor(datawithcondition1andBad$Bad)
summary(factor(datawithcondition1andBad$Bad))

#drop the NA for good customer score
summary(datawithcondition1andBad$`Good Customer Score`)

datawithcondition1andBad <- na.omit(datawithcondition1andBad)

dim(datawithcondition1andBad)

##Drop date and other variables
names(datawithcondition1andBad)
dataformodel <- datawithcondition1andBad[c(2, 7, 10, 11, 12, 14, 16, 18, 20, 21, 28, 32, 33, 34)]
names(dataformodel)
str(dataformodel)
summary(dataformodel)

#finding all missing value for all column
sapply(dataformodel, function(dataformodel) sum(is.na(dataformodel)))

#split the dataset
ind <- sample(2, nrow(dataformodel), replace = T, prob = c(0.8, 0.2))
train <- dataformodel[ind==1,]
test <- dataformodel[ind==2,]


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
df.KS.MARS = data.frame(test$Bad, test.earth.probs)
colnames(df.KS.MARS) = c("BadActual", "BadPredict")
InformationValue::ks_plot(df.KS.MARS$BadActual, df.KS.MARS$BadPredict)


InformationValue::ks_stat(df.KS.MARS$BadActual, df.KS.MARS$BadPredict)
InformationValue::ks_stat(df.KS.MARS$BadActual, df.KS.MARS$BadPredict, returnKSTable = T)



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

###model with good customer score

dataforgoodmodel <- datawithcondition1andBad[c(2,27)]

##lots of NA value, drop them
dataforgoodmodel <- na.omit(dataforgoodmodel)


ind<- sample(2, nrow(dataforgoodmodel), replace = T, prob = c(0.8, 0.2))
traingood <- dataforgoodmodel[ind==1,]
testgood <- dataforgoodmodel[ind==2,]

loggood <- glm(Bad~ `Good Customer Score`, data= traingood, family = "binomial")
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
plot(perf.fullf, main = "ROC from logistic regression with good score", col = 1)
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

#MARS for good customer
library(earth)
##MARS model

##simple earth function 
marsgood <- earth(Bad ~ ., data = traingood)


print("Summary of the MARS model")

print(marsgood)
summary(marsgood)

print("Some important plot from MARS model")
plot(marsgood, which = 1)
plot(marsgood, which = 2)
plot(marsgood, which = 3)
plot(marsgood, which = 4)


plotmo(marsgood)
plotd(marsgood)
evimp(marsgood)

print("Prediction")
test.earth.probs.good <- predict(marsgood, newdata = testgood, type = "response")
print("Misclassification error")
misClassError(testgood$Bad, test.earth.probs.good)
print("Confusion Matrix")
confusionMatrix(testgood$Bad, test.earth.probs.good)

print("ROC")
pred.full.earth.good <- prediction(test.earth.probs, testgood$Bad)
perf.full.earth.good <- performance(pred.full.earth.good, "tpr", "fpr")
plot(perf.full.earth.good, main = "ROC from MARS", col = 1)
abline(0,1,col="grey")

print("Performance rate")
performance(pred.full.earth.good, "auc")@y.values

#ks plt
ks_stat(testgood$Bad, test.earth.probs.good)
ks_stat(testgood$Bad, test.earth.probs.good, returnKSTable = T)
ks_plot(testgood$Bad, test.earth.probs.good)

#lift curve
earthlift.good <- performance(pred.full.earth.good,"lift","rpp")
plot(earthlift.good, main="lift curve", colorize=T)


#gain table
print("Gains table")
actual1.good <- ifelse(testgood$Bad==1,1,0)
gains.cross1.good <- gains(actual=actual1.good ,
                      predicted= test.earth.probs.good,
                      groups=10)

print(gains.cross1.good)



#Are we making profit
mydata <- RetentionDataRaw <- read.csv("C:/Users/DELETE ME METTE/Desktop/My Folder/Spring 2019/551/Final/draft final/RetentionDataRaw.csv")

mydata$Net.Purchases.During.Cycle <- as.integer(mydata$Net.Purchases.During.Cycle)
profitdataforallcustomer <-aggregate(mydata$Net.Purchases.During.Cycle, by=list(DebtDimId=mydata$DebtDimId), sum)

# merge two data frames by ID
profitidandbad <- merge(dataonlybadfromlastrow, profitdataforallcustomer , by="DebtDimId")
dim(profitidandbad)

profitfromgood <- subset(profitidandbad, profitidandbad$Bad==0)
summary(profitfromgood)
colSums(profitfromgood)


profitfrombad <- subset(profitidandbad, profitidandbad$Bad==1)

summary(profitfrombad)
colSums(profitfrombad)



