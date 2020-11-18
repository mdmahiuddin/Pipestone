library(readxl)

RetentionDataRaw <- read_excel("C:/Users/DELETE ME METTE/Desktop/My Folder/Spring 2019/551/Final/draft final/RetentionDataRaw.xlsx")

dim(RetentionDataRaw)
names(RetentionDataRaw)

RetentionDataRaw1 <- RetentionDataRaw[ , -c(24:26) ]

RetentionDataRaw2 <- subset(RetentionDataRaw1, RetentionDataRaw1$`Row Num`==1)

count<- unique(RetentionDataRaw1$DebtDimId)

length(count)


nrow(RetentionDataRaw2)
#condition_0 <- subset(RetentionDataRaw, !`Row Num`==1)
#condition1 <- subset(RetentionDataRaw1, `Row Num`==1)

#condition2 <- subset(condition1, !condition1$`Days Deliq` > 0)

#summary(factor(condition2$`External Status`))

#rename the NA to Open in ext status
#condition2$`External Status` <- as.character(condition2$`External Status`)
#condition2$`Ext Status` <- as.character(condition2$`Ext Status`)
#condition2$`External Status`[is.na(condition2$`External Status`)] <- Open
#condition2$`Ext Status`[is.na(condition2$`Ext Status`)] <- "Open"


#condition3 <- subset(condition2, is.na(condition2$`External Status`))

summary(factor(RetentionDataRaw2$`External Status`))

table(RetentionDataRaw2$`External Status`, useNA = "ifany")

#condition4 <- subset(condition3, condition3$`Opening Balance` > condition3$`Credit Limit`)

#condition5 <- subset(condition4, condition4$`Ending Balance` > condition4$`Credit Limit`)


#conditionall <- subset(RetentionDataRaw2, RetentionDataRaw2$`Days Deliq` > 0 | !is.na(RetentionDataRaw2$`External Status`) | RetentionDataRaw2$`Opening Balance` > RetentionDataRaw2$`Credit Limit` | RetentionDataRaw2$`Ending Balance` > RetentionDataRaw2$`Credit Limit` )

conditionalldraft <- with(RetentionDataRaw2, which(RetentionDataRaw2$`Days Deliq` > 0 | !is.na(RetentionDataRaw2$`External Status`) | RetentionDataRaw2$`Opening Balance` > RetentionDataRaw2$`Credit Limit` | RetentionDataRaw2$`Ending Balance` > RetentionDataRaw2$`Credit Limit` ))

newdata <- RetentionDataRaw2[-conditionalldraft, ]

#conditionall1 <- subset(RetentionDataRaw2, RetentionDataRaw2$`Days Deliq` < 30 | is.na(RetentionDataRaw2$`External Status`) | RetentionDataRaw2$`Opening Balance` <= RetentionDataRaw2$`Credit Limit` | RetentionDataRaw2$`Ending Balance` <= RetentionDataRaw2$`Credit Limit`)

#conditionall2 <- RetentionDataRaw2[!conditionall]

#closed vs not closed


#summary(factor(condition_secondary$`External Status`))

#condition_secondary$bad <- ifelse(condition_secondary$`Months On Book` >= 7, 1, 0)

#summary(factor(condition_secondary$bad))


#condition1_2 <- subset(RetentionDataRaw, `Days Deliq` ==  0)
#condition1_3 <- subset(condition1_2, `Opening Balance` > `Credit Limit` )
#condition1_4 <- subset(condition1_3, condition1_3$`Ending Balance` > `Credit Limit` )
#condition_5 <- subset(condition1_4, !`Row Num` == 1)


###all conditions at a time




#------------------------------------------------------------------------------------------
# CONDENSE DATA TO ONE ROW PER CUSTOMER
#------------------------------------------------------------------------------------------
# Have to specify certain IDs which do not have a Row Num = 1
#mod.df <- RetentionDataRaw[`Row Num` == 1 | (DebtDimId == "10336095" & `Row Num` == 2) | (DebtDimId == "18492458" & `Row Num` == 2) |(DebtDimId == "18757462" & `Row Num` == 2) | (DebtDimId == "19487137" & `Row Num` == 2) | (DebtDimId == "21170332" & `Row Num` == 2)]

# Remove duplicate rows
#mod.df <- mod.df[!duplicated(mod.df$DebtDimId), ]



inputs = RetentionDataRaw1[!duplicated(RetentionDataRaw1$DebtDimId),]
dim(inputs)

outputs= RetentionDataRaw1[!duplicated(RetentionDataRaw1$DebtDimId,fromLast=T),]
dim(outputs)




#total <- rbind(inputs, outputs)

#attach(total)
#total <- total[order(DebtDimId),] 


#new conditons
#t1 <- with(total, which(total$`Days Deliq` > 0 | !is.na(total$`External Status`) | total$`Opening Balance` > total$`Credit Limit` | total$`Ending Balance` > total$`Credit Limit` ))
#newdata1 <- total[-t1, ]


##
#outputs[is.na(outputs)] <-"O"

#summary(factor(total$`External Status`))

##define the bad variable
#outputs$Bad <- ifelse(outputs$`Days Deliq` > 90, 1, ifelse(outputs$`External Status` == "E", 1, ifelse(outputs$`External Status` == "F", 1, ifelse(outputs$`External Status`== "I", 1, ifelse(outputs$`External Status`== "Z", 1, ifelse(outputs$`Months On Book` > 7, 1, 0))))))

#1(9803) and 0(194)



###try again

#total$Bad <- with(total, ifelse( `External Status` %in% c("E", "F", "I", "Z") | total$`Days Deliq` > 90 | total$`Months On Book` > 7, 1,0))


outputs$Bad <- with(outputs, ifelse(`External Status` %in% c("E", "F", "I", "Z") | outputs$`Days Deliq` > 90, 1 ,0))


datarow1 <- data.frame(inputs[ , ], outputs$Bad)





summary(factor(datarow1$outputs.Bad))


#finaldata <- subset(datarow1, datarow1$Months.On.Book >7) 

#dim(finaldata)

#summary(factor(finaldata$outputs.Bad))


finaldata1 <- with(datarow1, which(datarow1$`Days Deliq` > 0 | !is.na(datarow1$`External Status`) | datarow1$`Opening Balance` > datarow1$`Credit Limit` | datarow1$`Ending Balance` > datarow1$`Credit Limit` ))

length(finaldata1)



dim(datarow1)

##why not second type of Bad?

summary(factor(finaldata$External.Status))

##creating some more variables

#Create New Variable
finaldata$UtilizationEnding <- finaldata$Ending.Balance/finaldata$Credit.Limit
#finaldata$UtilizationBegin <- finaldata$Opening.Balance /(finaldata$Credit.Limit++ 0.00000000000001)
finaldata$OverLimitFlag <- ifelse(finaldata$Over.limit.Amount > 0, 1, 0)
finaldata$NetBalchange <- finaldata$Ending.Balance - finaldata$Opening.Balance
#finaldata$PayRatio <- finaldata$Net.Payments.During.Cycle/finaldata$Total.Min.Pay.Due
finaldata$OverLimitProp <- finaldata$Over.limit.Amount/finaldata$Credit.Limit

#phase2data$Utilization <- phase2data$Ending.Balance/phase2data$Credit.Limit
#phase2data$Over.Limit.Flag <- ifelse(phase2data$Over.limit.Amount>0,1,0)
#phase2data$Net.Balance.Change <- phase2data$Ending.Balance-phase2data$Opening.Balance

# Create ratio of net payments to minimum owed
###mod.df$pay_ratio <- mod.df$`Net Payments During Cycle`/ mod.df$`Total Min Pay Due`
# Create ratio of net purchases to net payments variable
###mod.df$bal_ratio <- mod.df$`Credit Limit`/ (mod.df$`Opening Balance` + 0.00000000000001)
# Create variable for proportion over the limit
###mod.df$PropOverLimit <- mod.df$`Over limit Amount` / mod.df$`Credit Limit`
# Create variable
####mod.df$LimitRatio <- mod.df$`Credit Limit` / mod.df$`Months On Book`

#name of the variables
names(finaldata)


library(ggplot2)
ggplot(finaldata, aes(x=Credit.Limit , y= outputs.Bad))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess", colour="blue", size=1.5)+
  xlab("Credit Limit")+
  ylab("Bad")+
  theme_bw()

##drop some variables

finaldata$DebtDimId <- NULL
finaldata$Row.Num <- NULL
finaldata$ClosureReason <- NULL
finaldata$Open.Date <- NULL
finaldata$Last.Statement.Date <- NULL
finaldata$Cycle.Date <-NULL
finaldata$Month.End.Date <- NULL
finaldata$Last.Payment.Date <- NULL

dim(finaldata)

summary(finaldata)
##Important variables: payment history, credit utilization, length of credit history

