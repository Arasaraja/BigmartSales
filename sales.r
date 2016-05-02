
train<- read.csv("C:\\Users\\431539\\Desktop\\BigMart\\TrainImp.csv",na.string ="") #8523

test<- read.csv("C:\\Users\\431539\\Desktop\\BigMart\\TestImp.csv",na.strings = "") #5681


fsales<- read.csv("E:\\Qlikview\\R\\Competition\\Sales\\sales_sample.csv",na.strings = "")


sum(is.na(train$Item_Weight))
sum(is.na(train$Outlet_Size))
str(train) #8523
str(test)#5681
sum(is.na(train))  #3873
sum(is.na(test)) #2582


sum(is.na(test$Item_Weight))
sum(is.na(test$Outlet_Size))

ptrain <- train[,-c(1,7)]
str(ptrain)

id <- test[,c(1,7)]
ptest <- test[,-c(1,7)]
str(ptest)


ptest$Item_Outlet_Sales <- NA

sales <- rbind(ptrain,ptest) #14204


unique(sales$Item_Fat_Content)
sales$Item_Fat_Content <- as.character(sales$Item_Fat_Content)
sales$Item_Fat_Content[sales$Item_Fat_Content=='low fat']<- 'LF'
sales$Item_Fat_Content[sales$Item_Fat_Content=='Low Fat']<- 'LF'  
sales$Item_Fat_Content[sales$Item_Fat_Content=='Regular']<- 'reg'  
sales$Item_Fat_Content <- as.factor(sales$Item_Fat_Content)
sum(is.na(sales$Item_Fat_Content))

unique(sales$Item_Fat_Content)

str(sales)

hist(sales$Item_Weight)
boxplot(sales$Item_Weight,horizontal = T)

####################################3
# item=sales[which(is.na(sales$Item_Weight)),]
# item=item[,-1]

# data=sales[-which(is.na(sales$Item_Weight)),]

sum(is.na(sales$Item_Weight))
sum(is.na(sales$Outlet_Size))

######Imputation#####

model <- lm(Item_Weight~.,data=sales[-which(is.na(sales$Item_Weight)),-c(6,7,8,9,10)])

pred=predict(model,newdata=sales[which(is.na(sales$Item_Weight)),-1])

sales$Item_Weight[which(is.na(sales$Item_Weight))] <- pred

##################

library(nnet)
gmodel <- multinom(Outlet_Size~.,data=sales[-which(is.na(sales$Outlet_Size)),-c(1:5,10)])

pred=predict(gmodel,newdata=sales[which(is.na(sales$Outlet_Size)),-7],type = 'class')

sales$Outlet_Size[which(is.na(sales$Outlet_Size))] <- pred

##############MICE Imputation #####################
library(mice)

miceMod <- mice(sales[, !names(sales) %in% "Item_Outlet_Sales"], method="rf")  # perform mice imputation, based on random forests.

sales <- complete(miceMod)  # generate the completed data.

anyNA(sales)

################RPart Imputation ###########################

library(rpart)

class_mod <- rpart(Outlet_Size ~ . - Item_Outlet_Sales, data=sales[!is.na(sales$Outlet_Size), ], method="class", na.action=na.omit)  # since rad is a factor

anova_mod <- rpart(Item_Weight ~ . - Item_Outlet_Sales, data=sales[!is.na(sales$Item_Weight), ], method="anova", na.action=na.omit)  # since ptratio is numeric.

rad_pred <- predict(class_mod, BostonHousing[is.na(BostonHousing$rad), ])

ptratio_pred <- predict(anova_mod, BostonHousing[is.na(BostonHousing$ptratio), ])

################################
sales#W############################
sum(is.na(sales))
salesbkp =sales
#sales = salesbkp 

# ftrain$Item_Weight <- log10(ftrain$Item_Weight)
# ftrain$Item_Fat_Content <- as.numeric(ftrain$Item_Fat_Content)
# ftrain$Item_Visibility <- scale(ftrain$Item_Visibility)
# ftrain$Item_Type <- as.numeric(ftrain$Item_Type)
# ftrain$Item_MRP <- log10(ftrain$Item_MRP)
# ftrain$Outlet_Establishment_Year <- log10(ftrain$Outlet_Establishment_Year) 
# ftrain$Outlet_Size <- as.numeric(ftrain$Outlet_Size)
# ftrain$Outlet_Location_Type <- as.numeric(ftrain$Outlet_Location_Type)
# ftrain$Outlet_Type <- as.numeric(ftrain$Outlet_Type)

sales$Item_Weight=sqrt(sales$Item_Weight)
sales$Item_Visibility =sqrt(sales$Item_Visibility)
#sales$Item_MRP = sqrt(sales$Item_MRP)
# sales$Outlet_Establishment_Year=sqrt(sales$Outlet_Establishment_Year)
sales$Item_Outlet_Sales =sales$Item_Outlet_Sales^(1/3)
sales$Outlet_Establishment_Year=as.factor(sales$Outlet_Establishment_Year)

# sales$Item_Visibility[which(sales$Item_Visibility>0.1)] = 'a'
# itevismodel = lm(Item_Visibility~.,data=sales[which(sales$Item_Visibility!='a'),c(1:5)])
# 
# preditvis= predict(itevismodel,newdata =sales[which(sales$Item_Visibility=='a'),c(1:5)])
# 
# sales$Item_Visibility[which(sales$Item_Visibility=='a')] <- preditvis
# sales$Item_Visibility = as.numeric(sales$Item_Visibility)

ftrain <- sales[1:8523,]
ftest <- sales[8524:14204,]
ftest <- ftest[,-10]

################Linear Regresssion##############################

fmodel <- lm(Item_Outlet_Sales~.,data=ftrain[,-c(4)])

fpred <- predict(fmodel,newdata = ftest)

output = cbind(id,fpred)
colnames(output)[3]='Item_Outlet_Sales'
write.csv(output,'C:\\Users\\431539\\Desktop\\BigMart\\subm.csv',row.names = FALSE)

######################################################################################################
library(randomForest)
tuneRF(ftrain[,-c(1,2,3,4,10)],ftrain[,10],ntreeTry = 100,stepFactor = 1,improve = 0.05,trace = TRUE,plot = TRUE,doBest = TRUE)

rfmodel <- randomForest(Item_Outlet_Sales ~ .,data=ftrain[,c(5,6,7,8,9,10)],ntree=200, mtry=3,importance=TRUE,replace=FALSE,do.trace=TRUE)

rfpred=predict(rfmodel,newdata = ftest)

rfpred=rfpred^3

output = cbind(id,rfpred)
colnames(output)[3]='Item_Outlet_Sales'
write.csv(output,'C:\\Users\\431539\\Desktop\\BigMart\\rfsubm.csv',row.names = FALSE)


########################################
library(gnm)

gnmmodel = gnm(Item_Outlet_Sales~.,data = ftrain[,c(1,2,3,4,5,6,7,8,9,10)],checkLinear = TRUE,
        verbose = TRUE,trace = TRUE,iterStart = 2, iterMax = 500,method = "gnmFit")

gnmpred=predict(gnmmodel,newdata=ftest)

output = cbind(id,gnmpred)
colnames(output)[3]='Item_Outlet_Sales'
write.csv(output,'C:\\Users\\431539\\Desktop\\BigMart\\gnmsubm.csv',row.names = FALSE)


#################
library(gbm)

gbmodel <- gbm(Item_Outlet_Sales~.,data = ftrain,n.trees = 500,distribution = "gaussian",
verbose = TRUE,train.fraction = 0.5,,bag.fraction = 0.5,shrinkage = 0.05)


gbpred = predict(gbmodel,newdata = ftest)

output = cbind(id,gbpred)
colnames(output)[3]='Item_Outlet_Sales'
write.csv(output,'C:\\Users\\431539\\Desktop\\BigMart\\gbmsubm.csv',row.names = FALSE)


######################################################library(dummies)
a=dummy(ftrain$Item_Type)
b=dummy(ftrain$Outlet_Size)
c=dummy(ftrain$Outlet_Location_Type)
d=dummy(ftrain$Outlet_Type)



str(ftrain)

ttrain = ftrain[,-c(4,7,8,9)]
ttrain=cbind(ttrain,a,b,c,d)
str(ttrain)

write.csv(ttrain,'E:\\Qlikview\\R\\Competition\\Sales\\ttrain.csv',row.names = FALSE)
write.csv(ftest,'E:\\Qlikview\\R\\Competition\\Sales\\ttest.csv',row.names = FALSE)

write.csv(sales,'E:\\Qlikview\\R\\Competition\\Sales\\sales.csv',row.names = FALSE)

pc <- prcomp(a, scale=TRUE, center=TRUE, tol=0)


a=ftrain[sapply(ftrain, is.numeric)]

#########################################



