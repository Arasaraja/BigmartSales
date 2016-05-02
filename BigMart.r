
train<- read.csv("C:\\Users\\431539\\Desktop\\BigMart\\Train.csv",na.string ="") #8523

test<- read.csv("C:\\Users\\431539\\Desktop\\BigMart\\Test.csv",na.strings = "") #5681

str(train)
str(test)
summary(train)
sum(is.na(train))
sum(is.na(test))

Coeffs<-lm(Item_Outlet_Sales~., data=train)

Predicted=predict(Coeffs, newdata = ptest)

id=test[,c(1,7)]
test=test[,-c(1,7)]
test$Item_Outlet_Sales = NA

train=train[,-c(1,7)]

mart=rbind(train,test)
str(mart)

sum(is.na(mart))

########Item_Weight

hist(mart$Item_Weight)

weightmodel = lm(Item_Weight ~.,data=mart[which(!is.na(mart$Item_Weight)),-c(9,10)])
preweight=predict(weightmodel,newdata = mart[which(is.na(mart$Item_Weight)),-c(9,10)])

mart$Item_Weight[which(is.na(mart$Item_Weight))]=preweight

sum(is.na(mart$Item_Weight))

######Outlet_Size
outletmodel = multinom(Outlet_Size ~.,data= mart[which(!is.na(mart$Outlet_Size)),-10])
predoutlet= predict(outletmodel , newdata = mart[which(is.na(mart$Outlet_Size)),-10],type = "class")

mart$Outlet_Size[which(is.na(mart$Outlet_Size))]=predoutlet
sum(is.na(mart$Outlet_Size))

mart$Item_Fat_Content = as.character(mart$Item_Fat_Content)
mart$Item_Fat_Content[which(mart$Item_Fat_Content=="low fat")] <- 'LF'
mart$Item_Fat_Content[which(mart$Item_Fat_Content=="Low Fat")] <- 'LF'
mart$Item_Fat_Content[which(mart$Item_Fat_Content=="Regular")] <- 'REG'
mart$Item_Fat_Content[which(mart$Item_Fat_Content=="reg")] <- 'REG'
mart$Item_Fat_Content[which(is.na(mart$Item_Fat_Content))] <- 'REG'

mart$Item_Fat_Content = as.factor(mart$Item_Fat_Content)

bmart=mart
bftrain=ftrain
bftest=ftest


ftrain=mart[1:8523,]
ftest=mart[8524:14204,]
ftest=ftest[,-10]

model=lm(Item_Outlet_Sales ~., data=ftrain)
predsales= predict(model,newdata = ftest)
Item_Outlet_Sales =predsales

Predicted = data.frame(id,Item_Outlet_Sales)


write.csv(Predicted,"C:\\Users\\431539\\Desktop\\BigMart\\pred.csv",row.names = FALSE)

write.csv(ftrain,"C:\\Users\\431539\\Desktop\\BigMart\\ftrain.csv",row.names = FALSE)


mformula <-as.formula(paste(paste(DEPENDENT$PARM_VAL), paste("Data"), sep="~"))

information.gain(Item_Outlet_Sales~.,data=train)

# dumm=dummy(ftrain$Item_Type)
# ftrain = cbind(ftrain,dumm)
# ftrain=ftrain[,-4]
# 
# dummt=dummy(ftest$Item_Type)
# ftest = cbind(ftest,dummt)
# ftest=ftest[,-4]
