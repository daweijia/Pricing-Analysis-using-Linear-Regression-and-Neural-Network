# Dawei Jia MSMA
setwd("/Users/jiadawei/Desktop/Market Analytics")
HW4DB <- read.csv("Homework 4 Student Data.csv")

## Part1: Pricing with LM
# a) most popular productNum
units_by_productNum <- aggregate(units~productNum,data = HW4DB, sum)
which.max(units_by_productNum$units) # no.89 is most popular product
# b) build upcfile
upcFile <- HW4DB[which(HW4DB$productNum==89),]
# c) regression
aggUPCFile <- aggregate(cbind(totalCost,units)~weekInYearNum+overallWeekNum+
                          storeNum+isFeature+isDisplay,data=upcFile,FUN = sum)
aggUPCFile$pricePerCan <- aggUPCFile$totalCost/aggUPCFile$units
model1 <- lm(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+
               factor(storeNum),data=aggUPCFile)
# d) newdata dataset
possiblePrices <- data.frame(price = seq(0,10,.01)) 
possiblePrices$demand <- NA
newData <- data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,
                      isDisplay=FALSE,pricePerCan=possiblePrices$price)
# e) predict demand
predicted_demand <- exp(predict(model1,newData))
# f) expected profit
expected_profit <- predicted_demand*(newData$pricePerCan-0.3)
# g) optimal price
which.max(expected_profit) #no.106
expected_profit[106]#2.083353 optimal expected profit
newData$pricePerCan[106] #1.05 optimal price
# h) repeat 1c-1g
model2 = lm(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile)
predicted_demand2 <- exp(predict(model2,newData))
expected_profit2 <- predicted_demand2*(newData$pricePerCan-0.3)
which.max(expected_profit2) #no.58
expected_profit2[58] #3.185952 optimal expected profit
newData$pricePerCan[58] #0.57 optimal price

## Part2: Pricing with nnet
# a) nnt models
library('nnet')
set.seed(1)
nnet1 = nnet(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+
               factor(storeNum),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)
nnet2 = nnet(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile,
             skip=TRUE,size=3,linout=1,maxit=10000)
# b) predict demand
nnt1_predict_50 <- exp(predict(nnet1,newData[which(newData$pricePerCan==0.50),])) # demand = 8.352401
nnt2_predict_50 <- exp(predict(nnet2,newData[which(newData$pricePerCan==0.50),])) # demand = 11.78383
nnt1_predict_100 <- exp(predict(nnet1,newData[which(newData$pricePerCan==1),])) # demand = 2.438454
nnt2_predict_100 <- exp(predict(nnet2,newData[which(newData$pricePerCan==1),])) # demand = 2.056536
nnt1_predict_50-nnt1_predict_100 #difference 5.913947
nnt2_predict_50-nnt2_predict_100 #difference 9.727299
# c) predict expected profit
#nnt1
predicted_demand_nnt1 <- exp(predict(nnet1,newData))
expected_profit_nnt1 <- predicted_demand_nnt1*(newData$pricePerCan-0.3)
which.max(expected_profit_nnt1) #no.1001
expected_profit_nnt1[1001] #291737.7 optimal expected profit
newData$pricePerCan[1001] #10 optimal price # strange output
#nnt2
predicted_demand_nnt2 <- exp(predict(nnet2,newData))
expected_profit_nnt2 <- predicted_demand_nnt2*(newData$pricePerCan-0.3)
which.max(expected_profit_nnt2) #no.72
expected_profit_nnt2[72] #4.52583 optimal expected profit
newData$pricePerCan[72] #0.71 optimal price

# d) strang output
plot(newData$pricePerCan,expected_profit_nnt1)
plot(newData$pricePerCan,expected_profit_nnt2)
min(aggUPCFile$pricePerCan) #minimun is 0.29
max(aggUPCFile$pricePerCan) #maximun is 1.19
min(newData$pricePerCan) #minimun is 0
max(newData$pricePerCan) #maximun is 10
# fix the strange output
newData2 <- newData[30:120,]
predicted_demand_nnt1_2 <- exp(predict(nnet1,newData2))
expected_profit_nnt1_2 <- predicted_demand_nnt1_2*(newData2$pricePerCan-0.3)
which.max(expected_profit_nnt1_2) #no.39
expected_profit_nnt1_2[39] #3.354292 optimal expected profit
newData2$pricePerCan[39] #0.67 optimal price
plot(newData2$pricePerCan,expected_profit_nnt1_2)
