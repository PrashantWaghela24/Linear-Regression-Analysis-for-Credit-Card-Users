library(readxl)
library(car)
library(performance)
library(see)
library(patchwork)

#Right Tail - Log, Root
#Left Tail Skew - Square or higher Power



creditData=read_excel('~/Documents/Stats Assignment/Credit_v2.xlsx')
summary(creditData)
names(creditData)

cor(creditData)
par(mfrow=c(3,3))
hist(creditData$age,main="Histogram for Age",xlab="Age",ylab="Frequency",col="light blue")
#pairs(creditData,panel = panel.smooth)

hist(creditData$ed,main="Histogram for Education",xlab="Education Level",ylab="Frequency",col="light blue")
hist(creditData$employ,main="Histogram for Employment",xlab="Years of Employment",ylab="Frequency",col="light blue")
hist(creditData$address,main="Histogram for Address",xlab="Years at same Address",ylab="Frequency",col="light blue")
hist(log(cleanData$income),main="Histogram for Income",xlab="Total Household Income",ylab="Frequency",col="light blue")
hist(sqrt(cleanData$debtinc),main="Histogram for Debt to Income Ratio",xlab="Debt to Income Ratio",ylab="Frequency",col="light blue")
hist(sqrt(cleanData$creddebt),main="Histogram for Credit Card Debt",xlab="Credit Card Debt",ylab="Frequency",col="light blue")
hist(sqrt(cleanData$othdebt),main="Histogram for Other Debt",xlab="Other Debt",ylab="Frequency",col="light blue")
hist(creditData$default,main="Histogram for Defaults",xlab="Default",ylab="Frequency",col="light blue")


#Removal of Outliers
upperIncome=IQR(creditData$income)*1.5+(quantile(creditData$income,0.75))
upperEmploy=IQR(creditData$employ)*1.5+(quantile(creditData$employ,0.80))
upperAddress=IQR(creditData$address)*1.5+(quantile(creditData$address,0.80))
upperDebtinc=IQR(creditData$debtinc)*1.5+(quantile(creditData$debtinc,0.80))
upperCreddebt=IQR(creditData$creddebt)*1.5+(quantile(creditData$creddebt,0.80))
upperOthdebt=IQR(creditData$othdebt)*1.5+(quantile(creditData$othdebt,0.80))

IQR(creditData$income)
quantile(creditData$income,0.75)
cleanData=subset(creditData,(income<=upperIncome & debtinc<=upperDebtinc & creddebt<=upperCreddebt & othdebt<=upperOthdebt & employ<=upperEmploy & address<=upperAddress))

par(mfrow=c(2,2))
#hist(log(cleanData$employ),main="Histogram for Income",xlab="Total Household Income",ylab="Frequency",col="light blue")
hist(log(cleanData$income),main="Histogram for Income",xlab="Total Household Income",ylab="Frequency",col="light blue")
hist(sqrt(cleanData$debtinc),main="Histogram for Debt to Income Ratio",xlab="Debt to Income Ratio",ylab="Frequency",col="light blue")
hist(sqrt(cleanData$creddebt),main="Histogram for Credit Card Debt",xlab="Credit Card Debt",ylab="Frequency",col="light blue")
hist(sqrt(cleanData$othdebt),main="Histogram for Other Debt",xlab="Other Debt",ylab="Frequency",col="light blue")

cor(cleanData)
#par(mfrow=c(1,7))
boxplot(cleanData$age,cleanData$employ,cleanData$address,cleanData$income,cleanData$debtinc,cleanData$creddebt,cleanData$othdebt, col="#CB9311",names = c("age","employ","address","income","debtinc","creddebt","othddebt"))

nrow(cleanData)

par(mfrow=c(2,2))

test2=lm(sqrt(creddebt)~log(income),data=cleanData)
test3=lm(sqrt(creddebt)~sqrt(debtinc),data=cleanData)
test4=lm(sqrt(creddebt)~sqrt(othdebt),data=cleanData)

test5=lm(sqrt(creddebt)~log(income)*sqrt(debtinc)*sqrt(othdebt),data=cleanData)
#test6=update(test5,~.-log(income):sqrt(debtinc):sqrt(othdebt))
#test7=update(test6,~.-sqrt(debtinc))
#test8=update(test7,~.-log(income))
#test9=update(test8,~.-sqrt(othdebt):sqrt(debtinc))
test10=lm(sqrt(creddebt)~log(income^(1/4)):log(debtinc)+sqrt(othdebt):sqrt(debtinc),data=cleanData)

plot(test10)
summary(test10)
ncvTest(test10)
vif(test10)
durbinWatsonTest(test10)
hist(test10$residuals)
check_model(test10)

plot(creditData$employ,creditData$creddebt,main="Scatter Plot: Employ Vs Creddebt",xlab="Employ",ylab="Credit Debt",col="#CB9311")
#abline(lsfit(creditData$age,creditData$creddebt),col="red")
abline(line(creditData$employ,creditData$creddebt), col="black",lwd=2)

plot(creditData$income,creditData$creddebt,main="Scatter Plot: Income Vs Creddebt",xlab="Income",ylab="Credit Debt",col="#CB9311")
#abline(lsfit(creditData$age,creditData$creddebt),col="red")
abline(line(creditData$income,creditData$creddebt), col="black",lwd=2)

plot(creditData$debtinc,creditData$creddebt,main="Scatter Plot: Debt Income Ratio Vs Creddebt",xlab="Debt Income Ratio",ylab="Credit Debt",col="#CB9311")
#abline(lsfit(creditData$age,creditData$creddebt),col="red")
abline(line(creditData$debtinc,creditData$creddebt), col="black",lwd=2)

plot(creditData$othdebt,creditData$creddebt,main="Scatter Plot: Other Debt Vs Creddebt",xlab="Other Debt",ylab="Credit Debt",col="#CB9311")
#abline(lsfit(creditData$age,creditData$creddebt),col="red")
abline(line(creditData$othdebt,creditData$creddebt), col="black",lwd=2)

