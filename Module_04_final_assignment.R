#Bank tele-marketing 
ydata <- read.csv(file = "C:\\DataSets\\Bank-full Dataset.csv", header = TRUE, sep = ";") 


tr(mydata) 
ROW(mydata) 
able(mydata$y) 
lass(mydata) 
dit(mydata) 


#Check how many unknowns are for Job 
unique(mydata$job) 
table(mydata$job) 
#Remove Unknows for Job 
mydata1 <- subset(mydata, subset=(job!="unknown")) 
table((mydata1$job)) 
unique(mydata1$job) 


#Check how many unknowns are for Education 
unique(mydata1$education) 
table(mydata1$education) 
#Remove Unknowns for Education 
mydata2 <- subset(mydata1, subset=(education!="unknown")) 
unique(mydata2$education) 
table(mydata2$education) 


unique(mydata2$contact) 
table(mydata2$contact) 


unique(mydata2$default) 
table(mydata2$default) 
prop.table(table(mydata2$default)) 
barplot(table(mydata2$default))#we will remove default, as yes % is less than 1% 


unique(mydata2$poutcome) 
table(mydata2$poutcome)# we will remove poutcome, as unknown is more 


#duration filed also should be removed, as the whole business objective is to get rid of calls 
# and predict before making the calls 


#remove the variables which are not required 
mydata3 <- subset(mydata2, select = -c(default, duration, poutcome, contact)) 
str(mydata3) 


#recode y 
table(mydata$y) 
mydata3$yvar <- ifelse(mydata3$y == "yes", 1, 2) 
table(mydata3$yvar) 
str(mydata3) 


# save the dataset 
write.csv(mydata3, file = "C:\\DataSets\\BankDataAll_Clean.csv") 


#convert yvar which is numeric now, as factor 
mydata3$yvar <- as.factor(mydata3$yvar) 
str(mydata3) 


#remove y, as it is not required 
mydata4 <- subset(mydata3, select = -c(y)) 
table(mydata4$yvar) 
NROW(mydata4) 
NROW(mydata) 


#we lost 3000 records, which is fine for simplicity 


unique(mydata4$pdays) 
table(mydata4$pdays)# let it be like this 


barplot(table(mydata4$yvar)) 
table(mydata4$yvar) 
prop.table(table(mydata4$yvar)) 


library(ggplot2) 
hist(mydata4$age) 


ggplot() +geom_bar(data = mydata4, aes(x=(mydata4$age), fill = factor(mydata4$yvar)), position = "fill") 


ggplot() +geom_bar(data = mydata4, aes(x=(mydata4$marital), fill = factor(mydata4$yvar)), position = "fill") 


hist(mydata4$balance) 


#create train and test data set 


set.seed(123) 
NROW(mydata4) 
datamixed = mydata4[order(runif(43193)), ]#shuffle the data 
traindata <- datamixed[1:30235, ] 
testdata <- datamixed[30236:43193, ] 


#use C5 
#library(C50) 
#modelc5 <- C5.0(traindata$yvar~., data = traindata) 
#str(mydata4) 
#predictedc5 = predict(modelc5, testdata[,1:12]) 
#plot(modelc5) 


table(traindata$yvar) 
prop.table(table(traindata$yvar)) 


#remove data from yvar = 2 so that it becomes 30:70 for 1:2 
orderddata<-traindata[order(-xtfrm(traindata$yvar)),]#order data..order command works for numerical data, here I am doing stransfor order - so that data with 2 come at first..means descedning 
barplot(table(orderddata$yvar)) 
NROW(orderddata) 
table(orderddata$yvar) 


sampletraindata1 <- orderddata[18656:30235, ] 
barplot(table(sampletraindata1$yvar)) 
barplot(table(mydata4$yvar)) 




#use C5 
modelc5_u1 <- C5.0(sampletraindata1$yvar~., data = sampletraindata1) 
#str(mydata4) 
predictedc5_u1 = predict(modelc5_u1, testdata[,1:12]) 
plot(predictedc5_u1) 


library(caret) 
confusionMatrix(predictedc5_u1, testdata[, 13]) 


#remove data from yvar = 2 so that it becomes 40:60 for 1:2 
sampletraindata2 <- orderddata[21559:30235, ] 
barplot(table(sampletraindata2$yvar)) 
modelc5_u2 <- C5.0(sampletraindata2$yvar~., data = sampletraindata2) 
predictedc5_u2 = predict(modelc5_u2, testdata[,1:12]) 
confusionMatrix(predictedc5_u2, testdata[, 13]) 


sampletraindata2 <- orderddata[21559:30235, ] 
barplot(table(sampletraindata2$yvar)) 
modelc5_u3 <- C5.0(sampletraindata2$yvar~., data = sampletraindata2, trials = 1) 
predictedc5_u3 = predict(modelc5_u3, testdata[,1:12]) 
confusionMatrix(predictedc5_u3, testdata[, 13]) 


sampletraindata2 <- orderddata[21559:30235, ] 
modelc5_u4 <- C5.0(sampletraindata2$yvar~., data = sampletraindata2, trials = 2) 
predictedc5_u4 = predict(modelc5_u4, testdata[,1:12]) 
confusionMatrix(predictedc5_u4, testdata[, 13]) 


sampletraindata2 <- orderddata[21559:30235, ] 
modelc5_u4 <- C5.0(sampletraindata2$yvar~., data = sampletraindata2, trials = 100) 
predictedc5_u4 = predict(modelc5_u4, testdata[,1:12]) 
confusionMatrix(predictedc5_u4, testdata[, 13]) 


sampletraindata2 <- orderddata[21559:30235, ] 
modelc5_u4 <- C5.0(sampletraindata2$yvar~., data = sampletraindata2, trials = 100, rules = TRUE) 
predictedc5_u4 = predict(modelc5_u4, testdata[,1:12]) 
confusionMatrix(predictedc5_u4, testdata[, 13]) 




#run neural net 
library(nnet) 
modelnnet <- nnet(sampletraindata1$yvar~., size = 7, data = sampletraindata1) 
library(NeuralNetTools) 
NeuralNetTools::plotnet(modelnnet) 
predictednet = predict(modelnnet, testdata[,1:12], type="class") 
confusionMatrix(predictednet, testdata[, 13]) 




library(kernlab) 
musk_svm = ksvm(yvar ~ ., data = sampletraindata1, kernel = "vanilladot") 
predictedsvm = predict(musk_svm, testdata[,1:12]) 
confusionMatrix(predictedsvm, testdata[, 13]) 