#Home Loan disbursement prediction
#load data 
Home_Loan <- read.csv("D:\\01_Reva\\PDGM\\02_Applied_Statistics_Mithun\\HomeLoan.csv", sep='|', header = TRUE, na.strings = c("", " ", NA) )
dim(Home_Loan)

#Find summary of all columns in the raw files
#install.packages("mlr")
#library(mlr)
summary(Home_Loan)
summarizeColumns(Home_Loan)
str(Home_Loan)

#find out no of missing values in dataframe
sapply(Home_Loan, function(x) sum(is.na(x)))

#Convert Credit_Histoty to factor
Home_Loan$Credit_History <- as.factor(Home_Loan$Credit_History)
str(Home_Loan)


#Impute Gender
#library(Hmisc)
summary(Home_Loan$Gender)
Home_Loan$Gender[is.na(Home_Loan$Gender)] = 'Male'
summary(Home_Loan$Gender)


#Impute Married
summary(Home_Loan$Married)
Home_Loan$Married[is.na(Home_Loan$Married)] = 'Yes'
summary(Home_Loan$Married)


#Impute Dependents
summary(Home_Loan$Dependents)
Home_Loan$Dependents[is.na(Home_Loan$Dependents)] = '0'
summary(Home_Loan$Dependents)


#Impute Self_Employed
summary(Home_Loan$Self_Employed)
Home_Loan$Self_Employed[is.na(Home_Loan$Self_Employed)] = 'No'
summary(Home_Loan$Self_Employed)

#Impute Credit_History
summary(Home_Loan$Credit_History)
Home_Loan$Credit_History[is.na(Home_Loan$Credit_History)] = '1'
summary(Home_Loan$Credit_History)

#Impute LoanAmount
summary(Home_Loan$LoanAmount)
Home_Loan$LoanAmount[is.na(Home_Loan$LoanAmount)] = mean(Home_Loan$LoanAmount, na.rm = TRUE)
summary(Home_Loan$LoanAmount)

#Impute Loan_Amount_Term
summary(Home_Loan$Loan_Amount_Term)
Home_Loan$Loan_Amount_Term[is.na(Home_Loan$Loan_Amount_Term)] = mean(Home_Loan$Loan_Amount_Term, na.rm = TRUE)
summary(Home_Loan$Loan_Amount_Term)

#find out no of missing values in dataframe
sapply(Home_Loan, function(x) sum(is.na(x)))

summary(Home_Loan)

#check  and remove outlier for ApplicantIncome
boxplot(Home_Loan$ApplicantIncome)
boxplot.stats(Home_Loan$ApplicantIncome, coef = 2)$out
hist(Home_Loan$ApplicantIncome)
Home_Loan_SubSet1 <- subset(Home_Loan, ApplicantIncome < 12000)
boxplot(Home_Loan_SubSet1$ApplicantIncome)
summary(Home_Loan_SubSet1)


#check and remove outlier for CoapplicantIncome
boxplot.stats(Home_Loan_SubSet1$CoapplicantIncome, coef = 2)$out
hist(Home_Loan_SubSet1$CoapplicantIncome)
summary(Home_Loan_SubSet1)
Home_Loan_SubSet2 <- subset(Home_Loan_SubSet1, CoapplicantIncome < 7200)
summary(Home_Loan_SubSet2)

#check and remove outlier for LoanAmount
boxplot(Home_Loan_SubSet2$LoanAmount)
boxplot.stats(Home_Loan_SubSet2$LoanAmount, coef = 2)$out
hist(Home_Loan_SubSet2$LoanAmount)
summary(Home_Loan_SubSet2)
Home_Loan_SubSet3 <- subset(Home_Loan_SubSet2, LoanAmount < 280)
summary(Home_Loan_SubSet3)
boxplot(Home_Loan_SubSet3$LoanAmount)

#Check dependency between Loan Status and Credit History
table(Home_Loan_SubSet3$Loan_Status)
summary(Home_Loan_SubSet3)
#Find frequency
#install.packages
library(gmodels)
CrossTable(Home_Loan_SubSet3$Credit_History, Home_Loan_SubSet3$Loan_Status)
tabl1 <- table(Home_Loan_SubSet3$Credit_History, Home_Loan_SubSet3$Loan_Status)
tabl1
chi2 = chisq.test(Home_Loan_SubSet3$Credit_History, Home_Loan_SubSet3$Loan_Status)
chi2
#p value is less than 0.05 and we can infer that there is dependency between
#CreditHistory and Loan Status


#Check if there is any relationship between Avg Loan Amount and Gender
#https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable
boxplot(Home_Loan_SubSet3$LoanAmount ~ Home_Loan_SubSet3$Gender)
#In the boxplot, we can observe that Mean Loan Amount is higher for Male
#Run anova test
mod1 = lm(Home_Loan_SubSet3$LoanAmount ~ Home_Loan_SubSet3$Gender, data = Home_Loan_SubSet3)
summary(mod1)
#Since p value is significantly less, so it can be inferred that there is differnce in means
#and there is relationship between the 2 variables

#Build Logistic regression model
summary(Home_Loan_SubSet3)
Home_Loan = Home_Loan_SubSet3[, 2:13]
summary(Home_Loan)

#split the dataset into train and test
#install.packages('caTools')
library(caTools)
set.seed(123)# random seed
split = sample.split(Home_Loan$Loan_Status, SplitRatio = 0.75) # it returns true if the observation goes to training set, if it goes to test set, then it returns false
training_set = subset(Home_Loan, split == TRUE)
test_set = subset(Home_Loan, split == FALSE)

str(Home_Loan)

#reference is https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
#check missing value
colSums(is.na(training_set))
colSums(is.na(test_set))

model = glm(formula = Loan_Status ~ ., 
            family = binomial(link = 'logit'),
            data = training_set,
            control = list(maxit = 50))
#summary(model)

#run anova 
#anova(model, test="Chisq")

#predicting test set result
#summary(test_set)

#subset(test_set, select = c())


#predicting test set result
prob_pred = predict(model, type = 'response', newdata = test_set[-12])
prob_pred
y_pred = ifelse((prob_pred > 0.5), 1, 0)
y_pred


#making confusion matrix
cm = table(test_set[, 12], y_pred)
cm

#ROCR curve
#install.packages("ROCR")
library(ROCR)
pr = prediction(prob_pred, test_set$Loan_Status)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#calculate auc
auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
auc

#auc is .8315038