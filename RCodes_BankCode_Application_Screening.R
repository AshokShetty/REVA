# 1. Perform hypothesis tests to see if 1) there is a relationship between checking account status and defaults and 2) there is a relationship between loan use and default.
# 2. Build a model to predict default. Score new applicants using the model build on historical data. Alternatively, add the option to weight by mis-classification cost.
 

library(foreign)
#load the datasets
dataset_german = read.spss("D:\\01_Reva\\PDGM\\05_Predictive_Analytics_with_IBM_Arnab_Adhikari\\GermanCredit.sav", to.data.frame = TRUE)
dataset_newapplicant = read.spss("D:\\01_Reva\\PDGM\\05_Predictive_Analytics_with_IBM_Arnab_Adhikari\\NewApplicants.sav", to.data.frame = TRUE)

dataset_german_orig = dataset_german
dataset_newapplicant_orig = dataset_newapplicant

str(dataset_german)

summary(dataset_german$CREDIT)

summary(dataset_german)

#check for null
sapply(dataset_german, function(x) sum(is.na(x)))

dataset_german1 = dataset_german

#dataset_german$CREDIT <- factor(dataset_german$CREDIT, levels = c(0,1))
#dataset_german$CREDIT[dataset_german$CREDIT=='GOOD']<- 1

#dataset_german$CREDIT[dataset_german$CREDIT == "BAD"] <- "0"
#dataset_german$CREDIT[dataset_german$CREDIT == "GOOD"] <- "1"



#str(dataset_german)

#boxplot(dataset_german$DURATION)
#dataset_german_subset1 <- subset(dataset_german, dataset_german$DURATION<45)
#boxplot(dataset_german_subset1$DURATION)
#summary(dataset_german_subset1)

#str(dataset_german)
#str(dataset_german_subset1)
#boxplot(dataset_german$NUM_CREDITS)
#table(dataset_german_subset1$CREDIT)
table(dataset_german$CREDIT)
#remove customer id
dataset_german = dataset_german[ ,2:33]

library(gmodels)
#CrossTable(dataset_german$CREDIT_USE, dataset_german$CREDIT)

#check the relationship between CREDIT_USE and CREDIT
tabl1 <- table(dataset_german$CREDIT_USE, dataset_german$CREDIT)
#tabl1 <- table(dataset_german_subset1$CREDIT_USE, dataset_german_subset1$CREDIT)
tabl1
chi1 = chisq.test(dataset_german$CREDIT_USE, dataset_german$CREDIT)
chi1
#p-value is 2.821e-05 which is less than 0.05, so there is relationship between Credit_Use and Credit

#check the relationship between RADIOTV and CREDIT
tabl2 <- table(dataset_german$RADIOTV, dataset_german$CREDIT)
tabl2
chi2 = chisq.test(dataset_german$RADIOTV, dataset_german$CREDIT)
chi2
#p-value is 0.000952 which is less than 0.05, so there is relationship between Credit_Use and Credit

#check the relationship between Checking Account Status and CREDIT
tabl3 <- table(dataset_german$CHK_ACCT, dataset_german$CREDIT)
tabl3
chi3 = chisq.test(dataset_german$CHK_ACCT, dataset_german$CREDIT)
chi3
#p-value is  2.2e-16 which is less than 0.05, so there is relationship between Credit_Use and Credit


library(caTools)
set.seed(123)# random seed
#split the dataset into training and test data set
split = sample.split(dataset_german$CREDIT, SplitRatio = 0.75) # it returns true if the observation goes to training set, if it goes to test set, then it returns false
training_set = subset(dataset_german, split == TRUE)
test_set = subset(dataset_german, split == FALSE)

#apply logistic regression to training set
classifier = glm(formula = CREDIT ~ .,family = binomial, data = training_set)
#predict test set data, it will return the probablity
prob_pred = predict(classifier, type = 'response', newdata = test_set[-32])
prob_pred
#create vector of predicted result, to convert to probablity percentage to 0 or 1
y_pred = ifelse((prob_pred > 0.5), 1, 0)
y_pred

#create confusion matrix to evaluate the model
test_set[, 32]
cm = table(test_set[, 32], y_pred)
cm

#correct classification
sum(diag(cm))/sum(cm)
#.76
#Incorrect classification
1-sum(diag(cm))/sum(cm)
#0.24

library(ROCR)

#model performance evaluation

pr = prediction(prob_pred, test_set$CREDIT)
prf = performancee(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
auc
#auc score is 78%

summary(model)
# str(dataset_german_subset1)
# model1 = glm(formula = CREDIT ~ CHK_ACCT + DURATION + HISTORY + CREDIT_USE + SAV_ACCT + INSTALL_RATE + OTHER_INSTALL,
#              family = binomial(link = 'logit'),
#              data = training_set,
#              control = list(maxit = 50))
# prob_pred = predict(model1, type = 'response', newdata = test_set[-33])
# prob_pred
# prob_pred1 = predict(model1, type = 'response', newdata = test_set[-33])
# prob_pred1
# y_pred1 = ifelse((prob_pred1 > 0.5), 1, 0)
# y_pred1
# cm1 = table(test_set[, 33], y_pred1)
# cm1
# library(ROCR)
# pr1 = prediction(prob_pred1, test_set$CREDIT)
# prf1 = performance(pr1, measure = "tpr", x.measure = "fpr")
# plot(prf1)
# auc1 = performance(pr1, measure = "auc")
# auc1 = auc1@y.values[[1]]
# auc1
auc
#apply the model (scoring) in the newapplicant dataset
dataset_newapplicant$pred <- predict(model, newdata=dataset_newapplicant, type='response')
prob_pred1
dataset_newapplicant$credit_pred = ifelse((dataset_newapplicant$pred > 0.5), 'Yes', 'No')
dataset_newapplicant$credit_pred
head(dataset_newapplicant)
