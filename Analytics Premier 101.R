# T-Tset
mtcars$mpg 
mtcars$am


#The gas mileage for automatic transmission can be listed as follows: 
L <- mtcars$am == 0 
mpg.auto = mtcars[L,]$mpg 
mpg.auto                    # automatic transmission mileage 

#By applying the negation of L, we can find the gas mileage for manual transmission.
mpg.manual = mtcars[!L,]$mpg 
mpg.manual                  # manual transmission mileage 


#We can now apply the t.test function to compute the difference in means of the two sample data.

t.test(mpg.auto, mpg.manual) 


# Correlation

duration = faithful$eruptions   # eruption durations 
waiting = faithful$waiting      # the waiting period 
cor(duration, waiting)          # apply the cor function 


# Chi-square test

library(MASS)       # load the MASS package 
tbl = table(survey$Smoke, survey$Exer) 
tbl                 # the contingency table 

chisq.test(tbl)

# Anova

# #Item1 Item2 Item3 
# 22    52    16 
# 42    33    24 
# 44     8    19 
# 52    47    18 
# 45    43    34 
# 37    32    39

#Solution
#The solution consists of the following steps:
  
#Copy and paste the sales figure above into a table file named "fastfood-1.txt" with a text editor.
#Load the file into a data frame named df1 with the read.table function. As the first line in the file contains the column names, we set the header argument as TRUE.

df1 = read.table("fastfood-1.txt", header=TRUE) 
df1 

#Concatenate the data rows of df1 into a single vector r .
r = c(t(as.matrix(df1))) # response data 
r 

#Assign new variables for the treatment levels and number of observations.
f = c("Item1", "Item2", "Item3")   # treatment levels 
k = 3                    # number of treatment levels 
n = 6                    # observations per treatment

#Create a vector of treatment factors that corresponds to each element of r in step 3 with the gl function.
tm = gl(k, 1, n*k, factor(f))   # matching treatments 
tm 

#Apply the function aov to a formula that describes the response r by the treatment factor tm.
> av = aov(r ~ tm)

#Print out the ANOVA table with the summary function.
summary(av) 
