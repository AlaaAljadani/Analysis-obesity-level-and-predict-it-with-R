#Read/Load the dataset in into R
library(readr)
Obesity <- read.csv("C:/Users/HP/Desktop/ObesityDataSet_raw_and_data_sinthetic.csv")
print(Obesity)

#Select a random 1500 instances.
ObesityData<-Obesity[sample(nrow(Obesity), 1500),]
str(ObesityData)

#Use all the attributes except MTRANS and CAEC
ObesityData$MTRANS <- NULL
ObesityData$CAEC <- NULL
str(ObesityData)

#Check number of instances, attributes, data types.
str(ObesityData)

#Check number of missing values
sum(is.na.data.frame(ObesityData))

#Provide statistical description
summary(ObesityData)

#Visualization

hist(ObesityData$Age, xlab="Age", col="blue")

boxplot(Age~NObeyesdad, data=ObesityData)

plot(Age~Weight, data=ObesityData)

  hist(ObesityData$Weight, xlab="Weight", col="blue")

#Date preprocessing 

    #Convert the problem into a binary classification
    ObesityData$NObeyesdad[ObesityData$NObeyesdad == "Insufficient_Weight" | ObesityData$NObeyesdad == "Normal_Weight"] = "Nonobese"
    ObesityData$NObeyesdad[ObesityData$NObeyesdad == "Obesity_Type_I" | 
                    ObesityData$NObeyesdad == "Obesity_Type_II"|
                    ObesityData$NObeyesdad == "Obesity_Type_III" |
                    ObesityData$NObeyesdad == "Overweight_Level_I" |
                    ObesityData$NObeyesdad == "Overweight_Level_II"] = "Obese"
    
    
    #Convert the data type from categorical to numerical
    
    
    require(dplyr)
    ObesityData <- ObesityData %>%
      mutate(Gender = ifelse(Gender == "Male",0,1))
    
    ObesityData <- ObesityData %>%
      mutate(NObeyesdad = ifelse(NObeyesdad == "Nonobese",0,1))
    
    ObesityData <- ObesityData %>%
      mutate(family_history_with_overweight = ifelse(family_history_with_overweight == "no",0,1))
    
    
    ObesityData <- ObesityData %>%
      mutate(FAVC = ifelse(FAVC == "no",0,1))
    
    ObesityData <- ObesityData %>%
      mutate(SMOKE = ifelse(SMOKE == "no",0,1))
    
    ObesityData <- ObesityData %>%
      mutate(SCC = ifelse(SCC == "no",0,1))
    
    
    #Convert the problem into a binary classification
    ObesityData$CALC[ObesityData$CALC == "no"] = "0"
    ObesityData$CALC[ObesityData$CALC == "Sometimes"] = "1"
    ObesityData$CALC[ObesityData$CALC == "Frequently"] = "2"
    ObesityData$CALC[ObesityData$CALC == "Always"] = "3"
    
    
    ObesityData$CALC<-as.numeric(ObesityData$CALC)
    
ObesityData$NObeyesdad
#Check final data set
str(ObesityData)
cor(ObesityData)



#Task 2
#Normally distributed + diff var = welech t-test
x=ObesityData$Weight
y=ObesityData$SMOKE
var(x)
var(y)
ttest = t.test(x,y)
#p-value < 0.05 = reject null
ttest




#Task 3

#Model1

#Split the data into train and test, the train will have 70% of the data while the test 30%

set.seed(123)
ind <- sample(2, nrow(ObesityData), replace = T, prob = c(0.7, 0.3))
train <- ObesityData[ind == 1,]
test <- ObesityData[ind == 2,]



#Build Logistic Regression model
#We build the first model with all attributes  in it to see the important attribute

#logistic1 <- glm(ObesityData$NObeyesdad~ObesityData$Weight +ObesityData$Age+ ObesityData$Gender+ ObesityData$FAVC
            #     +                 + ObesityData$family_history_with_overweight + ObesityData$FAF + ObesityData$NCP+ ObesityData$FCVC+ ObesityData$SMOKE
             #    +                 + ObesityData$SCC+ ObesityData$TUE+ ObesityData$CALC+ ObesityData$CH2O, data =train, family= binomial)

#We build the second model and use the attribute which has a high correlation with NObeyesdad
logistic <- glm(ObesityData$NObeyesdad~ObesityData$Weight +ObesityData$Age
                 + ObesityData$family_history_with_overweight + ObesityData$FAF + ObesityData$NCP
                + ObesityData$SCC, data =train, family= binomial)

summary(logistic)

#prediction 
pred <- predict(logistic, test, type = "response")

#prediction as 1 for Obese and 0 for NonObese 
Label_pred <- ifelse( pred > 0.5, 1, 0)

#Plot ROC
library(plotROC)
rocplot <- ggplot(df, aes(m = Label_pred, d = ObesityData$NObeyesdad))+ geom_roc(n.cuts=20,labels=FALSE)
rocplot + style_roc(theme = theme_grey) + geom_rocci(fill="pink")



#Testing 
library(Metrics)
library(RSNNS)
library(caret)



#confusionMatrix
actual<-  as.factor(ObesityData$NObeyesdad)
predic<- as.factor(Label_pred)

confusionMatrix(predic, actual)

#Recall
recall(predic, actual)


#precision
precision(predic, actual)



#Model2

library(FSelector)
require(tree)
library(rpart)
library(rpart.plot)


ObesityData_DT = data.frame(age =ObesityData$Age,Weight=ObesityData$Weight,NObeyesdad =ObesityData$NObeyesdad,
                     history = ObesityData$family_history_with_overweight,
                     FAF = ObesityData$FAF,
                     NCP = ObesityData$NCP,
                     SCC = ObesityData$SCC)

# convert the NObeyesdad  to factor
ObesityData_DT[ObesityData_DT$NObeyesdad ==1,]$NObeyesdad = "Obese"
ObesityData_DT[ObesityData_DT$NObeyesdad ==0,]$NObeyesdad = "NonObese"

ObesityData_DT$NObeyesdad = as.factor(ObesityData_DT$NObeyesdad)

#Split the data into train and test, the train will have 70% of the data while the test 30%
DT = sample(2, No_Obs, replace = TRUE, prob = c(0.7,0.3))
DT_train <- ObesityData_DT[DT==1, ] 
DT_test <- ObesityData_DT[DT ==2, ]


Obesity_DT = rpart(DT_train$NObeyesdad ~.,DT_train)
# Draw the tree
rpart.plot(Obesity_DT)

summary(Obesity_DT)

#predict the Obesity in the test set
test_Label = predict(Obesity_DT,DT_testing_set, type="class")
test_Label

#Evaluate 
confusionMatrix = confusionMatrix(test_Label,DT_testing_set$NObeyesdad)
confusionMatrix





