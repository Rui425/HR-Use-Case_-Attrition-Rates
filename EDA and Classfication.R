library(dplyr)
library(corrplot)
library(ggplot2)
library(caret)
library(party)
library(fastAdaboost)
library(ibmos2sparkR)
library(e1071)
library(rpart.plot)
library(rpart)
library(ggpubr)


#### Importing data 
# @hidden_cell
# This function accesses a file in your Object Storage. The definition contains your credentials.
# You might want to remove those credentials before you share your notebook.
getObjectStorageFileWithCredentials_2c31d44f8d214b3487b0bb239a8df945 <- function(container, filename) {
    # This functions returns a textConnection object for a file
    # from Bluemix Object Storage.

    if(!require(httr)) install.packages('httr')
    if(!require(RCurl)) install.packages('RCurl')
    library(httr, RCurl)
    auth_url <- paste("https://identity.open.softlayer.com",'/v3/auth/tokens', sep= '')
    auth_args <- paste('{"auth": {"identity": {"password": {"user": {"domain": {"id": ', "6df94213534b4502a13c762d2a68d85d",'},"password": ',
                    "nGc15.v{aMa19I^b",',"name": ', "member_ba7ce91d62223a8550fbdc7200ad20a3ad299fc3",'}},"methods": ["password"]}}}', sep='"')
    auth_response <- httr::POST(url = auth_url, body = auth_args)
    x_subject_token <-  headers(auth_response)[['x-subject-token']]
    auth_body <-  content(auth_response)
    access_url <-  unlist(lapply(auth_body[['token']][['catalog']], function(catalog){
        if((catalog[['type']] == 'object-store')){
            lapply(catalog[['endpoints']], function(endpoints){
                if(endpoints[['interface']] == 'public' && endpoints[['region_id']] == 'dallas') {
                    paste(endpoints[['url']], container, filename, sep='/')}
            })
        }
    }))
    data <- content(httr::GET(url = access_url, add_headers ("Content-Type" = "application/json", "X-Auth-Token" = x_subject_token)), as="text")
    textConnection(data)
}

df.data.4 <-  read.csv(file = getObjectStorageFileWithCredentials_2c31d44f8d214b3487b0bb239a8df945("HumanResourceProject", "EmployeePerformance.csv"))
head(df.data.4)
attrition = as.data.frame(df.data.4)
dim(attrition)
class(attrition)
head(attrition)

###### EDA 
## employment performance
## Variables like: 
unique(attrition$PerformanceRating)
unique(attrition$performance)
dim(attrition)
names(attrition)

data1 = attrition %>% 
  select(-c(X, EmployeeCount,EmployeeNumber,StandardHours,FirstName,LastName,Over18,X.XF.Attrition,X.XFC.Attrition))
str(data1)

### Correlation matrix 
chr_indx = sapply(data1, function(x) is.factor(x))
num_indx = sapply(data1, function(x) is.numeric(x))
data_chrToNum = sapply(data1[chr_indx],function(x) as.integer(x))
data_num_all = cbind(data1[num_indx],data_chrToNum)

M = cor(data_num_all)
corrplot(M, method="circle")

#### Data Visualization

## How many people in each department 
pDep = ggplot(data=data1, aes(Department)) +
  geom_bar(aes(fill = Department)) +
  labs(title="Number of people in each department")


## Salary distribution 
pPeop = ggplot(data1, aes(MonthlyIncome, colour = Department)) +
  geom_freqpoly(binwidth = 500) +
  labs(title="Salary Distribution for Each Department")


## How many employees left the company
pAttri = ggplot(data=data1, aes(Attrition)) +
  geom_bar(aes(fill = Attrition)) +
  labs(title="How Many Emplyees Left Company")

## left by department 
pDepAttr = ggplot(data=data1, aes(Attrition,fill = Department)) +
  geom_bar(position=position_dodge()) + scale_fill_brewer(palette="Paired") + 
  theme_minimal() +
  labs(title="People Left by Different departments", 
      y = "Number of people")

## left by enviornment satisfaction 
pSatisAttr = ggplot(data=data1, aes(EnvironmentSatisfaction,fill = Attrition)) +
  geom_bar(position=position_dodge()) + scale_fill_brewer(palette="Paired") + 
  theme_minimal() +
  scale_x_discrete (limits = c('Low', 'Medium', 'High', 'Very High')) +
  labs(title="People Left for Each Level of Satisfactory", 
       y = "Number of people")


## left by overtime 
pOverAttr = ggplot(data=data1, aes(OverTime,fill = Attrition)) +
  geom_bar(position=position_dodge()) + scale_fill_brewer(palette="Paired") + 
  theme_minimal() +
  labs(title="People Left for Overtime", 
       y = "Number of people")


## left for marital
pMariAttr = ggplot(data=data1, aes(MaritalStatus,fill = Attrition)) +
  geom_bar(position=position_dodge()) + scale_fill_brewer(palette="Paired") + 
  theme_minimal() +
  labs(title="People Left for Marriage", 
       y = "Number of people")


## Left and performance 
pPerfoAttr = ggplot(data=data1, aes(Attrition,fill = performance)) +
  geom_bar(position=position_dodge()) + scale_fill_brewer(palette="Paired") + 
  theme_minimal() +
  labs(title="Different performance Left for...", 
       y = "Number of people")


## Total working years and Job level 
pYearsAttr = ggplot(data1, aes(TotalWorkingYears, colour = Attrition)) +
  geom_freqpoly(binwidth = 2) +
  labs(title="Salary Distribution for Each Department")


ggarrange(pPeop, pAttri, pDepAttr, pSatisAttr, 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)

ggarrange(pOverAttr, pMariAttr, pPerfoAttr, pYearsAttr, 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)
          
          
# Depatment, Education and attrition, performance 
pEdu1 = ggplot(data=data1, aes(Education)) +
  geom_bar(aes(fill = Attrition,colour = Attrition),position = "fill",width = .7) +
  facet_wrap(~Department,dir = "v") 
pEdu2 = ggplot(data=data1, aes(Education)) +
  geom_bar(aes(fill = Attrition),width = .7) 
pEdu1
pEdu2


#################### Variable selection 
# AIC
lm.before = glm(Attrition ~ Age + DistanceFromHome + MonthlyIncome + NumCompaniesWorked + TrainingTimesLastYear +
                   WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion  + WorkLifeBalanceScore + 
                   LeadershipScore + Department + JobLevel + JobSatisfaction + MaritalStatus + RelationshipSatisfaction + 
                   StockOptionLevel, family=binomial(link='logit'), data = data1)
library(MASS)
lm.AIC <- stepAIC(lm.before)
lm.AIC$anova

################### Classfication mode_10 fold fold cross validation  
## Prediction - decision tree 
model1 = train(
  Attrition ~ Age + MonthlyIncome + YearsInCurrentRole +
    WorkLifeBalanceScore + Department + JobLevel + JobSatisfaction + MaritalStatus + OverTime + 
    StockOptionLevel, data1,
  method = 'rpart',
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
model1$results
model1$finalModel
pred1 = predict(model1,attrition)
sum(pred1 == attrition$Attrition)/dim(attrition)[1]

## Prediction - random forest 
model2 = train(
  Attrition ~ Age + MonthlyIncome + TotalWorkingYears + YearsInCurrentRole +
    WorkLifeBalanceScore + Department + JobLevel + JobSatisfaction + MaritalStatus + OverTime + 
    StockOptionLevel, data1,
  method = 'cforest',
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
model2$results
pred2 = predict(model2,attrition)
sum(pred2 == attrition$Attrition)/dim(attrition)[1]

### Decision tree visualization 
model<-rpart(Attrition ~ Age + MonthlyIncome + TotalWorkingYears + YearsInCurrentRole +
    WorkLifeBalanceScore + Department + JobLevel + JobSatisfaction + MaritalStatus + OverTime + 
    StockOptionLevel, data = data1)
prp(model1$finalModel,box.palette = "Reds")
