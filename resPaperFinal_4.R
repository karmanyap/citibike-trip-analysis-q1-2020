install.packages("dplyr",repos = "http://cran.us.r-project.org")
install.packages("Amelia",repos = "http://cran.us.r-project.org")
install.packages("ggplot2",repos = "http://cran.us.r-project.org")
install.packages("caret",repos = "http://cran.us.r-project.org")
install.packages("pscl",repos = "http://cran.us.r-project.org")
install.packages("ROCR",repos = "http://cran.us.r-project.org")
install.packages("aod", repos = "http://cran.us.r-project.org")
library(readr)
library(ggplot2)
library(dplyr)
library(Amelia)
library(caret)
library(pscl)
library(ROCR)

"Loading data"

"2016 Data"

persons2016 = read.csv("D:/Education/Harrisburg/Lectures/Spring 2020/Analytical Methods I - 502-53-A/Research Paper/CRS2016/PERSON.CSV")
accident2016 = read.csv("D:/Education/Harrisburg/Lectures/Spring 2020/Analytical Methods I - 502-53-A/Research Paper/CRS2016/ACCIDENT.CSV")
vehicle2016 = read.csv("D:/Education/Harrisburg/Lectures/Spring 2020/Analytical Methods I - 502-53-A/Research Paper/CRS2016/VEHICLE.CSV")


"2017 Data"

persons2017 = read.csv("D:/Education/Harrisburg/Lectures/Spring 2020/Analytical Methods I - 502-53-A/Research Paper/CRS2017/PERSON.CSV")
accident2017 = read.csv("D:/Education/Harrisburg/Lectures/Spring 2020/Analytical Methods I - 502-53-A/Research Paper/CRS2017/ACCIDENT.CSV")
vehicle2017 = read.csv("D:/Education/Harrisburg/Lectures/Spring 2020/Analytical Methods I - 502-53-A/Research Paper/CRS2017/VEHICLE.CSV")


"2018 Data"

persons2018 = read.csv("D:/Education/Harrisburg/Lectures/Spring 2020/Analytical Methods I - 502-53-A/Research Paper/CRS2018/PERSON.CSV")
accident2018 = read.csv("D:/Education/Harrisburg/Lectures/Spring 2020/Analytical Methods I - 502-53-A/Research Paper/CRS2018/ACCIDENT.CSV")
vehicle2018 = read.csv("D:/Education/Harrisburg/Lectures/Spring 2020/Analytical Methods I - 502-53-A/Research Paper/CRS2018/VEHICLE.CSV")


"data cleanup"

#2016

acc2016CuriousFields = accident2016 %>%
  inner_join(vehicle2016, by="CASENUM", suffix=c("_acc","_veh")) %>% 
  inner_join(persons2016, by=c("CASENUM"="CASENUM", "VEH_NO"="VEH_NO"), suffix=c("_acc","_per")) %>%
  filter(PER_TYP == 1, VEH_NO == 1, MAX_SEV != 9, MAX_SEV!=1,MAX_SEV!=5, AGE!=998, AGE!=999, VSPD_LIM != 98, VSPD_LIM != 99, TRAV_SP <900, HOUR!= 99,  MAN_COLL<90, LGT_COND < 5, SEX<3, HIT_RUN <= 1, HARM_EV!= 99, VSURCOND<=11) %>%
  select(
    MAX_SEV,
    DAY_WEEK, 
    HOUR, 
    HARM_EV, MAN_COLL, REL_ROAD, 
    LGT_COND, 
    ALCOHOL, WEIGHT, 
    VSPD_LIM, DR_SF1, VSURCOND, 
    AGE, 
    SEX, 
    TRAV_SP, 
    HIT_RUN, 
    VSURCOND
  ) %>%
  mutate(fatalornonfatal = (MAX_SEV>0)) %>%
  mutate(motorVehicleOrNot = HARM_EV %in% c(12, 54, 55)) %>%
  mutate(dryOrNot = VSURCOND %in% c(0,1))

#2017

acc2017CuriousFields = accident2017 %>%
  inner_join(vehicle2017, by="CASENUM", suffix=c("_acc","_veh")) %>% 
  inner_join(persons2017, by=c("CASENUM"="CASENUM", "VEH_NO"="VEH_NO"), suffix=c("_acc","_per")) %>%
  filter(PER_TYP == 1, VEH_NO == 1, MAX_SEV != 9, MAX_SEV!=1, MAX_SEV!=5, AGE!=998, AGE!=999, VSPD_LIM != 98, VSPD_LIM != 99, TRAV_SP <900, HOUR!=99,  MAN_COLL<90, LGT_COND < 5, SEX<3, HIT_RUN <= 1, HARM_EV!= 99, VSURCOND<=11) %>%
  select(
    MAX_SEV,
    DAY_WEEK, 
    HOUR, 
    HARM_EV, MAN_COLL, REL_ROAD, 
    LGT_COND, 
    ALCOHOL, WEIGHT, 
    VSPD_LIM, DR_SF1, VSURCOND, 
    AGE, 
    SEX, 
    TRAV_SP, 
    HIT_RUN, 
    VSURCOND
  ) %>%
  mutate(fatalornonfatal = (MAX_SEV>0)) %>%
  mutate(motorVehicleOrNot = HARM_EV %in% c(12, 54, 55)) %>%
  mutate(dryOrNot = VSURCOND %in% c(0,1))

#2018

acc2018CuriousFields = accident2018 %>%
  inner_join(vehicle2018, by="CASENUM", suffix=c("_acc","_veh")) %>% 
  inner_join(persons2018, by=c("CASENUM"="CASENUM", "VEH_NO"="VEH_NO"), suffix=c("_acc","_per")) %>%
  filter(PER_TYP == 1, VEH_NO == 1, MAX_SEV != 9, MAX_SEV!=1,MAX_SEV!=5, AGE!=998, AGE!=999, VSPD_LIM != 98, VSPD_LIM != 99, TRAV_SP <900, HOUR!=99, MAN_COLL<90, LGT_COND < 5, SEX<3, HIT_RUN <= 1, HARM_EV!= 99, VSURCOND<=11) %>%
  select(
    MAX_SEV,
    DAY_WEEK, 
    HOUR, 
    HARM_EV, MAN_COLL, REL_ROAD, 
    LGT_COND, 
    ALCOHOL, WEIGHT, 
    VSPD_LIM, DR_SF1, VSURCOND, 
    AGE, 
    SEX, 
    TRAV_SP, 
    HIT_RUN, 
    VSURCOND
  ) %>%
  mutate(fatalornonfatal = (MAX_SEV>0)) %>%
  mutate(motorVehicleOrNot = HARM_EV %in% c(12, 54, 55)) %>%
  mutate(dryOrNot = VSURCOND %in% c(0,1))

#Get all the year's data into allYearAcc


allYearAcc = rbind(acc2016CuriousFields, acc2017CuriousFields, acc2018CuriousFields)
allYearAcc$dryOrNot = as.numeric(allYearAcc$dryOrNot)

allYearAcc

#Splitting all the data into Train and Test data 

smp_size <- floor(0.75 * nrow(allYearAcc))

set.seed(123)
train_ind <- sample(seq_len(nrow(allYearAcc)), size = smp_size)
train_ind
train <- allYearAcc[train_ind, ]
test <- allYearAcc[-train_ind, ]

#Checking for Missing data

missmap(train, col=c("blue", "red"), legend=FALSE)

#Start of Exploratory Data Analysis - EDA 

str(allYearAcc)


allYearAcc %>% 
  ggplot(aes(y=TRAV_SP, x=fatalornonfatal)) +
  geom_boxplot() +
  labs(x="Fatality", y="Travel Speed", title="Fatality by Travel Speed")

summary(train$TRAV_SP[train$fatalornonfatal == FALSE])

summary(train$TRAV_SP[train$fatalornonfatal == TRUE])

allYearAcc %>% 
  ggplot(aes(x=as.factor(VSPD_LIM), y=TRAV_SP)) +
  facet_wrap(~fatalornonfatal) +
  geom_boxplot() + 
  labs(x="Speed Limit", y="Travel Speed", title="Travel Speed vs Speed Limit") +
  scale_y_continuous(breaks = seq(0, 200, by = 10))

allYearAcc %>% 
  ggplot(aes(y=HOUR, x=fatalornonfatal)) +
  geom_boxplot() +
  labs(x="Fatality", y="Hour", title="Fatality by Hour")

allYearAcc %>% 
  ggplot(aes(y= DAY_WEEK, x=fatalornonfatal)) +
  geom_boxplot() +
  labs(x="Fatality", y="Day of the Week", title="Fatality by Days of the Week")

allYearAcc %>% 
  ggplot(aes(y=AGE, x=fatalornonfatal)) +
  geom_boxplot() +
  labs(x="Fatality", y="Age", title="Fatality by Age")

allYearAcc %>% 
  ggplot(aes(y=fatalornonfatal, x=TRAV_SP  )) + 
  geom_point() +
  labs(x="Travel Speed", y="Fatality", title="Travel Speed for types of Fatality")

allYearAcc %>% 
  ggplot(aes(x=as.factor(VSPD_LIM), y=TRAV_SP)) +
  facet_wrap(~fatalornonfatal) +
  geom_line() + 
  labs(x="Speed Limit", y="Travel Speed", title="Travel Speed vs speed limit") +
  scale_y_continuous(breaks = seq(0, 200, by = 10))

allYearAcc %>% 
  ggplot(aes(x=as.factor(VSPD_LIM), y=TRAV_SP)) +
  facet_wrap(~fatalornonfatal) +
  geom_point() + 
  labs(x="Speed Limit", y="Travel Speed", title="Travel Speed vs speed limit") +
  scale_y_continuous(breaks = seq(0, 200, by = 10))

hist(allYearAcc$AGE, main = "Ages of Drivers involved in the crash", xlab = "Age", ylab = "Frequency")

#End of Exploratory Data Analysis - EDA


#Model Fit

model1 = glm( fatalornonfatal ~  TRAV_SP+ALCOHOL+VSPD_LIM+dryOrNot+motorVehicleOrNot+LGT_COND+SEX+AGE,
             data = train, family = binomial)
             
summary(model1)


model2 = glm( fatalornonfatal ~  TRAV_SP+VSPD_LIM+dryOrNot+motorVehicleOrNot+LGT_COND+AGE,
              data = train, family = binomial)

summary(model2)


#Model Fit End

#testing model, CI, exponential coefficients


anova(model2, test= "Chisq")
anova(model1, test="Chisq")

summary(model2) #display results
par(mfrow = c(2,2))
plot(model2)

test$fatalornonfatal


confint(model2) # 95% CI for the coefficients
exp(coef(model2)) # exponentiated coefficients
exp(confint(model2)) # 95% CI for exponentiated coefficients


#testing model, CI, exponential coefficients end


#prediction

test$probFatal <- predict(model2,newdata= test,type='response')
head(test)

fit <- ifelse(test$probFatal > 0.5,1,0)


misClasificError <- mean(fit != test$fatalornonfatal)
print(paste('Accuracy',1-misClasificError))


pr <- prediction(test$fatal, test$fatalornonfatal)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")


plot(prf)


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

pdata <- predict(logitMod, newdata = train, type = "response")

#Prediction End
