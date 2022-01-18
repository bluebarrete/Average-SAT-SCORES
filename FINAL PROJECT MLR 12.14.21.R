library(class)
library(broom)
library(dplyr)
library(plyr)
library (readr)
if(!require(pastecs)) install.packages("pastecs")
library(pastecs)
if (!require(DT)) install.packages('DT') 
library(DT)
if (!require(stargazer)) install.packages("stargazer") 
library(stargazer)
if (!require(reshape)) install.packages("reshape")
library(reshape)
library(ggplot2)
library(car)

scores <- read.csv("Average SAT Scores NYC Schools Patched.csv")
names(scores)
View(scores)
summary(scores)
is.na(scores)
newdata <- select(scores, -c(4:7,11,23,25,27,29,31,33,35,37))
newdata <- na.omit(newdata)


#newdata <- rename(scores2, c(AveMathscore = Average.Score..SAT.Math., AveReadscore = Average.Score..SAT.Reading., AveWritescore = Average.Score..SAT.Writing.))
names(newdata)[14:16] = c("AveMathscore", "AveReadscore", "AveWritescore")

# to check on categorical and continuous variables:
str(newdata)

# to convert chr to a factor #9,10,11,12,17 and change from % to ratio:

sapply(newdata, class)

newdata$Percent.White <- as.numeric(sub("%","",newdata$Percent.White))/100
newdata$Percent.Black <- as.numeric(sub("%","",newdata$Percent.Black))/100
newdata$Percent.Hispanic <- as.numeric(sub("%","",newdata$Percent.Hispanic))/100
newdata$Percent.Asian <- as.numeric(sub("%","",newdata$Percent.Asian))/100
newdata$Percent.Tested <- as.numeric(sub("%","",newdata$Percent.Tested))/100

sapply(newdata, class)      

#CALCULATE a new variable: a total average score:

newdata$totalscore <- newdata$AveMathscore + newdata$ AveReadscore + newdata$ AveWritescore
names(newdata)

dataEDA = na.omit(newdata)
#375 and 26 (with additional variable)
dim(dataEDA)

#EDA Stats

stargazer(dataEDA, type = "text", title = "Table 1: Summary Statistics", out = "table1.txt", digits = 1)

#Plot by neighborhood vs total score:
boxplot(totalscore ~ Borough, data = dataEDA, main = "Fig 1: The Average Total SAT Score by NYC Borough")
#ScatterPlots for individual relationships:


# % of Tested and SAT Scores:
par(mfrow=c(1,1))
attach(dataEDA)
plot(Percent.Tested,totalscore, main = "% of Tested Students and Average SAT Score", xlab = "Percent of Tested Students", ylab = "Total Average SAT Score", pch=19)
lines(lowess(Percent.Tested,totalscore), col="blue")
abline(lm(totalscore ~ Percent.Tested), col="red")

# Number of Enrolled and SAT Scores
plot(Student.Enrollment,totalscore, main = "Student Enrollment and Average SAT Score", xlab = "# of Enrolled Students", ylab = "Total Average SAT Score", pch=19)
lines(lowess(Student.Enrollment,totalscore), col="blue")
abline(lm(totalscore ~ Student.Enrollment), col="red")

#Scatterplot for Extra curriculum vs Total Ave score and Math Score

ggplot(dataEDA,aes(X..of.ap.courses,totalscore,colour = AveMathscore))+geom_point() #shows correlation
ggplot(dataEDA,aes(X..of.ap.courses,totalscore,colour = AveReadscore))+geom_point()#shows correlation
ggplot(dataEDA,aes(X..of.ap.courses,totalscore,colour = AveWritescore))+geom_point()#shows correlation

#ggplot(dataEDA,aes(X..of.extracurriculars,totalscore,colour = AveMathscore))+geom_point() 
#ggplot(dataEDA,aes(X..of.online.ap.courses,totalscore,colour = AveMathscore))+geom_point()
#ggplot(dataEDA,aes(X..of.Language.Classes,totalscore,colour = AveMathscore))+geom_point()
#ggplot(dataEDA,aes(X..of.Language.Classes,totalscore,colour = AveReadscore))+geom_point()
#ggplot(dataEDA,aes(X..of.Language.Classes,totalscore,colour = AveWritescore))+geom_point()

par("mar")
par(mar=c(1, 1, 1, 1))

#Correlation
data_eda0 <- select(dataEDA, c(26,4,5,6,9:25))
round(cor(data_eda0), digits = 2)

#Scatterplots on all numerical variables for various datasets

data_eda1 <- select(dataEDA, c(26,4,5,6,9:13,17:25))
names(data_eda1)
pairs(data_eda1) 
round(cor(data_eda1), digits = 1)

data_eda2 <- select(dataEDA, c(26,9,14:17))
names(data_eda2)
pairs(data_eda2)
cor(data_eda2)


#show the correlation between an independent variable and a dependent variable,
#conditional on other independent variables. 
#Added-variable plots are also useful for spotting influential outliers in the data which affect the estimated regression parameters.


#top 10 highscoring schools in NYC.

top <-
  dataEDA %>%
  select(School.Name, AveMathscore, AveWritescore, AveReadscore, totalscore) %>%
  arrange(desc(totalscore)) %>%
  top_n(10)

top.m <- top %>% select(-totalscore)
names(top.m) <- c("School.Name", "AveMathscore", "AveReadscore", "AveWritescore")
top.m <- melt(top.m, id.vars = "School.Name")
top.m$variable <- factor(top.m$variable, levels = c("Writing", "Reading", "Math")) #switch order of legend


ggplot(top, aes(x = reorder(School.Name,totalscore), y =totalscore)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(1400, 2200)) +
  geom_text(aes(label=totalscore),vjust=0.3, hjust=-.2, position=position_dodge(width=0.9), size=2.5) +
  labs(title = "Fig 2: SAT TOP SCORES", subtitle = "Top 10 Public Schools of NYC",
       x = "", y = "SAT Average score") +
  theme(axis.title.y=element_text(margin=margin(0,15,0,0))) +
  theme(axis.title.x=element_text(margin=margin(10,0,0,0))) +
  theme(plot.title=element_text(margin=margin(0,0,5,0),hjust = 0.5)) +
  theme(plot.subtitle=element_text(margin=margin(0,0,10,0),hjust = 0.5))

#LOCATION BASED ANALYSIS - Grouped by borough

top2 <-
  dataEDA %>%
  select(School.Name,Borough,AveMathscore, AveWritescore, AveReadscore, totalscore) %>%
  arrange(desc(totalscore)) %>%
  top_n(10)

top.b <- top2 %>% select(-totalscore)
names(top.b) <- c("School.Name", "Borough", "AveMathscore", "AveReadscore", "AveWritescore")
top.b <- melt(top.b, id.vars = "School.Name")
top.b$variable <- factor(top.b$variable, levels = c("Writing", "Reading", "Math")) #switch order of legend

ggplot(top2, aes(x = reorder(Borough,totalscore), y =totalscore)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip(ylim=c(1400, 2200)) +
  geom_text(aes(label=School.Name),vjust=2, hjust=1.7, position=position_dodge(width=4), size=2, check_overlap = TRUE) +
  labs(title = "Fig 2: SAT TOP SCORES", subtitle = "Boroughs for Top 10 Public Schools of NYC",
       x = "", y = "SAT Average score") 

# The violin box plot:

section_scores <- select(dataEDA , AveMathscore, AveReadscore, AveWritescore)
names(section_scores) <- c("Math", "Reading", "Writing")
section_scores <- reshape::melt(section_scores, id.vars = NULL)
section_scores <- filter(section_scores, value != "NA")
means <- aggregate(value ~  variable, section_scores, mean)
means[,2] <- round(means[,2])

ggplot(section_scores, aes(x = variable, y = value, fill = variable)) + 
  geom_violin(color="steelblue",fill="steelblue",size=1) +
  geom_boxplot(fill="steelblue",width = 0.3) +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape="_", size=5, show.legend = FALSE) +  #shape=16, size 1
  geom_text(data = means, aes(label = value, y = value + 12)) +
  labs(title = "Breakdown of Each SAT Section", subtitle="Violin Plot with Boxplot",x = "", y = "Section Score") +
  theme(legend.position="none") 

#The boxplots within each violin plot show that the Math section has the highest average score of all the sections (433) and has several high outliers,
  
#AVERAGE MATHS SAT SCORE
ggplot(top, aes(x = reorder(School.Name, totalscore), y = AveMathscore)) +
  geom_bar(stat = "identity", col = "white", fill = "steelblue") +
  coord_flip(ylim=c(475, 800)) +
  labs(title = "Top 10 NYC Public Schools SAT Scores", x = "", y = "Average Math SAT Score for School")

#AVERAGE READING SAT SCORE
ggplot(top, aes(x = reorder(School.Name,totalscore), y = AveReadscore)) +
  geom_bar(stat = "identity", col = "white", fill = "maroon3") +
  coord_flip(ylim=c(475, 800)) +
  labs(title = "Top 10 NYC Public Schools SAT Scores", x = "", y = "Average Reading SAT Score for School")

#Average WRITING SAT SCORE
ggplot(top, aes(x = reorder(School.Name,totalscore), y = AveWritescore)) +
  geom_bar(stat = "identity", col = "white", fill = "orange") +
  coord_flip(ylim=c(475, 800)) +
  labs(title = "Top 10 NYC Public Schools SAT Scores", x = "", y = "Average Writing SAT Score for School")


#Stacked By Section
ggplot(top.m, aes(x = reorder(School.Name, value), y = value, fill=variable)) +
  geom_bar(stat='identity',position = "fill") +
  scale_fill_manual(values = c("darkred","darkblue", "steelblue"),
                    breaks = c("AveMathscore", "AveReadingscore", "AveWritingscore"),
                    labels = c("AveMathscore", "AveReadingscore", "AveWritingscore")) +
  coord_flip() +
  labs(title = "PER SECTION SAT SCORE", subtitle="Top ten percent NYC PUBLIC SCHOOLS", x = "", y = "") +
  geom_text(aes(label=value), size=2,position = "fill",hjust = 2, vjust=0.3, color="white") +
  #geom_hline(aes(yintercept=.6666), color="gray", linetype="dashed", size=0.4) +
  #geom_hline(aes(yintercept=.3333), color="gray", linetype="dashed", size=0.4) +
  theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) +
  theme(plot.title=element_text(margin=margin(0,0,5,0),hjust = 0.5)) +
  theme(plot.subtitle=element_text(margin=margin(0,0,10,0),hjust = 0.5))


#BOTTOM 10 SCHOOLS:

bottom <-
  dataEDA %>%
  select(School.Name,AveMathscore,AveWritescore,AveReadscore, totalscore) %>%
  filter(totalscore != 0) %>%
  arrange(totalscore) %>%
  top_n(-10)

bottom.m <- bottom %>% select(-totalscore)
names(bottom.m) <- c("School.Name", "AveMathscore", "AveReadscore", "AveWritescore")
bottom.m <- melt(bottom.m, id.vars = "School.Name")
bottom.m$variable <- factor(bottom.m$variable, levels = c("Writing", "Reading", "Math")) #switch legend order

ggplot(bottom, aes(x = reorder(School.Name, -totalscore), y = totalscore)) +
  geom_bar(stat = "identity", col = "white", fill = "purple") +
  coord_flip(ylim=c(900, 1200)) +
  labs(title = "Bottom 10 NYC Public Schools SAT Scores", x = "", y = "Average SAT Score for School")

ggplot(bottom, aes(x = reorder(School.Name, -totalscore), y = AveMathscore)) +
  geom_bar(stat = "identity", col = "white", fill = "darkgreen") +
  coord_flip(ylim=c(275, 500)) +
  labs(title = "Bottom 10 NYC Public Schools SAT Scores", x = "", y = "Average Math SAT Score for School")

ggplot(bottom, aes(x = reorder(School.Name, -totalscore), y = AveReadscore)) +
  geom_bar(stat = "identity", col = "white", fill = "darkred") +
  coord_flip(ylim=c(275, 500)) +
  labs(title = "Bottom 10 NYC Public Schools SAT Scores", x = "", y = "Average Reading SAT Score for School")

ggplot(bottom, aes(x = reorder(School.Name, -totalscore), y = AveWritescore)) +
  geom_bar(stat = "identity", col = "white", fill = "darkblue") +
  coord_flip(ylim=c(275, 500)) +
  labs(title = "Bottom 10 NYC Public Schools SAT Scores", x = "", y = "Average Writing SAT Score for School")

detach(dataEDA)

#Data Selection for MLR
data_eda3 <- select(dataEDA, c(3:16,17:26))
data_eda3 <- select(data_eda3, -c(1,2,3,4,5,6)) #have added student enrollment j.trying to add percent tested
names(data_eda3)
pairs(data_eda3)
tibble(data_eda3)

#linear regression model

lm.fit= lm(totalscore ~.  ,data = data_eda3) 
summary(lm.fit)
plot(lm.fit)
#high multicollinearity among variables gives flatline plot for regression, 
# in residual vs fitted plot we can see the red line,(shows avg. value of residuals at each value of fitted value which shows) is perfectly straight,
#no discernible non-linear trends
summary(lm.fit)#points 1 and 6 are outliers with high residuals.

lm.fit1= lm(log(totalscore) ~.  -Zip.Code -Latitude - Longitude -AveMathscore -AveReadscore -AveWritescore ,data = data_eda3)
# we decide to drop off some variables with high multicolinearity,also  some variables  don't provide any real value to the regression
#.. to see if it gives us better significance
summary(lm.fit1)
vif(lm.fit1)
plot(lm.fit1) #etenicity coloumns give a a high significance because they are so collinear, so we drop these..

lm.fit2= lm(log(totalscore) ~. -Percent.Asian -Percent.Black -Percent.Hispanic -Percent.White -Zip.Code -Latitude - Longitude -AveMathscore -AveReadscore -AveWritescore ,data = data_eda3)
#remove variables with the fighest collinearity
#use log function to remove hetroscadsity
summary(lm.fit2)
plot(lm.fit2) #as we can see some of the outliers have significant leverage .
#adjusted R square shows us how well the model fits overall (0.5282) for thsi model.


#standardized residuals
rs=rstandard(lm.fit2) #type of residual we  use to identify outliers in a regression model..
rs
plot(rs)



#use cooks distance to see if there are any outliers or high levrage points..

cooksD=cooks.distance(lm.fit2)


#find out any values greater than 3 times the mean , as a general  rule of thumb

OUTL = cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
OUTL # a number of observations meet this criteria


#confidence intervals of the cofficients..

confint(lm.fit2, c("Student.Enrollment","Percent.Tested", "X..of.Language.Classes", 
                   "X..of.online.language.courses", "X..of.ap.courses", "X..of.online.ap.courses",
                   "X..of.extracurriculars", "X..of.boys.sports", "X..of.girls.sports", "X..of.coed.sports"), level = .95)


#removing outliers
#202, 231, 359, 314
mlr_data2 = data_eda3[-c(6, 39, 154, 200, 244, 299, 202, 231, 359, 314), ]
mlr.lm2 = lm(log(totalscore) ~ . -Percent.Asian -Percent.Black - Percent.Hispanic - 
               Percent.White - AveMathscore - AveReadscore - AveWritescore -Zip.Code , -Latitude ,-Longitude,data = mlr_data2)
summary(mlr.lm2)
plot(mlr.lm2)
vif(mlr.lm2)
avPlots( mlr.lm2 ,data=data_eda3)  #added variable plot

#after removing the ouliers we can clearly see that the adjusted r square value is increased to .58 showing a clear improvement.
#RSE IS .72.. 


#check multicollinearity
library(car)
vif(lm.fit2) # vif is greater than 5 or 10 , means high multicolinearity
vif(mlr.lm2)#low values indicate better fit.
plot(vif(mlr.lm2))
avPlots(lm(log(totalscore) ~. -Percent.Asian -Percent.Black -Percent.Hispanic -Percent.White -Zip.Code -Latitude - Longitude -AveMathscore -AveReadscore -AveWritescore ,data = mlr_data2))  #added variable plot






## RANDOM FOREST BIPUL
data_eda4 <- select(dataEDA, c(9:26))
data_eda4 <- na.omit(data_eda4)
names(data_eda4)

data_final <- data_eda4[, c("Percent.White", "Percent.Black", "Percent.Hispanic", 
                            "Percent.Asian", "X..of.Language.Classes", "X..of.online.language.courses",
                            "X..of.ap.courses", "X..of.online.ap.courses", 
                            "X..of.extracurriculars", "X..of.boys.sports", 
                            "X..of.girls.sports", "X..of.coed.sports", "totalscore")]
#data_final <- data_eda4[, c(2:5, 10:17)]
data_final

library(randomForest)

set.seed(1)
train = sample(1:nrow(data_final), nrow(data_final)/2)
data_final.test = data_final[-train, "totalscore"]

# The argument mtry = 12 indicates that all 12 predictors should be considered
# for each split of the tree-in other words, that bagging should be done
set.seed(1)
bag.data_final = randomForest(totalscore ~., data = data_final, subset=train, mtry=12, importance=TRUE)
bag.data_final

# How well does this bagged model perform on the test set
yhat.bag = predict(bag.data_final, newdata=data_final[-train,])

plot(yhat.bag , data_final.test)
abline (0,1)

# The test set MSE associated with the bagged regression tree is 15685.06
mean(( yhat.bag - data_final.test)^2)

# Here we try mtry=12/3
rf.data_final = randomForest(totalscore ~., data = data_final, subset=train, mtry=12/3, importance=TRUE)
yhat.rf = predict(rf.data_final, newdata=data_final[-train,])


# The MSE is greater, so bagging performed better
mean((yhat.rf-data_final.test)^2)

# Using the importance() function, we can view the importance of each variable.
importance(bag.data_final)

# The results indicate that across all of the trees considered in the random
# forest, the Ethnicity (Percent.Asian, Percent.Black, Percent.Hispanic, Percent.Asian) and # of AP Courses taken (X..of.ap.courses) are by
# far the most important variables

varImpPlot(bag.data_final)




# Comparing tow linear models:

install.packages("stargazer")
library(stargazer) #use for well-formatted regression tables

lm.fit1= lm(log(totalscore) ~. -AveMathscore -AveReadscore -AveWritescore,data = data_eda3)
lm.fit2= lm(log(totalscore) ~. -Percent.Asian -Percent.Black -Percent.Hispanic -Percent.White  -AveMathscore -AveReadscore -AveWritescore ,data = data_eda3)
mlr.lm2 = lm(log(totalscore) ~ . -Percent.Asian -Percent.Black - Percent.Hispanic - 
               Percent.White - AveMathscore - AveReadscore - AveWritescore -Zip.Code , -Latitude ,-Longitude,data = mlr_data2)



stargazer(lm.fit1,lm.fit2, type="text", title="Results", align=TRUE, dep.var.labels = c("totalscore"), covariate.labels =c("Student.Enrollment","Percent.Tested", "X..of.Language.Classes", 
                                                                                                                           "X..of.online.language.courses", "X..of.ap.courses", "X..of.online.ap.courses",
                                                                                                                           "X..of.extracurriculars", "X..of.boys.sports", "X..of.girls.sports", "X..of.coed.sports"), out = "SATScore.txt") #compare the two linear models


stargazer(mlr.lm2,lm.fit2, type="text", title="Results", align=TRUE, dep.var.labels = c("totalscore"), covariate.labels =c("Student.Enrollment","Percent.Tested", "X..of.Language.Classes", 
                                                                                                                           "X..of.online.language.courses", "X..of.ap.courses", "X..of.online.ap.courses",
                                                                                                                           "X..of.extracurriculars", "X..of.boys.sports", "X..of.girls.sports", "X..of.coed.sports"), out = "SATScore2.txt") #compare the two linear models



