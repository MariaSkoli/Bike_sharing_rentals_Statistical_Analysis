#################
#---Libraries---#
#################

library(dplyr)
library(ggplot2)
library(lubridate)

########################
#--- Data cleansing ---#
########################


# loading in the data and discarding the index column

setwd("C:\\Users\\Maria Skoli\\Documents\\R")  
getwd()
data <- read.csv('bike_50.csv', header = TRUE, sep = ';')
#View(data)
data <- data[,-1]

#print dataset variables 
install.packages('kable')
library("kableExtra")
kbl(t(data),digits = 18, caption = "Dataset Variables", valine="t",allign="H") %>%
  kable_paper(full_width = F) %>%
  kableExtra::kable_styling(latex_options = "hold_position")

# removing all missing values
data <- data[complete.cases(data), ]

# getting a quick view of the data
str(data)

# filling the zero- windspeed data with random data from the normal distribution
y<-sum(data$windspeed==0)
n<-1500
for (i in 1:n)
{
  if (data$windspeed[i]==0)
  {data$windspeed[i]<-abs(round(rnorm(1,0.19,0.13),3))}
}

# converting the dates to Date objects
data$dteday <- as.Date(data$dteday, format = "%Y-%m-%d")

# renaming the date
data <- data %>% rename(date = dteday)


# recalculating the season based on the date
data <- data %>% 
  mutate(season = ifelse(month(date) %in% c(12, 1, 2), 4,
                         ifelse(month(date) %in% c(3, 4, 5), 1,
                                ifelse(month(date) %in% c(6, 7, 8),2,
                                       3))))
# converting the season into a factor
data$season <- as.factor(data$season)

# making sure that all dates belong to the two indicated years (2011 and 2012)
sum(!(year(data$date) %in% c(2011, 2012)))

# recoding the year based on the date
data <- data %>% 
  rename(year = yr) %>% 
  mutate(year = ifelse(year(date) == "2011", 0,1))

# converting the year into a factor
data$year <- as.factor(data$year)

# recoding the month based on the date
data <- data %>% 
  rename(month = mnth) %>% 
  mutate(month = month(date))

# converting the month into a factor
data$month<- as.factor(data$month)

# renaming the hour column
data <- data %>% rename(hour = hr)

# making sure that all hours are within the range 0-23
sum(!(data$hour %in% 0:23))

# converting the hour into a factor
data$hour <- as.factor(data$hour)

# making sure that the holiday values are always either 0 or 1
sum(!(data$holiday %in% c(0,1)))

# converting the holiday into a factor
data$holiday <- as.factor(data$holiday)

# recalculating the weekday based on the date
data <- data %>% mutate(weekday= wday(date, label = FALSE))

#converting the weekday into a factor 
data$weekday <- as.factor(data$weekday)

# making sure that the working day values are always either 0 or 1
sum(!(data$workingday %in% c(0,1)))

#converting the working day into factor
data$workingday<- as.factor(data$workingday)

# making sure that the weathersit values are always either 1-4
sum(!(data$weathersit %in% c(1,2,3,4)))

#converting the weathersit into factor
data$weathersit<-as.factor(data$weathersit)

#converting all the normalized variables into numeric by replacing the decimal point ',' 
# with the decimal point '.' and turning into numeric
data$temp <- as.numeric(sub(",",".",data$temp))
data$atemp <- as.numeric(sub(",",".",data$atemp))
data$hum <- as.numeric(sub(",",".",data$hum))
data$windspeed <- as.numeric(sub(",",".",data$windspeed))

#converting the continuous variables into numeric 
data$instant<-as.numeric(data$instant)
data$casual <- as.numeric(data$casual)
data$registered <- as.numeric(data$registered)
data$cnt<- as.numeric(data$cnt)

#---------------------------------------------------------------------------------------------------------------------------------------#


##############################
#--- Descriptive Analysis ---#
##############################

#descriptive statistics for numeric variables 
data <- data[,-c(1,2)]
require(psych)
index <- sapply(data, class) == "numeric"
ridenum <- data[,index] 
head(ridenum)
table1<-round(t(describe(ridenum)),2)

library("kableExtra")
kbl(t(table1),digits = 18, caption = "Statistics for Numeric Variables", valine="t",allign="H") %>%
kable_paper(full_width = F) %>%
kableExtra::kable_styling(latex_options = "hold_position")

#θα μπορούσαμε να παράτηρήσουμε οτι mean = median για τα κανονικοποιημένα
#οτί έχουμε διαφορές μεταξύ casual kai registered, και για το που κυμαίνεται το cnt και τη διασπορά που έχει. (skew/ kurtosis?)


#############################################
#--Visual Analysis for numerical variables--#
#############################################

n <- nrow(ridenum)

#histograms
par(mfrow=c(2,4))
for (i in 1:7){
  hist(ridenum[,i], main=names(ridenum)[i], xlab=names(ridenum)[i], col='light blue')
}

#QQ plots
par(mfrow=c(2,4))
for (i in 1:7){
  qqnorm(ridenum[,i], main=names(ridenum)[i],col='blue')
  qqline(ridenum[,i])
}

#Boxplots 
par(mfrow=c(2,4))
for (i in 1:7){
  boxplot(ridenum[,i], main=names(ridenum)[i],col='light blue', horizontal=TRUE)
}


#όλα τα τεστ κανονικότητας απορρίπτονται, το περιμέναμε και απο τα γραφήματα γτ δεν είναι συμμετρικo

sapply(ridenum,shapiro.test)



#####################################
#--- Visual Analysis for factors ---#
#####################################

require(psych)
ridefac <- data[,!index] # αυτο θα μου δώσει τις κατηγορικές. 
n <- nrow(ridefac)


#visualize working days and holidays 
par(mfrow=c(1,1))
fac <-ridefac[,c(5,7)]
barplot(sapply(fac,table)/n, horiz=T, las=1, col=2:3, ylim=c(0,8), cex.names=0.9)
legend('center', fil=2:3, legend=c('No','Yes'), ncol=2, bty='n',cex=1.2)


par(mfrow=c(2,2))
plot(table(ridefac[,1])/n, type='h', xlim=range(ridefac[,1])+c(-1,1), main=names(ridefac)[1], ylab='Relative frequency')
plot(table(ridefac[,2])/n, type='h', xlim=range(ridefac[,2])+c(-1,1), main=names(ridefac)[2], ylab='Relative frequency')
plot(table(ridefac[,3])/n, type='h', xlim=range(ridefac[,3])+c(-1,1), main=names(ridefac)[3], ylab='Relative frequency')
par(mfrow=c(2,3))
for (i in c(1,2,3,4,6,8)){
barplot(table(data[,i]), col="light blue", xlab=names(data)[i], ylab='Relative frequency')
}
?barplot
par(mfrow=c(2,3))
plot(table(ridenum[,1])/n, type='h', xlim=range(ridenum[,1])+c(-1,1), main=names(ridenum)[1], ylab='Relative frequency')
plot(table(ridenum[,2])/n, type='h', xlim=range(ridenum[,2])+c(-1,1), main=names(ridenum)[2], ylab='Relative frequency')
plot(table(ridenum[,3])/n, type='h', xlim=range(ridenum[,3])+c(-1,1), main=names(ridenum)[3], ylab='Relative frequency')
plot(table(ridenum[,4])/n, type='h', xlim=range(ridenum[,4])+c(-1,1), main=names(ridenum)[4], ylab='Relative frequency')
plot(table(ridenum[,5])/n, type='h', xlim=range(ridenum[,5])+c(-1,1), main=names(ridenum)[5], ylab='Relative frequency')
plot(table(ridenum[,6])/n, type='h', xlim=range(ridenum[,6])+c(-1,1), main=names(ridenum)[6], ylab='Relative frequency')

plot(ride)



install.packages('ggplot2')
library(ggplot2)
     ggplot(ridefac, aes(x=reorder(year,cnt,function(x)-length(x))))
par(mfrow=c(2,4))
ggplot(data, aes(x=season),y=cnt)+geom_bar(fill="lightblue",col='red') +labs(y='number of rides',x='season')+ggtitle('Number of rides per season')
ggplot(data, aes(x=year),y=cnt)+geom_bar(fill="lightblue",col='red') +labs(y='number of rides',x='year')+ggtitle('Number of rides per year')
ggplot(data, aes(x=month),y=cnt)+geom_bar(fill="lightblue",col='red') +labs(y='number of rides',x='month')+ggtitle('Number of rides per month')
ggplot(data, aes(x=hour),y=cnt)+geom_bar(fill="lightblue",col='red') +labs(y='number of rides',x='hour')+ggtitle('Number of rides per hour')
ggplot(data, aes(x=weekday),y=cnt)+geom_bar(fill="lightblue",col='red') +labs(y='number of rides',x='weekday')+ggtitle('Number of rides per day')
ggplot(data, aes(x=weathersit),y=cnt)+geom_bar(fill="lightblue",col='red') +labs(y='number of rides',x='weather')+ggtitle('Number of rides per weather condition')



##################################
#----- Pairwise comparisons -----#
##################################

#Pairs of numerical variables


pairs(ridenum) #σκατερ πλοτ ανα 2

#πολύ καλύτερο το carrplot// 
require(corrplot)
#τα πιο σκουρόχρωμα είναι τα πιο σημαντικά
corrplot(cor(ridenum))#Use ?corrplot to explore other methods

par(mfrow=c(2,3))
for(j in 1:6){
  plot(ridenum[,j], ridenum[,7], xlab=names(ridenum)[j], ylab='Cnt',cex.lab=1.5)
  abline(lm(ridenum[,7]~ridenum[,j]))
}

##########################################################################

# (our response) on factor variables ( πως διαχειρίζομαι τα φακτορς)
par(mfrow=c(3,3))
for(j in 1:8){
  boxplot(ridenum[,7]~ridefac[,j], xlab=names(ridefac)[j], ylab='cnt',cex.lab=1.5)
}
#εδώ βλέπουμε ότι για το year holiday kai weathersit to median διαφέρει άρα περιμένουμε οι μεταβλητές αυτές
#να είναι στατ σημαντικές για το μοντέλο μας

#αντίθετα το working day kai to weekday δεν φαίνονται να έχουν διαφορές επομένως έχουμε ενδείξεις οτί μπορεί να αφαιρεθούν
#από το μοντέλο. 

#εδω θα μπορούσαμε να κάνουμε και ελέγχους φισερ για τους μέσους. 
#να το ψάξω

############################
#------ Correlations ------#
############################

round(cor(ridenum), 2) #Correlations valid on the numerical variables 

#( το πιρσον μετράει μόνο γραμμική συσχέτιση, μπορεί να υπάρχει αλλη συσχετιση)
require(corrplot)
par(mfrow = c(1,1))
corrplot(cor(ridenum), method = "number") 

#---------------------------------------------------------------------------------------------------------#
data <- data %>% 
  mutate(hour = ifelse(hour %in% c(9, 10, 11,12,13,14,15,21,22), 2,
                       ifelse(hour %in% c(7,8,16,17,18,19,20), 3,
                                                                   1)))
data$hour<-as.factor(data$hour)

install.packages('psych')
library(psych)
model<-lm(cnt~.-registered-casual,data=data)
X <- model.matrix(model)[,-1]
require(car)
vif(model)
summary(model)
anova(model)

facta<-data.frame(data$month,data$season)
facta <-table(data$month,data$season)
chisq.test(facta)

facta2<-table(data$season,data$year)
chisq.test(facta2)

facta3<-table(data$workingday,data$holiday)
chisq.test(facta3)

model<-lm(cnt~.-atemp-registered-casual-month,data=data)
model<-lm(cnt~.-atemp-registered-casual,data=data)
X <- model.matrix(model)[,-1]

modelfac<-lm(cnt~month+season, data=data)
vif(modelfac)
X <- model.matrix(modelfac)[,-1]

acf(data$month)
require(glmnet)   
lasso <- glmnet(X, data$cnt)
plot(lasso, xvar = "lambda", label = T)
#Use cross validation to find a reasonable value for lambda 
lasso1 <- cv.glmnet(X, data$cnt, alpha =1)
lasso1$lambda
lasso1$lambda.min
lasso1$lambda.1se
plot(lasso1)
#θα κρατήσω το δευτερο λ γιατί παραμένει μικρό το μιν σκοθερ ερρορ και έχουμε λιγότερες μεταβλητές στο μοντέλο. 
coef(lasso1, s = "lambda.min")
coef(lasso1, s = "lambda.1se")
plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)

model3<-lm(cnt~.-atemp-registered-casual-holiday-weekday-workingday-windspeed-month,data=data)
model2<-lm(cnt~.-atemp-registered-casual-holiday-weekday-workingday-windspeed-month,data=data)
model2<-lm(cnt~.-atemp-registered-casual-weekday-workingday-month-holiday,data=data)
X <- model.matrix(model2)[,-1]
vif(model2)
anova(model3,model2)
anova(model2)
summary(model2)

step(model2, direction='both') #Stepwise 
#αυτο ξεκινάει με το σταθερό και προσθέτει
mnull <- lm(PRICE~1,data=realEstate)
step(mnull, scope=list(lower=mnull,upper=mfull), direction='both')
#αυτο το σαμμαρυ γιατί επιλέχτηκε απο το ΑΙΣ 
summary(step(model2, direction='both')) #Summary of the model selected by stepwise επιστεφει κατευθειαν το τελικο 
mfinal <-step(model2, direction='both')
summary(mfinal)
anova(mfinal)


require(car)
round(vif(mfinal),1)


true.r2 <- 1-sum(model7$res^2)/((n-1)*var(example61$price))
true.r2



#----------------------------------------------------------------------------------------------------------------------------------#


######################################################
#---------------- Assumptions -----------------------#
######################################################


plot(mfinal, which = 2) 

Stud.residuals <- rstudent(mfinal)
shapiro.test(Stud.residuals)
#Normality of the residuals

#μπορουμε να κανουμε εναν σαπιρο
#Costant variance 
plot(mfinal, which = 3)
Stud.residuals <- rstudent(mfinal)
yhat <- fitted(mfinal)
par(mfrow=c(1,2))
#θελουμε να βλεπουμε ολες τις τελιτσες μεσα και να μην ανοιγει.  και μπορω να ρο ελεγξω με ncv test 
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)

# ------------------
library(car)
ncvTest(mfinal)

# ------------------
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(mfinal)~yhat.quantiles)
boxplot(rstudent(mfinal)~yhat.quantiles)

# ------------------
# non linearity για τη γραμμικότητα
# ------------------
library(car)
residualPlot(mfinal, type='rstudent')
residualPlots(mfinal, plot=F, type = "rstudent")

# -------------------
# Independence 
# -------------------
plot(rstudent(mfinal), type='l')
library(randtests); runs.test(mfinal$res)
library(lmtest);dwtest(mfinal)
library(car); durbinWatsonTest(mfinal)

#top 10 leverages points

plot(mfinal,4,id.n=10)

data <- data[-c(54,67,399,691,832,864,873,938,942,1072),]


#---------------------------------------- TRANSFORMATIONS -----------------------------------------------#

fdata<-data[,c(1,2,4,8,9,11,12,15)]
fdata$hour<-as.numeric(fdata$hour)
fdata$month <- as.numeric(fdata$month)
# ------------------
# Log
# ------------------
# model 1 only with lotsize
# ------------------

par(mfrow=c(1,2))
plot(lm(cnt~.,data=fdata),2, main='Price')
plot(lm(sqrt(cnt)~.,data=fdata),2, main='Price')
plot(lm(log(cnt)~.+poly(temp,5)+poly(hum,5)+poly(windspeed,5),data=fdata),2, main='log of price')
logmodel<-lm(log(cnt)~.+poly(temp,2),data=fdata)


logmodel<-lm(sqrt(cnt)~.+poly(temp,4)+(hum+1)-windspeed,data=fdata)
summary(logmodel)


par(mfrow=c(1,2))
plot(lm(cnt~.,data=fdata),2, main='Price')
plot(lm(log(cnt)~.,data=fdata),2, main='Price')
plot(lm((cnt)~.+poly(temp,2)+poly(hum,4)-season-year-hum,data=fdata),2, main='log of price')

logmodel<-lm(log(cnt)~.+poly(temp,2)-hour-year-weathersit,data=fdata)
Stud.residuals <- rstudent(logmodel)
par(mfrow=c(2,2))
plot( logmodel, 2 )
#shapiro.test(Stud.residuals)
plot( logmodel, 3 )
residualPlot(logmodel, type='rstudent')
plot(rstudent(logmodel), type='l')

ncvTest(logmodel)
residualPlots(logmodel, plot=F)
summary(logmodel)
# ------------------
# model 2 only with poly-lotsize
# ------------------
par(mfrow=c(1,2))
plot(lm(cnt~.+poly(temp),data=fdata),2, main='Price')
plot(lm(log(cnt)~.+poly(hum,5)+poly(temp,5)-weathersit,data=fdata),2, main='log of price')

logmodel<-lm(log(cnt)~.+poly(hum,4),data=fdata)
summary(logmodel)
par(mfrow=c(2,2))
plot( logmodel, 2 )
plot( logmodel, 3 )
residualPlot(logmodel, type='rstudent')
plot(rstudent(logmodel), type='l')


yhat <- fitted(logmodel)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(logmodel)~yhat.quantiles)
boxplot(rstudent(logmodel)~yhat.quantiles)


ncvTest(logmodel)
residualPlots(logmodel, plot=F)
# ------------------
# model 3 poly-lotsize-5 + the rest of covariates
# ------------------
par(mfrow=c(1,2))
plot(lm(price~.+poly(lotsize,5),data=example61),2, main='Price')
plot(lm(log(price)~.+poly(lotsize,5),data=example61),2, main='log of price')

logmodel<-lm(log(price)~.+poly(lotsize,5),data=example61)
par(mfrow=c(2,2))
plot( logmodel, 2 )
plot( logmodel, 3 )
residualPlot(logmodel, type='rstudent')
plot(rstudent(logmodel), type='l')

ncvTest(logmodel)
residualPlots(logmodel, plot=F)
# ------------------
# finding which covariate destroys normality
# ------------------
library('nortest')
for (k in 1:7){ 
  p1<- shapiro.test(rstudent( lm( log(cnt)~.+poly(temp,2)+poly(hum,4),data=data[,-k] )))$p.value
  p2 <- lillie.test(rstudent( lm( log(cnt)~.+poly(temp,2)+poly(hum,4),data=data[,-k] )))$p.value 
  print( round(c(k, p1, p2 ),3) )
}

logmodel<-lm(log(price)~.+poly(lotsize,5)-stories,data=example61)
par(mfrow=c(2,2))
plot( logmodel, 2 )
plot( logmodel, 3 )
residualPlot(logmodel, type='rstudent')
plot(rstudent(logmodel), type='l')

ncvTest(logmodel)
residualPlots(logmodel, plot=F)


# ------------------
# model 4 poly(lotsize,5)+poly(garage,2) + all remaining - stories 
# ------------------


par(mfrow=c(1,2))
plot(lm(price~.+poly(lotsize,5)+poly(garage,2)-stories,data=example61),2, main='Price')
plot(lm(log(price)~.+poly(lotsize,5)+poly(garage,2)-stories,data=example61),2, main='log of price')

logmodel<-lm(log(price)~.+poly(lotsize,5)+poly(garage,2)-stories, data=example61)
par(mfrow=c(2,2))
plot( logmodel, 2 )
plot( logmodel, 3 )
residualPlot(logmodel, type='rstudent')
plot(rstudent(logmodel), type='l')

ncvTest(logmodel)
residualPlots(logmodel, plot=F)

shapiro.test(rstudent(logmodel))
lillie.test(rstudent(logmodel))


################################################################################
y<-sum(data$windspeed==0)
n<-1500
for (i in 1:n)
{
  if (data$windspeed[i]==0)
  {data$windspeed[i]<-abs(round(rnorm(1,0.19,0.13),3))}
}

for (i in 1:n)
{
  if (data$windspeed[i]==0){
    data <- data[-i,]
}}

n<-nrow(data)


for (i in 1:n)
{
  if (d1[i,]==0)
    d1[i,]<-abs(round(rnorm(1,0,67),3))
}
########################################
d1<-data$windspeed
d1<- as.data.frame(d1)
for (i in 1:n){
  d1[i,]<-67*data$windspeed[i]
}
z<-sum(d1$d1==0)
m<-min(d1$d1)
sample_values<-sample(d1$d1,z)
s<-0
for (i in 1:n)
{
  if (d1[i,]==0)
    s<-sample(m:67,1)
  if (s!=0)
    d1[i,]<-s
  else i=i-1
}
d2<-0
d2<-as.data.frame(d2)
for (i in 1:n){
  d2[i,]<-d1$d1[i]/67
}

hist(d2$d2)


#-----------------------------------------------------------------------------------------------------------------------------------#


setwd("C:\\Users\\Maria Skoli\\Documents\\R")  
getwd()
test_data <- read.csv('bike_test.csv', header = TRUE, sep = ';')


test_data <- test_data[,-1]


# removing all missing values
test_data <- test_data[complete.cases(test_data), ]

# getting a quick view of the data
str(test_data)

# renaming the date
test_data <- test_data %>% rename(date = dteday)


# recalculating the season based on the date
test_data <- test_data %>% 
  mutate(season = ifelse(month(date) %in% c(12, 1, 2), 4,
                         ifelse(month(date) %in% c(3, 4, 5), 1,
                                ifelse(month(date) %in% c(6, 7, 8),2,
                                       3))))
# converting the season into a factor
test_data$season <- as.factor(test_data$season)

# making sure that all dates belong to the two indicated years (2011 and 2012)
sum(!(year(test_data$date) %in% c(2011, 2012)))

# recoding the year based on the date
test_data <- test_data %>% 
  rename(year = yr) %>% 
  mutate(year = ifelse(year(date) == "2011", 0,1))

# converting the year into a factor
test_data$year <- as.factor(test_data$year)

# recoding the month based on the date
test_data <- test_data %>% 
  rename(month = mnth) %>% 
  mutate(month = month(date))

# converting the month into a factor
test_data$month<- as.factor(test_data$month)

# renaming the hour column
test_data <- test_data %>% rename(hour = hr)

# making sure that all hours are within the range 0-23
sum(!(test_data$hour %in% 0:23))

# converting the hour into a factor
test_data$hour <- as.factor(test_data$hour)

# making sure that the holiday values are always either 0 or 1
sum(!(test_data$holiday %in% c(0,1)))

# converting the holiday into a factor
test_data$holiday <- as.factor(test_data$holiday)

# recalculating the weekday based on the date
test_data <- test_data %>% mutate(weekday= wday(date, label = FALSE))

#converting the weekday into a factor 
test_data$weekday <- as.factor(test_data$weekday)

# making sure that the working day values are always either 0 or 1
sum(!(test_data$workingday %in% c(0,1)))

#converting the working day into factor
test_data$workingday<- as.factor(test_data$workingday)

# making sure that the weathersit values are always either 1-4
sum(!(test_data$weathersit %in% c(1,2,3,4)))

#converting the weathersit into factor
test_data$weathersit<-as.factor(test_data$weathersit)

#converting all the normalized variables into numeric by replacing the decimal point ',' 
# with the decimal point '.' and turning into numeric
test_data$temp <- as.numeric(sub(",",".",test_data$temp))
test_data$atemp <- as.numeric(sub(",",".",test_data$atemp))
test_data$hum <- as.numeric(sub(",",".",test_data$hum))
test_data$windspeed <- as.numeric(sub(",",".",test_data$windspeed))

#converting the continuous variables into numeric 
test_data$instant<-as.numeric(test_data$instant)
test_data$casual <- as.numeric(test_data$casual)
test_data$registered <- as.numeric(test_data$registered)
test_data$cnt<- as.numeric(test_data$cnt)

test_data<-test_data[,-c(1,2)]

test_data <- test_data %>% 
  mutate(hour = ifelse(hour %in% c(9, 10, 11,12,13,14,15,21,22), 2,
                       ifelse(hour %in% c(7,8,16,17,18,19,20), 3,
                              1)))
test_data$hour<-as.factor(test_data$hour)



mod<- lm(cnt~+season+year+hour+weathersit+temp+hum,data=test_data)
summary(mod)

mod<- lm(cnt~+season+year+hour+temp+hum,data=data)
summary(mod)

p<- predict(mod,test_data)

real<-test_data$cnt

d=round(real-p,2)

#-----------------------------------------------

#Use lasso regression model to predict response value 
y_predicted<-predict(mfinal, s=best_lamba, test_data)

#find SST and SSE
sst<-sum((y-mean(y))^2)
sse<-sum((y_predicted-y)^2)

#find R-Squared
rsq <- 1 - see/sst
rsq



predict.full <- (predict(mod, test_data))
mean(predict.full)

# Extract Predictions
#predict.full <- exp(predict(model.full, ames_test))

# Extract Residuals
resid.full <- test_data$cnt - predict.full

# Calculate RMSE
rmse.full <- sqrt(mean(resid.full^2))
rmse.full

data<-data[,-c(10,13,14)]

mfull<- lm(cnt~.,data=data)
mnull<-lm(cnt~1,data=data)
mfinal<-lm(cnt~+season+year+hour+temp+hum,data=data)
anova(mfull,mfinal)
anova(mnull,mfinal)
anova(mnull,mfull)

summary(mfull)
summary(mnull)
summary(mfinal)


##mnull

predict.full <- (predict(mnull, test_data))
mean(predict.full)

resid.full <- test_data$cnt - predict.full

# Calculate RMSE
rmse.full <- sqrt(mean(resid.full^2))
rmse.full

#mfull

predict.full <- (predict(mfull, test_data))
mean(predict.full)

# Extract Residuals
resid.full <- test_data$cnt - predict.full

# Calculate RMSE
rmse.full <- sqrt(mean(resid.full^2))
rmse.full

##mfinal

predict.full <- (predict(mfinal, test_data))
mean(predict.full)

# Extract Residuals
resid.full <- test_data$cnt - predict.full

# Calculate RMSE
rmse.full <- sqrt(mean(resid.full^2))
rmse.full
