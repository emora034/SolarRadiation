library(skimr)
library(dplyr)
library(reshape2) #melt function
library(ggplot2)
library(tidyverse)
library(lubridate)
library(caret)
library(gsubfn)
library(MASS)
library(tidyr)
library(psych) #pairs.panels function
library(corrplot)
library(plyr) #revalue function
library(devtools)
library(ggpubr) #ggarrange function


set.seed(13)
#load the dataset
radiation <- read.csv("C:/Users/emora/OneDrive/Desktop/Adv Reg/Project Part I/SolarRadiation.csv", header=TRUE)

###############################
###############################
###### Data Engineering #######

#check for missing values
skim(radiation) #no missing values

#change the timestamp to date/time type
radiation$UNIXTime<-as.POSIXct(radiation$UNIXTime, origin="1970-01-01")
#note it's returned in CEST 

#We now change the timestap from date/time to day of the year to avoid
#any confusions between time zones and the time the date was collected.
#This way we can eliminate the first 3 columns.

DayofYear<-as.numeric(strftime(radiation$UNIXTime, format="%j"))
Month<-as.numeric(strftime(radiation$UNIXTime, format="%m"))

#check values and summary before adding to our datafram
summary(DayofYear)
min(DayofYear)
max(DayofYear)

#Add to dataframe
radiation<-cbind(DayofYear,radiation)
radiation<-cbind(Month,radiation)
#change to factor and rename
radiation$Month<-factor(radiation$Month)
radiation$Month<-revalue(radiation$Month, c("9"="September", "10"="October",
                                            "11"="November", "12"="December"))

#eliminate measurements from the first month of the year
#there are only 132
sum(radiation$DayofYear==1)
radiation<-radiation%>%filter(DayofYear!=1)
summary(radiation$DayofYear)
#We can now get rid of UNIXTime, Data, and Time columns.
radiation<-radiation[,c(-3,-4,-5)]


#change the sunrise and sunset variables from factor to time,
#return the total minutes given the recorded time, and compute
#the hours of daylight per day (diff between sunrise & sunset)
sunrise<-as.numeric(gsubfn("(\\d+):(\\d+):(\\d+)", 
                           ~ as.numeric(x)*60 + as.numeric(y) + as.numeric(z)/60, 
                           as.character(radiation$TimeSunRise)))

sunset<-as.numeric(gsubfn("(\\d+):(\\d+):(\\d+)", 
                          ~ as.numeric(x)*60 + as.numeric(y) + as.numeric(z)/60, 
                          as.character(radiation$TimeSunSet)))

Daylight<-round((sunset-sunrise)/60, 2)

#add daylight to the dataframe and get rid of sunrise, sunset
radiation<-radiation[,c(-9,-10)]
radiation<-cbind(Daylight,radiation)

#reorder data
radiation<-radiation[,c(4,2,3,1,5:9)]

######## DESCRIPTIVE #########
#Since there are several measurements taken in a day, we will group by
#the mean values of all vars given that day.
radiation2<-aggregate(radiation[,c(1,3:9)], list(radiation$DayofYear), mean)

t<-ggplot(radiation2, aes(x=Group.1, y=Temperature)) + ylab("Temperature") + 
  xlab("Day of the Year")+geom_line(color="red") + ggtitle("Temperature")
r<-ggplot(radiation2, aes(x=Group.1, y=Radiation)) + ylab("Radiation") + 
  xlab("Day of the Year")+geom_line(color="dodgerblue3") + ggtitle("Radiation")
ggarrange(t,r,ncol=2, nrow=1)
##############################
#PV doesn't work at night and typically require a minimum of 100 to 
# to provide minimum power. Therefore, we only consider obs with 
#radiation >100 W/m2 which is also the minimum solar radiance past sunrise

radiation<-radiation%>%filter(Radiation>=100)

#colMeans(radiation[,c(1,3:9)])
###################################
###################################
########### Descriptive ###########
radiation2<-aggregate(radiation[,c(1,3:9)], list(radiation$DayofYear), mean)

radiation2<-radiation2[,-1]

in_train <- createDataPartition(radiation2$Radiation, p = 0.75, list = FALSE)  # 75% for training
training <- radiation2[ in_train,]
testing <- radiation2[-in_train,]
nrow(training)
nrow(testing)
colMeans(training[,c(1,3:8)])

#individual analysis of the variables
#Plots of densities of all variables in order to see the distributions
summarydata_melt <- melt(training)

ggplot(summarydata_melt, aes(value, fill=variable)) +
  geom_density() +
  facet_wrap(~variable, scales="free") +
  theme(legend.position="none")

#check the temperature behaviour
ggplot(training, aes(x=DayofYear, y=Temperature)) + ylab("Temperature") + 
  xlab("Day of the Year")+geom_line(color="firebrick") + ggtitle("Temperature")

#DayofYear show data was mostly recorded in autum through winter,
#starting on September

#check the temperature behaviour
ggplot(training, aes(x=Temperature, y=Radiation)) + ylab("Radiation") + 
  xlab("Temperature")+geom_point(color="goldenrod3") + ggtitle("Radiation")


ggplot(training, aes(x=(Temperature), y=Radiation)) + ylab("Radiation") + 
  xlab("Temperature")+geom_point(color="red", size=3) + ggtitle("Radiation")


pairs.panels(training, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             cex=5,
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

corrad<-cor(training)
corrplot(corrad, order="hclust")


corr_rad <- sort(cor(training)["Radiation",], decreasing = T)
corr=data.frame(corr_rad)
ggplot(corr,aes(x = row.names(corr), y = corr_rad)) + 
  geom_bar(stat = "identity", fill = "lightblue") + 
  scale_x_discrete(limits= row.names(corr)) +
  labs(x = "Predictors", y = "Radiation", title = "Correlations") + 
  theme(plot.title = element_text(hjust = 0, size = rel(1.5)),
        axis.text.x = element_text(angle = 45, hjust = 1))+ylim(-0.5,1)


# Simple regression: try first the most relevant predictor from previous analysis
linFit <- lm(Radiation ~ Temperature, data=training)
summary(linFit)
# R2 is roughly 48%, temperature is highly significant

# Plot model
ggplot(training, aes(x=Temperature, y=Radiation)) + geom_line(color="red", size=1.2) + 
  geom_line(y=predict(linFit))  + theme_minimal()

# residuals
par(mfrow=c(2,2))
plot(linFit, pch=23 ,bg='orange',cex=2)

#Simple poly fit
poly.fit<-lm(Radiation~poly(Temperature,2), training)
summary(poly.fit)

# Plot model
ggplot(training, aes(x=Temperature, y=Radiation)) + geom_line(color="red", size=1.2) + 
  geom_line(y=predict(poly.fit))  + theme_minimal()

# residuals
par(mfrow=c(2,2))
plot(poly.fit, pch=23 ,bg='orange',cex=2)

# 
# The variables in the polynomial are correlated and poly()
# tries to avoid these correlation by producing orthogonal polynomials
# (scales the columns in order each column is orthogonal to the previous ones)
# 

#log transform of temp
log.fit<-lm(log(Radiation)~log(Temperature), training)
summary(log.fit)
# Plot model
ggplot(training, aes(x=Temperature, y=Radiation)) + geom_line(color="red", size=1.2) + 
  geom_line(y=predict(log.fit))  + theme_minimal()
# residuals
par(mfrow=c(2,2))
plot(sq.fit, pch=23 ,bg='orange',cex=2)

# Hence, better to use poly(): better estimation but worse interpretation
# Residuals
ggplot() + aes(x=training$Radiation, y=residuals(poly.fit)) + geom_point()
# ok

###########################
######## MULTIPLE #########
#log transform of temp
linfit2<-lm(Radiation~Temperature+Humidity, training)
summary(linfit2)
# Plot model
ggplot(training, aes(x=Temperature, y=Radiation)) + geom_line(color="red", size=1.2) + 
  geom_line(y=predict(linfit2))  + theme_minimal()
# residuals
par(mfrow=c(2,2))
plot(sq.fit, pch=23 ,bg='orange',cex=2)

######### BACKWARD AIC #############

#stepAIC
modall<-lm(Radiation~., training)
stepAIC(modall)

modAIC<-lm(Radiation~Temperature+Humidity+WindDirection.Degrees.+Speed, training)
summary(modAIC)

# residuals
par(mfrow=c(2,2))
plot(modAIC, pch=23 ,bg='orange',cex=2)

#without speed.
modAIC2<-lm(Radiation~Temperature+Humidity+WindDirection.Degrees., training)
summary(modAIC2)

### Prediction

# Use modAIC with speed
predictions <-(predict(modAIC, newdata=testing))
cor(testing$Radiation, predictions)^2
RMSE <- sqrt(mean((predictions - testing$Radiation)^2))
RMSE

###############################
###############################
######## Advance methods ######
# Each model can be automatically tuned and evaluated 
# In this case, we are goint to use 4 repeats of 5-fold cross validation
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, repeats = 4)
# Linear regression
test_results <- data.frame(Radiation = testing$Radiation)

#Model from AIC
modaic<-Radiation~Temperature+Humidity+WindDirection.Degrees.+Speed
lm_tune <- train(modaic, data = training, 
                 method = "lm", 
                 preProc=c('scale', 'center'),
                 trControl = ctrl)
lm_tune
test_results$lm <- predict(lm_tune, testing)
postResample(pred = test_results$lm,  obs = test_results$Radiation)

qplot(test_results$lm, test_results$Radiation) + 
  labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
  #lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()

#################################
#Model 
modaic<-Radiation~Temperature+Humidity+WindDirection.Degrees.+Speed
lm_tune <- train(modaic, data = training, 
                 method = "lm", 
                 preProc=c('scale', 'center'),
                 trControl = ctrl)
lm_tune
test_results$lm <- predict(lm_tune, testing)
postResample(pred = test_results$lm,  obs = test_results$Radiation)

qplot(test_results$lm, test_results$Radiation) + 
  labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
  #lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()

########################
mod2<-log(Radiation)~log(Temperature)+Humidity+WindDirection.Degrees.+Speed
test_results2 <- data.frame(Radiation = log(testing$Radiation))

alm_tune <- train(mod2, data = training, 
                  method = "lm", 
                  preProc=c('scale', 'center'),
                  trControl = ctrl)
length(coef(alm_tune$finalModel))

test_results2$alm <- predict(alm_tune, testing)
postResample(pred = test_results2$alm,  obs = test_results2$Radiation)
# is overfitting dangerous to predict?

qplot(test_results2$alm, test_results2$Radiation) + 
  labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
  #lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()

# Stepwise regression
ModelF<-Radiation~Temperature+Humidity+WindDirection.Degrees.+Speed+DayofYear*Daylight
step_tune <- train(ModelF, data = training, 
                   method = "leapSeq", 
                   preProc=c('scale', 'center'),
                   tuneGrid = expand.grid(nvmax = 2:8),
                   trControl = ctrl)
plot(step_tune)
# which variables are selected?
coef(step_tune$finalModel, step_tune$bestTune$nvmax)
test_results$seq <- predict(step_tune, testing)
postResample(pred = test_results$seq,  obs = test_results$Radiation)

##############################
##############################
######### Ridge ##############
library(glmnet)

# X matrix
X = model.matrix(ModelF, data=training)[,-1]  # skip column of ones
# y variable
y = log(training$Radiation)

# Ridge regression involves tuning a hyperparameter, lambda:
grid = seq(0, .1, length = 50)  # a 100-size grid for lambda (rho in slides)

# perform ridge regression
ridge.mod = glmnet(X, y, alpha=0, lambda=grid)  # alpha=0 for ridge regression

# Note in this way we are shrinking only beta's associated with variables, not beta0
dim(coef(ridge.mod))
coef(ridge.mod)
# for each of the 50 values of rho (lambda), there are 8 estimated parameters

# Solution path
plot(ridge.mod, xvar="lambda", ylim=c(-0.1,0.1))

X.test = model.matrix(ModelF, data=testing)[,-1]  # skip column of ones
y.test = log(testing$Radiation)

RSS.out = matrix(NA, nrow = 50, ncol = 1)
i <- 0
for(lambda in grid){
  i <- i + 1
  # Prediction with lambda = 4
  ridge.pred = predict(ridge.mod, s=lambda, newx=X.test)
  RSS.out[i] = sum((ridge.pred-y.test)^2)
}

plot(RSS.out)

idx = which.min(RSS.out)
RSS.out[idx]
lambdaopt = grid[idx]
lambdaopt

# Automatic way:
ridge.cv = cv.glmnet(X, y, type.measure="mse", alpha=0)
plot(ridge.cv)

# the lowest point in the curve indicates the optimal lambda: 
# the log value of lambda that best minimised the error in cross-validation
opt.lambda <- ridge.cv$lambda.min
opt.lambda
ridge.pred = predict(ridge.cv$glmnet.fit, s=opt.lambda, newx=X.test)
postResample(pred = ridge.pred,  obs = y.test)


# The Lasso

lasso.mod = glmnet(X, y, alpha=1, lambda=seq(.01, 1, length = 50))  
# alpha=1 for lasso regression

# Automatic way:
lasso.cv = cv.glmnet(X, y, type.measure="mse", alpha=1)
plot(lasso.cv)
# the lowest point in the curve indicates the optimal lambda: 
# the log value of lambda that best minimised the error in cross-validation
opt.lambda <- lasso.cv$lambda.min
opt.lambda
lasso.pred = predict(lasso.cv$glmnet.fit, s=opt.lambda, newx=X.test)
postResample(pred = lasso.pred,  obs = y.test)

# Solution path
plot(lasso.cv$glmnet.fit, xvar="lambda")

# Elastic net
modelLookup('glmnet')
elastic_grid = expand.grid(alpha = seq(0, .2, 0.01), lambda = seq(0, .1, 0.01))
glmnet_tune <- train(ModelF, data = training,
                     method='glmnet',
                     preProc=c('scale','center'),
                     tuneGrid = elastic_grid,
                     trControl=ctrl)
plot(glmnet_tune)
glmnet_tune$bestTune
test_results$glmnet <- predict(glmnet_tune, testing)
postResample(pred = test_results$glmnet,  obs = test_results$Radiation)

library(pls)

pcr.fit = pcr(ModelF, data=training, ncomp = 7, validation = "LOO")
summary(pcr.fit)
plot(RMSEP(pcr.fit), legendpos = "topright")

pred = predict(pcr.fit, ncomp=4, newdata=testing)
y.test = testing$Radiation
postResample(pred = pred,  obs = y.test)

