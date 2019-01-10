## This script predicts and analyzes different cryptocurrencies on different parameters and

crypto14<-read.csv(file.choose())

crypto14$name <- factor(crypto14$name, levels = c("Bitcoin","Litecoin","Ripple","Ethereum","Bitcoin Cash"), 
                        labels = c(1,2,3,4,5))
crypto16<-data.frame(crypto14)

summary(crypto14)
names(crypto14)
crypto14<-crypto14[1:6329,c(-1,-2,-4,-5,-10,-11)]
crypto14<-data.frame(crypto14)

summary(crypto14)


View(crypto14)




set.seed(2)
train.index <- sample(c(1:dim(crypto14)[1]), dim(crypto14)[1]*0.6)  
train.df <- crypto14[train.index, ]
valid.df <- crypto14[-train.index, ]


cori<-crypto14[,c(-1)]
crypto12<-crypto12[,c(-1)]
str(crypto14)
cor(cori)  
plot(cori)
plot(crypto14)

install.packages("sqldf")
library(sqldf)
names<-sqldf("Select count(distinct name) from crypto14")
names




head(crypto14)
#create category variable

fit1<-lm(spread~open+high+low+close+I(high+low+close+open^2),data=crypto14)
anova(fit1,fit.full,test="chi")
par(mfrow=c(2,2))
plot(fit1)


summary(fit1)

#for the qq plot some of the normality constraints have been violated as the points are not on the 45 degree line
#Residual vs fitted shows a curved relationship hence a cleary quadratic term is required
#Homoscedicity has been met as all the points in the residual vs leverage graph are a random collection around a linear line

library(class)
crypto.knn.pred<- knn(train.df, test=valid.df, cl=train.df, k=5)
crypto.knn.pred<-data.frame(crypto.knn.pred)
crypto.valid<-valid.df
cor(crypto.valid$close_ratio, as.numeric(crypto.knn.pred))#0.8034

residplot <-function(fit1, nbreaks=10) {
  z <- rstudent(fit1)
  hist(z, breaks =nbreaks, freq = FALSE,
       xlab = "Studentized Residuals ",main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x,mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,col="red", lwd=2, lty=2)
  legend("topright",legend = c("Normal Curve","Kernel Density curve"),lty=1:2,col=c("blue","red"), cex=.7)}

residplot(fit1)
#multiple linear regression ,classifocation  ,logistic regression then decision tree 
AIC(fit1,fit2)

library("car")


ins_model <- lm(,data=train.df)
ins_model
ins_predict <- predict(ins_model, valid.df)

ins_predict<-data.frame(ins_predict)

View(ins_predict)

#optimal predictor variables 


logit.reg.pred <- predict(fit1, valid.df, type = "response")
logit.reg.pred<-data.frame(logit.reg.pred)
View(logit.reg.pred)


# first 5 actual and predicted records
close<-data.frame(actual = valid.df$close_ratio, predicted = logit.reg.pred)

data.frame(actual = valid.df$close_ratio,poisson(link = log))

summary(logit.reg.pred)





predict<-ins_predict[!is.na(ins_predict)]
predict

scatterplot(open~high,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="high")

scatterplot(open~low,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="low")

scatterplot(open~close,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="close")

scatterplot(open~volume,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="volume")

scatterplot(open~spread,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="spread")

library("car")

library("residplot")
residplot(fit1)


durbinWatsonTest(fit1)


scatterplotMatrix(crypto14,spread = FALSE,lty.smooth=2,main="Scatter Matrix")

install.packages("gvlma")
library(gvlma)


rowSums(is.na(crypto16))

#counting 0's
gvlma1<-sapply(crypto14,function(x)sum(is.na(x)))


summary(gvlma1)
#logistic regression
logit.reg <- glm(name ~ ., data = train.df, family = "binomial") 

options(scipen=999)

summary(logit.reg)



logit.reg.pred <- predict(logit.reg, valid.df[, -1], type = "response")

logistic.prediction<-data.frame(actual = valid.df$spread, predicted = logit.reg.pred)

logit.reg <- glm(name ~ ., data = train.df, family = "binomial") 
logit.reg<-data.frame(logit.reg)
View(logis)

options(scipen=999)

summary(logit.reg)



logit.reg.pred <- predict(logit.reg, valid.df[, -1], type = "response")

logistic.prediction<-data.frame(actual = valid.df$spread, predicted = logit.reg.pred)

View(
  logistic.prediction)

logit.reg <- glm(crypto14$name ~ ., data = train.df, family = "binomial") 

#


fit.full <-glm(formula = close_ratio~ open+high+low+close+spread, family = binomial(), data = crypto14)



fit2<-glm(formula = close_ratio~ open+high+low+close, family = binomial(), data = crypto14)

summary(fit2)



anova(fit2,fit.full,test = "F")



testdata$prob <- predict(fit2, newdata=testdata, type="response")

testdata

testdata <- data.frame(name=c(1, 2, 3, 4, 5), open=mean(crypto14$open),high=mean(crypto14$high),
                       low=mean(crypto14$low),close=mean(crypto14$close),close_ratio=mean(crypto14$close_ratio))

predicti<-predict(fit2,interval="confidence")
predicti<-data.frame(predicti)
View(predicti)



View(fit2)

View(
  logistic.prediction)

logit.reg <- glm(crypto14$name ~ ., data = train.df, family = "binomial") 

#


fit.full <-glm(formula = close_ratio~ open+high+low+close+spread, family = binomial(), data = crypto14)

fit2<-glm(formula = spread~+open+high+low+close, family = binomial(), data = crypto14)

summary(fit2)



library(car)

outlierTest(crypto14$spread)
boxplot(crypto14$spread)


crypto15<-sort(crypto14$spread,decreasing = FALSE)

View(crypto14)
View(crypto15)
asc <- crypto14[order(crypto14$spread),] 





proj.dat<-data.frame(Bitcoin, Ethereum,Ripple, BitcoinCash, Litecoin)
proj<-data.frame(proj,proj.dat)
crypto14<-data.frame(crypto14)
View(crypto14)
summary(crypto14)


View(crypto14)



set.seed(2)
train.index <- sample(c(1:dim(crypto14)[1]), dim(crypto14)[1]*0.6)  
train.df <- crypto14[train.index, ]
valid.df <- crypto14[-train.index, ]


cori<-crypto14[,c(-1)]
crypto12<-crypto12[,c(-1)]
str(crypto14)
cor(crypto14)
cor(cori)  
plot(cori)
plot(crypto14)

#create category variable


summary(fit1)

#for the qq plot some of the normality constraints have been violated as the points are not on the 45 degree line
#Residual vs fitted shows a curved relationship hence a cleary quadratic term is required
#Homoscedicity has been met as all the points in the residual vs leverage graph are a random collection around a linear line

library(class)
crypto.knn.pred<- knn(train.df, test=valid.df, cl=train.df, k=5)
crypto.knn.pred<-data.frame(crypto.knn.pred)
crypto.valid<-valid.df
cor(crypto.valid$close_ratio, as.numeric(crypto.knn.pred))#0.8034

residplot(fit1)
#multiple linear regression ,classifocation  ,logistic regression then decision tree 

library("car")


ins_model <- lm(fit2,data=train.df)
ins_model
ins_predict <- predict(ins_model, valid.df)

ins_predict<-data.frame(ins_predict)

View(ins_predict)

predicted<-data.frame(ins_predict,valid.df$spread)
View(predicted)

#optimal predictor variables 


logit.reg.pred <- predict(fit1, valid.df, type = "response")
logit.reg.pred<-data.frame(logit.reg.pred)
View(logit.reg.pred)


# first 5 actual and predicted records



Close<-data.frame(actual = valid.df$close_ratio, predicted = logit.reg.pred)
View(Close)

data.frame(actual = valid.df$close_ratio,poisson(link = log))

summary(logit.reg.pred)




predict<-ins_predict[!is.na(ins_predict)]
predict

scatterplot(open~high,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="high")

scatterplot(open~low,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="low")

scatterplot(open~close,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="close")

scatterplot(open~volume,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="volume")

scatterplot(open~spread,data=crypto14,spread=FALSE,lty.smooth=2,pch=19,main="scatter plot",xlab="open",ylab="spread")

residplot(fit1)


## Function - To Install & Load Packages ##
package_install_load <- function(package) {
  
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dep = TRUE)
    if (!require(package, character.only = TRUE)) {
      stop("Package not found")
    }
  }
  else{
    print(paste("Package", package, "loaded"))
  }
}

### Load Required Packages ###
packages <- c("plyr", "dplyr", "readr", "tidyr","ggplot2","sqldf", "scales",
              "gains","rpart", "rpart.plot","ellipse","corrplot","RColorBrewer","jsonlite")
invisible(lapply(packages, package_install_load))

crypto <- crypto14

heat_data <- sqldf("SELECT name as Name, avg(open) as Open, avg(high) as High,
                   avg(low) as Low, avg(close) as Close, avg(close_ratio) as CloseRatio, 
                   avg(spread) as Spread
                   FROM crypto 
                   GROUP BY name ORDER BY name")

names(heat_data)
head(heat_data)

scale_rescale=function(x){
  return(rescale(scale(x)))
}

heat_data <-cbind(heat_data$Name,as.data.frame(lapply(heat_data[c(2,3,4,5,6,7)], scale_rescale)))

names(heat_data)[1] <- "Name"

head(heat_data)

heat_data <- heat_data %>%
  gather(Parameter, Value, Open:Spread)

head(heat_data)


ggplot(heat_data, aes(Name, Parameter )) +
  geom_tile(aes(fill = Value), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  #scale_colour_gradientn(colours = color_palette) +
  ylab("Market Parameters") +
  xlab("Cryptocurrency Type") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) 
