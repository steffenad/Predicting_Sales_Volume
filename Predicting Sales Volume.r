
library(Amelia)
library(dplyr)
library(corrplot)
library(corrgram)
library(caret)
library(ggplot2)
library(mlbench)
library(RODBC)
library(tidyverse)



#reading data sets 

existing2017 <- read.csv('existingproductattributes2017.csv')

incomplete2017 <- read.csv('newproductattributes2017.csv')


#converting "ProductNum" to character (index)

existing2017$ProductNum <- as.character(existing2017$ProductNum)

#finding and plotting missing values

any(is.na(existing2017))

missmap(existing2017, main="Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

#removing feature "BestSellersRank" 

existing2017 <- select(existing2017,-c('BestSellersRank'))

#correlation matrix

num.cols <- sapply(existing2017, is.numeric)

cor.data <- cor(existing2017[,num.cols])

var8 <- c('Price','5Stars','4Stars','3Stars','2Stars','1Stars','PosRev','NegRev',
          'Recom','Weight','Depth','Width','Heigth','Margin','Volume')

existing2017b <- cor.data

colnames(existing2017b)<-var8

corrgram(existing2017b, order = TRUE, lower.panel = panel.shade, upper.panel = panel.cor,
         text.panel = panel.txt, cex = 1.6)

#Feature Selection
# -> Collinearity: removing redudant features  with corr > 0.9

#removing 5StarReviews, 3StarReview and 1 StarReviews

existing2017 <- select(existing2017,-c("x5StarReviews", "x3StarReviews", "x1StarReviews"))

#creating dummy variables

dummy <- dummyVars("~ ProductType",data=existing2017)

existing2017dummy <- data.frame(predict(dummy,newdata=existing2017))

existing2017 <- data.frame(existing2017,existing2017dummy)

existing2017 <- existing2017[,-1]

#outlier detection with Cook's distance

mod <- lm(Volume ~ Price + x4StarReviews + x2StarReviews + NegativeServiceReview,
          data = existing2017)

cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=1.3, main="Influential Obs by Cooks distance")

sample_size <- nrow(existing2017)

abline(h = 4/sample_size, col="red")

text( x = 1:length(cooksd) + 1, y = cooksd,
  labels = ifelse(cooksd > 4 / sample_size, names(cooksd), ""),
  col = "red", cex = 1.1, offset = 3)

influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])

#removing outliers "ProductNum" 150,198,148,123,122

existing2017 <- existing2017[-influential,]

#data partition

set.seed(101)

#excluding "ProductNum"(character)

Produktnummer_com <- existing2017[,1]

#75% training data, 25% test data

existing2017 <- existing2017[,-1]

inTrain <- createDataPartition(y = existing2017$Volume,p = .75,list = FALSE)

training <- existing2017[ inTrain,]

testing <- existing2017[ -inTrain,]

#model training, using "caret" package

#cross validation with 3 repeats

options(warn=-1)
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3, summaryFunction = defaultSummary)

#training "Support Vector Machine" model

SVMfit <- train( Volume ~ .,data = training, method = "svmLinear2",
                 tuneLength = 5, trControl = ctrl)


options(warn=0)

#testing SVM model

predictions <- predict(SVMfit, newdata =testing)

pred_testing <- cbind(testing,predictions)

postResample(pred = pred_testing$predictions, obs = pred_testing$Volume)


#plotting residuals

res <- residuals(SVMfit)

res <- as.data.frame(res)

ggplot(res,aes(res)) + geom_histogram(fill="blue",alpha=0.5,bins=30)

#Importance of Features

varImp(SVMfit)

#training "Random Forest" model

tunegrid <- expand.grid(.mtry = (1:5))

ctrl <- trainControl(method = "repeatedcv", repeats = 3,
               summaryFunction = defaultSummary, search='grid')

RFfit <- train(Volume ~ ., data = training, method = "rf",
               tuneGrid = tunegrid, trControl = ctrl, importance = TRUE)


#testing "Random Forest" model

predictions <- predict(RFfit, newdata =testing)

predictions <- round(predictions)

pred_testing <- cbind(testing,predictions)

postResample(pred = pred_testing$predictions, obs = pred_testing$Volume)

#plotting residuals to check distribution of errors

res <- residuals(RFfit)

res <- as.data.frame(res)

ggplot(res,aes(res)) + geom_histogram(fill="blue",alpha=0.5,bins=20)

#Importance of Features

varImp(RFfit)

####comparing models####

resamps <- resamples(list(svm = SVMfit, rf=RFfit))

summary(resamps)

#preparation of new data set

incomplete2017$ProductNum <- as.character(incomplete2017$ProductNum)

incomplete2017<-select(incomplete2017,-c('BestSellersRank'))

incomplete2017 <- select(incomplete2017,-c("x5StarReviews", "x3StarReviews", "x1StarReviews"))

#creating dummy variables

dummys <- dummyVars("~ ProductType",data=incomplete2017)

dummy <- data.frame(predict(dummys,newdata=incomplete2017))

incomplete2017 <- data.frame(incomplete2017,dummy)

incomplete2017 <- incomplete2017[,-1]

#predictions for sales volume

predicted_Volume <- round(predict(RFfit,newdata=incomplete2017))

Produktnummer_inc <- incomplete2017['ProductNum']

incomplete_results <- cbind(incomplete2017,predicted_Volume)

incomplete_results1 <- select(incomplete_results, ProductNum, Price, ProfitMargin, predicted_Volume)


ProductType <- incomplete2017[,1]

incomplete_results <- cbind(ProductType,incomplete_results1)


#predicting profitability

incomplete_results <- mutate(incomplete_results, Profit = round(Price * ProfitMargin * predicted_Volume))

select(incomplete_results, ProductNum, Price, predicted_Volume, Profit)
