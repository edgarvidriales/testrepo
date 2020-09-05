par(mfrow=c(1,1))
Spot1<-top2018[c(-1,-2,-3)]
attach(Spot1)
pairs(danceability~., data = Spot1) 
round(vif(Spotty), 2) 

Spotty<-lm(danceability~., data = Spot1)

shapiro.test(Spotty$residuals) #can you add this test too please

#Do we need a transformation?



summary(powerTransform(Spotty))

Spotty2<-lm((danceability)^2~., data = Spot1)
plot(Spotty2, c(1,2))
shapiro.test(Spotty2$residuals) #Cool, very normal, can you add this test to the slide please
Spotty2
plot(hatvalues(Spotty2), rstandard(Spotty2), xlab="Leverage",
     ylab = "Standardized Residuals")
p <- 11
n <- nrow(Spotty)
abline(v= 2*(p+1)/n)
abline(h=c(-2,2,-4,4), lty=2)
which(abs(rstandard(Spotty2)) > 2 & hatvalues(Spotty2) > 2*(p+1)/n)
top2018$name[22] #Te Bote' by Nio Garcia
top2018$name[98] #No Brainer by DJ Khaled





#Backwards Elimination
summary(Spotty2) #Add screenshot of this one (Has highest R-squared)
lam2 <- update(Spotty2~ . - mode)
summary(lam2)
lam3 <- update(lam2, ~ . - duration_ms)
summary(lam3)
lam4<-update(lam3,~. -key) 
summary(lam4)
lam5<-update(lam4,~.-time_signature)
summary(lam5)
lam6<-update(lam5,~.-duration_ms)
summary(lam6)
lam7<-update(lam6,~.-mode)
summary(lam7)
lam8<-update(lam7,~.-liveness)
summary(lam8)
lam9<-update(lam8,~.-loudness)
summary(lam9)
lam10<-update(lam9,~.-acousticness)
summary(lam10)
lam11<-update(lam10,~.-tempo) 
summary(lam11) #and this one

summary(Spotty2)$adj.r.squared
summary(lam2)$adj.r.squared
summary(lam3)$adj.r.squared
summary(lam4)$adj.r.squared
summary(lam5)$adj.r.squared
summary(lam6)$adj.r.squared
summary(lam7)$adj.r.squared 
summary(lam8)$adj.r.squared
summary(lam9)$adj.r.squared
summary(lam10)$adj.r.squared

#Subset Selection
regsub_fit <- regsubsets((danceability)^2 ~ ., data=Spot1, nvmax=12)
summary(regsub_fit)
attributes(regsub_summ)
round(regsub_summ$adjr2, 4)
which.max(regsub_summ$adjr2) #y~energy+key+loudness+speechiness+acousticness+liveness+valence+tempo



#Backwards Stepwise selcetion
regbwd_fit <- regsubsets((danceability)^2 ~ ., data=Spot1, nvmax=12, method="backward")
regbwd_summ <- summary(regbwd_fit)
aic_vec <- n*log(regbwd_summ$rss/n) + 2*c(1:12)
which.min(aic_vec)
coef(regbwd_fit, 5) #y~energy+speechiness+acousticness+valence+tempo Using AIC

Spot3<-step(Spotty2)
summary(Spot3) 


Spot4<-step((Spotty2), k=log(n))
summary(Spot4) #y~.energy+speechiness+acousticness+valence Using BIC (dropped tempo)



#Cross Validation
library(glmnet)
library(ISLR)
x <- model.matrix((danceability)^2 ~ ., data=Spot1)
y <- (danceability)^2

set.seed(100)
nrow(x)
(train <- sample(1:100, size=70))
y_train <- y[train]
y_test <- y[-train] 
x_train <- x[train, ] 
x_test <- x[-train, ]

#OLS 
lm_fit <- lm((danceability)^2 ~ ., data=Spotify, subset=train)

lm_step_fit <- step(lm_fit, trace=F)
ridge_fit <- cv.glmnet(x_train, y_train, alpha=0)
lasso_fit <- cv.glmnet(x_train, y_train, alpha=1)


#RMSE
compute_rmse <- function(y, y_pred) { 
  n <- length(y) 
  sqrt((1 / n) * sum((y - y_pred)^2))
}
#ORD LS
lm_pred <- predict(lm_fit, newdata = Spotify[-train, ]) 
compute_rmse(y_test, lm_pred)

#Step
lm_step_pred <- predict(lm_step_fit, newdata = Spotify[-train, ]) 
compute_rmse(y_test, lm_step_pred)

#Ridge
ridge_pred <- predict(ridge_fit, newx = x_test, s = "lambda.min") 
ridge_pred <- as.numeric(ridge_pred) 
compute_rmse(y_test, ridge_pred)  #Best

#Lasso
lasso_pred <- predict(lasso_fit, newx = x_test, s = "lambda.min") 
lasso_pred <- as.numeric(lasso_pred) 
compute_rmse(y_test, lasso_pred)         #Best (switches)

#Can you make a table with these 4 RMSE and which they model we used. The models:OLD Full, OLS Step, Ridge, and Lasso



#Potential predictors to add: language, # of band members, instruments? Things to try, just taking the outliers out (will probably satisfy normaility). Comparing that model to these.






#removing outliers
p<-12
n<-nrow(Spot1)
outliers<-which(abs(rstandard(Spotty))>2& hatvalues(Spotty)>2*(p+1)/n)
outliers        
Spot2<-Spot1[-c(22,98), ]

pairs(danceability~., data = Spot2) 



Spotty2<-lm(danceability~., data = Spot2)
round(vif(Spotty2), 2) 
shapiro.test(Spotty2$residuals)



#NOT NORMAL!



