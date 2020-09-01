setwd('C:/Users/Dasha/Documents/HW_6357')
data = read.table('CH01PR28.txt', header = FALSE, col.names = c('CRIMERATE','HIGHSCHOOL'))

fit <- lm(CRIMERATE~HIGHSCHOOL,data=data) #calculate the estimated regression function
summary(fit)

# Ploting the estimated regression function and the data 
plot(data$CRIMERATE ~ data$HIGHSCHOOL, main = 'Estimated Regression vs Data', xlab = '% of HIGHSCHOOL', ylab = 'CRIMERATE', col = 'purple')
abline(fit, col = 'orange')

#2
Y=fit$coefficients[[1]] + fit$coefficients[[2]] * 80 #the mean crime rate last year in counties with high school graduation percentage
Y
#3
resid(fit)[10] #residual for the 10th observation

#4
sum(fit$residuals^2)/fit$df.residual # mean squared error for the model




#sum(fit$residuals^2)/length(fit$residuals)
#var(resid(fit)) what is this

