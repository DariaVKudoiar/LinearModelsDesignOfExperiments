setwd('C:/Users/Dasha/Documents/HW_6357')

data = read.table('CH01PR19.txt', header = FALSE, col.names = c('GPA', 'ACT'))
fit <- lm(GPA ~ ACT,data=data)
summary(fit)
plot(data$GPA ~ data$ACT, main = 'Predicting GPA from ACT Score', xlab = 'ACT', ylab = 'GPA', col = 'green')
abline(fit, col = 'blue')
Y = fit$coefficients[[1]]+ fit$coefficients[[2]] * 30 
Y
sum(fit$residuals^2)/fit$df.residual

confint(fit,level=0.99)
