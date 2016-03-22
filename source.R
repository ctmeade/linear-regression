# Chris Meade and Cordelia Roberts
# Class Project Appendix (R Code)
# PSTAT 126
# March 8, 2016

# Import libraries for plots
install.packages("ggplot2")
library(ggplot2, GGally)

# Import our data into R
setwd("~/Desktop")
income = read.table('income.txt', header=TRUE)
attach(income)

# Exploratory analysis
summary(income)
head(income)
tail(income)

# Scatterplot matrix
pairs(income)

# We notice from the scatterplot matrix that
# age and yrExp appear highly correlated.
cor(age,yrExp)
# Indeed, the correlation coefficient is approximately
# 0.98. We can drop one of these variables

##plotting numeric data with regression
ggplot(income) + 
  geom_jitter(aes(age,salary), colour="blue") + geom_smooth(aes(age,salary), method=lm, se=FALSE) +
  geom_jitter(aes(yrExp,salary), colour="green") + geom_smooth(aes(yrExp,salary), method=lm, se=FALSE) +
  geom_jitter(aes(iq,salary), colour='red') + geom_smooth(aes(iq,salary), method=lm, se=FALSE) +
  labs(x = "Percentage cover (%)", y = "Salary ($)")

##Examining possible relationships between variables with some scatterplot matrices
pairs(income)
ggpairs(income, colour='Red', alpha=0.4)


# In several of the scatterplots, we notice an outlier:
# the lowest salary is $17,500, far below the second-lowest
# of $55,000. We remove this outlier

newincome<-subset(income, salary != 17500)

detach()
attach(newincome)

# An exploratory plot to check for possible interaction between 
# gender and yrExp
plot(yrExp[gender=='F'],salary[gender=='F'],pch=1,xlim=range(yrExp),ylim=range(salary),col='red',ylab = 'Salary',xlab='Years of Experience' )
points(yrExp[gender=='M'],salary[gender=='M'],pch=2,xlim=range(yrExp),ylim=range(salary), col='blue')
abline(lm(salary[gender=='F']~yrExp[gender=='F']), col='red')
abline(lm(salary[gender=='M']~yrExp[gender=='M']), col = 'blue')
legend('topleft', inset=.05,cex=.75,pch=1:2,lty=c(1,1),col=c('red','blue'),legend=c('Female','Male'))

# A similar plot to look at interactions between degree and yrExp
plot(yrExp[degree=='BA'],salary[degree=='BA'],pch=1,xlim=range(yrExp),ylim=range(salary),col='red',ylab = 'Salary',xlab='Years of Experience' )
points(yrExp[degree=='MA'],salary[degree=='MA'],pch=2,xlim=range(yrExp),ylim=range(salary), col='blue')
points(yrExp[degree=='PhD'],salary[degree=='PhD'],pch=2,xlim=range(yrExp),ylim=range(salary), col='green')
abline(lm(salary[degree=='BA']~yrExp[degree=='BA']), col='red')
abline(lm(salary[degree=='MA']~yrExp[degree=='MA']), col = 'blue')
abline(lm(salary[degree=='PhD']~yrExp[degree=='PhD']), col = 'green')
legend('topleft', inset=.05,cex=.75,pch=1:2,lty=c(1,1),col=c('red','blue','green'),legend=c('BA','MA','PhD'))

# Testing multiple regression models, backwards elimination by partial f test
# Start by fitting model with all predictors except age
fit1 <- lm(salary~yrExp+gender+iq+degree)
summary(fit1)
drop1(fit1, test='F') 

# Conclude drop IQ from AIC, since p-value associated
# with F-test is not significant 

fit2 <- lm(salary~yrExp+gender+degree)
summary(fit2)

# Can we include this model by adding the interaction
# between degree and yrExp?
fit3 <- lm(salary~yrExp+gender+degree+yrExp*degree)
summary(fit3)

# Hypothesis test
anova(fit2,fit3)
# Conclude fit3 is a better model

# Run diagnostics to see if this model fits assumptions
plot(fit3)

# fit3 appears to fit the assumptions of linearity, 
# normality, and constant variance. Since we do not
# know the order in which the data was collected, we
# do not include a graph of residuals vs. index to
# assess independence