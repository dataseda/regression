install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("haven")


library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(haven)

# sample data
y <- rnorm(50, mean = 102, sd = 10)  
x1 <- rnorm(50, mean = 528, sd = 50)  
x2 <- rnorm(50, mean = 32, sd = 2)  


# dataframe
reg.data <- data.frame(y, x1, x2)

# checking
print(reg.data)

summary(reg.data)


summary(linear_model)


#normality

hist(reg.data$y)
hist(reg.data$x1)
hist(reg.data$x2)
hist(linear_model$residuals)

plot(linear_model)
install.packages('lmtest')

library(lmtest)
bptest(linear_model)

cor.test(reg.data$x1, reg.data$x2)


#linearity

plot(y ~ x1, data = reg.data)

plot(y ~ x2, data = reg.data)


linear_model_x1 <- lm(y ~ x1, data = reg.data)
summary(linear_model_x1 )

linear_model_x2 <- lm(y ~ x2, data = reg.data)

summary(linear_model_x2 )
library(car)
vif(linear_model)

model_1 <- lm(y ~ x1, data = reg.data)
model_2 <- lm(y ~ x1 + x2,  data = reg.data)
summary(model_1)
summary(model_2)

anova(model_1,model_2)

library(lme4)

lmer()


