library(carData)
library(MASS)
library(ggplot2)


#Tomado de https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5

data(WVS) 
head(WVS)

summary(WVS)


ggplot(WVS, aes(x = poverty, y = age, fill = poverty)) +   
  geom_boxplot(size = .75) +   facet_grid(country ~ gender, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


model_fit <- polr(poverty~religion+degree+country+age+gender, data = WVS, Hess = TRUE)
summary(model_fit)




summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table


new_data <- data.frame("religion"= "yes","degree"="no","country"="Norway","age"=30,"gender"="male")
round(predict(model_fit,new_data,type = "p"), 3)