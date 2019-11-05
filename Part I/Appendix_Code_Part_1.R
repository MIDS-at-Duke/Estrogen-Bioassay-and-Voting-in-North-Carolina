Appendix

```
library(ggplot2)
library(lme4)
library(readr)
library(arm)
library(car)
library(e1071)
library(caret)
library(pROC)
library(xtable)
library(kableExtra)
```

```

estrogen <- read.table('bioassay.txt', header=T, sep=" ", colClasses = c('protocol' = 'factor'))

#convert factors to numeric
estrogen$uterus <- as.character(estrogen$uterus)
estrogen$uterus <- as.numeric(estrogen$uterus)
estrogen$weight <- as.character(estrogen$weight)
estrogen$weight <- as.numeric(estrogen$weight)
estrogen$group <- as.factor(estrogen$group)
#remove empty data
estrogen <- na.omit(estrogen)
#need to mean center weight
estrogen$mweight <- estrogen$weight - mean(estrogen$weight)

# added another binary variable to distinguish between mature and inmature rats
estrogen$mature <- 1
estrogen$mature[estrogen$protocol == 'A' | estrogen$protocol == 'B' ] <- 0
estrogen$mature <- as.factor(estrogen$mature)

#summary(estrogen)
```


```
# check the distribution of the response variable
hist(estrogen$uterus)
hist(log(estrogen$uterus))
#Log looks the best compared to the other transformations, use this for analysis
hist(sqrt(estrogen$uterus))
hist((estrogen$uterus)^(1/3))
```

```
# check the distribution of the response variable
hist(estrogen$uterus)
hist(log(estrogen$uterus))
#Log looks the best compared to the other transformations, use this for analysis
hist(sqrt(estrogen$uterus))
hist((estrogen$uterus)^(1/3))
```

```

ggplot(data = estrogen, aes(x=mweight, y=log(uterus))) + geom_point(alpha = .5 , color = 'blue') + geom_smooth(method= 'lm' ,col= 'red3') + facet_wrap(~protocol, ncol = 3)
#the mature rats have a negative pattern, the immature rats do not appear to have a pattern

ggplot(data = estrogen, aes(x=EE, y=log(uterus))) + geom_point(alpha = .5 , color = 'blue') + geom_smooth(method= 'lm' ,col= 'red3') + facet_wrap(~protocol, ncol = 3)
#There appears to be a positive relationship between EE and log uterus weight

ggplot(data = estrogen, aes(x=ZM, y=log(uterus))) + geom_point(alpha = .5 , color = 'blue') + geom_smooth(method= 'lm' ,col= 'red3') + facet_wrap(~protocol, ncol = 3)
#There appears to be a negative relationship between ZM and uterus weight

ggplot(data = estrogen, aes(x=lab, y=log(uterus))) + geom_point(alpha = .5 , color = 'blue') + geom_smooth(method= 'lm' ,col= 'red3') + facet_wrap(~protocol, ncol = 3) + theme(axis.text.x = element_text(angle = 90))
#It appears that labs doing similar experiments have similar outcomes

ggplot(data = estrogen, aes(x=lab, y=log(uterus))) + geom_point(alpha = .5 , color = 'blue') + geom_smooth(method= 'lm' ,col= 'red3') + theme(axis.text.x = element_text(angle = 90))
#See differences in labs, but this could be due to some do not do certain protocols. Need to build the model to check

ggplot(data = estrogen, aes(x=group, y=log(uterus))) + geom_point(alpha = .5 , color = 'blue') + geom_smooth(method= 'lm' ,col= 'red3') + facet_wrap(~protocol, ncol = 3)
#replicate groups are different because each replicate group was given a different dose of EE and ZM

ggplot(data = estrogen, aes(x=group, y=log(uterus))) + geom_point(alpha = .5 , color = 'blue') + geom_smooth(method= 'lm' ,col= 'red3')
#Between all protocols the groups show similar trends of more EE leads to higher uterus weight. Interaction between EE and ZM may be significant

ggplot(data = estrogen, aes(x=protocol, y=log(uterus))) + geom_point(alpha = .5 , color = 'blue') + geom_smooth(method= 'lm' ,col= 'red3')
#Difference in weight, but expected since the age of the rats differed in the protocols

plot(log(uterus)~mweight, data = estrogen)
# very obvious clustering of mature and inmature rats
```


```
partial_estrogen_model1 <- lmer(log(uterus)~ mweight + EE + ZM + (1 | lab) + (1| protocol), data = estrogen)
summary(partial_estrogen_model1)
#ranef(partial_estrogen_model1)
#T values for these are large, and therefore the model is acceptable
car::vif(partial_estrogen_model1)
AIC(partial_estrogen_model1)
BIC(partial_estrogen_model1)

# weight is not very significant 
```

```

# created mature and replace weight to mature and adding level for protocol, qq plot not improving
two_level_estrogen_model <- lmer(log(uterus)~ EE + ZM + mature + (1| protocol) + (1 | lab), data = estrogen)
summary(two_level_estrogen_model)
car::vif(two_level_estrogen_model)
AIC(two_level_estrogen_model)
BIC(two_level_estrogen_model)

# tried to log weight
logx_estrogen_model <- lmer(log(uterus)~ log(mweight) + EE + ZM + (1 | protocol) + (1 | lab), data = estrogen)
summary(logx_estrogen_model)
AIC(logx_estrogen_model)
BIC(logx_estrogen_model)

# then for answering sensitivity of each protocol in detecting EE and ZM levels, we tried to add EE and ZM random slope effect
two_level_estrogen_model1 <- lmer(log(uterus)~ EE + ZM + mature + (1+EE+ZM| protocol) + (1 | lab), data = estrogen)
# converging issue here!!! NEED TO CHANGE
```


```
# because converge issue, add protocal as a normal predictor and added two interaction terms to see the sensitivity based on Protocol A as the baseline. Assuming baseline A has no sensitivity. 
full_estrogen_model <- lmer(log(uterus)~EE + ZM + protocol + (1 | lab) + EE:protocol + ZM:protocol, data = estrogen)


#adding group in because it is an indicator of the biological (maybe) interaction between EE and ZM that cannot be captured by including the interaction term of EE and ZM
summary(full_estrogen_model)
ranef(full_estrogen_model)
AIC(full_estrogen_model)
BIC(full_estrogen_model)
car::vif(full_estrogen_model)
qqnorm(residuals(full_estrogen_model));qqline(residuals(full_estrogen_model))
#Including protocol means weight now has a lower t value, therefore dropping weight for next model
```

```

plot(full_estrogen_model)
#equal variance is violated, they are not scattered around 0
#Linearity is violated, there are downward trends in the second half of the graph

# resid_full <- resid(full_estrogen_model)
# plot(estrogen$mweight, resid_full)
# plot(estrogen$EE, resid_full)
# #We are underfitting for 1mg dose and overfitting for 10 mg dose
# plot(estrogen$ZM, resid_full)
# plot(estrogen$protocol, resid_full)
# plot(estrogen$group, resid_full)

qqnorm(residuals(full_estrogen_model));qqline(residuals(full_estrogen_model))
# not very good 


```