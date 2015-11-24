#206
#3.
xh = c(1,0,0,0)
inv = matrix(c(0.087,-0.014,-0.035,-0.004,-0.014,0.115,-0.012,-0.052,-0.035,-0.012,0.057,-0.014,-0.004,-0.052,-0.014,0.050),4,4)

s = sqrt(1.087)*1.02
yh = 0.9918
yh - qt(0.975,26)*s

#4.
property = read.table("~/academic/Sta206/property.txt", header = FALSE)
#colnames(property) = c("age", "operating_expense", "vacancy_rate", "total_sqaure_footage", "rental_rates")
colnames(property) = c("Y", "X1","X2", "X3", "X4")

#(a)
pairs(property)
cor(property)
#From the plot matrix, I find that Y seems to have a linear relationship with X2 and X4, and from the correlation matrix, I find that cor(Y,X3) is very close to 0, and the covariance amony X1~X4 are not very significant.

#(b)
fit = lm(Y ~ X1 + X2 + X3 + X4, data = property)
summary(result)
#beta_hat = [0.122, -0.142, 0.282, 0.282, 0.619, 7.924-e06]
#Y = 0.122 - 0.142X_1 + 0.282X_2 + 0.619X_3 + 7.924-e06X_4
#MSE = 1.137^2 = 1.292769
#R2 = 0.5847
#R2_a = 0.5629

#(c)
par(mfrow = c(2,2))
par(mar = c(5,5,3,3))
plot(fit, which = 1, cex = 0.5)
plot(fit, which = 2, cex = 0.5)
boxplot(fit$residuals, main = "Box Plot of Residuals")
#Frome the Residuals vs. Fitted plot, we find that there is no obvious non-linear pattern, and its close to the line y = 0,
#From the QQ plot, we can find that the error distribution is a little bit heavy tailed
#From the boxplot we can also find that the error distribution is not skewed.

#(d)
par(mfrow = c(2,2))
plot(property$X1, fit$residuals, xlab = "age", ylab = "residuals", main = "residual vs. age", cex = 0.5)
plot(property$X2, fit$residuals, xlab = "operating expense", ylab = "residuals", main = "residual vs. expense", cex = 0.5)
plot(property$X3, fit$residuals, xlab = "vacancy rate", ylab = "residuals", main = "residual vs. vacancy rate", cex = 0.5)
plot(property$X4, fit$residuals, xlab = "total square footage", ylab = "residuals", main = "residual vs. total square footage", cex = 0.5)

choose(4,2)
#There are 6 two-way interaction terms.
par(mfrow = c(3,2))
plot(property$X1*property$X2, fit$residuals, xlab = "X1X2", ylab = "residuals", main = "residual vs. X1X2", cex = 0.5)
plot(property$X1*property$X3, fit$residuals, xlab = "X1X3", ylab = "residuals", main = "residual vs. X1X3", cex = 0.5)
plot(property$X1*property$X4, fit$residuals, xlab = "X1X4", ylab = "residuals", main = "residual vs. X1X4", cex = 0.5)
plot(property$X3*property$X2, fit$residuals, xlab = "X2X3", ylab = "residuals", main = "residual vs. X2X3", cex = 0.5)
plot(property$X4*property$X2, fit$residuals, xlab = "X2X4", ylab = "residuals", main = "residual vs. X2X4", cex = 0.5)
plot(property$X3*property$X4, fit$residuals, xlab = "X3X4", ylab = "residuals", main = "residual vs. X3X4", cex = 0.5)


#For the first-order variables, ther is no obvious pattern in their plots. which means the first-order is likely to be correct.
#For the two-way interactions, same to previous, there is no obvious pattern in the plots, so two-way interactions should not be included in out model.

#(e)
summary(fit)
#Null hypothesis H_0: beta_k = 0
#Alternative hypothesis H_a: beta_k != 0
#test statistics: (beta_hat - beta)/s(beta_hat)
#Null distribution: beta_hat/s(beta_hat) ~ t(n-p), t(76)
#the p_value for beta_0 is < 2e-16, se beta_0 is significant, the p-value for beta_1 is 3.89e-09, so beta_1 is significant, the p-value for beta_2 is 2.75e-05, so beta_2 is significant the p-value for beta_3 is 0.57, so beta_3 is insignificant, the p-value for beta_4 is 1.98e-07, so beta_4 is significant.
#This implies that the rental rates is not related to vacancy rate, but related to the other 3 variables.

#(f) PPPPPPPPPPPPPPPPPPPPPPPPPPPP
anova(fit)
anova_table = anova(fit)
SSTO = sum(anova_table$`Sum Sq`)
df_ssto = sum(anova_table$`Df`)
SSTO
df_ssto 
SSE = anova_table["Residuals",]$`Sum Sq`
df_sse = anova_table["Residuals",]$`Df`
SSR = SSTO - SSE
df_ssr = df_ssto - df_sse

#Null hypothesis H_0: beta_1 = beta_2 = beta_3 = .. = beta_p-1 =0
#Alternative hypothesis H_a: beta_i !=0
#test statistics f_statistics = MSR/MSE = SSR/SSE*(df_sse/df_ssr)
f_statistics = SSR/SSE*(df_sse/df_ssr)
#Null distribution: f_statistics ~ F(df_ssr, df_sse), which is F(4, 76)
#Decision Rule: if f_statistics <= qf(0.99, 4, 76), conclude H_0, otherwise we conclude H_a.
f_statistics <= qf(0.99, 4, 76)
#So we conclude H_a, exist beta_i != 0, so there is a regression realtion at alpha = 0.01

#(g)
#Beacuse the coefficient of X3 is insignificant.
fit2 = lm(Y ~ X1 + X2 + X4, data = property)
summary(fit2)
#The fitted regression function. Y_i = 0.1237 - 0.1442X_i1 + 0.2673X_i2 + 8.178*10^(-6)
#MSE = 1.132^2 = 1.281424
#R2 = 0.583
#R2_a = 0.5667
#These number is very close to our Model 1, because in Model 1, the coefficent of X_3 is small, and its change has little impact to Y, so the MSE, R2, R2_a are about the same.

#(h)
summary(fit)$coefficients[,"Std. Error"][c(1,2,3,5)]
summary(fit2)$coefficients[,"Std. Error"]
#The standard errors of the regression coefficient is more smaller in Model 2.
#For beta_1
#Model1: 
confint(fit, parm = c("X1","X2","X3","X4"),level = 0.95)
confint(fit2, parm = c("X1","X2","X4"),level = 0.95)
#lwr_model1 = (summary(fit)$coefficients[,"Estimate"] - qt(0.975,76)*summary(fit)$coefficients[,"Std. Error"])[c(1,2,3,5)]
#upr_model1 = (summary(fit)$coefficients[,"Estimate"] + qt(0.975,76)*summary(fit)$coefficients[,"Std. Error"])[c(1,2,3,5)]
lwr_model2 = summary(fit2)$coefficients[,"Estimate"] - qt(0.975,77)*summary(fit2)$coefficients[,"Std. Error"]
upr_model2 = summary(fit2)$coefficients[,"Estimate"] + qt(0.975,77)*summary(fit2)$coefficients[,"Std. Error"]
#lwr_model2
#upr_model2
#for beta_1 the 95% confidence interval is [-1.858219e-01, -1.025074e-01]
#for beta_2 the 95% confidence interval is [1.530784e-01, 3.812557e-01]
#for beta_4 the 95% confidence interval is [5.578873e-06, 1.077755e-05]

#The width of intervals for model1 is upr_model1 - lwr_model1
width_model1 = upr_model1 - lwr_model1
#The width of intervals for model2 is upr_model2 - lwr_model2
width_model2 = upr_model2 - lwr_model2
width_model1 > width_model2
#The confidence intervals under Model1 is much wider.

#(i)
newX_model1 = data.frame(X1 = 4, X2 = 10, X3 = 0.1, X4 = 80000)
pred_model1 = predict(fit, newX_model1, interval = 'prediction', level = 0.99)

#So the 99% prediction interval for model 1 is [12.1027, 18.19429]
newX_model2 = data.frame(X1 = 4, X2 = 10, X4 = 80000)
pred_model2 = predict(fit2, newX_model2, interval = 'prediction', level = 0.99)

#So the 99% prediction interval for model 2 is [12.07797, 18.16173]
pred_model2[,"upr"] - pred_model2[,"lwr"] < pred_model1[,"upr"] - pred_model1[,"lwr"]
#I find that the width of prediction interval for model 2 is narrower than model 1.

#(j)
#I prefer Model 2, First, its cost is less than Model 1, it only needs 3 variables. Second, the variation of Model 2's estimated coefficients and prediction is smaller than Model 1's.

myTable = data.frame(c("Regression","Error","Total"), c(SSR, SSE, SSTO), c(df_ssr, df_sse, df_ssto), c(SSR, SSE, SSTO)/c(df_ssr, df_sse, df_ssto))
colnames(myTable) = c("Scource of variation", "SS", "df", "MS")
myTable

