set.seed(666)

## Generate random data
df <- data.frame(y = 10*rnorm(1000,2,1.5),
                 x1 = rnorm(1000,1,0.3),
                 x2 = rnorm(1000,1,4))

beta_1 = 0.2
beta_2 = -2
bruit = rnorm(1000)
#attach(df)
df$y = beta_1*df$x1 + beta_2*df$x2 + bruit
## Partial regressions

# Residual of y regressed on x1
y_res <- lm(y ~ x1, df)$residuals

# Residual of x2 regressed on x1
x_res <- lm(x2 ~ x1, df)$residuals

resids <- data.frame(y_res, x_res)

## Compare the beta values for x2
# Multivariate regression:
summary(lm(y~x1+x2, df))

summary(lm(y_res ~ x_res, resids))
