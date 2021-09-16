## RECITATION 1

# upload data (from FRED)
mydata <- read.csv('/Users/UQAM/Dropbox/Econ103Private/Recitations Material/Recitation Handouts/Recitation 2/macro_data.csv')
# data summary
summary(mydata)
attach(mydata)

# Distributions of different macro time series
par(mfrow=c(2,3))
hist(Inflation)
hist(GDP_g)
hist(Cons)
hist(Fed_rate)
hist(Un_rate)

par(mfrow=c(1,1))

## Old Phillips curve example

# statistics
mean(Inflation)
mean(Un_rate)
cor(Inflation,Un_rate)

# Scatterplot of inflation and unemployment
# Whole sample
results <- lm(Inflation ~ Un_rate)
predict(results,newdata=data.frame(Un_rate=10))

plot(Un_rate, Inflation, ylab = 'Inflation', 
     xlab = 'Unemployment rate', main = 'Whole sample')
abline(results$coef)

# create subsamples
mydata.before70 = mydata[1:40,]
mydata.after2000 = mydata[161:230,]

# After 2000
resultsa2000 <- lm(mydata.after2000$Inflation ~ mydata.after2000$Un_rate, data=mydata.after2000)
plot(mydata.after2000$Un_rate,mydata.after2000$Inflation, ylab = 'Inflation', 
     xlab = 'Unemployment rate', main = 'After 2000')
abline(resultsa2000$coef)
cor(mydata.after2000$Inflation,mydata.after2000$Un_rate)

# Before 1970
resultsb70 <- lm(mydata.before70$Inflation ~ mydata.before70$Un_rate, data=mydata.before70)
plot(mydata.before70$Un_rate,mydata.before70$Inflation, ylab = 'Inflation', 
     xlab = 'Unemployment rate', main = 'Before 1970')
abline(resultsb70$coef)
cor(mydata.before70$Inflation,mydata.before70$Un_rate)



#prediction vs causality, with regression example
# many variables, ommited variables
# reliability of t-stats 
#linearité vs non-linéarité
summary(lm(Inflation~GDP_g+Un_rate+Fed_rate+Cons))
## Add-on for last R




