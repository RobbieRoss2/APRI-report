
help(trees)
data(trees)
force(trees)
View(trees)

#i)
cor(trees$Height, trees$Volume)
cor(trees$Height, trees$Volume)
cor(trees$Girth, trees$Volume)
#height correlation = 0.598, girth correlation = 0.967

Model.Height <- lm(Volume ~ Height, data = trees)
Model.Girth <- lm(Volume ~ Girth, data = trees)
summary(Model.Girth)
summary(Model.Height)
summary(Model.Girth)

#iii)
plot(trees$Height, trees$Volume, xlab = "Hieght in ft", ylab = "Volme of Timber in cubic ft")
plot(trees$Girth, trees$Volume, xlab = "Diameter in inches", ylab = "Volme of Timber in cubic ft")

#iii)
plot(trees$Height, trees$Volume, xlab = "Hieght in ft", ylab = "Volme of Timber in cubic ft")
abline(Model.Height, lwd = 3, col = "red", lty = 3)
plot(trees$Girth, trees$Volume, xlab = "Diameter in inches", ylab = "Volme of Timber in cubic ft")
abline(Model.Girth, lwd = 4, col = "orange", lty = 4)
title(main = "Height of Tree vs Volume")
title(main = "Girth of Tree vs Volume")

#v)
Model.Joint <- lm(Volume ~ Girth + Height, data = trees)
sum((trees$Volume - mean(trees$Volume))^2)
sum(residuals(Model.Joint)^2)

#vi)
qf(1-0.05, df1 = 2, df2 = 31-2-1)

#vii)
summary(Model.Joint)
help(trees)
data(trees)
trees3.lm <- lm(Volume~Height+Girth)
trees3.lm <- lm(Volume ~ Height+Girth)
trees3.lm <- lm(Volume ~ Height+Girth)
help(trees)
data(trees)
trees3.lm <- lm(Volume ~ Height + Girth)
trees3.lm <- lm(volume ~ Height + Girth)
help(trees)
data(trees)
force(trees)
trees.lm <- lm(Volume ~ Height + Girth)
trees.lm <- lm(trees$Volume ~ trees$Height + trees$Girth)
trees3.lm <- lm(trees$Volume ~ trees$Height + trees$Girth)
summary(trees3.lm)
trees3.lm <- lm(Volume ~ Height + Girth, data = trees)
summary(trees3.lm)

#ii)
confint.lm(trees3.lm)
confint.lm(trees3.lm, alpha = 0.1)
confint.lm(trees3.lm, level = 0.90)

#iv)
predict(trees3.lm, data.frame(Height = 65, Girth = 9.7), interval = "confidence", level = 0.95)

#v)
predict(trees3.lm, data.frame(Height = 65, Girth = 9.7), interval = "prediction", level = 0.95)
library(spatial, lib.loc = "C:/Program Files/R/R-4.0.2/library")
require(rgdal)
require(sf)
require(ggplot2)
install.packages("rgdal")
install.packages("sf")
require(ggplot2)
install.packages("ggplot2")
# Just regular data - not very exciting
pollutants <- read.csv("Pollutants_IZ.csv")
# shape will appear in environment at a spatial polygons data frame
shape <- readOGR(".", "SG_IntermediateZone_Bdry_2011")
# can only handle one year at a time so extract 20
shape@data <- pollutants
setwd("C:/Users/Robbie/Desktop/University/MM401 - Dissertation/My Data")
CombinedData <- read.csv("CombinedData1.csv")
View(CombinedData)

#no2 models
plot(no2 ~ employment, data = CombinedData)
linmod = lm(no2 ~ employment, data = CombinedData)
abline(linmod)
abline(linmod, col = "red")

plot(no2 ~ income, data = CombinedData)
linmod = lm(no2 ~ income, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ crime, data = CombinedData)
linmod = lm(no2 ~ crime, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ housing, data = CombinedData)
linmod = lm(no2 ~ housing, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ housing, data = CombinedData)
linmod = lm(no2 ~ housing, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ education, data = CombinedData)
linmod = lm(no2 ~ education, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ access, data = CombinedData)
linmod = lm(no2 ~ access, data = CombinedData)
abline(linmod, col = "red")
par(mfrow=c(2,4))

plot(no2 ~ employment, data = CombinedData)
linmod = lm(no2 ~ employment, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ income, data = CombinedData)
linmod = lm(no2 ~ income, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ crime, data = CombinedData)
linmod = lm(no2 ~ crime, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ housing, data = CombinedData)
linmod = lm(no2 ~ housing, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ education, data = CombinedData)
linmod = lm(no2 ~ education, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ employment, data = CombinedData)
linmod = lm(no2 ~ employment, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ access, data = CombinedData)
linmod = lm(no2 ~ access, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ employment, data = CombinedData)
linmod = lm(no2 ~ employment, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ employment, data = CombinedData)
linmod = lm(no2 ~ employment, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ income, data = CombinedData)
linmod = lm(no2 ~ income, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ crime, data = CombinedData)
linmod = lm(no2 ~ crime, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ housing, data = CombinedData)
linmod = lm(no2 ~ housing, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ education, data = CombinedData)
linmod = lm(no2 ~ education, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ employment, data = CombinedData)
linmod = lm(no2 ~ employment, data = CombinedData)
abline(linmod, col = "red")

plot(no2 ~ access, data = CombinedData)
linmod = lm(no2 ~ access, data = CombinedData)
abline(linmod, col = "red")

lm.no2.employment <- lm(no2 ~ employment, data = CombinedData)
lm.no2.employment

lm.no2.income <- lm(no2 ~ income, data = CombinedData)
lm.no2.income

lm.no2.crime <- lm(no2 ~ crime, data = CombinedData)
abline(lm.now.crime, col = "red")
abline(lm.no2.crime, col = "red")
lm.no2.crime

lm.no2.housing <- lm(no2 ~ housing, data = CombinedData)
abline(lm.no2.housing, col = "red")
lm.no2.housing

lm.no2.education <- lm(no2 ~ education, data = CombinedData)
lm.no2.education

lm.no2.access <- lm(no2 ~ access, data = CombinedData)
lm.no2.access

plot(employment ~ no2, data = CombinedData)
lm.no2.employment <- lm(employment ~ no2, data = CombinedData)
abline(lm.no2.employment, col = "red")
lm.no2.employment

plot(income ~ no2, data = CombinedData)
lm.no2.income <- lm(income ~ no2, data = CombinedData)
abline(lm.no2.income, col = "red")
lm.no2.income

plot(crime ~ no2, data = CombinedData)
lm.no2.crime <- lm(crime ~ no2, data = CombinedData)
abline(lm.no2.crime, col = "red")
lm.no2.crime

summary(lm.no2.crime)
summary(lm.no2.income)
summary(lm.no2.employment)

plot(housing ~ no2, data = CombinedData)
lm.no2.housing <- lm(housing ~ no2, data = CombinedData)
abline(lm.no2.housing, col = "red")
summary(lm.no2.housing)

plot(education ~ no2, data = CombinedData)
lm.no2.education <- lm(education ~ no2, data = CombinedData)
abline(linmod, col = "red")
summary(lm.no2.education)

plot(access ~ access, data = CombinedData)

plot(access ~ no2, data = CombinedData)
lm.no2.access <- lm(access ~ no2, data = CombinedData)
abline(linmod, col = "red")
summary(lm.no2.access)
par(mfrow=c(2,3))

plot(employment ~ no2, data = CombinedData)
lm.no2.employment <- lm(employment ~ no2, data = CombinedData)
abline(lm.no2.employment, col = "red")
summary(lm.no2.employment)

plot(income ~ no2, data = CombinedData)
lm.no2.income <- lm(income ~ no2, data = CombinedData)
abline(lm.no2.income, col = "red")
summary(lm.no2.income)

plot(crime ~ no2, data = CombinedData)
lm.no2.crime <- lm(crime ~ no2, data = CombinedData)
abline(lm.no2.crime, col = "red")
summary(lm.no2.crime)

plot(housing ~ no2, data = CombinedData)
lm.no2.housing <- lm(housing ~ no2, data = CombinedData)
abline(lm.no2.housing, col = "red")
summary(lm.no2.housing)

plot(education ~ no2, data = CombinedData)
lm.no2.education <- lm(education ~ no2, data = CombinedData)
abline(linmod, col = "red")
summary(lm.no2.education)

plot(access ~ no2, data = CombinedData)
lm.no2.access <- lm(access ~ no2, data = CombinedData)
abline(linmod, col = "red")
summary(lm.no2.access)

plot(employment ~ no2, data = CombinedData)
lm.no2.employment <- lm(employment ~ no2, data = CombinedData)
abline(lm.no2.employment, col = "red")
summary(lm.no2.employment)

plot(income ~ no2, data = CombinedData)
lm.no2.income <- lm(income ~ no2, data = CombinedData)
abline(lm.no2.income, col = "red")
summary(lm.no2.income)

plot(crime ~ no2, data = CombinedData)
lm.no2.crime <- lm(crime ~ no2, data = CombinedData)
abline(lm.no2.crime, col = "red")
summary(lm.no2.crime)

plot(housing ~ no2, data = CombinedData)
lm.no2.housing <- lm(housing ~ no2, data = CombinedData)
abline(lm.no2.housing, col = "red")
summary(lm.no2.housing)

plot(education ~ no2, data = CombinedData)
lm.no2.education <- lm(education ~ no2, data = CombinedData)
abline(linmod, col = "red")
summary(lm.no2.education)
abline(lm.no2.education, col = "red")

plot(access ~ no2, data = CombinedData)
lm.no2.access <- lm(access ~ no2, data = CombinedData)
abline(lm.no2.access, col = "red")

summary(lm.no2.access)
summary(lm.no2.employment)
summary(lm.no2.income)
summary(lm.no2.crime)
summary(lm.no2.housing)
summary(lm.no2.education)
summary(lm.no2.access)

plot(employment ~ nox, data = CombinedData)
lm.nox.employment <- lm(employment ~ nox, data = CombinedData)
abline(lm.nox.employment, col = "red")
summary(lm.nox.employment)

plot(income ~ nox, data = CombinedData)
lm.nox.income <- lm(income ~ nox, data = CombinedData)
abline(lm.nox.income, col = "red")
summary(lm.nox.income)

plot(crime ~ nox, data = CombinedData)
lm.nox.crime <- lm(crime ~ nox, data = CombinedData)
abline(lm.nox.crime, col = "red")
summary(lm.nox.crime)

plot(housing ~ nox, data = CombinedData)
lm.nox.housing <- lm(housing ~ nox, data = CombinedData)
abline(lm.nox.housing, col = "red")
summary(lm.nox.housing)

plot(education ~ nox, data = CombinedData)
lm.nox.education <- lm(education ~ nox, data = CombinedData)
abline(lm.nox.education, col = "red")
summary(lm.nox.education)

plot(access ~ nox, data = CombinedData)
lm.nox.access <- lm(access ~ nox, data = CombinedData)
abline(lm.nox.access, col = "red")
summary(lm.nox.access)

plot(employment ~ pm10, data = CombinedData)
lm.pm10.employment <- lm(employment ~ pm10, data = CombinedData)
abline(lm.pm10.employment, col = "red")
summary(lm.pm10.employment)

plot(income ~ pm10, data = CombinedData)
lm.pm10.income <- lm(income ~ pm10, data = CombinedData)
abline(lm.pm10.income, col = "red")
summary(lm.pm10.income)

plot(crime ~ pm10, data = CombinedData)
lm.pm10.crime <- lm(crime ~ pm10, data = CombinedData)
abline(lm.pm10.crime, col = "red")
summary(lm.pm10.crime)

plot(housing ~ pm10, data = CombinedData)
lm.pm10.housing <- lm(housing ~ pm10, data = CombinedData)
abline(lm.pm10.housing, col = "red")
summary(lm.pm10.housing)

plot(education ~ pm10, data = CombinedData)
lm.pm10.education <- lm(education ~ pm10, data = CombinedData)
abline(lm.pm10.education, col = "red")
summary(lm.pm10.education)

plot(access ~ pm10, data = CombinedData)
lm.pm10.access <- lm(access ~ pm10, data = CombinedData)
abline(lm.pm10.access, col = "red")
summary(lm.pm10.access)

plot(employment ~ pm25, data = CombinedData)
lm.pm25.employment <- lm(employment ~ pm25, data = CombinedData)
abline(lm.pm25.employment, col = "red")
summary(lm.pm25.employment)

plot(income ~ pm25, data = CombinedData)
lm.pm25.income <- lm(income ~ pm25, data = CombinedData)
abline(lm.pm25.income, col = "red")
summary(lm.pm25.income)

plot(crime ~ pm25, data = CombinedData)
lm.pm25.crime <- lm(crime ~ pm25, data = CombinedData)
abline(lm.pm25.crime, col = "red")
summary(lm.pm25.crime)

plot(housing ~ pm25, data = CombinedData)
lm.pm25.housing <- lm(housing ~ pm25, data = CombinedData)
abline(lm.pm25.housing, col = "red")
summary(lm.pm25.housing)

plot(education ~ pm25, data = CombinedData)
lm.pm25.education <- lm(education ~ pm25, data = CombinedData)
abline(lm.pm25.education, col = "red")
summary(lm.pm25.education)

plot(access ~ pm25, data = CombinedData)
lm.pm25.access <- lm(access ~ pm25, data = CombinedData)
abline(lm.pm25.access, col = "red")

summary(lm.pm25.access)
summary(lm.no2.employment)
summary(lm.no2.income)
summary(lm.no2.crime)
summary(lm.no2.housing)
summary(lm.no2.education)
summary(lm.no2.access)
summary(lm.nox.employment)
summary(lm.nox.income)
summary(lm.nox.crime)
summary(lm.nox.housing)
summary(lm.nox.education)
summary(lm.nox.access)
summary(lm.pm10.employment)
summary(lm.pm10.income)
summary(lm.pm10.crime)
summary(lm.pm10.housing)
summary(lm.pm10.education)
summary(lm.pm10.access)
summary(lm.pm25.employment)
summary(lm.pm25.income)
summary(lm.pm25.crime)
summary(lm.pm25.housing)
summary(lm.pm25.education)

plot(no2 ~ employment, data = CombinedData)
lm.no2.employment <- lm(no2 ~ employment, data = CombinedData)
abline(lm.no2.employment, col = "red")
summary(lm.no2.employment)
par(mfrow=c(2,3))

plot(no2 ~ employment, data = CombinedData)
lm.no2.employment <- lm(no2 ~ employment, data = CombinedData)
abline(lm.no2.employment, col = "red")
summary(lm.no2.employment)

plot(no2 ~ income, data = CombinedData)
lm.no2.income <- lm(no2 ~ income, data = CombinedData)
abline(lm.no2.income, col = "red")
summary(lm.no2.income)

plot(no2 ~ crime, data = CombinedData)
lm.no2.crime <- lm(no2 ~ crime, data = CombinedData)
abline(lm.no2.crime, col = "red")
summary(lm.no2.crime)

plot(no2 ~ housing, data = CombinedData)
lm.no2.housing <- lm(no2 ~ housing, data = CombinedData)
abline(lm.no2.housing, col = "red")
summary(lm.no2.housing)

plot(no2 ~ education, data = CombinedData)
lm.no2.education <- lm(no2 ~ education, data = CombinedData)
abline(lm.no2.education, col = "red")
summary(lm.no2.education)

plot(no2 ~ access, data = CombinedData)
lm.no2.access <- lm(no2 ~ access, data = CombinedData)
abline(lm.no2.access, col = "red")
summary(lm.no2.access)

plot(pm10 ~ employment, data = CombinedData)
lm.pm10.employment <- lm(pm10 ~ employment, data = CombinedData)
abline(lm.pm10.employment, col = "red")
summary(lm.pm10.employment)

plot(pm10 ~ income, data = CombinedData)
lm.pm10.income <- lm(pm10 ~ income, data = CombinedData)
abline(lm.pm10.income, col = "red")
summary(lm.pm10.income)

plot(pm10 ~ crime, data = CombinedData)
lm.pm10.crime <- lm(pm10 ~ crime, data = CombinedData)
abline(lm.pm10.crime, col = "red")
summary(lm.pm10.crime)

plot(pm10 ~ housing, data = CombinedData)
lm.pm10.housing <- lm(pm10 ~ housing, data = CombinedData)
abline(lm.pm10.housing, col = "red")
summary(lm.pm10.housing)

plot(pm10 ~ education, data = CombinedData)
lm.pm10.education <- lm(pm10 ~ education, data = CombinedData)
abline(lm.pm10.education, col = "red")
summary(lm.pm10.education)

plot(pm10 ~ access, data = CombinedData)
lm.pm10.access <- lm(pm10 ~ access, data = CombinedData)
abline(lm.pm10.access, col = "red")
summary(lm.pm10.access)

setwd("C:/Users/Robbie/Desktop/University/MM401 - Dissertation/My Data")
CombinedData <- read.csv("CombinedData1.csv")

plot(nox ~ employment, data = CombinedData)
lm.nox.employment <- lm(nox ~ employment, data = CombinedData)
#nox models - crime
par(mfrow=c(2,3))

plot(nox ~ employment, data = CombinedData)
lm.nox.employment <- lm(nox ~ employment, data = CombinedData)
abline(lm.nox.employment, col = "red")
summary(lm.nox.employment)

plot(nox ~ income, data = CombinedData)
lm.nox.income <- lm(nox ~ income, data = CombinedData)
abline(lm.nox.income, col = "red")
summary(lm.nox.income)

plot(nox ~ crime, data = CombinedData)
lm.nox.crime <- lm(nox ~ crime, data = CombinedData)
abline(lm.nox.crime, col = "red")
summary(lm.nox.crime)

plot(nox ~ housing, data = CombinedData)
lm.nox.housing <- lm(nox ~ housing, data = CombinedData)
abline(lm.nox.housing, col = "red")
summary(lm.nox.housing)

plot(nox ~ education, data = CombinedData)
lm.nox.education <- lm(nox ~ education, data = CombinedData)
abline(lm.nox.education, col = "red")
summary(lm.nox.education)

plot(nox ~ access, data = CombinedData)
lm.nox.access <- lm(nox ~ access, data = CombinedData)
abline(lm.nox.access, col = "red")
summary(lm.nox.access)

#pm10 models - housing 6.98, education 0.27, employment 0.02, income 0.018, crime 0.001 access -0.04
par(mfrow=c(2,3))

plot(pm25 ~ employment, data = CombinedData)
lm.pm25.employment <- lm(pm25 ~ employment, data = CombinedData)
abline(lm.pm25.employment, col = "red")
summary(lm.pm25.employment)

plot(pm25 ~ income, data = CombinedData)
lm.pm25.income <- lm(pm25 ~ income, data = CombinedData)
abline(lm.pm25.income, col = "red")
summary(lm.pm25.income)

plot(pm25 ~ crime, data = CombinedData)
lm.pm25.crime <- lm(pm25 ~ crime, data = CombinedData)
abline(lm.pm25.crime, col = "red")
summary(lm.pm25.crime)

plot(pm25 ~ housing, data = CombinedData)
lm.pm25.housing <- lm(pm25 ~ housing, data = CombinedData)
abline(lm.pm25.housing, col = "red")
summary(lm.pm25.housing)

plot(pm25 ~ education, data = CombinedData)
lm.pm25.education <- lm(pm25 ~ education, data = CombinedData)
abline(lm.pm25.education, col = "red")
summary(lm.pm25.education)

plot(pm25 ~ access, data = CombinedData)
lm.pm25.access <- lm(pm25 ~ access, data = CombinedData)
abline(lm.pm25.access, col = "red")
summary(lm.pm25.access)

setwd("C:/Users/Robbie/Desktop/University/MM401 - Dissertation/My Data")
CombinedData <- read.csv("CombinedData1.csv")

GLM.pol <- glm(log(respiratory.prim) ~ offset(log(exp.respiratory.prim)) + housing + education + income + employment, family="poisson")
GLM.pol <- glm(log(Respiratory.Prim) ~ offset(log(Exp.Respiratory.Prim)) + housing + education + income + employment, family="poisson")
View(CombinedData)
GLM.pol <- GLM(log(Respiratory.Prim) ~ offset(log(Exp.Respiratory.Prim)) + housing + education + income + employment, family="poisson")
GLM.pol <- glm(log(Respiratory.Prim) ~ offset(log(Exp.Respiratory.Prim)) + housing + education + income + employment, family="poisson")
GLM.pol <- glm(log("Respiratory.Prim") ~ offset(log(Exp.Respiratory.Prim)) + housing + education + income + employment, family="poisson")
GLM.pol <- glm(log(Respiratory.Prim) ~ offset(log(Exp.Respiratory.Prim)) + housing + education + income + employment, family="poisson")
Respiratory.Prim

CombinedData <- read.csv("CombinedData1.csv")
head(CombinedData)
GLM.pol <- glm(log(Respiratory.Prim) ~ offset(log(Exp.Respiratory.Prim)) + housing + education + income + employment, family="poisson")
GLM.pol <- glm(log(Respiratory.Prim) ~ offset(log(Exp.Respiratory.Prim)) + housing + education + income + employment, family="poisson", data = CombinedData)
GLM.pol
housing.rate = log(0.3642944)
housing.rate = exp(0.3642944)
education.rate = exp(0.1034555
                     education.rate = exp(0.1034555)
                     housing.rate = exp(0.3642944)
                     education.rate = exp(0.1034555)
                     income.rate = exp(0.0001685)
                     employment.rate = exp(0.0010391)
                     