# install.packages("tidyverse")
# install.packages("gganimate")
# install.packages("xlsx")
# install.packages("gifski")
# install.packages("installr")
#
# install.packages("rmarkdown")
#
install.packages("zoo")
library(gganimate)
library(ggplot2)
# library("installr")
# library("gifski")
library(zoo)

primary <- "#FF9999"
secondary <- "#00AFBB"

# 1. Chapter 3 visual relationship between two variables - scatter diagram
# 2. Chapter 4 coefficient of correlation and covariance
# 3. Chapter 16 Linear regression
# 4. Chapter 18 Regression models


co2 <- read.xlsx('C03-01b.xlsx', 1 , header=TRUE)
temperature$Date = ISOdate(temperature$Year, temperature$Month)

p <- ggplot(co2, aes(x = Month, y = CO2)) + geom_point() + transition_time(Year)


temperature <- na.omit(read.xlsx('C03-01a.xlsx', 1 , header=TRUE))
ggplot(temperature, aes(x = Year, y = Land)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  theme_bw() 
ggplot(temperature, aes(x = Year, y = Ocean)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  theme_bw()

### boxplot side by side Land
require(gridExtra)
library(xlsx)
primary <- "#FF9999"
secondary <- "#00AFBB"
temperature <- na.omit(read.xlsx('C03-01a.xlsx', 1 , header=TRUE))
landPlot <- ggplot(temperature, aes(x = Year, y = Land, group = Year)) + 
  geom_boxplot(colour = secondary) +
  ggtitle("Boxplot by year") +
  theme_bw()

landPlotAll <- ggplot(temperature, aes(y = Land)) + 
  geom_boxplot(colour = primary) +
  ggtitle("Total") +
  xlab("Total") +
  theme_bw()
grid.arrange(landPlot, landPlotAll, ncol=2, widths = c(4, 1))



### boxplot side by side Ocean
### some variables defined in previous code snippets
oceanPlot <- ggplot(temperature, aes(x = Year, y = Ocean, group = Year)) + 
  geom_boxplot(colour = secondary) +
  ggtitle("Boxplot by year") +
  ylim(-1.9, 1.9) +
  theme_bw()

oceanPlotAll <- ggplot(temperature, aes(y = Ocean)) + 
  geom_boxplot(colour = primary) +
  ggtitle("Total") +
  xlab("Total") +
  ylim(-1.9, 1.9) +
  theme_bw()
grid.arrange(oceanPlot, oceanPlotAll, ncol=2, widths = c(4, 1))

###
### regression land
library(lubridate)
primary <- "#FF9999"
secondary <- "#00AFBB"
temperature <- na.omit(read.xlsx('C03-01a.xlsx', 1 , header=TRUE))
landReg <- lm(Land~Year, temperature)
oceanReg <- lm(Ocean~Year, temperature)
temperature$LandResidual <- rstandard(landReg)
temperature$OceanResidual <- rstandard(oceanReg)

temperature2 <- temperature[temperature$LandResidual < 3 & temperature$LandResidual > -3,]
landReg2 <- lm(Land~Year, temperature2)
summary(landReg2)

landRegResidual <- ggplot(temperature2, aes(x = Year, y = LandResidual)) + 
  geom_point(colour = primary) +
  ggtitle("Standartized land residuals") +
  geom_hline(yintercept=3, color = secondary) +
  geom_hline(yintercept=-3, color = secondary) +
  theme_bw()
oceanRegResidual <- ggplot(temperature, aes(x = Year, y = OceanResidual)) + 
  geom_point(colour = secondary) +
  ggtitle("Standartized ocean residuals") +
  geom_hline(yintercept=3, color = primary) +
  geom_hline(yintercept=-3, color = primary) +
  theme_bw()
landRegResidualQq <- ggplot(temperature2, aes(sample = LandResidual)) + 
  stat_qq(color = primary) +
  stat_qq_line(colour = secondary) +
  ggtitle("QQ plot of land temp residuals") +
  theme_bw() 
oceanRegResidualQq <- ggplot(temperature, aes(sample = OceanResidual)) + 
  stat_qq( color = secondary) +
  stat_qq_line(colour = primary) +
  ggtitle("QQ plot of ocean temp residuals") +
  theme_bw() 
grid.arrange(landRegResidual, oceanRegResidual, landRegResidualQq, oceanRegResidualQq, ncol=2, nrow=2)

### Temperature predictions

library(lubridate)
primary <- "#FF9999"
secondary <- "#00AFBB"
temperature <- na.omit(read.xlsx('C03-01a.xlsx', 1 , header=TRUE))
landReg <- lm(Land~Year, temperature)
oceanReg <- lm(Ocean~Year, temperature)
Year <- seq(1880, 2050, by=1)
landPrediction <- as.data.frame(predict(landReg, newdata = data.frame(Year=Year), interval = "confidence", level = 0.9))
landPrediction <- cbind(Year, landPrediction)
landPredictionPlot <- ggplot() +
  geom_point(aes(x = temperature$Year, y = temperature$Land), colour = primary, shape=1) +
  geom_line(aes(x = landPrediction$Year, y = landPrediction$upr), colour = secondary, size = 1) +
  geom_line(aes(x = landPrediction$Year, y = landPrediction$lwr), colour = secondary, size = 1) +
  geom_line(aes(x = landPrediction$Year, y = landPrediction$fit), size = 1) +
  ggtitle("Land") +
  xlab("Year") +
  ylab("Temperature") +
  ylim(-1.5, 1.5) +
  theme_bw()
oceanPrediction <- as.data.frame(predict(oceanReg, newdata = data.frame(Year=Year), interval = "confidence", level = 0.9))
oceanPrediction <- cbind(Year, oceanPrediction)
oceanPredictionPlot <- ggplot() +
  geom_point(aes(x = temperature$Year, y = temperature$Ocean), colour = secondary, shape=1) +
  geom_line(aes(x = oceanPrediction$Year, y = oceanPrediction$upr), colour = primary, size = 1) +
  geom_line(aes(x = oceanPrediction$Year, y = oceanPrediction$lwr), colour = primary, size = 1) +
  geom_line(aes(x = oceanPrediction$Year, y = oceanPrediction$fit), size = 1) +
  ggtitle("Ocean") +
  xlab("Year") +
  ylab("Temperature") +
  ylim(-1.5, 1.5) +
  theme_bw()

temperatureAfter <- temperature[temperature$Year>= 1970,]
landRegAfter <- lm(Land~Year, temperatureAfter)
summary(landRegAfter)
oceanRegAfter <- lm(Ocean~Year, temperatureAfter)
summary(oceanRegAfter)
YearAfter <- seq(1970, 2050, by=1)
landPredictionAfter <- as.data.frame(predict(landRegAfter, newdata = data.frame(Year=YearAfter), interval = "confidence", level = 0.9))
landPredictionAfter <- cbind(YearAfter, landPredictionAfter)
landPredictionAfterPlot <- ggplot() +
  geom_point(aes(x = temperatureAfter$Year, y = temperatureAfter$Land), colour = primary, shape=1) +
  geom_line(aes(x = landPredictionAfter$Year, y = landPredictionAfter$upr), colour = secondary, size = 1) +
  geom_line(aes(x = landPredictionAfter$Year, y = landPredictionAfter$lwr), colour = secondary, size = 1) +
  geom_line(aes(x = landPredictionAfter$Year, y = landPredictionAfter$fit), size = 1) +
  ggtitle("Land after 1970") +
  xlab("Year") +
  ylab("Temperature") +
  theme_bw()
oceanPredictionAfter <- as.data.frame(predict(oceanRegAfter, newdata = data.frame(Year=YearAfter), interval = "confidence", level = 0.9))
oceanPredictionAfter <- cbind(YearAfter, oceanPredictionAfter)
oceanPredictionAfterPlot <- ggplot() +
  geom_point(aes(x = temperatureAfter$Year, y = temperatureAfter$Ocean), colour = secondary, shape=1) +
  geom_line(aes(x = oceanPredictionAfter$Year, y = oceanPredictionAfter$upr), colour = primary, size = 1) +
  geom_line(aes(x = oceanPredictionAfter$Year, y = oceanPredictionAfter$lwr), colour = primary, size = 1) +
  geom_line(aes(x = oceanPredictionAfter$Year, y = oceanPredictionAfter$fit), size = 1) +
  ggtitle("Ocean after 1970") +
  xlab("Year") +
  ylab("Temperature") +
  theme_bw()

grid.arrange(landPredictionPlot, oceanPredictionPlot, landPredictionAfterPlot, oceanPredictionAfterPlot, ncol=2, nrow=2)


### CO2
require(gridExtra)
library(ggplot2)
library(xlsx)
library(lubridate)
co2 <- read.xlsx('C03-01b.xlsx', 1 , header=TRUE)
co2Plot <- ggplot(co2, aes(x = Year, y = CO2, group = Year)) + 
  geom_boxplot(colour = secondary) +
  ggtitle("Boxplot by year") +
  theme_bw()
co2PlotAll <- ggplot(co2, aes(y = CO2)) + 
  geom_boxplot(colour = primary) +
  ggtitle("Total") +
  xlab("Total") +
  theme_bw()
grid.arrange(co2Plot, co2PlotAll, ncol=2, widths = c(4, 1))

co2$Date <- ymd(paste0(co2$Year, " ", co2$Month, " ", "15"))

co2Reg <- ggplot(co2, aes(x = Date, y = CO2)) + 
  geom_line(colour=primary) +
  geom_smooth(method = lm, colour=secondary) + 
  ggtitle("CO2") +
  theme_bw() 
co2Reg

### co2 fit
co2 <- read.xlsx('C03-01b.xlsx', 1 , header=TRUE)
co2reg <- lm(CO2~Year, co2)
summary(co2reg)
co2$Date <- ymd(paste0(co2$Year, " ", co2$Month, " ", "15"))
co2$Residual <- rstandard(co2reg)

oceanRegResidual <- ggplot(co2, aes(x = Date, y = Residual)) + 
  geom_point(colour=primary) +
  ggtitle("Standartized CO2 residuals") +
  geom_hline(yintercept=3, color = secondary) +
  geom_hline(yintercept=-3, color = secondary) +
  ylim(-4, 4) +
  theme_bw()
oceanRegResidualQq <- ggplot(co2, aes(sample = Residual)) + 
  stat_qq(colour=secondary) +
  stat_qq_line(colour=primary) +
  ylim(-4, 4) +
  ggtitle("QQ plot of residuals") +
  theme_bw() 
grid.arrange(oceanRegResidual, oceanRegResidualQq, ncol=2, widths = c(1, 1))

primary <- "#FF9999"
secondary <- "#00AFBB"
Year <- seq(1958, 2040, by=1)

co2log <- d(log(CO2)~Year, co2)
co2LogPred <- as.data.frame(predict(co2log, newdata = data.frame(Year=Year), interval = "confidence", level = 0.9))
co2LogPred <- cbind(Year, co2LogPred)
summary(co2log)
co2linear <- lm(CO2~Year, co2)
CO2LinearPred <- as.data.frame(predict(co2linear, newdata = data.frame(Year=Year), interval = "confidence", level = 0.9))
CO2LinearPred <- cbind(Year, CO2LinearPred)
summary(co2linear)
co2PredictionPlot <- ggplot() +
  geom_point(aes(x = co2$Year, y = co2$CO2), shape=1) +
  geom_line(aes(x = CO2LinearPred$Year, y = CO2LinearPred$fit, colour="y = ax + b"), size = 1) +
  geom_line(aes(x = co2LogPred$Year, y = exp(co2LogPred$fit), colour="y = ab^x"), size = 1) +
  ggtitle("Co2 levels, linear vs simple exp fit") +
  xlab("Year") +
  ylab("Co2 levels") +
  theme_bw()
ggsave("co2 linear vs exponential.png")



## CO2 log fit

primary <- "#FF9999"
secondary <- "#00AFBB"
minCo2 <- min(co2$CO2)
coeficients <- c(0, 0.2, 0.4, 0.6, 0.8, 0.99)
regression <- function(coef) {
  c <- minCo2*coef
  regression <- lm(log(CO2 - c)~Year, co2)
  c(coef, summary(regression)$r.squared)
}
lapply(coeficients, regression)
coefC <- min(co2$CO2)*0.8
co2logReg <- lm(log(CO2 - coefC)~Year, co2)
summary(co2logReg)
Year <- seq(1958, 2070, by=1)
co2LogPrediction <- as.data.frame(predict(co2logReg, newdata = data.frame(Year=Year), interval = "confidence", level = 0.9))
co2LogPrediction <- cbind(Year, co2LogPrediction)
co2LogPredictionPlot <- ggplot() +
  geom_point(aes(x = co2$Year, y = co2$CO2), colour = primary, shape=1) +
  geom_line(aes(x = co2LogPrediction$Year, y = exp(co2LogPrediction$upr)+coefC), colour = secondary, size = 1) +
  geom_line(aes(x = co2LogPrediction$Year, y = exp(co2LogPrediction$lwr)+coefC), colour = secondary, size = 1) +
  geom_line(aes(x = co2LogPrediction$Year, y = exp(co2LogPrediction$fit)+coefC), size = 1) +
  ggtitle("CO2 exp fit prediction") +
  xlab("Year") +
  ylab("Co2") +
  theme_bw()
ggsave("co2 exponential fit.png")
co2LogPredictionPlot
exp(co2LogPrediction[Year==2060,])+coefC

co2$Residual <- rstandard(co2logReg)
co2LogResiduals <- ggplot(co2, aes(x = Year, y = Residual)) + 
  geom_point(colour=primary) +
  ggtitle("Standartized CO2 exp fitresiduals") +
  geom_hline(yintercept=3, color = secondary) +
  geom_hline(yintercept=-3, color = secondary) +
  ylim(-4, 4) +
  theme_bw()
co2LogResidualQq <- ggplot(co2, aes(sample = Residual)) + 
  stat_qq(colour=secondary) +
  stat_qq_line(colour=primary) +
  ylim(-4, 4) +
  ggtitle("QQ plot of exp fit residuals") +
  theme_bw() 
grid <- grid.arrange(co2LogResiduals, co2LogResidualQq, ncol=2, widths = c(1, 1))
ggsave("co2 exponential residuals.png", grid)




mocel <- lm(CO2~poly(Year,2), co2)

co2$co2Log <- log(co2$CO2)
mocel <- lm(CO2~poly(Year,2), co2)
ggplot(co2, aes(x = Date, y = co2Log)) + 
  geom_line(colour=primary) +
  geom_smooth(method = mocel, colour=secondary) + 
  ggtitle("co2Log") +
  theme_bw() 
summary(lm(CO2~Year, co2))
summary(lm(co2Log~Year, co2))
summary(lm(CO2~poly(Year,2), co2))


### CO2 vs TEMP
require(gridExtra)
library(ggplot2)
library(xlsx)
library(lubridate)
co2 <- na.omit(read.xlsx('C03-01b.xlsx', 1 , header=TRUE))
co2$Date <- ymd(paste0(co2$Year, " ", co2$Month, " ", "15"))
temperature <- na.omit(read.xlsx('C03-01a.xlsx', 1 , header=TRUE))
temperature$Date <- ymd(paste0(temperature$Year, " ", temperature$Month, " ", "15"))
co2AndTemperature <- merge(co2, temperature, by = "Date")
plot(co2AndTemperature$CO2, co2AndTemperature$Land, col="green")
abline(lm(co2AndTemperature$Land ~ co2AndTemperature$CO2), col="blue")


landPlot <- ggplot(co2AndTemperature, aes(x = CO2, y = Land)) + 
  geom_point(colour=primary) +
  ggtitle("Land temperature vs CO2") +
  theme_bw()
oceanPlot <- ggplot(co2AndTemperature, aes(x = CO2, y = Ocean)) + 
  geom_point(colour=secondary) +
  ggtitle("Ocean temperature vs CO2") +
  theme_bw()
grid <- grid.arrange(landPlot, oceanPlot, ncol=2, widths = c(1, 1))
ggsave("CO2 vs temp.png", grid)


co2ToLandReg <- lm(Land ~ CO2, co2AndTemperature)
summary(co2ToLandReg)
co2ToOceanReg <- lm(Ocean ~ CO2, co2AndTemperature)
summary(co2ToOceanReg)

co2AndTemperature$LandResidual <- rstandard(co2ToLandReg)
co2AndTemperature$OceanResidual <- rstandard(co2ToOceanReg)

## 
oceanTempCo2Residual <- ggplot(co2AndTemperature, aes(x = CO2, y = LandResidual)) + 
  geom_point(colour=primary) +
  ggtitle("Standartized land temp vs CO2  residuals") +
  geom_hline(yintercept=3, color = secondary) +
  geom_hline(yintercept=-3, color = secondary) +
  ylim(-4, 4) +
  theme_bw() +

landTempCo2Residual <- ggplot(co2AndTemperature, aes(x = CO2, y = OceanResidual)) + 
  geom_point(color = secondary) +
  ggtitle("Standartized ocean temp vs CO2 residuals") +
  geom_hline(yintercept=3, color = primary) +
  geom_hline(yintercept=-3, color = primary) +
  ylim(-4, 4) +
  theme_bw()
grid.arrange(oceanTempCo2Residual, landTempCo2Residual, ncol=2, widths = c(1, 1))

ggplot(co2AndTemperature) + 
  stat_qq(aes(sample = LandResidual, color = "Land")) +
  stat_qq(aes(sample = OceanResidual, color = "Ocean")) +
  ggtitle("QQ plot of residuals") +
  theme_bw() 

co2AndTemperatureWoOutliers <- co2AndTemperature[co2AndTemperature$LandResidual < 2.5 & co2AndTemperature$LandResidual > -2.5,]
co2ToLandReg2 <- lm(Land ~ CO2, co2AndTemperatureWoOutliers)
summary(co2ToLandReg2)


### time to CO2 to temperature

primary <- "#FF9999"
secondary <- "#00AFBB"
coefC <- min(co2$CO2)*0.8
co2logReg <- lm(log(CO2 - coefC)~Year, co2)
summary(co2logReg)
Year <- seq(1958, 2050, by=1)
co2LogPrediction <- as.data.frame(predict(co2logReg, newdata = data.frame(Year=Year), interval = "confidence", level = 0.9))

upperCo2 <- exp(co2LogPrediction$upr)+coefC
lowerCo2 <- exp(co2LogPrediction$lwr)+coefC
meanCo2 <- exp(co2LogPrediction$fit)+coefC

co2ToLandReg <- lm(Land ~ CO2, co2AndTemperature)
co2ToOceanReg <- lm(Ocean ~ CO2, co2AndTemperature)

meanLandTempFromCo2 <- as.data.frame(predict(co2ToLandReg, newdata = data.frame(CO2=meanCo2), interval = "confidence", level = 0.9))
lowerLandTempFromCo2 <- as.data.frame(predict(co2ToLandReg, newdata = data.frame(CO2=lowerCo2), interval = "confidence", level = 0.9))
upperLandTempFromCo2 <- as.data.frame(predict(co2ToLandReg, newdata = data.frame(CO2=upperCo2), interval = "confidence", level = 0.9))
meanLandTempFromCo2 <- cbind(Year, meanLandTempFromCo2)
lowerLandTempFromCo2 <- cbind(Year, lowerLandTempFromCo2)
upperLandTempFromCo2 <- cbind(Year, upperLandTempFromCo2)

tempFromCo2Pred <- ggplot() +
  geom_line(aes(x = meanLandTempFromCo2$Year, y = meanLandTempFromCo2$fit), size = 1) +
  geom_line(aes(x = lowerLandTempFromCo2$Year, y = lowerLandTempFromCo2$lwr), colour = primary, size = 1) +
  geom_line(aes(x = upperLandTempFromCo2$Year, y = upperLandTempFromCo2$upr), colour = primary, size = 1) +
  geom_ribbon(aes(x = meanLandTempFromCo2$Year, ymin=upperLandTempFromCo2$upr, ymax=lowerLandTempFromCo2$lwr), alpha = 0.2) +
  ggtitle("Temperature from Co2 prediction") +
  xlab("Year") +
  ylab("Land temperature") +
  theme_bw()
ggsave("temp from co2 predction.png")

