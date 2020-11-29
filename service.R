primary = "#FF9999"
secondary = "#00AFBB"

withAnnotations <- function(plot, title) {
  plot %>% add_annotations(
    text = title,
    x = 0,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "left",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
}

landTeperatureBoxPlot <- function(temp) {
  landPlot <- ggplotly(ggplot(temp, aes(x = Year, y = Land, group = Year)) +
    geom_boxplot(colour = secondary) +
    theme_bw()
  )
  landPlotAll <- ggplotly(ggplot(temp, aes(y = Land)) +
    geom_boxplot(colour = primary) +
    theme_bw()
  )
  subplot(list(withAnnotations(landPlotAll, "Total"), withAnnotations(landPlot,"Boxplot by year")), widths = c(0.2, 0.8))
}

oceanTeperatureBoxPlot <- function(temp) {
  oceanPlot <- ggplotly(ggplot(temp, aes(x = Year, y = Ocean, group = Year)) +
                         geom_boxplot(colour = secondary) +
                         theme_bw()
  )
  oceanPlotAll <- ggplotly(ggplot(temp, aes(y = Ocean)) +
                            geom_boxplot(colour = primary) +
                            theme_bw()
  )
  subplot(list(withAnnotations(oceanPlotAll, "Total"), withAnnotations(oceanPlot,"Boxplot by year")), widths = c(0.2, 0.8))
}

temperatureRegression <- function(temp) {
  landReg <- lm(Land~Year, temp)
  oceanReg <- lm(Ocean~Year, temp)
  temp$LandResidual <- rstandard(landReg)
  temp$OceanResidual <- rstandard(oceanReg)
  landRegResidual <- ggplotly(ggplot(temp, aes(x = Year, y = LandResidual)) +
    geom_point(colour = primary) +
    geom_hline(yintercept=3, color = secondary) +
    geom_hline(yintercept=-3, color = secondary) +
    theme_bw())
  oceanRegResidual <- ggplotly(ggplot(temp, aes(x = Year, y = OceanResidual)) +
    geom_point(colour = secondary) +
    geom_hline(yintercept=3, color = primary) +
    geom_hline(yintercept=-3, color = primary) +
    theme_bw())
  landRegResidualQq <- ggplotly(ggplot(temp, aes(sample = LandResidual)) +
    stat_qq(color = primary) +
    stat_qq_line(colour = secondary) +
    theme_bw())
  oceanRegResidualQq <- ggplotly(ggplot(temp, aes(sample = OceanResidual)) +
    stat_qq(color = secondary) +
    stat_qq_line(colour = primary) +
    theme_bw())
  subplot(list(withAnnotations(landRegResidual,"Standartized land residuals, R-squared=0.37"),
               withAnnotations(oceanRegResidual,"Standartized ocean residuals, R-squared=0.66"),
               withAnnotations(landRegResidualQq,"QQ plot of land temp residuals"),
               withAnnotations(oceanRegResidualQq,"QQ plot of ocean temp residuals")
          ), nrows=2, margin = 0.07
  )
}

temperaturePredictions <- function(temperature) {
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
    theme_bw()
  oceanPrediction <- as.data.frame(predict(oceanReg, newdata = data.frame(Year=Year), interval = "confidence", level = 0.9))
  oceanPrediction <- cbind(Year, oceanPrediction)
  oceanPredictionPlot <- ggplot() +
    geom_point(aes(x = temperature$Year, y = temperature$Ocean), colour = secondary, shape=1) +
    geom_line(aes(x = oceanPrediction$Year, y = oceanPrediction$upr), colour = primary, size = 1) +
    geom_line(aes(x = oceanPrediction$Year, y = oceanPrediction$lwr), colour = primary, size = 1) +
    geom_line(aes(x = oceanPrediction$Year, y = oceanPrediction$fit), size = 1) +
    theme_bw()
  # temperatureAfter <- temperature[temperature$Year>= 1970,]
  # landRegAfter <- lm(Land~Year, temperatureAfter)
  # oceanRegAfter <- lm(Ocean~Year, temperatureAfter)
  # YearAfter <- seq(1970, 2050, by=1)
  # landPredictionAfter <- as.data.frame(predict(landRegAfter, newdata = data.frame(Year=YearAfter), interval = "confidence", level = 0.9))
  # landPredictionAfter <- cbind(YearAfter, landPredictionAfter)
  # landPredictionAfterPlot <- ggplot() +
  #   geom_point(aes(x = temperatureAfter$Year, y = temperatureAfter$Land), colour = primary, shape=1) +
  #   geom_line(aes(x = landPredictionAfter$Year, y = landPredictionAfter$upr), colour = secondary, size = 1) +
  #   geom_line(aes(x = landPredictionAfter$Year, y = landPredictionAfter$lwr), colour = secondary, size = 1) +
  #   geom_line(aes(x = landPredictionAfter$Year, y = landPredictionAfter$fit), size = 1) +
  #   theme_bw()
  # oceanPredictionAfter <- as.data.frame(predict(oceanRegAfter, newdata = data.frame(Year=YearAfter), interval = "confidence", level = 0.9))
  # oceanPredictionAfter <- cbind(YearAfter, oceanPredictionAfter)
  # oceanPredictionAfterPlot <- ggplot() +
  #   geom_point(aes(x = temperatureAfter$Year, y = temperatureAfter$Ocean), colour = secondary, shape=1) +
  #   geom_line(aes(x = oceanPredictionAfter$Year, y = oceanPredictionAfter$upr), colour = primary, size = 1) +
  #   geom_line(aes(x = oceanPredictionAfter$Year, y = oceanPredictionAfter$lwr), colour = primary, size = 1) +
  #   geom_line(aes(x = oceanPredictionAfter$Year, y = oceanPredictionAfter$fit), size = 1) +
  #   theme_bw()
  subplot(list(withAnnotations(ggplotly(landPredictionPlot),"Land"),
               withAnnotations(ggplotly(oceanPredictionPlot),"Ocean")
               # withAnnotations(ggplotly(landPredictionAfterPlot),"Land after 1970"),
               # withAnnotations(ggplotly(oceanPredictionAfterPlot),"Ocean after 1970")
  ), margin = 0.07
  )
}

co2Boxplot <- function(co2) {
  co2Plot <- ggplotly(ggplot(co2, aes(x = Year, y = CO2, group = Year)) +
    geom_boxplot(colour = secondary) +
    theme_bw())
  co2PlotAll <- ggplotly(ggplot(co2, aes(y = CO2)) +
    geom_boxplot(colour = primary) +
    theme_bw())
  subplot(list(withAnnotations(co2PlotAll, "Total"), withAnnotations(co2Plot,"Boxplot by year")), widths = c(0.2, 0.8))
}

co2Regression <- function(co2) {
  co2reg <- lm(CO2~Year, co2)
  co2$Date <- ymd(paste0(co2$Year, " ", co2$Month, " ", "15"))
  co2$Residual <- rstandard(co2reg)
  oceanRegResidual <- ggplotly(ggplot(co2, aes(x = Date, y = Residual)) +
    geom_point(colour=primary) +
    geom_hline(yintercept=3, color = secondary) +
    geom_hline(yintercept=-3, color = secondary) +
    ylim(-4, 4) +
    theme_bw())
  oceanRegResidualQq <- ggplotly(ggplot(co2, aes(sample = Residual)) +
    stat_qq(colour=secondary) +
    stat_qq_line(colour=primary) +
    ylim(-4, 4) +
    theme_bw())
  subplot(list(withAnnotations(oceanRegResidual, "Residuals, R-squared=0.98"), withAnnotations(oceanRegResidualQq,"Boxplot by year")))
}

co2Predictions <- function(co2) {
  Year <- seq(1958, 2040, by=1)
  co2log <- lm(log(CO2)~Year, co2)
  co2LogPred <- as.data.frame(predict(co2log, newdata = data.frame(Year=Year), interval = "confidence", level = 0.9))
  co2LogPred <- cbind(Year, co2LogPred)
  co2linear <- lm(CO2~Year, co2)
  CO2LinearPred <- as.data.frame(predict(co2linear, newdata = data.frame(Year=Year), interval = "confidence", level = 0.9))
  CO2LinearPred <- cbind(Year, CO2LinearPred)
  ggplot() +
    geom_point(aes(x = co2$Year, y = co2$CO2), shape=1) +
    geom_line(aes(x = CO2LinearPred$Year, y = CO2LinearPred$fit, colour="y = ax + b"), size = 1) +
    # geom_line(aes(x = co2LogPred$Year, y = exp(co2LogPred$fit), colour="y = ab^x"), size = 1) +
    ggtitle("Co2 levels, linear vs simple exp fit") +
    xlab("Year") +
    ylab("Co2 levels") +
    theme_bw()
}

co2LogRegression <- function(co2) {
  coefC <- min(co2$CO2)*0.8
  co2logReg <- lm(log(CO2 - coefC)~Year, co2)
  co2$Residual <- rstandard(co2logReg)
  co2LogResiduals <- ggplotly(ggplot(co2, aes(x = Year, y = Residual)) +
    geom_point(colour=primary) +
    geom_hline(yintercept=3, color = secondary) +
    geom_hline(yintercept=-3, color = secondary) +
    ylim(-4, 4) +
    theme_bw())
  co2LogResidualQq <- ggplotly(ggplot(co2, aes(sample = Residual)) +
    stat_qq(colour=secondary) +
    stat_qq_line(colour=primary) +
    ylim(-4, 4) +
    theme_bw())
  subplot(list(withAnnotations(co2LogResiduals, "Exp fit residuals, R-squared=0.99"), withAnnotations(co2LogResidualQq,"QQ plot of exp fit residuals")))
}

co2LogPredictions <- function(co2) {
  coefC <- min(co2$CO2)*0.8
  co2logReg <- lm(log(CO2 - coefC)~Year, co2)
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
}

tempToCo2Regression <- function(temperature, co2) {
  co2$Date <- ymd(paste0(co2$Year, " ", co2$Month, " ", "15"))
  temperature$Date <- ymd(paste0(temperature$Year, " ", temperature$Month, " ", "15"))
  co2AndTemperature <- merge(co2, temperature, by = "Date")
  co2ToLandReg <- lm(Land ~ CO2, co2AndTemperature)
  co2ToOceanReg <- lm(Ocean ~ CO2, co2AndTemperature)
  co2AndTemperature$LandResidual <- rstandard(co2ToLandReg)
  co2AndTemperature$OceanResidual <- rstandard(co2ToOceanReg)

  ##
  landPlot <- ggplot(co2AndTemperature, aes(x = CO2, y = Land)) +
    geom_point(colour=primary) +
    theme_bw()
  oceanPlot <- ggplot(co2AndTemperature, aes(x = CO2, y = Ocean)) +
    geom_point(colour=secondary) +
    theme_bw()
  oceanTempCo2Residual <- ggplot(co2AndTemperature, aes(x = CO2, y = LandResidual)) +
    geom_point(colour=primary) +
    geom_hline(yintercept=3, color = secondary) +
    geom_hline(yintercept=-3, color = secondary) +
    ylim(-4, 4) +
    theme_bw()

  landTempCo2Residual <- ggplot(co2AndTemperature, aes(x = CO2, y = OceanResidual)) +
    geom_point(color = secondary) +
    geom_hline(yintercept=3, color = primary) +
    geom_hline(yintercept=-3, color = primary) +
    ylim(-4, 4) +
    theme_bw()
  subplot(list(withAnnotations(ggplotly(landPlot),"Land temperature vs CO2"),
               withAnnotations(ggplotly(oceanPlot),"Ocean temperature vs CO2"),
               withAnnotations(ggplotly(oceanTempCo2Residual),"Land residuals, R-squared=0.50"),
               withAnnotations(ggplotly(landTempCo2Residual),"Ocean residuals, R-squared=0.73")
  ), nrows=2, margin = 0.07
  )
}

tempToCo2Predictions <- function(temperature, co2) {
  co2$Date <- ymd(paste0(co2$Year, " ", co2$Month, " ", "15"))
  temperature$Date <- ymd(paste0(temperature$Year, " ", temperature$Month, " ", "15"))
  co2AndTemperature <- merge(co2, temperature, by = "Date")
  
  coefC <- min(co2$CO2)*0.8
  co2logReg <- lm(log(CO2 - coefC)~Year, co2)
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
}
