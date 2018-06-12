

dat1 <- read.csv("C:\\NRS_404_504\\00_app\\01_data\\processing\\08_cloudmetrics\\WebModelingApp_metrics.csv")

dat2 <- read.csv("C:\\NRS_404_504\\00_app\\01_data\\Lidar_Field_Plots_Shapefile_2014.csv")

alldat <- merge(dat2, dat1, by.x="plot_id", by.y="Identifier")

write.csv(alldat, "C:\\NRS_404_504\\00_app\\01_data\\AGB_metrics.csv")

subdat <- alldat[c("AGB", "Elev.maximum", "Elev.mean", "Elev.mode", 
                   "Elev.stddev", "Elev.variance", "Elev.CV", "Elev.IQ", "Elev.skewness", 
                   "Elev.kurtosis", "Elev.AAD", "Elev.MAD.median", "Elev.MAD.mode", 
                   "Elev.P75", "Elev.P90", "Elev.P99")]

cor(subdat)

library(PerformanceAnalytics)
chart.Correlation(subdat)

lm1 <- lm(AGB ~ Elev.mean, data=subdat)
summary(lm1)

library(randomForest)
rf1 <- randomForest(AGB ~ Elev.mean + Elev.mode + Elev.stddev + Elev.CV + Elev.IQ + Elev.skewness + Elev.P75, data=subdat)
varImpPlot(rf1)
