library(tidyverse,dbplyr)

emissions <- read_csv("emissions.csv")
#rewrite column names
emissions <- rename(emissions, 
                    c("CO2" = "Emissions.Type.CO2",
                      "N2O" = "Emissions.Type.N2O",
                      "CH4" = "Emissions.Type.CH4",
                      "PowerIndustry" = "Emissions.Sector.Power Industry",
                      "Buildings" = "Emissions.Sector.Buildings",
                      "Transport" = "Emissions.Sector.Transport",
                      "OtherIndustry" = "Emissions.Sector.Other Industry",
                      "Others" = "Emissions.Sector.Other sectors"))

#select needed
data <- select(emissions, c("Country","Year","CO2","N2O","CH4","PowerIndustry",
                    "Buildings","Transport","OtherIndustry","Others"))
#add total gas emissions
data$CO2 <- as.numeric(data$CO2)
data$N2O <- as.numeric(data$N2O)
data$CH4 <- as.numeric(data$CH4)
emissionsSum <- data$CO2 + data$N2O + data$CH4
data <- mutate(data, emissionsSum)

#ref lists
countries <- vector("list", 195)
for(i in 1:195) {
  countries[i] <- data$Country[43*i]
}
sectors <- list("Power Industry", "Buildings", "Transport", "Other Industry",
                "Others")

#regression fits
#sector regression values
PIreg <- vector("list", 195)
Breg <- vector("list", 195)
Treg <- vector("list", 195)
OIreg <- vector("list", 195)
Oreg <- vector("list", 195)
sectorReg <- list(PIreg, Breg, Treg, OIreg, Oreg)

#curve fitting
# for(i in 1:195) {
#  for(j in 1:5) {
    #variable set ups
    tempRegression <- vector("list", 5)
    tempSectorEmissions <- c()
    for(k in 1:43) {
      tempSector <- as.double(data[43*(132-1)+k,5+3])
      tempSectorEmissions <- c(tempSectorEmissions, tempSector)
    }
    
    df <- data.frame(x=1970:2012, y=tempSectorEmissions)
    plot(df$x, df$y, pch=19, xlab='x', ylab='y')
    #fit up to degree 5
    fit1 <- lm(y~x, data=df)
    fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
    fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
    fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
    fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)

    #adjusted R-squared values
    tempRegression[1] <- summary(fit1)$adj.r.squared
    tempRegression[2] <- summary(fit2)$adj.r.squared
    tempRegression[3] <- summary(fit3)$adj.r.squared
    tempRegression[4] <- summary(fit4)$adj.r.squared
    tempRegression[5] <- summary(fit5)$adj.r.squared
    
    plot(df$x, df$y, pch=19, xlab='x', ylab='y')
    x_axis <- seq(1970, 2012, length=43)
    # 
    # lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green', lwd=3)
    # lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red', lwd=3)
    lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='#dd1367', lwd=3)
    # lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue', lwd=2)
    # lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange', lwd=1)
    
    # sectorReg[[j]][[i]] <- tempRegression
#   }
# }

# highestR <- c(0,0,0,0)
# for(i in 1:5) {
#   for(j in 1:195) {
#     for(k in 1:5) {
#       val <- sectorReg[[i]][[j]][[k]]
#       if(is.nan(val)) {
#         val <- 0
#       } else {
#         val <- as.numeric(val)
#       }
#       if(val > highestR[4]) {
#         highestR <- c(i,j,k, val)
#       }
#     }
#   }
# }
# highestR


# 
# 
# #determine best curves
# # bestCurves <- vector("list",length=0)
# # for(i in 1:5) { #sector
# #   for(j in 1:5) { #power
#     tempSectorReg <- c()
#     for(k in 1:195) {
#       tempVal <- sectorReg[[3]][[k]][[3]]
#       if(is.nan(tempVal)) {
#         tempSectorReg <- c(tempSectorReg, 0)
#       } else {
#         tempSectorReg <- c(tempSectorReg, tempVal)
#       }
#     }
#     # res <- t.test(tempSectorReg, mu=0.6, alternative="greater")
#     # if(0.05 > res$p.value) { #assumed significance of 0.05
#     #   bestCurves <- append(bestCurves, c(i,j,res$p.value), after=length(bestCurves))
#     # }
# #   }
# # }
# # 
# # bestCurves
# 
# df <- data.frame(x=tempSectorReg)
# ggplot(df, aes(x=x), binwidth=2) + 
#   geom_dotplot(dotsize = .75, stackratio = 1.2, fill = "#dd1367") +
#   scale_y_continuous(NULL, breaks = NULL)


