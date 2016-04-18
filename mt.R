# Coursera Data Science Specialization
# Regression Models Course Project
# Author: John James
# Date: April 15, 2016
# mt.R

## ---- environment
library(boot)
library(car)
library(dplyr)
library(GGally)
library(ggplot2)
library(grid)
library(gridExtra)
library(pastecs)
library(RColorBrewer)
data("mtcars")

## ---- end

######################################################################################
##                                        DATA                                      ##
######################################################################################
## ---- data
cars <- mtcars[c("mpg", "disp", "hp", "drat", "wt", "qsec")]
mtStats <- lapply(cars, function(x) rbind( mean = mean(x) ,
                                         median = median(x) ,
                                         minimum = min(x) ,
                                         maximum = max(x) ,
                                         s.size = length(x) ) )
mtStats <- data.frame(mtStats)

cars$cyl  <- as.factor(mtcars$cyl)
cars$vs   <- as.factor(mtcars$vs)
cars$am   <- as.factor(mtcars$am)
cars$gear <- as.factor(mtcars$gear)
cars$carb <- as.factor(mtcars$carb)

label <- c("Miles per Gallon", "Cylinders", "Displacement", "Horsepower", 
                      "Rear Axle Ratio", "Weight", "Qtr Mile Time", "Engine Type", "Transmission Type", 
                      "Forward Gears", "Carburetors")
type <-  c("Continuous", "Categorical", "Continuous", "Continuous",
                      "Continuous", "Continuous", "Continuous", "Categorical", "Categorical",
                      "Categorical", "Categorical")
variable <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
desc <- c("Miles (US) / Gallon", "Number of Cylinders (4, 6, or 8)", "Displacement (cu. in.)", "Gross Horsepower",
                     "Rear Axle Ratio", "Weight (1000 lb)", "1/4 Mile Time (sec.)", "V/Straight Engine (0 = V, 1 = Straight)", 
                     "Transmission (0 = automatic, 1 = manual)", "Number of Forward Gears (3,4, or 5)", 
                     "Number of Carburetors (1,2,3,4,6 or 8)")
carsFormat <- data.frame(label, type, variable, desc)
## ---- end


######################################################################################
##                              DATA ANALYSIS                                       ##
######################################################################################

## ---- corrOutcome
corcyl  <- abs(cor(mtcars$mpg, mtcars$cyl))
cordisp <- abs(cor(mtcars$mpg, mtcars$disp))
corhp   <- abs(cor(mtcars$mpg, mtcars$hp))
cordrat <- abs(cor(mtcars$mpg, mtcars$drat))
corwt   <- abs(cor(mtcars$mpg, mtcars$wt))
corqsec <- abs(cor(mtcars$mpg, mtcars$qsec))
corvs   <- abs(cor(mtcars$mpg, mtcars$vs))
corgear <- abs(cor(mtcars$mpg, mtcars$gear))
corcarb <- abs(cor(mtcars$mpg, mtcars$carb))

cordf1 <- data.frame(Variable = c("Cylinders", "Displacement", "HP", "Rear Axle Ratio", "Weight", "Qtr Mile", "VS", "Gears", "Carburetors"),
                    Correlation = c(corcyl, cordisp, corhp, cordrat, corwt, corqsec, corvs, corgear, corcarb))
cordf1 <- arrange(cordf1, desc(Correlation))
## ---- end


## ---- corrPredictors
corcyl  <- abs(cor(mtcars$am, mtcars$cyl))
cordisp <- abs(cor(mtcars$am, mtcars$disp))
corhp   <- abs(cor(mtcars$am, mtcars$hp))
cordrat <- abs(cor(mtcars$am, mtcars$drat))
corwt   <- abs(cor(mtcars$am, mtcars$wt))
corqsec <- abs(cor(mtcars$am, mtcars$qsec))
corvs   <- abs(cor(mtcars$am, mtcars$vs))
corgear <- abs(cor(mtcars$am, mtcars$gear))
corcarb <- abs(cor(mtcars$am, mtcars$carb))

cordf2 <- data.frame(Variable = c("Cylinders", "Displacement", "HP", "Rear Axle Ratio", "Weight", "Qtr Mile", "VS", "Gears", "Carburetors"),
                     Correlation = c(corcyl, cordisp, corhp, cordrat, corwt, corqsec, corvs, corgear, corcarb))
cordf2 <- arrange(cordf2, desc(Correlation))
## ---- end


## ---- modelSets
lm1   <- lm(mpg ~ factor(am), data = mtcars)
lm2   <- update(lm1, mpg ~ factor(am) + wt)
lm3   <- update(lm2, mpg ~ factor(am) + wt + factor(cyl))
lm4   <- update(lm3, mpg ~ factor(am) + wt + factor(cyl) + disp)
lm5   <- update(lm4, mpg ~ factor(am) + wt + factor(cyl) + disp + hp)
lm6   <- update(lm5, mpg ~ factor(am) + wt + factor(cyl) + disp + hp + drat)
lm7   <- update(lm6, mpg ~ factor(am) + wt + factor(cyl) + disp + hp + drat + factor(vs)) 
lm8   <- update(lm7, mpg ~ factor(am) + wt + factor(cyl) + disp + hp + drat + factor(vs) + factor(carb))
lm9   <- update(lm8, mpg ~ factor(am) + wt + factor(cyl) + disp + hp + drat + factor(vs) + factor(carb) + factor(gear))
lm10  <- update(lm9, mpg ~ factor(am) + wt + factor(cyl) + disp + hp + drat + factor(vs) + factor(carb) + factor(gear) + qsec)

lm11   <- update(lm1,  mpg ~ factor(am) + wt)
lm12   <- update(lm11, mpg ~ factor(am) + wt + hp)
lm13   <- update(lm12, mpg ~ factor(am) + wt + hp + disp)
lm14   <- update(lm13, mpg ~ factor(am) + wt + hp + disp + carb)

lm21   <- update(lm1,  mpg ~ factor(am) + hp)
lm22   <- update(lm11, mpg ~ factor(am) + hp + cyl)
lm23   <- update(lm12, mpg ~ factor(am) + hp + cyl + wt)
lm24   <- update(lm13, mpg ~ factor(am) + hp + cyl + wt + drat)

lm31  <- update(lm1,  mpg ~ factor(am) + carb)
lm32  <- update(lm31, mpg ~ factor(am) + carb + vs)
lm33  <- update(lm32, mpg ~ factor(am) + carb + vs + qsec)
lm34  <- update(lm33, mpg ~ factor(am) + carb + vs + qsec + hp)
lm35  <- update(lm34, mpg ~ factor(am) + carb + vs + qsec + hp + cyl)
lm36  <- update(lm35, mpg ~ factor(am) + carb + vs + qsec + hp + cyl + disp)
lm37  <- update(lm36, mpg ~ factor(am) + carb + vs + qsec + hp + cyl + disp + wt)
lm38  <- update(lm37, mpg ~ factor(am) + carb + vs + qsec + hp + cyl + disp + wt + drat)
lm39  <- update(lm38, mpg ~ factor(am) + carb + vs + qsec + hp + cyl + disp + wt + drat + gear)

lm41  <- update(lm1,  mpg ~ factor(am) + carb)
lm42  <- update(lm41, mpg ~ factor(am) + carb + vs)
lm43  <- update(lm42, mpg ~ factor(am) + carb + vs + hp)
lm44  <- update(lm43, mpg ~ factor(am) + carb + vs + hp + qsec)

lm61   <- update(lm1, mpg ~ factor(am) + log(wt))
lm62   <- update(lm61, mpg ~ factor(am) + log(wt) + factor(cyl))
lm63   <- update(lm62, mpg ~ factor(am) + log(wt) + factor(cyl) + disp)
lm64   <- update(lm63, mpg ~ factor(am) + log(wt) + factor(cyl) + disp + hp)
lm65   <- update(lm64, mpg ~ factor(am) + log(wt) + factor(cyl) + disp + hp + drat)
lm66   <- update(lm65, mpg ~ factor(am) + log(wt) + factor(cyl) + disp + hp + drat + vs)
lm67   <- update(lm66, mpg ~ factor(am) + log(wt) + factor(cyl) + disp + hp + drat + vs + carb)
lm68   <- update(lm67, mpg ~ factor(am) + log(wt) + factor(cyl) + disp + hp + drat + vs + carb + gear)
lm69   <- update(lm68, mpg ~ factor(am) + log(wt) + factor(cyl) + disp + hp + drat + vs + carb + gear + qsec)

lm71   <- update(lm1, mpg ~  factor(am) + log(wt))
lm72   <- update(lm71, mpg ~ factor(am) + log(wt) + log(hp))
lm73   <- update(lm72, mpg ~ factor(am) + log(wt) + log(hp) + factor(cyl))
lm74   <- update(lm73, mpg ~ factor(am) + log(wt) + log(hp) + factor(cyl) + disp)
## ---- end

## ---- plots
carsVars <- mtcars[c("mpg", "am", "disp", "hp", "cyl", "wt")]
sp1 <- ggpairs(carsVars)
## ---- end

## ---- getPvalue
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

lm1p  <- lmp(lm1)
lm10p <- lmp(lm10)
lm72p <- lmp(lm72)
## ---- end

######################################################################################
##                                  BOOTSTRAP                                       ##
######################################################################################
mpgEst <- function(data, indices) {
  d       = data[indices, ]
  lm3r2   = summary(lm3)$adj.r.squared
  lm12r2  = summary(lm12)$adj.r.squared
  lm22r2  = summary(lm22)$adj.r.squared
  lm23r2  = summary(lm23)$adj.r.squared
  lm72r2  = summary(lm72)$adj.r.squared
  lm73r2  = summary(lm73)$adj.r.squared
  r2      = c(lm3r2, lm12r2, lm22r2, lm23r2, lm72r2, lm73r2)
  return(r2)
  
}

results <- boot(data = mtcars, statistic = mpgEst, R = 5000)
print(results)