# Coursera Data Science Specialization
# Regression Models Course Project
# Author: John James
# Date: April 15, 2016
# mt.R

## ---- environment
library(car)
library(dplyr)
library(GGally)
library(ggplot2)
library(grid)
library(gridExtra)
library(pastecs)
library(RColorBrewer)
library(relaimpo)
data("mtcars")

## ---- end

######################################################################################
##                                        DATA                                      ##
######################################################################################
## ---- data
cars <- mtcars
mtStats <- lapply( cars , function(x) rbind( mean = mean(x) ,
                                         median = median(x) ,
                                         minimum = min(x) ,
                                         maximum = max(x) ,
                                         s.size = length(x) ) )
mtStats <- data.frame(mtStats)

## Create Transmission Categorical Variable
cars$Transmission[cars$am == 0] <- "Automatic"
cars$Transmission[cars$am == 1] <- "Manual"

## Boxplot of MPG versus transmission
mpgByTran <- ggplot(data = cars, aes(x = Transmission, y = mpg)) + 
  geom_boxplot(aes(fill=Transmission)) +
  ylab("MPG") +
  xlab("Transmission") +
  theme_bw() +
  ggtitle("MPG by Transmission") +
  scale_fill_brewer(name = "Transmission", palette = "Dark2") 

## ---- end


######################################################################################
##                              DATA ANALYSIS                                       ##
######################################################################################
## ---- model0
lm0   <- lm(mpg ~ Transmission,  data = cars)
lm0s  <- summary(lm0)
lm0t  <- t.test(mpg ~ Transmission, data = cars)
## ---- end


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
cordf2 <- arrange(cordf2, Correlation)
## ---- end

## ---- modelSets
lm1   <- lm(mpg ~ factor(am), data = mtcars)
lm2   <- update(lm1, mpg ~ factor(am) + wt)
lm3   <- update(lm2, mpg ~ factor(am) + wt + cyl)
lm4   <- update(lm3, mpg ~ factor(am) + wt + cyl + disp)
lm5   <- update(lm4, mpg ~ factor(am) + wt + cyl + disp + hp)
lm6   <- update(lm5, mpg ~ factor(am) + wt + cyl + disp + hp + drat)
lm7   <- update(lm6, mpg ~ factor(am) + wt + cyl + disp + hp + drat + vs)
lm8   <- update(lm7, mpg ~ factor(am) + wt + cyl + disp + hp + drat + vs + carb)
lm9   <- update(lm8, mpg ~ factor(am) + wt + cyl + disp + hp + drat + vs + carb + gear)
lm10  <- update(lm9, mpg ~ factor(am) + wt + cyl + disp + hp + drat + vs + carb + gear + qsec)

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

## ---- end
