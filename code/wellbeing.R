# -----------------------------------------------------------------------
# Title: Wellbeing Centering Example
# Author: William Murrah
# Description: from Enders and Tofighi 2007
# Created: Tuesday, 19 July 2022
# R version: R version 4.2.1 (2022-06-23)
# Project(working) directory: /Users/wmm0017/Projects/Courses/ERMA_Multilevel_Modeling/products/projects/Wellbeing_EndersTofighi2007Centering
# -----------------------------------------------------------------------
library(psych)
library(lme4)
library(performance)
library(texreg)
library(ggeffects)
library(sjPlot)
welldat <- read.csv(file = "data/welldata.csv", header = TRUE)

welldat <- within(welldat, {
  size.f <- factor(size)
  hours.cgm <- hours - mean(hours)
  hours.cwc <- hours - ave(hours, size, FUN = mean)
  hours.xbar <- mean(hours)
  hours.xbarj <- ave(hours, size, FUN = mean)
  hours.xbarjc <- hours.xbarj - mean(hours.xbarj)
  wellbeing.ybar <- mean(wellbeing)
})


describeBy(welldat, group = welldat$size)

round(cor(welldat[, -c(1,11)]), 2)


rawmod <- lmer(wellbeing ~ hours + (hours | size.f), 
               data = welldat)
summary(rawmod)
icc(rawmod)

plot(ave(wellbeing, size, FUN = mean) ~ 
       ave(hours, size, FUN = mean), 
     data = welldat)

cgmmod <- lmer(wellbeing ~ hours.cgm + (hours.cgm | size),
               data = welldat)
summary(cgmmod)

screenreg(list(rawmod, cgmmod), 
          custom.model.names = c("raw", "cgm"))

plot(ggpredict(cgmmod, terms = c("hours.cgm", "size"), 
               type = "random"), ci = FALSE, add.data = TRUE)

cwcmod <- lmer(wellbeing ~ hours.cwc + (hours.cwc | size),
               data = welldat)
screenreg(list(rawmod, cgmmod, cwcmod),
          custom.model.names = c("raw", "cgm", "cwc"))
plot(ggpredict(cwcmod, terms = c("hours.cwc", "size"),
               type = "random"), ci = FALSE, add.data = TRUE)

cwcmod.gm <- lmer(wellbeing ~ hours.cwc + hours.xbarjc + (hours.cwc | size),
                  data = welldat)


screenreg(list(rawmod, cgmmod, cwcmod, cwcmod.gm),
          custom.model.names = c("raw", "cgm", "cwc", "cwc.gm"))


plot(ggpredict(cwcmod.gm, 
               terms = c("hours.xbarjc","hours.cwc","size"),
               type = "random"), ci = FALSE, add.data = TRUE)
