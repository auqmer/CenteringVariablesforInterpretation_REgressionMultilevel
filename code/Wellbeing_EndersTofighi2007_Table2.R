# -----------------------------------------------------------------------
# Title: Wellbeing Centering Example Larger Sample
# Author: William Murrah
# Description: Example of centering from Enders and Tofighi (2007)  using
#              larger artificial data (Table 2).  
# Created: Thursday, 21 July 2022
# R version: R version 4.2.1 (2022-06-23)
# -----------------------------------------------------------------------
library(lme4)
library(texreg)
# Import Table 2-3 data with variable names from SPSS syntax file.
welldat <- read.table("data/table2-3.dat", # Modify to find your copy of data
                      header = FALSE, 
                      col.names = c("CLUSTER", "WELLBEING", "HOURS", 
                                    "SIZE", "HOURS_XBARj", "HOURSCGM", 
                                    "HOURSCWC", "SIZECGM", "HOURS_XBARjCGM"))

# To make typing names easier, convert to all lowercase.
names(welldat) <- tolower(names(welldat))

# Table 2, Model 1.
model_1 <- lmer(wellbeing ~ 1 + (1 |cluster), data = welldat,
              REML = FALSE)
summary(model_1)

# Table 2, Model 2.
model_2CGM <- lmer(wellbeing ~ hourscgm + (hourscgm | cluster),
                   data = welldat,
                   REML = FALSE)
summary(model_2CGM)

model_2CWC <- lmer(wellbeing ~ hourscwc + (hourscwc | cluster),
                   data = welldat,
                   REML = FALSE)
summary(odel_2CWC)

# Table 2, Model 3.
model_3CGM <- lmer(wellbeing ~ sizecgm + hourscgm + (hourscgm | cluster),
                   data = welldat,
                   REML = FALSE)
summary(model_3CGM)

model_3CWC <- lmer(wellbeing ~ sizecgm + hourscwc + (hourscwc | cluster),
                   data = welldat,
                   REML = FALSE)
summary(model_3CWC)

screg <- function(l, ...) {
  screenreg(l, include.aic = FALSE,
                   include.bic = FALSE,
                   include.nobs = FALSE,
                   include.groups = FALSE,
                   include.loglik = FALSE)
}

screg(model_1, single.row = TRUE, 
          custom.coef.names = c("(Intercept)"),
          )
screg(list(model_2CGM, model_2CWC), 
          single.row = TRUE,
          custom.coef.names = c("(Intercept)", "Hours", "Hours"))
screg(list(model_3CGM, model_3CWC), 
          single.row = TRUE,
          custom.coef.names = c("(Intercept)", "Size", "Hours", "Hours"))

screenreg(list(mod1tab, mod2tab, mod3tab))
