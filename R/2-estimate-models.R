###################
#
# Author: Jakob Speier <jakob.speier+r(at)gmail.com>
# Filename: 2-estimate-models.R
# Last Edited: 2017-10-10
# Purpose: Use Data from (1) to estimate panel models & plot residuals etc.
#
###################

load("output/tax_final.rds")

tax <- plm.data(tax_final, c("iso2c", "year"))

# TODO:
# Balanced Panel
# Add missing PIT
# Regression Diagnostics


pooled <-
  plm(CIT_f ~ log(GDP) + popUrb + pop65 + pop14 + kaopen + PIT + OECD + EU + 
        execR + govspend + lag(CIT_f),
      tax, model = "pooling", effect = "twoways")

model <-
  plm(CIT_f ~ log(GDP) + popUrb + pop65 + pop14 + kaopen + PIT  + OECD + EU + 
        execR  + lag(CIT_f),
    tax, model = "within", effect = "twoways")

model2 <-
  plm(CIT_f ~ log(GDP) + popUrb + pop65 + pop14 + kaopen + PIT + OECD + EU + 
        execR + lag(CIT_f),
      tax, model = "random", effect = "twoways")


# Regression Diagnostic ----

phtest(model2, model) # --> p < 0.05 therefore use FE model

plmtest(pooled, type = "bp")



 Effectiveness & unemployment 
  
EATR ~ "-"

