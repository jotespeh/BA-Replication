###################
#
# Author: Jakob Speier <jakob.speier+r(at)gmail.com>
# Filename: 2-estimate-models.R
# Last Edited: 2017-10-10
# Purpose: Use Data from (1) to estimate panel models & plot residuals etc.
#
###################

CIT ~ log(GDP) + PIT + popUrban + popOld + popYoung + kaopen + OECD + EU | Effectiveness & unemployment 
  
EATR ~ "-"

