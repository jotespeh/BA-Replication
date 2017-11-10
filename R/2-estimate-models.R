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

# Summary Stats & Descriptive ----

path_table <- "/Users/jakob/Uni/papers/BA/tables/"

tax_final %>% mutate(GDP = log(GDP), popTot = log(popTot)) %>% 
  select(CIT_f, GDP, popTot, ecoOpen, kaopen, EU, OECD, execR, govspend, PIT,
         pop14, pop65, popUrb,) %>%
  stargazer(out = paste0(path_table, "summary_statistics.tex"),
            style = "apsr", title = "Summary Statistics", label = "tab_sum",
            digit.separate = 0, covariate.labels =
              c("Corporatoe Income Tax Rate", "Size (GDP)", "Size (Population)",
                "Economic Openness", "Capital Openness", "EU membershio", "OECD membership",
                "Rightwing Executive", "Final Government Expenditure", "Personal Income Tax Rate",
                "Population share <14", "Population share >65", "Population share Urban"))

# TODO:
# Balanced Panel
# Regression Diagnostics


baseline <- CIT_f ~ log(popTot) + popUrb + pop65 + pop14 + PIT + govspend
openness <- CIT_f ~ log(popTot) + popUrb + pop65 + pop14 + PIT + govspend +
                    ecoOpen + kaopen
membersh <- CIT_f ~ log(popTot) + popUrb + pop65 + pop14 + PIT + govspend +
                    ecoOpen + kaopen + EU + OECD
altrntve <- CIT_f ~ log(GDP) + popUrb + pop65 + pop14 + PIT + govspend +
                    ecoOpen + kaopen + EU + OECD

model_baseline <- plm(baseline, tax, model = "within", effect = "twoways")
model_openness <- plm(openness, tax, model = "within", effect = "twoways")
model_membersh <- plm(membersh, tax, model = "within", effect = "twoways")
model_altrntve <- plm(altrntve, tax, model = "within", effect = "twoways")


stargazer(model_baseline, model_openness, model_membersh,
          out = paste0(path_table, "regression1.tex"), title = "Regression Results",
          label = "tab_reg1", style = "apsr")

pooled <-
  plm(CIT_f ~ log(GDP) + popUrb + pop65 + pop14 + kaopen + PIT + OECD + EU + 
        execR + govspend,
      tax, model = "pooling", effect = "twoways")

model <-
  plm(openness, tax, model = "within", effect = "twoways")

model2 <-
  plm(CIT_f ~ log(GDP) + popUrb + pop65 + pop14 + kaopen + govspend + PIT +
        EU + OECD + execR,
      tax, model = "random", effect = "twoways")

tax_final %>% filter(is.na(PIT)) %>% group_by(country) %>% summarise(N = n())
stargazer(tax_final, type = "text")




# Regression Diagnostic ----

phtest(model, model2) # --> p < 0.05 therefore use FE model

plmtest(model, effects = "twoways", type = "bp")



# EATR Model as robustness check ----

load("output/tax_comb")
tax_etr <- select(tax_comb, country, year, ETR) %>%
  mutate(iso2c = countrycode(country, "country.name", "iso2c")) %>%
  select(-country) %>%
  filter(!is.na(ETR))

tax_alt <- left_join(tax_final, tax_etr, by = c("iso2c", "year")) %>%
  filter(!is.na(ETR))

tax2 <- pdata.frame(tax_alt, index = c("iso2c", "year"))

effective <- ETR ~ log(popTot) + popUrb + pop65 + pop14 + PIT + govspend +
             ecoOpen + kaopen + EU + OECD

eff_gdp <- ETR ~ log(GDP) + popUrb + pop65 + pop14 + PIT + govspend +
           ecoOpen + kaopen + EU + OECD


model_effective <- plm(effective, tax2, model = "within", effect = "twoways")
model_eff_alt <- plm(eff_gdp, tax2, model = "within", effect = "twoways")
model_subset    <- plm(membersh, tax2, model = "within", effect = "twoways")

stargazer(model_effective, model_eff_alt, model_subset,
          out = paste0(path_table, "regression2.tex"), title = "Regression Results",
          label = "tab_reg2", style = "apsr")



