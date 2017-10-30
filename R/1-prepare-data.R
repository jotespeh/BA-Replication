################### Ein lekker
#
# Author: Jakob Speier <jakob.speier+r(at)gmail.com>
# Filename: 1-prepare-data.R
# Last Edited: 2017-10-09
# Purpose: Load raw (prepared excel) data from Taxes in Europe Database,
#          tidy them and convert to R datafiles. Save into output folder.
# 
################### R File

source("R/0-load-libraries.R")

# --> TODO: Property Taxes, Environment Taxes, Social Contributions

# Statutory Tax Rates ----
# TODO: include reduced rates
path_st <- here("raw", "statutory-tax-rates.xls")
tax_st_sheets <- excel_sheets(path_st)

tax_st <- map(tax_st_sheets, read_excel, path = path_st, skip = 4) %>%
  map(select, country = Country, ends_with("rate"), num_range("", 1995:2016))

names(tax_st) <- c("VAT", "PIT", "CIT")

tax_st$VAT <- filter(tax_st$VAT, `VAT rate` == "Standard") %>% 
  select(-`VAT rate`) %>%
  map_at(as.character(c(2000:2016)), as.numeric) %>%
  bind_rows()

countries <- c(tax_st$VAT$country, "Iceland", "Norway")
tax_st$PIT$country = countries
tax_st$CIT$country = countries

tax_st %<>%
  map(gather, "year", "rate", num_range("",1995:2016)) %>%
  bind_rows(.id = "tax") %>%
  spread(tax, rate) %>%
  mutate(year = as.numeric(year))
  
save(tax_st, file = here("output", "tax_st.rds"))

# Effective Tax Rate ----

tax_ef <- read_csv2(here("raw", "effective-tax.csv")) %>%
  gather("year", "ETR", num_range("",2005:2015)) %>%
  mutate(year = as.numeric(year))

save(tax_ef, file = here("output", "tax_ef.rds"))

# Capital Tax ----

path_ca <- here("raw", "taxes-on-capital.xlsx")
tax_ca_sheets <- excel_sheets(path_ca)

tax_ca <- map(tax_ca_sheets, read_excel, path = path_ca,
              col_types = c("text", rep("numeric", 13)),
              range = as.cell_limits("A4:N36")) %>%
  map(gather, "year", "value", num_range("", 2002:2014))

# Capital tax revenue as share of GDP/all taxation 
# of: total, corporations, households, self-employed, capital stock

names(tax_ca) <- paste0("CAP-",
                        c("gdp-total", "tax-total", "gdp-corp", "tax-corp",
                          "gdp-house", "tax-house", "gdp-self", "tax-self",
                          "gdp-stock", "tax-stock"))

lab_ca <- map_df(tax_ca_sheets, read_excel, path = path_ca,
                 col_names = FALSE, range = "A1")

# --> values as percentages TODO: Labelling
tax_ca %<>% bind_rows(.id = "tax") %>%
  spread(tax, value) %>%
  rename(country = X__1) %>%
  mutate(year = as.numeric(year))

save(tax_ca, file = here("output", "tax_ca.rds"))

# Property Taxes -----

path_pro <- here("raw", "property-taxes.xlsx")
pro_sheets <- excel_sheets(path_pro)
lab_pro <- map(pro_sheets, read_excel, path = path_pro,
               range = as.cell_limits("A1"))

tax_pro <- map(pro_sheets, read_excel, path = path_pro,
               col_types = c("text", rep("numeric",13)),
               range = as.cell_limits("B4:O36"))

# Tax Foundation Data ----
# for years <1995

tax_found <- read_csv(here("raw", "OECD_corp_income_tax_rates_1981-2015.csv")) %>%
  gather(year, CIT, -country) %>%
  map_at(c("year","CIT"), as.numeric) %>% 
  bind_rows() %>%
  filter(year < 1995) %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c"), CIT = CIT*100)

 

## Database of Political Institutions ----

# Manual Cleanup cuz we don't want to consider turk cyprus
DPI <- read_dta("raw/DPI2015.dta") %>% 
  select(country = countryname, year, execLR = execrlc) %>%
  mutate(iso2c = countrycode(country, "country.name", "iso2c"), 
         execR = ifelse(execLR == 1, TRUE, FALSE)) %>%
  filter(iso2c %in% countries, year > 1980,
         country != "Turk Cyprus") %>%
  select(iso2c, year, execR)

save(DPI, file = "output/DPI.rds")



## OECD PIT ----




## Mergin', yo ----

tax_comb <- left_join(tax_st, tax_ef, by = c("country", "year")) %>%
  left_join(tax_ca, by = c("country", "year")) %>%
  full_join(tax_found, by = c("country", "year")) 


save(tax_comb, file = here("output", "tax_comb"))

rm(list = ls())
