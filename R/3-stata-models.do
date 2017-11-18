


use ../output/tax_final.dta, clear

egen id = group(country)
xtset id year



replace GDP = log(GDP)
replace popTot = log(popTot)

* Lag endogenous variables
gen laggovspend = govspend[_n-1]
gen lagGDP = GDP[_n-1]
gen lagecoOpen = ecoOpen[_n-1]
gen lagCIT = CIT_f[_n-1]

local controls "popUrb pop14 pop65 lagGDP laggovspend execR kaopen lagecoOpen PIT OECD EU"

local controls "popUrb pop14 pop65 GDP govspend execR kaopen ecoOpen PIT OECD EU"

xtregar CIT_f popUrb pop14 pop65 popTot laggovspend execR kaopen lagecoOpen PIT OECD EU, fe
xtpcse CIT_f popUrb pop14 pop65 popTot laggovspend execR kaopen lagecoOpen PIT OECD EU lagCIT i.id i.year, correlation(ar1)


xtscc CIT_f popUrb pop14 pop65 popTot laggovspend execR kaopen lagecoOpen PIT OECD EU lagCIT, fe

xtreg CIT_f popUrb pop14 pop65 popTot laggovspend execR kaopen lagecoOpen PIT OECD EU lagCIT, fe cluster(id)


xtreg CIT_f popUrb pop14 pop65 popTot laggovspend lagGDP execR kaopen lagecoOpen PIT OECD EU lagCIT, fe cluster(id)
