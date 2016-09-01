clear all

set obs 2

gen v_1 = _n
gen v_2 = _n
gen long_name_multiple_underscores = _n

compress

save "underscore.dta", replace
