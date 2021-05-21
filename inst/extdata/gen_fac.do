clear all

set obs 2

gen v1 = _n

label define v1 1 "one"

label values v1 v1

compress

save "gen_fac.dta", replace
