clear all

set obs 2

gen double v1 = _n
recode v1 2 = 1.2

label define v1 1 "one"

label values v1 v1

save "nonint.dta", replace
