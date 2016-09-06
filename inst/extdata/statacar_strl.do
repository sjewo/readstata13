clear all
use "statacar.dta"

generate strL modelStrL = model

save "statacar_strl.dta", replace
