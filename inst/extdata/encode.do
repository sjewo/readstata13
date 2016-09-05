clear all

set obs 6
gen int num = _n
label variable num äöüß

label define numlabel 1 "ä" 2 "ö" 3 "ü" 4 "ß" 5 "€" 6 "Œ"
label values num numlabel

// create character variable from labels
decode num, gen(chr)

save "encode.dta", replace
