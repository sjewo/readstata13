clear all

set obs 4
gen int num = _n
label variable num дцья

label define numlabel 1 "д" 2 "ц" 3 "ь" 4 "я"
label values num numlabel

// create character variable from labels
decode num, gen(chr)

save "encode.dta", replace
