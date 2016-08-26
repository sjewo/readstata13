clear all

set obs 4
gen int num = _n
label variable num ����

label define numlabel 1 "�" 2 "�" 3 "�" 4 "�"
label values num numlabel

// create character variable from labels
decode num, gen(chr)

save "encode.dta", replace
