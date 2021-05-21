
// do file used to create stata datetimes
// commands used: https://www.stata.com/manuals/ddrop.pdf
. use "https://www.stata-press.com/data/r17/visits", replace

. generate admit = date(admit_d, "YMD")
. generate dob = date(dateofbirth, "MDY")
. list admit_d admit dateofbirth dob

. format admit dob %td
. list admit dob

. generate double admit_time = clock(admit_t, "YMDhms")
. generate double disch_time = clock(discharge_t, "YMDhm")
. format admit_time disch_time %tc
. list admit_time disch_time

. format disch_time %tcHH:MM
. list discharge_t disch_time

. generate double admit_Time = Clock(admit_t, "YMDhms")
. format admit_Time %tC

. generate admonth = month(admit)
. generate adyear = year(admit)
. format adyear %ty // inserted by me
. list admit admonth adyear

. generate monthly = ym(adyear,admonth)
. format monthly %tm
. list admit monthly

. generate monthly2 = ym(year(admit), month(admit))
. format monthly2 %tm

. generate dateoftime = dofc(admit_time)
. format dateoftime %td
. list admit_time dateoftime

. generate monthofdate = mofd(admit)
. format monthofdate %tm
. list admit monthofdate

. generate quarterly = qofd(dofm(monthofdate))
. format quarterly %tq
. list monthofdate quarterly


// trim down
. keep dob adyear disch_time admit_time monthly quarterly
// rename
. rename (dob admit_time disch_time monthly quarterly adyear) (td tc tc_hh_mm tm tq ty)
// save
save "readstata13/inst/extdata/datetime.dta", replace
