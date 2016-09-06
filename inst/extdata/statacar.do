
clear all


input int(id) str20 brand str20 model long(type) int(hp) double(max) float(mileage) byte(ecar) long(ldate) str20(ldatecal)
1 "Meyer" "Speed Start 2000" 2 150 176.5 10.2 0 1 2001-01-03
2 "Meyer" "Happy Family" 4 98 145 5.6 0 247 2001-12-31
3 "Akiko" "Susumu 1" 3 45 118.7 -1 0 14 2001-01-23
4 "Akiko" "Susumu 3" 4 80 127.3 6.8 0 134 2001-07-16
5 "Hutch" "Lumberjack 3000" 1 180 156.2 14.2 0 110 2001-06-11
6 "Erikson" "E-Car 2000" 3 . . -2 1 100 2001-05-25
7 "Erikson" "Maxinator" 2147483620 32740 8.988e+307 1.701e+38 100 19 2001-01-30
7 "Erikson" "Mimizer" -2147483647 -32767 -1.798e+308 -1.701e+38 -127 1 2001-01-03
end

gen ldatecal2 = date(ldatecal, "YMD")

generate strL modelStrL = model

drop ldatecal

rename ldatecal2 ldatecal

// bcal uses a special format.
// %tb for business calendar and following the calendar name
format ldatecal %td
format ldate %tbsp500

// missings
replace mileage = .a if mileage ==-1 // no info
replace mileage = .b if mileage ==-2 // not applicable

// Label en
label language en, rename

label var id "Numeric ID"
label var brand "Brand of car"
label var type "Car classification"
label var model "Car model"
label var hp "Horse Power"
label var max "Maximum speed"
label var ldate "Launch date"
label var ldatecal "Launch date (calendar)"

label define type_en 1 "Off-Road" 2 "Roadster" 3 "City car" 4 "Family car" 2147483620 "max" -2147483647 "min", modify
label value type type_en

// Label de
label language de, new

label var id "Numerische ID"
label var brand "Herstellermarke"
label var type "Klassifikation"
label var model "Automodell"
label var hp "Pferdestärken"
label var max "Höchstgeschwindigkeit"
label var ldate "Einführungsdatum"
label var ldatecal "Einführungsdatum (Kalender)"

label define type_de 1 "Geländewagen" 2 "Sportwagen" 3 "Stadtauto" 4 "Familienauto" 2147483620 "max" -2147483647 "min", modify
label value type type_de 

label language en
save "statacar.dta", replace

