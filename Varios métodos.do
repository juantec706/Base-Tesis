


** Callaway
ssc install csdid
ssc install drdid
** No me deja usar duplicados
duplicates report Municipio Año 
duplicates list Municipio Año 
sort Municipio Año
by Municipio Año: gen ocurrencia = _n
drop if ocurrencia == 2



csdid VotoOficial, ivar(Municipio) time(Año) gvar(AñoLlegada) agg(event) asinr method(stdipw) wboot
ssc install addplot
coefplot, vertical yline(0)

addplot: xscale(range(0 20)) ci, ylabel(,labsize(tiny)) legend(order(1 "Pre Treatment" 3 "Post Treatment")) 
addplot, nodraw : ///
       (line variable1 tiempo, lcolor(blue)) ///
       ylabel(, labsize(tiny)) ///
       legend(order(1 "Pre Treatment" 3 "Post Treatment")) ///
       xscale(range(0 20))
estat event

csdid_plot
ssc install did_imputation




webuse nlswork, clear
gen union_year = year if union == 1
bysort idcode: egen first_union = min(union_year)
drop union_year
gen ry = year - first_union
gen never_union = (first_union == .)
forvalues k = 18(-1)2 {
           gen g_`k' = ry == -`k'
        }
        forvalues k = 0/18 {
             gen g`k' = ry == `k'
        }
set matsize 800
eventstudyinteract ln_wage g_* g0-g18, cohort(first_union) control_cohort(never_union) covariates(south) absorb(i.idcode i.year) vce(cluster idcode)
matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
ssc install coefplot
coefplot matrix(C[1]), se(C[2])



gen g_2 = ry <= -2
gen g0 = ry == 0
gen g1 = ry == 1
gen g2 = ry >= 2
eventstudyweights g_2 g0 g1 g2, absorb(i.idcode i.year) cohort(first_union) rel_time(ry) saveweights("weights")
import excel "C:\Users\jmom_\OneDrive\Documentos\weights.xlsx", clear firstrow
keep g_2 first_union ry
reshape wide g_2, i(ry) j(first_union)
graph twoway line g_2* ry, xtitle("relative time") ytitle("weight in TWFE g_2 coefficient") graphregion(fcolor(white)) scheme(sj)
