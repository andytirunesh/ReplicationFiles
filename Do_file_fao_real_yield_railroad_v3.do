
cd "E:/Africa" 


do master_merge.do



/********************************************************************
* District Fixed Effects Regression and Robustness Analysis - FAO
********************************************************************/

* Set global macro for global vars
global controls class1 class2 alt_mean prec_mean km_to_river km_to_city_1900 km_to_coast // dist2coast  
global coord lat1 long1 //lat2 long2 lat_long
global a a(adm2_code year)
global b a(tribe_country year)
global c a(pair_year)



/*==================================================
= (1) District Fixed Effects Regression =
==================================================*/

use "final_fao_data.dta", clear
eststo clear	


eststo I:   reghdfe ln_real_yield rail_dummy, $a cluster(adm2_code)
eststo II:  reghdfe ln_real_yield rail_dummy $controls, $a cluster(adm2_code)
eststo III: reghdfe ln_real_yield rail_dummy $coord $controls, $a cluster(adm2_code)
eststo IV:  reghdfe ln_real_yield rail_dummy $coord $controls, $b cluster(adm2_code)

esttab I II III IV using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs title(District FE regression) ///
    s(Controls lat_long District Tribe Year N r2, ///
    label("Controls" "Lat & Long" "District fixed effects" ///
    "Tribe x Country fixed effects" "Year fixed effects" ///
    "N" "R-squared")) starlevels(* 0.10 ** 0.05 *** 0.010) ///
    replace label keep(rail_dummy)


	
/*=========================================
= (2) District-Level Contiguous Pairs =
=========================================*/

** (a) Continuous Treatment

use "final_fao_data.dta", clear
*collapse (mean) real_yield, by(adm0_code adm1_code adm2_code year)
joinby adm0_code adm1_code adm2_code using "Road/rail_district_pair_level_data_clusterd", unmatched(none)

keep if rail_density > 0
egen pair_year = group(dist_pair_id year)
gen ln_rail_density = ln(rail_density)
*gen ln_real_yield = ln(real_yield)
label var ln_rail_density "ln(Railroad per sqkm)"

eststo clear
eststo I:  reghdfe ln_real_yield rail_density if clusterd == 1, a(pair_year) cluster(cluster_id)
eststo II:  reghdfe ln_real_yield rail_density $controls if clusterd == 1, a(pair_year) cluster(cluster_id)

	

use "final_fao_data.dta", clear
joinby adm0_code adm1_code adm2_code using "Road/_c", unmatched(none)
keep if rail_density > 0

egen state_id = group(adm1_code adm0_code)
gen ln_rail_density = ln(rail_density)
label var ln_rail_density "ln(Railroad per sqkm)"

	
eststo III:  reghdfe ln_real_yield rail_density, a(state_id year) cluster(adm2_code)
eststo IV:  reghdfe ln_real_yield rail_density $controls, a(state_id year) cluster(adm2_code)
	

esttab I II III IV using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs title(Contiguous District Pair regression Results) ///
    s(Controls Piar_year Year N r2, ///
    label("Controls" "Pair x time effects" ///
    "Year fixed effects" "N" "R-squared")) ///
    starlevels(* 0.10 ** 0.05 *** 0.010) replace label ///
    keep(rail_density)
	
	
	
/*======================================
= (3) Placebo vs Actual Distance Effect =
======================================*/

use "final_fao_data.dta", clear

replace km_to_actual_plbo = -1 * km_to_actual_plbo if placebo == 0
gen distance_pos    = km_to_actual_plbo * placebo
gen distance_pos_2  = distance_pos^2
gen km_to_actual_plbo_2 = km_to_actual_plbo^2

eststo clear

eststo I:   reghdfe ln_real_yield placebo  ///
            if inrange(abs(km_to_actual_plbo), 0, 25), ///
            a(tribe_country year) cluster(adm2_code)

eststo II:  reghdfe ln_real_yield placebo   ///
            $controls if inrange(abs(km_to_actual_plbo), 0, 25), ///
            a(tribe_country year) cluster(adm2_code)

eststo III: reghdfe ln_real_yield placebo ///
            if inrange(abs(km_to_actual_plbo), 25, 50), ///
            a(tribe_country year) cluster(adm2_code)

eststo IV:  reghdfe ln_real_yield placebo  ///
            $controls if inrange(abs(km_to_actual_plbo), 25, 50), ///
            a(tribe_country year) cluster(adm2_code)

esttab I II III IV using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs title(Placebo vs Actual) ///
    s(Controls lat_long Tribe_vcount Year N r2, ///
    label("Controls" "Lat & Long" "Tribe x Country fixed effects" ///
    "Year fixed effects" "N" "R-squared")) ///
    starlevels(* 0.10 ** 0.05 *** 0.010) replace label ///
    keep(placebo)




/*==================================
= (4) Military and Mining Dummy =
==================================*/

use "final_fao_data.dta", clear
eststo clear	

eststo I:   reghdfe ln_real_yield dummy_min_milt, $a cluster(adm2_code)
eststo II:  reghdfe ln_real_yield dummy_min_milt $controls, $a cluster(adm2_code)
eststo III: reghdfe ln_real_yield dummy_min_milt $coord $controls, $a cluster(adm2_code)
eststo IV:  reghdfe ln_real_yield dummy_min_milt $coord $controls, $b cluster(adm2_code)

esttab I II III IV using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs title(District FE regression: Mining and military) ///
    s(Controls lat_long District Tribe Year N r2, ///
    label("Controls" "Lat & Long" "District fixed effects" ///
    "Tribe x Country fixed effects" "Year fixed effects" ///
    "N" "R-squared")) starlevels(* 0.10 ** 0.05 *** 0.010) ///
    append label keep(dummy_min_milt)


/*====================
= (5) NDVI Outcomes =
====================*/

use "final_fao_data.dta", clear
eststo clear	

eststo I:   reghdfe ln_evi rail_dummy, $a cluster(adm2_code)
eststo II:  reghdfe ln_evi rail_dummy $controls, $a cluster(adm2_code)
eststo III: reghdfe ln_evi rail_dummy $coord $controls, $a cluster(adm2_code)
eststo IV:  reghdfe ln_evi rail_dummy $coord $controls, $b cluster(adm2_code)

esttab I II III IV using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs title(NDVI) ///
    s(Controls lat_long District Tribe Year N r2, ///
    label("Controls" "Lat & Long" "District fixed effects" ///
    "Tribe x Country fixed effects" "Year fixed effects" ///
    "N" "R-squared")) starlevels(* 0.10 ** 0.05 *** 0.010) ///
    append label keep(rail_dummy)


/*=========================================
= (6) Alternative SE (HAC 100km & 200km) =
=========================================*/



use "final_fao_data.dta", clear


eststo clear	
						//100 km
eststo I: acreg ln_real_yield rail_dummy, latitude(lat1) longitude(long1) id(grid_id) time(year) ///
						dist(100) spatial pfe1(adm2_code)  pfe2(year) hac lagcutoff(1)
eststo II: acreg ln_real_yield rail_dummy $controls, latitude(lat1) longitude(long1) id(grid_id) time(year) ///
						dist(100) spatial pfe1(adm2_code)  pfe2(year) hac lagcutoff(1)
eststo III: acreg ln_real_yield rail_dummy $coord $controls, latitude(lat1) longitude(long1) id(grid_id) time(year) ///
						dist(100) spatial pfe1(adm2_code)  pfe2(year) hac lagcutoff(1)
						
						//200 km
eststo V: acreg ln_real_yield rail_dummy, latitude(lat1) longitude(long1) id(grid_id) time(year) ///
						dist(200) spatial pfe1(adm2_code)  pfe2(year) hac lagcutoff(1)
eststo VI: acreg ln_real_yield rail_dummy $controls, latitude(lat1) longitude(long1) id(grid_id) time(year) ///
						dist(200) spatial pfe1(adm2_code)  pfe2(year) hac lagcutoff(1)
eststo VII: acreg ln_real_yield rail_dummy $coord $controls, latitude(lat1) longitude(long1) id(grid_id) time(year) ///
						dist(200) spatial pfe1(adm2_code)  pfe2(year) hac lagcutoff(1)
					
						
esttab I II III V VI VII  using "Tables/Table_fao.rtf", b(4) se r2 obs title(District FE regression: HAC se 200km cut-off) s(Controls lat_long District Tribe Year N r2 , label("Controls" "Lat & Long" "District fixed effects" "Tribe X Country fixed effects" "Year fixed effects" "N" "R-squared")) ///
starlevels( * 0.10 ** 0.05 *** 0.010)  append label keep(rail_dummy) 


** 
eststo clear						//100 km
eststo I:   acreg ln_real_yield placebo  ///
            if inrange(abs(km_to_actual_plbo), 0, 25), ///
            latitude(lat1) longitude(long1) id(grid_id) time(year) ///
			dist(100) spatial pfe1(tribe_country)  pfe2(year) hac lagcutoff(1)

eststo II:  acreg ln_real_yield placebo   ///
            $controls if inrange(abs(km_to_actual_plbo), 0, 25), ///
            latitude(lat1) longitude(long1) id(grid_id) time(year) ///
			dist(100) spatial pfe1(tribe_country)  pfe2(year) hac lagcutoff(1)
			
						//200 km
eststo III: acreg ln_real_yield placebo ///
            if inrange(abs(km_to_actual_plbo), 0, 25), ///
            latitude(lat1) longitude(long1) id(grid_id) time(year) ///
			dist(200) spatial pfe1(tribe_country)  pfe2(year) hac lagcutoff(1)

eststo IV:  acreg ln_real_yield placebo  ///
            $controls if inrange(abs(km_to_actual_plbo), 0, 25), ///
            latitude(lat1) longitude(long1) id(grid_id) time(year) ///
			dist(200) spatial pfe1(tribe_country)  pfe2(year) hac lagcutoff(1)

esttab I II III IV using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs title(Placebo vs Actual) ///
    s(Controls lat_long Tribe_vcount Year N r2, ///
    label("Controls" "Lat & Long" "Tribe x Country fixed effects" ///
    "Year fixed effects" "N" "R-squared")) ///
    starlevels(* 0.10 ** 0.05 *** 0.010) replace label ///
    keep(placebo)
	
	
	

/*============================
= (7) Mechanism: Roads Path =
============================*/

use "final_fao_data.dta", clear

foreach v of varlist km_to_*_road* {
    gen d_`v' = (`v' < 4.5 & !missing(`v'))
}

label var d_km_to_paved_road2000 "Paved road dummy in 2000"
label var d_km_to_paved_road1960 "Paved road dummy in 1960"

* First-stage: road development
eststo clear
eststo I:   reghdfe d_km_to_paved_road2000 rail_dummy d_km_to_paved_road1960, a(adm2_code year) cluster(adm2_code)
eststo II:  reghdfe d_km_to_paved_road2000 rail_dummy d_km_to_paved_road1960 $coord $controls, a(adm2_code year) cluster(adm2_code)
eststo III: reghdfe d_km_to_paved_road2000 rail_dummy d_km_to_paved_road1960 $coord $controls, a(tribe_country year) cluster(adm2_code)

esttab I II III using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs title(Path I: Infrastructure) ///
    s(Controls lat District Tribe Year N r2, ///
    label("Controls" "Lat & Long" "District fixed effects" ///
    "Tribe x Country fixed effects" "Year fixed effects" "N" "R-squared")) ///
    starlevels(* 0.10 ** 0.05 *** 0.010) append label keep(rail_dummy)

	
	
* Second-stage: railroad effect response to mechanisms
eststo clear
eststo I:   reghdfe ln_real_yield rail_dummy, a(adm2_code year) cluster(adm2_code)
eststo II:  reghdfe ln_real_yield rail_dummy d_km_to_paved_road2000, a(adm2_code year) cluster(adm2_code)
*eststo III: reghdfe ln_real_yield rail_dummy d_km_to_paved_road2000 cash_crop, a(adm2_code year) cluster(adm2_code)
eststo IV:  reghdfe ln_real_yield rail_dummy d_km_to_paved_road2000 cash_crop share_allocated, a(adm2_code year) cluster(adm2_code)
eststo V:  reghdfe ln_real_yield rail_dummy d_km_to_paved_road2000 cash_crop share_allocated nl_intensity km_to_city_2010, a(adm2_code year) cluster(adm2_code)

esttab I II IV V using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs title(Mechanisms) ///
    s(District Tribe Year N r2, ///
    label("District fixed effects" "Tribe x Country fixed effects" ///
    "Year fixed effects" "N" "R-squared")) ///
    starlevels(* 0.10 ** 0.05 *** 0.010) append label drop(_cons)

	
	
	
/*===============================
= Balance Check: Model (1) =
===============================*/

use "final_fao_data.dta", clear
keep if year == 2010

* Covariate balance (other characteristics)
eststo clear
foreach v of varlist class1 class2 alt_mean prec_mean {
    eststo I_`v': reghdfe `v' rail_dummy, a(adm2_code year) cluster(adm2_code)
}

* Join Potential Yield
replace latitude = lat1
replace longitude = long1
joinby latitude longitude using "FAO_yield/Potential Yield/potential_yield_all.dta", unmatched(none)
keep if year == 2010
label var p_yield "Potential yield"

eststo p_yield: reghdfe p_yield rail_dummy, a(adm2_code year crop_name) cluster(adm2_code)

* Export table
esttab p_yield I_* using "Tables/Table_fao.rtf", b(4) se r2 obs ///
    title(Covariate balance check for model (1)) ///
    s(District Year crop N r2, label("District fixed effects" "Year fixed effects" "Crop fixed effect" "N" "R-squared")) ///
    starlevels(* 0.10 ** 0.05 *** 0.010) append label keep(rail_dummy)


/*===============================
= Balance Check: Model (2) =
===============================*/

use "final_fao_data.dta", clear
keep if year == 2010

* Create distance-related variables
replace km_to_actual_plbo = -1 * km_to_actual_plbo if placebo == 0
gen distance_pos = km_to_actual_plbo * placebo
egen tribe_year = group(tribe_code year)

* Covariate balance
eststo clear
foreach v of varlist class1 class2 alt_mean prec_mean {
    eststo I_`v': reghdfe `v' placebo km_to_actual_plbo distance_pos ///
        if inrange(abs(km_to_actual_plbo), 0, 20), a(tribe_country year) cluster(adm2_code)
}

* Join Potential Yield
replace latitude = lat1
replace longitude = long1
joinby latitude longitude using "FAO_yield/Potential Yield/potential_yield_all.dta", unmatched(none)
keep if year == 2010
label var p_yield "Potential yield"

eststo p_yield: reghdfe p_yield placebo km_to_actual_plbo distance_pos ///
    if inrange(abs(km_to_actual_plbo), 0, 20), a(tribe_country year crop_name) cluster(adm2_code)

* Export table
esttab p_yield I_* using "Tables/Table_fao.rtf", b(4) se r2 obs ///
    title(Covariate balance check for model (2)) ///
    s(District Crop Year N r2, label("District fixed effects" "Crop fixed effects" "Year fixed effects" "N" "R-squared")) starlevels(* 0.10 ** 0.05 *** 0.010) append label keep(placebo)

	
	

/*=======================================
= Heterogeneity: Country-by-Subsample =
=======================================*/

use "final_fao_data.dta", clear

tempname coef_storage
tempfile coef_file
capture postutil clear
postfile `coef_storage' str20 State state_coeff using `coef_file'

levelsof adm0_name, local(states)
foreach state in `states' {
    reghdfe ln_real_yield rail_dummy if adm0_name ~= `"`state'"', $a cluster(adm2_code)
    matrix b = e(b)
    scalar state_coeff = b[1, "rail_dummy"]
    post `coef_storage' ("`state'") (state_coeff)
}
postclose `coef_storage'
use `coef_file', clear

twoway (kdensity state_coeff, lcolor(blue) lwidth(medthick)) ///
    , xline(0.0162, lpattern(dash) lcolor(gs12)) ///
    ytitle("Density") xtitle("Coefficient Estimate") ///
    legend(off) graphregion(color(white)) plotregion(color(white))


/*========================================
= Heterogeneity: Regional Subsamples =
========================================*/

use "final_fao_data.dta", clear

* Step 1: Loop over regions, drop one at a time
eststo clear
levelsof region, local(regions)

local i = 1
foreach r of local regions {
    local region_label = "`r'"
    
    * Store with safe short name (R1, R2, etc.)
    eststo R`i': reghdfe ln_real_yield rail_dummy if region != `"`region_label'"', ///
        $a cluster(adm2_code)
    
    local ++i
}

* Step 2: Export to RTF table
esttab R* using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs label append ///
    title("Rail Effect: Excluding One Region at a Time") ///
	stats(District Year N r2, ///
    labels("District fixed effects" "Year fixed effects" "N" "R-squared")) ///
	mtitle("Eastern Africa" "Middle Africa" "Southern Africa" "Western Africa") ///
    starlevels(* 0.10 ** 0.05 *** 0.010) ///
    keep(rail_dummy)

	

	
	

/*=======================================================
= Heterogeneity by Colonial History (Direct vs Indirect) =
=======================================================*/

use "final_fao_data.dta", clear
do "rename_colonies.do"
keep if !mi(rail_dummy)

tab colonizer_identity
gen col_dummy = 0
replace col_dummy = 1 if colonizer_identity == "France"
replace col_dummy = 2 if colonizer_identity == "United Kingdom"

eststo clear
foreach m in I II III {
    if "`m'" == "I" {
        eststo I: reghdfe ln_real_yield rail_dummy i.rail_dummy##i.col_dummy, ///
            a(adm2_code year) cluster(adm2_code)
    }
    else if "`m'" == "II" {
        eststo II: reghdfe ln_real_yield rail_dummy i.rail_dummy##i.col_dummy ///
            $coord $controls, a(adm2_code year) cluster(adm2_code)
    }
    else {
        eststo III: reghdfe ln_real_yield rail_dummy i.rail_dummy##i.col_dummy ///
            $coord $controls, a(tribe_country year) cluster(adm2_code)
    }

    lincom 1.rail_dummy#2.col_dummy - 1.rail_dummy#1.col_dummy
    estadd scalar diff = r(estimate)
    estadd scalar diff_se = r(se)
    estadd scalar diff_p = r(p)
}

esttab I II III using "Tables/Table_fao.rtf", b(4) se r2 obs ///
    stats(diff diff_se Controls District Tribe_country Year N r2, ///
    labels("British - French Diff" "SE (Diff)" "Controls" "District fixed effects" "Tribe x Country fixed effects" "Year fixed effects" "N" "R-squared")) ///
    title("Effect of Rail by Colonial Origin") starlevels(* 0.10 ** 0.05 *** 0.010) ///
    append label keep(rail_dummy 1.rail_dummy#2.col_dummy 1.rail_dummy#1.col_dummy)


	
	
	
	
/*============================
= Summary Statistics =
============================*/

*--- Panel A: Main Variables ---*
use "final_fao_data.dta", clear
drop if mi(km_to_all_rail)

eststo clear
estpost tabstat real_yield km_to_all_rail rail_dummy ///
    km_to_impvd_road1960 km_to_paved_road2000 km_to_city_2010 share_allocated nl_intensity km_to_city_2010 ///
    evi $controls, stat(mean min p10 p50 p90 max sd) columns(s)

esttab using "Tables/Table_descriptive_statistics.rtf", replace ///
    cells("mean(fmt(2) label(Mean)) min(fmt(2) label(Min)) p10(fmt(2) label(P10)) p50(fmt(2) label(Median)) p90(fmt(2) label(P90)) max(fmt(2) label(Max)) sd(fmt(2) label(Std. Dev.))")  nostar unstack nomtitles label width(5.3in) ///
    title("Table 1: Descriptive Statistics")


*--- Panel B: Placebo ---*
use "final_fao_data.dta", clear
drop if mi(km_to_actual_plbo)

eststo clear
estpost tabstat placebo, stat(mean min p10 p50 p90 max sd) columns(s)

esttab using "Tables/Table_descriptive_statistics.rtf", append ///
    cells("mean(fmt(2) label(Mean)) min(fmt(2) label(Min)) p10(fmt(2) label(P10)) p50(fmt(2) label(Median)) p90(fmt(2) label(P90)) max(fmt(2) label(Max)) sd(fmt(2) label(Std. Dev.))") nostar unstack nomtitles label width(5.3in)


*--- Panel C: Rail Density in Contiguous Pairs ---*
use "Road/rail_district_level_data.dta", clear
joinby adm0_code adm1_code adm2_code using "Grid_11km/district_pair_fao_cleaned", unmatched(none)

bysort dist_pair_id: egen max = max(rail_density)
bysort dist_pair_id: egen min = min(rail_density)
gen complet = (max != min)

eststo clear
estpost tabstat rail_density if complet == 1, stat(mean min p10 p50 p90 max sd) columns(s)

esttab using "Tables/Table_descriptive_statistics.rtf", append ///
    cells("mean(fmt(2) label(Mean)) min(fmt(2) label(Min)) p10(fmt(2) label(P10)) p50(fmt(2) label(Median)) p90(fmt(2) label(P90)) max(fmt(2) label(Max)) sd(fmt(2) label(Std. Dev.))") nostar unstack nomtitles label width(5.3in)

	
	
	
/*
use "final_fao_data.dta", clear
keep if placebo == 1
gen placebo_placebo = (km_to_actual_plbo <= 25) if km_to_actual_plb ~= . 

eststo clear
eststo I:   reghdfe ln_real_yield placebo_placebo, $a cluster(adm2_code)
eststo II:  reghdfe ln_real_yield placebo_placebo $controls, $a cluster(adm2_code)
eststo III: reghdfe ln_real_yield placebo_placebo $coord $controls, $a cluster(adm2_code)
eststo IV:  reghdfe ln_real_yield placebo_placebo $coord $controls, $b cluster(adm2_code)
		
esttab I II using "Tables/Table_fao.rtf", ///
    b(4) se r2 obs title(Placebo vs Actual) ///
    s(Controls lat_long Tribe_vcount Year N r2, ///
    label("Controls" "Lat & Long" "Tribe x Country fixed effects" ///
    "Year fixed effects" "N" "R-squared")) ///
    starlevels(* 0.10 ** 0.05 *** 0.010) replace label ///
    keep(placebo_placebo)

  