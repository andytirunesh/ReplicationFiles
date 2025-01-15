		// Directory setup based on username
		if c(username) == "aube" {
		cd "D:\Project\Climate change and labor use\weather_data\Nigeria"
		} 
		else {
			cd "E:\Nigeria"
			*Set number of processors for computations
			*set processors 2
		}

		
		
		*---------------------------------------------------------
		** Temperature and Farm Labor in the Tropics
		
		*  Replication of Figures and Tables
		*---------------------------------------------------------

		
		*---------------------------------------------------------
		* Analysis of Degree Days
		*---------------------------------------------------------
		use "final_main_data.dta", clear
		

		* Define variables for regression analysis
		global xlist HDD GDD precip pre_square farm_area fhh hh_members

		* Clear previous estimations
		eststo clear

		* Estimate regressions for labor types
		foreach v in labor_hired labor_family hired_share {
			reghdfe `v' $xlist, a(year ea) cluster(ea)
			estimates store `v'
		}

		* Export results to a table
		esttab labor_hired labor_family hired_share using "Table.rtf", b(4) se r2 obs title(Table 2) ///
			mtitles("Hired labor" "Family labor" "Hired labor share") ///
			s(Controls Year Zone N r2, label("Additional controls" "Year fixed effects" "Zone fixed effects" "N" "R-squared")) ///
			starlevels(* 0.10 ** 0.05 *** 0.010) replace label keep(HDD* GDD*)

			
			
		*---------------------------------------------------------
		* Equidistance Bin
		*---------------------------------------------------------

		use "final_main_data.dta", clear

		* Define variables for regression analysis
		global xlist c_a34 c_31_34 c_25_28 c_b25 precip pre_square farm_area fhh hh_members 

		* Clear previous estimations
		eststo clear

		* Estimate regressions for labor types
		foreach v in labor_hired labor_family hired_share {
			reghdfe `v' $xlist, a(year ea) cluster(ea)
			estimates store `v'
		}

		* Export results to a table
		esttab labor_hired labor_family hired_share using "Table.rtf", b(4) se r2 obs title(Table 3) ///
			mtitles("Hired labor" "Family labor" "Hired labor share") ///
			s(Controls Year Zone N r2, label("Additional controls" "Year fixed effects" "Zone fixed effects" "N" "R-squared")) ///
			starlevels(* 0.10 ** 0.05 *** 0.010) append label keep(c_*)

			
			
		*---------------------------------------------------------
		* Local Labor Market Effects
		*---------------------------------------------------------
		use "final_main_data.dta", clear
		global xlist HDD GDD precip pre_square farm_area fhh hh_members  

		* Collapse data for aggregation
		collapse (sum) total_hrs labor_total (mean) farm_area fhh hh_members  (first) average_wage HDD GDD precip pre_square (first) zone state, by(year ea)

		* Estimate regressions
		reghdfe total_hrs HDD GDD precip pre_square , a(year ea) cluster(ea)
		estimates store a

		reghdfe total_hrs $xlist, a(year ea) cluster(ea)
		estimates store b

		* Export results to a table
		esttab a b using "Table.rtf", b(4) se r2 obs title(Table 4) ///
			mtitles("Total Non-farm labor" "Total Non-farm labor") ///
			s(Year Zone N r2, label("Year fixed effects" "Zone fixed effects" "N" "R-squared")) ///
			starlevels(* 0.10 ** 0.05 *** 0.010) append label keep(HDD GDD)

			
			
		*---------------------------------------------------------
		* Mechanisms: Crop Productivity and Wage Effects
		*---------------------------------------------------------
		use "final_main_data.dta", clear
		
		* Crop Productivity
		foreach v in yield_hv_maize yield_hv_rice yield_hv_wheat yield_hv_sorgum yield_hv_millet yield_hv_cowpea ///
		yield_hv_grdnt yield_hv_yam yield_hv_swtptt yield_hv_cassav yield_hv_banana yield_hv_cocoa yield_hv_soy ///
		yield_hv_beans {
			rename `v' y_`v'
		}

		reshape long y_yield_hv_, i(hhid year) j(crop) string
		encode crop, gen(crop_id)

		* Filter to major crops and save data
		keep if crop == "cassav" | crop == "yam" | crop == "sorgum" | crop == "maiz" | crop == "millet" | crop == "rice" | crop == "cowpea"
		save "yield_data", replace

		* Regressions for productivity 
		use "yield_data", clear
		gen ln_yield = ln(y_yield_hv_ + 0.001)
		global xlist HDD GDD precip pre_square labor_family labor_hired fhh hh_members 

		eststo clear
		eststo I: reghdfe ln_yield $xlist, absorb(ea crop_id year) cluster(ea)
		eststo II: reghdfe ln_yield $xlist, absorb(ea year) cluster(ea)

		
		* Wage regressions
		use "final_main_data.dta", clear
		egen zone_year = group(year state)
		collapse (max) HDD GDD precip pre_square average_wage zone state lga zone_year (mean) farm_area hh_members, by(ea year)
		global xlist HDD GDD precip pre_square

		eststo III: reghdfe average_wage $xlist, absorb(yea ea) cluster(ea)
		eststo IV: reghdfe average_wage $xlist farm_area hh_members, absorb(yea ea) cluster(ea)

		* Export results to a table
		esttab I II III IV using "Table.rtf", b(4) se r2 obs title(Table 5: Mechanisms) ///
			mtitles("Productivity(Yield)" "Productivity (TFP)" "Wage rate" "Wage rate") ///
			s(Controls Crop Year Zone N r2, label("Additional controls" "Crop fixed effects" "Year fixed effects" "Zone fixed effects" "N" "R-squared")) ///
			starlevels(* 0.10 ** 0.05 *** 0.010) append label keep(HDD GDD labor_family labor_hired)

			
			
		*---------------------------------------------------------
		* Robustness Checks
		*---------------------------------------------------------
		
		
		* Alternative bins
		use "final_main_data.dta", clear

		
		global xlist HDD_35 GDD_35 precip pre_square farm_area fhh hh_members

		eststo clear
		foreach v in labor_hired labor_family hired_share {
			reghdfe `v' $xlist, a(year ea) cluster(ea)
			estimates store `v'
		}

		* Export alternative bin results
		esttab labor_hired labor_family hired_share using "Table.rtf", b(4) se r2 obs title(Table A2) ///
			mtitles("Hired labor" "Family labor" "Hired labor share") ///
			s(Controls Year Zone N r2, label("Additional controls" "Year fixed effects" "Zone fixed effects" "N" "R-squared")) ///
			starlevels(* 0.10 ** 0.05 *** 0.010) append label keep(HDD* GDD*)
		
		
		* No Controls
		use "final_main_data.dta", clear

		* Define variables for regression analysis without controls
		global xlist HDD GDD precip pre_square //farm_area average_wage fhh hh_members
		global cv ea

		* Clear previous estimations
		eststo clear

		* Estimate regressions for labor types without controls
		foreach v in labor_hired labor_family hired_share {
			reghdfe `v' $xlist, a(year ea) cluster(ea)
			estimates store `v'
		}

		* Export results to a table
		esttab labor_hired labor_family hired_share using "Table.rtf", b(4) se r2 obs title(Table A3) ///
			mtitles("Hired labor" "Family labor" "Hired labor share") ///
			s(Controls Year Zone N r2, label("Additional controls" "Year fixed effects" "Zone fixed effects" "N" "R-squared")) ///
			starlevels(* 0.10 ** 0.05 *** 0.010) append label keep(HDD* GDD*)

			
			
		* Household Fixed Effects
		use "final_main_data.dta", clear

		* Define variables for regression analysis with household fixed effects
		global xlist HDD GDD precip pre_square farm_area average_wage fhh hh_members
		global cv ea

		* Clear previous estimations
		eststo clear

		* Estimate regressions for labor types with household fixed effects
		foreach v in labor_hired labor_family hired_share {
			reghdfe `v' $xlist, a(year hhid) cluster(ea)
			estimates store `v'
		}

		* Export results to a table
		esttab labor_hired labor_family hired_share using "Table.rtf", b(4) se r2 obs title(Table A3) ///
			mtitles("Hired labor" "Family labor" "Hired labor share") ///
			s(Controls Year Zone N r2, label("Additional controls" "Year fixed effects" "Zone fixed effects" "N" "R-squared")) ///
			starlevels(* 0.10 ** 0.05 *** 0.010) replace label keep(HDD* GDD*)


		*---------------------------------------------------------
		* End of Code
		*---------------------------------------------------------
