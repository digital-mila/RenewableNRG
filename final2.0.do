*********************************************************
* ECON 490 011 - Econometric Files for Final Paper

* By: Milena Kolesnikova, Aurelia Gracia & Melissa Phua 

/* Research Question: How does renewable energy consumption 
influence the long-run economic growth of G20 countries, and
how does urbanization  modulate this relationship?
*/
*********************************************************

*Step 1: Command to open a new log file
clear *
log using final, text replace

*Step 2: Import the Penn World Tables and merge this dataset with datasets from the World Bank Open Data
/*wbopendata: 
- Renewable energy consumption (% of total final energy consumption)
- Urban population (% of total population)
*/
ssc install wbopendata
wbopendata, language(en - English) country() topics() indicator(EG.FEC.RNEW.ZS; SP.URB.TOTL.IN.ZS) long clear

use pwt100.dta, clear
merge 1:1 countrycode year using  wb_final.dta,

*Step 3: Drop the observations that did not merge
drop if _merge!=3 
/* 7,968 observations were deleted because they did not have a match in both datasets. In a 1:1 merge, only the pairs of observations that have the same key values in both datasets are retained in the merged file. Observations that were "not matched," i.e. dropped from the final merged dataset:

2,010 observations were from the PWT dataset with no corresponding entries in the World Bank dataset.
5,958 observations were from the World Bank dataset with no corresponding entries in the PWT dataset.
*/

*Step 4: Keep the following variables
keep countrycode country year pop rgdpe csh_i hc eg_fec_rnew_zs sp_urb_totl_in_zs

rename eg_fec_rnew_zs clean_nrg
rename sp_urb_totl_in_zs urbanization

gen keep = 0
foreach c in "Argentina" "Australia" "Brazil" "Canada" "China" "France" "Germany" "India" "Indonesia" "Italy" "Japan" "Mexico" "Russia" "South Africa" "South Korea" "Turkey" "United Kingdom" "United States" "Austria" "Belgium" "Bulgaria" "Croatia" "Republic of Cyprus" "Czech Republic" "Denmark" "Estonia" "Finland" "Greece" "Hungary" "Ireland" "Latvia" "Lithuania" "Luxembourg" "Malta" "Netherlands" "Poland" "Portugal" "Romania" "Slovakia" "Slovenia" "Spain" "Sweden"{
    replace keep = 1 if country == "`c'"
}
keep if keep == 1
drop keep // filter G20 countries, but excluding Saudi Arabia because their renewable energy consumption is negligible 

keep if year > 1989 // no data for renewable energy consumption prior to 1990

*Step 5: Summary statistics table for simple regression variables
ssc install asdoc
asdoc summarize rgdpe pop csh_i hc clean_nrg if year == 2010 & !missing(clean_nrg), ///
     stat(N mean sd min max) title(Table I: Snapshot Summary Statistics in Year 2010) replace
	 
*Step 6: Generate logged versions of the dependent and explanatory variables for the long-run growth accounting regression
encode countrycode, gen(ccode) 
label var ccode "Numeric code that represents the country"
xtset ccode year, yearly

gen lngdp=ln(rgdpe/pop)
gen lnpop=ln(ln(pop)-ln(L1.pop))
gen dlngdp=ln(rgdpe/pop) - ln(L1.rgdpe/L1.pop)
gen lnsave=ln(csh_i)
gen lnhc=ln(hc)
gen lnclean=ln(L1.clean_nrg)
gen lnurb= ln(urbanization)

*Step 7: Year and Panel fixed effects test
xtreg dlngdp L1.lngdp lnsave lnhc lnpop lnclean i.year, fe
testparm i.year
xtreg dlngdp L1.lngdp lnsave lnhc lnpop lnclean i.ccode, fe
*testparm i.ccode // the specified varlist does not identify any testable coefficients

/* The test results show that both country and year fixed effects are statistically significant in explaining the variation in the dependent variable, GDP growth. The significance of country fixed effects suggests that unobserved country-specific factors are important, while the significance of year fixed effects implies that common temporal shocks or trends also play a crucial role.
*/

*Step 8: Heteroskedasticity test
xttest3
/* Result indicates that the variability or spread of the error terms (i.e. unexplained factors in model) is different for each country. This finding suggests that each country has unique characteristics influencing its economic growth rate that are not consistent across all countries in the sample.
*/

*Step 9: Serial correlation test
search xtserial
xtserial dlngdp lnsave lnhc lnpop lnclean
/*Since the p-value (Prob > F) is 0.0000, we reject the null hypothesis, suggesting there is significant first-order autocorrelation in our panel data.
*/

scatter dlngdp lnclean || lowess dlngdp lnclean // linearity check

*Step 10: Run single properly specified regression with all variables included
xtpcse dlngdp L1.lngdp lnsave lnhc lnpop c.lnclean i.year i.ccode, het corr(ar1)
estimates store single

*Step 11: Winsorize variables and run winsorized simple regression
/*sum lngdp, d
gen lngdp_winsor = lngdp
replace lngdp_winsor = r(p1) if lngdp_winsor<r(p1) & lngdp_winsor!=.
replace lngdp_winsor = r(p99) if lngdp_winsor>r(p99) & lngdp_winsor!=.

sum lnpop, d
gen lnpop_winsor = lnpop
replace lnpop_winsor = r(p1) if lnpop_winsor<r(p1) & lnpop_winsor!=.
replace lnpop_winsor = r(p99) if lnpop_winsor>r(p99) & lnpop_winsor!=.

sum dlngdp, d
gen dlngdp_winsor = lngdp
replace dlngdp_winsor = r(p1) if dlngdp_winsor<r(p1) & dlngdp_winsor!=.
replace dlngdp_winsor = r(p99) if dlngdp_winsor>r(p99) & dlngdp_winsor!=.

sum lnsave, d
gen lnsave_winsor = lngdp
replace lnsave_winsor = r(p1) if lnsave_winsor<r(p1) & lnsave_winsor!=.
replace lnsave_winsor = r(p99) if lnsave_winsor>r(p99) & lnsave_winsor!=.

sum lnhc, d
gen lnhc_winsor = lngdp
replace lnhc_winsor = r(p1) if lnhc_winsor<r(p1) & lnhc_winsor!=.
replace lnhc_winsor = r(p99) if lnhc_winsor>r(p99) & lnhc_winsor!=.

sum lnclean, d
gen lnclean_winsor = lngdp
replace lnclean_winsor = r(p1) if lnclean_winsor<r(p1) & lnclean_winsor!=.
replace lnclean_winsor = r(p99) if lnclean_winsor>r(p99) & lnclean_winsor!=.

xtpcse dlngdp_winsor L1.lngdp_winsor lnsave_winsor lnhc_winsor lnpop_winsor c.lnclean_winsor i.year i.ccode, het corr(ar1)
estimates store model_winsor
*/ // insufficient observations, i.e. will not be included in analysis

*Step 12: Run regression with interaction between clean energy consumption & countries level of investment
xtpcse dlngdp L1.lngdp c.lnsave lnhc lnpop c.lnclean c.lnclean##c.lnsave i.year i.ccode, het corr(ar1)
estimates store interaction

predict yhat, xb
twoway (scatter yhat lnclean) (lfit yhat lnclean)

drop if lnclean==.
ssc install xtgcause
xtgcause dlngdp lnclean
/* For at least one G20 panel, the Dumitrescu & Hurlin Granger non-causality test results strongly reject the null hypothesis that renewable energy consumption (lnclean) does not Granger-cause GDP per capita growth (dlngdp). The test statistics—W-bar at 1.9771, Z-bar at 4.3146 with a p-value of 0.0000, and Z-bar tilde at 3.3826 with a 0.0007—indicate that renewable energy use can forecast economic development. In the panels studied, renewable energy consumption causes economic growth, suggesting that renewable energy policy may affect economic performance. This causal association suggests that renewable energy policies may affect economic growth and provide a solid foundation for sustainable energy-based economic development plans. 
*/ 

*Step 13: Subset specifications 
sum lnurb
local lnurb_mean=r(mean)
gen lowurb=0
replace lowurb=1 if lnurb<=`lnurb_mean'
sum lowurb

*Step 14: Run regressions with subset specifications
xtpcse dlngdp L1.lngdp lnsave lnhc lnpop c.lnclean i.year i.ccode if lowurb==0, het corr(ar1)
estimates store highurb

xtpcse dlngdp L1.lngdp lnsave lnhc lnpop c.lnclean i.year i.ccode if lowurb==1, het corr(ar1)
estimates store lowurb

xtpcse dlngdp L1.lngdp lnsave lnhc lnpop c.lnclean c.lnclean##c.lnsave i.year i.ccode if lowurb==0, het corr(ar1)
estimates store highurb_int

xtpcse dlngdp L1.lngdp lnsave lnhc lnpop c.lnclean c.lnclean##c.lnsave i.year i.ccode if lowurb==1, het corr(ar1)
estimates store lowurb_int


*Step 15: Display and export output
etable, estimates(single interaction) mstat(N) mstat(r2) showstars showstarsnote varlabel column(dvlabel) title("Table II: Fixed Effects Multiple Regression Results") keep(dlngdp L1.lngdp lnhc lnpop c.lnclean##c.lnsave) export(tableII.docx, replace)

etable, estimates(highurb lowurb highurb_int lowurb_int) mstat(N) mstat(r2) showstars showstarsnote varlabel column(dvlabel) title("Table III: Alternative Regressions with Urbanization Level Subsets") keep(dlngdp L1.lngdp lnsave lnhc lnpop c.lnclean##c.lnsave) export(tableIII.docx, replace)

*Step 16: Graph of economic story (scatter plot)
twoway (scatter dlngdp lnclean if year==2010, color(emidblue) mlabel(ccode)) ///
                (lfit dlngdp lnclean if year==2010, color(emidblue)), ///
                xtitle("LN Renewable Energy Consumption (% of total energy consumption)", size(3)) ///
                ytitle("LN GDP per capita of G20 countries",size(3)) ///
                title("Figure I: Simple Regression of Renewable Energy Consumption vs. G20 GDP (2010)", size(3.75)) ///
				note("Data source: World Bank and Penn World Table (2010)",size(1.5))				
graph export "scatter1.png", as(png) replace
		
twoway (scatter dlngdp lnclean if year==2010 & lowurb==0, color(blue)) ///
       (lfit dlngdp lnclean if year==2010 & lowurb==0, color(blue)) ///
       (scatter dlngdp lnclean if year==2010 & lowurb==1, color(green)) ///
       (lfit dlngdp lnclean if year==2010 & lowurb==1, color(green)), ///
       legend(label(1 "High Urbanization") label(2 "Fitted High Urb.") ///
              label(3 "Low Urbanization") label(4 "Fitted Low Urb.")) ///
       xtitle("LN Renewable Energy Consumption (% of total energy consumption)", size(3)) ///
       ytitle("LN GDP per capita of G20 countries", size(3)) ///
       title("Figure II: Renewable Energy Consumption vs. G20 GDP w/ Urbanization Subsets (2010)", size(3.25)) ///
	   note("Data source: World Bank and Penn World Table (2010)",size(1.5))
graph export "scatter2.png", as(png) replace

*Step 17: Histograms of LN Clean Energy Consumption Distributions
twoway (histogram lnclean, color(ltblue) lcolor(black)),     ///
     aspectratio(1)                                              ///
     title("LN Renewable Energy Consumption Total Distribution", size(3)) ///
	  name("R1", replace) ///
	  note("Data source: World Bank",size(1.5))
graph export hist1.png, as(png) replace
	
twoway (histogram lnclean if lowurb==0, color(blue) lcolor(black)) ///
       (histogram lnclean if lowurb==1, color(green) lcolor(black)), ///
       aspectratio(1) legend(label(1 "High Urbanization") label(2 "Low Urbanization")) ///
       title("Combined Histogram of LN Renewable Energy Consumption", size(3)) ///
	   name("R2", replace) ///
	   note("Data source: World Bank",size(1.5))
graph export hist2.png, as(png) replace

capture log close
		
/* Works Cited:

Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182, available for download at www.ggdc.net/pwt

World Bank. "CO2 emissions (metric tons per capita)." World Bank Open Data. Accessed March 25, 2024. https://data.worldbank.org/indicator/EN.ATM.CO2E.PC

World Bank. "Renewable energy consumption (% of total final energy consumption)." World Bank Open Data. Accessed March 25, 2024. https://data.worldbank.org/indicator/EG.FEC.RNEW.ZS

World Bank. "Urban population (% of total population)." World Bank Open Data. Accessed March 25, 2024. https://data.worldbank.org/indicator/SP.URB.TOTL.IN.ZS
*/
