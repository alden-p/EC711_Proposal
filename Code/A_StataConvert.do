/* Convert stata files to csv files */

clear all

set more off

cd "C:\Users\Porte\Documents\Courses\EC711\Research-Proposal\Emperics\Input"

use nsw, clear

export delimited using "nsw.csv", replace

use nsw_dw, clear

export delimited using "nsw_dw.csv", replace
