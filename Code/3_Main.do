*** MAIN

*********************************************************
* This is the master stata script
*********************************************************

****************************
***         PATHS        ***
****************************

set more off 

* Set the paths
global raw_data "path\of\tour\data\directory"
global dofiles "path\of\your\scripts\directory"
global stat_desc "path\of\your\stat\desc\directory"
global DDD "path\of\your\ddd\directory"
global ES "path\of\your\event\studies\directory"



****************************
***       ANALYSIS       ***
****************************

* Executes all the scripts
do $dofiles/4_Stat_desc
do $dofiles/5_Thresholds
do $dofiles/6_DDD
do $dofiles/7_ES
do $dofiles/8_Static_panel

exit



