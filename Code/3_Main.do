*** MAIN

****************************
***         PATHS        ***
****************************

* 1 = Mathis, Dropbox personal computer
* 2 = Mathis, Margaux INED computer
* 3 = Mathis, Local personal computer
* 4 = Anne
* 5 = Marion 

set more off 
scalar user_indiv = 3

if (user_indiv == 1) {
global raw_data "C:\Users\Mathis\Dropbox\Divorce\Data\Database"
global results_ES "C:\Users\Mathis\Dropbox\Divorce\Results\Event_Study_Analysis"
global results_DID1 "C:\Users\Mathis\Dropbox\Divorce\Results\DID1"
global results_DID2 "C:\Users\Mathis\Dropbox\Divorce\Results\DID2"
global dofiles "C:\Users\Mathis\Dropbox\Divorce\Code"
global stat_desc "C:\Users\Mathis\Dropbox\Divorce\Stat_desc"
global DDD "C:\Users\Mathis\Dropbox\Divorce\DDD"
global ES "C:\Users\Mathis\Dropbox\Divorce\ES"
}

if (user_indiv == 2) {
global raw_data "/home/users/sansu_mat/Divorce/Data/Database"
global results_ES "/home/users/sansu_mat/Divorce/Results/Event_Study_Analysis"
global results_DID1 "/home/users/sansu_mat/Divorce/Results/DID1"
global results_DID2 "/home/users/sansu_mat/Divorce/Results/DID2"
global dofiles "/home/users/sansu_mat/Divorce/Code"
global stat_desc "/home/users/sansu_mat/Divorce/Stat_desc"
global DDD "/home/users/sansu_mat/Divorce/DDD"
global ES "/home/users/sansu_mat/Divorce/ES"
}

if (user_indiv == 3) {
global raw_data "C:\Users\Mathis\Desktop\Ined\Divorce\Data\Database"
global results_ES "C:\Users\Mathis\Desktop\Ined\Divorce\Results\Event_Study_Analysis"
global results_DID1 "C:\Users\Mathis\Desktop\Ined\Divorce\Results\DID1"
global results_DID2 "C:\Users\Mathis\Desktop\Ined\Divorce\Results\DID2"
global dofiles "C:\Users\Mathis\Desktop\Ined\Divorce\Code"
global stat_desc "C:\Users\Mathis\Desktop\Ined\Divorce\Stat_desc"
global DDD "C:\Users\Mathis\Desktop\Ined\Divorce\DDD"
global ES "C:\Users\Mathis\Desktop\Ined\Divorce\ES"
}

if (user_indiv == 4) {
global raw_data XXXX
global results_ES XXXX
global results_DID1 XXXX
global results_DID2 XXXX
global dofiles XXXXX
global stat_desc XXXXX
global DDD XXXXX
global ES XXXXX
}

if (user_indiv == 5) {
global raw_data XXXX
global results_ES XXXX
global results_DID1 XXXX
global results_DID2 XXXX
global dofiles XXXXX
global stat_desc XXXXX
global DDD XXXXX
global ES XXXXX
}


****************************
***       ANALYSIS       ***
****************************

*do $dofiles/4_Stat_desc
*do $dofiles/5_Thresholds
*do $dofiles/6_DDD
*do $dofiles/7_ES
do $dofiles/8_Static_panel


*do $dofiles/5_Event_Study
*do $dofiles/6_DID1
*do $dofiles/7_DID2

exit



