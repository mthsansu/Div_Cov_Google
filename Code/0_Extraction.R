#################################################################################################
##################################   Internship INED - Divorce   ################################
##################################             Sansu             ################################              
#################################################################################################

#################################################################################################
################################## Almost all the following code ################################
##################################       has been done by        ################################
##################################        Giulia Ferrari         ################################
#################################################################################################

#################################################################################################
##################################       Packages imports        ################################
#################################################################################################

install.packages("dplyr")
library(dplyr)
install.packages("haven")
library(haven)
install.packages("gtrendsR")
library(gtrendsR)
install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)

#################################################################################################
##################################     Environment cleaning      ################################
#################################################################################################

#rm(list=ls())

#################################################################################################
##################################      Working Environment      ################################
#################################################################################################

# Specify the paths
paths <- c("C:\\Users\\Mathis\\Dropbox\\Divorce\\Data\\Gtrends","C:\\Users\\sansu_mat\\Dropbox\\Divorce\\Data\\Gtrends","C:\\Users\\Mathis\\Desktop\\Divorce\\Data\\Gtrends", "XXXXX", "XXXXX")

# Choose the appropriate path
# 1 = Mathis, Dropbox personal computer
# 2 = Mathis, Dropbox INED computer
# 3 = Mathis, Local personal computer
# 4 = Anne
# 5 = Marion

pathwd <- paths[3]

# Set the working directory
setwd(pathwd)

# Verify the working directory so that it's right 
getwd()

# Options setting : set the significant digits to avoid any scientific scripture
options(scipen = 1000000000)

#################################################################################################
##################################           Extraction          ################################
#################################################################################################

# Because Google Trends doesn't allow too much automatic extraction,
# we need to extract the data in several times. No more than 200 series each time.

# Create a dataframe with two columns: 1) States 2) Language spoken
data_states <- data.frame(
	geo = c('US-AL','US-AK','US-AZ','US-AR','US-CA','US-CO','US-CT','US-DE',
    			'US-DC','US-FL','US-GA','US-HI','US-ID','US-IL','US-IN','US-IA',
			'US-KS','US-KY','US-LA','US-ME','US-MD','US-MA','US-MI','US-MN',
			'US-MS','US-MO','US-MT','US-NE','US-NV','US-NH','US-NJ','US-NM',
			'US-NY','US-NC','US-ND','US-OH','US-OK','US-OR','US-PA','US-RI',
			'US-SC','US-SD','US-TN','US-TX','US-UT','US-VT','US-VA','US-WA',
			'US-WV','US-WI','US-WY'
),
	lang = c('en-us','en-us','en-us','en-us','en-us','en-us','en-us','en-us',
			'en-us','en-us','en-us','en-us','en-us','en-us','en-us','en-us',
			'en-us','en-us','en-us','en-us','en-us','en-us','en-us','en-us',
			'en-us','en-us','en-us','en-us','en-us','en-us','en-us','en-us',
			'en-us','en-us','en-us','en-us','en-us','en-us','en-us','en-us',
			'en-us','en-us','en-us','en-us','en-us','en-us','en-us','en-us',
			'en-us','en-us','en-us'
)
)

# Create a data.frame where each row correponds to a language, and each column to a specific keyword.
# Each cell will report the translation of a keyword in a language.

data_keywords <- data.frame(
	lang = c('en-us'),
	#div=c("divorce - kardashian - blake - katie - jon - evans - mccartney - britney - heidi"),
	#div_cov=c("divorce coronavirus"),
	#div_file=c("divorce file - kardashian - katie - jon - obama"),
	#div_papers=c("divorce papers - kardashian - katie - jon - obama"),
	#div_legal=c("divorce court + divorce lawyer + divorce lawyers + divorce attorney + divorce legal + divorce law"),
	#div_lawyer=c("divorce lawyer + divorce lawyers"),
	#div_court=c("divorce court + divorce attorney"),
	#div_law=c("divorce law + divorce laws + divorce legal"),
	#lock=c("lockdown - love - six - r6"),
	div_cov_media=c("divorce - kardashian - blake - katie - jon - evans - mccartney - britney - coronavirus")#,
	#alimony=c("alimony + spousal support"),
	#child_support=c("child support"),
	#child_custody=c("child custody"),
	#div_how=c("how divorce"),
	#div_much=c("how much divorce"),
	#div_long=c("how long divorce")
)

# Left join the two datasets
data_extraction <- data_states %>% left_join(data_keywords)
#View(data_extraction)

# Iterate in order to have a .csv file for each country_timeframe_keyword
for(s in 1:(dim(data_extraction)[1])){
	keywords <- data_extraction[s,] %>% dplyr::select(-geo,-lang) %>% unlist()
	state_code <- data_extraction[s,1] 

	for(k in 1:length(keywords)){
	# Extraction function for Google Trends
      series <- gtrends(keyword = as.vector(keywords[k]),geo = state_code,time = "2004-01-01 2020-10-31",gprop = "web",onlyInterest= TRUE)
	# export the .csv file
      write.csv(series$interest_over_time[,c("date","hits")],file=paste0(gsub("-","_",state_code),"_",names(keywords[k]),".csv"),
	row.names = F) 
	}
}



