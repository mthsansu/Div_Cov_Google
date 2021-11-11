#################################################################################################
##################################  Internship INED - Divorce   ################################
##################################             Sansu             ################################              
#################################################################################################

#################################################################################################
# This script is conceived to create a unique database from the Google Trends series already
# extracted, and to couple it with ground data: population data, divorce data, Covid data, etc.
# It also pretreats the data to obtain standardized series
#################################################################################################

#################################################################################################
##################################       Packages imports        ################################
#################################################################################################

#install.packages(c("readxl","dplyr","haven","readr","data.table","zoo","FIACH","pracma","purrr","cli"))
library(readxl)
library(dplyr)
library(haven)
library(readr)
library(data.table)
library(zoo)
library(FIACH)
library(pracma)
library(purrr)
library(cli)


#################################################################################################
##################################      Working Environment      ################################
#################################################################################################

# Specify the different paths

# For the working directory
pathwd <- "path\\of\\your\\working\\directory"
# For the data imports or exports
path_data <- "path\\of\\your\\data\\directory"
# For the outputs
path_outputs <- "path\\of\\your\\output\\directory"

# Set the working directory
setwd(pathwd)

#################################################################################################
##################################            DATABASE           ################################
#################################################################################################


# States (+ District of Columbia) list
states <- c("US_AL","US_AK","US_AZ","US_AR","US_CA","US_CO","US_CT","US_DE","US_DC","US_FL","US_GA","US_HI","US_ID","US_IL","US_IN","US_IA","US_KS","US_KY","US_LA","US_ME","US_MD","US_MA","US_MI","US_MN","US_MS","US_MO","US_MT","US_NE","US_NV","US_NH","US_NJ","US_NM","US_NY","US_NC","US_ND","US_OH","US_OK","US_OR","US_PA","US_RI","US_SC","US_SD","US_TN","US_TX","US_UT","US_VT","US_VA","US_WA","US_WV","US_WI","US_WY")
# Variables list
list_var <- c("div","div_cov","div_file","div_papers","div_legal","div_lawyer","div_court","div_law","lock","div_cov_media","alimony","child_support","child_custody","div_how","div_much","div_long")
# Incoming normalized variables list
list_var_ref <- c(paste0(list_var,"_ref"))

##################################################
########## GTRENDS DATA STATES (monthly)
##################################################

# Function formating the .csv files to obtain a base of the combined data

data_gtrends_states <- function(codegeo,var){ # specify the state and variable to access the data stored in the .csv file
df <- read_csv(paste0(path_data,"Gtrends\\",codegeo,"_",var,".csv")) # Bring back the corresponding .csv file
# Verify that the .csv file is not empty
if (length(df) == 2){ 
	df$geo <- codegeo # Variable geo in the database
	if (is.character(df$hits) == TRUE){
	df$hits[df$hits == "<1"] <- "0"
	df$hits <- as.numeric(df$hits)}
	setnames(df,"hits",var) # Name of the Google Trends variable
	assign(paste0(codegeo,"_",var),df) # Assinging the name of the state to the reduced dataframe for this state
	return(get(paste0(codegeo,"_",var))) # Function returning the reduced dataframe 
} else { # Case where the .csv file is empty (no data extracted for these specific state and variable)
# We then create a new dataframe of the reduced form but which is empty
df <- data.frame(geo = rep(codegeo,16*12+10), # Variable of the state
		var_name = NA, # Create an empty colum for the unobserved data
		date = seq(as.Date("2004-01-01"),as.Date("2020-10-01"),by="month") # Create the corresponding column of the date
)
df$geo <- as.character(df$geo)
df$var_name <- as.numeric(df$var_name)
setnames(df,"var_name",var)
}
}

########## Joining the data together to have every state for a variable

# Function joining the reduced dataframes to obtain a base of the combined states for a variable

states_join <- function(var){ # The argument is the name of the variable
data <- data_gtrends_states(states[1],var) # Initialization with the first state on the list
for (s in 1:length(states)){ # Apply the function for each state
	data <- data %>% full_join(data_gtrends_states(states[s],var)) # Joining the reduced dataframes
}
return(data) # Function returning the complete dataframe (every state) for the specified variable
}

########## Database with each variable and each state

# Loop joining the variables dataframes to obtain a complete datatbase
 
# Initialization with the first variable
data_gtrends <- states_join(list_var[1])
# Complete with the orther variables
for (i in 2:length(list_var)){
data_gtrends <- data_gtrends %>% full_join(states_join(list_var[i]))
}

# We obtain a complete database

##################################################
########## NORMALIZATION
##################################################

# Add the columns for the normalized variables to the database
for (i in 1:length(list_var_ref)){
# The normalized variables take the figures of the raw variables
data_gtrends[,list_var_ref[i]] <- data_gtrends[,list_var[i]]}

# Normalization function by state and by variable

normalization <- function(codegeo,var_ref){ # Arguments are the state and the variable
mean_var_ref <- colMeans(subset(data_gtrends, geo==codegeo)[,var_ref]) # For a state and variable, the mean of the time series is calculated
# Normalization procedure where the raw time series is divided by its mean
# The normalized time series has to be understood as a deviation to its mean which is set to 100 (over time)
data_gtrends[data_gtrends$geo == codegeo,var_ref] <- data_gtrends[data_gtrends$geo == codegeo,var_ref]*100/mean_var_ref
return(data_gtrends) # Return the database after normalization procedure
}

# Apply the function for every combination of state and variable
for (v in 1:length(list_var_ref)){ # Loop over the variables
	for (s in 1:length(states)){ # Loop over the states
	data_gtrends <- normalization(states[s],list_var_ref[v]) # Normalization function
	}
}

# Database with the normalized variables

##################################################
########## LOCKDOWN DATABASE
##################################################

# Base for the lockdown dates (beginning and end)
lockdown <- read_excel(paste0(path_data,"Lockdown\\Lockdown_dates.xlsx")) # Lockdown file
lockdown <- lockdown[1:51,2:4] # Select the useful data in the file
lockdown$begin_lockdown <- as.Date(lockdown$begin_lockdown) # Set the variable to the format "Date"
lockdown$end_lockdown <- as.Date(lockdown$end_lockdown) # Set the variable to the format "Date"

# Lockdown database 

# Joining the lockdown period by state
data_gtrends <- data_gtrends %>% full_join(lockdown)

# Database with the lockdown variables

##################################################
########## NEW VARAIBLES (DATE, REGION, LOCKDOWN)
##################################################

# Number of the observation in the time series for the state
data_gtrends$numeric_date <- rep(1:length(table(data_gtrends$date)),51)

# Geographic regions (dummies)
data_gtrends$US_NorthEast <- 0
data_gtrends$US_MidWest <- 0
data_gtrends$US_South <- 0
data_gtrends$US_West <- 0
data_gtrends$US_NorthEast[data_gtrends$geo=="US_ME"|data_gtrends$geo=="US_VT"|data_gtrends$geo=="US_NH"|data_gtrends$geo=="US_MA"|data_gtrends$geo=="US_RI"|data_gtrends$geo=="US_CT"|data_gtrends$geo=="US_NJ"|data_gtrends$geo=="US_NY"|data_gtrends$geo=="US_PA"] <- 1 
data_gtrends$US_MidWest[data_gtrends$geo=="US_ND"|data_gtrends$geo=="US_SD"|data_gtrends$geo=="US_NE"|data_gtrends$geo=="US_KS"|data_gtrends$geo=="US_MN"|data_gtrends$geo=="US_IA"|data_gtrends$geo=="US_MO"|data_gtrends$geo=="US_WI"|data_gtrends$geo=="US_IL"|data_gtrends$geo=="US_IN"|data_gtrends$geo=="US_MI"|data_gtrends$geo=="US_OH"] <- 1 
data_gtrends$US_South[data_gtrends$geo=="US_TX"|data_gtrends$geo=="US_OK"|data_gtrends$geo=="US_AR"|data_gtrends$geo=="US_LA"|data_gtrends$geo=="US_MS"|data_gtrends$geo=="US_AL"|data_gtrends$geo=="US_GA"|data_gtrends$geo=="US_FL"|data_gtrends$geo=="US_SC"|data_gtrends$geo=="US_NC"|data_gtrends$geo=="US_TN"|data_gtrends$geo=="US_KY"|data_gtrends$geo=="US_WV"|data_gtrends$geo=="US_VA"|data_gtrends$geo=="US_MD"|data_gtrends$geo=="US_DE"|data_gtrends$geo=="US_DC"] <- 1 
data_gtrends$US_West[data_gtrends$geo=="US_WA"|data_gtrends$geo=="US_OR"|data_gtrends$geo=="US_CA"|data_gtrends$geo=="US_AK"|data_gtrends$geo=="US_ID"|data_gtrends$geo=="US_MT"|data_gtrends$geo=="US_WY"|data_gtrends$geo=="US_NV"|data_gtrends$geo=="US_UT"|data_gtrends$geo=="US_AZ"|data_gtrends$geo=="US_CO"|data_gtrends$geo=="US_NM"|data_gtrends$geo=="US_HI"] <- 1 

# Geographic regions (character variable)
data_gtrends$region <- ""
data_gtrends$region[data_gtrends$geo=="US_ME"|data_gtrends$geo=="US_VT"|data_gtrends$geo=="US_NH"|data_gtrends$geo=="US_MA"|data_gtrends$geo=="US_RI"|data_gtrends$geo=="US_CT"|data_gtrends$geo=="US_NJ"|data_gtrends$geo=="US_NY"|data_gtrends$geo=="US_PA"] <- "Northeast"
data_gtrends$region[data_gtrends$geo=="US_ND"|data_gtrends$geo=="US_SD"|data_gtrends$geo=="US_NE"|data_gtrends$geo=="US_KS"|data_gtrends$geo=="US_MN"|data_gtrends$geo=="US_IA"|data_gtrends$geo=="US_MO"|data_gtrends$geo=="US_WI"|data_gtrends$geo=="US_IL"|data_gtrends$geo=="US_IN"|data_gtrends$geo=="US_MI"|data_gtrends$geo=="US_OH"] <- "Midwest"
data_gtrends$region[data_gtrends$geo=="US_TX"|data_gtrends$geo=="US_OK"|data_gtrends$geo=="US_AR"|data_gtrends$geo=="US_LA"|data_gtrends$geo=="US_MS"|data_gtrends$geo=="US_AL"|data_gtrends$geo=="US_GA"|data_gtrends$geo=="US_FL"|data_gtrends$geo=="US_SC"|data_gtrends$geo=="US_NC"|data_gtrends$geo=="US_TN"|data_gtrends$geo=="US_KY"|data_gtrends$geo=="US_WV"|data_gtrends$geo=="US_VA"|data_gtrends$geo=="US_MD"|data_gtrends$geo=="US_DE"|data_gtrends$geo=="US_DC"] <- "South" 
data_gtrends$region[data_gtrends$geo=="US_WA"|data_gtrends$geo=="US_OR"|data_gtrends$geo=="US_CA"|data_gtrends$geo=="US_AK"|data_gtrends$geo=="US_ID"|data_gtrends$geo=="US_MT"|data_gtrends$geo=="US_WY"|data_gtrends$geo=="US_NV"|data_gtrends$geo=="US_UT"|data_gtrends$geo=="US_AZ"|data_gtrends$geo=="US_CO"|data_gtrends$geo=="US_NM"|data_gtrends$geo=="US_HI"] <- "West"

# Year of the observation
data_gtrends$year <- as.numeric(substr(as.Date(as.numeric(data_gtrends$date)),1,4))
# Month of the observation
data_gtrends$month <- as.numeric(substr(as.Date(as.numeric(data_gtrends$date)),6,7))
# Number of days per month
data_gtrends$nb_days <- as.numeric(as.Date(paste0(data_gtrends$year,"-",data_gtrends$month+1,"-01")) - data_gtrends$date)
data_gtrends$nb_days[is.na(data_gtrends$nb_days) == TRUE] <- 31

# Dummy about the presence of a lockdown
data_gtrends$has_lockdown <- as.numeric(is.na(data_gtrends$begin_lockdown) == FALSE)
# Dummy for the month where we can count at least one day under lockdown (1 if lockdown for the month of the observation, 0 otherwise)
data_gtrends$month_under_lockdown <- as.numeric((as.Date(paste0(substr(data_gtrends$begin_lockdown,1,8),"01")) - data_gtrends$date <= 0) & data_gtrends$date < data_gtrends$end_lockdown)
# Last of the lockdown (in days)
data_gtrends$last_lockdown <- as.numeric(data_gtrends$end_lockdown - data_gtrends$begin_lockdown)
# Variable for the number of days until the beginning of the lockdown
data_gtrends$time_to_lockdown <- as.numeric(data_gtrends$date - data_gtrends$begin_lockdown)
# Variable for the number of days until the end of the lockdown
data_gtrends$time_to_end_lockdown <- as.numeric(data_gtrends$date - data_gtrends$end_lockdown)
# Variable to count the number of days without lockdown in the first month of lockdown
data_gtrends$weight_lockdown1 <- as.numeric(data_gtrends$date == as.Date(paste0(substr(as.Date(as.numeric(data_gtrends$begin_lockdown)),1,8),"01")))
data_gtrends$weight_lockdown1 <- data_gtrends$weight_lockdown1 * (as.numeric(data_gtrends$begin_lockdown - data_gtrends$date))
# Variable to count the number of days without lockdown in the last month of lockdown
data_gtrends$weight_lockdown2 <- as.numeric(data_gtrends$date == as.Date(paste0(substr(as.Date(as.numeric(data_gtrends$end_lockdown)),1,8),"01")))
data_gtrends$weight_lockdown2 <- data_gtrends$weight_lockdown2 * abs(data_gtrends$nb_days - (as.numeric(data_gtrends$end_lockdown - data_gtrends$date + 1)))
# Weight for the days under lockdown within a month
data_gtrends$under_lockdown <- data_gtrends$month_under_lockdown * ((data_gtrends$nb_days - data_gtrends$weight_lockdown1 - data_gtrends$weight_lockdown2) / data_gtrends$nb_days)

# Database with the variables included above

##################################################
########## OUTLIERS DETECTION
##################################################

# Addition of the variable for the outliers (dummies, which takes 0 as default value)
for (i in 1:length(list_var_ref)){
data_gtrends[,paste0(list_var_ref[i],"_out")] <- 0}

# Function to have an automatic detection of the outliers
test_outliers <- function(codegeo, var_ref, k, MAD){
	
	# Chauvenet's Criterion
	crit <- 1/(2*(dim(subset(data_gtrends,geo==codegeo))[1]))
	MEAN <- colMeans(subset(data_gtrends,geo==codegeo & time_to_lockdown <0)[1:(dim(subset(data_gtrends,geo==codegeo & time_to_lockdown <0))[1]),var_ref])
	SD <- colsd(subset(data_gtrends,geo==codegeo & time_to_lockdown <0)[1:(dim(subset(data_gtrends,geo==codegeo & time_to_lockdown <0))[1]),var_ref])
	data_out <- abs(subset(data_gtrends,geo==codegeo & time_to_lockdown <0)[1:(dim(subset(data_gtrends,geo==codegeo & time_to_lockdown <0))[1]),var_ref] -MEAN)
	data_out <- data_out/SD
	for (i in 1:(dim(data_out))[1]){
		data_out[i,var_ref] <- erfc(data_out[i,var_ref])}
	data_out <- data_out/crit
	data_out[data_out <= 1] <-1 
	data_out[data_out > 1] <-0
	idx <- which(as_vector(data_out)==1)

	# Hampel filter
	b <- hampel(as_vector(subset(data_gtrends, geo == codegeo)[,var_ref]), k, MAD)
	b$ind

	# Loess decomposition outlier
	c <- forecast::tsoutliers(as_vector(subset(data_gtrends, geo == codegeo & time_to_lockdown <0)[,var_ref]))
	c$index

	index_out <- c(as_vector(idx),as_vector(b$ind),as_vector(c$index))
	tab_index <- table(index_out)
	tab_index <- tab_index[tab_index >2]
	outliers <- as_vector(as.numeric(names(tab_index)))

	if(length(outliers) != 0){
	for (i in 1:length(outliers)){
		data_gtrends[data_gtrends[,"geo"] == codegeo & data_gtrends[,"numeric_date"] == outliers[i],paste0(var_ref,"_out")] <- 1}}

return(data_gtrends)
}

# Choice for the Hampel Filter : 6 months
for (s in 1:length(states)){ # Loop over the states
	for (v in 1:length(list_var_ref)){ # Loop over the normalized variables
	if (is.na(subset(data_gtrends, geo == states[s])[1,list_var_ref[v]])== FALSE){ # Verify thatwe're not dealing with missing values
		data_gtrends <- test_outliers(states[s],list_var_ref[v], 6, 3) # Apply the outliers detection function
	} else{data_gtrends <- data_gtrends} # Dealing with missing values: don't apply the function
	}
}

# Database with the outliers for the normalized variables

##################################################
########## POPULATION BY STATE BY YEAR (CENSUS)
##################################################

# NOTE: I decided to interpolate the population figures (piecewise constant interpolation)
# Since the state population is only given once a year and not monthly,
# I decided to consider the figure constant over the months of the year

# 2004-2009 period
pop <- read_excel(paste0(path_data,"Ground\\population\\pop_2000_2009.xls")) # Corresponding file
pop <- pop[9:59,c(7:12)] # Take only the useful values
# Create the dataframe that will merge adequately with the database
pop1 <- data.frame(geo = rep(states,each = 72), # State code
		date = rep(seq(as.Date("2004-01-01"),as.Date("2009-12-01"),by="month"),51), # Corresponding date
		pop = rep(as.vector(as.numeric(t(pop))),each = 12) # Population figures
)
pop1$geo <- as.character(pop1$geo)

# 2010-2019 period
pop <- read_excel(paste0(path_data,"Ground\\population\\pop_2010_2019.xlsx")) # Corresponding file
pop <- pop[9:59,c(4:13)] # Take only the useful values
pop2 <- data.frame(geo = rep(states,each = 120), # State code
		date = rep(seq(as.Date("2010-01-01"),as.Date("2019-12-01"),by="month"),51), # Corresponding date
		pop = rep(as.vector(as.numeric(t(pop))),each = 12) # Population figures
)
pop2$geo <- as.character(pop2$geo)

# 2020 (projections)
pop <- read_excel(paste0(path_data,"Ground\\population\\pop_proj_2020.xls")) # Corresponding file
pop <- pop[5:55,4] # Take only the useful values
pop3 <- data.frame(geo = rep(states,each = 10), # State code
		date = rep(seq(as.Date("2020-01-01"),as.Date("2020-10-01"),by="month"),51), # Corresponding date
		pop = rep(as.vector(as.numeric(t(pop))),each = 10) # Projections figures
)
pop3$geo <- as.character(pop3$geo)

# Joining the population databases together
pop <- pop1 %>% full_join(pop2) %>% full_join(pop3)
# Complete population database for our sampling period

# Join the population database
data_gtrends <- data_gtrends %>% full_join(pop)

# Complete database with the population

##################################################
########## UNEMPLOYMENT DATA
##################################################

# Function adding the monthly unemployment rates by state to the database

data_unempl_states <- function(codegeo){ # Argument of the function is the state
d_unempl <- read_excel(paste0(path_data,"Ground\\unempl_states\\",codegeo,".xlsx")) # Takes the file of the unemployment rates for the corresponding state
d_unempl <- d_unempl[c(11:dim(d_unempl)[1]),-c(1)] # Take only the useful values
# Create a dataframe that will merge adequately with the database
A <- data.frame(geo = rep(codegeo,17*12), # State code
		date = seq(as.Date("2004-01-01"),as.Date("2020-12-01"),by="month"), # Corresponding date
		unempl_monthly_rate = as.vector(as.numeric(t(d_unempl))) # Unemployment monthly rates
)
assign(paste0("unempl_",codegeo),A) # Assingn a specific name to the reduced unemployment dataframe
return(get(paste0("unempl_",codegeo))) # Function returns this dataframe
}

# Loop to join the reduced unemployment dataframes for all states

# Initializationwith the first state
unempl_US_States <- data_unempl_states(states[1])
for (s in 2:length(states)){ # Loop over the states
unempl_US_States <- unempl_US_States %>% full_join(data_unempl_states(states[s])) # Joining the reduced unemployment dataframes
}
unempl_US_States$geo <- as.character(unempl_US_States$geo)
# Add the year to the unemployment database
unempl_US_States$year <- year(unempl_US_States$date)

########## Lagged Unemployment Rates

data_lag <- unempl_US_States[,c("geo","date","unempl_monthly_rate")] # define a dataframe with the variable we want to lag
# Up to 11 monthly lags
data_lag[paste0("unempl_monthly_rate_lag",1:11)] <- 0 # Add the new lagged variables to this dataframe (set to 0)
for (s in 1:length(states)){ # Loop over the states
	for (i in 1:11){ # Loop over the number of lags
		# Applying the lag function
		data_lag[data_lag$geo==states[s],3+i] <- shift(data_lag[data_lag$geo==states[s],3],i,"lag")
	}
}
# Joining the lagged datframe to the database
unempl_US_States <- unempl_US_States %>% full_join(data_lag)

unempl_annual <- aggregate(unempl_US_States[,3], by=list(geo=unempl_US_States$geo,date=unempl_US_States$year),FUN=mean)
# Set the date of the annual average to december
unempl_annual$date <- rep(seq(as.Date("2004-12-01"),as.Date("2020-12-01"),by="year"),each=51)

# Setting the names for the variables
setnames(unempl_annual,names(unempl_annual)[3],"unempl_annual_rate")

# Joining this annual unemployment dataframe to the monthly unemployment dataframe
unempl_US_States <- unempl_US_States %>% full_join(unempl_annual)
unempl_US_States <- unempl_US_States[unempl_US_States$date < as.Date("2020-11-01"),]

# Complete unemployment dataframe

# Joining the unemployment dataframe with the database
data_gtrends <- data_gtrends %>% full_join(unempl_US_States)

# Databse with the unemployment figures

##################################################
########## COVID-19 DATA (JOHNS HOPKINS)          
##################################################

##################################################
########## DEATHS US
##################################################

# Bring the corresponding file
data <- read_csv(paste0(path_data,"Ground\\covid\\time_series_covid19_deaths_US.csv"))
# Here, the observations are made by cites/counties : we want them by state (13 is the first index referring to a date)
data <- aggregate(data[,13:(dim(data)[2])], by = list(data$Province_State), FUN = sum)
# Formating the dataframe
date <- names(data)[2:(dim(data)[2])]
# Take the date observations to create a vector
date <- as.POSIXct(paste0(date,"20"), tz = "GMT", format = "%m/%d/%Y")
# Tanspose the deaths dataframe
data <- as.data.frame(t(data))
# Set the variables names for the Covid dataframe
for (i in 1:(dim(data)[2])){
	names(data)[i] <- as.character(data[1,i])}
# Suppress the first lin, no more useful
data <- data[-c(1),]
# Insert the date vector as a variable in our cleaned dataframe 
data$date <- date

# We keep the figures for the last day each the month since we have monthly data
# The indexes refer to the last day of each month, from January to October (last day observed for October)
data <- data[c(10,39,70,100,131,161,192,223,253,284),]
# We keep only the states (not the US territories as Puerto Rico)
data <-data[,-c(3,10,14,15,40,45,53)]
# Detecting which variables are factor
index <- sapply(data,is.factor)
# Converting the factor variables to numeric variables
data[index] <- lapply(data[index], function(x) as.numeric(as.character(x)))

# We wantthe monthly variation and not the absolute numbers since the beginning of the epidemics
# Thus, we have to manipulate the figures toobtain the variation
for (i in 0:(dim(data)[1]-2)){ # Loop over the months
	for (j in 1:(length(data)-1)){ # Loop over the states
		# Beginning with the last observation, we subtract the precedent observation
		# This gives us the monthly variation
		data[(dim(data)[1])-i,j] <- data[(dim(data)[1])-i,j] - data[(dim(data)[1]-1)-i,j] 
	}
}

# Set the geographic codes
setnames(data, c(names(data)[1:51]), c('US_AL','US_AK','US_AZ','US_AR','US_CA','US_CO','US_CT','US_DE','US_DC','US_FL','US_GA','US_HI','US_ID','US_IL','US_IN','US_IA','US_KS','US_KY','US_LA','US_ME','US_MD','US_MA','US_MI','US_MN','US_MS','US_MO','US_MT','US_NE','US_NV','US_NH','US_NJ','US_NM','US_NY','US_NC','US_ND','US_OH','US_OK','US_OR','US_PA','US_RI','US_SC','US_SD','US_TN','US_TX','US_UT','US_VT','US_VA','US_WA','US_WV','US_WI','US_WY'))
# Create the deaths dataframe that will merge adequately with the database
deaths_US <- data.frame(date = rep(as.vector(paste0(substr(data$date,1,8),"01")),51), # Corrsponding date
		geo = rep(as.vector(names(data)[1:51]), each = dim(data)[1]), # State code
		deaths = as_vector(data[,1:51]) # deaths figures
)
deaths_US$date <- as.Date(deaths_US$date)
deaths_US$geo <- as.character(deaths_US$geo)

# Deaths dataframe

##################################################
########## Confirmed US
##################################################

# NOTE: Exactly the same operations as above for the confirmed dataframe

data <- read_csv(paste0(path_data,"Ground\\covid\\time_series_covid19_confirmed_US.csv"))
data <- aggregate(data[,13:(dim(data)[2])], by = list(data$Province_State), FUN = sum)
date <- names(data)[2:(dim(data)[2])]
date <- as.POSIXct(paste0(date,"20"), tz = "GMT", format = "%m/%d/%Y")
data <- as.data.frame(t(data))
for (i in 1:(dim(data)[2])){
	names(data)[i] <- as.character(data[1,i])}
data <- data[-c(1),]
data$date <- date
data <- data[c(9,38,69,99,130,160,191,222,252,283),]
data <-data[,-c(3,10,14,15,40,45,53)]
index <- sapply(data,is.factor)
data[index] <- lapply(data[index], function(x) as.numeric(as.character(x)))
for (i in 0:(dim(data)[1]-2)){
	for (j in 1:(length(data)-1)){
		data[(dim(data)[1])-i,j] <- data[(dim(data)[1])-i,j] - data[(dim(data)[1]-1)-i,j]
}
}

setnames(data, c(names(data)[1:51]), c('US_AL','US_AK','US_AZ','US_AR','US_CA','US_CO','US_CT','US_DE','US_DC','US_FL','US_GA','US_HI','US_ID','US_IL','US_IN','US_IA','US_KS','US_KY','US_LA','US_ME','US_MD','US_MA','US_MI','US_MN','US_MS','US_MO','US_MT','US_NE','US_NV','US_NH','US_NJ','US_NM','US_NY','US_NC','US_ND','US_OH','US_OK','US_OR','US_PA','US_RI','US_SC','US_SD','US_TN','US_TX','US_UT','US_VT','US_VA','US_WA','US_WV','US_WI','US_WY'))
confirmed_US <- data.frame(date = rep(as.vector(paste0(substr(data$date,1,8),"01")),51),
		geo = rep(as.vector(names(data)[1:51]), each = dim(data)[1]),
		confirmed = as_vector(data[,1:51])
)
confirmed_US$date <- as.Date(confirmed_US$date)
confirmed_US$geo <- as.character(confirmed_US$geo)

# Confirmed dataframe

########## Joining the Covid-19 dataframes
data_gtrends <- data_gtrends  %>% full_join(deaths_US) %>% full_join(confirmed_US)

# Recoding the missing values (before the epidemics) as 0
data_gtrends$deaths[is.na(data_gtrends$deaths)] <- 0
data_gtrends$confirmed[is.na(data_gtrends$confirmed)] <- 0

# Add the variables of cases and deaths / population
data_gtrends$deaths_pop <- data_gtrends$deaths / data_gtrends$pop * 1000000
data_gtrends$confirmed_pop <- data_gtrends$confirmed / data_gtrends$pop * 1000000

# Variables to observe enough cumulative cases (100) or deaths (20) of COVID-19
# This will help us in setting a threshold to evaluate the advance of the epidemics
# And therfore to make econometric treatments

# Setting the cumulative deaths / cases variables to 0
data_gtrends$sum_deaths <- 0
data_gtrends$sum_cases <-0

# Setting the the threshold variables (dummies with 0 as default values)
data_gtrends$after_deaths <- 0
data_gtrends$after_cases <- 0

# Count the absolute numbers
for (s in 1:length(states)){ # Loop over the states
for (i in 2:length(unique(data_gtrends$date))){ # Loop over the date
# Variables for the cumulative deaths / cases
data_gtrends[data_gtrends$geo==states[s] & data_gtrends$date==unique(data_gtrends$date)[i],]$sum_deaths <- sum(data_gtrends[data_gtrends$geo==states[s] & data_gtrends$date <= unique(data_gtrends$date)[i],]$deaths)
data_gtrends[data_gtrends$geo==states[s] & data_gtrends$date==unique(data_gtrends$date)[i],]$sum_cases <- sum(data_gtrends[data_gtrends$geo==states[s] & data_gtrends$date <= unique(data_gtrends$date)[i],]$confirmed)
}

# A and B are the first dates to be above the thresholds for cases and deaths
# For now, the thresholds are set to 100 cases and 20 deaths
A <- min(subset(data_gtrends,geo==states[s] & sum_cases > 99)$date)
B <- min(subset(data_gtrends,geo==states[s] & sum_deaths > 19)$date)
# Set the dummies of the thresholds according to the observations
data_gtrends[data_gtrends$geo==states[s] & data_gtrends$date >= A,]$after_cases <- 1
data_gtrends[data_gtrends$geo==states[s] & data_gtrends$date >= B,]$after_deaths <- 1
}

# Database with the COVID-19 data

##################################################
########## CDC-NVSS DATA DIVORCE RATES BY STATE
##################################################

# Corresponding file
data_div <- read_excel(paste0(path_data,"Ground\\divorce\\divorce_rates_us.xlsx"))
# Take only the useful values
data_div <- data_div[-c(1:4,6,58:64),-c(17:23)]
# Set the geographic codes
data_div[1:52,1] <- c("geo","US_AL","US_AK","US_AZ","US_AR","US_CA","US_CO","US_CT","US_DE","US_DC","US_FL","US_GA","US_HI","US_ID","US_IL","US_IN","US_IA","US_KS","US_KY","US_LA","US_ME","US_MD","US_MA","US_MI","US_MN","US_MS","US_MO","US_MT","US_NE","US_NV","US_NH","US_NJ","US_NM","US_NY","US_NC","US_ND","US_OH","US_OK","US_OR","US_PA","US_RI","US_SC","US_SD","US_TN","US_TX","US_UT","US_VT","US_VA","US_WA","US_WV","US_WI","US_WY")
# Formating the divorce dataframe so that it merges with the database
setnames(data_div,c(names(data_div)),as_vector(data_div[1,]))
data_div <- data_div[-c(1),]
data_div[data_div == "---"] <- NA

# Vector for the date
date <- rep(seq(as.Date("2004-12-01"),as.Date("2018-12-01"),by="year"),dim(data_div)[1])
# Vector for the geographic areas
geo <- rep(as_vector(data_div[,1]),each=length(unique(date)))
# Vector of the divorce rates
div_rate <- as_vector(as.numeric(rev(data_div[1,2:dim(data_div)[2]])))
for (j in 2:dim(data_div)[1]){
	div_rate <- as_vector(as.numeric(append(div_rate,rev(data_div[j,2:dim(data_div)[2]]))))}

# Dataframe with the three vectors above
data_div <- data.frame(geo = geo,date = date,div_rate = div_rate)
data_div$geo <- as.character(data_div$geo)

# Divorce dataframe

##################################################
########## CDC-NVSS DATA WEDDING RATES BY STATE
##################################################

# NOTE: same operations as above for the wedding rates dataframe

data_wedd <- read_excel(paste0(path_data,"Ground\\wedding\\wedding_rates_us.xlsx"))
data_wedd <- data_wedd[-c(1:4,6,58:65),-c(17:23)]
data_wedd[1:52,1] <- c("geo","US_AL","US_AK","US_AZ","US_AR","US_CA","US_CO","US_CT","US_DE","US_DC","US_FL","US_GA","US_HI","US_ID","US_IL","US_IN","US_IA","US_KS","US_KY","US_LA","US_ME","US_MD","US_MA","US_MI","US_MN","US_MS","US_MO","US_MT","US_NE","US_NV","US_NH","US_NJ","US_NM","US_NY","US_NC","US_ND","US_OH","US_OK","US_OR","US_PA","US_RI","US_SC","US_SD","US_TN","US_TX","US_UT","US_VT","US_VA","US_WA","US_WV","US_WI","US_WY")
setnames(data_wedd,c(names(data_wedd)),as_vector(as.character(data_wedd[1,])))
data_wedd <- data_wedd[-c(1),]
data_wedd[data_wedd == "---"] <- NA

date <- rep(seq(as.Date("2004-12-01"),as.Date("2018-12-01"),by="year"),dim(data_wedd)[1])
geo <- rep(as_vector(data_wedd[,1]),each=length(unique(date)))
wedd_rate <- as_vector(as.numeric(rev(data_wedd[1,2:dim(data_wedd)[2]])))
for (j in 2:dim(data_wedd)[1]){
	wedd_rate <- as_vector(as.numeric(append(wedd_rate,rev(data_wedd[j,2:dim(data_wedd)[2]]))))}

data_wedd <- data.frame(geo = geo,date = date,wedd_rate = wedd_rate)
data_wedd$geo <- as.character(data_wedd$geo)

# Wedding dataframe

# Join the divorce and wedding dataframes to the database
data_gtrends <- data_gtrends %>% full_join(data_div) %>% full_join(data_wedd)

# New variable: divorce rate / marriage rate
data_gtrends$div_wedd_rate <- data_gtrends$div_rate / data_gtrends$wedd_rate

# Database with the divorce and wedding rates

##################################################
########## AGE AND SEX DATA (CENSUS)
##################################################

# The new variables added are the following:
# Proportion of men, proportion of women,
# Proportion of 0-17 years, proportion of 18-64 years, proportion of +65 years,
# Same age variables but among men, and among women,
# Median age for overall population, among men, and among women,
# And the sex-ratio

# Function formating the files to merge them with the database

data_age_sex_2009 <- function(codegeo){ # Argument is the state
d_age_sex <- read_excel(paste0(path_data,"Ground\\age_sex\\2004_2009\\",codegeo,".xls")) # Corresponding file
d_age_sex <- d_age_sex[c(3,4,24,28,32,38,39,59,63,67,73,74,94,98,102,108),-c(2:6,13,14)] # Take only the useful values
# Create the reduced dataframe for the state over the period withe the new variables
A <- data.frame(geo = rep(codegeo,6*12),
		date = seq(as.Date("2004-01-01"),as.Date("2009-12-01"),by="month"),
		men = as.numeric(rep(as_vector(d_age_sex[7,2:7]),each=12)),
		women = as.numeric(rep(as_vector(d_age_sex[12,2:7]),each=12)),
		age1 = as.numeric(rep(as_vector(d_age_sex[3,2:7]),each=12)),
		age2 = as.numeric(rep(as_vector(d_age_sex[4,2:7]),each=12)),
		age3 = as.numeric(rep(as_vector(d_age_sex[5,2:7]),each=12)),
		med_age = as.numeric(rep(as_vector(d_age_sex[6,2:7]),each=12)),
		age1_m = as.numeric(rep(as_vector(d_age_sex[8,2:7]),each=12)),
		age2_m = as.numeric(rep(as_vector(d_age_sex[9,2:7]),each=12)),
		age3_m = as.numeric(rep(as_vector(d_age_sex[10,2:7]),each=12)),
		med_age_m = as.numeric(rep(as_vector(d_age_sex[11,2:7]),each=12)),
		age1_f = as.numeric(rep(as_vector(d_age_sex[13,2:7]),each=12)),
		age2_f = as.numeric(rep(as_vector(d_age_sex[14,2:7]),each=12)),
		age3_f = as.numeric(rep(as_vector(d_age_sex[15,2:7]),each=12)),
		med_age_f = as.numeric(rep(as_vector(d_age_sex[16,2:7]),each=12))
)
A$sex_ratio <- A$men/A$women
assign(paste0("age_sex_2009_",codegeo),A)
return(get(paste0("age_sex_2009_",codegeo)))
}

# Loop to join the reduced dataframe in one
age_sex_2009 <- data_age_sex_2009(states[1]) # Initialization over the first state
for (s in 2:length(states)){ # Loop over the state
age_sex_2009 <- age_sex_2009 %>% full_join(data_age_sex_2009(states[s])) # Joining the reduced dataframes
}
age_sex_2009$geo <- as.character(age_sex_2009$geo)

# Dataframe of the age-sex variables for 2004-2009

# NOTE: same operations for 2010-2019

data_age_sex_2019 <- function(codegeo){
d_age_sex <- read_excel(paste0(path_data,"Ground\\age_sex\\2010_2019\\",codegeo,".xlsx"))
d_age_sex <- d_age_sex[c(4,5,25,29,33,39),]
A <- data.frame(geo = rep(codegeo,10*12),
		date = seq(as.Date("2010-01-01"),as.Date("2019-12-01"),by="month"),
		men = as.numeric(rep(as_vector(d_age_sex[2,seq(9,36,by=3)]),each=12)),
		women = as.numeric(rep(as_vector(d_age_sex[2,seq(10,37,by=3)]),each=12)),
		age1 = as.numeric(rep(as_vector(d_age_sex[3,seq(8,35,by=3)]),each=12)),
		age2 = as.numeric(rep(as_vector(d_age_sex[4,seq(8,35,by=3)]),each=12)),
		age3 = as.numeric(rep(as_vector(d_age_sex[5,seq(8,35,by=3)]),each=12)),
		med_age = as.numeric(rep(as_vector(d_age_sex[6,seq(8,35,by=3)]),each=12)),
		age1_m = as.numeric(rep(as_vector(d_age_sex[3,seq(9,36,by=3)]),each=12)),
		age2_m = as.numeric(rep(as_vector(d_age_sex[4,seq(9,36,by=3)]),each=12)),
		age3_m = as.numeric(rep(as_vector(d_age_sex[5,seq(9,36,by=3)]),each=12)),
		med_age_m = as.numeric(rep(as_vector(d_age_sex[6,seq(9,36,by=3)]),each=12)),
		age1_f = as.numeric(rep(as_vector(d_age_sex[3,seq(10,37,by=3)]),each=12)),
		age2_f = as.numeric(rep(as_vector(d_age_sex[4,seq(10,37,by=3)]),each=12)),
		age3_f = as.numeric(rep(as_vector(d_age_sex[5,seq(10,37,by=3)]),each=12)),
		med_age_f = as.numeric(rep(as_vector(d_age_sex[6,seq(10,37,by=3)]),each=12))
)
A$sex_ratio <- A$men/A$women
assign(paste0("age_sex_2019_",codegeo),A)
return(get(paste0("age_sex_2019_",codegeo)))
}

age_sex_2019 <- data_age_sex_2019(states[1])
for (s in 2:length(states)){
age_sex_2019 <- age_sex_2019 %>% full_join(data_age_sex_2019(states[s]))
}
age_sex_2019$geo <- as.character(age_sex_2019$geo)

# Dataframe of the age-sex variables for 2010-2019

# Joining the age-sex dataframes
age_sex <- age_sex_2009 %>% full_join(age_sex_2019)

# Joining the age-sex database to the working database
data_gtrends <- data_gtrends %>% full_join(age_sex)

# Make the variables as proportions when they are in absolute terms
for (v in c(95:99,101:103,105:107)){
data_gtrends[,v] <- data_gtrends[,v]/data_gtrends$pop
}

# Database with age and sex variables

##################################################
########## LAGGED VARIABLES
##################################################

# Loop to apply lags to the Google Trends variables

for (v in 1:length(list_var_ref)){ # loop over the Google Trends variables
	data_lag <- data_gtrends[,c("geo","date",list_var_ref[v])] # define a dataframe with the variable we want to lag

	# Up to 11 monthly lags
	data_lag[paste0(list_var_ref[v],"_lag",1:11)] <- 0 # Add the new lagged variables to this dataframe (set to 0)
	for (s in 1:length(states)){ # Loop over the states
		for (i in 1:11){ # Loop over the number of lags
			# Applying the lag function
			data_lag[data_lag$geo==states[s],3+i] <- shift(data_lag[data_lag$geo==states[s],3],i,"lag")
		}
	}
	# Joining the lagged datframe to the database
	data_gtrends <- data_gtrends %>% full_join(data_lag)
}

# Database with the lagged variables

##################################################
########## LEAD VARIABLES
##################################################

# Loop to apply leads to the Google Trends variables

for (v in 1:length(list_var_ref)){ # loop over the Google Trends variables
	data_lead <- data_gtrends[,c("geo","date",list_var_ref[v])] # define a dataframe with the variable we want to lead

	# Up to 6 monthly leads
	data_lead[paste0(list_var_ref[v],"_lead",1:6)] <- 0 # Add the new lead variables to this dataframe (set to 0)
	for (s in 1:length(states)){ # Loop over the states
		for (i in 1:6){ # Loop over the number of leads
			# Applying the lead function
			data_lead[data_lead$geo==states[s],3+i] <- shift(data_lead[data_lead$geo==states[s],3],i,type="lead")
		}
	}
	# Joining the leadd datframe to the database
	data_gtrends <- data_gtrends %>% full_join(data_lead)
}

# Database with the lead variables

##################################################
########## MISSING GTRENDS DATA 
##################################################

# Create a .txt file compilling the missing time series (state and variable)
sink(file = paste0(path_data,"\\Missing\\missing_states.txt")) # Set the file path
for (i in 1:length(list_var)){ # Loop over the var
	print(paste0(list_var[i],": ",names(table(subset(data_gtrends, is.na(data_gtrends[,list_var[i]])==TRUE)$geo)))) # Missing states for a variable
}
sink(file = NULL)


#################################################################################################
##################################         Export Data           ################################
#################################################################################################

write_dta(data_gtrends, paste0(path_data,"Database\\data_gtrends.dta"))
write_csv(data_gtrends, paste0(path_data,"Database\\data_gtrends.csv"))

#################################################################################################
##################################   Aggregated Data until 2018  ################################
#################################################################################################

# Bring the database to aggregate it further
data_agg <- data_gtrends

# Convert the variable "region" into a numeric one
# 1 = Midwest ; 2 = Northeast ; 3 = South ; 4 = West
data_agg$region <- as.numeric(as.factor(data_agg$region))

# Aggreagtion (with variables up to 11 lags i.e. 1 year)
# Aggragation for the sampling period 2004-2018
data_agg <- aggregate.data.frame(subset(data_agg,year<2019)[,c(19:34,38:42,46,70,71:82,95:381)],by=list(subset(data_agg,year<2019)$year,subset(data_agg,year<2019)$geo),FUN=mean)
# Set the key variables names
setnames(data_agg,names(data_agg)[1:2],c("year","geo"))
# Set the name for the unemployment annual average
setnames(data_agg,"unempl_monthly_rate","unempl_annual_rate")
for (i in 27:37) {
setnames(data_agg,names(data_agg)[i],paste0("unempl_annual_rate_lag",i-26))
}

# Wedding annual rates
wedd_agg <- subset(data_gtrends, is.na(data_gtrends$wedd_rate) == FALSE & year < 2019)[,c("geo","year","wedd_rate")]
# Divorce annual rates
div_agg <- subset(data_gtrends, is.na(data_gtrends$div_rate) == FALSE & year < 2019)[,c("geo","year","div_rate")]
# Annual variable divorce rate / wedding rate
div_wedd_agg <- subset(data_gtrends, is.na(data_gtrends$div_wedd_rate) == FALSE & year < 2019)[,c("geo","year","div_wedd_rate")]

# Add these variables to the aggregated database
data_agg <- data_agg %>% full_join(div_agg) %>% full_join(wedd_agg) %>% full_join(div_wedd_agg)

# We drop the missing values for our variable of interest (divorce rate)
# Thus we don't include California, Georgia, Hawaii, Indiana, Louisiana, Minesota and New Mexico
data_agg <- data_agg[data_agg$geo != "US_CA" & data_agg$geo != "US_GA" & data_agg$geo != "US_HI" & data_agg$geo != "US_IN" & data_agg$geo != "US_LA" & data_agg$geo != "US_MN" & data_agg$geo != "US_NM",]

# Aggregated database over 2004-2018 period

#################################################################################################
##################################    Export Aggregated Data     ################################
#################################################################################################

write_dta(data_agg, paste0(path_data,"Database\\data_agg.dta"))
write_csv(data_agg, paste0(path_data,"Database\\data_agg.csv"))

#################################################################################################
##################################       Aggregated Data         ################################
#################################################################################################

# Aggregated data to make our predictions
data_pred <- data_gtrends

# Convert the "region" variable into a numeric one
# 1 = Midwest ; 2 = Northeast ; 3 = South ; 4 = West
data_pred$region <- as.numeric(as.factor(data_pred$region))

# Aggreagtion 
# NOTE: we're omitting the missing values so that wo could aggregate our variables for 2020 (and then nowcast)
data_pred <- aggregate.data.frame(data_pred[,c(19:34,38:42,46,70,71:82,95:381)],by=list(data_pred$year,data_pred$geo),FUN=mean, na.rm=TRUE,na.action=NULL)
# Set key variables names
setnames(data_pred,names(data_pred)[1:2],c("year","geo"))
# Set the name for the unemployment annual averages
setnames(data_pred,"unempl_monthly_rate","unempl_annual_rate")
for (i in 27:37) {
setnames(data_pred,names(data_pred)[i],paste0("unempl_annual_rate_lag",i-26))
}

# Wedding rate variable
wedd_agg <- subset(data_gtrends, month == 12)[,c("geo","year","wedd_rate")]
# Divorce rate variable
div_agg <- subset(data_gtrends, month == 12)[,c("geo","year","div_rate")]
# Divorce rate / wedding rate variable
div_wedd_agg <- subset(data_gtrends, month == 12)[,c("geo","year","div_wedd_rate")]

# Join the aggregated data with these variables
data_pred <- data_pred %>% full_join(div_agg) %>% full_join(wedd_agg) %>% full_join(div_wedd_agg)

# Drop the missing values for our variable of interest (divorce rate)
data_pred <- data_pred[data_pred$geo != "US_CA" & data_pred$geo != "US_GA" & data_pred$geo != "US_HI" & data_pred$geo != "US_IN" & data_pred$geo != "US_LA" & data_pred$geo != "US_MN" & data_pred$geo != "US_NM",]

# Aggregated data over 2004-2020

#################################################################################################
##################################         Export Data           ################################
#################################################################################################

write_dta(data_pred, paste0(path_data,"Database\\data_pred.dta"))
write_csv(data_pred, paste0(path_data,"Database\\data_pred.csv"))



