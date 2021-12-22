#################################################################################################
##################################  Internship INED - Divorce   ################################
##################################             Sansu             ################################              
#################################################################################################


#################################################################################################
##################################       Packages imports        ################################
#################################################################################################

install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("haven")
library(haven)
install.packages("readr")
library(readr)
install.packages("data.table") 
library(data.table)
install.packages("zoo")
library(zoo)
install.packages("FIACH")
library(FIACH)
install.packages("pracma")
library(pracma)
install.packages("purrr")
library(purrr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
install.packages("ggfortify")
library(ggfortify)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)

#################################################################################################
##################################     Environment cleaning      ################################
#################################################################################################

#rm(list=ls())

#################################################################################################
##################################      Working Environment      ################################
#################################################################################################

# Specify the paths (working directory, data, outputs)

paths <- c("C:\\Users\\Mathis\\Dropbox\\Divorce\\Code","C:\\Users\\sansu_mat\\Dropbox\\Divorce\\Code","C:\\Users\\Mathis\\Desktop\\Divorce\\Code", "XXXXX", "XXXXX")
paths_data <- c("C:\\Users\\Mathis\\Dropbox\\Divorce\\Data\\","C:\\Users\\sansu_mat\\Dropbox\\Divorce\\Data\\","C:\\Users\\Mathis\\Desktop\\Divorce\\Data\\", "XXXXX", "XXXXX")
paths_outputs <- c("C:\\Users\\Mathis\\Dropbox\\Divorce\\Outputs\\","C:\\Users\\sansu_mat\\Dropbox\\Divorce\\Outputs\\","C:\\Users\\Mathis\\Desktop\\Divorce\\Outputs\\", "XXXXX", "XXXXX")

# 1 = Mathis, Dropbox personal computer
# 2 = Mathis, Dropbox INED computer
# 3 = Mathis, Local personal computer
# 4 = Anne
# 5 = Marion

# Set the paths

pathwd <- paths[3]
path_data <- paths_data[3]
path_outputs <- paths_outputs[3]

# Set the working directory
setwd(pathwd)
# Verify the working directory
getwd()



# Enables to have significant numbers (and not a scientific scripture type)
options(scipen = 100000)

#################################################################################################
##################################            DATABASE           ################################
#################################################################################################

# Import the previously constituted database

data_gtrends <- read_csv(paste0(path_data,"Database\\data_gtrends.csv"))
#View(data_gtrends)

# List of the states
states <- c("US_AL","US_AK","US_AZ","US_AR","US_CA","US_CO","US_CT","US_DE","US_DC","US_FL","US_GA","US_HI","US_ID","US_IL","US_IN","US_IA","US_KS","US_KY","US_LA","US_ME","US_MD","US_MA","US_MI","US_MN","US_MS","US_MO","US_MT","US_NE","US_NV","US_NH","US_NJ","US_NM","US_NY","US_NC","US_ND","US_OH","US_OK","US_OR","US_PA","US_RI","US_SC","US_SD","US_TN","US_TX","US_UT","US_VT","US_VA","US_WA","US_WV","US_WI","US_WY")
# Variables list
list_var <- c("div","div_cov","div_file","div_papers","div_legal","div_lawyer","div_court","div_law","lock","div_cov_media","alimony","child_support","child_custody","div_how","div_much","div_long")
# Normalized variables list
list_var_ref <- c(paste0(list_var,"_ref"))

#################################################################################################
##################################            OUTPUTS            ################################
#################################################################################################

# Function to plot the time series with the lockdownperiod colorized, and the outliers being shown

ggplot_series <- function(codegeo, var_ref){ # Arguments: geographic code and variable
graph <- ggplot(subset(data_gtrends, geo == codegeo), aes(x = date, y = eval(parse(text = var_ref)))) + # Select the time series
		geom_line(color = "darkblue", size = 1) + # Time series
		geom_rect(data = subset(data_gtrends, geo == codegeo & numeric_date == 1), aes(xmin = begin_lockdown, xmax = end_lockdown, ymin = 0, ymax = Inf), fill = "red", alpha = 0.4) + # Colorize the lockdown period
		geom_point(data = subset(data_gtrends, geo == codegeo & eval(parse(text = paste0(var_ref,"_out"))) == 1), color = "yellow", size = 3) + # Colorize the outliers	
		xlab("Date") + ylab("Index of Google searches") +
		ggtitle(paste0("Index of interest for the search term ",var_ref," in ",codegeo)) +
		theme_hc()	
graph
}	

########## OUTLIERS SERIES

# Compile amm the plots in one .pdf file

pdf("Series.pdf") # .pdf file in the working directory
for (s in 1:length(states)){ # Loop over the states
	for (v in 1:length(list_var_ref)){ # Loop over the normalized variables
	plot(ggplot_series(states[s],list_var_ref[v])) # Apply the function
	}
}
dev.off()

#################################################################################################
##################################      Outliers Detection       ################################
#################################################################################################

# Function that detects outliers widely present in a geographic / population sense

outliers_geo <- function(threshold, pond){ # Arguments : a threshold to set, and the geographic ("FALSE") or population ("TRUE") count
A <- data_gtrends # Database
if (pond == "TRUE"){ # If population count set
	for (v in 1:length(list_var_ref)){ # Loop for each variable
		A[,paste0(list_var_ref[v],"_out")] <- A[,paste0(list_var_ref[v],"_out")]*A[,"pop"] # Ponderation by the population when outlieers previously observed
	}
}
B <- aggregate(A[,c(paste0(list_var_ref,"_out"))], by = A[,"date"], FUN = sum) # Aggregation on date: every var"_out" observation set to 0, otherwise it is an utlier that could be ponderated
C <- aggregate(A[,"pop"], by = A[,"date"], FUN = sum) # Aggregation of the population (it means we have the population of the US in total for each date)
setnames(C, "pop", "pop_tot") # Set the names of the key variables
D <- B %>% full_join(C) # Joining the dataframes
setnames(D, c(paste0(list_var_ref,"_out")),c(paste0(list_var_ref,"_out","_US"))) # Defining names
if (pond == "TRUE"){ # If population count set
	D[,c(paste0(list_var_ref,"_out_US"))] <- D[,c(paste0(list_var_ref,"_out_US"))] / D[,"pop_tot"]*100 # Outliers weights in reference to the whole population of the US
} else { # If geographic count set
	D[,c(paste0(list_var_ref,"_out_US"))] <- D[,c(paste0(list_var_ref,"_out_US"))]/(dim(unique(A[,"geo"]))[1])*100 # Outliers weight in reference to the numbers of states
}
for (v in 1:length(list_var_ref)){ # Loop over the variables
	for (i in 1:dim(D)[1]){ # For each observation
		if (D[i,paste0(list_var_ref[v],"_out_US")] < threshold){ # Compare the outliers weight to the previously setted threshold 
			D[i,paste0(list_var_ref[v],"_out","_US")] <- 0} else { # Dummy variable: 0if underneath the threshold
			D[i,paste0(list_var_ref[v],"_out_US")] <- 1 # 1 if equal or above the threshold
		}
	}
}
for (v in 1:length(list_var_ref)){ # Loop over the variables
	for (i in 1:dim(D)[1]){ # For each observation
		if (D[i,paste0(list_var_ref[v],"_out_US")] == 1){ # If dummy=1, i.e. outliers above the geographic / population threshold setted
			print(paste0(list_var_ref[v],": ", as.Date(D[i,"date"]))) # return the variable and date of this outlier
		}
	}
}
#return(D)
}

# Compile the geographic / population outliers in a .txt file

sink(file = paste0(path_outputs,"Outliets\\outliers_geo.txt")) # Specify the file path

# We make varying assumptions
print("TRANSVERSALITY OF OUTLIERS")
print("Threshold = 20%")
print("In number of states concerned by the outlier")
outliers_geo(20,"FALSE")
print("Ponderation by the population")
outliers_geo(20,"TRUE")
print("Threshold = 30%")
print("In number of states concerned by the outlier")
outliers_geo(30,"FALSE")
print("Ponderation by the population")
outliers_geo(30,"TRUE")
print("Threshold = 40%")
print("In number of states concerned by the outlier")
outliers_geo(40,"FALSE")
print("Ponderation by the population")
outliers_geo(40,"TRUE")
print("Threshold = 50%")
print("In number of states concerned by the outlier")
outliers_geo(50,"FALSE")
print("Ponderation by the population")
outliers_geo(50,"TRUE")

sink(file = NULL)

# Function detecting extreme values in terms of standard deviations

outliers_spec <-function(codegeo,var_ref,threshold){ # Arguments: geographic code, variable, number of standard deviations
A <- zoo(subset(data_gtrends, geo == codegeo & numeric_date < 193)[,var_ref], subset(data_gtrends, geo == codegeo & numeric_date < 193)$date) # Seriesin zoo type
reg <- lm(A ~ subset(data_gtrends, geo == codegeo & numeric_date < 193)$numeric_date) # Regression on the date to account for a trend            
B <- abs(reg$residuals)/sd(A) # Divide the residuals of the regression by the standard deviation of the series
index <- which(as_vector(B)> threshold) # See which observtion is above the setted threshold
if(is.na(index[1]) == FALSE){ # If there is at least one observation above the threshold
	print(paste0(codegeo," - ",var_ref,": ",subset(data_gtrends, geo == codegeo & numeric_date < 193)[index,"date"]$date)) # return the state, the vaiable, and the date for this observation
}
}

# Compile this type of outliers in a .txt file

sink(file = paste0(path_outputs,"Outliers\\outliers_spec.txt")) # Specify the path of the .txt file

print("OUTLIERS BY SERIES (IN STANDARD DEVIATIONS)")
for (s in 1:length(states)){ # Loop over the states
	for (v in 1:length(list_var_ref)){ # Loop over the variables
		if (is.na(subset(data_gtrends, geo == states[s])[1,list_var_ref[v]])== FALSE){ # If the series is not a series of missing values
		outliers_spec(states[s],list_var_ref[v],6) # Apply the function with a threshold of 6 standard deviations
		}
	}
}

sink(file = NULL)


#################################################################################################
################################## Preliminary Series Treatments ################################
#################################################################################################

# Function to compile descriptive plots for each time series

TS_Plots <- function(codegeo,var){ # Arguments: geographic code and variable
# Select a time series 
AAA <- ts(subset(data_gtrends, geo == codegeo)[,var],start=c(2004,1),end=c(2020,10),frequency=12) # Time series type
if (is.na(AAA[1]) == FALSE){ # If it is not a series of missing values
# Automatic STL decomposition of the time series
print(autoplot(stl(AAA[,1],s.window="periodic")) + ggtitle(paste0(codegeo," - ",var)) + theme_hc())
# Boxplots (over the months) to check for the seasonality of the time series
print(boxplot(AAA ~ cycle(AAA)))
# Seasonally adjusted series (STL method)
print(ggplot(fortify(stl(AAA[,1],s.window="periodic"))) +
		geom_line(aes(x=Index,y=Data),colour="green") +
		geom_line(aes(x=Index,y=trend),colour="red") +
		geom_line(data= fortify(seasadj(stl(AAA[,1],s.window="periodic"))),aes(x=Index,y=Data),size=1) +
		geom_smooth(aes(x=Index,y=Data),method="lm",se=FALSE,size=0.5) +
		ylab(var) +
		ggtitle(codegeo) +
		theme_hc())
# Seasonal Frequency: cartesian coordinates
print(ggseasonplot(seasadj(stl(AAA[,1],s.window="periodic")),12,col=rainbow(17),year.labels=TRUE) +
	ggtitle(paste0(codegeo," - ",var," - Seasonal Frequency")) + theme_hc())
# Seasonal Frequency: polar coordinates
print(ggseasonplot(seasadj(stl(AAA[,1],s.window="periodic")),12,col=rainbow(17),year.labels=TRUE,polar=TRUE) +
	ggtitle(paste0(codegeo," - ",var," - Seasonal Frequency (polar representation)")) + theme_hc())
# Plot series and ACF and PACF
print(ggtsdisplay(AAA,plot.type="partial",smooth=FALSE,theme=theme_hc(),main=paste0(codegeo," - ",var)))
}
}

########## TESTS

# Function to compile tests of stationarity for each time series

TS_Tests <- function(codegeo,var){
# Select a time series 
AAA <- ts(subset(data_gtrends, geo == codegeo)[,var],start=c(2004,1),end=c(2020,10),frequency=12) # Time series type
if (is.na(AAA[1]) == FALSE){ # If it is not a series of missing values
print(paste0("########## ",codegeo," - ",var," ##########"))
print("### Tests on raw series ###")
# ADF Test
print(adf.test(AAA)) # alternative hypothesis : stationarity
if (adf.test(AAA)$p.value < 0.05){
print("ADF Test : rejection of the null hypothesis. Stationarity supposed.")
} else {print("ADF Test : null hypothesis not rejected. Stationarity rejected.")}
# PP Test
print(pp.test(AAA)) # alternative hypothesis : stationarity
if (pp.test(AAA)$p.value < 0.05){
print("PP Test : rejection of the null hypothesis. Stationarity supposed.")
} else {print("PP Test : null hypothesis not rejected. Stationarity rejected.")}
# KPSS Test
print(kpss.test(AAA)) # alternative hypothesis : presence of a unit root
if (kpss.test(AAA)$p.value < 0.05){
print("KPSS Test : rejection of the null hypothesis. Presence of a unit root. Stationarity rejected.")
} else {print("KPSS Test : null hypothesis not rejected. Stationarity supposed.")}

# If the series is not stationary
if (ndiffs(AAA,type="trend",max.d=11) > 0){
# Number of differences to have a stationary time series (ADF Test, PP Test, KPSS Test)
# deterministic component in the regression : trend
print(paste0("Number of differences to have a stationary time series: ",ndiffs(AAA,type="trend",max.d=11)))
# Differenciation for the number of times indicated by previous function ndiffs
d_AAA <- diff(AAA,1,ndiffs(AAA,type="trend",max.d=11))
print("### Tests on differentiated series ###")
print(adf.test(d_AAA)) # alternative hypothesis : stationarity
# ADF Test
if (adf.test(d_AAA)$p.value < 0.05){
print("ADF Test : rejection of the null hypothesis. Stationarity supposed.")
} else {print("ADF Test : null hypothesis not rejected. Stationarity rejected.")}
# PP Test
print(pp.test(d_AAA)) # alternative hypothesis : stationarity
if (pp.test(d_AAA)$p.value < 0.05){
print("PP Test : rejection of the null hypothesis. Stationarity supposed.")
} else {print("PP Test : null hypothesis not rejected. Stationarity rejected.")}
# KPSS Test
print(kpss.test(d_AAA)) # alternative hypothesis : presence of a unit root
if (kpss.test(d_AAA)$p.value < 0.05){
print("KPSS Test : rejection of the null hypothesis. Presence of a unit root. Stationarity rejected.")
} else {print("KPSS Test : null hypothesis not rejected. Stationarity supposed.")}

print("### Automatic procedure to fit a SARIMA model ###")
# Fitting a SARIMA model to the differentiated series
print(auto.arima(d_AAA))

} else {
print("### Automatic procedure to fit a SARIMA model ###")
# Fitting a SARIMA model to the series
print(auto.arima(AAA))}

}
}

# Complie the plots in several .pdf files
# Compile the tests in several .txt files

for (v in 1:length(list_var_ref)){ # Loop over the variables
	pdf(paste0(list_var_ref[v],".pdf")) # One .pdf file per variable
	for (s in 1:length(states)){ # Loop over the states
		TS_Plots(states[s],list_var_ref[v]) # Apply the function
	}
	dev.off()
	sink(file = paste0(path_outputs,"Tests\\",list_var_ref[v],".txt")) # One .txt file per variable
	for (s in 1:length(states)){ # Loop over the states
		TS_Tests(states[s],list_var_ref[v]) # Apply the function
	}
	sink(file = NULL)
}
	






