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
install.packages("cli")
library(cli)

#################################################################################################
##################################     Environment cleaning      ################################
#################################################################################################

#rm(list=ls())

#################################################################################################
##################################      Working Environment      ################################
#################################################################################################

# Specify the different paths

# For the working directory
paths <- c("C:\\Users\\Mathis\\Dropbox\\Divorce\\Code","C:\\Users\\sansu_mat\\Dropbox\\Divorce\\Code","C:\\Users\\Mathis\\Desktop\\Ined\\Divorce\\Code", "XXXXX", "XXXXX")
# For the data imports or exports
paths_data <- c("C:\\Users\\Mathis\\Dropbox\\Divorce\\Data\\","C:\\Users\\sansu_mat\\Dropbox\\Divorce\\Data\\","C:\\Users\\Mathis\\Desktop\\Ined\\Divorce\\Data\\", "XXXXX", "XXXXX")
# For the outputs
paths_outputs <- c("C:\\Users\\Mathis\\Dropbox\\Divorce\\Outputs\\","C:\\Users\\sansu_mat\\Dropbox\\Divorce\\Outputs\\","C:\\Users\\Mathis\\Desktop\\Ined\\Divorce\\Outputs\\", "XXXXX", "XXXXX")

# Choose the different paths
# 1 = Mathis, Dropbox personal computer
# 2 = Mathis, Dropbox INED computer
# 3 = Mathis, Local personal computer
# 4 = Anne
# 5 = Marion

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

# States (+ District of Columbia) list
states <- c("US_AL","US_AK","US_AZ","US_AR","US_CA","US_CO","US_CT","US_DE","US_DC","US_FL","US_GA","US_HI","US_ID","US_IL","US_IN","US_IA","US_KS","US_KY","US_LA","US_ME","US_MD","US_MA","US_MI","US_MN","US_MS","US_MO","US_MT","US_NE","US_NV","US_NH","US_NJ","US_NM","US_NY","US_NC","US_ND","US_OH","US_OK","US_OR","US_PA","US_RI","US_SC","US_SD","US_TN","US_TX","US_UT","US_VT","US_VA","US_WA","US_WV","US_WI","US_WY")

##################################################
###### CDC-NVSS DATA DIVORCE RATES BY STATE 2019
##################################################

# Corresponding file
data_div_19 <- read_excel(paste0(path_data,"Ground\\divorce\\divorce_rates_us_2019.xlsx"))
# Set the geographic codes
data_div_19[1:52,1] <- c("geo","US_AL","US_AK","US_AZ","US_AR","US_CA","US_CO","US_CT","US_DE","US_DC","US_FL","US_GA","US_HI","US_ID","US_IL","US_IN","US_IA","US_KS","US_KY","US_LA","US_ME","US_MD","US_MA","US_MI","US_MN","US_MS","US_MO","US_MT","US_NE","US_NV","US_NH","US_NJ","US_NM","US_NY","US_NC","US_ND","US_OH","US_OK","US_OR","US_PA","US_RI","US_SC","US_SD","US_TN","US_TX","US_UT","US_VT","US_VA","US_WA","US_WV","US_WI","US_WY")
data_div_19[1,2] <- "div_rate"
# Formating the divorce dataframe so that it merges with the database
setnames(data_div_19,c(names(data_div_19)),as_vector(data_div_19[1,]))
data_div_19 <- data_div_19[-c(1),]
data_div_19[data_div_19 == "---"] <- NA
data_div_19$div_rate <- as.numeric(data_div_19$div_rate)
data_div_19$year <- 2019

# Divorce dataframe
#View(data_div_19)

##################################################
###### Join Aggregated data and actual data
##################################################

data_pred_19 <- read_csv(paste0(path_data,"Database\\data_pred.csv"))
for (s in 1:length(states)){
data_pred_19[data_pred_19$geo == states[s] & data_pred_19$year == 2019, "div_rate"] <- data_div_19[data_div_19$geo == states[s] & data_div_19$year == 2019, "div_rate"]
}
#View(data_pred_19)

#################################################################################################
##################################         Export Data           ################################
#################################################################################################

write_dta(data_pred_19, paste0(path_data,"Database\\data_pred_19.dta"))
write_csv(data_pred_19, paste0(path_data,"Database\\data_pred_19.csv"))

