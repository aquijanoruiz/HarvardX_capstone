##################################################################################
#####                             1 Loading the data                         #####
#####                             1.1 The packages                           #####
##################################################################################

if(!require(readstata13)) install.packages("readstata13", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

##################################################################################
#####                             1.2 The datasets                           #####
##################################################################################

# Number of births 
url <- "https://raw.githubusercontent.com/aquijanoruiz/HarvardX_capstone/master/SexRisk/database/ecu_births.csv"
births <- read_csv(url)

url <- "https://raw.githubusercontent.com/aquijanoruiz/HarvardX_capstone/master/SexRisk/database/ecu_population.csv"
population <- read_csv(url)

##################################################################################
#####                             1.2 The datasets                           #####
##################################################################################