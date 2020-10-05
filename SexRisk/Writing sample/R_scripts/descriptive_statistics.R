##################################################################################
#####                             1 Loading the data                         #####
#####                             1.1 The packages                           #####
##################################################################################

if(!require(readstata13)) install.packages("readstata13", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

##################################################################################
#####                             1.2 The datasets                           #####
##################################################################################


# Female population in Ecuador ----------
url <- "https://raw.githubusercontent.com/aquijanoruiz/HarvardX_capstone/master/SexRisk/database/women_population.csv"
women_population <- read_csv(url)

# The code below filters the data we need, female population of ages 10 to 14 and 15 to 19 from 2010 to 2019
women_pop_10to19 <- women_population %>% filter(age == "10 - 14" | age == "15 - 19") %>% select(-c(`2020`, "age"))
women_pop_10to19 <- cbind(data.frame(year = as.numeric(colnames(women_pop_10to19))), 
                          data.table::transpose(women_pop_10to19))
colnames(women_pop_10to19)[2:3] <- c("pop_10to14", "pop_15to19")

# Number of births 
url <- "https://raw.githubusercontent.com/aquijanoruiz/HarvardX_capstone/master/SexRisk/database/ecu_births.csv"
births <- read_csv(url)

# The code below filters the data we need, births from females of ages 10 to 14 and 15 to 19, and sums the number of births for each age group
births_10to19 <- data.frame(year = births$year, 
                            births_10to14 = rowSums(births[,as.character(c(10:14))], na.rm = TRUE),
                            births_15to19 = rowSums(births[,as.character(c(15:19))], na.rm = TRUE)) %>%
  filter(year >= 2010)

# We combine the two data sets into a single one, and calculate the prevalence (number of births per 1000 women)
teenage_preg_rate <- left_join(births_10to19, women_pop_10to19, by = "year")
teenage_preg_rate <- teenage_preg_rate %>% mutate(prevalence_10to14 = births_10to14/pop_10to14 * 1000,
                                                  prevalence_15to19 = births_15to19/pop_15to19 * 1000) 

##################################################################################
#####                             2.1 Graphs                                 #####
##################################################################################

# We transform the wide data to long data and create a graph
teenage_preg_rate %>% select(year, prevalence_10to14, prevalence_15to19) %>% 
  dplyr::rename(`15 to 19 years old` = prevalence_15to19,
                `10 to 14 years old` = prevalence_10to14) %>%
  gather(age, prevalence, -year) %>% group_by(age) %>% 
  ggplot(aes(x = year, y = prevalence)) + geom_point() + geom_line() +
  facet_wrap(~age, nrow = 2, scales = "free") + scale_x_continuous(breaks = 2010:2019) + 
  labs(x = "", y = "", title = "Number of births per 1000 girls", caption = "Source: National Institute of Statistics and Census of Ecuador") + theme_bw() + 
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"))


