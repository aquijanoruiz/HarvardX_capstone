#####################################################################
####                       1 Loading the data                    ####
#####################################################################

####                            WARNING                          ####

# During my work on this project, the government updated the URL to 
# the databases several times. I have uploaded the datasets to my 
# Github account. However, I have also left the URL of the statistics
# bureau website in case you want to check it. Thank you!

#####################################################################
####                      1.1 The packages                       ####
#####################################################################

if(!require(readstata13)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

#####################################################################
####                   1.2 Downloading the data                  ####
#####################################################################

# We will download the files and save them on our system with the names: people, women, house, and behavior
# 1) The people dataset will contain the demographic and economic data of each of the members of the 
# household, contained in the "1_BDD_ENS2018_f1_personas.dta" file
# 2) The house dataset will contain the data about the house the household lives in, contained in the 
# "2_BDD_ENS2018_f1_hogar.dta" file
# 3) The women dataset will contain the data about the sexual health of women from 10 to 49, contained in the 
# "4_BDD_ENS2018_f2_mef.dta" file
# 4) The behavior dataset will contain the data about behavioral risk factors of people from 5 to 18,
# contained in the "8_BDD_ENS2018_f4_fact_riesgo.dta" file

# This is the original url. However, as we are not sure whether the url will be stil available by the time of
# the assessment of this project. We wil use the Github url.
org.url <- "https://www.ecuadorencifras.gob.ec/documentos/web-inec/Estadisticas_Sociales/ENSANUT/ENSANUT_2018/BDD_ENSANUT_2018_STATA_.zip"
# We give the url a name
url <- "https://github.com/aquijanoruiz/HarvardX_capstone/raw/master/SexRisk/BDD_ENSANUT_2018_STATA.zip"
# We create a temporary directory
td <- tempdir()
# We create the placeholder file
tf <- tempfile(tmpdir=td, fileext = ".zip")
# We download the data into the placeholder file
download.file(url,tf)

# We get the name of the file inside the zip file that contains the demographic and economic data, 
# unzip it, get the full path name of it, and finally load it
people.f.name <- unzip(tf, list=TRUE)$Name[2] # The people dataset is the number 2
unzip(tf, files=people.f.name, exdir=td, overwrite=TRUE)
people.f.path <- file.path(td, people.f.name)
people <- read.dta13(people.f.path)

# Now, we need to do the same for the house, women, and behavior datasets
house.f.name <- unzip(tf, list=TRUE)$Name[3] # The house dataset is the number 3
women.f.name <- unzip(tf, list=TRUE)$Name[5] # The women dataset is the number 5
behavior.f.name <- unzip(tf, list=TRUE)$Name[9] # The behavior dataset is the number 9

unzip(tf, files=c(house.f.name, women.f.name, behavior.f.name), exdir=td, overwrite=TRUE)
house.f.path <- file.path(td, house.f.name)
women.f.path <- file.path(td, women.f.name)
behavior.f.path <- file.path(td, behavior.f.name)

# Now, we can load the three files
house <- read.dta13(house.f.path)
women <- read.dta13(women.f.path)
behavior <- read.dta13(behavior.f.path)

# As these are STATA files, the label of each of the variables is stored inside the datasets,
# we can extract them using the following code
data.key.people <- data.frame(variable = names(people), 
                              label = attr(people,"var.labels"))

data.key.house <- data.frame(variable = names(house), 
                             label = attr(house,"var.labels"))

data.key.women <- data.frame(variable = names(women), 
                             label = attr(women,"var.labels"))

data.key.behavior <- data.frame(variable = names(behavior), 
                             label = attr(behavior,"var.labels"))

# If you know how to read Spanish, you can check what each variable means. Just use the View()
# fucntion like this: View(data.key.people) 
# For example, let's look at the first 20 variables of the people set and their labels
head(data.key.people, 20) # Can you read Spanish?

# The name of each variable is assigned according to its code in the survey. For example, 
# whether a woman between 12 and 49 has ever had  intercourse can be found in variable f2_s8_803
# of the women set, which means form 2, section 8, question 803
summary(women$f2_s8_803)

# We can see that the questions and the answers are all provided in Spanish. This should be
# no surprise as the official language in Ecuador is Spanish. For your better understanding, 
# I will recode every variable of interest from Spanish into English

#####################################################################
####                       2 Data wrangling                      ####
####                2.1 Extracting the mothers' Ids              ####
#####################################################################

# We have loaded four set: people, house, women, and behavior. Our variables of interest
# are scattered in all these datasets. We need to create a singe dataset with all these
# variables. For now, we will create two datsets: the daughters and the mothers set.

# Who is whose mother? Daughters and mothers are in the same dataset in the people set, but
# as a different observarion. Let's extract the Ids of each person surveyed and the variables
# we will use to link the data.
people <- people %>% mutate(id.household = id_hogar, # Let's first change the names into English
                            id.subject = id_per,
                            persona = as.integer(persona),
                            sex = revalue(sexo, c("hombre"="male", "mujer"="female")),
                            mother.code = f1_s2_15_1,
                            father.code = f1_s2_14_1,
                            age = f1_s2_3_1)

people.id <- people %>% select(id.household, id.subject, persona, sex, age, mother.code, father.code)

head(people.id, 10)

# Now, let's look for example at people in household 010150000201011
people.id %>% filter(id.household == "010150000201011")

# We can see that there are six members, 1 is the father, 2 is the mother, and 3 to 6 are the
# children (all males...). As the numebrs 1, 2, 3, etc. are repeated all over the dataset 
# and not unique Ids, we cannot work with this data set simply as it is. To be able to work with 
# it, we need to add two columns to each observation with the fathers' and the mothers' unique Ids.
# We actually only need the mothers's unique Ids, but we will take both for explanatory purposes.

# We can write the code like this:
id.mothers <- people.id %>% group_by(id.household) %>% slice(mother.code) %>%
  distinct(persona, .keep_all = TRUE) %>% ungroup() %>%
  select(-mother.code) %>% mutate(id.mother = id.subject, mother.code = persona) %>%
  select(id.household, id.mother, mother.code)

id.fathers <- people.id %>% group_by(id.household) %>% slice(father.code) %>%
  distinct(persona, .keep_all = TRUE) %>% ungroup() %>%
  select(-father.code) %>% mutate(id.father = id.subject, father.code = persona) %>%
  select(id.household, id.father, father.code)

# Combining the datasets:
people.id <- left_join(people.id, id.mothers, by = c("id.household" = "id.household", 
                                                     "mother.code" = "mother.code")) %>%
             left_join(id.fathers, by = c("id.household" = "id.household", 
                                         "father.code" = "father.code"))

# Now, let's look again at people in household 010150000201011. We have added the mothers' and the
# fathers' Ids.
people.id %>% filter(id.household == "010150000201011")

#####################################################################
####               2.2 Combining all the datasets                ####
#####################################################################

# The dataset we will mostly work with is the women dataset. However, There are some variables in the women 
# set that also appear in the people, house, and behavior sets. We will delete them so that we do not have 
# repeated variables when combining the datasets. Note that you do not need to know now what each of the
# following variables represent.

women <- women %>% dplyr::rename(id.subject = id_per,
                                id.household = id_hogar)

people <- people %>% select(-c("area", "prov", "upm", "id_viv", "persona", "fecha_anio", 
                               "fecha_mes", "fecha_dia", "region", "etnia", "edadanios", 
                               "gedad_anios", "nivins", "nbi_1", "nbi_2", "escolaridad", 
                               "fexp", "estrato", "id_hogar"))

house <- house %>% select(-c("area", "prov", "upm", "id_viv", "fecha_anio", "fecha_mes", 
                             "fecha_dia", "region", "fexp", "estrato")) %>%
                   dplyr::rename(id.household = id_hogar)
  
behavior <- behavior %>% select(-c("area", "prov", "upm", "id_viv", "id_hogar", "persona", 
                                   "sexo", "region", "etnia", "edadanios", "gedad_anios", 
                                   "nivins", "fexp", "estrato")) %>%
                         dplyr::rename(id.subject = id_per)

people.id <- people.id %>% select(-c("id.household", "persona", "sex", "age", "mother.code", 
                                     "father.code"))

# We will called data to the comnbined dataset. It is constructed by combining the women, people, house, 
# behavior, and people.id sets. Why people.id? Because we later need to create another dataset with the 
# mothers' data, and we need to filter according the daughters included in our analysis.

data <- women %>% left_join(people %>% select(-id.household) , by = "id.subject") %>% 
  left_join(people.id, by = "id.subject") %>% # we add id.mother and id.father
  left_join(house, by = "id.household") %>% # we add the data about the house
  left_join(behavior, by = "id.subject") # we add the behavioral variables

dim(data) # Let's look at how many observarions and variables we have (a lot of variables...)
# At the end, we won't need most of them so we will delete them.

#####################################################################
####              2.3 Creating the daughters set                #####
#####################################################################

# We have joined all the datasets. Now, we will filter only the women from 12 to 18
daughters <- women %>% left_join(people %>% select(-id.household) , by = "id.subject") %>% 
  left_join(people.id, by = "id.subject") %>% # we add id.mother and id.father
  left_join(house, by = "id.household") %>% # we add the data about the house
  left_join(behavior, by = "id.subject") %>% # we add the behavioral variables
  filter(between(age,12,18))

# Note that this won't be the final set we will use. We still need to work a lot on it!
nrow(daughters) # We can look at how many subjects we will use to build the algorithm

#####################################################################
####              2.4 Creating the mothers set                  #####
#####################################################################

# We filter the data including only all those poeple who are the mothers of those subjects in our analysis
mothers <- people %>% left_join(women %>% select(-id.household) , by = "id.subject") %>% 
  left_join(people.id, by = "id.subject") %>% # we add id.mother and id.father
  left_join(house, by = "id.household") %>% # we add the data about the house
  left_join(behavior, by = "id.subject") %>%
  semi_join(daughters, by = c("id.subject" = "id.mother"))

nrow(mothers) # We can look at how many mothers there are

#####################################################################
####                        3. The variables                    #####
#####################################################################

# The next step is to open the questionnaires and check what variables we want to use to build the algorithm,
# look at the code of the question we want to add, and translate it into English.

# We previosly explained how the variables' names were constructed. For example, the variable f2_s8_803 means
# form 2, section 8, question 803, and it referes to wether a woman has ever had intercourse. We will quickly
# rename each variable of interest from English into Spanish, without explaining each detail of the questions
# If you want to know more about the census used to contruct these datasets, please go to the following site
#https://www.ecuadorencifras.gob.ec/salud-salud-reproductiva-y-nutricion/ and check the methodology.

#####################################################################
####                  3.1 The explanatory variables             #####
####                  3.1.1 Demographic variables               #####
#####################################################################

# age ------------------------------
summary(daughters$age)

# area ------------------------------
daughters$area <- factor(revalue(daughters$area, c("urbano" = "urban")),
                         levels = c("urban", "rural"))
summary(daughters$area)

# ethnicity ------------------------------
daughters$ethnicity <- daughters$f1_s2_9
levels(daughters$ethnicity) <- c("indigenous","african ecuadorian", "black", "mulatto", 
                                 "montuvio", "mestizo", "white", "other")

summary(daughters$ethnicity)

# m.age and d.m.diff.age ------------------------------
# These are the age of the mother and the age difference between the mother and the daughter.
# We first create the m.age variable in the mothers set and then move it into the daughters set to compute
# the age difference between the mother and the daughter

m.age <- mutate(mothers, m.age = mothers$age) %>% 
         select(id.subject, m.age)

daughters <- left_join(daughters, m.age, by = c("id.mother" = "id.subject")) %>%
  mutate(d.m.age.diff = m.age - age)

hist(daughters$m.age)
hist(daughters$d.m.age.diff)

# m.num.children ------------------------------
mothers$m.num.children <- mothers$f2_s2_217_4
hist(mothers$m.num.children)

# The question f2_s2_217_4 asks for all the children given birth by a woman (curently dead or alive,
# living with the household or not). However, not all women are included in the form 2. Hence, all the
# missing values. For the missing mothers, we approximate to the number of children living in the house 
# which is given in form 1.
sum(is.na(mothers$m.num.children)) # We can look at the number of NAs

m.num.children.house <- people.id %>% group_by(id.mother) %>% dplyr::summarize(m.num.children.house = n())
mothers <- mothers %>% left_join(m.num.children.house, by = c("id.subject" = "id.mother"))
mothers <- mothers %>% mutate(m.num.children = ifelse(is.na(m.num.children), m.num.children.house, 
                                                      m.num.children))
sum(is.na(mothers$m.num.children))
hist(mothers$m.num.children) # Now we have no NAs

#####################################################################
####          3.1.2 Economic and social variables               #####
#####################################################################

# num.bedrooms ------------------------------
# Number of bedrooms in the house
daughters$num.bedrooms <- daughters$f1_s1_8
hist(daughters$num.bedrooms)

# internet ------------------------------
# Whether there is internet access in the house
daughters$internet <- revalue(daughters$f1_s1_42, c("si"="yes"))
summary(daughters$internet)

# cellphone ------------------------------
# Whether the daughter has an activated cellphone
daughters$cellphone <- revalue(daughters$f1_s2_23, c("si"="yes"))
summary(daughters$cellphone)

# transfer ------------------------------
# Whether someone in the household receives an economic transfer from the government, known in Ecuador
# as "bono de desarrollo humano"
transfer <- people %>% select(id.household, f1_s3_27) %>% filter(f1_s3_27 == "si") %>%
  dplyr::rename(transfer = f1_s3_27)

daughters <- daughters %>% left_join(transfer, by = "id.household")
daughters$transfer[which(is.na(daughters$transfer))] <- "no"
summary(daughters$transfer)

# m.job ------------------------------
# Whether the mother has a job or no
mothers$m.job <- mothers$f1_s3_1
levels(mothers$m.job) <- c("yes", "no")
summary(mothers$m.job)

# m.live.house ------------------------------
# Whether the mother lives in the house
daughters$m.live.house <- revalue(daughters$f1_s2_15, c("si" = "yes"))
summary(daughters$m.live.house)

# f.live.house ------------------------------
# Whether the father lives in the house
daughters$f.live.house <- revalue(daughters$f1_s2_14, c("si" = "yes"))
summary(daughters$f.live.house)

#####################################################################
####               3.1.3 Educational variables                  #####
#####################################################################

# attend.school ------------------------------
# Whether the daughter attends school
daughters$attend.school <- revalue(daughters$f1_s2_17, c("si"="yes"))
summary(daughters$attend.school)

table(daughters$attend.school, daughters$age) # a huge portion of people at age 18 do not go to school
# maybe it is because the already finished it

# m.education ------------------------------
# Mother's education attainment (none, primary school, secondary school, university)
mothers$m.education <- mothers$f1_s2_19_1
levels(mothers$m.education) <- c("none", "none", "none", "primary school", "primary school", "secondary school", 
                                 "secondary school", "university", "university", "university")
summary(mothers$m.education)

# contraception.info ------------------------------
# Whether the daughter has ever received information about contraception
daughters$contraception.info <- factor(revalue(daughters$f2_s8_800f,c("si"="yes")),
                                       levels = c("yes","no"))
summary(daughters$contraception.info)

# contraception.info.family ------------------------------
# Whether the daughter has learned about contraception mainly from herfamily
daughters$contraception.info.family <- ifelse(daughters$f2_s8_801f == "familiar?", "yes", "no")
daughters$contraception.info.family[which(is.na(daughters$contraception.info.family))] <- "no"
daughters$contraception.info.family <- factor(daughters$contraception.info.family, levels = c("yes","no"))
summary(daughters$contraception.info.family)

# contraception.info.school ------------------------------
# Whether the daughter has learned about contraception mainly fromschool
daughters$contraception.info.school <- ifelse(daughters$f2_s8_801f == "escuela/colegio?", "yes", "no")
daughters$contraception.info.school[which(is.na(daughters$contraception.info.school))] <- "no"
daughters$contraception.info.school <- factor(daughters$contraception.info.school, levels = c("yes","no"))
summary(daughters$contraception.info.school)

# period.info ------------------------------
# Whether the daughter knew about menstruation when she had her first period
daughters$period.info <- factor(revalue(daughters$f2_s8_841,c("si"="yes",
                                                              "no sabe/ no responde"="doesn't know")),
                                levels = c("yes","no", "doesn't know"))
table(daughters$period.info)

# pregnancy.info ------------------------------
# Whether the daughter can correctly answer the question: can a women get pregnantat first intercourse?
# (We consider as "no" people who do not know the answer to this question)
daughters$pregnancy.info <- factor(revalue(daughters$f2_s8_845,c("si"="yes",
                                                                 "no sabe/ no responde" = "no")),
                                   levels = c("yes","no"))
summary(daughters$pregnancy.info)
# aids.info ------------------------------
# Whether the daughter can correctly answer the question: can AIDS be transmited throughhandshake?
daughters$aids.info <- factor(revalue(daughters$f2_s10_1011_1,
                                      c("si"="yes","no sabe/ no responde"="yes")),
                              levels = c("yes","no"))
table(daughters$aids.info)

# m.pregnancy.info ------------------------------
# Whether the mother can correctly answer the question: can a women get pregnantat first intercourse?
mothers$m.pregnancy.info <- factor(revalue(mothers$f2_s8_845,c("si"="yes",
                                                                   "no sabe/ no responde" = "no")),
                                       levels = c("yes","no"))

summary(mothers$m.pregnancy.info)

# m.aids.info  ------------------------------
# Whether the daughter can correctly answer the question: can AIDS be transmited throughhandshake?
mothers$m.aids.info <- factor(revalue(mothers$f2_s10_1011_1,
                                      c("si"="yes","no sabe/ no responde"="yes")),
                              levels = c("yes","no"))
summary(mothers$m.aids.info)

#####################################################################
####               3.1.4 Behavioral variables                   #####
#####################################################################

# alcohol  ------------------------------
# Whether the daughter has ever drunk alcohol
daughters$alcohol <- revalue(daughters$f4_s5_500, c("si" = "yes"))
summary(daughters$alcohol)

# alcohol.recent ------------------------------
# Whether the daughter has drunk alcohol in the past 30 days
daughters$alcohol.recent <- factor(ifelse(daughters$f4_s5_502 >=1, "yes", "no"),
                                   levels = c("yes", "no"))
summary(daughters$alcohol.recent)

# smoke ------------------------------
# Whether the daughter has ever smoked
daughters$smoke <- revalue(daughters$f4_s6_600, c("si" = "yes"))
summary(daughters$smoke)

# smoke.recent ------------------------------
# Whether the daughter has smoked in the past 30 days
daughters$smoke.recent <- revalue(daughters$f4_s6_611, c("si" = "yes"))
daughters$smoke.recent[which(daughters$smoke.recent == "no sabe/no responde")] <- NA
daughters$smoke.recent <- factor(daughters$smoke.recent, levels = c("yes", "no"))
summary(daughters$smoke.recent)

# m.age.first.intercourse ------------------------------
# Mother's age at first intercourse
mothers$m.age.first.intercourse <- ifelse(mothers$f2_s8_831 == 88 |
                                            mothers$f2_s8_831 == 77 |
                                            mothers$f2_s8_831 == 99, NA, mothers$f2_s8_831)
hist(mothers$m.age.first.intercourse)

# m.contraception ------------------------------
# Mother's use of contraception (is using, has ever used, has never used)
mothers$m.contraception <- mothers$f2_s6_604
levels(mothers$m.contraception) <- c("is using", "has ever used", "has never used")
summary(mothers$m.contraception)

#####################################################################
####               3.1.4 The outcome variable                   #####
#####################################################################

# The outcome variable will be the type of sexual behavior of the daughter, a categorical variable 
# defined as: risky or not risky. To obtain the outcome variable we first need to analyze the behavioral
# characteristics of the daughter

# intercourse ------------------------------
# Whether the daughter has ever had intercourse
daughters$intercourse <- factor(revalue(daughters$f2_s8_803,c("si"="yes")),
                                    levels = c("yes","no"))
summary(daughters$intercourse)

# contraception ------------------------------
# Daughter's use of contraception (is using, has ever used, has never used)
daughters$contraception <- daughters$f2_s6_604
levels(daughters$contraception) <- c("is using", "has ever used", "has never used")
summary(daughters$contraception)

# Many women who have never used contraception have never had done so because they have never had 
# intercourse. However, some of them have never used contraception despite having had intercourse.
table(daughters$intercourse,daughters$contraception,
      dnn = c("intercourse", "contraception"))

# We want to differentiate those who have had intercourse but never used contraception and those who 
#have neverused contraception because they have never needed to
daughters <- daughters %>% mutate(contraception = factor(ifelse(contraception == "has never used" & intercourse == "no",
                                                                "no sex", contraception),
                                                         levels = c("is using", "has ever used", "has never used", "no sex")))
table(daughters$intercourse,daughters$contraception,
      dnn = c("intercourse", "contraception")) # no inconsistencies

# contraception.no.use.reason ------------------------------
# Reason why the daughter does not use contraception (wants to get pregnant, postnatal, no sex life,
# doesn't like it, afraid of side effects, had side effects, partner doesn't like it, feels embarrassed,
# because of economic reasons, no knowledge about contraception, religious reasons, others, doesn't know )
daughters$contraception.no.use.reason <- daughters$f2_s6_613
levels(daughters$contraception.no.use.reason) <- 
  c("wants to get pregnant","postnatal","no sex life", "because of age", "doesn't like it", 
    "afraid of side effects", "had side effects", "partner doesn't like it", "feels embarrassed", 
    "feels embarrassed", "because of economic reasons", "no knowledge about contraception", 
    "religious reasons", "others" , "doesn't know")
summary(daughters$contraception.no.use.reason)

# contraception.first ------------------------------
# Whether the daughter used contraception at first intercourse
daughters$contraception.first <- as.character(revalue(as.factor(daughters$f2_s8_808),
                                                      c("1"="yes","2"="no")))

# This is a conditional question. People who were asked this question were the ones who claimed having
# had intercourse. We have lots of NAs, some belong to those who have never had intercourse, and some 
# to those who claimed having had intercourse but did not provide information about contraception
# We need to fix those NAs
daughters$contraception.first <- ifelse (daughters$intercourse == "no" & 
                                         is.na(daughters$contraception.first), 
                                         "no sex", daughters$contraception.first)
daughters$contraception.first <- factor(daughters$contraception.first,levels = c("yes","no","no sex"))
summary(daughters$contraception.first)

# contraception.last ------------------------------
# Whether the daughter used contraception at last intercourse
daughters$contraception.last <- as.character(revalue(as.factor(daughters$f2_s8_834),
                                                     c("si"="yes")))
# We do the same as we did with contraception.first. We change the NAs into "no sex"
daughters$contraception.last <- ifelse (daughters$intercourse == "no" & 
                                        is.na(daughters$contraception.last), 
                                        "no sex", daughters$contraception.last)
daughters$contraception.last <- factor(daughters$contraception.last,levels = c("yes","no","no sex"))
summary(daughters$contraception.last)

# pregnant ------------------------------
# Whether the daughter has ever been pregnant
daughters$pregnant <- revalue(daughters$f2_s2_207,c("si"="yes"))
summary(daughters$pregnant)

#####################################################################
####                    4. Putting it all together              #####
####       4.1 Checking for inconsistencies and errors          #####
#####################################################################

# Now, we need to combine the two data sets (daughters and mothers set) into one. Before that, we 
# need to check for inconsistencies and erros in the data

# Some women were not willing to provide information their sexual behavior. We need to remove their data
sum(is.na(daughters$intercourse))

# We have missing contraception data of many of the women who claimed having had intercourse, and they
# were erroneously marked as not having had intercourse. 
table(daughters$contraception.first,daughters$contraception.last, 
      dnn = c("first","last"))

# Some women claimed not having had intercourse, but claimed being using contraception or having 
# used contraception.
table(daughters$intercourse, daughters$contraception,
      dnn = c("intercourse", "contraception"))

# These is missing data about pregnancy.
sum(is.na(daughters$pregnant))

# A few declared being pregnant but never had sexual intercourse
table(daughters$intercourse, daughters$pregnant,
      dnn = c("intercourse", "pregnant"))

#####################################################################
####          4.2 Removing inconsistencies and errors           #####
#####################################################################

# Now, we need to filter the data to keep only what we want:

daughters <- daughters %>%
  filter(m.live.house == "yes") %>% # we select the daughters whose mother lives in the house
  filter(!is.na(intercourse)) %>% # we remove missing data about intercourse
  # we remove those who have had intercourse but do not want to provide information whether they used 
  # contraception at first or last intercourse
  filter(!(intercourse == "yes" & is.na(contraception.first))) %>%  
  filter(!(intercourse == "yes" & is.na(contraception.last))) %>%   
  # we also remove other inconsistencies such as those who claimed not having had intercourse 
  # but claimed being using contraception or having used contraception
  filter(!(intercourse == "no" & contraception == "is using")) %>%
  filter(!(intercourse == "no" & contraception == "has ever used")) %>%
  # we remove data of those who claimed having been pregnant but never had sexual intercourse
  filter(!(intercourse == "no" & pregnant == "yes")) %>%
  filter(!is.na(pregnant)) # we remove missing data about pregnancy

# We can check now on the data:
sum(is.na(daughters$intercourse)) # no NAs

table(daughters$contraception.first,daughters$contraception.last, 
      dnn = c("first","last")) # no inconsistencies
  
table(daughters$intercourse, daughters$contraception,
      dnn = c("intercourse", "contraception")) # no inconsistencies

sum(is.na(daughters$pregnant)) # no NAs

table(daughters$intercourse, daughters$pregnant,
      dnn = c("intercourse", "pregnant")) # no inconsistencies

#####################################################################
####              4.3 Keeping only what we want                 #####
#####################################################################

# daughters data ------------------------------
daughters <- daughters %>% 
  select(id.subject, id.household, id.mother, contraception, contraception.no.use.reason, 
         contraception.first, contraception.last, pregnant, age, area, ethnicity, m.age,
         d.m.age.diff, num.bedrooms, internet, cellphone, transfer, f.live.house, attend.school, 
         contraception.info, contraception.info.family, contraception.info.school, 
         period.info, pregnancy.info, aids.info, intercourse, alcohol, alcohol.recent, 
         smoke, smoke.recent)

# mothers data ------------------------------
mothers <- mothers %>% 
  select(id.subject, m.num.children, m.job, m.education, m.pregnancy.info, m.aids.info, 
         m.age.first.intercourse, m.contraception)

#####################################################################
####                4.4 Putting it all together                 #####
#####################################################################

preg.risk <- daughters %>% left_join(mothers, by = c("id.mother" = "id.subject"))

#####################################################################
####             5 Constructing the outcome variable            #####
#####################################################################

# We want to predict if the woman is at risk of pregnancy or is already pregnant

# 1. The woman has ever been pregnant:risk
# 2. The woman has ever had intercourse and never used contraception:risk
# 3. The woman did not use contraception at first intercourse:risk
# 4. The woman did not use contraception at last intercourse:risk
# 5. The woman wouldn't use contraception because does not like it:risk
# 6. The woman wouldn't use contraception because is afraid of side effects:risk
# 7. The woman wouldn't use contraception because the partner doesn't like it:risk
# 8. The woman wouldn't use contraception because she feels embarrassed:risk
# 9. The woman wouldn't use contraception because of economic reasons:risk
# 10. The wouldn't use contraception because she has no knowledge about contraception:risk
# 11. The woman wouldn't use contraception because of other reasons or simply does not know why:risk
# 12. The rest:no risk

# We can observe why women wouldn't be willing to use contraception
table(preg.risk$intercourse, preg.risk$contraception.no.use.reason,
      dnn = c("has had inercourse", "reason why wouldn't use contraception"))

# People who used contraception at first and last intercourse and claim to be currently using 
# contraception are considered not at risk
table(preg.risk$contraception.first, preg.risk$contraception.first,
      dnn = c("used contraception at first intercourse", "used contraception at last intercourse"))

preg.risk <- preg.risk %>% 
  mutate(risk = ifelse(pregnant == "yes", "risk", # 1
                       ifelse(contraception == "has never used", "risk", # 2
                              ifelse(contraception.first == "no", "risk", # 3
                                     ifelse(contraception.last == "no", "risk", # 4
                                            ifelse(contraception.no.use.reason == "doesn't like it" | # 5
                                                     contraception.no.use.reason == "afraid of side effects" | # 6 
                                                     contraception.no.use.reason == "partner doesn't like it" | # 7
                                                     contraception.no.use.reason == "feels embarrassed" | # 8
                                                     contraception.no.use.reason == "because of economic reasons" | # 9
                                                     contraception.no.use.reason == "no knowledge about contraception" | #10
                                                     contraception.no.use.reason == "others" | # 11
                                                     contraception.no.use.reason == "doesn't know", "risk", "no risk")))))) #12

preg.risk$risk[which(is.na(preg.risk$risk))] <- "no risk" # we change the NAs into "no risk"
preg.risk$risk <- factor(preg.risk$risk, levels = c("risk", "no risk"))
summary(preg.risk$risk)

# I manually order the number of columns of the data frame
saveRDS(preg.risk, file = "pregnancy_risk.rds")

#####################################################################
####                      6 Analyzing the data                  #####
####                   6.1 Demographic variabels                #####
#####################################################################

# age ------------------------------
age.plot1 <- preg.risk %>% ggplot(aes(age, fill = risk)) + 
  geom_histogram() + ggtitle("Relationship age/risk")

age.plot2 <- preg.risk %>% ggplot(aes(age, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship age/risk")

grid.arrange(age.plot1,age.plot2,ncol=2)

chisq.test(preg.risk$age, preg.risk$risk) # the chi square test shows it's true

# area ------------------------------
preg.risk %>% ggplot(aes(area, fill = risk)) + 
  geom_bar() + ggtitle("Relationship area/risk")

chisq.test(preg.risk$area, preg.risk$risk) # we can't reject the null

# ethnicity ------------------------------
ethnicity.plot1 <- preg.risk %>% ggplot(aes(ethnicity, fill = risk)) + 
  geom_bar() + ggtitle("Relationship ethnicity/risk") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ethnicity.plot2 <- preg.risk %>% ggplot(aes(ethnicity, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship ethnicity/risk") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(ethnicity.plot1,ethnicity.plot2,ncol=2)

chisq.test(preg.risk$ethnicity, preg.risk$risk) # we can't reject the null

# d.m.age.difference ------------------------------
preg.risk %>% ggplot(aes(d.m.age.diff, fill = risk)) + 
  geom_density() + ggtitle("Relationship d.m.age.diff/risk")

#####################################################################
####           6.2 Economic and social variables                #####
#####################################################################

# n.bedrooms, internet, cellphone, transfer ------------------------------
num.bedrooms.plot <- preg.risk %>% ggplot(aes(num.bedrooms, fill = risk)) + 
  geom_histogram() + ggtitle("Relationship num.bedrooms/risk")

internet.plot <- preg.risk %>% ggplot(aes(internet, fill = risk)) + 
  geom_bar() + ggtitle("Relationship internet/risk")

cellphone.plot <- preg.risk %>% ggplot(aes(cellphone, fill = risk)) + 
  geom_bar() + ggtitle("Relationship cellphone/risk")

transfer.plot <- preg.risk %>% ggplot(aes(transfer, fill = risk)) + 
  geom_bar() + ggtitle("Relationship transfer/risk")

grid.arrange(num.bedrooms.plot, internet.plot, cellphone.plot, transfer.plot, nrow=2, ncol=2)

chisq.test(preg.risk$internet, preg.risk$risk)$p.value # we reject the null
chisq.test(preg.risk$cellphone, preg.risk$risk)$p.value  # we reject the null
chisq.test(preg.risk$transfer, preg.risk$risk)$p.value  # there seems to be a correlation
chisq.test(preg.risk$num.bedrooms, preg.risk$risk)$p.value  # the approximation may be incorrect

# f.live.house ------------------------------
f.live.house.plot1 <- preg.risk %>% ggplot(aes(f.live.house, fill = risk)) + 
  geom_bar() + ggtitle("Relationship f.live.house/risk")

f.live.house.plot2 <- preg.risk %>% ggplot(aes(f.live.house, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship f.live.house/risk (percentage)")

grid.arrange(f.live.house.plot1, f.live.house.plot2, ncol=2)
chisq.test(preg.risk$f.live.house, preg.risk$risk) # we reject the null

#####################################################################
####                 6.3 Educational variables                  #####
#####################################################################

# attend.school ------------------------------
attend.school.plot1 <- preg.risk %>% ggplot(aes(attend.school, fill = risk)) + 
  geom_bar() + ggtitle("Relationship attend.school/risk")

attend.school.plot2 <- preg.risk %>% ggplot(aes(attend.school, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship attend.school/risk (percentage)")

grid.arrange(attend.school.plot1, attend.school.plot2, ncol=2)
chisq.test(preg.risk$attend.school, preg.risk$risk) # we reject the null

# contraception.info ------------------------------
contraception.info.plot1 <- preg.risk %>% filter(!is.na(contraception.info)) %>% 
  ggplot(aes(contraception.info, fill = risk)) + 
  geom_bar() + ggtitle("Relationship contraception.info/risk")

contraception.info.plot2 <- preg.risk %>% filter(!is.na(contraception.info)) %>% 
  ggplot(aes(contraception.info, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship contraception.info/risk (percentage)")

grid.arrange(contraception.info.plot1, contraception.info.plot2, ncol=2)
chisq.test(preg.risk$contraception.info, preg.risk$risk) # we reject the null

# contraception.info.family and contraception.info.school ------------------------------
contraception.info.family.plot1 <- preg.risk %>% ggplot(aes(contraception.info.family, fill = risk)) + 
  geom_bar() + ggtitle("Relationship contraception.info.family/risk")

contraception.info.family.plot2 <- preg.risk %>% ggplot(aes(contraception.info.family, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship contraception.info.family/risk (percentage)")

contraception.info.school.plot1 <- preg.risk %>% ggplot(aes(contraception.info.school, fill = risk)) + 
  geom_bar() + ggtitle("Relationship contraception.info.school/risk")

contraception.info.school.plot2 <- preg.risk %>% ggplot(aes(contraception.info.school, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship contraception.info.school/risk (percentage)")

grid.arrange(contraception.info.family.plot1, contraception.info.family.plot2,
             contraception.info.school.plot1, contraception.info.school.plot2, nrow=2, ncol=2)

chisq.test(preg.risk$contraception.info.family, preg.risk$risk)$p.value # we reject the null
chisq.test(preg.risk$contraception.info.school, preg.risk$risk)$p.value # we can't reject the null

# period.info and pregnancy.info ------------------------------
period.info.plot1 <- preg.risk %>% filter(period.info == "yes" | period.info == "no") %>% 
  ggplot(aes(period.info, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar() + ggtitle("Relationship period.info/risk")

period.info.plot2 <- preg.risk %>% filter(period.info == "yes" | period.info == "no") %>% 
  ggplot(aes(period.info, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar(position = "fill") + ggtitle("Relationship period.info/risk (percentage)")

pregnancy.info.plot1 <- preg.risk %>% ggplot(aes(pregnancy.info, fill = risk)) + 
  geom_bar() + ggtitle("Relationship pregnancy.info/risk")

pregnancy.info.plot2 <- preg.risk %>% ggplot(aes(pregnancy.info, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship pregnancy.info/risk (percentage)")

grid.arrange(period.info.plot1, period.info.plot2,
             pregnancy.info.plot1, pregnancy.info.plot2, nrow=2, ncol=2)

chisq.test((preg.risk %>% filter(period.info == "yes" | period.info == "no"))$period.info, 
           (preg.risk %>% filter(period.info == "yes" | period.info == "no"))$risk)$p.value # we reject the null
chisq.test(preg.risk$pregnancy.info, preg.risk$risk)$p.value # we reject the null

# aids.info ------------------------------
aids.info.plot1 <- preg.risk %>% filter(!is.na(aids.info)) %>% ggplot(aes(aids.info, fill = risk)) + 
  geom_bar() + ggtitle("Relationship aids.info/risk")

aids.info.plot2 <- preg.risk %>% filter(!is.na(aids.info)) %>% ggplot(aes(aids.info, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship aids.info/risk (percentage)")

grid.arrange(aids.info.plot1, aids.info.plot2, ncol=2)
chisq.test(preg.risk$aids.info, preg.risk$risk) # we reject the null

# m.pregnancy.info and m.aids.info ------------------------------
m.pregnancy.info.plot1 <- preg.risk %>% filter(!is.na(m.pregnancy.info)) %>% 
  ggplot(aes(m.pregnancy.info, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar() + ggtitle("Relationship m.pregnancy.info/risk")

m.pregnancy.info.plot2 <- preg.risk %>% filter(!is.na(m.pregnancy.info)) %>% 
  ggplot(aes(m.pregnancy.info, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar(position = "fill") + ggtitle("Relationship m.pregnancy.info/risk (percentage)")

m.aids.info.plot1 <- preg.risk %>% filter(!is.na(m.aids.info))%>% 
  ggplot(aes(m.aids.info, fill = risk)) +  # we remove those who do not remember and the NAs
  geom_bar() + ggtitle("Relationship m.aids.info/risk")

m.aids.info.plot2 <- preg.risk %>% filter(!is.na(m.aids.info)) %>% 
  ggplot(aes(m.aids.info, fill = risk)) +  # we remove those who do not remember and the NAs
  geom_bar(position = "fill") + ggtitle("Relationship m.aids.info/risk (percentage)")

grid.arrange(m.pregnancy.info.plot1, m.pregnancy.info.plot2,
             m.aids.info.plot1, m.aids.info.plot2, nrow=2, ncol=2)

chisq.test(preg.risk$m.pregnancy.info, preg.risk$risk) # we can't reject the null
chisq.test(preg.risk$m.aids.info, preg.risk$risk) # we reject the null

#####################################################################
####                 6.4 Bahavioral variables                   #####
#####################################################################

# intercourse ------------------------------
intercourse.plot1 <- preg.risk %>% ggplot(aes(intercourse, fill = risk)) + 
  geom_bar() + ggtitle("Relationship intercourse/risk")

intercourse.plot2 <- preg.risk %>% ggplot(aes(intercourse, fill = risk)) + 
  geom_bar(position = "fill") + ggtitle("Relationship intercourse/risk (percentage)")

grid.arrange(intercourse.plot1, intercourse.plot2, ncol=2)
chisq.test(preg.risk$intercourse, preg.risk$risk) # we reject the null

# alcohol and alcohol.recent ------------------------------
alcohol.plot1 <- preg.risk %>% filter(!is.na(alcohol)) %>% 
  ggplot(aes(alcohol, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar() + ggtitle("Relationship alcohol/risk")

alcohol.plot2 <- preg.risk %>% filter(!is.na(alcohol)) %>% 
  ggplot(aes(alcohol, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar(position = "fill") + ggtitle("Relationship alcohol/risk (percentage)")

alcohol.recent.plot1 <- preg.risk %>% filter(!is.na(alcohol.recent))%>% 
  ggplot(aes(alcohol.recent, fill = risk)) +  # we remove those who do not remember and the NAs
  geom_bar() + ggtitle("Relationship alcohol.recent/risk")

alcohol.recent.plot2 <- preg.risk %>% filter(!is.na(alcohol.recent)) %>% 
  ggplot(aes(alcohol.recent, fill = risk)) +  # we remove those who do not remember and the NAs
  geom_bar(position = "fill") + ggtitle("Relationship alcohol.recent/risk (percentage)")

grid.arrange(alcohol.plot1, alcohol.plot2,
             alcohol.recent.plot1, alcohol.recent.plot2, nrow=2, ncol=2)

chisq.test(preg.risk$alcohol, preg.risk$risk) # we reject the null
chisq.test(preg.risk$alcohol.recent, preg.risk$risk) # we reject the null

# smoke and smoke.recent ------------------------------
smoke.plot1 <- preg.risk %>% filter(!is.na(smoke)) %>% 
  ggplot(aes(smoke, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar() + ggtitle("Relationship smoke/risk")

smoke.plot2 <- preg.risk %>% filter(!is.na(smoke)) %>% 
  ggplot(aes(smoke, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar(position = "fill") + ggtitle("Relationship smoke/risk (percentage)")

smoke.recent.plot1 <- preg.risk %>% filter(!is.na(smoke.recent))%>% 
  ggplot(aes(smoke.recent, fill = risk)) +  # we remove those who do not remember and the NAs
  geom_bar() + ggtitle("Relationship smoke.recent/risk")

smoke.recent.plot2 <- preg.risk %>% filter(!is.na(smoke.recent)) %>% 
  ggplot(aes(smoke.recent, fill = risk)) +  # we remove those who do not remember and the NAs
  geom_bar(position = "fill") + ggtitle("Relationship smoke.recent/risk (percentage)")

grid.arrange(smoke.plot1, smoke.plot2,
             smoke.recent.plot1, smoke.recent.plot2, nrow=2, ncol=2)

chisq.test(preg.risk$smoke, preg.risk$risk) # we reject the null
chisq.test(preg.risk$smoke.recent, preg.risk$risk) # we reject the null

# m.age.first.intercourse ------------------------------
preg.risk %>% ggplot(aes(m.age.first.intercourse, fill = risk)) + 
  geom_density() + ggtitle("Relationship m.age.first.intercourse/risk")

# m.contraception ------------------------------
m.contraception.plot1 <- preg.risk %>% filter(!is.na(m.contraception)) %>% 
  ggplot(aes(m.contraception, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar() + ggtitle("Relationship m.contraception/risk")

m.contraception.plot2 <- preg.risk %>% filter(!is.na(m.contraception)) %>% 
  ggplot(aes(m.contraception, fill = risk)) + # we remove those who do not remember and the NAs
  geom_bar(position = "fill") + ggtitle("Relationship m.contraception/risk (percentage)")

grid.arrange(m.contraception.plot1, m.contraception.plot2, ncol=2)
chisq.test(preg.risk$m.contraception, preg.risk$risk) # we can't reject the null

#####################################################################
####                          7 The algorithm                   #####
####                      7.1 Partitioning the data             #####
#####################################################################

# We first create a partition of the data 
set.seed(25)
test.index <- createDataPartition(preg.risk$risk, times = 1, p = 0.2, list = FALSE)

train.set <- preg.risk[-test.index,]
test.set <- preg.risk[test.index,]

# We need to transform the categorical variable into a numeric class variable
train.set <- mutate(train.set, y = as.numeric(risk == "risk"))

#####################################################################
####               7.2 The logistic regression                  #####
####             7.2.1 Model 1 (three predictors)               #####
#####################################################################

colnames(preg.risk) # we can look at all the variables we can apply

# we do not want to add intercourse, m.age.first.intercourse, and m.contraception as predictors
variables <- (9:37)[!(9:37 == 26 | 9:37 == 36 | 9:37 == 37)] 

predictors <-
  sapply(variables, function(x1){
    train.set$x1 <- train.set[,x1]
    test.set$x1 <- test.set[,x1]
    glm.fit <- glm(y ~ x1, data = train.set, family = "binomial")
    p.hat.logit <- predict(glm.fit, newdata = test.set, type = "response")
    y.hat.logit <- ifelse(p.hat.logit > 0.20, "risk", "no risk") %>% factor(levels = c("risk", "no risk"))
    conf.matrix <- confusionMatrix(y.hat.logit, test.set$risk)
    return(unlist(data.frame(Sensitivity = conf.matrix$byClass[["Sensitivity"]],
                             Specificity = conf.matrix$byClass[["Specificity"]],
                             Balanced.Accuracy = conf.matrix$byClass[["Balanced Accuracy"]])))
  })

cbind(data.frame(Variable = colnames(test.set[variables])), t(as.data.frame(predictors)))


# we can see how well they work together
glm.fit.1 <- glm(y ~ age + attend.school + smoke, data = train.set, family = "binomial")
p.hat.logit.1 <- predict(glm.fit.1, newdata = test.set, type = "response")
y.hat.logit.1 <- ifelse(p.hat.logit.1 > 0.20, "risk", "no risk") %>% factor(levels = c("risk", "no risk"))
conf.matrix.1 <- confusionMatrix(y.hat.logit.1, test.set$risk)
glm.fit.1.sensitivity <- conf.matrix.1$byClass[["Sensitivity"]]
glm.fit.1.accuracy <- conf.matrix.1$byClass[["Balanced Accuracy"]]
conf.matrix.1

#####################################################################
####              7.2.2 Model 2 (four predictors)               #####
#####################################################################

variables <- (9:37)[!(9:37 == 26 | 9:37 == 36 | 9:37 == 37 | 9:37 == 9 | 9:37 == 19 | 9:37 == 29)]

predictors <-
  sapply(variables, function(x4){
    train.set$x4 <- train.set[,x4]
    test.set$x4 <- test.set[,x4]
    glm.fit <- glm(y ~ age + attend.school + smoke + x4, data = train.set, family = "binomial")
    p.hat.logit <- predict(glm.fit, newdata = test.set, type = "response")
    y.hat.logit <- ifelse(p.hat.logit > 0.20, "risk", "no risk") %>% factor(levels = c("risk", "no risk"))
    conf.matrix <- confusionMatrix(y.hat.logit, test.set$risk)
    data.frame(Sensitivity = conf.matrix$byClass[["Sensitivity"]],
               Specificity = conf.matrix$byClass[["Specificity"]],
               Balanced.Accuracy = conf.matrix$byClass[["Balanced Accuracy"]])
  })

cbind(data.frame(Variable = colnames(test.set[variables])), t(as.data.frame(predictors))) %>%
  mutate(Sen.Improv = as.numeric(Sensitivity) - glm.fit.1.sensitivity,
         Acc.Improv = as.numeric(Balanced.Accuracy) - glm.fit.1.accuracy)


# we can see how well they work together
glm.fit.2 <- glm(y ~ age + attend.school + smoke + alcohol.recent, data = train.set, family = "binomial")
p.hat.logit.2 <- predict(glm.fit.2, newdata = test.set, type = "response")
y.hat.logit.2 <- ifelse(p.hat.logit.2 > 0.20, "risk", "no risk") %>% factor(levels = c("risk", "no risk"))
conf.matrix.2 <- confusionMatrix(y.hat.logit.2, test.set$risk)
glm.fit.2.sensitivity <- conf.matrix.2$byClass[["Sensitivity"]]
glm.fit.2.accuracy <- conf.matrix.2$byClass[["Balanced Accuracy"]]
conf.matrix.2

#####################################################################
####              7.2.3 Model 3 (eight predictors)              #####
#####################################################################

variables <- (9:37)[!(9:37 == 26 | 9:37 == 36 | 9:37 == 37 | 9:37 == 9 | 9:37 == 19 | 9:37 == 29 | 9:37 == 27)]

predictors <-
  sapply(variables, function(x5){
    train.set$x5 <- train.set[,x5]
    test.set$x5 <- test.set[,x5]
    glm.fit <- glm(y ~ age + attend.school + smoke + alcohol.recent + x5, data = train.set, family = "binomial")
    p.hat.logit <- predict(glm.fit, newdata = test.set, type = "response")
    y.hat.logit <- ifelse(p.hat.logit > 0.20, "risk", "no risk") %>% factor(levels = c("risk", "no risk"))
    conf.matrix <- confusionMatrix(y.hat.logit, test.set$risk)
    data.frame(Sensitivity = conf.matrix$byClass[["Sensitivity"]],
               Specificity = conf.matrix$byClass[["Specificity"]],
               Balanced.Accuracy = conf.matrix$byClass[["Balanced Accuracy"]])
  })

cbind(data.frame(Variable = colnames(test.set[variables])), t(as.data.frame(predictors))) %>%
  mutate(Sen.Improv = as.numeric(Sensitivity) - glm.fit.2.sensitivity,
         Acc.Improv = as.numeric(Balanced.Accuracy) - glm.fit.2.accuracy)

# we can see how well they work together
glm.fit.3 <- glm(y ~ age + attend.school + smoke + alcohol.recent + d.m.age.diff +
                   transfer + f.live.house + aids.info, data = train.set, family = "binomial")
p.hat.logit.3 <- predict(glm.fit.3, newdata = test.set, type = "response")
y.hat.logit.3 <- ifelse(p.hat.logit.3 > 0.20, "risk", "no risk") %>% factor(levels = c("risk", "no risk"))
conf.matrix.3 <- confusionMatrix(y.hat.logit.3, test.set$risk)
glm.fit.3.sensitivity <- conf.matrix.3$byClass[["Sensitivity"]]
glm.fit.3.accuracy <- conf.matrix.3$byClass[["Balanced Accuracy"]]
conf.matrix.3

#####################################################################
####                7.2.4 Model 4 (nine predictors)             #####
#####################################################################

variables <- (9:37)[!(9:37 == 26 | 9:37 == 36 | 9:37 == 37 | 9:37 == 9 | 9:37 == 19 | 9:37 == 29 | 9:37 == 27 |
                        9:37 == 13 | 9:37 == 17 | 9:37 == 18 | 9:37 == 25)]
colnames(preg.risk)
predictors <-
  sapply(variables, function(x9){
    train.set$x9 <- train.set[,x9]
    test.set$x9 <- test.set[,x9]
    glm.fit <- glm(y ~ age + attend.school + smoke + alcohol.recent + d.m.age.diff +
                     transfer + f.live.house + aids.info + x9, data = train.set, family = "binomial")
    p.hat.logit <- predict(glm.fit, newdata = test.set, type = "response")
    y.hat.logit <- ifelse(p.hat.logit > 0.20, "risk", "no risk") %>% factor(levels = c("risk", "no risk"))
    conf.matrix <- confusionMatrix(y.hat.logit, test.set$risk)
    data.frame(Sensitivity = conf.matrix$byClass[["Sensitivity"]],
               Specificity = conf.matrix$byClass[["Specificity"]],
               Balanced.Accuracy = conf.matrix$byClass[["Balanced Accuracy"]])
  })

cbind(data.frame(Variable = colnames(test.set[variables])), t(as.data.frame(predictors))) %>%
  mutate(Sen.Improv = as.numeric(Sensitivity) - glm.fit.3.sensitivity,
         Acc.Improv = as.numeric(Balanced.Accuracy) - glm.fit.3.accuracy)

# we can see how well they work together
glm.fit.4 <- glm(y ~ age + attend.school + smoke + alcohol.recent + d.m.age.diff +
                   transfer + f.live.house + aids.info + period.info, data = train.set, family = "binomial")
p.hat.logit.4 <- predict(glm.fit.4, newdata = test.set, type = "response")
y.hat.logit.4 <- ifelse(p.hat.logit.4 > 0.20, "risk", "no risk") %>% factor(levels = c("risk", "no risk"))
conf.matrix.4 <- confusionMatrix(y.hat.logit.4, test.set$risk)
glm.fit.4.sensitivity <- conf.matrix.4$byClass[["Sensitivity"]]
glm.fit.4.accuracy <- conf.matrix.4$byClass[["Balanced Accuracy"]]
conf.matrix.4

#####################################################################
####             7.2.5 Can we still add more predictors?        #####
#####################################################################

# Adding an additional predictor lowers either sensitivity, or specificity, or both.
variables <- (9:37)[!(9:37 == 26 | 9:37 == 36 | 9:37 == 37 | 9:37 == 9 | 9:37 == 19 | 9:37 == 29 | 9:37 == 27 |
                        9:37 == 13 | 9:37 == 17 | 9:37 == 18 | 9:37 == 25 | 9:37 == 23)]

predictors <-
  sapply(variables, function(x10){
    train.set$x10 <- train.set[,x10]
    test.set$x10 <- test.set[,x10]
    glm.fit <- glm(y ~ age + attend.school + smoke + alcohol.recent + d.m.age.diff +
                     transfer + f.live.house + aids.info + period.info + x10, data = train.set, family = "binomial")
    p.hat.logit <- predict(glm.fit, newdata = test.set, type = "response")
    y.hat.logit <- ifelse(p.hat.logit > 0.20, "risk", "no risk") %>% factor(levels = c("risk", "no risk"))
    conf.matrix <- confusionMatrix(y.hat.logit, test.set$risk)
    data.frame(Sensitivity = conf.matrix$byClass[["Sensitivity"]],
               Specificity = conf.matrix$byClass[["Specificity"]],
               Balanced.Accuracy = conf.matrix$byClass[["Balanced Accuracy"]])
  })

cbind(data.frame(Variable = colnames(test.set[variables])), t(as.data.frame(predictors))) %>%
  mutate(Sen.Improv = as.numeric(Sensitivity) - glm.fit.4.sensitivity,
         Acc.Improv = as.numeric(Balanced.Accuracy) - glm.fit.4.accuracy)
