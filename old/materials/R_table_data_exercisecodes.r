## This code contain my solutions for the exercises at the CALDISS workshop: Working with table data in R ##
## March 2019

# Loading libraries
library(tidyverse)
library(haven)
library(lubridate)
library(gmodels)
library(stargazer)


## EXERCISE 1: Getting acquainted with the data
'
1. Load the dataset ESS2014_mainsub.dta using read_dta() (remember to set the working directory)
2. Get acquainted with the data using the commands from the previous slide
3. Use as_factor() on variables giving you trouble
'

file_path <- '/user/adm.aau.dk/kgk/my_folder/ws_rtabledata'
setwd(file_path)
ess2014_main <- read_dta("ESS2014_mainsub.dta")

ess_allattr <- lapply(ess2014_main, attributes)  #save attributes in a list
ess_varlabels <- unlist(lapply(ess_allattr, `[[`, "label"))  #save variable labels in a vector

as_factor2 <- function(var) {  #alternative as_factor function (if as_factor gives errors)
  if (is.labelled(var)) {
    var <- as_factor(var)
  } else {}
  return(var)
}

ess2014_work <- as.data.frame(map(ess2014_main, as_factor2))  #Converting all variables to factors

colnames(ess2014_work)
head(ess2014_work)
dim(ess2014_work)


## EXERCISE 2: COUNTRY-WISE AND GENDER-WISE MEAN AGE AND HEIGHT
'
1. Create the variable age using the mutate command and the variable yrbrn
2. Produce a data frame containing the following summary statistics:
  a. The mean age for each gender (gndr) in each country (cntry)
  b. The mean height for each gender (gndr) in each country (cntry)
  c. The number of males and females in each country (NOTE: n() counts the number of observatoin in a group specified with group_by)

### Tips:

- Use `group_by` to group observations in each combination of gender and country
- Use `summarise` to create the summary statistics `mean` and count `n()`
- Use the pipe `%>%` to chain commands together
'

#Create age variable
ess2014_work <- ess2014_work %>%
  mutate(yrbrn = as.numeric(as.character(yrbrn))) %>% #factor to numeric coercion
  mutate(age = 2014 - yrbrn)  #creating age variable

#DF with mean and age summaries
ess2014_ageheightsum <- ess2014_work %>%
  mutate(height = as.numeric(as.character(height))) %>%  #factor to numeric coercion
  group_by(gndr, cntry) %>%
  summarise(mean_height = mean(height, na.rm = TRUE),
            mean_age = mean(age, na.rm = TRUE),
            count = n()) %>%
  arrange(cntry)

ess2014_ageheightsum



## EXERCISE 3: COMBINING DATA
'
The dataset "ESS2014_mainsub.dta" is only a subset of the ESS 2014 data. It contains only a selection of variables and about a third of the observations.

The datasets "ESS2014_mainsub_p2.dta" and "ESS2014_mainsub_p3.dta" contain the rest of the observations.

The datasets "ESS2014_empsub.dta" and "ESS2014_polsub.dta" contian additional variables.

1. Use the proper append command to add the additional observations (there should be 40.185 observations in the final dataset)
2. Use the proper merge command to add the additional variables - if necessary, merge by variable ident (there should be 43 variables in total)

When done, you should have a dataset with all 40.185 observations and 42 variables.
'

#Loading the other datasets
ess2014_main2 <- read_dta("ESS2014_mainsub_p2.dta")
ess2014_main3 <- read_dta("ESS2014_mainsub_p3.dta")
ess2014_emp <- read_dta("ESS2014_empsub.dta")
ess2014_pol <- read_dta("ESS2014_polsub.dta")

#Combining datasets (using the raw datasets)
ess2014_all <- dplyr::union(ess2014_main, ess2014_main2) %>%
  dplyr::union(ess2014_main3) %>%
  inner_join(ess2014_emp, by = "ident") %>%
  inner_join(ess2014_pol, by = "ident")

ess_allattr <- lapply(ess2014_all, attributes)  #save attributes in a list
ess_varlabels <- unlist(lapply(ess_allattr, `[[`, "label"))  #save variable labels in a vector

#Converting all variables to factors
ess2014_work <- as.data.frame(map(ess2014_all, as_factor2))  #Converting all variables to factors

##WARMING UP DAY 2
'
1. (Re-)create the age variable in the combined dataset (beware of class).
2. Look for a variable that contains information about the respondents last year in employment.
3. Create the variable year_unemp containing the number of years the respondents has been unemployed (beware of class).
4. Use filter to create the subset ESS2014_GB consisting of only observations from Great Britain (cntry == "GB")
'

#Recreate age variable
ess2014_work <- mutate(ess2014_work, yrbrn = as.numeric(as.character(yrbrn))) %>%  #recreate age variable
  mutate(age = 2014 - yrbrn)

#Looking for variable about last year in employment using vector of variable labels (pdjobyr)
ess_varlabels

#Create years unemployed
ess2014_work <- ess2014_work %>%
  mutate(pdfjobyr = as.numeric(as.chracter(pdfjobyr))) %>%  #factor-numeric coercion
  mutate(year_unemp = 2014 - pdjobyr)  #creating variable

#Create GB subset: ess2014_gb
ess2014_gb <- ess2014_work %>%
  mutate(cntry = as.character(cntry)) %>%  #Coerce cntry to character
  filter(cntry == "GB")  #Filter by "GB" (Great Britain)


## EXERCISE 4: DATES
'
The variables inwdds, inwmms, inwyys, inwshh, inwsmm, inwdde, inwmme, inwyye, inwehh, inwemm contain information about the date and time for the interview.

1. Use paste or unite to create a variable for interview start and end (names: int_start and int_end). It should contain both date and time (beware of the classes).
2. Convert the variable to a datetime object using the proper lubridate command (like ymd_hm)
3. Create the variable int_length for the length of the interview (use as.period)
4. Recode the variable int_length to length in minutes (use as.numeric(int_length, unit = "minutes"))
5. Find out in which country the interviews took the longest.
'

#Find interview time variables
int_vars <- str_subset(colnames(ess2014_work), "^inw.*")

#Convert interview time variables to character
ess2014_work[, int_vars] <- map_dfc(ess2014_work[, int_vars], as.character)

#Add int_start and int_end for interview start and end times
ess2014_work <- ess2014_work %>%
  unite(int_start, inwyys, inwmms, inwdds, inwshh, inwsmm, sep = "-") %>%
  unite(int_end, inwyye, inwmme, inwdde, inwehh, inwemm, sep = "-") %>%
  mutate(int_start = ymd_hm(int_start),
         int_end = ymd_hm(int_end)) %>%
  mutate(int_length = as.period(int_end - int_start)) %>%
  mutate(int_length = as.numeric(int_length, unit = "minutes"))

#Find longest interview
ess_intlength <- select(ess2014_work, cntry, int_length) %>%
  group_by(cntry) %>%
  summarise(max_int = max(int_length, na.rm = TRUE)) %>%
  arrange(desc(max_int))

#Find longest interview with filter (interviews longer than 600 minutes)
ess_intlength <- select(ess2014_work, cntry, int_length) %>%
  filter(int_length < 600) %>%
  group_by(cntry) %>%
  summarise(max_int = max(int_length, na.rm = TRUE)) %>%
  arrange(desc(max_int))


## EXERCISE 5: EXTRAS

#### Trust in institutions
'
The variables imported from "ESS2014_polsub.dta" contains information about the repsondents trust in various state and political institutions. All these variables start with "trst".

1. Create the the index variable inst_trst which gives the respondent a combined score of 0-100 based on how much they rate their trust in the individual institutions (100 = Highest trust score in all institutions).
  a. Use str_which to extract the indexes of the institutional trust variables.
    - ^ can be used to specify that a string should start with a specific set of characters
    - . can be used as a wildcard (any character)
    - * can be used to repeat the preceding character 1 or more times
  b. Use mutate to create a new variable
  c. Use rowSums to calculate the row sum across the institutional trust variables
  d. Calculate the score in percent (sum of scores / max score * 100)
'  

#Vector of trust variables
trst_vars <- str_subset(colnames(ess2014_work), "^trst.*")

#Vector of trust variables indices
trst_varsin <- str_which(colnames(ess2014_work), "^trst.*")

#Convert trust variables to character (factor to character, keeping numerical values)
ess2014_work[, trst_vars] <- as.data.frame(map(ess2014_work[, trst_vars], as.character))

#Convert trust variables to numeric (character to numeric)
ess2014_work[, trst_vars] <- as.data.frame(map(ess2014_work[, trst_vars], as.numeric))

#Create inst_trst variable
ess2014_work <- mutate(ess2014_work, inst_trst = rowSums(ess2014_work[, trst_vars])) %>%  #Row sums of trust variable
  mutate(inst_trst = (inst_trst / max(inst_trst, na.rm = TRUE)) * 100)  #Convert score to percentage


## EXERCISE 6: LISTS AND FOR LOOPS - Guided version
'
1. Create a list containing a subset for each country. 
  a. Create a vector containing the unique values of cntry (use unique) - fx ess_countries
  b. Create an empty list (fx ess_subsets <- list())
  c. Use a for loop to create a subset for each country and store them in the list.
    - for (i in ess_countries) { - Note that i in the loop will be individual countries in this case!
    - Within the loop, subset the data frame so it only contains rows from the given country (ess_subset <- filter(ess2014_work, cntry == i))
    - Add the subset to the list using country as name for the list element: ess_subsets[[i]] <- ess_subset
'		

#Coerce country variable (cntry) to chracter
ess2014_work$cntry <- as.character(ess2014_work$cntry)

#Vector of unique countries
ess_countries <- unique(ess2014_work$cntry)

#Create ess_subsets - list of ess subsets
ess_subsets <- list()  #Create empty list
for (i in ess_countries) {  #Loop through each value in ess_countries
  ess_subset <- filter(ess2014_work, cntry == i)  #Create subset filtered by the value in ess_countries
  ess_subsets[[i]] <- ess_subset  #Add the subset to the list using country value as name of list element
}


## EXERCISE 7: TESTS OF INDEPENDENCE
'
Use your GB subset for the following exercises.

1. Examine the variables tvtot and happy. What are they measuring?
2. Create a crosstabel of tvtot and happy. Drop unwanted factor levels with droplevels()
3. Compute a chi-squared statistic for the crosstable. Is there a connection between the two variables?
4. Examine the variables icpdwrk and lrscale. What are they measuring? If we are to assume a relationship between the two, determine the dependent (Y) and independet (X) variable.
5. Compute a t-test. What does it tell us? (beware of classes - dependent has to be numeric)
'

#Look at the variable labels for tvtot and happy
ess_varlabels

#Check attributes for tvtot and happy (works for factors)
attributes(ess2014_gb$tvtot)
attributes(ess2014_gb$happy)

#Crosstabel of tvtot and happy
tvhap_tab <- with(ess2014_gb, table(droplevels(tvtot), droplevels(happy)))

#Chi-squared test
summary(tvhap_tab)  #Chi-squared with summary
chisq.test(tvhap_tab)  #JJust chi-squared

#Check attributes for icpdwrk and lrscale. (works for factors)
attributes(ess2014_gb$icpdwrk)
attributes(ess2014_gb$lrscale)

#t-test - left-right scale (lrscale) as dependent (lrscale converted from factor to numeric)
with(ess2014_gb, 
     t.test(as.numeric(as.character(droplevels(lrscale))) ~ icpdwrk)
     )


## EXERCISE 8: REGRESSION MODELS
'
Use you GB subset for the following exercise.

(@) Examine the variables happy, tvtot, ctzcntr. What are they measuring?
(@) Assuming a relationship between the variables above, determine the dependent variable.
(@) Fit a multiple linear regression model using the variables above. Include gndr and age as control/additional independent variables (beware of classes - we are treating several categorical variables as interval variables for this exercise).
'

#Check attributes for happy, tvtot and ctzcntr (works for factors)
attributes(ess2014_gb$happy)  #Dependent
attributes(ess2014_gb$tvtot)
attributes(ess2014_gb$ctzcntr)

#Factor levels for 0-10 scale
faclevels <- c(NA, NA, NA, c(0:10))

levels(ess2014_gb$happy) <- faclevels

#Recoding variable
ess2014_gb$happy <- fct_recode(ess2014_gb$happy, "0" = "Extremely unhappy", "10" = "Extremely happy")  #Changing factor levels
ess2014_gb$happy <- as.numeric(as.character(ess2014_gb$happy))

#Creating model
ess_happytvtot_model <- lm(happy ~ tvtot + ctzcntr + gndr + age, data = ess2014_gb)

#Model summary
summary(ess_happytvtot_model)

## Exporting results with stargazer
stargazer(ess_happytvtot_model, type = "html", out = paste0(file_path, "modelout.html"))  #For models

## Exporting data
write_dta(ess2014_gb, "ess2014_gb.dta")
write_csv(ess2014_gb, "ess2014_gb.csv")
