library(readr)

ess_data <- read_csv("https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/ess2014_mainsub_p1.csv")

#First 6 rows of data
head(ess_data)

#Summary statistics
summary(ess_data)

#Names of variables (columns)
colnames(ess_data)

#First six values of a variable (column)
head(ess_data$gndr)

#The class of the variable
class(ess_data$gndr)

#Basic R subsetting with index
ess_data[c(1:5), c("gndr", "alcfreq")]

#Basic R subsetting with logical
ess_data[which(ess_data$height > 190), ] #Select respondents over 190 in cm

library(tidyverse)

#Select specific columns
ess_data %>%
    select(idno, ppltrst, vote) %>%
    head(4)

#Select all columns except ppltrst with '-'
ess_data %>%
    select(-ppltrst)

#Select all columns but with a specific column moved (yrbrn)
ess_data %>%
    select(yrbrn, everything())

#Filter for a given condition
ess_data %>%
    filter(yrbrn > 1990)

#Filter for several conditions
ess_data %>%
    filter(yrbrn > 1990 & vote == "Yes")

#Filter for non-missing
ess_data %>%
    filter(is.na(cgtsday)==FALSE) #or !(is.na(cgtsday))

#Filter across variables - only observations with missing
ess_data %>%
    filter_all(any_vars(is.na(.)))

#Filter across variables - only complete observations
ess_data %>%
    filter_all(all_vars(is.na(.)==FALSE))

#Alternative (for missing)
ess_data %>%
    drop_na()

#Sort ascending
ess_data %>%
    arrange(yrbrn)

#Sort descending using desc()
ess_data %>%
    arrange(desc(yrbrn))

#Sort by several
ess_data %>%
    arrange(desc(yrbrn), height)

#Chaining commands with pipe
ess_data %>%
    drop_na() %>%
    filter(yrbrn > 1983) %>%
    select(yrbrn, height, weight, gndr) %>%
    arrange(desc(yrbrn), height) %>%
    head(4)

#Basic R recoding for numerical values
ess_copy <- ess_data

ess_copy[which(ess_copy$height > 190), "height"] <- 99999

head(ess_copy, 4)

#Basic R recoding for text
ess_copy <- ess_data

ess_copy[which(ess_copy$alcfreq == "Once a week"), "alcfreq"] <- "WEEKLY DRINKER"

head(ess_copy, 4)

ess_data$bmi <- ess_data$weight / (ess_data$height/100)**2
head(ess_data)

#Creating bmi variable with mutate
ess_data %>%
    select(idno, weight, height, gndr, yrbrn) %>%
    mutate(bmi = weight/(height/100)**2) %>%
    head(4)

#Creating smoker dummy with mutate and if_else
ess_data %>%
    mutate(smoker = if_else(is.na(cgtsday), "Yes", "No")) %>%
    head(4)

#Creating both smoker dummy and bmi
ess_data %>%
    mutate(smoker = if_else(is.na(cgtsday), "No", "Yes"),
          bmi = weight/(height/100)**2) %>%
    select(idno, gndr, bmi, cgtsday, smoker) %>%
    head(4)

#Creating height_cat using case_when
ess_data %>%
    mutate(height_cat = case_when(
        height >= 190 ~ "tall",
        height < 190 ~ "not tall"
    )) %>%
    select(idno, height, gndr, height_cat) %>%
    head(4)

#Recoding alcfreq to two categories
ess_data %>%
    mutate(alcfreq = recode(alcfreq, "Every day" = "DAILY DRINKER", "Once a week" = "WEEKLY DRINKER"))

#Recoding alcfreq to three categories
ess_data %>%
    mutate(alcfreq = recode(alcfreq, "Every day" = "DAILY DRINKER", "Once a week" = "WEEKLY DRINKER", 
                            .default = "IRRELEVANT"))

ess_data %>% #note that this code also recodes missing
    mutate(health = if_else(health == "Very good", "HEALTHY PERSON", "LESS HEALTHY PERSON"))

#Recoding health to healthy/unhealthy
ess_data %>%
    mutate(health = case_when(
        health == "Very good" ~ "healthy", 
        health == "Good" ~ "healthy",
        health == "Bad" ~ "unhealthy",
        health == "Very bad" ~ "unhealthy", 
        TRUE ~ health)) #This line keeps remaining values as they are

#Coerce as factor
ess_data %>%
    mutate(gndr = as.factor(gndr)) 

#Isolating a factor
gend_cat <- as.factor(ess_data$gndr)

#Inspecting values and levels
unique(gend_cat)

#Create factor as ordered/ordinal (but what order?)
gend_order <- factor(ess_data$gndr, ordered = TRUE)

#Inspecting values and levels
unique(gend_order)

#Creating ordered factor but setting custom order
polintr_fact <- factor(ess_data$polintr, levels = c('Not at all interested', 'Hardly interested',
                                                    'Quite interested', 'Very interested'), ordered = TRUE)

unique(polintr_fact)

#Linear model for weight and yrbrn
lm(weight ~ yrbrn, ess_data)

#Multiple
lm(bmi ~ weight + height, ess_data)

#Storing model
bmi_model <- lm(bmi ~ weight + height, ess_data)

#Summary statistics for bmi_model
summary(bmi_model)

#Linear model with categorical (2 values)
lm(height ~ yrbrn + gndr, ess_data)

#Linear model with ordinal
ess_data$healthcat <- factor(ess_data$health, levels = c('Very bad', 'Bad', 'Fair', 'Good', 'Very good'), ordered = TRUE)

summary(lm(height ~ yrbrn + healthcat, ess_data))

#Linear model with nominal (character as factor)
summary(lm(height ~ yrbrn + health, ess_data))

library(stargazer)

height_model <- lm(height ~ yrbrn + health, ess_data)
stargazer(height_model, type = "html", out = "../output/modelout.html")
