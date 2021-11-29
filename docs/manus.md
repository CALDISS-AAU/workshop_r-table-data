- R Table Data - Manus

  

  ## Introduktion

  - Hvad skal vi i dag?

  

  ## Introduktion til tidyverse

  - Hvad er det?
  - Hvilken type data egner det sig til?
  - Hvad er tidy data?

  

  ## Problemet med basis R

  - Eksempler med selektion, filtrering, rekodning

  

  ## Indlæsning af data - `readr`

  `read_csv('https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/ess2014_mainsub_p1.csv')`

  ## Inspicér en data frame

  - head
  - colnames
  - dim, nrow, ncol
  - summary

  

  ***ØVELSE 1***

  

  ## Visualisering med ggplot

  ```
  ggplot(ess2014_p1, aes(x = height)) + 
      geom_histogram()
      
  ggplot(ess2014_p1, aes(x = gndr, fill = cgtsmke)) + 
      geom_bar()
  
  ggplot(ess2014_p1, aes(x = yrbrn, y = weight, colour = gndr)) + 
      geom_point()
  
  ggplot(ess2014_p1, aes(x = gndr, y = weight, colour = gndr)) + 
      geom_jitter()
  
  ggplot(ess2014_p1, aes(x = yrbrn, y = weight, colour = gndr)) + 
      geom_point() + 
      geom_smooth()
      
  ggplot(ess2014_p1, aes(x = gndr, y = weight, fill = cgtsmke)) + 
      geom_boxplot()
  ```

  

  ***ØVELSE 2***

  ## Data wrangling med dplyr

  - select
  - arrange
  - filter

  

  Vis først en ad gangen, så pipe

  ```
  ess_subset <- select(ess_data, idno, polintr, happy)
  # Vis brug af - og everything()
  
  ess_subset <- filter(ess_data, yrbrn > 1985)
  
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
  
  ess_ordered <- arrange(ess_data, height)
  
  ess_ordered <- arrange(ess_data, desc(height))
  ```

  

  **Forklar pipe**

  

  ```
  ess_subset <- ess_data %>%  # the data frame ess_subset is created as a copy of ess_data and then "fed" into the pipe
      select(idno, polintr, happy, yrbrn, height) %>% # 5 variables selected. the first arguement is omitted as that is given through the pipe
      filter(yrbrn > 1985) %>% # observations filtered
      arrange(desc(height))  # ordered by height, descending
  
  head(ess_subset, 3)
  ```

  

  ***ØVELSE 3***

  ## Rekodning

  - Mutate
  - recode
  - ifelse
  - case-when

  ```
  ess_data %>%    # note no object assignment meaning this change is not stored
      mutate(bmi = weight, (height/100)^2) %>%
      select(idno, yrbrn, age) %>%
      head(3)
      
      
  ess_data %>%
      mutate(new_alcfreq = recode(alcfreq, "Every day" = "DAILY DRINKER", "Once a week" = "WEEKLY DRINKER"))
  
  #Recoding alcfreq to three categories
  ess_data %>%
      mutate(new_alcfreq = recode(alcfreq, "Every day" = "DAILY DRINKER", "Once a week" = "WEEKLY DRINKER", .default = "IRRELEVANT"))
      
  ess_data %>%
      mutate(height_cat = case_when(
          height >= 190 ~ "tall",
          height < 177 ~ "not tall"
      ))
      
  ess_data %>% #note that this code also recodes missing
      mutate(health = if_else(health == "Very good", "HEALTHY PERSON", "LESS HEALTHY PERSON"))
  
  ```

  

  ## Joins

  - append
    - `bind_rows()`: "stack" one data set on top of another
    - `intersect()`: creates a data set containing observations appearing in both data sets
    - `setdiff()`: creates a data set containing observations in one data set but not the other
    - `union()`: combines observations from two data sets and removing duplicates (`union_all()` keeps duplicates) - checks for same variables
  - joins
    - `inner_join()`: Includes all variables but only observations present in both datasets
    - `left_join()` / `right_join()`: Includes all variables and all observations from one data set (non-matched observations set to `NA`)
    - `full_join()`: Includes all variables and all observations from both data sets

  

  ```
  ess2014_comb <- bind_rows(ess2014_p1, ess2014_p2)
  
  ess2014_trst <- read_csv("https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/ess2014_trstsub.csv")
  
  ess2014_joined <- left_join(ess2014_comb, ess2014_trst)
  
  # What happens when joining with different observations?
  
  ess2014_p1_join <- left_join(ess2014_p1, ess2014_trst)
  ```

  `left_join()` / `right_join()` keeps all observations from one data set. If observations are not present in the data set to be combined with, those observations will be set to `NA` in the added variables.

  We can see this by doing the same join as above but using `right_join()` instead, which keeps all observations from "ess2014_trst" instead of those from "ess2014_p1":

  ```
  ess2014_join <- right_join(ess2014_p1, ess2014_trst)
  
  dim(ess2014_join)
  ess2014_join
  ```

  

  

  ## Pivoting

  - pivot_longer
  - pivot_wider

  

  ```
  reg_data <- read_csv("https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/bef_dream_2015.csv")
  
  reg_data_long <- reg_data %>%
      pivot_longer(cols = starts_with("br_"), names_to = "month_year", values_to = "branche")
      
  head(drop_na(reg_data_long))
  ```

  

  

  ## Dates

  - Lubridate

  ```
  reg_data <- read_csv("https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/bef_dream_2015.csv")
  ```

  

  ## Factors

  - Hvad er en factor?
  - Factor og character?

  

  