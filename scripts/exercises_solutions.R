library(tidyverse)
library(ggplot2)

# Exercise 1: Inspecting data
ess2014 <- read_csv('https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/ess2014_mainsub_p1.csv')

head(ess2014)

dim(ess2014)

print(2014 - min(ess2014$yrbrn))

# Exercise 2: Visualization
ggplot(data = ess2014, aes(x = yrbrn)) + 
  geom_histogram()

ggplot(data = ess2014, aes(x = polintr, y = yrbrn)) + 
  geom_boxplot()

# Exercise 3: Data wrangling
ess2014 %>%
  drop_na() %>%
  filter(yrbrn < 1970) %>%
  arrange(yrbrn, desc(cgtsday)) %>%
  select(idno, yrbrn, health, cgtsday) %>%
  head()

# Exercise 4: Mutating

ess2014 <- ess2014 %>%
  mutate(age = 2014 - yrbrn) %>%
  mutate(smoker = if_else(is.na(cgtsday) | cgtsday == 0, 0, 1),
         smoker2 = case_when(
           cgtsmke == "I smoke but not every day" ~ "smoker",
           cgtsmke == "I have never smoked" ~ "non-smoker",
           cgtsmke == "I don't smoke now but I used to" ~ "non-smoker",
           cgtsmke == "I smoke daily" ~ "smoker",
           cgtsmke == "I have only smoked a few times" ~ "non-smoker",
           TRUE ~ as.character(NA)
         )
         )

# Exercise 5: Combining data

ess2014_2 <- read_csv('https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/ess2014_mainsub_p2.csv')

ess2014 <- ess2014 %>%
  bind_rows(ess2014_2)

ess2014_inw <- read_csv('https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/ess2014_inwsub.csv')

ess2014 <- ess2014 %>%
  right_join(ess2014_inw, by = "idno")

dim(ess2014)

# Exercise 6: Pivoting data

udda <- read_csv('https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/udda_sim_2010-2015.csv')

head(udda)

udda_wide <- pivot_wider(udda, names_from = year, names_prefix = "HFAUDD", values_from = HFAUDD) %>%
  select(-HFAUDD2015, HFAUDD2015)
