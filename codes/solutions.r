## REFRESHER EXERCISE

ess_mainsub <- read_csv('https://github.com/CALDISS-AAU/workshop_r-table-data/raw/master/data/ess2014_mainsub_p1.csv') %>%
    mutate(age = 2014 - yrbrn) %>%
    mutate(smoker = case_when(cgtsmke == 'I smoke but not every day' ~ 1,
                              cgtsmke == 'I have never smoked' ~ 0,
                              cgtsmke == "I don't smoke now but I used to" ~ 0,
                              cgtsmke == 'I smoke daily' ~ 1,
                              cgtsmke == 'I have only smoked a few times' ~ 0,
                              cgtsmke == 'No answer' ~ as.numeric(NA))) %>%
    filter(smoker == 1 & age > 40)

head(ess_mainsub)
dim(ess_mainsub)