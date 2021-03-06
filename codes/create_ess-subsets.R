library(tidyverse)
library(haven)

work_path <- 'D:/OneDrive/OneDrive - Aalborg Universitet/CALDISS/Aktiviteter/Workshopmateriale/R table data/data/'
data_path <- 'D:/Data/ESS/ESS2014/'

ess_2014 <- read_dta(paste0(data_path, 'ESS7e02_2.dta'))

## FILTER DANISH

ess_sub <- ess_2014 %>%
  filter(cntry == "DK")

## KEEP VARIABLES
keep_var <- c("idno", "ppltrst", "polintr", "trstprl", "trstlgl", "trstplc", "trstplt", 
              "trstprt", "trstep", "trstun", "vote", "lrscale", "happy", "health", "cgtsday",
              "cgtsmke", "alcfreq", "brncntr", "height", "weight", "gndr", "yrbrn", "edlvddk", 
              "marsts")

ess_sub <- ess_sub %>%
  select(keep_var)

## COMBINED POLITICAL PARTY VARIABLE ##
party_varind <- grep('^prtvt.*', colnames(ess_2014))
party_varnames <- colnames(ess_2014)[party_varind]

ess_sub <- ess_sub %>%
  left_join(ess_2014[, c("idno", party_varnames)])

party_varindsub <- grep('^prtvt.*', colnames(ess_sub))

for (i in party_varindsub){
  ess_sub[, i] <- as_factor(ess_sub[, i], levels = "both")
}

level_to_char <- function(var){
  chr_var <- as.character(levels(var))[var]
  return(chr_var)
}

extract_chr <- function(row){
  chr_val <- min(as.character(row), na.rm = TRUE)
  return(chr_val)
}

ess_2014prtvt <- map_df(ess_sub[, party_varindsub], level_to_char)
ess_2014_newvar <- apply(ess_2014prtvt, 1, extract_chr)

ess_sub <- ess_sub %>%
  mutate(polpartvt = ess_2014_newvar) %>%
  select(-one_of(party_varnames))

## FIX LABELLED ##
class_check <- function(var){
  if (class(var) == "haven_labelled"){
    var = as_factor(var)
  }
  return(var)
}

ess_sub <- map_dfc(ess_sub, class_check)

num_var <- c("cgtsday", "height", "weight", "yrbrn")

lvl_to_num <- function(var) {
  var = as.numeric(levels(var))[var]
  return(var)
}

ess_sub[, num_var] <- map_dfc(ess_sub[, num_var], lvl_to_num)

## INTERVIEW VARIABLES
library(stringr)
# inwdds: day of month, start
# inwmms: month, start
# inwyys: year, start
# inwshh: hour, start
# inwsmm: minute, start
# inwdde: day of month, end
# inwmme: month, end
# inwyye: year, end
# inwehh: hour, end
# inwemm: minute, end

int_var <- str_subset(colnames(ess_sub), "^inw.{3}")

## SPLIT DATA ##

trstvars <- colnames(ess_sub)[grep('^trst.*', colnames(ess_sub))]

ess_mainsub <- select(ess_sub, -all_of(trstvars))
ess_trstsub <- select(ess_sub, idno, trstvars)
ess_inwsub <- select(ess_sub, idno, all_of(int_var))

## SPLIT OBSERVATIONS ##
set.seed(42)
id_split <- sample(ess_mainsub$idno, floor(nrow(ess_mainsub)/2))

id_keep <- setdiff(ess_mainsub$idno, unique(id_split))


ess_mainsub_p1 <- ess_mainsub %>%
  filter(idno %in% id_split)

ess_mainsub_p2 <- ess_mainsub %>%
  filter(idno %in% id_keep)

## SAVE DTA ##
setwd(work_path)
write_dta(ess_mainsub, "ess2014_mainsub.dta")
write_dta(ess_mainsub_p1, "ess2014_mainsub_p1.dta")
write_dta(ess_mainsub_p2, "ess2014_mainsub_p2.dta")
write_dta(ess_trstsub, "ess2014_trstsub.dta")
write_dta(ess_inwsub, "ess2014_inwsub.dta")

write_csv(ess_mainsub, "ess2014_mainsub.csv")
write_csv(ess_mainsub_p1, "ess2014_mainsub_p1.csv")
write_csv(ess_mainsub_p2, "ess2014_mainsub_p2.csv")
write_csv(ess_trstsub, "ess2014_trstsub.csv")
write_csv(ess_inwsub, "ess2014_inwsub.csv")