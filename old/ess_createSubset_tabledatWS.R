library(tidyverse)

work_path <- '//ADM.AAU.DK/Users/kgk/Documents/CALDISS/Aktiviteter/Workshopmateriale/R table data/'
data_path <- 'D:/Data/ESS/ESS2014/'

ess_2014 <- read_dta(paste0(data_path, 'ESS7e02_2.dta'))

## IDENTIFIER ##
ess_2014$ident <- seq(1, nrow(ess_2014))

## COMBINED POLITICAL PARTY VARIABLE ##
party_varind <- grep('^prtvt.*', colnames(ess_2014))
party_varnames <- colnames(ess_2014)[party_varind]

party_varindsub <- grep('^prtvt.*', colnames(ess_2014))

for (i in party_varindsub){
  ess_2014[, i] <- as_factor(ess_2014[, i], levels = "both")
}

level_to_char <- function(var){
  chr_var <- as.character(levels(var))[var]
  return(chr_var)
}

extract_chr <- function(row){
  chr_val <- min(as.character(row), na.rm = TRUE)
  return(chr_val)
}

ess_2014prtvt <- map_df(ess_2014[, party_varindsub], level_to_char)
ess_2014_newvar <- apply(ess_2014prtvt, 1, extract_chr)

ess_2014$polpartALL <- ess_2014_newvar


## TRUST IN INSTITUTIONS INDEX ## - Missin values
trst_varind <- grep('^trst.*', colnames(ess_2014))
trst_var <- colnames(ess_2014)[trst_varind]
#trst_varsum <- rowSums(ess_2014[, c(trst_varind)], na.rm = TRUE)
#trst_numrepl <- rowSums(!(is.na(ess_2014[, c(trst_varind)])), na.rm = TRUE)
#trst_newvar <- (trst_varsum / (length(trst_varind) * 10)) * 100

#ess_2014$INSTtrst <- trst_newvar


## HEALTH PROBLEM INDEX - NUmber of health problems last 12 months (theoretical max: 11##
health_varind <- grep('^hltpr.*', colnames(ess_2014))[1:11]  #Excluding missing
health_varsum <- rowSums(ess_2014[, c(health_varind)], na.rm = TRUE)

ess_2014$hltprIND <- health_varsum

## INTERVIEW LENGTH VARIABLES ##
inw_varind <- grep('^inw.*', colnames(ess_2014))
inw_var <- colnames(ess_2014)[inw_varind]

## INTERVIEW LENGTH (solution)##
int_start <- as.character(with(ess_2014, c(inwyys[1], inwmms[1], inwdds[1], inwshh[1], inwsmm[1])))
int_start <- paste(int_start, collapse = "-")
int_start <- ymd_hm(int_start)

int_end <- as.character(with(ess_2014, c(inwyye[1], inwmme[1], inwdde[1], inwehh[1], inwemm[1])))
int_end <- paste(int_end, collapse = "-")
int_end <- ymd_hm(int_end)

int_length <- as.period(int_end - int_start)
as.numeric(int_length, unit = "hours")

## VARIABLES TO KEEP ##
varkeep_data1 <- c('ident', 'cntry', 'proddate', 'gndr', inw_var, 'yrbrn', 'height', 'weight', 'tvtot', 'ppltrst', 
              'ctzcntr', 'brncntr', 'emprf14', 'edulvlb', 'emprm14', 'happy', 'hltprIND')

varkeep_data2 <- c('ident', 'mainact', 'icpdwrk', 'pdjobev', 'pdjobyr', 'isco08')  #EMPLOYMENT

varkeep_data3 <- c('ident', trst_var, 'polintr', 'polpartALL', 'euftf', 'lrscale')  #POLITICAL

## SPLIT VARIABLES ##
ess2014_mainsub <- ess_2014[, varkeep_data1]
ess2014_empsub <- ess_2014[, varkeep_data2]
ess2014_polsub <- ess_2014[, varkeep_data3]

## SPLIT OBSERVATIONS ##
set.seed(42)
id_split1 <- sample(ess2014_mainsub$ident, floor(40185/3))
set.seed(52)
id_split2 <- sample(ess2014_mainsub$ident, floor(40185/3))

id_keep <- setdiff(ess2014_mainsub$ident, unique(c(id_split1, id_split2)))


ess2014_mainsub_p2 <- ess2014_mainsub[ess2014_mainsub$ident %in% id_split1, ]
ess2014_mainsub_p3 <- ess2014_mainsub[ess2014_mainsub$ident %in% id_split2, ] 
ess2014_mainsub_p1 <- ess2014_mainsub[ess2014_mainsub$ident %in% id_keep, ]

## SAVE DTA ##
save_path <- paste0(work_path, "data/")
setwd(save_path)
write_dta(ess2014_mainsub_p1, "ESS2014_mainsub.dta")
write_dta(ess2014_mainsub_p2, "ESS2014_mainsub_p2.dta")
write_dta(ess2014_mainsub_p2, "ESS2014_mainsub_p3.dta")
write_dta(ess2014_empsub, "ESS2014_empsub.dta")
write_dta(ess2014_polsub, "ESS2014_polsub.dta")
