install.packages("geosphere")
library(geosphere)
library(readr)
library(tidyverse)
library(shiny)
library('rsconnect')
library(readxl)
library("rvest")
library("janitor")
library("maps")
library(bslib)
library(googlesheets4)
setwd("C:\\Users\\world\\OneDrive\\Documents")
#This is just calling the Department of Education dataset for 2018-19 and choosing the data we want
MERGED2020_21_PP <- read_csv("MERGED2018_19_PP.csv")
da <- MERGED2020_21_PP
d <- da %>% select(UNITID,  INSTNM, CITY, ZIP, STABBR, PREDDEG, REGION, ADM_RATE, SAT_AVG_ALL, SATVRMID, SATMTMID, ACTCMMID, UGDS, UGDS_WHITE, UGDS_BLACK, UGDS_HISP, UGDS_ASIAN, UGDS_AIAN, UGDS_NHPI, NPT4_PUB, NPT4_PRIV, NPT41_PUB, NPT41_PUB, NPT41_PRIV, NPT42_PUB, NPT42_PRIV, NPT43_PRIV, NPT43_PUB, NPT44_PUB, NPT44_PRIV, NPT45_PUB, 
                   NPT45_PRIV, NUM4_PUB, NUM41_PUB, NUM42_PUB, NUM43_PUB, NUM44_PUB, NUM45_PUB, NUM4_PRIV, NUM41_PRIV, NUM42_PRIV, NUM43_PRIV, NUM44_PRIV, NUM45_PRIV, COSTT4_A, TUITIONFEE_IN, TUITIONFEE_OUT, , INEXPFTE, AVGFACSAL, PCTPELL, C150_4, C150_4_WHITE, C150_4_BLACK, C150_4_HISP, 
                   C150_4_AIAN, C150_4_ASIAN, PCTFLOAN, ENRL_ORIG_YR2_RT, DEBT_MDN, LO_INC_DEBT_MDN, MD_INC_DEBT_MDN, HI_INC_DEBT_MDN, PELL_DEBT_MDN, COUNT_WNE_P10, MD_EARN_WNE_P10, C100_4, ICLEVEL, UGDS_MEN, GRADS, UG12MN, PPLUS_PCT_HIGH, PPLUS_PCT_LOW, DBRR1_FED_UGCOMP_RT, 
                   ROOMBOARD_ON, ENDOWBEGIN, ENDOWEND, ADMCON7, MD_EARN_WNE_MALE0_P10, MD_EARN_WNE_MALE1_P10, STUFACR, IRPS_WHITE, IRPS_ASIAN, IRPS_NRA, IRPS_BLACK, IRPS_HISP, IRPS_WOMEN, IRPS_MEN, DBRR1_FED_UGCOMP_RT, PFTFAC, OVERALL_YR4_N, LO_INC_YR4_N, MD_INC_YR4_N, HI_INC_YR4_N, UGDS_NRA)
#Now we get rid of colleges with too little data and make all our numeric values into numbers
d <- d %>% filter(ADM_RATE != "NULL")
d <- d %>% mutate(ADM_RATE = as.numeric(ADM_RATE))
d <- d %>% mutate(UGDS = as.numeric(UGDS))
d <- d %>% mutate(UGDS_WHITE = as.numeric(UGDS_WHITE))
d <- d %>% mutate(UGDS_BLACK = as.numeric(UGDS_BLACK))
d <- d %>% mutate(UGDS_HISP = as.numeric(UGDS_HISP))
d <- d %>% mutate(UGDS_ASIAN = as.numeric(UGDS_ASIAN))
d <- d %>% mutate(UGDS_AIAN = as.numeric(UGDS_AIAN))
d <- d %>% mutate(UGDS_NHPI = as.numeric(UGDS_NHPI))
d <- d %>% mutate(PFTFAC = as.numeric(PFTFAC))
d <- d %>% mutate(TOT_COST = as.numeric(COSTT4_A)) %>% select(-COSTT4_A)
d <- d %>% mutate(TUITIONFEE_IN = as.numeric(TUITIONFEE_IN))
d <- d %>% mutate(TUITIONFEE_OUT = as.numeric(TUITIONFEE_OUT))
d <- d %>% filter(is.na(TUITIONFEE_IN) == FALSE)
d <- d %>% mutate(FUNDPERSTU = as.numeric(INEXPFTE)) %>% select(-INEXPFTE)
d <- d %>% filter(is.na(FUNDPERSTU) == FALSE)
d <- d %>% mutate(AVGFACSAL = as.numeric(AVGFACSAL))
d <- d %>% mutate(PCTPELL = as.numeric(PCTPELL))
d <- d %>% mutate(COMPLETE_RT = as.numeric(C150_4)) %>% select(-C150_4)
d <- d %>% mutate(COMPLETE_RT_WHT = as.numeric(C150_4_WHITE)) %>% select(-C150_4_WHITE)
d <- d %>% mutate(COMPLETE_RT_BLCK = as.numeric(C150_4_BLACK)) %>% select(-C150_4_BLACK)
d <- d %>% mutate(COMPLETE_RT_HISP = as.numeric(C150_4_HISP)) %>% select(-C150_4_HISP)
d <- d %>% mutate(COMPLETE_RT_NTV = as.numeric(C150_4_AIAN)) %>% select(-C150_4_AIAN)
d <- d %>% mutate(COMPLETE_RT_ASN = as.numeric(C150_4_ASIAN)) %>% select(-C150_4_ASIAN)
d <- d %>% mutate(PCTFLOAN = as.numeric(PCTFLOAN))
d <- d %>% mutate(RETENTION = as.numeric(ENRL_ORIG_YR2_RT)) %>% select(-ENRL_ORIG_YR2_RT)
d <- d %>% mutate(DEBT_MDN = as.numeric(DEBT_MDN))
d <- d %>% mutate(LO_INC_DEBT_MDN = as.numeric(LO_INC_DEBT_MDN))
d <- d %>% mutate(MD_INC_DEBT_MDN = as.numeric(MD_INC_DEBT_MDN))
d <- d %>% mutate(HI_INC_DEBT_MDN = as.numeric(HI_INC_DEBT_MDN))
d <- d %>% mutate(PELL_DEBT_MDN = as.numeric(PELL_DEBT_MDN))
d <- d %>% mutate(AFTER10EMP = as.numeric(COUNT_WNE_P10)) %>% select(-COUNT_WNE_P10)
d <- d %>% mutate(EARNAFTER10 = as.numeric(MD_EARN_WNE_P10)) %>% select(-MD_EARN_WNE_P10)
d <- d %>% mutate(COMPLETE_RT_4YR = as.numeric(C100_4)) %>% select(-C100_4)
d <- d %>% mutate(UGDS_MEN = as.numeric(UGDS_MEN))
d <- d %>% mutate(GRADS = as.numeric(GRADS))
d <- d %>% select(-UG12MN)
d <- d %>% mutate(PPLUS_PCT_HIGH = as.numeric(PPLUS_PCT_HIGH) / 100)
d <- d %>% mutate(PPLUS_PCT_LOW = as.numeric(PPLUS_PCT_LOW) / 100)
d <- d %>% mutate(ROOMBOARD_ON = as.numeric(ROOMBOARD_ON))
d <- d %>% mutate(REPAYRATE = as.numeric(DBRR1_FED_UGCOMP_RT)) %>% select(-DBRR1_FED_UGCOMP_RT)
d <- d %>% mutate(ENDOWBEGIN = as.numeric(ENDOWBEGIN))
d <- d %>% mutate(ENDOWEND = as.numeric(ENDOWEND))
d <- d %>% mutate(TSTREQ = as.numeric(ADMCON7)) %>% select(-ADMCON7)
d <- d %>% mutate(NOTMALEEARN = as.numeric(MD_EARN_WNE_MALE0_P10)) %>% select(-MD_EARN_WNE_MALE0_P10)
d <- d %>% mutate(MALEEARN = as.numeric(MD_EARN_WNE_MALE1_P10)) %>% select(-MD_EARN_WNE_MALE1_P10)
d <- d %>% mutate(STUFACR = as.numeric(STUFACR))
d <- d %>% mutate(FAC_WHT = as.numeric(IRPS_WHITE)) %>% select(-IRPS_WHITE)
d <- d %>% mutate(FAC_BLCK = as.numeric(IRPS_BLACK)) %>% select(-IRPS_BLACK)
d <- d %>% mutate(FAC_ASN = as.numeric(IRPS_ASIAN)) %>% select(-IRPS_ASIAN)
d <- d %>% mutate(FAC_INTL = as.numeric(IRPS_NRA)) %>% select(-IRPS_NRA)
d <- d %>% mutate(FAC_HISP = as.numeric(IRPS_HISP)) %>% select(-IRPS_HISP)
d <- d %>% mutate(FAC_FEMALE = as.numeric(IRPS_WOMEN)) %>% select(-IRPS_WOMEN)
d <- d %>% mutate(FAC_MALE = as.numeric(IRPS_MEN)) %>% select(-IRPS_MEN)
d <- d %>% mutate(TYPE = ifelse(NPT41_PUB=="NULL", 1, 0))
#This is to aggregate financial aid data for public and private schools, we also rename some variables
for (i in 1:1853) {
  if (d$NPT41_PUB[i] == "NULL") {
    d$NPT41_PUB[i] = d$NPT41_PRIV[i]
  }
}
d <- d %>% select(-NPT41_PRIV) %>% mutate(NETPRICE_LESS30 = as.numeric(NPT41_PUB)) %>% select(-NPT41_PUB)
for (i in 1:1853) {
  if (d$NPT42_PUB[i] == "NULL") {
    d$NPT42_PUB[i] = d$NPT42_PRIV[i]
  }
}
d <- d %>% select(-NPT42_PRIV) %>% mutate(NETPRICE_30_48 = as.numeric(NPT42_PUB)) %>% select(-NPT42_PUB)
for (i in 1:1853) {
  if (d$NPT43_PUB[i] == "NULL") {
    d$NPT43_PUB[i] = d$NPT43_PRIV[i]
  }
}
d <- d %>% select(-NPT43_PRIV) %>% mutate(NETPRICE_48_75 = as.numeric(NPT43_PUB)) %>% select(-NPT43_PUB)
for (i in 1:1853) {
  if (d$NPT44_PUB[i] == "NULL") {
    d$NPT44_PUB[i] = d$NPT44_PRIV[i]
  }
}
d <- d %>% select(-NPT44_PRIV) %>% mutate(NETPRICE_75_110 = as.numeric(NPT44_PUB)) %>% select(-NPT44_PUB)
for (i in 1:1853) {
  if (d$NPT45_PUB[i] == "NULL") {
    d$NPT45_PUB[i] = d$NPT45_PRIV[i]
  }
}
d <- d %>% select(-NPT45_PRIV) %>% mutate(NETPRICE_110PLUS = as.numeric(NPT45_PUB)) %>% select(-NPT45_PUB)
for (i in 1:1853) {
  if (d$NPT4_PUB[i] == "NULL") {
    d$NPT4_PUB[i] = d$NPT4_PRIV[i]
  }
}
d <- d %>% select(-NPT4_PRIV) %>% mutate(NETPRICE_AGG = as.numeric(NPT4_PUB)) %>% select(-NPT4_PUB)
for (i in 1:1853) {
  if (d$NUM4_PUB[i] == "NULL") {
    d$NUM4_PUB[i] = d$NUM4_PRIV[i]
  }
}
d <- d %>% select(-NUM4_PRIV) %>% mutate(FINAIDSTU = as.numeric(NUM4_PUB)) %>% select(-NUM4_PUB)
for (i in 1:1853) {
  if (d$NUM41_PUB[i] == "NULL") {
    d$NUM41_PUB[i] = d$NUM41_PRIV[i]
  }
}
d <- d %>% select(-NUM41_PRIV) %>% mutate(FINAIDSTU_LESS30 = as.numeric(NUM41_PUB)) %>% select(-NUM41_PUB)
for (i in 1:1853) {
  if (d$NUM42_PUB[i] == "NULL") {
    d$NUM42_PUB[i] = d$NUM42_PRIV[i]
  }
}
d <- d %>% select(-NUM42_PRIV) %>% mutate(FINAIDSTU_30_48 = as.numeric(NUM42_PUB)) %>% select(-NUM42_PUB)
for (i in 1:1853) {
  if (d$NUM43_PUB[i] == "NULL") {
    d$NUM43_PUB[i] = d$NUM43_PRIV[i]
  }
}
d <- d %>% select(-NUM43_PRIV) %>% mutate(FINAIDSTU_48_75 = as.numeric(NUM43_PUB)) %>% select(-NUM43_PUB)
for (i in 1:1853) {
  if (d$NUM44_PUB[i] == "NULL") {
    d$NUM44_PUB[i] = d$NUM44_PRIV[i]
  }
}
d <- d %>% select(-NUM44_PRIV) %>% mutate(FINAIDSTU_75_110 = as.numeric(NUM44_PUB)) %>% select(-NUM44_PUB)
for (i in 1:1853) {
  if (d$NUM45_PUB[i] == "NULL") {
    d$NUM45_PUB[i] = d$NUM45_PRIV[i]
  }
}
d <- d %>% select(-NUM45_PRIV) %>% mutate(FINAIDSTU_PLUS110 = as.numeric(NUM45_PUB)) %>% select(-NUM45_PUB)
d <- d %>% filter(is.na(DEBT_MDN) == FALSE)
d <- d %>% filter(UGDS > 100)
d <- d %>% select(-SATVRMID, -SATMTMID, -ACTCMMID)
d <- d %>% mutate(SAT_AVG_ALL = as.numeric(SAT_AVG_ALL))
d <- d %>% filter(is.na(NETPRICE_AGG) == FALSE)
d <- d %>% filter(is.na(AVGFACSAL) == FALSE)
d <- d %>% filter(PREDDEG == 3) %>% select(-PREDDEG)
d <- d %>% filter(is.na(ROOMBOARD_ON) == FALSE)
d <- d %>% mutate(LO_INC_DEBT_DIFF = ((LO_INC_DEBT_MDN + MD_INC_DEBT_MDN)/2) - HI_INC_DEBT_MDN)
d <- d %>% mutate(SCHOOL = INSTNM)
d <- d %>% mutate(OVERALL_YR4_N = as.numeric(OVERALL_YR4_N))
d <- d %>% mutate(LO_INC_YR4_N = as.numeric(LO_INC_YR4_N))
d <- d %>% mutate(MD_INC_YR4_N = as.numeric(MD_INC_YR4_N))
d <- d %>% mutate(HI_INC_YR4_N = as.numeric(HI_INC_YR4_N))
d <- d %>% filter(is.na(MD_INC_YR4_N) == FALSE)
d <- d %>% filter(is.na(HI_INC_YR4_N) == FALSE)
d <- d %>% filter(is.na(LO_INC_YR4_N) == FALSE)
d <- d %>% mutate(AVG_INC_EST = ((15000*LO_INC_YR4_N)+ (52500 * MD_INC_YR4_N ) + (HI_INC_YR4_N * 181000)) / OVERALL_YR4_N)
d <- d %>% mutate(INC_GAIN = EARNAFTER10 - AVG_INC_EST)
d <- d %>% mutate(GEN_GAP = MALEEARN - NOTMALEEARN)
d <- d %>% filter(is.na(PFTFAC) == FALSE)
d <- d %>% mutate(UGDS_NRA = as.numeric(UGDS_NRA))
d <- d %>% select(-ICLEVEL)
d4 <- read_csv("MERGED2021_22_PP.csv")
#2020-21 has some data that wasn't collected before then, namely urban vs. rural, so we also bring that data in
d4 <- d4 %>% select(UNITID, LOCALE, LATITUDE, LONGITUDE)
d <- left_join(d, d4 , by="UNITID")
d$LOCALE[1045] = "13"
d$LATITUDE[1045] = 40.79139
d$LONGITUDE[1045] = -77.85861
d <- d %>% filter(is.na(LOCALE) == FALSE)

#This code takes the longest to run, it scrapes the web for information about faculty publishes
tables <-
  gsub(" ", "", paste("https://exaly.com/institutions/articles3/", as.character(1))) %>%
  read_html() %>%
  html_nodes(css = "table")
pub <- tables %>%
  purrr::pluck(1) %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names()
dfs <- vector("list", 850)
pubtot <- data.frame(number = NULL, INSTNM = NULL, articles = NULL, citations = NULL, elite_art = NULL)
for(i in 1:850) {
  table <-
    gsub(" ", "", paste("https://exaly.com/institutions/articles3/", as.character(i))) %>%
    read_html() %>%
    html_nodes(css = "table")
  dfs[[i]] <- table %>%
    purrr::pluck(1) %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    rename(
      INSTNM = institution,
      elite_art = x
    )
  pubtot <- rbind(pubtot, dfs[[i]])
} 

d <- left_join(d, pubtot , by="INSTNM")
d <- d %>% mutate(elite_art = as.numeric(gsub(",","",elite_art)))
d <- d %>% mutate(TYPE = as.numeric(TYPE))
# This is just so I can predict publishes for the schools I don't have data for
m2 <- lm(elite_art ~ AVGFACSAL + UGDS + ENDOWBEGIN + GRADS + TUITIONFEE_IN + STUFACR, data=d)
summary(m2)
d <- d %>% mutate(elite_art = ifelse(is.na(elite_art) == TRUE, (44.17 + .03681 * AVGFACSAL + ENDOWBEGIN * (2.301 * 10 ^ (-7)) - UGDS * (.008864) + GRADS * .05377 - TUITIONFEE_IN * .004837 - 16.74 * STUFACR), elite_art))
for (i in 1:1364) {
  if(is.na(d$citations[i]) == TRUE) {
    next
  }
  else {
    mc <- (substr(d$citations[i], nchar(d$citations[i]), nchar(d$citations[i])) == "M")
    kc <- (substr(d$citations[i], nchar(d$citations[i]), nchar(d$citations[i])) == "K")
    if(mc == TRUE) {
      d$citations[i] = gsub(" ", "", paste(substr(d$citations[i], 1, nchar(d$citations[i]) - 1) , "00000"))
    }
    if(kc == TRUE) {
      d$citations[i] = gsub(" ", "", paste(substr(d$citations[i], 1, nchar(d$citations[i]) - 1) , "00"))
    }
    else {
      d$citations[i] = d$citations[i]
    }
  }
}
d <- d %>% mutate(citations = as.numeric(str_replace_all(citations, "[^[:alnum:]]", "")))
m3 <- lm(citations ~ PFTFAC + AVGFACSAL + ENDOWBEGIN + UGDS + GRADS + TUITIONFEE_IN + STUFACR + TYPE, data=d)
summary(m3)
m4 <- lm(citations ~ elite_art, data=d)
summary(m4)
d <- d %>% mutate(LOCALE = as.character(LOCALE))
d <- d %>% mutate(PUBSCORE = 0)
d <- d %>% mutate(elp = elite_art / UGDS)
# This is where we start creating scores for individual categories
for (i in 1:1364) {
  if(is.na(d$citations[i]) == TRUE) {
    next
  }
  else {
    if (d$elp[i] > .1) {
      d$PUBSCORE[i] = 100
    }
    else if (d$elp[i] < 0) {
      d$PUBSCORE[i] = 0
    }
    else {
      d$PUBSCORE[i] = (1010 * d$elp[i]) - (100 * (d$elp[i])^2 )
    }
  }
}
d <- d %>% mutate(PUBSCORE = log(PUBSCORE + 10))
d <- d %>% mutate(PUBSCORE = (PUBSCORE - 2.3) * (100 / 2.4))
d <- d %>% mutate(PRESTIGE_NOTEST = ifelse(is.na(elite_art) == TRUE, (.19 * (FUNDPERSTU - 1550)/1261.19) + .81 * (-31.9 * log(ADM_RATE)), .34 * PUBSCORE + (.12 * (FUNDPERSTU - 1550)/1261.19) + .54 * (-31.9 * log(ADM_RATE))))
d <- d %>% mutate(PRESTIGE_TEST = ifelse(is.na(elite_art) == TRUE, (ifelse(is.na(SAT_AVG_ALL)==FALSE, (.10 * (FUNDPERSTU - 1550)/1261.19) + .60 * (-31.9 * log(ADM_RATE)) + .30 * ((SAT_AVG_ALL - 785)/7.81), PRESTIGE_NOTEST)), (ifelse(is.na(SAT_AVG_ALL)==FALSE, (.06 * (FUNDPERSTU - 1550)/1261.19) + .40 * (-31.9 * log(ADM_RATE)) + .20 * ((SAT_AVG_ALL - 785)/7.81) + .34 * PUBSCORE, PRESTIGE_NOTEST)) ))
d <- d %>% mutate(GRADS = ifelse(is.na(GRADS) == TRUE, 0, GRADS))
d <- d %>% mutate(SIZE_SMALL = .15*(100 * (UGDS/(UGDS+GRADS))) + .25*((0.00000869450934584 * STUFACR^2) - 0.0864907850308 * STUFACR + 100) + .6*(ifelse(UGDS > 5000, 129.223203223 - (0.00612618752617 * UGDS) + (.000000056309376308) * (UGDS^2), 100)))
d <- d %>% mutate(SIZE_MED = .10*(100 * (UGDS/(UGDS+GRADS))) + .25*((0.00000869450934584 * STUFACR^2) - 0.0864907850308 * STUFACR + 100) + .65*(ifelse((UGDS > 15000 | UGDS < 5000), ifelse(UGDS < 5000, (UGDS - 107)/4893, (UGDS - 15000)/65170), 100)))
d <- d %>% mutate(SIZE_LARGE = .05*(100 * (UGDS/(UGDS+GRADS))) + .25*((0.00000869450934584 * STUFACR^2) - 0.0864907850308 * STUFACR + 100) + .7*(ifelse(UGDS < 15000, (UGDS - 107)/14893, 100)))
d <- d %>% mutate(BW_RATIO = UGDS_BLACK / (UGDS_WHITE + 1))
d <- d %>% mutate(HW_RATIO = UGDS_HISP / (UGDS_WHITE + 1))
d <- d %>% mutate(AW_RATIO = UGDS_ASIAN / (UGDS_WHITE + 1))
d <- d %>% mutate(NAW_RATIO = (UGDS_AIAN + UGDS_NHPI) / (UGDS_WHITE + 1))
d <- d %>% mutate(DIV_SCORE = 5.339 * ((.12 * BW_RATIO) + (.1873 * HW_RATIO) + (.06 * AW_RATIO) + (.01 * NAW_RATIO)))
d <- d %>% mutate(NWW_FAC_RATIO = (FAC_BLCK + FAC_ASN + FAC_INTL + FAC_HISP) / (FAC_WHT + 1))
d <- d %>% mutate(COMP_RT_DIFF = ((COMPLETE_RT_BLCK + COMPLETE_RT_HISP) / 2) - COMPLETE_RT_WHT + .7797)
d <- d %>% mutate(RACE_DIVERSITY = ifelse((is.na(COMP_RT_DIFF) == TRUE), ((65 * DIV_SCORE) + (.35 * (100 * NWW_FAC_RATIO))), (50 * DIV_SCORE) + (.2 * (100 * NWW_FAC_RATIO)) + (.3 * (COMP_RT_DIFF * (100/1.30845)))))
d <- d %>% mutate(EXTRATIME = COMPLETE_RT - COMPLETE_RT_4YR)
d <- d %>% mutate(INSTATE_DISCOUNT = 128.64 * (TUITIONFEE_OUT - TUITIONFEE_IN) / (TUITIONFEE_OUT))
d <- d %>% mutate(AFFORD = 1.165647483 * (.45 * (106.422200281 - 0.00331624635484 * NETPRICE_AGG + (2.3567690911*(10^-8) * (NETPRICE_AGG^2))) + .3 * (114.207774913 - 0.00318516880368 * DEBT_MDN - (3.869439947*(10^-8) * (DEBT_MDN^2))) + .1 * (ifelse(is.na(REPAYRATE) == TRUE, .8, (REPAYRATE - .5468923) * (100/.5938147))) + .025*(INSTATE_DISCOUNT) + .025 * (124.515005417 - 0.00227703149633 * TOT_COST + (8.3573445118*(10^-9) * (TOT_COST^2))) + .1*(ifelse(is.na(EXTRATIME) == TRUE, 50, (EXTRATIME+.122) * 100))))
d <- d %>% mutate(EST_PPLUS = (PPLUS_PCT_HIGH + PPLUS_PCT_LOW) / 2)
d <- d %>% mutate(LO_INC_DEBT_MDN = ifelse(is.na(LO_INC_DEBT_MDN) == TRUE, 0, LO_INC_DEBT_MDN))
d <- d %>% mutate(PELL_DEBT_MDN =  ifelse(is.na(PELL_DEBT_MDN) == TRUE, 0, PELL_DEBT_MDN))
d <- d %>% mutate(MD_INC_DEBT_MDN = ifelse(is.na(MD_INC_DEBT_MDN) == TRUE, 0, MD_INC_DEBT_MDN))
d <- d %>% mutate(HI_INC_DEBT_MDN = ifelse(is.na(HI_INC_DEBT_MDN) == TRUE, 0, HI_INC_DEBT_MDN))
d <- d %>% mutate(FINSTURATE = FINAIDSTU / UGDS)
d <- d %>% filter(INSTNM != "Bacone College")
d <- d %>% mutate(LO_INC_PAY_RT = (NETPRICE_LESS30 / TOT_COST) + .05)
d <- d %>% mutate(FIN_AVAIL = 1.245 * (.2 * (119.411764706 - 803.921568627 * EST_PPLUS + (1098.03921569 * (EST_PPLUS^2))) + .2 * (PCTFLOAN * 103) + .1 * ((PCTPELL - .09) * 120) + .05 * (FINSTURATE * 250) + .45 * (100 - LO_INC_PAY_RT * 3.14787403128 - (119.867588202 * (LO_INC_PAY_RT^2)))))
d <- d %>% mutate(ENDOW_GRWTH = (ENDOWEND - ENDOWBEGIN) / ENDOWBEGIN)
d <- d %>% mutate(ENDOW_PER = (ENDOWEND) / (UGDS+GRADS))
m1 <- lm(ENDOWEND ~ STUFACR + STUFACR^2 + FUNDPERSTU + AVGFACSAL + PFTFAC, data=d)
summary(m1)
d <- d %>% mutate(SCHL_RESC = ifelse(is.na(ENDOWEND)==TRUE, .8 * (.1 * ((0.00000869450934584 * STUFACR^2) - 0.0864907850308 * STUFACR + 100) + .45 * (((FUNDPERSTU - 1550)/1261.19)  + .4 * ((AVGFACSAL - 1333)/191.50)) + .05 * (PFTFAC)), 1.7 * ((.125 * 14.2 * (ENDOW_GRWTH + .97818) + .375 * (ENDOW_PER / 30845.49)) + .05 * ((0.00000869450934584 * STUFACR^2) - 0.0864907850308 * STUFACR + 100) + .225 * (((FUNDPERSTU - 1550)/1261.19)  + .2 * ((AVGFACSAL - 1333)/191.50)) + .025 * (PFTFAC) ))) 
d <- d %>% mutate(FFAC_R = log((FAC_FEMALE / FAC_MALE) + 1))
d <- d %>% mutate(GENDER_EQUITY = ifelse((is.na(GEN_GAP)) == TRUE, 50 + 24 * (FFAC_R - .75)  , .2 * (100 - ((100/.57) * abs(UGDS_MEN - .43)) ) + .45 * (FFAC_R * 24.219) + .35 * (91.29 - 0.0009820867379 * GEN_GAP)) )
d <- d %>% mutate(EMP_RT = AFTER10EMP / UGDS)
d6 <- read_csv("MERGED2008_09_PP.csv")
d5 <- d6 %>% select(UNITID, UGDS)
d5 <- d5 %>% mutate(UG08 = as.numeric(UGDS)) %>% select(-UGDS)
d <- left_join(d, d5 , by="UNITID")
d <- d %>% mutate(EMP_RT = AFTER10EMP / ifelse(is.na(UG08)==FALSE, UG08, UGDS))
d <- d %>% mutate(INC_GAIN_PER = INC_GAIN / AVG_INC_EST)
d <- d %>% filter(is.na(INC_GAIN_PER) == FALSE)
d <- d %>% filter(EMP_RT < 2)
d <- d %>% mutate(OUTCOMES = 1.222 * ((.15 * (EMP_RT - .02) * 70)  + .65 * ((EARNAFTER10 - 21276) / 1003) + .2 * ((INC_GAIN_PER + .7319202) / .01626536739)))
d <- d %>% mutate(RET_AND_COMPLETE = 1.142536449 * (.1 * ifelse(is.na(EXTRATIME) == TRUE, 50, (EXTRATIME+.122) * 100) + .4 * ((COMPLETE_RT - .1) * 104) + .5 * ((RETENTION - .09448819) * 129.2282498)))
d <- d %>% mutate(SAT_AVG_ALL = ifelse(is.na(SAT_AVG_ALL) == TRUE, 835.11028 - (78.62504 * ADM_RATE) + (0.01332 * AVGFACSAL) + (435.58822 * COMPLETE_RT), SAT_AVG_ALL))
d <- d %>% mutate(COMPETE = (ADM_RATE * 13.6506285863) + (86.3493714137 * (ADM_RATE) ^ 2))
d <- d %>% mutate(LOCALE = substr(LOCALE, 1, 1))
d <- d %>% mutate(LOCALE = ifelse(LOCALE == "4", "3", LOCALE))
d <- d %>% mutate(LOCALE = as.numeric(LOCALE))
d <- d %>% select(UNITID, INSTNM, CITY, SAT_AVG_ALL, ZIP, STABBR, REGION, LATITUDE, LONGITUDE, PRESTIGE_NOTEST, PRESTIGE_TEST, SIZE_SMALL, SIZE_MED, SIZE_LARGE, RACE_DIVERSITY, AFFORD, FIN_AVAIL, GENDER_EQUITY, OUTCOMES, RET_AND_COMPLETE, TYPE, COMPETE, LOCALE)

rsconnect::setAccountInfo(name='casey-watkins-apps', token='461A73D91D806B811A4661310D3119A7', secret='e2KlijMxe9s5EHTbUB+hFO5cIVCUgHHb3lnYPumX')
zipdata <- read_excel("uszips.xlsx", col_types = c("text", "numeric", "numeric", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text"))
zipdata <- zipdata %>% select(zip, lat, lng)
for(i in 1:33788) {
  if (nchar(zipdata$zip[i]) == 3) {
    zipdata$zip[i] <- gsub(" ", "", paste("00",zipdata$zip[i]))
  }
  else if (nchar(zipdata$zip[i]) == 4) {
    zipdata$zip[i] <- gsub(" ", "", paste("0", zipdata$zip[i]))

  }
  else {
    zipdata$zip[i] <- zipdata$zip[i]
  }
}
zipdata <- zipdata %>% rename(ZIP = zip)
inputs <- d %>% filter(is.na(RET_AND_COMPLETE) == FALSE)
inputs <- inputs %>% mutate(LATITUDE = as.numeric(LATITUDE))
inputs <- inputs %>% mutate(LONGITUDE = as.numeric(LONGITUDE))
inputd <- inputs %>% mutate(dist = 0)
inputd <- inputd %>% mutate(DIST_GEO = 0)
inputd <- inputd %>% mutate(TST_RANK = 0)
inputd <- inputd %>% mutate(LOC_RANK = 0)
inputd <- inputd %>% mutate(SCORE = 0)
m2 <- lm(SAT_AVG_ALL ~ ADM_RATE + AVGFACSAL + COMPLETE_RT, data=d)
summary(m2)

# I needed to write these CSVs for the cleaned data because the free version of shinyapp can't store more than 1000 MB
# and all the dataset combined we more than that
inputd <- read.csv("usedata.csv")
zipdata <- read.csv("zipdata.csv")

#This is the list generating function, you'll notice it is also written in app.R
gen_list <- function(GEO_WT = geo, ZIP = zip, AP_WT = academic, SIZE_WT = size, SIZE = size_pref, 
                     RACE_WT = race, AFF_WT = afford, FA_WT = avail, TEST_YN = test, SAT_WT = tst_pref, 
                     SAT = tstscore, RES_WT = res, GEN_WT = gender, OUT_WT = outcomes, RET_WT = retention,
                     COMP_WT = compete, LOC = loc_pref, LOC_WT = loc) {
  find = 7
  for (i in 1:33788) {
    if(ZIP == zipdata$ZIP[i]) {
      find = i
    }
  }
  lati = zipdata$lat[find]
  long = zipdata$lng[find]
  for (i in 1:1344) {
    lat = inputd$LATITUDE[i]
    lng = inputd$LONGITUDE[i]
    inputd$dist[i] <<- distHaversine(c(lng, lat), c(long, lati))
  }
  for (i in 1:1344) {
    inputd$DIST_GEO[i] <<- ((3.056103413*(10^-13)*(inputd$dist[i])^2) - (0.00000781422745525 * (inputd$dist[i])) + 100)
  }
  small = 100
  for (i in 1:1344) {
    if(inputd$DIST_GEO[i] < small) {
      small <- inputd$DIST_GEO[i]
    }
  }
  for (i in 1:1344) {
    inputd$DIST_GEO[i] <<- ((inputd$DIST_GEO[i] - small) * (100 / (100 - small)))
  }
  for (i in 1:1344) {
    if(inputd$SAT_AVG_ALL[i] > SAT) {
      inputd$TST_RANK[i] <<- 100 - (inputd$SAT_AVG_ALL[i] - SAT) * (100 / (1566 - SAT))
    }
    else {
      inputd$TST_RANK[i] <<- (inputd$SAT_AVG_ALL[i] - 785) * (100 / (SAT - 785))
    }
  }
  
  if (LOC == 1) {
    for (i in 1:1344) {
      if(inputd$LOCALE[i] == 1) {
        inputd$LOC_RANK[i] <<- 100
      }
      else if(inputd$LOCALE[i] == 2) {
        inputd$LOC_RANK[i] <<- 50
      }
      else {
        inputd$LOC_RANK[i] <<- 0
      }
    }
  }
  
  else if (LOC == 2) {
    for (i in 1:1344) {
      if(inputd$LOCALE[i] == 1) {
        inputd$LOC_RANK[i] <<- 50
      }
      else if(inputd$LOCALE[i] == 2) {
        inputd$LOC_RANK[i] <<- 100
      }
      else {
        inputd$LOC_RANK[i] <<- 50
      }
    }
  }
  
  else {
    for (i in 1:1344) {
      if(inputd$LOCALE[i] == 1) {
        inputd$LOC_RANK[i] <<- 0
      }
      else if(inputd$LOCALE[i] == 2) {
        inputd$LOC_RANK[i] <<- 50
      }
      else {
        inputd$LOC_RANK[i] <<- 100
      }
    }
  }
  
  TOT_WT = GEO_WT + AP_WT + SIZE_WT + RACE_WT + AFF_WT + FA_WT + ifelse(TEST_YN == "YES", SAT_WT, 0) + RES_WT + GEN_WT + OUT_WT + RET_WT + COMP_WT + LOC_WT
  GEO_M = GEO_WT / TOT_WT
  AP_M = AP_WT / TOT_WT
  SIZE_M = SIZE_WT / TOT_WT
  RACE_M = RACE_WT / TOT_WT
  AFF_M = AFF_WT / TOT_WT
  FA_M = FA_WT / TOT_WT
  SAT_M = SAT_WT / TOT_WT
  RES_M = RES_WT / TOT_WT
  GEN_M = GEN_WT / TOT_WT
  OUT_M = OUT_WT / TOT_WT
  RET_M = RET_WT / TOT_WT
  COMP_M = COMP_WT / TOT_WT
  LOC_M = LOC_WT / TOT_WT
  if (TEST_YN == "Yes") {
    if (SIZE == 1) {
      for (i in 1:1344) {
        inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_TEST[i] + SIZE_M * inputd$SIZE_SMALL[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i] + COMP_M * inputd$COMPETE[i] + LOC_M * inputd$LOC_RANK[i])
      }
    }
    else if (SIZE == 2) {
      for (i in 1:1344) {
        inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_TEST[i] + SIZE_M * inputd$SIZE_MED[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
      }
    }
    else {
      for (i in 1:1344) {
        inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_TEST[i] + SIZE_M * inputd$SIZE_LARGE[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
      }
    }
  }
  else {
    if (SIZE == 1) {
      for (i in 1:1344) {
        inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_NOTEST[i] + SIZE_M * inputd$SIZE_SMALL[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i] + SAT_M * inputd$TST_RANK[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
      }
    }
    else if (SIZE == 2) {
      for (i in 1:1344) {
        inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_NOTEST[i] + SIZE_M * inputd$SIZE_MED[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i] + SAT_M * inputd$TST_RANK[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
      }
    }
    else {
      for (i in 1:1344) {
        inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_NOTEST[i] + SIZE_M * inputd$SIZE_LARGE[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i] + SAT_M * inputd$TST_RANK[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
      }
    }
  }
  
  return(inputd)
}
inputd <- inputd[!duplicated(inputd), ]

gen_list(GEO_WT = 7, ZIP = "43022", AP_WT = 7, SIZE_WT = 7, SIZE = 3, 
         RACE_WT = 7, AFF_WT = 1, FA_WT = 1, TEST_YN = "Yes", SAT_WT = 10, 
         SAT = 1300, RES_WT = 1, GEN_WT = 1, OUT_WT = 9, RET_WT = 1, 
         COMP_WT=10, LOC = 2, LOC_WT = 9)
size_num = 0
loc_num = 0
bootswatch_themes()
write_csv(inputd, 'C:\\Users\\world\\OneDrive\\Documents\\usedata.csv')
write_csv(zipdata, file = 'C:zipdata.csv')
#I'm not going to comment the app because it may mess with its functionality, but it essentially just sets up the UI and server,
#and is pretty comprehensible.