### data cleaning ###

# libraries
library(dplyr)
library(tidyr)
library(naniar)

# load ESS data round 9
ess <- read.csv("ESS9e03_1.csv")

# create new dataframe with selected variables
data <- ess %>%
  select(idno, cntry, agea, eisced, gndr, rlgdgr, domicil, blgetmg, hinctnta,
         uemp3m, isco08, hincfel, stfdem,
         stfeco, gincdif, sofrdst, lrscale, trstep, trstun, stfgov, euftf, vteurmmb, vteubcmb,
         vteumbgb, imsmetn, imdfetn,impcntr, imbgeco, imwbcnt, trstprl, trstplt, 
         trstprt,polintr, psppsgva, psppipla, frprtpl, freehms, hmsacld, atchctr,
         imueclt, impenv, vote,
         prtvtcat, prtvtdbe, prtvtdbg, prtvtgch, prtvtbcy, prtvtecz, prtvede1,
         prtvtddk, prtvtgee, prtvtees, prtvtdfi, prtvtdfr, prtvtcgb, prtvtahr,
         prtvtfhu, prtvtcie, prtvtcis, prtvtcit, prtvblt1, prtvtalv, prtvtme,
         prtvtgnl, prtvtbno, prtvtdpl, prtvtcpt, prtvtrs, prtvtcse, prtvtfsi,
         prtvtdsk) %>% 
  rename(vote_at = prtvtcat, # rename voting variables for better overview
         vote_be = prtvtdbe,  
         vote_bg = prtvtdbg,
         vote_ch = prtvtgch,
         vote_cy = prtvtbcy,
         vote_cz =prtvtecz,
         vote_de = prtvede1,
         vote_dk = prtvtddk,
         vote_ee = prtvtgee,
         vote_es = prtvtees,
         vote_fi = prtvtdfi,
         vote_fr = prtvtdfr,
         vote_gb = prtvtcgb,
         vote_hr = prtvtahr,
         vote_hu = prtvtfhu,
         vote_ie = prtvtcie,
         vote_is = prtvtcis,
         vote_it = prtvtcit,
         vote_lt = prtvblt1, 
         vote_lv = prtvtalv,
         vote_me = prtvtme,
         vote_nl = prtvtgnl,
         vote_no = prtvtbno,
         vote_pl = prtvtdpl,
         vote_pt = prtvtcpt,
         vote_rs = prtvtrs,
         vote_se = prtvtcse,
         vote_si = prtvtfsi,
         vote_sk = prtvtdsk)

# remove data that needs to be excluded
data <- data %>%
  subset(vote != 3) %>% # remove observations that are not eligible to vote -> 3710
  filter(vote_be != 11 | is.na(vote_be)) %>% # exclude observations that voted for parties that are not in populism classification data sources -> 9
  filter(vote_ee != 9 | is.na(vote_ee)) %>%
  filter(vote_lt != 4 & vote_lt != 9 | is.na(vote_lt)) %>%
  filter(vote_no != 9 | is.na(vote_no)) %>%
  filter(vote_ch != 9 | is.na(vote_ch)) %>%
  filter(cntry != "ME" & cntry != "PT" & cntry != "HR", cntry != "BG") # exclude countries because of party coalitions -> 5987

# recode values (refusal, don't know, no answer, missing) as NAs
data <- data %>%
  replace_with_na(replace = list(   
    agea = 999,
    eisced = c(55, 77, 88, 99),
    gndr = 9,
    rlgdgr = c(77, 88, 99),
    domicil = c(7, 8, 9),
    blgetmg = c(7, 8, 9),
    hinctnta = c(77, 88, 99),
    uemp3m = c(7, 8, 9),
    isco08 = c(66666, 77777, 88888, 99999),
    hincfel = c(7, 8, 9),
    stfdem = c(77, 88, 99),
    stfeco = c(77, 88, 99),
    gincdif = c(7, 8, 9),
    sofrdst = c(7, 8, 9),
    lrscale = c(77, 88, 99),
    trstep = c(77, 88, 99),
    trstun = c(77, 88, 99),
    stfgov = c(77, 88, 99),
    euftf = c(77, 88, 99),
    vteurmmb = c(77, 88, 99),
    vteubcmb = c(77, 88, 99),
    vteumbgb = c(77, 88, 99),
    imsmetn = c(7, 8, 9),
    imdfetn = c(7, 8, 9),
    impcntr = c(7, 8, 9),
    imbgeco = c(77, 88, 99),
    imwbcnt = c(77, 88, 99),
    trstprl = c(77, 88, 99),
    trstplt = c(77, 88, 99),
    trstprt = c(77, 88, 99),
    polintr = c(7, 8, 9),
    psppsgva = c(7, 8, 9),
    psppipla = c(7, 8, 9),
    frprtpl = c(7, 8, 9),
    freehms = c(7, 8, 9),
    hmsacld = c(7, 8, 9),
    atchctr = c(77, 88, 99),
    imueclt = c(77, 88, 99),
    impenv = c(7, 8, 9),
    vote = c(7, 8, 9)
    ))


# create populism vote variable

# combine all county vote variables into one
data <- data %>%
  mutate(vote_party = coalesce(vote_at, vote_be, vote_bg, vote_ch, vote_cy,
                               vote_cz, vote_de, vote_dk, vote_ee, vote_es,
                               vote_fi, vote_fr, vote_gb, vote_hr, vote_hu,
                               vote_ie, vote_is, vote_it, vote_lt, vote_lv,
                               vote_me, vote_nl, vote_no, vote_pl, vote_pt,
                               vote_rs, vote_se, vote_si, vote_sk))%>%
  mutate(across(vote_party, as.factor))


# load data on populism classification
setwd("~/Uni/Master/Masterarbeit/Data")

pop_class <- read.csv("pop_class_parties.csv", sep = ";") %>%
  mutate(across(vote_party, as.factor))

# merge data with populism classification
data <- left_join(data, pop_class, by = c("cntry", "vote_party")) %>%
  select(-vote_party) # remove vote for party variable


# remove voted for party in country variables
data <- data %>%
  select(-c(vote_at, vote_be, vote_bg, vote_ch, vote_cy,
            vote_cz, vote_de, vote_dk, vote_ee, vote_es,
            vote_fi, vote_fr, vote_gb, vote_hr, vote_hu,
            vote_ie, vote_is, vote_it, vote_lt, vote_lv,
            vote_me, vote_nl, vote_no, vote_pl, vote_pt,
            vote_rs, vote_se, vote_si, vote_sk))


# createvalue for abstainers in vote_pop_1 and vote_pop_2 
data$vote_pop_1 <- ifelse(data$vote == 2, 4, data$vote_pop_1)
data$vote_pop_2 <- ifelse(data$vote == 2, 4, data$vote_pop_2)

# remove vote variable
data <- data %>%
  select(-vote)

# add country level data

# load county level data
country_level <- read.csv("country_level_data.csv", sep = ";") %>%
  select(-country)%>%
  mutate(across(c(elec_system, pop_choice_1, pop_choice_2), as.factor))

# merge county level data with dataset
data <- left_join(data, country_level, by = "cntry")


# group occupations into major occupational groups
data <- data %>%
  mutate(occup = case_when(
    isco08 < 1000 ~ 10,
    999 < isco08 & isco08 < 2000 ~ 1,
    1999 < isco08 & isco08 < 3000 ~ 2,
    2999 < isco08 & isco08 < 4000 ~ 3,
    3999 < isco08 & isco08 < 5000 ~ 4,
    4999 < isco08 & isco08 < 6000 ~ 5,
    5999 < isco08 & isco08  < 7000 ~ 6,
    6999 < isco08 & isco08 < 8000 ~ 7,
    7999 < isco08 & isco08 < 9000 ~ 8,
    8999 < isco08 ~ 9)) %>%
  mutate(across(occup, as.factor))%>%
  select(-isco08)

# combined EU membership vote variable
data <- data %>%
  mutate(vote_eu = ifelse(vteurmmb %in% 1:2, vteurmmb,
                          ifelse(vteubcmb %in% 1:2, vteubcmb,
                                 ifelse(vteumbgb %in% 1:2, vteumbgb, NA)))) %>%
  mutate(across(vote_eu, as.factor)) %>%
  select(-c(vteurmmb, vteubcmb, vteumbgb))


# combine political mistrust into one populist attitude variable
data$pop_att <- rowSums(data[,c("trstplt", "trstprl", "trstprt")])/3

# remove individual trust variables
data <- data %>%
  select(-c(trstprl, trstplt, trstprt))

# create different datasets for vote_pop_1 and vote_pop_2

data_pop_1 <- data %>%
  select(-c(vote_pop_2, pop_choice_2)) %>%
  drop_na(vote_pop_1) # drop NAs in vote variable


data_pop_2 <- data %>%
  select(-c(vote_pop_1, pop_choice_1)) %>%
  drop_na(vote_pop_2)  # drop NAs in vote variable


# save data
write.csv(data , "~/Uni/Master/Masterarbeit/Data/Clean Data/data.csv")
write.csv(data_pop_1 , "~/Uni/Master/Masterarbeit/Data/Clean Data/data_pop_1.csv")
write.csv(data_pop_2 , "~/Uni/Master/Masterarbeit/Data/Clean Data/data_pop_2.csv")
