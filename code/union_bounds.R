# Côté-Gendreau 2026
# This file that establishes temporal bounds to unions (creation to dissolution) based on contextual information

# Packages we need
list_packages <- c("dplyr","readr","stringr","tidytable","lubridate")

# Unload all other packages to make sure functions work as expected
if (length(sessionInfo()$otherPkgs) != 0) {
  to_detach <- names(sessionInfo()$otherPkgs)[!names(sessionInfo()$otherPkgs) %in% list_packages]
  if (length(to_detach) != 0) {
    invisible(suppressWarnings(lapply(paste("package:", to_detach, sep=""),
                                      detach,
                                      character.only = TRUE,
                                      unload = TRUE)))
  }
}

installed <- list_packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(list_packages[!installed])
}

lapply(list_packages[!(list_packages %in% names(sessionInfo()$otherPkgs))], library, character.only = TRUE)

options(scipen = 999)

inds <- read_csv("data_modif/inds_mod.csv", guess_max = 5000) %>% 
  mutate(across(c(DOD,DOB), ~as.Date(.x)))
unions <- read_csv("data_modif/unions_mod.csv")
mentions <- read_csv("data_modif/mentions_mod.csv")

# Identify "real" records (as opposed to estimated records)
# I keep those for which at least the month is known
real_b <- mentions %>% 
  filter(!(CodeLocation %in% c(0,22222,2,7600,6909,7900:7904)) & (CodeLocation < 8000 | CodeLocation >= 20000)) %>% 
  filter(Type == "b", Role == 1, flag_Date_precise != 2) %>% 
  write_csv("data_modif/real_b.csv")

real_s <- mentions %>% 
  filter(!(CodeLocation %in% c(0,22222,2,7600,6909,7900:7904)) & (CodeLocation < 8000 | CodeLocation >= 20000)) %>% 
  filter(Type == "s", Role == 1, flag_Date_precise != 2) %>% 
  write_csv("data_modif/real_s.csv")

# To make union bounds more precise, let's calculate dates of child conception

# When we don't have birth records, we can use some death records
# Take all records identified as stillbirths or births where the baby died immediately after birth
# I also consider death records where age is less than 1 year or expressed in months (1 year not okay, 14 months okay)
conceptions <- real_b %>% 
  bind_rows(real_s %>% filter(!(idInd %in% real_b$idInd))) %>% 
  filter(stillborn == 1 | (!is.na(Age_tf) & Age_tf <= 366) | str_sub(Age,-1,-1) %in% c("m","s")) %>% 
  mutate(DateEvent_mod = if_else(DateEvent_precise == 99999999, 
                                     ymd(Date_precise) - ddays(Age_tf),
                                     ymd(DateEvent_precise) - ddays(Age_tf),
                                     missing = NA_Date_),
         DateEvent_mod = as.Date(DateEvent_mod)) %>% 
  left_join(select(inds,idMother,idFather,idInd)) %>% 
  mutate(Date_conception = DateEvent_mod - dweeks(40)) %>% 
  arrange(Date_conception)

conceptions_long <- conceptions %>% 
  pivot_longer(c(idMother,idFather),names_to = "Parent", values_to = "idParent") %>% 
  filter(idParent != -1) %>% 
  mutate(Sex = if_else(Parent == "idMother","f","m")) %>% 
  mutate(Type = "i",
         DateEvent_mod = if_else(Sex == "m", Date_conception, DateEvent_mod),
         Presence = NA,
         Presence_revised = "p",
         across(c(idAct,idMention), ~.x*-1),
         Role = if_else(Sex == "m",3,4),
         Age = "xxx") %>% 
  select(-c(idInd,Parent,Age_tf)) %>% 
  rename(idInd = idParent) %>% 
  mutate(across(c(DateEvent_mod,Date_conception), ~as.integer(format(.x,"%Y%m%d")))) %>% 
  mutate(Date_mod_precise = DateEvent_mod,
         DateEvent_precise = DateEvent_mod,
         Role_revised = Role) %>% 
  write_csv("data_modif/conceptions.csv")

union_order <- unions %>% 
  mutate(yearU = year(DateU)) %>% 
  arrange(yearU,desc(QualityDate)) %>% 
  group_by(idHusband) %>% 
  mutate(rank_h = row_number()) %>% 
  group_by(idWife) %>% 
  mutate(rank_w = row_number()) %>% 
  ungroup()

unions <- unions %>% 
  dplyr::left_join(select(conceptions,Date_first_conception=Date_conception,idHusband=idFather,idWife=idMother),join_by(idHusband,idWife),
            multiple = "first") %>%
  dplyr::left_join(select(conceptions,Date_last_conception=Date_conception,idHusband=idFather,idWife=idMother),join_by(idHusband,idWife),
            multiple = "last") %>%
  mutate(gap_fc = Date_first_conception-DateU) %>% 
  mutate(DateU = if_else(QualityDate > 3 & !is.na(Date_first_conception) & gap_fc < dyears(1), Date_first_conception, DateU)) %>%
  left_join(select(union_order, rank_h, idHusband, idWife)) %>% 
  left_join(select(union_order, rank_w, idHusband, idWife)) %>% 
  arrange(rank_h,rank_w)
  
unions_men <- unions %>% 
  group_by(idHusband) %>% 
  mutate(next_marr_m = lead(DateU)) %>% 
  ungroup() %>% 
  select(idUnion, next_marr_m)

unions_women <- unions %>% 
  arrange(rank_w) %>% 
  group_by(idWife) %>% 
  mutate(next_marr_w = lead(DateU)) %>% 
  ungroup() %>% 
  select(idUnion, next_marr_w)

unions_bound <- unions %>% 
  left_join(select(filter(inds,Sex=="m"),DateDeath_m = DOD,idHusband=idInd)) %>% 
  left_join(select(filter(inds,Sex=="f"),DateDeath_w = DOD,idWife=idInd)) %>%
  left_join(unions_men) %>% 
  left_join(unions_women) %>% 
  mutate(Date_end = pmin(DateDeath_m,DateDeath_w,next_marr_m,next_marr_w,na.rm = T),
         w_remarries = if_else(!is.na(next_marr_w),1,0),
         m_remarries = if_else(!is.na(next_marr_m),1,0),
         flag_both_remarry = w_remarries & m_remarries,
         DateU = if_else(!is.na(Date_end) & !is.na(DateU) & DateU > Date_end, Date_end, DateU)) %>% 
  write_csv("data_modif/unions_bounds.csv")

unions_long <- unions_bound %>% 
  pivot_longer(c(idHusband,idWife),names_to = "Spouse", values_to = "idInd") %>% 
  arrange(DateU)

# Add information about people's parents' births, deaths, etc.
inds <- inds %>% 
  arrange(DOB) %>% 
  left_join(select(unions_bound,QualityDateMP = QualityDate,idFather = idHusband,idMother = idWife,DateMP = DateU)) %>% 
  dplyr::left_join(select(unions_bound,QualityDateM1_mother = QualityDate,idMother = idWife,DateM1_mother = DateU), by = join_by(idMother), multiple="first") %>% 
  dplyr::left_join(select(unions_long,QualityDateM1 = QualityDate,idInd,DateM1 = DateU), by = join_by(idInd), multiple="first") %>% 
  dplyr::left_join(select(filter(.,QualityDateBirth <= 5,idMother!=-1),QualityDateChild1_mother = QualityDateBirth,idMother,DateChild1_mother = DOB), by = join_by(idMother), multiple="first") %>% 
  dplyr::left_join(select(filter(.,QualityDateBirth <= 5,idMother!=-1),QualityDateChildn = QualityDateBirth,idMother,DateChildn = DOB), by = join_by(idInd == idMother), multiple="last") %>% 
  dplyr::left_join(select(.,DOB,DOD,QualityDateBirth,QualityDateDeath,idInd),join_by(idFather==idInd),suffix=c("","_father")) %>% 
  dplyr::left_join(select(.,DOB,DOD,QualityDateBirth,QualityDateDeath,idInd),join_by(idMother==idInd),suffix=c("","_mother")) %>% 
  write_csv("data_modif/inds_mod.csv")

rm(list = ls())
