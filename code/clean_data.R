# Côté-Gendreau 2026
# This file cleans the RPQA database in multiple ways to make it usable

# Packages we need
list_packages <- c("haven","dplyr","readr","tibble","stringr","tidytable","lubridate","readxl")

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

# Load SPSS files
# Only keep relevant variables and translate their names
# Remove some repeat individuals
# Save files as csv files in data_modif
inds <- read_sav("data_raw/data_INDIVIDU.2025-07.sav") %>% 
  select(idInd = idIndividu, Sex = Sexe, idFather = idPere, idMother = idMere,
         DateBirth = DateNaissance, QualityDateBirth = QualiteDateNaissance, DateDeath = DateDeces, QualityDateDeath = QualiteDateDeces, 
         Immigrant, Emigrant, OutOfPop = HorsPopulation, Ill = Illegitime,
         fname_std = prenomStandard, lname_std = nomStandard) %>% 
  filter(idInd != -1) %>% 
  arrange(DateBirth) %>% 
  distinct(idInd, .keep_all = TRUE) %>%
  write_csv("data_modif/inds.csv")
  
unions <- read_sav("data_raw/data_UNION.2025-07.sav") %>% 
  select(idUnion, idHusband = idHomme, idWife = idFemme, Date, QualityDate = QualiteDate, CodeLocation = CodeLieu) %>% 
  write_csv("data_modif/unions.csv")

# ObiitOndoiement is a dummy variable that means that a birth record contains a date of death or that a death record contains a date of birth or baptism
# Date refers to the date of recording while DateEvent refers to the date when the event being recorded occurred
# Hence, DateEvent is at the latest the same as Date but often 1-2 days prior
acts <- read_sav("data_raw/data_ACTE.2025-07.sav") %>% 
  select(idAct = idActe, Type, CodeLocation = CodeParoisse, Date, DateEvent = DateEvenement, Provenance, ObiitOndoiement)

# Role indicates the role of the individual mentioned in the event (e.g. subject, parent, etc.)
mentions <- read_sav("data_raw/data_MENTION.2025-07.sav") %>% 
  select(idMention, idAct = idActe, Role, Sex = Sexe, idInd = idIndividu, Age, Rank = Rang, MaritalStatus = EtatMatrimonial, Presence,
         fname = prenom, fname_std = prenomStandard, lname = nom, lname_std = nomStandard) %>% 
  left_join(acts, by = "idAct") %>% 
  write_csv("data_modif/mentions.csv")

rm(acts)

# Unions without day or month: pick middle date
unions <- read_csv("data_modif/unions.csv") %>% 
  mutate(DateU = case_when(Date %in% c(99999999,0) ~ NA,
                           str_sub(Date, -4, -1) %in% c("0000","9999","0099","9900") ~ as.integer(paste0(str_sub(Date, 1, -5), "0701")),
                           str_sub(Date, -2, -1) %in% c("00","99") ~ as.integer(paste0(str_sub(Date, 1, -3), "15")),
                           str_sub(Date, 5, 6) %in% c("00","99") ~ as.integer(paste0(str_sub(Date,1,4),"06",str_sub(Date,7,8))),
                           T ~ Date),
         DateU = ymd(DateU)) %>% 
  arrange(DateU)

write_csv(unions,"data_modif/unions_mod.csv")

# Months with 31 days
months_31 <- c("01","03","05","07","08","10","12")

# Clean mentions
## When we only have an interval, get min, max and estimated ("precise") dates
## Flag when dates are interpolated, either within month (1) or within year (2)
## Get age in time format

# Records with parents or spouse mentioned
acts_w_parents <- mentions %>% filter(Role %in% c(3,4)) %>% pull(idAct) %>% unique()
acts_w_spouse <- mentions %>% filter(Role == 2) %>% pull(idAct) %>% unique()

# Clean dates and age mentions
mentions <- mentions %>% 
  filter(!is.na(idAct)) %>% 
  mutate(Age = tolower(Age)) %>% 
  mutate(across(c(Date,DateEvent), ~case_when(.x == 8560108 ~ 18560108,
                                              .x == 9999999 ~ 99999999,
                                              T ~ .x))) %>% 
  mutate(across(c(Date,DateEvent), 
                ~case_when(.x == 99999999 ~ 99999999,
                           str_sub(.x, -4, -1) %in% c("0000","9999","0099","9900") ~ as.integer(paste0(str_sub(.x, 1, -5), "0101")),
                           str_sub(.x, -2, -1) %in% c("00","99") ~ as.integer(paste0(str_sub(.x, 1, -3), "01")),
                           str_sub(.x, 5, 6) %in% c("00","99") ~ as.integer(paste0(str_sub(.x,1,4),"01",str_sub(.x,7,8))),
                           T ~ .x),
                .names = "{.col}_min")) %>% 
  mutate(across(c(Date,DateEvent), 
                ~case_when(.x == 99999999 ~ 99999999,
                           str_sub(.x, -4, -1) %in% c("0000","9999","0099","9900") ~ as.integer(paste0(str_sub(.x, 1, -5), "1231")),
                           str_sub(.x, -2, -1) %in% c("00","99") ~ 
                             as.integer(paste0(str_sub(.x, 1, -3), 
                                               ifelse(str_sub(.x, 5, 6) %in% months_31,
                                                      "31",
                                                      ifelse(str_sub(.x,5,6) == "02" & leap_year(as.numeric(str_sub(.x,1,4))),
                                                             "29",
                                                             "28")))),
                           str_sub(.x, 5, 6) %in% c("00","99") ~ as.integer(paste0(str_sub(.x,1,4),"12",str_sub(.x,7,8))),
                           T ~ .x),
                .names = "{.col}_max")) %>% 
  mutate(Date_min = if_else(Date_min < DateEvent_min & DateEvent_min != 99999999, DateEvent_min, Date_min),
         DateEvent_max = if_else(DateEvent_max > Date_max & DateEvent_max != 99999999, Date_max, DateEvent_max)) %>% 
  mutate(Date_precise = as.integer(format(ymd(Date_min) + .5*(ymd(Date_max)-ymd(Date_min)),"%Y%m%d")),
         DateEvent_precise = if_else(DateEvent == 99999999, 99999999, 
                                         as.integer(format(ymd(DateEvent_min) + .5*(ymd(DateEvent_max)-ymd(DateEvent_min)),"%Y%m%d")))) %>% 
  mutate(Date_mod = ifelse(Date == 99999999, DateEvent, Date),
         Date_mod_precise = ifelse(Date_precise == 99999999, DateEvent_precise, Date_precise)) %>% 
  mutate(flag_Date_precise = if_else(Date != Date_precise, if_else(str_sub(Date, 5, 6) %in% c("00","99"), 2, 1), 0),
         flag_DateEvent_precise = if_else(DateEvent != DateEvent_precise, if_else(str_sub(Date, 5, 6) %in% c("00","99"), 2, 1), 0),
         flag_Date_mod_precise = if_else(Date_mod != Date_mod_precise, if_else(str_sub(Date, 5, 6) %in% c("00","99"), 2, 1), 0)) %>% 
  mutate(Age_tf = dplyr::case_when(Age == "xxx" & Role == 1 & Type == "b" & DateEvent_precise != 99999999 & Date_precise != 99999999 ~ as.duration(interval(ymd(DateEvent_precise),ymd(Date_precise))),
                            Age == "xxx" & Role == 1 & Type == "b" & DateEvent == 99999999 & (!is.na(MaritalStatus) | MaritalStatus %in% c("c","x")) & !(idAct %in% acts_w_spouse) & 
                            idAct %in% acts_w_parents ~ ddays(0),
                            Age %in% c("xxx","maj","min") ~ NA,
                            Age %in% c("nvn","tts","qqi","qqh") | str_sub(Age,-1,-1) == "h" ~ ddays(0), # "a few moments/hours", "just now", "newborn"
                            Age %in% c("qqj","qqu","qqy") ~ ddays(3), # "a few days" and typos
                            Age %in% c("qqm","qqt") ~ dmonths(3), # "a few months", "some time"
                            Age == "qqs" ~ dweeks(3), # "a few weeks"
                            Age == "qqa" ~ dyears(3), # "a few years"
                            !grepl("\\d", Age) ~ NA,
                            str_sub(Age,-1,-1) %in% c("j","f") ~ ddays(parse_number(Age)), # number of days
                            str_sub(Age,-1,-1) == "s" ~ dweeks(parse_number(Age)), # number of weeks
                            str_sub(Age,-1,-1) == "m" ~ dmonths(parse_number(Age)), # number of months
                            str_sub(Age,-1,-1) == "a" | !is.na(as.numeric(Age)) ~ dyears(parse_number(Age)))) %>% # number of years
  mutate(Age_tf = time_length(Age_tf,"days"))

# Identify "real" records (as opposed to estimated records)
# I keep those for which at least the month is known
real_b <- mentions %>% 
  filter(Type == "b", Role == 1, flag_Date_precise != 2) %>% 
  write_csv("data_modif/real_b.csv")

real_s <- mentions %>% 
  filter(Type == "s", Role == 1, flag_Date_precise != 2) %>% 
  write_csv("data_modif/real_s.csv")

inds <- inds %>% 
  mutate(has_b = idInd %in% real_b$idInd,
         has_s = idInd %in% real_s$idInd)

# Identify death records of stillborn children or children who died immediately 
## since we can get their date of birth
stillborn <- mentions %>% 
  filter(Type == "s" & Role == 1 & 
           !(MaritalStatus %in% c("m","v")) & 
           (Age == "xxx" | ddays(Age_tf) < ddays(1)) &
           !(idAct %in% acts_w_spouse)) %>% 
  left_join(select(inds,has_b,idInd,DateBirth)) %>% 
  mutate(keep = case_when(ObiitOndoiement == 1 ~ 1,
                          ddays(Age_tf) < ddays(1) & has_b == 0 ~ 1,
                          !(idAct %in% acts_w_parents) ~ 0,
                          has_b == 1 | DateBirth != Date | DateBirth != DateEvent ~ 0,
                          (is.na(fname_std) | fname_std %in% c("","anonyme","anonime","enfant")) & !is.na(lname) ~ 1,
                          T ~ 0)) %>% 
  filter(keep == 1) %>% 
  write_csv("data_modif/stillborn.csv")

mentions <- mentions %>% 
  mutate(stillborn = idAct %in% stillborn$idAct) %>% 
  mutate(Age_tf = if_else(stillborn == 1 & is.na(Age_tf) & Role == 1, time_length(ddays(0),"days"), Age_tf))

# Get the exact dates of censuses in the database
censuses <- read_xlsx("data_aux/censuses_dates.xlsx") %>% 
  select(-Source) %>% 
  mutate(Type = "r",
         Date_precise = Date_mod,
         Date_mod_precise = Date_mod)

# Correct census dates
mentions <- mentions %>% dplyr::left_join(censuses,join_by(Type,Date),suffix = c("","_r"))

mentions <- mentions %>% mutate(across(c(Date_min,Date_max,Date_mod,Date_mod_precise,Date_precise),  
                                       ~if_else(is.na(pull(mentions,paste0(cur_column(),"_r"))),.x,pull(mentions,paste0(cur_column(),"_r"))))) %>% 
  select(-ends_with("_r"))

# Get dates of birth and death from records to complement the individuals file
b <- mentions %>% 
  filter(!(CodeLocation %in% c(0,22222,2,7600,6909,7900:7904)) & (CodeLocation < 8000 | CodeLocation >= 20000)) %>% 
  filter(Type == "b", Role == 1) %>% 
  mutate(priority = !(Provenance %in% c("g","j","x"))) %>% 
  arrange(flag_DateEvent_precise, desc(priority), Date) %>% 
  select(idInd,DateEvent,DateEvent_precise,Date,Date_precise,idAct,Age_tf,Provenance,flag_DateEvent_precise) %>% 
  rename_with(~paste0(.,"_b"), -idInd)

s <- mentions %>% 
  filter(!(CodeLocation %in% c(0,22222,2,7600,6909,7900:7904)) & (CodeLocation < 8000 | CodeLocation >= 20000)) %>% 
  filter(Type == "s", Role == 1) %>% 
  mutate(priority = !(Provenance %in% c("g","j","x"))) %>% 
  arrange(flag_DateEvent_precise, desc(priority), Date) %>% 
  select(idInd,DateEvent,DateEvent_precise,Date,Date_precise,idAct,Age_tf,Provenance,flag_DateEvent_precise) %>% 
  rename_with(~paste0(.,"_s"), -idInd)

# Standardize DOBs for individuals
## Sometimes there are mixes of 00s and 99s (which both mean missing or unknown), let's all standardize to 99
## Add dates of birth or death missing from individual files but actually known from a linked record
inds <- inds %>% 
  mutate(DOB = case_when(DateBirth == 99999999 ~ 99999999,
                         str_sub(DateBirth, -4, -1) %in% c("0000","9999","0099","9900") ~ as.integer(paste0(str_sub(DateBirth, 1, -5), "0701")),
                         str_sub(DateBirth, -2, -1) %in% c("00","99") ~ as.integer(paste0(str_sub(DateBirth, 1, -3), "15")),
                         str_sub(DateBirth, 5, 6) %in% c("00","99") ~ as.integer(paste0(str_sub(DateBirth,1,4),"06",str_sub(DateBirth,7,8))),
                         T ~ DateBirth),
         DOB = ymd(DOB)) %>%
  dplyr::left_join(b,join_by(idInd),multiple = "first") %>% 
  mutate(DOB = case_when(DateBirth != 99999999 & QualityDateBirth %in% c(1,3,4,5) ~ DOB, # Evenement, naissance ondoiement, ondoiement, acte chercheur
                         DateBirth == 99999999 & DateEvent_precise_b < Date_precise_b ~ ymd(DateEvent_precise_b),
                         DateBirth == 99999999 & DateEvent_precise_b > Date_precise_b & is.na(Age_tf_b) ~ ymd(Date_precise_b),
                         DateBirth == 99999999 & DateEvent_precise_b > Date_precise_b ~ ymd(Date_precise_b) - ddays(Age_tf_b),
                         QualityDateBirth == 2 & is.na(Age_tf_b) & idAct_b %in% acts_w_spouse ~ NA,
                         QualityDateBirth == 2 & is.na(Age_tf_b) ~ DOB,
                         QualityDateBirth == 2 ~ ymd(Date_precise_b) - ddays(Age_tf_b),
                         T ~ DOB),
         flag_DOB = QualityDateBirth == 2 & !is.na(Age_tf_b)) 

# Standardize DODs for individuals
inds <- inds %>% 
  mutate(DOD = case_when(DateDeath == 99999999 ~ 99999999,
                         str_sub(DateDeath, -4, -1) %in% c("0000","9999","0099","9900") ~ as.integer(paste0(str_sub(DateDeath, 1, -5), "0701")),
                         str_sub(DateDeath, -2, -1) %in% c("00","99") ~ as.integer(paste0(str_sub(DateDeath, 1, -3), "15")),
                         str_sub(DateDeath, 5, 6) %in% c("00","99") ~ as.integer(paste0(str_sub(DateDeath,1,4),"06",str_sub(DateDeath,7,8))),
                         str_sub(DateDeath, 5, 6) == "55" ~ as.integer(paste0(str_sub(DateDeath,1,4),"05",str_sub(DateDeath,7,8))),
                         T ~ DateDeath),
         DOD = ymd(DOD)) %>%
  dplyr::left_join(s,join_by(idInd),multiple = "first") %>% 
  mutate(DOD = case_when(DateDeath != 99999999 & QualityDateDeath %in% c(1,2,3,4) ~ DOD, # Evenement, acte, obiit, acte chercheur
                         DateDeath == 99999999 ~ ymd(pmin(DateEvent_precise_s,Date_precise_s)),
                         T ~ DOD)) 


# Make presence and role variables internally coherent

## Get marital status of spouse (we want to know whether v = widowed or m = married)
married_bs <- mentions %>% 
  filter(Role %in% c(1,2), idAct %in% acts_w_spouse, Type %in% c("b","s")) %>% 
  group_by(idAct) %>% 
  filter(max(Role) == 2) %>% 
  arrange(Role) %>% 
  mutate(MS_lag = lag(MaritalStatus)) %>% 
  ungroup()

## Spouses of people getting married (to a new person) are dead
marriages <- mentions %>% 
  filter(Type %in% c("m","y"), Role == 2) %>% 
  mutate(Presence_revised = "d")

## People are necessarily present when getting married or baptized
## Also correct roles
mentions_revised <- mentions %>% 
  left_join(select(married_bs,idMention,MS_lag)) %>% 
  left_join(select(marriages,idMention,Presence_revised)) %>% 
  mutate(Presence_revised = ifelse(is.na(Presence_revised),Presence,Presence_revised)) %>% 
  mutate(Presence_revised = ifelse(is.na(Presence_revised),"x",Presence_revised)) %>% 
  mutate(Presence_revised = case_when(Type == "m" & Role == 1 ~ "p",
                                  Type == "b" & Role == 1 ~ "p",
                                  Role == 2 & MS_lag == "v" & Presence_revised == "x" ~ "d",
                                  T ~ Presence_revised)) %>% 
  ungroup() %>% 
  group_by(idAct) %>% 
  mutate(max_role = max(Role),
         n = n()) %>% 
  mutate(Role_revised = case_when(Type %in% c("d","v") ~ 1,
                              max_role == 0 & Type %in% c("r","a","c","h","l","p","x") ~ 1,
                              T ~ Role)) %>% 
  ungroup() %>% 
  mutate(flag_nonsingle = if_else(idMention %in% married_bs$idMention,1,0)) %>% 
  mutate(Sex = str_to_lower(Sex))

## In many records, both parents were automatically marked present even if that is not what the record said
## Remove these mentions when both parents are marked present (the presence status was generally only indicated for fathers)
b_both_parents_p <- mentions_revised %>% 
  filter(Role %in% c(3,4), Type == "b") %>% 
  group_by(idAct) %>% 
  filter(length(unique(Presence_revised)) == 1 & first(Presence_revised) == "p") %>% 
  pull(idMention)

mentions_revised <- mentions_revised %>% 
  mutate(Presence_revised = if_else(idMention %in% b_both_parents_p, "x", Presence_revised))

# Remove unreliable linkages

if (file.exists("data_modif/linkage_reliability.csv")) {
  linkage_reliability <- read_csv("data_modif/linkage_reliability.csv")
} else {
  source("code/linkage_reliability.R")
  linkage_reliability <- read_csv("data_modif/linkage_reliability.csv")
}

# Because links that are not perfect are usually checked by a human, I only remove those
## that are very dubious (lost at least 4 points)

links_w_parents_to_remove <- linkage_reliability %>% 
  filter(V3 < 2)

links_wo_parents_to_remove <- linkage_reliability %>% 
  filter(V2 < 2)

mentions_revised <- mentions_revised %>% 
  mutate(idInd = if_else(idAct %in% links_w_parents_to_remove$ids & Role_revised %in% c(3,4), -1, idInd),
         idInd = if_else(idAct %in% links_wo_parents_to_remove$ids, -1, idInd))

unions_long <- unions %>% 
  filter(QualityDate < 4) %>% 
  pivot_longer(c(idHusband,idWife),names_to = "Spouse", values_to = "idInd") %>% 
  arrange(DateU)

# Update DOBs and DODs in the individuals file to remove those that we want to unlink
inds <- inds %>% 
  mutate(flag_b_w_parents = ifelse(idAct_b %in% links_w_parents_to_remove$ids,1,0),
         flag_d_w_parents = ifelse(idAct_s %in% links_w_parents_to_remove$ids,1,0),
         flag_d_wo_parents = ifelse(idAct_s %in% links_wo_parents_to_remove$ids,1,0),
         flag_b_wo_parents = ifelse(idAct_b %in% links_wo_parents_to_remove$ids,1,0),
         idAct_b = if_else(flag_b_w_parents == 1 | flag_b_wo_parents == 1, NA, idAct_b,
                            missing = idAct_b),
         idAct_s = if_else(flag_d_w_parents == 1 | flag_d_wo_parents == 1, NA, idAct_s,
                            missing = idAct_s),
         DOB = if_else(flag_b_wo_parents == 1, NA, DOB, missing = DOB),
         idFather = if_else(flag_b_w_parents == 1 |  flag_d_w_parents == 1, -1, idFather, missing = idFather),
         idMother = if_else(flag_b_w_parents == 1 |  flag_d_w_parents == 1, -1, idMother, missing = idMother),
         DOD = if_else(flag_d_wo_parents == 1, NA, DOD, missing = DOD))


# Remove impossible births and deaths relative to marriage
# Allow those that happened before 1750 because more checking was done for the earlier years
## and early marriages of girls were more common
# Allow 0.5 years flexibility
inds <- inds %>% 
  dplyr::left_join(select(unions_long, idInd, DateU_first = DateU), join_by(idInd), multiple = "first") %>% 
  dplyr::left_join(select(unions_long, idInd, DateU_last = DateU), join_by(idInd), multiple = "last") %>% 
  mutate(flag_DOD = DOD < DateU_last,
         age_min_m = if_else(Sex == "m", 13.5, 11.5),
         flag_DOB = DOB > DateU_first - dyears(age_min_m) & year(DateU_first) > 1750) %>% 
  mutate(DOD = if_else(flag_DOD, NA, DOD, missing = DOD),
         DOB = if_else(flag_DOB, NA, DOB, missing = DOB),
         QualityDateDeath = if_else(flag_DOD, 5, QualityDateDeath, missing = QualityDateDeath),
         QualityDateBirth = if_else(flag_DOB, 7, QualityDateBirth, missing = QualityDateBirth)) 


flagged_s <- inds %>% 
  filter(flag_DOD) %>% 
  pull(idAct_s)

flagged_b <- inds %>% 
  filter(flag_DOB) %>% 
  pull(idAct_b)

# Unlink records that we flagged as incorrect
# Keep parents linked on baptism records (only remove the child link)
mentions_revised <- mentions_revised %>% 
  mutate(idInd = if_else(idAct %in% flagged_s, -1, idInd)) %>% 
  mutate(idInd = if_else(idAct %in% flagged_b & Role_revised == 1, -1, idInd))

write_csv(mentions_revised,"data_modif/mentions_mod.csv")

write_csv(inds,"data_modif/inds_mod.csv")

rm(list = ls())
