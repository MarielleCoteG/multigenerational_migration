# Côté-Gendreau 2026
# This file estimates dates of birth by combining information such as reported ages and union bounds

# Packages we need
list_packages <- c("dplyr","readr","tidytable","lubridate")

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

# Load all the objects necessary to estimate ages
load_estimate_DOB <- function() {
  
  inds <<- read_csv("data_modif/inds_DOD_est.csv", guess_max = 5000)
  
  unions_bounds <<- read_csv("data_modif/unions_bounds.csv") %>% 
    left_join(select(inds,idHusband=idInd,DOD_husband=DOD,DOD_max_husband=DOD_max,DOD_min_husband=DOD_min)) %>% 
    left_join(select(inds,idWife=idInd,DOD_wife=DOD,DOD_max_wife=DOD_max,DOD_min_wife=DOD_min)) %>% 
    mutate(Date_end = pmin(Date_end,DOD_max_husband,DOD_max_wife,na.rm=T)) %>% 
    mutate(Date_end_min = pmin(Date_end,pmax(Date_last_conception + dweeks(40),pmin(dplyr::if_else(is.na(DOD_husband),DOD_min_husband,DOD_husband),dplyr::if_else(is.na(DOD_wife),DOD_min_wife,DOD_wife)),na.rm=T), na.rm=T)) %>% 
    group_by(idHusband) %>% 
    mutate(prev_end_h = lag(Date_end),
           prev_end_min_h = lag(Date_end_min)) %>%
    arrange(rank_w) %>% 
    group_by(idWife) %>% 
    mutate(prev_end_w = lag(Date_end),
           prev_end_min_w = lag(Date_end_min)) %>% 
    mutate(across(c(prev_end_h,prev_end_min_h,prev_end_w,prev_end_min_w), ~dplyr::if_else(.x > DateU, NA, .x))) %>% 
    write_csv("data_modif/unions_bounds_mod.csv")
  
  mentions <- read_csv("data_modif/mentions_mod.csv", guess_max = 5000) %>% 
    filter(Type != "b") %>% 
    mutate(Age_tf = ddays(Age_tf),
           Date_mod_precise = ymd(Date_mod_precise),
           DateEvent_precise = ymd(DateEvent_precise)) %>% 
    mutate(DateEv = dplyr::if_else(is.na(DateEvent_precise),Date_mod_precise,DateEvent_precise)) %>% 
    arrange(DateEv)
  
  mentions_age <<- mentions %>% 
    filter(!is.na(Age_tf)) %>% 
    select(idInd,idAct,Age_tf,DateEv,Type,Role) %>% 
    mutate(Age_tf = as.numeric(Age_tf,units="years"),
           Age_tf = dplyr::if_else(Age_tf > 130, Age_tf - 100, Age_tf),
           Age_tf = dyears(Age_tf)) %>% 
    distinct(idInd,.keep_all=T)
  
  inds_knownb <- inds %>% 
    select(idInd,Sex,DOB,QualityDateBirth,DOD,QualityDateDeath,DOD_max,DOD_est,DOD_min) %>% 
    filter(QualityDateBirth == 1) %>% 
    inner_join(mentions_age)
  
  age_error <- inds_knownb %>% 
    mutate(DOB_est = as.Date(DateEv - Age_tf),
           Age_true = DateEv - DOB,
           across(c(Age_true,Age_tf), ~dplyr::if_else(.x == ddays(0), ddays(1), .x)),
           error_abs = time_length(DOB_est - DOB,"years"),
           error_rel = error_abs / time_length(Age_true,"years")) 
  
  # Typical error by reported age (based on people for whom we have age mentions and linked birth)
  age_error_sum <<- age_error %>% 
    mutate(Age_tf = time_length(Age_tf,"years"),
           Age_tf = round(Age_tf)) %>% 
    group_by(Age_tf) %>% 
    summarize(mean = mean(error_abs), sd = sd(error_abs), median = median(error_abs),
              zperc25 = quantile(error_abs, 0.25), aperc75 = quantile(error_abs, 0.75)) %>% 
    mutate(ahigh = mean + sd,
           zlow = mean - sd) %>% 
    pivot_longer(c(mean,ahigh,zlow,zperc25,median,aperc75), names_to = "measure", values_to = "value")
  
  # First marriages with major/minor mentions
  first_m <<- mentions %>% 
    select(idInd,idAct,Age,Date_mod_precise,Type,Role_revised) %>% 
    filter(Type %in% c("m","n","y"), Role_revised == 1) %>% 
    select(-Role_revised) %>% 
    arrange(Date_mod_precise) %>% 
    distinct(idInd,.keep_all=T) %>% 
    filter(Age %in% c("maj","min")) %>% 
    rename(Date_fm = Date_mod_precise,
           idAct_fm = idAct,
           Age_fm = Age,
           Type_fm = Type)
  
  # Whether or not brides and grooms with a majority mention were indeed over 21
  inds_knownb_m <<- inds %>% 
    filter(QualityDateBirth == 1) %>% 
    inner_join(first_m) %>% 
    mutate(Age_true = time_length(Date_fm - DOB,"years")) %>% 
    mutate(maj_true = Age_true >= 21) %>% 
    write_csv("data_modif/inds_knownb_m.csv")
  
  # Add first age mention to inds
  # I have verified that the first mention is a better estimate than the mean of all age mentions
  # Most individuals only have one mention anyway (usually their death)
  inds <<- inds %>% 
    dplyr::left_join(select(mentions,date_1st_mention = DateEv,idInd), join_by(idInd), multiple = "first")
}

estimate_DOB <- function(time_gap = 5, central = "median") {
  
  list_of_objects <- c("inds","unions_bounds","age_error_sum","mentions_age","first_m","inds_knownb_m")
  
  if(min(sapply(list_of_objects,exists)) != 1) {
    load_estimate_DOB()
  }
  
  # Typical error by reported age
  age_error_central <- age_error_sum %>% 
    filter(measure == central,Age_tf!=0) %>% 
    add_row(Age_tf = c(115,117:119)) %>% 
    mutate(correction = dplyr::if_else(is.na(sd), 92-Age_tf,value)) %>% 
    select(Age_tf,correction)
  
  # Typical ages at marriage for minors and majors by sex
  age_mar_central <- inds_knownb_m %>% 
    group_by(Sex,Age_fm) %>% 
    summarize(age_fm_central = get(central)(Age_true))
  
  # See Supporting Information for an explanation of this process
  inds_DOB_est <- inds %>% 
    select(idInd,idFather,idMother,Sex,Ill,DateM1,QualityDateM1,DOB,QualityDateBirth,DateMP,QualityDateMP,
           DOB_mother,QualityDateBirth_mother,
           DOD_mother,DOD_father,DOD_max_mother,DOD_max_father,flag_DOD_est_mother,flag_DOD_est_father,
           DateM1_mother,QualityDateM1_mother,DateChild1_mother,QualityDateChild1_mother,DateChild1_mother,QualityDateChild1_mother,
           DateChildn,QualityDateChildn,date_1st_mention,DOD) %>%
    mutate(no_DOB = QualityDateBirth %in% c(6,7) | is.na(DOB)) %>% 
    left_join(mentions_age) %>% 
    left_join(first_m) %>% 
    left_join(age_mar_central) %>% 
    left_join(select(unions_bounds,idFather=idHusband,idMother=idWife,prev_end_h,prev_end_w,prev_end_min_h,prev_end_min_w)) %>% 
    mutate(Age_num = round(time_length(Age_tf,"years")),
           DOD_father = dplyr::if_else(flag_DOD_est_father == 1,DOD_max_father,DOD_father),
           DOD_mother = dplyr::if_else(flag_DOD_est_mother == 1,DOD_max_mother,DOD_mother)) %>%
    dplyr::left_join(age_error_central,join_by(Age_num == Age_tf)) %>% 
    mutate(maj_threshold = Date_fm-dyears(21)) %>% 
    mutate(DateM1_mother = dplyr::if_else(QualityDateM1_mother <= 3, as.Date(DateM1_mother), NA_Date_),
           DateM1 = dplyr::if_else(QualityDateM1 <= 3, as.Date(DateM1), NA_Date_),
           DateChild1_mother = dplyr::if_else(QualityDateChild1_mother <= 5, as.Date(DateChild1_mother), NA_Date_),
           DateChildn = dplyr::if_else(QualityDateChildn <= 5, as.Date(DateChildn), NA_Date_),
           DateMother_55 = dplyr::if_else(QualityDateBirth_mother <= 5,DOB_mother + dyears(55),NA_Date_),
           across(starts_with(c("DOB","DOD","Date")), ~as.Date(.x))) %>% 
    mutate(date_rel_min = dplyr::if_else(QualityDateMP <= 3, DateMP, NA_Date_),         
           date_rel_min = pmax(date_rel_min,prev_end_min_h,prev_end_min_w, na.rm = T),
           date_rel_max = pmin(DOD_father + dweeks(28), DOD_mother, DateMother_55, DateM1_mother + dyears(40), DateChild1_mother + dyears(40), na.rm = T),
           age_min_marr = if_else(Sex=="f",12,14),
           date_self_max = pmin(date_1st_mention,DOD,DateM1-dyears(age_min_marr), na.rm = T),
           date_self_min = DateChildn-dyears(53),
           date_min_nomarr = pmax(date_rel_min,date_self_min,na.rm=T),
           date_max_nomarr = pmin(date_rel_max,date_self_max,na.rm=T),
           date_mar_min = dplyr::if_else(Age_fm == "min", Date_fm-dyears(21)+ddays(1), NA),
           date_mar_max = dplyr::if_else(Age_fm == "maj", Date_fm-dyears(21), NA),
           date_min = pmax(date_min_nomarr,date_mar_min,na.rm=T),
           date_max = pmin(date_max_nomarr,date_mar_max,na.rm=T),
           Ill_imp = case_when(Ill == 1 ~ 1,
                               date_self_max < date_rel_min ~ 1,
                               T ~ 0)) %>% 
    mutate(DOB_firstage = dplyr::if_else(is.na(correction), DateEv-Age_tf, DateEv - (Age_tf + dyears(correction))),
           DOB_firstage_nocorr = DateEv-Age_tf,
           DOB_fm = Date_fm - dyears(age_fm_central),
           across(c(DOB_firstage,DOB_firstage_nocorr,DOB_fm), ~as.Date(.x)),
           keep_estimation = dplyr::case_when(!is.na(DOB) & QualityDateBirth == 5 & year(DOB_firstage_nocorr) != year(DOB) ~ 1, 
                                              !is.na(DOB) & QualityDateBirth == 6 & year(DOB_firstage_nocorr) != year(DOB) ~ 1,
                                              T ~ 0))
  
  # I concatenate the imputed DOB and the number of the case applied to be able to follow how cases are distributed
  inds_DOB_est <- inds_DOB_est %>% 
    mutate(DOB_est = case_when(date_min <= date_max & dplyr::between(DOB_firstage,date_min,date_max) ~ paste(DOB_firstage,1,sep="_"),
                               date_min <= date_max & dplyr::between(DOB_firstage_nocorr,date_min,date_max) ~ paste(DOB_firstage_nocorr,2,sep="_"),
                               date_min <= date_max & is.na(DOB_firstage) & Age_fm == "min" & dplyr::between(DOB_fm,date_min,date_max) ~ paste(DOB_fm,3,sep="_"),
                               date_min <= date_max & is.na(DOB_firstage) & date_max - date_min <= dyears(time_gap) ~ paste(as.Date(date_min + (date_max-date_min)*0.5),4,sep="_"),
                               Ill_imp == 1 & !is.na(DOB_firstage) & !is.na(date_self_min) & DOB_firstage >= date_self_min & !is.na(date_mar_min) & DOB_firstage >= date_mar_min ~ paste(DOB_firstage,5,sep="_"),
                               Ill_imp == 1 & !is.na(DOB_firstage_nocorr) & !is.na(date_self_min) & DOB_firstage_nocorr >= date_self_min & !is.na(date_mar_min) & DOB_firstage_nocorr >= date_mar_min ~ paste(DOB_firstage_nocorr,6,sep="_"),
                               date_min <= date_max & DOB_firstage < date_min ~ paste(date_min,7,sep="_"),
                               date_min <= date_max & DOB_firstage > date_max ~ paste(date_max,8,sep="_"),
                               date_min_nomarr <= date_max_nomarr & dplyr::between(DOB_firstage_nocorr,date_min_nomarr,date_max_nomarr) ~ paste(DOB_firstage_nocorr,9,sep="_"),
                               date_min_nomarr <= date_max_nomarr & is.na(DOB_firstage) & Age_fm == "min" & dplyr::between(DOB_fm,date_min_nomarr,date_max_nomarr) ~ paste(DOB_fm,10,sep="_"),
                               date_min_nomarr <= date_max_nomarr & is.na(DOB_firstage) & date_max_nomarr - date_min_nomarr <= dyears(time_gap) ~ paste(as.Date(date_min_nomarr + (date_max_nomarr-date_min_nomarr)*0.5),11,sep="_"),
                               Ill_imp == 1 & !is.na(DOB_firstage) & !is.na(date_self_min) & DOB_firstage >= date_self_min ~ paste(DOB_firstage,12,sep="_"),
                               Ill_imp == 1 & !is.na(DOB_firstage_nocorr) & !is.na(date_self_min) & DOB_firstage_nocorr >= date_self_min ~ paste(DOB_firstage_nocorr,13,sep="_"),
                               date_min_nomarr <= date_max_nomarr & DOB_firstage < date_min_nomarr ~ paste(date_min_nomarr,14,sep="_"),
                               date_min_nomarr <= date_max_nomarr & DOB_firstage > date_max_nomarr ~ paste(date_max_nomarr,15,sep="_"),
                               is.na(date_min) & DOB_firstage <= date_max ~ paste(DOB_firstage,16,sep="_"),
                               is.na(date_max) & DOB_firstage >= date_min ~ paste(DOB_firstage,17,sep="_"),
                               is.na(date_min) & DOB_firstage > date_max ~ paste(date_max,18,sep="_"),
                               is.na(date_max) & DOB_firstage < date_min ~ paste(date_min,19,sep="_"),
                               is.na(date_min) & is.na(date_max) & !is.na(DOB_firstage) ~ paste(DOB_firstage,20,sep="_"),
                               is.na(date_min) & Age_fm == "min" & DOB_fm <= date_max ~ paste(DOB_fm,21,sep="_"),
                               is.na(date_max) & Age_fm == "min" & DOB_fm >= date_min ~ paste(DOB_fm,22,sep="_"),
                               is.na(date_min) & is.na(date_max) & !is.na(DOB_fm) ~ paste(DOB_fm,23,sep="_"),
                               T ~ NA)) %>% 
    separate(DOB_est, into = c("DOB_est", "rule_DOB"), sep = "_") %>% 
    mutate(DOB_est = as.Date(ymd(DOB_est)),
           flag_min_max = date_max < date_min,
           has_DOB_firstage = !is.na(DOB_firstage),
           error_DOB_abs = time_length(DOB_est - DOB,"years"),
           Age_true = DateEv - DOB,
           error_DOB_rel = error_DOB_abs/time_length(Age_true,"years"),
           DOB_min = case_when(date_min <= date_max | is.na(date_max) ~ date_min, 
                               date_min_nomarr <= date_max ~ date_min_nomarr,
                               T ~ NA),
           DOB_max = case_when(date_min <= date_max | is.na(date_min) ~ date_max, 
                               date_max_nomarr >= date_min ~ date_max_nomarr,
                               T ~ NA),
           DOB_mean = as.Date(DOB_min + (DOB_max-DOB_min)*0.5))
  
  inds <- inds %>% 
    left_join(select(inds_DOB_est,idInd,DOB_est,error_DOB_abs,error_DOB_rel,rule_DOB,has_DOB_firstage,DOB_min,DOB_max,DOB_mean,keep_estimation)) %>% 
    mutate(DOB = dplyr::if_else(DOB > DOD & flag_DOD_est == 0, DOD, DOB, missing = DOB),
           DOD_min = dplyr::if_else(DOD_min < DOB, DOB, DOD_min, missing = DOD_min),
           DOB_max = dplyr::if_else(DOB_max > DOD, DOD, DOB_max, missing = DOB_max)) %>% 
    mutate(flag_DOB_est = dplyr::if_else(!is.na(DOB_est) & (has_b == F & keep_estimation == 0), 1, 0),
           DOB = as.Date(dplyr::if_else(flag_DOB_est == 1, DOB_est, DOB)))
  
  inds_branks <- inds %>% 
    filter(idFather != -1 | idMother != -1) %>% 
    filter(idInd %in% unions_bounds$idHusband | idInd %in% unions_bounds$idWife) %>% 
    group_by(idFather, Sex) %>% 
    mutate(n_f = n(), rank_f = row_number()) %>% 
    group_by(idMother, Sex) %>% 
    mutate(n_m = n(), rank_m = row_number()) %>% 
    group_by(idFather, idMother, Sex) %>% 
    mutate(n_fm = n(), rank_fm = row_number()) %>% 
    ungroup() %>% 
    mutate(across(c(n_f,rank_f,n_fm,rank_fm), ~if_else(idFather == -1, 0, .x)),
           across(c(n_m,rank_m,n_fm,rank_fm), ~if_else(idMother == -1, 0, .x)),
           nsibs_em_sex = n_f + n_m - n_fm,
           brank_em_sex = rank_f + rank_m - rank_fm)
  
  inds <- inds %>% 
    left_join(inds_branks %>% select(idInd,nsibs_em_sex,brank_em_sex)) %>% 
    write_csv("data_modif/inds_DOB_est.csv")
  
  print(paste0("Estimated DOB for ", nrow(inds %>% filter(flag_DOB_est == 1)), " individuals"))
}

estimate_DOB()

rm(list = ls())
