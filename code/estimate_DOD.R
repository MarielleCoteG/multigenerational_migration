# Côté-Gendreau 2026
# This file estimates dates of death when it can be inferred from other records with sufficient precision

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

load_estimate_DOD <- function() {
  
  inds <<- read_csv("data_modif/inds_mod.csv", guess_max = 5000) %>% 
    mutate(across(c(DOD,DOB), ~as.Date(.x)))
  unions <<- read_csv("data_modif/unions_mod.csv")
  mentions <<- read_csv("data_modif/mentions_mod.csv")
  
  # Files produced in union_bounds.R
  real_b <<- read_csv("data_modif/real_b.csv")
  real_s <<- read_csv("data_modif/real_s.csv")
  conceptions <<- read_csv("data_modif/conceptions.csv")
  unions_bound <<- read_csv("data_modif/unions_bounds.csv")
  
  # Trust "x" as "v" for bride and groom's parents when at least one has an explicit presence mention
  # About 75% trustworthy
  trustworthy_ms <<- mentions %>% 
    filter(Type == "m", Role_revised %in% c(3,4), Presence_revised %in% c("a","p","d","v")) %>% 
    pull(idAct) %>% 
    unique()
  
  # "conservative" record: reliable observation of someone as being alive (conception/birth of child, census, acts where people need to be alive)
  mentions_revised <<- mentions %>% 
    bind_rows(conceptions) %>% 
    mutate(cons = ifelse(Role_revised == 1 & Type %in% c("a","b","c","d","e","f","g","h","l","m","n","r","t","v","x","y","z") | Role_revised %in% c(3,4) & Type == "i" | Role_revised == 2 & Type %in% c("n","m","y","z") | Type == "p",
                         1,0),
           trust = if_else(Type == "m" & Role_revised %in% c(3,4) & !(idAct %in% trustworthy_ms),0,1)) %>% 
    arrange(Date_mod_precise,desc(Type))
  
  first_d <<- mentions_revised %>% 
    select(idInd,Presence_revised,Type,Role_revised,Date_mod_precise,cons,idAct) %>% 
    filter(idInd != -1, Presence_revised == "d", !(Type == "s" & Role_revised == 1)) %>% 
    arrange(Date_mod_precise) %>% 
    distinct(idInd,.keep_all=T) %>% 
    mutate(Date_mod_precise = ymd(Date_mod_precise))
  
  # Last presence before first death
  p_before_d <<- mentions_revised %>% 
    select(idInd,Presence_revised,Type,Role_revised,Date_mod_precise,cons,trust,idAct) %>% 
    filter(Presence_revised != "d") %>% 
    mutate(Date_mod_precise = ymd(Date_mod_precise)) %>% 
    left_join(select(first_d,Date_firstd = Date_mod_precise,idInd)) %>% 
    filter(Date_mod_precise <= Date_firstd | is.na(Date_firstd)) %>% 
    arrange(desc(Date_mod_precise))
  
  return()
}

# time_gap is the maximum number of years between the minimum and maximum dates of death to interpolate
# conservative 
estimate_DOD <- function(time_gap = 5, conservative = 1) {
  
  list_of_objects <- c("inds","unions","mentions","real_b","real_s","conceptions",
                       "unions_bound","trustworthy_ms","mentions_revised","first_d",
                       "p_before_d")
  
  if(min(sapply(list_of_objects,exists)) != 1) {
    load_estimate_DOD()
  }

  if (conservative == 2) {
    
    # Last presence but conservative
    last_p <- p_before_d %>% 
      filter(Presence_revised %in% c("p","v","a"),cons == 1) %>% 
      distinct(idInd,.keep_all=T)
    
  } else if (conservative == 1) {
    
    # Last presence, all those with "p","v","a"
    last_p <- p_before_d %>% 
      filter(Presence_revised %in% c("p","a","v")) %>%
      distinct(idInd,.keep_all=T)
    
  } else {
    
    # Accept trustworthy marriages
    # Parents not marked dead are assumed to be alive
    last_p <- p_before_d %>% 
      filter(Presence_revised %in% c("p","a","v") | (idAct %in% trustworthy_ms & Role_revised %in% c(3,4))) %>%
      distinct(idInd,.keep_all=T)
  }

  inds_death_est <- inds %>% 
    left_join(select(first_d,Date_firstd=Date_mod_precise,idAct_firstd=idAct,idInd,Type_firstd=Type)) %>% 
    left_join(select(last_p,DOD_min=Date_mod_precise,idAct_lastp=idAct,idInd,Type_lastp=Type)) %>% 
    dplyr::left_join(select(unions_bound,idHusband,next_marr_w), join_by(idInd == idHusband), multiple = "last") %>% 
    dplyr::left_join(select(unions_bound,idWife,next_marr_m), join_by(idInd == idWife), multiple = "last") %>% 
    mutate(sp_remarriage = if_else(is.na(next_marr_w),next_marr_m,next_marr_w),
           sp_remarriage = ymd(sp_remarriage)) %>% 
    mutate(Date_firstd = pmin(Date_firstd,sp_remarriage)) %>% 
    mutate(DOD_max = pmin(Date_firstd,sp_remarriage),
           gap = time_length(difftime(DOD_max,DOD_min),"years"),
           DOD_in_int = dplyr::between(DOD,DOD_min,DOD_max),
           DOD_est = if_else(dplyr::between(gap,0,time_gap),as.Date(DOD_min+dyears(gap)/2),NA),
           error_DOD_abs = time_length(difftime(DOD_est,DOD),"years"),
           across(c(DOD_min,DOD_max), ~if_else(!is.na(DOD),DOD,.x)),
           flag_DOD_est = if_else(is.na(DOD) & !is.na(DOD_est), 1, 0),
           DOD = as.Date(if_else(flag_DOD_est == 1, DOD_est, DOD))) %>% 
    dplyr::left_join(select(.,DOD,DOD_max,DOD_min,flag_DOD_est,error_DOD_abs,idInd),join_by(idFather==idInd),suffix=c("","_father")) %>% 
    dplyr::left_join(select(.,DOD,DOD_max,DOD_min,flag_DOD_est,error_DOD_abs,idInd),join_by(idMother==idInd),suffix=c("","_mother")) %>% 
    write_csv("data_modif/inds_DOD_est.csv")
  
  print(paste0("Estimated DOD for ", nrow((inds_death_est %>% filter(flag_DOD_est == 1))), " individuals"))
}

estimate_DOD()

rm(list = ls())
