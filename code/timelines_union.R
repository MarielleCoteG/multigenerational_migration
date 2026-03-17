# Côté-Gendreau 2026
# This file builds family timelines by combining all information that informs us on a family's residential location

# Packages we need
list_packages <- c("dplyr","readr","stringr","tidytable","lubridate","ecospace")

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

inds <- read_csv("data_modif/inds_DOB_est.csv", guess_max = 5000)
mentions <- read_csv("data_modif/mentions_mod.csv") %>% 
  filter(!(CodeLocation %in% c(0,22222,2,7600,6909,7900:7904)) & (CodeLocation < 8000 | CodeLocation >= 20000)) %>% 
  filter(idInd != -1)
  
parishes <- read_csv("data_modif/parishes.csv")
source("code/timelines_helper.R")

union_register <- function(idU, interpolation = 0.5, silent = F) {
  
  union <- unions %>% filter(idUnion == idU)
  
  idHusband <- union$idHusband
  idWife <- union$idWife
  
  union_creation <- union$DateU
  union_dissolution <- union$Date_dissolution
  
  sort <- mentions_clean %>% 
    filter(Date_mod_precise >= union_creation) %>% 
    filter(is.na(union_dissolution) | Date_mod_precise <= union_dissolution + ddays(1)) %>% 
    filter(idFather == idHusband | idMother == idWife | idInd == idHusband | idInd == idWife) %>% 
    distinct(idAct,.keep_all=T) %>% 
    mutate(idU = idU)
  
  if (silent == F & union$i %% 5000 == 0) {
    print(union$i)
  }
  
  if (nrow(sort) < 2 | first(sort$flag_uncertain) != 0) {return(NULL)}
  
  if (length(unique(sort$CodeLocation)) == 1) {
    
    intervals_i <- sort %>% 
      dplyr::summarize(idU = first(idU),
                CodeLocation = first(CodeLocation),
                Date_from = first(Date_mod_precise),
                Date_to = last(Date_mod_precise),
                length_time = Date_to - Date_from,
                n_acts = dplyr::n(),
                only_uncertain = min(flag_uncertain)) %>% 
      mutate(Date_first = Date_from,
             Date_last = Date_to) %>% 
      select(idU,CodeLocation,Date_from,Date_to,length_time,n_acts,Date_first,Date_last,only_uncertain)
    
    return(intervals_i)
  }
  
  first_uncertain <- which(sort$flag_uncertain == 1)[1]
  
  if(!is.na(first_uncertain)) {
    
    sort_short <- sort[(first_uncertain-1):nrow(sort),]
    
    later_cycle <- min_cost_transformation(sort_short$CodeLocation, sort_short$flag_uncertain)
    
    sort_short <- sort_short %>% 
      mutate(keep = later_cycle$optimal_sequence) %>% 
      filter(keep == 1) %>% 
      select(-keep) %>% 
      slice(-1)
    
    sort <- sort %>% 
      slice(1:(first_uncertain-1)) %>% 
      bind_rows(sort_short)
    
  }
  
  if (nrow(sort) < 2) {return(NULL)}
  
  if (length(unique(sort$CodeLocation)) == 1) {
    
    intervals_i <- sort %>% 
      dplyr::summarize(idU = first(idU),
                CodeLocation = first(CodeLocation),
                Date_from = first(Date_mod_precise),
                Date_to = last(Date_mod_precise),
                length_time = Date_to - Date_from,
                n_acts = dplyr::n(),
                only_uncertain = min(flag_uncertain)) %>% 
      mutate(Date_first = Date_from,
             Date_last = Date_to) %>% 
      select(idU,CodeLocation,Date_from,Date_to,length_time,n_acts,Date_first,Date_last,only_uncertain)
    
    return(intervals_i)
  }
  
  sort <- sort %>% 
    select(idU,idAct,CodeLocation,Date_mod_precise,flag_uncertain,parish_opening) %>% 
    mutate(loc_lead = lead(CodeLocation),
           loc_lag = lag(CodeLocation)) %>% 
    mutate(A = as.integer(loc_lead != CodeLocation & !is.na(loc_lead)),
           B = as.integer(loc_lag != CodeLocation & !is.na(loc_lag))) %>% 
    mutate(id_loc = consecutive_id(CodeLocation))
  
  sort <- sort %>% 
    group_by(id_loc) %>% 
    mutate(n_acts = n(),
           only_uncertain = min(flag_uncertain),
           Date_first = first(Date_mod_precise),
           Date_last = last(Date_mod_precise)) %>% 
    ungroup() %>% 
    filter(A == 1 | B == 1 | is.na(loc_lead) | is.na(loc_lag)) %>% 
    mutate(across(c(Date_mod_precise,idAct,CodeLocation,parish_opening), ~lead(.x),
                  .names = "{.col}_lead")) %>% 
    mutate(Date_mod_precise_lag = lag(Date_mod_precise)) %>% 
    filter(A == 1 | is.na(loc_lead) | is.na(loc_lag)) %>% 
    mutate(gap = if_else(A == 1, Date_mod_precise_lead - Date_mod_precise, NA),
           gap_opening = if_else(A == 1 & dplyr::between(parish_opening_lead, Date_mod_precise, Date_mod_precise_lead), Date_mod_precise_lead - parish_opening_lead, NA)) %>% 
    mutate(Date_move = pmin(if_else(is.na(gap_opening),
                               as.Date(Date_mod_precise + interpolation*gap),
                               as.Date(parish_opening_lead + interpolation*gap_opening)),
                            Date_mod_precise_lead))

  intervals_i <- sort %>% 
    mutate(Move_lag = lag(Date_move)) %>% 
    filter(A == 1 | is.na(loc_lead)) %>% 
    mutate(Date_from = case_when(is.na(Move_lag) & is.na(Date_mod_precise_lag) ~ Date_mod_precise,
                                 is.na(Move_lag) ~ Date_mod_precise_lag, 
                                 T ~ Move_lag+ddays(1)),
           Date_to = dplyr::if_else(is.na(Date_move),Date_mod_precise,Date_move)) %>% 
    mutate(across(c(Date_from,Date_to), ~dplyr::if_else(!is.na(union_dissolution) & .x > union_dissolution, union_dissolution, .x, missing = .x))) %>%
    select(idU,CodeLocation,Date_from,Date_to,n_acts,Date_first,Date_last,only_uncertain) %>% 
    mutate(CodeLocation_before = lag(CodeLocation),
           CodeLocation_after = lead(CodeLocation),
           length_time = Date_to - Date_from)
  
  return(as.data.frame(intervals_i))
}


interpolate_FU <- function(age_young_m = 14, age_young_f = 14, interpolation = 0.5, nslice = NULL, silent = F) {
  
  ptm <- proc.time()
  
  deaths <- mentions %>% 
    filter(Role_revised == 1, Type == "s") %>% 
    mutate(Date_precise = ymd(Date_precise))
  
  unions <<- read_csv("data_modif/unions_bounds_mod.csv") %>% 
    left_join(select(inds,idHusband=idInd,DOD_m=DOD,DOD_max_m=DOD_max,DOD_min_m=DOD_min,idAct_s_m = idAct_s)) %>% 
    left_join(select(inds,idWife=idInd,DOD_f=DOD,DOD_max_f=DOD_max,DOD_min_f=DOD_min,idAct_s_f = idAct_s)) %>% 
    left_join(select(deaths,idAct_s_m = idAct,Date_precise_s_m = Date_precise)) %>% 
    left_join(select(deaths,idAct_s_f = idAct,Date_precise_s_f = Date_precise)) %>% 
    mutate(Date_both_dead = case_when(!is.na(DOD_m) & !is.na(DOD_f) ~ pmax(DOD_m,DOD_f),
                                      !is.na(DOD_m) ~ pmax(DOD_m,DOD_max_f),
                                      !is.na(DOD_f) ~ pmax(DOD_f,DOD_max_m),
                                      T ~ pmax(DOD_max_m,DOD_max_f))) %>% 
    mutate(Date_dissolution = dplyr::if_else(is.na(next_marr_m) & is.na(next_marr_w),pmax(Date_both_dead,Date_precise_s_m,Date_precise_s_f), pmin(next_marr_m,next_marr_w,na.rm=T)-ddays(1))) %>% 
    mutate(DateU = as.Date(DateU))
  
  bd_young <- mentions %>% 
    filter(Type %in% c("b","s","c"), Role_revised == 1) %>% 
    left_join(select(inds,idInd,DOB,idFather,idMother,DOB_min)) %>% 
    filter(idFather != -1 | idMother != -1) %>% 
    mutate(Date_mod_precise = paste(Date_mod_precise)) %>% 
    filter(!(Provenance %in% c("g","a") & str_sub(Date_mod_precise, -4, -1) %in% c("0000","9999"))) %>% 
    mutate(Date_mod_precise = ymd(Date_mod_precise)) %>% 
    mutate(age_true = time_length(Date_mod_precise - DOB, unit="years"),
           age_max = time_length(Date_mod_precise - DOB_min, unit="years"),
           age_young = if_else(Sex == "f",age_young_f,age_young_m)) %>% 
    mutate(keep = Type == "b" & ((is.na(Age_tf) & Age != "maj") | (ddays(Age_tf) < dyears(age_young)))) %>% 
    mutate(keep = keep == TRUE | (!is.na(age_true) & age_true < age_young | !is.na(age_max) & age_max < age_young | grepl("qq|tts|min|nvn",Age) | str_sub(Age,-2,-1) %in% c("j","m","s") | (ddays(Age_tf) < dyears(age_young)))) %>% 
    mutate(keep = if_else(flag_nonsingle == 1, FALSE, keep, missing = keep)) %>% 
    filter(keep == TRUE) %>% 
    write_csv("data_modif/bd_young.csv")
  
  d_parents_mentioned <- mentions %>% 
    filter(Type == "s", Role_revised %in% c(3,4)) %>% 
    pull(idAct)
  
  d_adult <- mentions %>% 
    filter(Type == "s", idAct %in% d_parents_mentioned, Role_revised == 1) %>% 
    filter(!(idInd %in% c(unions$idHusband,unions$idWife))) %>% 
    filter(!(idMention %in% bd_young$idMention)) %>% 
    mutate(across(c(Date_mod_precise), ~ymd(.x))) %>% 
    left_join(select(inds,idInd,idFather,idMother))
    
  mentions_others <- mentions %>% 
    filter(Role_revised == 1) %>% 
    mutate(across(c(Date_mod_precise), ~ymd(.x)))
  
  # First marriages only
  # Need to mention parents
  
  m_parents_mentioned <- mentions %>% 
    filter(Type == "m", Role_revised %in% c(3,4), idInd != -1) %>% 
    pull(idAct)
  
  first_marriages <- mentions %>% 
    filter(Type == "m", Role_revised == 1) %>% 
    mutate(Date_mod_precise = ymd(Date_mod_precise)) %>% 
    arrange(Date_mod_precise) %>% 
    distinct(idInd, .keep_all = T) %>% 
    filter(MaritalStatus != "v",idAct %in% m_parents_mentioned) %>% 
    left_join(select(inds,idInd,idFather,idMother))
  
  adult <- bind_rows(d_adult, first_marriages) %>% 
    mutate(flag_uncertain = 1)
  
  mentions_clean <<- bind_rows(bd_young,mentions_others,adult) %>% 
    mutate(flag_uncertain = if_else(flag_uncertain == 1, 1, 0, missing = 0)) %>% 
    mutate(has_coordinates = if_else(CodeLocation %in% parishes$CodeLocation, 1, 0)) %>% 
    filter(has_coordinates == 1) %>% 
    left_join(select(parishes,CodeLocation,year_par)) %>% 
    mutate(parish_opening = ymd(paste0(year_par,"0101"))) %>% 
    select(idAct,idInd,idFather,idMother,Date_mod_precise,CodeLocation,flag_uncertain,parish_opening) %>% 
    arrange(Date_mod_precise,CodeLocation)

  unions <<- unions %>% 
    filter(!is.na(DateU))
  
  if (!is.null(nslice)) {
    unions <<- slice_sample(unions,n=nslice)
  }
  
  unions <<- unions %>% mutate(i = row_number())
  
  register <- lapply(unions$idUnion,union_register,interpolation=interpolation, silent=silent)

  intervals <- rbind_listdf(lists = register)
  
  write_csv(intervals, "register/intervals_u.csv")
  
  time_hours <- (proc.time() - ptm)[3]/3600
  
  return(paste0("Success! Built timelines for ", length(unique(intervals$idU)), " unions in ", round(time_hours,digits=2), " hours."))
}

interpolate_FU()

rm(list = ls())
